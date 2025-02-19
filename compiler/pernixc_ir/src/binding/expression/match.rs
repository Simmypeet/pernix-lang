use std::{
    collections::{hash_map::Entry, HashMap, VecDeque},
    num::NonZero,
};

use drain_filter_polyfill::VecExt;
use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_source_file::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{self, ConnectedList};
use pernixc_table::{
    component::{Member, Parent, SymbolKind, VariantDeclarationOrder},
    diagnostic::Diagnostic,
    GlobalID, Table,
};
use pernixc_term::{
    r#type::{Primitive, Qualifier, Type},
    Symbol,
};

use super::{Bind, Config, Expression, Target};
use crate::{
    address::{Address, Memory},
    binding::{
        diagnostic::{NonExhaustiveMatch, UnreachableMatchArm},
        infer::{self, Expected},
        pattern::Path,
        Abort, Binder, Error,
    },
    control_flow_graph::Block,
    instruction::{
        ConditionalJump, Instruction, Jump, ScopePop, ScopePush, SwitchJump,
        SwitchValue, Terminator, UnconditionalJump,
    },
    model::Constraint,
    pattern::{NameBindingPoint, Refutable, Wildcard},
    scope,
    value::{
        literal::{self, Literal, Numeric},
        register::{
            Assignment, Binary, BinaryOperator, Load, Phi, RelationalOperator,
            VariantNumber,
        },
        Value,
    },
};

impl Refutable {
    fn get_conditional_value(&self, table: &Table) -> Option<SwitchValue> {
        match self {
            Self::Boolean(boolean) => {
                Some(SwitchValue::Positive(boolean.value.into()))
            }
            Self::Integer(integer) => Some(integer.value),
            Self::Enum(variant) => Some(SwitchValue::Positive(
                table.get::<VariantDeclarationOrder>(variant.variant_id).order
                    as u64,
            )),

            _ => None,
        }
    }
}

impl Binder<'_> {
    #[allow(clippy::cast_sign_loss)]
    fn is_exhaustive(
        &self,
        values: impl Iterator<Item = SwitchValue>,
        pattern: &Refutable,
        ty: &Type<infer::Model>,
    ) -> bool {
        match pattern {
            Refutable::Boolean(_) => {
                assert_eq!(*ty, Type::Primitive(Primitive::Bool));

                let mut true_found = false;
                let mut false_found = false;

                for value in values {
                    if value == SwitchValue::Positive(0) {
                        assert!(!false_found, "duplicated false value");
                        false_found = true;
                    } else if value == SwitchValue::Positive(1) {
                        assert!(!true_found, "duplicated true value");
                        true_found = true;
                    } else {
                        panic!("unexpected value {value:?}");
                    }
                }

                true_found && false_found
            }

            Refutable::Integer(_) => {
                // FIXME: implement this
                false
            }

            Refutable::Enum(_) => {
                let Type::Symbol(Symbol { id: enum_id, .. }) = ty else {
                    panic!("unexpected type {ty:#?}");
                };

                let member = self.table.get::<Member>(*enum_id);
                let mut variant_handled =
                    bit_vec::BitVec::from_elem(member.len(), false);

                for value in values {
                    assert!(
                        !variant_handled
                            .get(
                                value
                                    .into_positive()
                                    .unwrap()
                                    .try_into()
                                    .unwrap()
                            )
                            .unwrap(),
                        "duplicate variant {value:?}"
                    );

                    variant_handled.set(
                        value.into_positive().unwrap().try_into().unwrap(),
                        true,
                    );
                }

                variant_handled.all()
            }

            x => panic!("pattern {x:#?} is not refutable"),
        }
    }

    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    fn handle_match_arms(
        &mut self,
        match_info: &MatchInfo,
        non_exhaustives: &mut Vec<NonExhaustive>,
        arm_infos: &mut [ArmInfo],
        mut arm_states: Vec<ArmState>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<(), Error> {
        if arm_states.is_empty() {
            return Ok(());
        }

        // pick the arm that is declared first
        let Some((main_path, main_root_refutable)) =
            arm_states.first_mut().unwrap().refutable_paths.pop_front().map(
                |x| {
                    (
                        x,
                        &arm_infos[arm_states.first().unwrap().arm_info_index]
                            .reftuable_pattern,
                    )
                },
            )
        else {
            // the rest of the arms are unreachable
            return self.bind_match_arm(
                match_info,
                &mut arm_infos[arm_states.first().unwrap().arm_info_index],
                handler,
            );
        };

        // remove the arms that have the same refutable paths as the main path.
        // they are considered handled
        for state in &mut arm_states {
            let Some(position) =
                state.refutable_paths.iter().position(|x| *x == main_path)
            else {
                continue;
            };

            assert!(state.refutable_paths.remove(position).is_some());
        }

        let mut free_arms = Vec::new();
        let mut arm_states_by_value = HashMap::<SwitchValue, Vec<_>>::new();

        for arm_state in arm_states {
            let pattern = arm_infos[arm_state.arm_info_index]
                .reftuable_pattern
                .get_from_path(&main_path)
                .unwrap();

            let Some(value) = pattern.get_conditional_value(self.table) else {
                for states in arm_states_by_value.values_mut() {
                    states.push(arm_state.clone());
                }

                free_arms.push(arm_state);
                continue;
            };

            // append the arm state to the corresponding value
            match arm_states_by_value.entry(value) {
                Entry::Occupied(mut occupied_entry) => {
                    occupied_entry.get_mut().push(arm_state.clone());
                }
                Entry::Vacant(vacant_entry) => {
                    // add the previous free arms and this arm state
                    vacant_entry.insert(free_arms.clone()).push(arm_state);
                }
            };
        }

        // at least the main path must be preset
        assert!(!arm_states_by_value.is_empty());

        let (load_address, load_ty, main_leaf_refutable) = self
            .get_address_and_type_from_path(
                main_root_refutable,
                &main_path,
                match_info.address.clone(),
                match_info.r#type.clone(),
            )?;

        let is_exhaustive = self.is_exhaustive(
            arm_states_by_value.keys().copied(),
            main_leaf_refutable,
            &load_ty,
        );

        let otherwise_block =
            (!free_arms.is_empty() && !is_exhaustive).then(|| {
                self.intermediate_representation.control_flow_graph.new_block()
            });

        /*
        if is not exhaustive:
            if there is a free arm:
                use those free arms as `otherwise` case
            else:
                report as non-exhaustive
         */

        match main_leaf_refutable {
            Refutable::Boolean(_) => {
                assert_eq!(load_ty, Type::Primitive(Primitive::Bool));
                assert!(arm_states_by_value.len() <= 2);

                let load_value = self.create_register_assignmnet(
                    Assignment::Load(Load { address: load_address }),
                    match_info.span.clone(),
                );

                match arm_states_by_value.len() {
                    1 => {
                        assert!(!is_exhaustive);

                        let (value, inner_arm_states) =
                            arm_states_by_value.into_iter().next().unwrap();

                        let block_for_arms = self
                            .intermediate_representation
                            .control_flow_graph
                            .new_block();

                        let alternative_block =
                            otherwise_block.unwrap_or_else(|| {
                                // report the non-exhaustive error
                                non_exhaustives.push(NonExhaustive {
                                    path: main_path,
                                    missing_value: MissingValue::Known(vec![
                                        SwitchValue::Positive(u64::from(
                                            value == SwitchValue::Positive(0),
                                        )),
                                    ]),
                                });

                                // use unreachable block
                                match_info.non_exhaustive_block_id
                            });

                        let (true_block, false_block) =
                            if value == SwitchValue::Positive(1) {
                                (block_for_arms, alternative_block)
                            } else {
                                (alternative_block, block_for_arms)
                            };

                        self.intermediate_representation
                            .control_flow_graph
                            .insert_terminator(
                                self.current_block_id,
                                Terminator::Jump(Jump::Conditional(
                                    ConditionalJump {
                                        condition: Value::Register(load_value),
                                        true_target: true_block,
                                        false_target: false_block,
                                    },
                                )),
                            );

                        let current_block_id = self.current_block_id;

                        // bind the arms
                        self.current_block_id = block_for_arms;
                        self.handle_match_arms(
                            match_info,
                            non_exhaustives,
                            arm_infos,
                            inner_arm_states,
                            handler,
                        )?;

                        self.current_block_id = current_block_id;
                    }

                    2 => {
                        // must be exhaustive
                        assert!(is_exhaustive);
                        assert!(otherwise_block.is_none());

                        let true_block_id = self
                            .intermediate_representation
                            .control_flow_graph
                            .new_block();
                        let false_block_id = self
                            .intermediate_representation
                            .control_flow_graph
                            .new_block();

                        let mut arm_states_iter =
                            arm_states_by_value.into_iter();

                        // extract 2 arms
                        let first = arm_states_iter.next().unwrap();
                        let second = arm_states_iter.next().unwrap();

                        assert!(arm_states_iter.next().is_none());

                        let (true_group, false_group) =
                            if first.0 == SwitchValue::Positive(1) {
                                (first.1, second.1)
                            } else {
                                (second.1, first.1)
                            };

                        self.intermediate_representation
                            .control_flow_graph
                            .insert_terminator(
                                self.current_block_id,
                                Terminator::Jump(Jump::Conditional(
                                    ConditionalJump {
                                        condition: Value::Register(load_value),
                                        true_target: true_block_id,
                                        false_target: false_block_id,
                                    },
                                )),
                            );

                        let current_block_id = self.current_block_id;

                        // bind the true arm
                        self.current_block_id = true_block_id;
                        self.handle_match_arms(
                            match_info,
                            non_exhaustives,
                            arm_infos,
                            true_group,
                            handler,
                        )?;

                        // bind the false arm
                        self.current_block_id = false_block_id;
                        self.handle_match_arms(
                            match_info,
                            non_exhaustives,
                            arm_infos,
                            false_group,
                            handler,
                        )?;

                        self.current_block_id = current_block_id;
                    }

                    x => panic!("invalid arm state count {x:#?}"),
                }
            }

            // both of the arms are integer comparison
            Refutable::Integer(_) | Refutable::Enum(_) => {
                let numeric_value = match main_leaf_refutable {
                    Refutable::Integer(_) => self.create_register_assignmnet(
                        Assignment::Load(Load { address: load_address }),
                        match_info.span.clone(),
                    ),

                    Refutable::Enum(pattern) => self
                        .create_register_assignmnet(
                            Assignment::VariantNumber(VariantNumber {
                                address: load_address,
                                enum_id: GlobalID::new(
                                    pattern.variant_id.target_id,
                                    self.table
                                        .get::<Parent>(pattern.variant_id)
                                        .parent
                                        .unwrap(),
                                ),
                            }),
                            match_info.span.clone(),
                        ),

                    _ => unreachable!(),
                };

                match (arm_states_by_value.len(), is_exhaustive) {
                    (0, _) => {
                        panic!("invalid value group {arm_states_by_value:#?}")
                    }

                    (1, true) => self.handle_match_arms(
                        match_info,
                        non_exhaustives,
                        arm_infos,
                        arm_states_by_value.into_iter().next().unwrap().1,
                        handler,
                    )?,

                    (2, true) | (1, false) => {
                        let mut iter = arm_states_by_value.into_iter();

                        let (first_value, first_arm_states) =
                            iter.next().unwrap();
                        let second = iter.next().map(|x| {
                            assert!(otherwise_block.is_none());

                            let second_binding_block_id = self
                                .intermediate_representation
                                .control_flow_graph
                                .new_block();

                            (second_binding_block_id, x.0, x.1)
                        });

                        assert!(iter.next().is_none());

                        let alternative_block = second.as_ref().map_or_else(
                            || {
                                otherwise_block.unwrap_or_else(|| {
                                    // report the non-exhaustive error
                                    non_exhaustives.push(NonExhaustive {
                                        path: main_path,
                                        missing_value: match main_leaf_refutable
                                        {
                                            Refutable::Integer(_) => {
                                                MissingValue::Unknown
                                            }
                                            Refutable::Enum(a) => {
                                                MissingValue::Known({
                                                    let enum_id = self
                                                        .table
                                                        .get::<Parent>(
                                                            a.variant_id,
                                                        )
                                                        .parent
                                                        .unwrap();

                                                    (0..self
                                                        .table
                                                        .get::<Member>(
                                                            GlobalID::new(
                                                                a.variant_id
                                                                    .target_id,
                                                                enum_id,
                                                            ),
                                                        )
                                                        .len())
                                                        .map(|x| SwitchValue::Positive(x as u64))
                                                        .filter(|x| {
                                                            *x != first_value
                                                        })
                                                        .collect()
                                                })
                                            }
                                            _ => unreachable!(),
                                        },
                                    });

                                    // use unreachable block
                                    match_info.non_exhaustive_block_id
                                })
                            },
                            |x| x.0,
                        );

                        let binding_block_id = self
                            .intermediate_representation
                            .control_flow_graph
                            .new_block();

                        let comparison_register = self
                            .create_register_assignmnet(
                                Assignment::Binary(Binary {
                                    lhs: Value::Register(numeric_value),
                                    rhs: Value::Literal(Literal::Numeric(
                                        Numeric {
                                            integer_string: first_value
                                                .to_string(),
                                            decimal_stirng: None,
                                            r#type: self.type_of_register(
                                                numeric_value,
                                                handler,
                                            )?,
                                            span: Some(match_info.span.clone()),
                                        },
                                    )),
                                    operator: BinaryOperator::Relational(
                                        RelationalOperator::Equal,
                                    ),
                                }),
                                match_info.span.clone(),
                            );

                        self.intermediate_representation
                            .control_flow_graph
                            .insert_terminator(
                                self.current_block_id,
                                Terminator::Jump(Jump::Conditional(
                                    ConditionalJump {
                                        condition: Value::Register(
                                            comparison_register,
                                        ),
                                        true_target: binding_block_id,
                                        false_target: alternative_block,
                                    },
                                )),
                            );

                        let current_block_id = self.current_block_id;

                        // bind the first arm
                        self.current_block_id = binding_block_id;
                        self.handle_match_arms(
                            match_info,
                            non_exhaustives,
                            arm_infos,
                            first_arm_states,
                            handler,
                        )?;

                        // bind the second arm (if any)
                        if let Some((binding_block_id, _, second_arm_states)) =
                            second
                        {
                            self.current_block_id = binding_block_id;
                            self.handle_match_arms(
                                match_info,
                                non_exhaustives,
                                arm_infos,
                                second_arm_states,
                                handler,
                            )?;
                        }

                        self.current_block_id = current_block_id;
                    }

                    _ => {
                        // use select inst
                        let groups = arm_states_by_value
                            .into_iter()
                            .map(|x| {
                                let block_id = self
                                    .intermediate_representation
                                    .control_flow_graph
                                    .new_block();

                                (block_id, x.0, x.1)
                            })
                            .collect::<Vec<_>>();

                        let otherwise = if is_exhaustive {
                            assert!(otherwise_block.is_none());
                            None
                        } else {
                            Some(otherwise_block.unwrap_or_else(|| {
                                // report the non-exhaustive error
                                non_exhaustives.push(NonExhaustive {
                                    path: main_path,
                                    missing_value: match main_leaf_refutable {
                                        Refutable::Integer(_) => {
                                            MissingValue::Unknown
                                        }
                                        Refutable::Enum(a) => {
                                            MissingValue::Known({
                                                let enum_id = self
                                                    .table
                                                    .get::<Parent>(a.variant_id)
                                                    .parent
                                                    .unwrap();

                                                (0..self
                                                    .table
                                                    .get::<Member>(
                                                        GlobalID::new(
                                                            a.variant_id
                                                                .target_id,
                                                            enum_id,
                                                        ),
                                                    )
                                                    .len())
                                                    .map(|x| {
                                                        SwitchValue::Positive(
                                                            x as u64,
                                                        )
                                                    })
                                                    .filter(|x| {
                                                        !groups.iter().any(
                                                            |(_, val, _)| {
                                                                *val == *x
                                                            },
                                                        )
                                                    })
                                                    .collect()
                                            })
                                        }
                                        _ => unreachable!(),
                                    },
                                });

                                // use unreachable block
                                match_info.non_exhaustive_block_id
                            }))
                        };

                        self.intermediate_representation
                            .control_flow_graph
                            .insert_terminator(
                                self.current_block_id,
                                Terminator::Jump(Jump::Switch(SwitchJump {
                                    integer: Value::Register(numeric_value),
                                    branches: groups
                                        .iter()
                                        .map(|(block_id, val, _)| {
                                            (*val, *block_id)
                                        })
                                        .collect(),
                                    otherwise,
                                })),
                            );

                        let current_block_id = self.current_block_id;

                        // bind the arms
                        for (block_id, _, arm_states) in groups {
                            self.current_block_id = block_id;
                            self.handle_match_arms(
                                match_info,
                                non_exhaustives,
                                arm_infos,
                                arm_states,
                                handler,
                            )?;
                        }

                        self.current_block_id = current_block_id;
                    }
                }
            }

            x => panic!("non refutable pattern {x:#?}"),
        }

        // bind the otherwise cases
        if let Some(otherwise_block) = otherwise_block {
            assert!(!free_arms.is_empty());

            let current_block_id = self.current_block_id;

            self.current_block_id = otherwise_block;
            self.handle_match_arms(
                match_info,
                non_exhaustives,
                arm_infos,
                free_arms,
                handler,
            )?;

            self.current_block_id = current_block_id;
        }

        Ok(())
    }

    fn bind_match_arm(
        &mut self,
        match_info: &MatchInfo,
        arm_info: &mut ArmInfo,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<(), Error> {
        // already bound
        if let Some(result) = &arm_info.binding_result {
            self.intermediate_representation
                .control_flow_graph
                .insert_terminator(
                    self.current_block_id,
                    Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                        target: result.entry_block_id,
                    })),
                );

            return Ok(());
        }

        let new_block =
            self.intermediate_representation.control_flow_graph.new_block();
        self.intermediate_representation.control_flow_graph.insert_terminator(
            self.current_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: new_block,
            })),
        );

        self.current_block_id = new_block;
        let starting_block_id = self.current_block_id;

        self.stack.push_scope(arm_info.scope_id);

        let _ = self.current_block_mut().add_instruction(
            Instruction::ScopePush(ScopePush(arm_info.scope_id)),
        );

        let mut name_binding_point = NameBindingPoint::default();
        self.insert_refutable_named_binding_point(
            &mut name_binding_point,
            &arm_info.reftuable_pattern,
            match_info.r#type,
            match_info.address.clone(),
            Some(match_info.address_span.clone()),
            match_info.qualifier,
            match_info.from_lvalue,
            self.stack.current_scope().scope_id(),
            handler,
        )?;

        // add the named binding point to the current scope
        self.stack
            .current_scope_mut()
            .add_named_binding_point(name_binding_point);

        let value = self.bind_value_or_error(arm_info.expression, handler)?;

        assert_eq!(
            self.stack.pop_scope().map(|x| x.scope_id()),
            Some(arm_info.scope_id),
        );
        let _ = self.current_block_mut().add_instruction(
            Instruction::ScopePop(ScopePop(arm_info.scope_id)),
        );

        self.intermediate_representation.control_flow_graph.insert_terminator(
            self.current_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: match_info.exit_block_id,
            })),
        );

        let ending_block_id = self.current_block_id;

        arm_info.binding_result = Some(ArmResult {
            entry_block_id: starting_block_id,
            end_block_id: ending_block_id,
            value,
        });

        self.current_block_id = starting_block_id;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ArmState {
    refutable_paths: VecDeque<Path>,
    arm_info_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArmResult {
    entry_block_id: ID<Block<infer::Model>>,
    end_block_id: ID<Block<infer::Model>>,
    value: Value<infer::Model>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ArmInfo<'a> {
    expression: &'a syntax_tree::expression::Expression,
    scope_id: ID<scope::Scope>,
    binding_result: Option<ArmResult>,
    reftuable_pattern: Refutable,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MatchInfo<'a> {
    address: Address<infer::Model>,
    address_span: Span,
    r#type: &'a Type<infer::Model>,
    qualifier: Qualifier,
    from_lvalue: bool,
    span: Span,
    exit_block_id: ID<Block<infer::Model>>,
    non_exhaustive_block_id: ID<Block<infer::Model>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MissingValue {
    Known(Vec<SwitchValue>),
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct NonExhaustive {
    path: Path,
    missing_value: MissingValue,
}

impl Bind<&syntax_tree::expression::Match> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Match,
        _config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let exit_block_id =
            self.intermediate_representation.control_flow_graph.new_block();

        let (address, qualifier, from_lvalue) = match self.bind(
            syntax_tree.parenthesized(),
            Config { target: Target::LValue },
            handler,
        ) {
            Ok(Expression::LValue(lvalue)) => {
                (lvalue.address, lvalue.qualifier, true)
            }

            Ok(Expression::RValue(value)) => {
                (
                    Address::Memory(Memory::Alloca(
                        self.create_alloca_with_value(
                            value,
                            self.stack.current_scope().scope_id(),
                            None,
                            syntax_tree.parenthesized().span(),
                            handler,
                        )?,
                    )),
                    Qualifier::Mutable, /* has the highest mutability */
                    false,
                )
            }

            Err(err) => match err {
                Error::Binding(semantic_error) => {
                    (
                        Address::Memory(Memory::Alloca({
                            let ty_inference = self
                                .create_type_inference(Constraint::All(false));
                            self.create_alloca_with_value(
                                Value::Literal(Literal::Error(
                                    literal::Error {
                                        r#type: Type::Inference(ty_inference),
                                        span: Some(semantic_error.0),
                                    },
                                )),
                                self.stack.current_scope().scope_id(),
                                None,
                                syntax_tree.parenthesized().span(),
                                handler,
                            )?
                        })),
                        Qualifier::Mutable, /* has the highest mutability */
                        false,
                    )
                }
                Error::Unrecoverable(abrupt_error) => {
                    return Err(Error::Unrecoverable(abrupt_error))
                }
            },
        };
        let ty = self
            .create_environment()
            .simplify(self.type_of_address(&address, handler)?)
            .map_err(|x| {
                x.report_overflow(|x| {
                    x.report_as_type_calculating_overflow(
                        syntax_tree.parenthesized().span(),
                        handler,
                    )
                })
            })?;

        let arm_count = syntax_tree
            .arms()
            .connected_list()
            .as_ref()
            .into_iter()
            .flat_map(ConnectedList::elements)
            .count();
        let scope_ids = if arm_count == 0 {
            Vec::new()
        } else {
            self.intermediate_representation
                .scope_tree
                .new_child_branch(
                    self.stack.current_scope().scope_id(),
                    NonZero::new(arm_count).unwrap(),
                )
                .unwrap()
        };

        let non_exhaustive_block_id =
            self.intermediate_representation.control_flow_graph.new_block();

        let mut arm_infos = syntax_tree
            .arms()
            .connected_list()
            .as_ref()
            .into_iter()
            .flat_map(ConnectedList::elements)
            .zip(scope_ids)
            .map(|(x, scope_id)| {
                let mut pat = self
                    .bind_pattern(&ty.result, x.refutable_pattern(), handler)?
                    .unwrap_or_else(|| {
                        Wildcard { span: x.refutable_pattern().span() }.into()
                    });

                Self::replace_refutable_in_tuple_pack(&mut pat, handler);

                Ok(ArmInfo {
                    expression: x.expression(),
                    scope_id,
                    binding_result: None,
                    reftuable_pattern: pat,
                })
            })
            .collect::<Result<Vec<_>, Abort>>()?;

        let arm_states = arm_infos
            .iter()
            .enumerate()
            .map(|(index, x)| ArmState {
                refutable_paths: Self::get_refutable_paths(
                    &x.reftuable_pattern,
                ),
                arm_info_index: index,
            })
            .collect::<Vec<_>>();

        let starting_block_id = self.current_block_id;
        let mut non_exhaustives = Vec::new();

        self.handle_match_arms(
            &MatchInfo {
                address,
                address_span: syntax_tree.parenthesized().span(),
                r#type: &ty.result,
                qualifier,
                from_lvalue,
                span: syntax_tree.span(),
                exit_block_id,
                non_exhaustive_block_id,
            },
            &mut non_exhaustives,
            &mut arm_infos,
            arm_states,
            handler,
        )?;

        self.intermediate_representation
            .control_flow_graph
            .insert_terminator(starting_block_id, Terminator::Panic);
        self.intermediate_representation
            .control_flow_graph
            .insert_terminator(non_exhaustive_block_id, Terminator::Panic);

        self.current_block_id = exit_block_id;

        // remove the match arms that are unreachable and report the error
        for unreachable in
            arm_infos.drain_filter(|x| x.binding_result.is_none())
        {
            handler.receive(Box::new(UnreachableMatchArm {
                match_arm_span: unreachable.expression.span(),
            }));
        }

        if arm_infos.is_empty() {
            assert!(!self.current_block().is_reachable());

            let ty = ty.result.reduce_reference();

            let is_inhabited = match ty {
                Type::Symbol(Symbol { id: enum_id, .. })
                    if {
                        *self.table.get::<SymbolKind>(*enum_id)
                            == SymbolKind::Enum
                    } =>
                {
                    self.table.get::<Member>(*enum_id).len() == 0
                }
                _ => false,
            };

            if !is_inhabited {
                handler.receive(Box::new(NonExhaustiveMatch {
                    match_expression_span: syntax_tree
                        .match_keyword()
                        .span
                        .join(&syntax_tree.parenthesized().span()),
                }));
            }

            Ok(Expression::RValue(Value::Literal(
                self.create_unreachable(syntax_tree.span()),
            )))
        } else {
            if !non_exhaustives.is_empty() {
                handler.receive(Box::new(NonExhaustiveMatch {
                    match_expression_span: syntax_tree
                        .match_keyword()
                        .span
                        .join(&syntax_tree.parenthesized().span()),
                }));
            }

            // no match arms reach the successor block
            let Some(first_reachable_arm_index) =
                arm_infos.iter().position(|x| {
                    self.current_block().predecessors().contains(
                        &x.binding_result.as_ref().unwrap().end_block_id,
                    )
                })
            else {
                assert!(self.current_block().predecessors().is_empty());

                return Ok(Expression::RValue(Value::Literal(
                    self.create_unreachable(syntax_tree.span()),
                )));
            };
            let match_ty = self.type_of_value(
                &arm_infos[first_reachable_arm_index]
                    .binding_result
                    .as_ref()
                    .unwrap()
                    .value,
                handler,
            )?;

            // do type check for each match arm
            for arm in arm_infos.iter().skip(first_reachable_arm_index + 1) {
                let ty = self.type_of_value(
                    &arm.binding_result.as_ref().unwrap().value,
                    handler,
                )?;

                self.type_check(
                    &ty,
                    Expected::Known(match_ty.clone()),
                    arm.expression.span(),
                    handler,
                )?;
            }

            let incoming_values = arm_infos
                .into_iter()
                .filter(|x| {
                    self.current_block().predecessors().contains(
                        &x.binding_result.as_ref().unwrap().end_block_id,
                    )
                })
                .map(|x| {
                    let result = x.binding_result.unwrap();

                    (result.end_block_id, result.value)
                })
                .collect::<HashMap<_, _>>();

            Ok(Expression::RValue(Value::Register(
                self.create_register_assignmnet(
                    Assignment::Phi(Phi { incoming_values, r#type: match_ty }),
                    syntax_tree.span(),
                ),
            )))
        }
    }
}

#[cfg(test)]
mod test;
