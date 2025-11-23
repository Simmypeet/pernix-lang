use std::{
    collections::{VecDeque, hash_map::Entry},
    num::NonZero,
};

use drain_filter_polyfill::VecExt;
use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_hash::HashMap;
use pernixc_ir::{
    address::{Address, Memory},
    control_flow_graph::Block,
    instruction::{
        ConditionalJump, Jump, SwitchJump, SwitchValue, Terminator,
        UnconditionalJump,
    },
    pattern::{NameBindingPoint, Refutable, Wildcard},
    scope::Scope,
    value::{
        Value,
        literal::{self, Literal, Numeric},
        register::{
            Assignment, Binary, BinaryOperator, Phi, RelationalOperator,
            VariantNumber, load::Load,
        },
    },
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{Kind, get_kind},
    member::get_members,
    parent::get_parent,
    variant_declaration_order::get_variant_declaration_order,
};
use pernixc_syntax::expression::block::Group;
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::Symbol,
    r#type::{Primitive, Qualifier, Type},
};

use crate::{
    bind::{Bind, Expression, Guidance, expression::group::BindGroupTarget},
    binder::{Binder, Error},
    diagnostic::{
        Diagnostic, FoundPackTuplePatternInMatchArmPattern, NonExhaustiveMatch,
        UnreachableMatchArm,
    },
    infer::constraint,
    pattern::{
        insert_name_binding,
        path::{Path, PathAccess},
    },
};

// TODO: this module is such a mess, needs to be refactored.

async fn get_conditional_value(
    refutable: &Refutable,
    engine: &TrackedEngine,
) -> Option<SwitchValue> {
    match refutable {
        Refutable::Boolean(boolean) => {
            Some(SwitchValue::Positive(boolean.value.into()))
        }
        Refutable::Integer(integer) => Some(integer.value),
        Refutable::Enum(variant) => Some(SwitchValue::Positive(
            engine.get_variant_declaration_order(variant.variant_id).await
                as u64,
        )),

        _ => None,
    }
}

impl Binder<'_> {
    #[allow(clippy::cast_sign_loss)]
    async fn is_exhaustive<T: IntoIterator<Item = SwitchValue>>(
        &self,
        values: T,
        pattern: &Refutable,
        ty: &Type,
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

                let member = self.engine().get_members(*enum_id).await;
                let mut variant_handled = bit_vec::BitVec::from_elem(
                    member.member_ids_by_name.len(),
                    false,
                );

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

    async fn get_alternative_block<F: Fn(&SwitchValue) -> bool>(
        &mut self,
        otherwise_block: Option<ID<Block>>,
        main_path: Path,
        main_leaf_refutable: &Refutable,
        filter: F,
        match_info: &MatchInfo,
        non_exhaustives: &mut Vec<NonExhaustive>,
    ) -> ID<Block> {
        if let Some(otherwise_block) = otherwise_block {
            return otherwise_block;
        }

        // report the non-exhaustive error
        non_exhaustives.push(NonExhaustive {
            path: main_path,
            missing_value: match main_leaf_refutable {
                Refutable::Integer(_) => MissingValue::Unknown,
                Refutable::Enum(a) => MissingValue::Known({
                    let enum_id =
                        self.engine().get_parent(a.variant_id).await.unwrap();

                    (0..self
                        .engine()
                        .get_members(Global::new(
                            a.variant_id.target_id,
                            enum_id,
                        ))
                        .await
                        .member_ids_by_name
                        .len())
                        .map(|x| SwitchValue::Positive(x as u64))
                        .filter(filter)
                        .collect()
                }),
                _ => unreachable!(),
            },
        });

        // use unreachable block
        match_info.non_exhaustive_block_id
    }

    #[allow(clippy::too_many_arguments, clippy::too_many_lines)]
    async fn handle_numeric_pattern(
        &mut self,
        arm_states_by_value: HashMap<SwitchValue, Vec<ArmState>>,
        load_address: Address,
        arm_infos: &mut [ArmInfo],
        match_info: &MatchInfo,
        main_path: Path,
        main_leaf_refutable: &Refutable,
        is_exhaustive: bool,
        non_exhaustives: &mut Vec<NonExhaustive>,
        otherwise_block: Option<ID<Block>>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), Error> {
        let numeric_value = match main_leaf_refutable {
            Refutable::Integer(_) => self.create_register_assignment(
                Assignment::Load(Load::new(load_address)),
                match_info.span,
            ),

            Refutable::Enum(pattern) => self.create_register_assignment(
                Assignment::VariantNumber(VariantNumber {
                    address: load_address,
                    enum_id: Global::new(
                        pattern.variant_id.target_id,
                        self.engine()
                            .get_parent(pattern.variant_id)
                            .await
                            .unwrap(),
                    ),
                }),
                match_info.span,
            ),

            _ => unreachable!(),
        };

        match (arm_states_by_value.len(), is_exhaustive) {
            (0, _) => {
                panic!("invalid value group {arm_states_by_value:#?}")
            }

            (1, true) => {
                Box::pin(self.handle_match_arms(
                    match_info,
                    non_exhaustives,
                    arm_infos,
                    arm_states_by_value.into_iter().next().unwrap().1,
                    handler,
                ))
                .await
            }

            (2, true) | (1, false) => {
                let mut iter = arm_states_by_value.into_iter();

                let (first_value, first_arm_states) = iter.next().unwrap();
                let second = iter.next().map(|x| {
                    assert!(otherwise_block.is_none());

                    let second_binding_block_id = self.new_block();

                    (second_binding_block_id, x.0, x.1)
                });

                assert!(iter.next().is_none());

                let alternative_block = if let Some(x) = second.as_ref() {
                    x.0
                } else {
                    self.get_alternative_block(
                        otherwise_block,
                        main_path,
                        main_leaf_refutable,
                        |x| *x != first_value,
                        match_info,
                        non_exhaustives,
                    )
                    .await
                };

                let binding_block_id = self.new_block();

                let comparison_register = self.create_register_assignment(
                    Assignment::Binary(Binary {
                        lhs: Value::Register(numeric_value),
                        rhs: Value::Literal(Literal::Numeric(Numeric {
                            integer_string: first_value.to_string().into(),
                            decimal_string: None,
                            r#type: self
                                .type_of_register(numeric_value, handler)
                                .await?,
                            span: Some(match_info.span),
                        })),
                        operator: BinaryOperator::Relational(
                            RelationalOperator::Equal,
                        ),
                    }),
                    match_info.span,
                );

                self.insert_terminator(Terminator::Jump(Jump::Conditional(
                    ConditionalJump {
                        condition: Value::Register(comparison_register),
                        true_target: binding_block_id,
                        false_target: alternative_block,
                    },
                )));

                let current_block_id = self.current_block_id();

                // bind the first arm
                self.set_current_block_id(binding_block_id);

                Box::pin(self.handle_match_arms(
                    match_info,
                    non_exhaustives,
                    arm_infos,
                    first_arm_states,
                    handler,
                ))
                .await?;

                // bind the second arm (if any)
                if let Some((binding_block_id, _, second_arm_states)) = second {
                    self.set_current_block_id(binding_block_id);
                    Box::pin(self.handle_match_arms(
                        match_info,
                        non_exhaustives,
                        arm_infos,
                        second_arm_states,
                        handler,
                    ))
                    .await?;
                }

                self.set_current_block_id(current_block_id);

                Ok(())
            }

            _ => {
                // use select inst
                let groups = arm_states_by_value
                    .into_iter()
                    .map(|x| {
                        let block_id = self.new_block();

                        (block_id, x.0, x.1)
                    })
                    .collect::<Vec<_>>();

                let otherwise = if is_exhaustive {
                    assert!(otherwise_block.is_none());
                    None
                } else {
                    Some(
                        self.get_alternative_block(
                            otherwise_block,
                            main_path,
                            main_leaf_refutable,
                            |x| !groups.iter().any(|(_, val, _)| *val == *x),
                            match_info,
                            non_exhaustives,
                        )
                        .await,
                    )
                };

                self.insert_terminator(Terminator::Jump(Jump::Switch(
                    SwitchJump {
                        integer: Value::Register(numeric_value),
                        branches: groups
                            .iter()
                            .map(|(block_id, val, _)| (*val, *block_id))
                            .collect(),
                        otherwise,
                    },
                )));

                let current_block_id = self.current_block_id();

                // bind the arms
                for (block_id, _, arm_states) in groups {
                    self.set_current_block_id(block_id);

                    Box::pin(self.handle_match_arms(
                        match_info,
                        non_exhaustives,
                        arm_infos,
                        arm_states,
                        handler,
                    ))
                    .await?;
                }

                self.set_current_block_id(current_block_id);

                Ok(())
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    async fn handle_boolean_pattern(
        &mut self,
        arm_states_by_value: HashMap<SwitchValue, Vec<ArmState>>,
        load_ty: Type,
        load_address: Address,
        arm_infos: &mut [ArmInfo],
        match_info: &MatchInfo,
        main_path: Path,
        is_exhaustive: bool,
        non_exhaustives: &mut Vec<NonExhaustive>,
        otherwise_block: Option<ID<Block>>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), Error> {
        assert_eq!(load_ty, Type::Primitive(Primitive::Bool));
        assert!(arm_states_by_value.len() <= 2);

        let load_value = self.create_register_assignment(
            Assignment::Load(Load::new(load_address)),
            match_info.span,
        );

        match arm_states_by_value.len() {
            1 => {
                assert!(!is_exhaustive);

                let (value, inner_arm_states) =
                    arm_states_by_value.into_iter().next().unwrap();

                let block_for_arms = self.new_block();

                let alternative_block = otherwise_block.unwrap_or_else(|| {
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

                self.insert_terminator(Terminator::Jump(Jump::Conditional(
                    ConditionalJump {
                        condition: Value::Register(load_value),
                        true_target: true_block,
                        false_target: false_block,
                    },
                )));

                let current_block_id = self.current_block_id();

                // bind the arms
                self.set_current_block_id(block_for_arms);

                Box::pin(self.handle_match_arms(
                    match_info,
                    non_exhaustives,
                    arm_infos,
                    inner_arm_states,
                    handler,
                ))
                .await?;

                self.set_current_block_id(current_block_id);
            }

            2 => {
                // must be exhaustive
                assert!(is_exhaustive);
                assert!(otherwise_block.is_none());

                let true_block_id = self.new_block();
                let false_block_id = self.new_block();

                let mut arm_states_iter = arm_states_by_value.into_iter();

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

                self.insert_terminator(Terminator::Jump(Jump::Conditional(
                    ConditionalJump {
                        condition: Value::Register(load_value),
                        true_target: true_block_id,
                        false_target: false_block_id,
                    },
                )));

                let current_block_id = self.current_block_id();

                // bind the true arm
                self.set_current_block_id(true_block_id);

                Box::pin(self.handle_match_arms(
                    match_info,
                    non_exhaustives,
                    arm_infos,
                    true_group,
                    handler,
                ))
                .await?;

                // bind the false arm
                self.set_current_block_id(false_block_id);
                Box::pin(self.handle_match_arms(
                    match_info,
                    non_exhaustives,
                    arm_infos,
                    false_group,
                    handler,
                ))
                .await?;

                self.set_current_block_id(current_block_id);
            }

            x => panic!("invalid arm state count {x:#?}"),
        }

        Ok(())
    }

    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    async fn handle_match_arms(
        &mut self,
        match_info: &MatchInfo,
        non_exhaustives: &mut Vec<NonExhaustive>,
        arm_infos: &mut [ArmInfo],
        mut arm_states: Vec<ArmState>,
        handler: &dyn Handler<Diagnostic>,
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
                        arm_infos[arm_states.first().unwrap().arm_info_index]
                            .refutable_pattern
                            .clone(),
                    )
                },
            )
        else {
            // the rest of the arms are unreachable
            return Box::pin(self.bind_match_arm(
                match_info,
                &mut arm_infos[arm_states.first().unwrap().arm_info_index],
                handler,
            ))
            .await;
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
        let mut arm_states_by_value = HashMap::<SwitchValue, Vec<_>>::default();

        for arm_state in arm_states {
            let pattern = main_path
                .get_sub_refutable_from_path(
                    &arm_infos[arm_state.arm_info_index].refutable_pattern,
                )
                .unwrap();

            let Some(value) =
                get_conditional_value(pattern, self.engine()).await
            else {
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
            }
        }

        // at least the main path must be preset
        assert!(!arm_states_by_value.is_empty());

        let PathAccess {
            address: load_address,
            r#type: load_ty,
            pattern: main_leaf_refutable,
        } = self
            .access_path_in_pattern(
                &main_root_refutable,
                match_info.address.clone(),
                match_info.address_ty.clone(),
                &main_path,
            )
            .await?;

        let is_exhaustive = self
            .is_exhaustive(
                arm_states_by_value.keys().copied(),
                main_leaf_refutable,
                &load_ty,
            )
            .await;

        let otherwise_block =
            (!free_arms.is_empty() && !is_exhaustive).then(|| self.new_block());

        /*
        if is not exhaustive:
            if there is a free arm:
                use those free arms as `otherwise` case
            else:
                report as non-exhaustive
         */

        match main_leaf_refutable {
            Refutable::Boolean(_) => {
                self.handle_boolean_pattern(
                    arm_states_by_value,
                    load_ty,
                    load_address,
                    arm_infos,
                    match_info,
                    main_path,
                    is_exhaustive,
                    non_exhaustives,
                    otherwise_block,
                    handler,
                )
                .await?;
            }

            // both of the arms are integer comparison
            Refutable::Integer(_) | Refutable::Enum(_) => {
                self.handle_numeric_pattern(
                    arm_states_by_value,
                    load_address,
                    arm_infos,
                    match_info,
                    main_path,
                    main_leaf_refutable,
                    is_exhaustive,
                    non_exhaustives,
                    otherwise_block,
                    handler,
                )
                .await?;
            }

            x => panic!("non refutable pattern {x:#?}"),
        }

        // bind the otherwise cases
        if let Some(otherwise_block) = otherwise_block {
            assert!(!free_arms.is_empty());

            let current_block_id = self.current_block_id();
            self.set_current_block_id(otherwise_block);

            Box::pin(self.handle_match_arms(
                match_info,
                non_exhaustives,
                arm_infos,
                free_arms,
                handler,
            ))
            .await?;

            self.set_current_block_id(current_block_id);
        }

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    async fn bind_match_arm(
        &mut self,
        match_info: &MatchInfo,
        arm_info: &mut ArmInfo,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), Error> {
        // already bound
        if let Some(result) = &arm_info.binding_result {
            self.insert_terminator(Terminator::Jump(Jump::Unconditional(
                UnconditionalJump { target: result.entry_block_id },
            )));

            return Ok(());
        }

        let new_block = self.new_block();
        self.insert_terminator(Terminator::Jump(Jump::Unconditional(
            UnconditionalJump { target: new_block },
        )));

        self.set_current_block_id(new_block);
        let starting_block_id = self.current_block_id();

        self.push_scope_with(arm_info.scope_id, false);

        let mut name_binding_point = NameBindingPoint::default();
        self.insert_name_binding_point(
            &mut name_binding_point,
            &arm_info.refutable_pattern,
            &match_info.address_ty,
            match_info.address.clone(),
            match_info.qualifier,
            &insert_name_binding::Config {
                must_copy: match_info.from_lvalue,
                scope_id: self.stack().current_scope().scope_id(),
                address_span: Some(match_info.address_span),
            },
            handler,
        )
        .await?;

        // add the named binding point to the current scope
        self.add_named_binding_point(name_binding_point);

        let inner_scope_id = self.new_child_branch(NonZero::new(1).unwrap())[0];
        let (value, _) = Box::pin(self.bind_group(
            &arm_info.expression,
            inner_scope_id,
            match_info.bind_group_target(),
            handler,
        ))
        .await?;

        self.pop_scope(arm_info.scope_id);

        self.insert_terminator(Terminator::Jump(Jump::Unconditional(
            UnconditionalJump { target: match_info.exit_block_id },
        )));

        let ending_block_id = self.current_block_id();

        arm_info.binding_result = Some(ArmResult {
            entry_block_id: starting_block_id,
            end_block_id: ending_block_id,
            value,
        });

        self.set_current_block_id(starting_block_id);

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
    entry_block_id: ID<Block>,
    end_block_id: ID<Block>,
    value: Value,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ArmInfo {
    expression: Group,
    scope_id: ID<Scope>,
    binding_result: Option<ArmResult>,
    refutable_pattern: Refutable,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MatchInfo {
    address: Address,
    address_span: RelativeSpan,
    address_ty: Type,
    bind_group_target: Option<Type>,
    qualifier: Qualifier,
    from_lvalue: bool,
    span: RelativeSpan,
    exit_block_id: ID<Block>,
    non_exhaustive_block_id: ID<Block>,
}

impl MatchInfo {
    #[must_use]
    const fn bind_group_target(&self) -> BindGroupTarget<'_> {
        match &self.bind_group_target {
            Some(ty) => BindGroupTarget::Expression(ty),
            None => BindGroupTarget::Statement,
        }
    }
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

fn replace_refutable_in_tuple_pack_internal(
    reftuable: &mut Refutable,
    in_tuple_pack: bool,
    handler: &dyn Handler<Diagnostic>,
) {
    match reftuable {
        pattern @ (Refutable::Boolean(_)
        | Refutable::Integer(_)
        | Refutable::Enum(_)) => {
            if in_tuple_pack {
                let span = pattern.span();
                *pattern = Refutable::Wildcard(Wildcard { span });

                handler.receive(
                    Diagnostic::FoundPackTuplePatternInMatchArmPattern(
                        FoundPackTuplePatternInMatchArmPattern {
                            pattern_span: span,
                        },
                    ),
                );
            }
        }

        Refutable::Wildcard(_) | Refutable::Named(_) => {}

        Refutable::Tuple(tuple) => {
            for pat in &mut tuple.elements {
                replace_refutable_in_tuple_pack_internal(
                    &mut pat.pattern,
                    pat.is_packed || in_tuple_pack,
                    handler,
                );
            }
        }
        Refutable::Structural(structural) => {
            for field in structural.patterns_by_field_id.values_mut() {
                replace_refutable_in_tuple_pack_internal(
                    field,
                    in_tuple_pack,
                    handler,
                );
            }
        }
    }
}

pub(super) fn replace_refutable_in_tuple_pack(
    reftuable: &mut Refutable,
    handler: &dyn Handler<Diagnostic>,
) {
    replace_refutable_in_tuple_pack_internal(reftuable, false, handler);
}

impl Bind<&pernixc_syntax::expression::block::Match> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::block::Match,
        config: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let (Some(binary), Some(match_kw)) =
            (syntax_tree.binary(), syntax_tree.match_keyword())
        else {
            return Err(Error::Binding(crate::binder::BindingError(
                syntax_tree.span(),
            )));
        };

        let bind_group_target = match config {
            Guidance::Expression(_) => {
                let expected = Type::Inference(
                    self.create_type_inference(constraint::Type::All(true)),
                );

                Some(expected)
            }
            Guidance::Statement => None,
        };

        let exit_block_id = self.new_block();

        let (address, qualifier, from_lvalue) = match self
            .bind(&binary, &Guidance::Expression(None), handler)
            .await
        {
            Ok(Expression::LValue(lvalue)) => {
                (lvalue.address, lvalue.qualifier, true)
            }

            Ok(Expression::RValue(value)) => {
                (
                    Address::Memory(Memory::Alloca(
                        self.create_alloca_with_value(
                            value,
                            self.stack().current_scope().scope_id(),
                            None,
                            binary.span(),
                            handler,
                        )
                        .await?,
                    )),
                    Qualifier::Mutable, /* has the highest mutability */
                    false,
                )
            }

            Err(err) => match err {
                Error::Binding(semantic_error) => {
                    (
                        Address::Memory(Memory::Alloca({
                            let ty_inference = self.create_type_inference(
                                constraint::Type::All(true),
                            );

                            self.create_alloca_with_value(
                                Value::Literal(Literal::Error(
                                    literal::Error {
                                        r#type: Type::Inference(ty_inference),
                                        span: Some(semantic_error.0),
                                    },
                                )),
                                self.stack().current_scope().scope_id(),
                                None,
                                binary.span(),
                                handler,
                            )
                            .await?
                        })),
                        Qualifier::Mutable, /* has the highest mutability */
                        false,
                    )
                }
                Error::Unrecoverable(abrupt_error) => {
                    return Err(Error::Unrecoverable(abrupt_error));
                }
            },
        };

        let address_ty = self.type_of_address(&address, handler).await?;
        let mut arms = Vec::new();

        if let Some(syn) = syntax_tree.body() {
            arms.extend(
                syn.arms().filter_map(|x| x.into_line().ok()).filter_map(|x| {
                    let group = x.group()?;
                    let pattern = x.refutable_pattern()?;

                    Some((group, pattern))
                }),
            );
        }

        let scope_ids = if arms.is_empty() {
            Vec::new()
        } else {
            self.new_child_branch(NonZero::new(arms.len()).unwrap())
        };

        let non_exhaustive_block_id = self.new_block();

        let mut arm_infos = Vec::with_capacity(arms.len());

        for ((group, pattern), scope_id) in arms.into_iter().zip(scope_ids) {
            let mut pat = self
                .bind_pattern(&pattern, &address_ty, handler)
                .await?
                .unwrap_or_else(|| Wildcard { span: pattern.span() }.into());

            replace_refutable_in_tuple_pack(&mut pat, handler);

            arm_infos.push(ArmInfo {
                expression: group,
                scope_id,
                binding_result: None,
                refutable_pattern: pat,
            });
        }

        let arm_states = arm_infos
            .iter()
            .enumerate()
            .map(|(index, x)| ArmState {
                refutable_paths: Path::get_refutable_paths(
                    &x.refutable_pattern,
                ),
                arm_info_index: index,
            })
            .collect::<Vec<_>>();

        let starting_block_id = self.current_block_id();
        let mut non_exhaustives = Vec::new();

        let match_info = MatchInfo {
            address,
            address_span: binary.span(),
            address_ty,
            qualifier,
            from_lvalue,
            span: syntax_tree.span(),
            exit_block_id,
            non_exhaustive_block_id,
            bind_group_target,
        };
        self.handle_match_arms(
            &match_info,
            &mut non_exhaustives,
            &mut arm_infos,
            arm_states,
            handler,
        )
        .await?;
        let address_ty = match_info.address_ty;

        self.set_current_block_id(starting_block_id);
        self.insert_terminator(Terminator::Panic);

        self.set_current_block_id(non_exhaustive_block_id);
        self.insert_terminator(Terminator::Panic);

        self.set_current_block_id(exit_block_id);

        // remove the match arms that are unreachable and report the error
        for unreachable in
            arm_infos.drain_filter(|x| x.binding_result.is_none())
        {
            handler.receive(Diagnostic::UnreachableMatchArm(
                UnreachableMatchArm {
                    match_arm_span: unreachable.expression.span(),
                },
            ));
        }

        if arm_infos.is_empty() {
            assert!(!self.current_block().is_reachable());

            let ty = address_ty.reduce_reference();

            let is_inhabited = match ty {
                Type::Symbol(Symbol { id: enum_id, .. })
                    if {
                        self.engine().get_kind(*enum_id).await == Kind::Enum
                    } =>
                {
                    self.engine()
                        .get_members(*enum_id)
                        .await
                        .member_ids_by_name
                        .is_empty()
                }
                _ => false,
            };

            if !is_inhabited {
                handler.receive(Diagnostic::NonExhaustiveMatch(
                    NonExhaustiveMatch {
                        match_expression_span: match_kw
                            .span
                            .join(&binary.span()),
                    },
                ));
            }

            Ok(Expression::RValue(Value::Literal(
                self.create_unreachable(syntax_tree.span()),
            )))
        } else {
            if !non_exhaustives.is_empty() {
                handler.receive(Diagnostic::NonExhaustiveMatch(
                    NonExhaustiveMatch {
                        match_expression_span: match_kw
                            .span
                            .join(&binary.span()),
                    },
                ));
            }

            // no match arms reach the successor block
            if !arm_infos.iter().any(|x| {
                self.current_block()
                    .predecessors()
                    .contains(&x.binding_result.as_ref().unwrap().end_block_id)
            }) {
                assert!(self.current_block().predecessors().is_empty());

                return Ok(Expression::RValue(Value::Literal(
                    self.create_unreachable(syntax_tree.span()),
                )));
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

            if incoming_values.len() == 1 {
                // only one arm reaches here
                let (_, value) = incoming_values.into_iter().next().unwrap();

                return Ok(Expression::RValue(value));
            }

            Ok(Expression::RValue(Value::Register(
                self.create_register_assignment(
                    Assignment::Phi(Phi {
                        incoming_values,
                        r#type: match_info
                            .bind_group_target
                            .unwrap_or_else(Type::unit),
                    }),
                    syntax_tree.span(),
                ),
            )))
        }
    }
}
