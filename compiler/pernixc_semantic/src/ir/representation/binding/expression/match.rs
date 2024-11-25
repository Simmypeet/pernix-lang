use std::{
    collections::{HashMap, VecDeque},
    num::NonZero,
    ops::Not,
};

use drain_filter_polyfill::VecExt;
use enum_as_inner::EnumAsInner;
use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{Bind, Config, Expression, Path, Target};
use crate::{
    arena::ID,
    error::{self, NonExhaustiveMatch, UnreachableMatchArm},
    ir::{
        address::{Address, Memory},
        control_flow_graph::Block,
        instruction::{
            ConditionalJump, Instruction, Jump, ScopePop, ScopePush,
            SelectJump, Terminator, UnconditionalJump,
        },
        pattern::{NameBindingPoint, Refutable, Wildcard},
        representation::binding::{
            infer::{self},
            Binder, Error, InternalError,
        },
        scope::Scope,
        value::{
            literal::{self, Literal, Numeric},
            register::{
                Assignment, Binary, BinaryOperator, Load, Phi,
                RelationalOperator, VariantNumber,
            },
            Value,
        },
    },
    symbol::{
        self,
        table::{self, representation::Index, resolution, Table},
        AdtID,
    },
    type_system::{
        self,
        instantiation::{self, Instantiation},
        simplify,
        term::{
            r#type::{self, Primitive, Qualifier, Type},
            Symbol, Term,
        },
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TupleCase {
    pub cases: Vec<Case>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StrcturalCase {
    pub cases_by_field_id: HashMap<ID<symbol::Field>, Case>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumCase {
    pub cases_by_variant_id: HashMap<ID<symbol::Variant>, Case>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BooleanCase {
    pub true_case_handled: bool,
    pub false_case_handled: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
#[allow(unused)]
pub enum Case {
    Unhandled,
    Handled,
    Boolean(BooleanCase),
    Enum(EnumCase),
    Tuple(TupleCase),
    Strctural(StrcturalCase),
}

/// Represents a single arm in the match expression.
#[derive(Debug, Clone, PartialEq, Eq)]
struct MatchArm<'a> {
    /// The bound refutable pattern from the syntax tree.
    pattern: Refutable,

    /// The expression that is executed if the pattern matches.
    expression: &'a syntax_tree::expression::Expression,

    /// The scope in which the refutable condition is declared.
    scope_id: ID<Scope>,

    /// List of paths to each refutable conditions in the pattern.
    ///
    /// This is used to determine which refutable conditions have been handled.
    refutable_paths: VecDeque<Path>,

    /// The result of the binding process.
    binding_result: Option<(ID<Block<infer::Model>>, Value<infer::Model>)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MatchInfo<'a> {
    address: Address<infer::Model>,
    r#type: &'a Type<infer::Model>,
    qualifier: Qualifier,
    from_lvalue: bool,
    span: Span,
}

impl Refutable {
    fn get_conditional_value(&self, table: &Table<impl table::State>) -> i128 {
        match self {
            Refutable::Boolean(boolean) => boolean.value as i128,
            Refutable::Integer(integer) => integer.value,
            Refutable::Enum(variant) => {
                let parent_enum_id =
                    table.get(variant.variant_id).unwrap().parent_enum_id();

                table
                    .get(parent_enum_id)
                    .unwrap()
                    .variant_declaration_order()
                    .iter()
                    .position(|x| *x == variant.variant_id)
                    .unwrap() as i128
            }

            x => panic!("pattern {x:#?} is not refutable"),
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Binder<'t, S, RO, TO>
{
    #[allow(unused)]
    fn handle_case(
        &mut self,
        case: &mut Case,
        refutable: &Refutable,
        ty: &Type<infer::Model>,
    ) {
        let ty = ty.reduce_reference();
        if let Type::Symbol(Symbol { id: AdtID::Enum(enum_id), .. }) = ty {
            let symbol = self.table.get(*enum_id).unwrap();
            if symbol.variant_declaration_order().is_empty() {
                *case = Case::Handled;
                return;
            }
        }

        match refutable {
            // mark the case as handled
            Refutable::Wildcard(_) | Refutable::Named(_) => {
                *case = Case::Handled;
            }

            // handle boolean case
            Refutable::Boolean(boolean) => match case {
                Case::Unhandled => {
                    *case = Case::Boolean(BooleanCase {
                        true_case_handled: boolean.value,
                        false_case_handled: !boolean.value,
                    });
                }
                Case::Handled => {
                    // do nothing
                }
                Case::Boolean(boolean_case) => {
                    boolean_case.true_case_handled |= boolean.value;
                    boolean_case.false_case_handled |= !boolean.value;

                    // if both cases are handled, mark the case as handled
                    if boolean_case.true_case_handled
                        && boolean_case.false_case_handled
                    {
                        *case = Case::Handled;
                    }
                }

                _ => panic!("unexpected case {case:#?}"),
            },

            Refutable::Integer(_) => {
                // currently, we don't support exhaustive integer matching
                // so we'll do nothing here
            }

            Refutable::Enum(en) => {
                let Type::Symbol(Symbol {
                    id: AdtID::Enum(enum_id),
                    generic_arguments,
                }) = ty
                else {
                    panic!("should've been an enum")
                };
                let enum_sym = self.table.get(*enum_id).unwrap();
                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments.clone(),
                    (*enum_id).into(),
                    &enum_sym.generic_declaration.parameters,
                )
                .unwrap();
                let variant_id = en.variant_id;
                let variant = self.table.get(variant_id).unwrap();

                match case {
                    Case::Unhandled => {
                        *case = Case::Enum(EnumCase {
                            cases_by_variant_id: enum_sym
                                .variant_declaration_order()
                                .iter()
                                .copied()
                                .map(|x| (x, Case::Unhandled))
                                .collect(),
                        });
                    }

                    // do nothing
                    Case::Handled => return,

                    Case::Enum(_) => {
                        // continue, the enum case has been created
                    }

                    _ => panic!("unexpected case {case:#?}"),
                }

                if let Some(ty) = &variant.associated_type {
                    let mut ty = Type::from_default_model(ty.clone());
                    instantiation::instantiate(&mut ty, &instantiation);

                    // handle the associated type
                    self.handle_case(
                        case.as_enum_mut()
                            .unwrap()
                            .cases_by_variant_id
                            .get_mut(&variant_id)
                            .unwrap(),
                        en.pattern.as_ref().unwrap(),
                        &ty,
                    );
                } else {
                    // just mark as unhandled
                    assert!(en.pattern.is_none());

                    *case
                        .as_enum_mut()
                        .unwrap()
                        .cases_by_variant_id
                        .get_mut(&variant_id)
                        .unwrap() = Case::Handled;
                }

                // check if all the cases are handled
                if case
                    .as_enum()
                    .unwrap()
                    .cases_by_variant_id
                    .values()
                    .all(|x| x == &Case::Handled)
                {
                    *case = Case::Handled;
                }
            }

            Refutable::Tuple(tuple) => {
                let Type::Tuple(tuple_ty) = ty else {
                    panic!("should've been a tuple")
                };
                let unpacked_position =
                    tuple.elements.iter().position(|x| !x.is_packed);

                match case {
                    Case::Unhandled => {
                        *case = Case::Tuple(TupleCase {
                            cases: (0..tuple_ty.elements.len())
                                .map(|_| Case::Unhandled)
                                .collect(),
                        });
                    }
                    Case::Tuple(_) => {
                        // continue, the tuple case has been created
                    }
                    Case::Handled => return,

                    _ => panic!("unexpected case {case:#?}"),
                }

                assert!(
                    tuple.elements.iter().filter(|x| x.is_packed).count() <= 1
                );

                if let Some(unpacked_position) = unpacked_position {
                    let start_range = 0..unpacked_position;
                    let end_range =
                        unpacked_position + 1..tuple_ty.elements.len();

                    let packed_type_range = unpacked_position
                        ..(tuple_ty.elements.len() - end_range.len());
                    let type_end_range =
                        packed_type_range.end..tuple_ty.elements.len();

                    assert_eq!(end_range.len(), type_end_range.len());

                    // check start case
                    for i in start_range {
                        self.handle_case(
                            case.as_tuple_mut()
                                .unwrap()
                                .cases
                                .get_mut(i)
                                .unwrap(),
                            &tuple.elements[i].pattern,
                            &tuple_ty.elements[i].term,
                        );
                    }

                    // check end case
                    for (i, j) in end_range.zip(type_end_range) {
                        self.handle_case(
                            case.as_tuple_mut()
                                .unwrap()
                                .cases
                                .get_mut(j)
                                .unwrap(),
                            &tuple.elements[i].pattern,
                            &tuple_ty.elements[j].term,
                        );
                    }

                    // make packed case handled since it's must be irrefutable
                    for i in packed_type_range {
                        *case
                            .as_tuple_mut()
                            .unwrap()
                            .cases
                            .get_mut(i)
                            .unwrap() = Case::Handled;
                    }
                } else {
                    for (i, element) in tuple.elements.iter().enumerate() {
                        self.handle_case(
                            case.as_tuple_mut()
                                .unwrap()
                                .cases
                                .get_mut(i)
                                .unwrap(),
                            &element.pattern,
                            &tuple_ty.elements[i].term,
                        );
                    }
                }

                // check if all the cases are handled
                if case
                    .as_tuple()
                    .unwrap()
                    .cases
                    .iter()
                    .all(|x| x == &Case::Handled)
                {
                    *case = Case::Handled;
                }
            }

            Refutable::Structural(structural) => {
                let Type::Symbol(Symbol {
                    id: AdtID::Struct(struct_id),
                    generic_arguments,
                }) = ty
                else {
                    panic!("should've been a struct")
                };
                let struct_sym = self.table.get(*struct_id).unwrap();
                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments.clone(),
                    (*struct_id).into(),
                    &struct_sym.generic_declaration.parameters,
                )
                .unwrap();

                match case {
                    Case::Unhandled => {
                        *case = Case::Strctural(StrcturalCase {
                            cases_by_field_id: struct_sym
                                .field_declaration_order()
                                .iter()
                                .copied()
                                .map(|x| (x, Case::Unhandled))
                                .collect(),
                        });
                    }

                    // do nothing
                    Case::Handled => return,

                    Case::Strctural(_) => {
                        // continue, the structural case has been created
                    }

                    _ => panic!("unexpected case {case:#?}"),
                }

                for (field_id, pattern) in &structural.patterns_by_field_id {
                    let field = struct_sym.fields().get(*field_id).unwrap();
                    let mut ty = Type::from_default_model(field.r#type.clone());
                    instantiation::instantiate(&mut ty, &instantiation);

                    self.handle_case(
                        case.as_strctural_mut()
                            .unwrap()
                            .cases_by_field_id
                            .get_mut(field_id)
                            .unwrap(),
                        pattern,
                        &ty,
                    );
                }

                // check if all the cases are handled
                if case
                    .as_strctural()
                    .unwrap()
                    .cases_by_field_id
                    .values()
                    .all(|x| x == &Case::Handled)
                {
                    *case = Case::Handled;
                }
            }
        }
    }

    fn bind_match_arm(
        &mut self,
        match_arm: &mut MatchArm,
        match_info: MatchInfo,
        match_exit_block_id: ID<Block<infer::Model>>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), InternalError> {
        self.stack.push_scope(match_arm.scope_id);

        let _ = self.current_block_mut().insert_instruction(
            Instruction::ScopePush(ScopePush(match_arm.scope_id)),
        );

        let mut name_binding_point = NameBindingPoint::default();
        self.insert_refutable_named_binding_point(
            &mut name_binding_point,
            &match_arm.pattern,
            match_info.r#type,
            match_info.address,
            match_info.qualifier,
            match_info.from_lvalue,
            handler,
        );

        // add the named binding point to the current scope
        self.stack
            .current_scope_mut()
            .add_named_binding_point(name_binding_point);

        let value = self.bind_value_or_error(match_arm.expression, handler)?;

        assert_eq!(
            self.stack.pop_scope().map(|x| x.scope_id()),
            Some(match_arm.scope_id),
        );
        let _ = self.current_block_mut().insert_instruction(
            Instruction::ScopePop(ScopePop(match_arm.scope_id)),
        );

        assert!(self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                    target: match_exit_block_id,
                })),
            )
            .map_or_else(|x| !x.is_invalid_block_id(), |()| true));

        match_arm.binding_result = Some((self.current_block_id, value));

        Ok(())
    }

    #[allow(clippy::cast_sign_loss)]
    fn is_exhaustive(
        &self,
        values: impl Iterator<Item = i128>,
        pattern: &Refutable,
        ty: &Type<infer::Model>,
    ) -> bool {
        match pattern {
            Refutable::Boolean(_) => {
                assert_eq!(*ty, Type::Primitive(Primitive::Bool));

                let mut true_found = false;
                let mut false_found = false;

                for value in values {
                    if value == 0 {
                        assert!(!false_found, "duplicated false value");
                        false_found = true;
                    } else if value == 1 {
                        assert!(!true_found, "duplicated true value");
                        true_found = true;
                    } else {
                        panic!("unexpected value {value}");
                    }
                }

                true_found && false_found
            }

            Refutable::Integer(_) => {
                // FIXME: implement this
                false
            }

            Refutable::Enum(_) => {
                let Type::Symbol(Symbol { id: AdtID::Enum(enum_id), .. }) = ty
                else {
                    panic!("unexpected type {ty:#?}");
                };

                let enum_sym = self.table.get(*enum_id).unwrap();
                let mut variant_handled = bit_vec::BitVec::from_elem(
                    enum_sym.variant_declaration_order().len(),
                    false,
                );

                for value in values {
                    assert!(
                        !variant_handled.get(value as usize).unwrap(),
                        "duplicate variant {value}"
                    );

                    variant_handled.set(value as usize, true);
                }

                variant_handled.all()
            }

            x => panic!("pattern {x:#?} is not refutable"),
        }
    }

    // bind a group of match arms that have the same refutable path
    #[allow(clippy::cast_lossless, clippy::too_many_lines)]
    fn bind_match_arm_groups(
        &mut self,
        match_arms: &mut [MatchArm],
        refutable_path: &Path,
        match_info: MatchInfo,
        continue_block_id: ID<Block<infer::Model>>,
        match_exit_block_id: ID<Block<infer::Model>>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), InternalError> {
        // switch match arms in the group such that the arms with the same
        // **refutable condition** are stored together. we'll do swapping
        // it would look like this: [a, b, c, b, d, c] -> [a, b, b, c, c, d]

        // FIXME: this algorithm is probably o(n^2), it can show a compiler
        // performance bottleneck when there are many match arms
        {
            let mut i = 0;

            // i + 1 is used here since the last arm won't have any other
            // match arms that have the same refutable condition
            while i + 1 < match_arms.len() {
                for j in i + 1..match_arms.len() {
                    if match_arms[i]
                        .pattern
                        .get_from_path(refutable_path)
                        .get_conditional_value(self.table)
                        != match_arms[j]
                            .pattern
                            .get_from_path(refutable_path)
                            .get_conditional_value(self.table)
                    {
                        continue;
                    }

                    // swap the arms
                    match_arms.swap(i + 1, j);

                    i += 1;
                }

                i += 1;
            }
        }

        // bind all the arms
        {
            let get_value_of = |index: usize| {
                match_arms
                    .get(index)
                    .unwrap()
                    .pattern
                    .get_from_path(refutable_path)
                    .get_conditional_value(self.table)
            };

            // vec of ranges of the same refutable condition
            let mut value_groups = Vec::new();

            let mut current_index = 0;
            let mut current_value = get_value_of(0);

            let mut i = 1;
            while i < match_arms.len() {
                let new_value = get_value_of(i);

                if new_value != current_value {
                    value_groups.push((current_index..i, current_value));
                    current_index = i;
                    current_value = new_value;
                }

                i += 1;
            }

            if current_index != match_arms.len() {
                value_groups
                    .push((current_index..match_arms.len(), current_value));
            }

            let (load_address, load_ty, pattern) = self
                .get_address_and_type_from_path(
                    &match_arms.first().unwrap().pattern,
                    refutable_path,
                    match_info.address.clone(),
                    match_info.r#type.clone(),
                );

            match pattern {
                Refutable::Boolean(_) => {
                    assert!(matches!(
                        load_ty,
                        Type::Primitive(Primitive::Bool)
                    ));

                    let load_value = self.create_register_assignmnet(
                        Assignment::Load(Load { address: load_address }),
                        Some(match_info.span.clone()),
                    );

                    if value_groups.len() == 1 {
                        let (range, value) = value_groups.pop().unwrap();

                        // if 0, negate the value
                        let should_negate = value == 0;
                        assert!(value == 0 || value == 1);

                        let binding_block_id = self
                            .intermediate_representation
                            .control_flow_graph
                            .new_block();

                        assert!(self
                            .intermediate_representation
                            .control_flow_graph
                            .insert_terminator(
                                self.current_block_id,
                                Terminator::Jump(Jump::Conditional(
                                    ConditionalJump {
                                        condition: Value::Register(load_value),
                                        true_target: if should_negate {
                                            continue_block_id
                                        } else {
                                            binding_block_id
                                        },
                                        false_target: if should_negate {
                                            binding_block_id
                                        } else {
                                            continue_block_id
                                        },
                                    },
                                )),
                            )
                            .map_or_else(
                                |x| !x.is_invalid_block_id(),
                                |()| true,
                            ));

                        let current_block_id = self.current_block_id;

                        // bind the arm
                        self.current_block_id = binding_block_id;
                        self.handle_match_arms(
                            match_arms[range].as_mut(),
                            match_info,
                            continue_block_id,
                            match_exit_block_id,
                            handler,
                        )?;
                        self.current_block_id = current_block_id;
                    } else {
                        assert_eq!(value_groups.len(), 2, "{value_groups:#?}");

                        let (true_range, false_range) =
                            if value_groups[0].1 == 1 {
                                (
                                    value_groups[0].0.clone(),
                                    value_groups[1].0.clone(),
                                )
                            } else {
                                (
                                    value_groups[1].0.clone(),
                                    value_groups[0].0.clone(),
                                )
                            };

                        let true_block_id = self
                            .intermediate_representation
                            .control_flow_graph
                            .new_block();
                        let false_block_id = self
                            .intermediate_representation
                            .control_flow_graph
                            .new_block();

                        assert!(self
                            .intermediate_representation
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
                            )
                            .map_or_else(
                                |x| !x.is_invalid_block_id(),
                                |()| true,
                            ));

                        let current_block_id = self.current_block_id;

                        // bind the true arm
                        self.current_block_id = true_block_id;
                        self.handle_match_arms(
                            match_arms[true_range].as_mut(),
                            match_info.clone(),
                            continue_block_id,
                            match_exit_block_id,
                            handler,
                        )?;

                        // bind the false arm
                        self.current_block_id = false_block_id;
                        self.handle_match_arms(
                            match_arms[false_range].as_mut(),
                            match_info,
                            continue_block_id,
                            match_exit_block_id,
                            handler,
                        )?;

                        self.current_block_id = current_block_id;
                    }
                }

                // both of the arms are integer comparison
                Refutable::Integer(_) | Refutable::Enum(_) => {
                    let numeric_value = match pattern {
                        Refutable::Integer(_) => self
                            .create_register_assignmnet(
                                Assignment::Load(Load {
                                    address: load_address,
                                }),
                                Some(match_info.span.clone()),
                            ),

                        Refutable::Enum(_) => self.create_register_assignmnet(
                            Assignment::VariantNumber(VariantNumber {
                                address: match_info.address.clone(),
                            }),
                            Some(match_info.span.clone()),
                        ),

                        _ => unreachable!(),
                    };

                    let is_exhaustive = self.is_exhaustive(
                        value_groups.iter().map(|x| x.1),
                        pattern,
                        &load_ty,
                    );

                    match (value_groups.len(), is_exhaustive) {
                        (0, _) => {
                            panic!("invalid value group {value_groups:#?}")
                        }

                        // simply bind without condition
                        (1, true) => self.handle_match_arms(
                            match_arms,
                            match_info,
                            continue_block_id,
                            match_exit_block_id,
                            handler,
                        )?,

                        // if (cond) { bind } else { continue }
                        (1, false) => {
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
                                                integer_string: value_groups
                                                    .first()
                                                    .unwrap()
                                                    .1
                                                    .to_string(),
                                                decimal_stirng: None,
                                                r#type: self.type_of_register(
                                                    numeric_value,
                                                )?,
                                                span: Some(
                                                    match_info.span.clone(),
                                                ),
                                            },
                                        )),
                                        operator: BinaryOperator::Relational(
                                            RelationalOperator::Equal,
                                        ),
                                    }),
                                    Some(match_info.span.clone()),
                                );

                            assert!(self
                                .intermediate_representation
                                .control_flow_graph
                                .insert_terminator(
                                    self.current_block_id,
                                    Terminator::Jump(Jump::Conditional(
                                        ConditionalJump {
                                            condition: Value::Register(
                                                comparison_register
                                            ),
                                            true_target: binding_block_id,
                                            false_target: continue_block_id,
                                        },
                                    )),
                                )
                                .map_or_else(
                                    |x| !x.is_invalid_block_id(),
                                    |()| true,
                                ));

                            let current_block_id = self.current_block_id;

                            // bind the arm
                            self.current_block_id = binding_block_id;
                            self.handle_match_arms(
                                match_arms,
                                match_info,
                                continue_block_id,
                                match_exit_block_id,
                                handler,
                            )?;
                            self.current_block_id = current_block_id;
                        }

                        // if (cond) { bind1 } else { bind2 }
                        (2, true) => {
                            let true_block_id = self
                                .intermediate_representation
                                .control_flow_graph
                                .new_block();
                            let false_block_id = self
                                .intermediate_representation
                                .control_flow_graph
                                .new_block();

                            let comparison_register = self
                                .create_register_assignmnet(
                                    Assignment::Binary(Binary {
                                        lhs: Value::Register(numeric_value),
                                        rhs: Value::Literal(Literal::Numeric(
                                            Numeric {
                                                integer_string: value_groups
                                                    .first()
                                                    .unwrap()
                                                    .1
                                                    .to_string(),
                                                decimal_stirng: None,
                                                r#type: self.type_of_register(
                                                    numeric_value,
                                                )?,
                                                span: Some(
                                                    match_info.span.clone(),
                                                ),
                                            },
                                        )),
                                        operator: BinaryOperator::Relational(
                                            RelationalOperator::Equal,
                                        ),
                                    }),
                                    Some(match_info.span.clone()),
                                );

                            assert!(self
                                .intermediate_representation
                                .control_flow_graph
                                .insert_terminator(
                                    self.current_block_id,
                                    Terminator::Jump(Jump::Conditional(
                                        ConditionalJump {
                                            condition: Value::Register(
                                                comparison_register
                                            ),
                                            true_target: true_block_id,
                                            false_target: false_block_id,
                                        },
                                    )),
                                )
                                .map_or_else(
                                    |x| !x.is_invalid_block_id(),
                                    |()| true,
                                ));

                            let current_block_id = self.current_block_id;

                            // bind the true arm
                            self.current_block_id = true_block_id;
                            self.handle_match_arms(
                                match_arms[value_groups[0].0.clone()].as_mut(),
                                match_info.clone(),
                                continue_block_id,
                                match_exit_block_id,
                                handler,
                            )?;

                            // bind the false arm
                            self.current_block_id = false_block_id;
                            self.handle_match_arms(
                                match_arms[value_groups[1].0.clone()].as_mut(),
                                match_info,
                                continue_block_id,
                                match_exit_block_id,
                                handler,
                            )?;

                            self.current_block_id = current_block_id;
                        }

                        // select inst
                        _ => {
                            // generate the binding block for each value
                            let block_ids = (0..value_groups.len())
                                .map(|_| {
                                    self.intermediate_representation
                                        .control_flow_graph
                                        .new_block()
                                })
                                .collect::<Vec<_>>();

                            // create select inst
                            assert!(self
                                .intermediate_representation
                                .control_flow_graph
                                .insert_terminator(
                                    self.current_block_id,
                                    Terminator::Jump(Jump::Select(
                                        SelectJump {
                                            integer: Value::Register(
                                                numeric_value
                                            ),
                                            branches: value_groups
                                                .iter()
                                                .enumerate()
                                                .map(|(idx, x)| (
                                                    x.1,
                                                    block_ids[idx]
                                                ))
                                                .collect(),
                                            otherwise: is_exhaustive
                                                .not()
                                                .then_some(continue_block_id),
                                        }
                                    ))
                                )
                                .map_or_else(
                                    |x| !x.is_invalid_block_id(),
                                    |()| true,
                                ));

                            let current_block_id = self.current_block_id;

                            // bind the arms
                            for (idx, (range, _)) in
                                value_groups.into_iter().enumerate()
                            {
                                self.current_block_id = block_ids[idx];
                                self.handle_match_arms(
                                    match_arms[range].as_mut(),
                                    match_info.clone(),
                                    continue_block_id,
                                    match_exit_block_id,
                                    handler,
                                )?;
                            }

                            self.current_block_id = current_block_id;
                        }
                    }
                }

                x => panic!("pattern {x:#?} is not refutable"),
            }
        }

        Ok(())
    }

    #[allow(clippy::cast_lossless, clippy::too_many_lines)]
    fn handle_match_arms(
        &mut self,
        match_arms: &mut [MatchArm],
        match_info: MatchInfo,
        continue_block_id: ID<Block<infer::Model>>,
        match_exit_block_id: ID<Block<infer::Model>>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), InternalError> {
        // no more arms to bind
        if match_arms.is_empty() {
            return Ok(());
        }

        // pick the path from the arm that is declared first

        // NOTE: the rest of the arms are unreachable, we can issue a warning
        // here
        let Some(main_path) =
            match_arms.first_mut().unwrap().refutable_paths.pop_front()
        else {
            return self.bind_match_arm(
                match_arms.first_mut().unwrap(),
                match_info,
                match_exit_block_id,
                handler,
            );
        };

        // find the match arms that have the same refutable paths as the main
        // path
        let mut including_end = 1;
        while including_end < match_arms.len() {
            // have the matching path
            let Some(position) = match_arms[including_end]
                .refutable_paths
                .iter()
                .position(|x| *x == main_path)
            else {
                // stop there, the rest of the arms will be handled in the next
                // iteration
                break;
            };

            // remove the path, mark it as handled
            assert!(match_arms[including_end]
                .refutable_paths
                .remove(position)
                .is_some());

            including_end += 1;
        }

        let next_block_id = (including_end != match_arms.len()).then(|| {
            self.intermediate_representation.control_flow_graph.new_block()
        });

        // 0..including_end is the range of match arm that will test a condition
        // to the same **path**
        self.bind_match_arm_groups(
            &mut match_arms[..including_end],
            &main_path,
            match_info.clone(),
            next_block_id.unwrap_or(continue_block_id),
            match_exit_block_id,
            handler,
        )?;

        if including_end == match_arms.len() {
            Ok(())
        } else {
            self.current_block_id = next_block_id.unwrap();

            self.handle_match_arms(
                &mut match_arms[including_end..],
                match_info,
                continue_block_id,
                match_exit_block_id,
                handler,
            )
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Match> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Match,
        _config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let successor_block_id =
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
                        self.create_alloca_with_value(value),
                    )),
                    Qualifier::Mutable, /* has the highest mutability */
                    false,
                )
            }

            Err(err) => match err {
                Error::Semantic(semantic_error) => {
                    (
                        Address::Memory(Memory::Alloca({
                            let ty_inference = self.create_type_inference(
                                r#type::Constraint::All(false),
                            );
                            self.create_alloca_with_value(Value::Literal(
                                Literal::Error(literal::Error {
                                    r#type: Type::Inference(ty_inference),
                                    span: Some(semantic_error.0),
                                }),
                            ))
                        })),
                        Qualifier::Mutable, /* has the highest mutability */
                        false,
                    )
                }
                Error::Internal(internal_error) => {
                    return Err(Error::Internal(internal_error))
                }
            },

            Ok(Expression::SideEffect) => unreachable!(),
        };
        let ty = simplify::simplify(
            &self.type_of_address(&address)?,
            &self.create_environment(),
        )
        .result;

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

        let unreachable_block =
            self.intermediate_representation.control_flow_graph.new_block();

        let mut match_arms = syntax_tree
            .arms()
            .connected_list()
            .as_ref()
            .into_iter()
            .flat_map(ConnectedList::elements)
            .zip(scope_ids)
            .map(|(x, scope_id)| {
                let mut pat = self
                    .bind_pattern(
                        &ty,
                        x.refutable_pattern(),
                        &self.create_handler_wrapper(handler),
                    )
                    .unwrap_or_else(|| {
                        Wildcard { span: x.refutable_pattern().span() }.into()
                    });

                Self::replace_refutable_in_tuple_pack(&mut pat, handler);

                let paths = Self::get_refutable_paths(&pat);

                MatchArm {
                    pattern: pat,
                    scope_id,
                    expression: x.expression(),
                    refutable_paths: paths,
                    binding_result: None,
                }
            })
            .collect::<Vec<_>>();

        let starting_block_id = self.current_block_id;

        self.handle_match_arms(
            &mut match_arms,
            MatchInfo {
                address: address.clone(),
                r#type: &ty,
                qualifier,
                from_lvalue,
                span: syntax_tree.span(),
            },
            unreachable_block,
            successor_block_id,
            handler,
        )?;

        assert!(self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(starting_block_id, Terminator::Panic)
            .map_or_else(|x| !x.is_invalid_block_id(), |()| true));
        assert!(self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(unreachable_block, Terminator::Panic)
            .map_or_else(|x| !x.is_invalid_block_id(), |()| true));

        self.current_block_id = successor_block_id;

        // remove the match arms that are unreachable and report the error
        for unreachable in
            match_arms.drain_filter(|x| x.binding_result.is_none())
        {
            self.create_handler_wrapper(handler).receive(Box::new(
                UnreachableMatchArm {
                    match_arm_span: unreachable.expression.span(),
                },
            ));
        }

        if match_arms.is_empty() {
            assert!(!self.current_block().is_reachable());

            let ty = ty.reduce_reference();

            let mut is_inhabited = false;
            if let Type::Symbol(Symbol { id: AdtID::Enum(enum_id), .. }) = ty {
                let symbol = self.table.get(*enum_id).unwrap();

                is_inhabited = symbol.variant_declaration_order().is_empty();
            }

            if !is_inhabited {
                self.create_handler_wrapper(handler).receive(Box::new(
                    NonExhaustiveMatch {
                        match_expression_span: syntax_tree
                            .match_keyword()
                            .span
                            .join(&syntax_tree.parenthesized().span())
                            .unwrap(),
                    },
                ));
            }

            Ok(Expression::RValue(Value::Literal(
                self.create_unreachable(Some(syntax_tree.span())),
            )))
        } else {
            if self
                .intermediate_representation
                .control_flow_graph
                .blocks()
                .get(unreachable_block)
                .unwrap()
                .is_reachable()
            {
                self.create_handler_wrapper(handler).receive(Box::new(
                    NonExhaustiveMatch {
                        match_expression_span: syntax_tree
                            .match_keyword()
                            .span
                            .join(&syntax_tree.parenthesized().span())
                            .unwrap(),
                    },
                ));
            }

            // no match arms reach the successor block
            let Some(first_reachable_arm_index) =
                match_arms.iter().position(|x| {
                    self.current_block()
                        .predecessors()
                        .contains(&x.binding_result.as_ref().unwrap().0)
                })
            else {
                assert!(self.current_block().predecessors().is_empty());

                return Ok(Expression::RValue(Value::Literal(
                    self.create_unreachable(Some(syntax_tree.span())),
                )));
            };
            let match_ty = self.type_of_value(
                &match_arms[first_reachable_arm_index]
                    .binding_result
                    .as_ref()
                    .unwrap()
                    .1,
            )?;

            // do type check for each match arm
            for arm in match_arms.iter().skip(first_reachable_arm_index + 1) {
                let ty = self
                    .type_of_value(&arm.binding_result.as_ref().unwrap().1)?;

                let _ = self.type_check(
                    &ty,
                    r#type::Expected::Known(match_ty.clone()),
                    arm.expression.span(),
                    true,
                    handler,
                );
            }

            let incoming_values = match_arms
                .into_iter()
                .filter(|x| {
                    self.current_block()
                        .predecessors()
                        .contains(&x.binding_result.as_ref().unwrap().0)
                })
                .map(|x| x.binding_result.unwrap())
                .collect::<HashMap<_, _>>();

            Ok(Expression::RValue(Value::Register(
                self.create_register_assignmnet(
                    Assignment::Phi(Phi { incoming_values, r#type: match_ty }),
                    Some(syntax_tree.span()),
                ),
            )))
        }
    }
}
