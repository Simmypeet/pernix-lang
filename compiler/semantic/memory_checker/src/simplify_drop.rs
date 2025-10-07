//! Contains the logic for simplifying the drop instructions.
//!
//! By the word "simplify", it means to remove the unnecessary drop instructions
//! such as the drop instructions that are invoked on the values that do not
//! impleemnt the `Drop` trait. Moreover, it breaks down the drop instructions
//! on the product types into the drop instructions on the individual fields if
//! the product type itself does not implement the `Drop` trait.

use pernixc_handler::Handler;
use pernixc_hash::HashSet;
use pernixc_ir::{
    address::{self, Address, Field, Index, Memory, Variant},
    instruction::{Drop, DropUnpackTuple, Instruction},
    value::{
        literal::{Literal, Unreachable},
        Environment, TypeOf, Value,
    },
    Values,
};
use pernixc_semantic_element::{
    fields::get_fields, parameter::get_parameters,
    variant::get_variant_associated_type,
};
use pernixc_symbol::{
    kind::{get_kind, Kind},
    member::get_members,
    name::get_by_qualified_name,
};
use pernixc_target::{Global, TargetID};
use pernixc_term::{
    generic_arguments::GenericArguments,
    predicate::{PositiveTrait, Predicate},
    r#type::{Primitive, Type},
};
use pernixc_type_system::{normalizer::Normalizer, UnrecoverableError};

use crate::diagnostic::Diagnostic;

pub(super) async fn simplify_drops<
    T: IntoIterator<Item = Instruction>,
    N: Normalizer,
>(
    drop_instructions: T,
    values: &Values,
    environment: &Environment<'_, N>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<Vec<Instruction>, UnrecoverableError> {
    let mut results = Vec::new();

    for instruction in drop_instructions {
        if let Instruction::Drop(drop) = instruction {
            results.extend(
                simplify_drop(
                    &drop,
                    values,
                    &mut HashSet::default(),
                    environment,
                    handler,
                )
                .await?,
            );
        } else {
            results.push(instruction);
        }
    }

    Ok(results)
}

#[allow(
    clippy::uninhabited_references,
    clippy::too_many_lines,
    clippy::cognitive_complexity
)]
pub(super) async fn simplify_drop<N: Normalizer>(
    drop: &Drop,
    values: &Values,
    visited_types: &mut HashSet<Type>,
    environment: &Environment<'_, N>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<Vec<Instruction>, UnrecoverableError> {
    let span_of = match drop.address.get_root_memory() {
        Memory::Parameter(id) => {
            let parameters = environment
                .tracked_engine()
                .get_parameters(environment.current_site)
                .await?;

            parameters.parameters[*id].span.unwrap()
        }

        Memory::Alloca(id) => values.allocas[*id].span.unwrap(),

        Memory::Capture(id) => {
            environment.captures().captures[*id].span.unwrap()
        }
    };

    let ty = values
        .type_of(&drop.address, environment)
        .await
        .map_err(|x| x.report_as_type_calculating_overflow(span_of, &handler))?
        .result;

    if !visited_types.insert(ty.clone()) {
        return Ok(Vec::new());
    }

    let drop_trait_id = environment
        .tracked_engine()
        .get_by_qualified_name(pernixc_corelib::drop::DROP_TRAIT_SEQUENCE)
        .await
        .unwrap();

    match &ty {
        Type::Symbol(symbol) => {
            let symbol_kind =
                environment.tracked_engine().get_kind(symbol.id).await;

            match symbol_kind {
                Kind::Struct => {
                    // the `core::NoDrop` intrinsic type never requires drop
                    if symbol.id.target_id == TargetID::CORE
                        && symbol.id
                            == environment
                                .tracked_engine()
                                .get_by_qualified_name(["core", "NoDrop"])
                                .await
                                .unwrap()
                    {
                        visited_types.remove(&ty);
                        return Ok(Vec::new());
                    }

                    let predicate = PositiveTrait::new(
                        drop_trait_id,
                        false, /* TODO: use correct boolean value */
                        GenericArguments {
                            lifetimes: Vec::new(),
                            types: vec![ty.clone()],
                            constants: Vec::new(),
                        },
                    );

                    if environment
                        .type_environment
                        .query(&predicate)
                        .await
                        .map_err(|x| {
                            x.report_as_undecidable_predicate(
                                Predicate::PositiveTrait(predicate.clone()),
                                None,
                                span_of,
                                &handler,
                            )
                        })?
                        .is_some()
                    {
                        visited_types.remove(&ty);
                        return Ok(vec![Instruction::Drop(drop.clone())]);
                    }

                    let fields = environment
                        .tracked_engine()
                        .get_fields(symbol.id)
                        .await?;

                    let mut instructions = Vec::new();

                    for field in fields.field_declaration_order.iter().copied()
                    {
                        // recursively simplify the drop instructions
                        instructions.extend(
                            Box::pin(simplify_drop(
                                &Drop {
                                    address: Address::Field(Field {
                                        struct_address: Box::new(
                                            drop.address.clone(),
                                        ),
                                        id: field,
                                    }),
                                },
                                values,
                                visited_types,
                                environment,
                                handler,
                            ))
                            .await?,
                        );
                    }

                    visited_types.remove(&ty);

                    Ok(instructions)
                }
                Kind::Enum => {
                    // if any of the variant requires drop, then we should drop
                    // the entire enum

                    let predicate = PositiveTrait::new(
                        drop_trait_id,
                        false, /* TODO: use correct boolean value */
                        GenericArguments {
                            lifetimes: Vec::new(),
                            types: vec![ty.clone()],
                            constants: Vec::new(),
                        },
                    );

                    if environment
                        .type_environment
                        .query(&predicate)
                        .await
                        .map_err(|x| {
                            x.report_as_undecidable_predicate(
                                Predicate::PositiveTrait(predicate.clone()),
                                None,
                                span_of,
                                &handler,
                            )
                        })?
                        .is_none()
                    {
                        // continue breaking if any variants requires drop
                    } else {
                        visited_types.remove(&ty);
                        return Ok(vec![Instruction::Drop(drop.clone())]);
                    }

                    let mut should_drop = false;
                    let member = environment
                        .tracked_engine()
                        .get_members(symbol.id)
                        .await;

                    for variant_id in member
                        .member_ids_by_name
                        .values()
                        .copied()
                        .map(|x| Global::new(symbol.id.target_id, x))
                    {
                        // recursively simplify the drop instructions
                        let variant_sym = environment
                            .tracked_engine()
                            .get_variant_associated_type(variant_id)
                            .await?;

                        if variant_sym.is_none() {
                            continue;
                        }

                        if !Box::pin(simplify_drop(
                            &Drop {
                                address: Address::Variant(Variant {
                                    enum_address: Box::new(
                                        drop.address.clone(),
                                    ),
                                    id: variant_id,
                                }),
                            },
                            values,
                            visited_types,
                            environment,
                            handler,
                        ))
                        .await?
                        .is_empty()
                        {
                            should_drop = true;
                            break;
                        }
                    }

                    visited_types.remove(&ty);

                    if should_drop {
                        Ok(vec![Instruction::Drop(drop.clone())])
                    } else {
                        Ok(Vec::new())
                    }
                }

                kind => panic!("unexpected symbol kind: {kind:?}"),
            }
        }

        Type::Tuple(tuple) => {
            let unpacked_position =
                tuple.elements.iter().position(|x| x.is_unpacked);

            let mut instructions = Vec::new();

            match unpacked_position {
                Some(packed_position) => {
                    for i in 0..packed_position {
                        instructions.extend(
                            Box::pin(simplify_drop(
                                &Drop {
                                    address: Address::Tuple(address::Tuple {
                                        tuple_address: Box::new(
                                            drop.address.clone(),
                                        ),
                                        offset: address::Offset::FromStart(i),
                                    }),
                                },
                                values,
                                visited_types,
                                environment,
                                handler,
                            ))
                            .await?,
                        );
                    }

                    instructions.push(Instruction::DropUnpackTuple(
                        DropUnpackTuple {
                            tuple_address: drop.address.clone(),
                            before_unpacked_element_count: packed_position,
                            after_unpacked_element_count: tuple.elements.len()
                                - packed_position
                                - 1,
                        },
                    ));

                    for i in packed_position + 1..tuple.elements.len() {
                        instructions.extend(
                            Box::pin(simplify_drop(
                                &Drop {
                                    address: Address::Tuple(address::Tuple {
                                        tuple_address: Box::new(
                                            drop.address.clone(),
                                        ),
                                        offset: address::Offset::FromEnd(
                                            tuple.elements.len() - i - 1,
                                        ),
                                    }),
                                },
                                values,
                                visited_types,
                                environment,
                                handler,
                            ))
                            .await?,
                        );
                    }
                }

                None => {
                    for (index, _) in tuple.elements.iter().enumerate() {
                        instructions.extend(
                            Box::pin(simplify_drop(
                                &Drop {
                                    address: Address::Tuple(address::Tuple {
                                        tuple_address: Box::new(
                                            drop.address.clone(),
                                        ),
                                        offset: address::Offset::FromStart(
                                            index,
                                        ),
                                    }),
                                },
                                values,
                                visited_types,
                                environment,
                                handler,
                            ))
                            .await?,
                        );
                    }
                }
            }

            visited_types.remove(&ty);
            Ok(instructions)
        }

        Type::MemberSymbol(_)
        | Type::Reference(_)
        | Type::Pointer(_)
        | Type::FunctionSignature(_)
        | Type::Primitive(_)
        | Type::Error(_)
        | Type::Phantom(_) => {
            visited_types.remove(&ty);
            Ok(Vec::new())
        }

        Type::Array(_) => {
            if Box::pin(simplify_drop(
                &Drop {
                    address: Address::Index(Index {
                        array_address: Box::new(drop.address.clone()),
                        indexing_value: Value::Literal(Literal::Unreachable(
                            Unreachable {
                                r#type: Type::Primitive(Primitive::Usize),
                                span: None,
                            },
                        )),
                    }),
                },
                values,
                visited_types,
                environment,
                handler,
            ))
            .await?
            .is_empty()
            {
                visited_types.remove(&ty);
                Ok(Vec::new())
            } else {
                visited_types.remove(&ty);
                Ok(vec![Instruction::Drop(drop.clone())])
            }
        }

        Type::TraitMember(_) | Type::Parameter(_) => {
            visited_types.remove(&ty);
            Ok(vec![Instruction::Drop(drop.clone())])
        }

        Type::Inference(infer) => {
            unreachable!("inference type should have been resolved: {infer:?}")
        }
    }
}
