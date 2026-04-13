//! Contains the logic for simplifying the drop instructions.
//!
//! By the word "simplify", it means to remove the unnecessary drop instructions
//! such as the drop instructions that are invoked on the values that do not
//! impleemnt the `Drop` trait. Moreover, it breaks down the drop instructions
//! on the product types into the drop instructions on the individual fields if
//! the product type itself does not implement the `Drop` trait.

use pernixc_handler::Handler;
use pernixc_hash::FxHashSet;
use pernixc_ir::{
    Values,
    address::{self, Address, Field, Index, Variant},
    instruction::{Drop, DropUnpackTuple, Instruction},
    value::{
        TypeOf, Value, ValueEnvironment,
        literal::{Literal, Unreachable},
    },
};
use pernixc_semantic_element::{
    fields::get_fields, variant::get_variant_associated_type,
};
use pernixc_symbol::{
    kind::{Kind, get_kind},
    member::get_members,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    instance::TraitRef,
    r#type::{Primitive, Type},
};
use pernixc_type_system::{UnrecoverableError, normalizer::Normalizer};

use crate::diagnostic::Diagnostic;

#[allow(clippy::unused_async)]
pub(super) async fn simplify_drops<
    T: IntoIterator<Item = Instruction>,
    N: Normalizer,
>(
    drop_instructions: T,
    values: &Values,
    environment: &ValueEnvironment<'_, N>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<Vec<Instruction>, UnrecoverableError> {
    let mut results = Vec::new();
    for instruction in drop_instructions {
        if let Instruction::Drop(drop) = instruction {
            results.extend(
                simplify_drop(
                    &drop,
                    values,
                    &mut FxHashSet::default(),
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

// Import will be needed when this function is uncommented
use pernixc_corelib::{get_drop_trait_id, get_no_drop_struct_id};

#[allow(
    clippy::uninhabited_references,
    clippy::too_many_lines,
    clippy::cognitive_complexity
)]
async fn simplify_drop<N: Normalizer>(
    drop: &Drop,
    values: &Values,
    visited_types: &mut FxHashSet<Type>,
    environment: &ValueEnvironment<'_, N>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<Vec<Instruction>, UnrecoverableError> {
    let span_of = values
        .span_of_memory(drop.address.get_root_memory(), environment)
        .await;

    let ty = values
        .type_of(&drop.address, environment)
        .await
        .map_err(|x| x.report_as_type_calculating_overflow(span_of, &handler))?
        .result;

    if !visited_types.insert(ty.clone()) {
        return Ok(Vec::new());
    }

    match &ty {
        Type::Symbol(symbol) => {
            let symbol_kind =
                environment.tracked_engine().get_kind(symbol.id()).await;

            match symbol_kind {
                Kind::Struct => {
                    // the `core::NoDrop` intrinsic type never requires drop
                    if symbol.id()
                        == environment
                            .tracked_engine()
                            .get_no_drop_struct_id()
                            .await
                    {
                        visited_types.remove(&ty);
                        return Ok(Vec::new());
                    }

                    let trait_ref = TraitRef::new(
                        environment.tracked_engine().get_drop_trait_id().await,
                        GenericArguments::new(
                            Vec::new(),
                            vec![ty.clone()],
                            Vec::new(),
                            Vec::new(),
                        ),
                    );

                    if environment
                        .type_environment()
                        .resolve_instance(&trait_ref)
                        .await
                        .map_err(|x| {
                            x.report_as_type_check_overflow(span_of, &handler)
                        })?
                        .is_ok()
                    {
                        visited_types.remove(&ty);
                        return Ok(vec![Instruction::Drop(drop.clone())]);
                    }

                    let fields = environment
                        .tracked_engine()
                        .get_fields(symbol.id())
                        .await;

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

                    let trait_ref = TraitRef::new(
                        environment.tracked_engine().get_drop_trait_id().await,
                        GenericArguments::new(
                            Vec::new(),
                            vec![ty.clone()],
                            Vec::new(),
                            Vec::new(),
                        ),
                    );

                    if environment
                        .type_environment()
                        .resolve_instance(&trait_ref)
                        .await
                        .map_err(|x| {
                            x.report_as_type_check_overflow(span_of, &handler)
                        })?
                        .is_ok()
                    {
                        visited_types.remove(&ty);
                        return Ok(vec![Instruction::Drop(drop.clone())]);
                    }

                    let mut should_drop = false;
                    let member = environment
                        .tracked_engine()
                        .get_members(symbol.id())
                        .await;

                    for variant_id in member
                        .member_ids_by_name
                        .values()
                        .copied()
                        .map(|x| Global::new(symbol.id().target_id, x))
                    {
                        // recursively simplify the drop instructions
                        let Some(_variant_sym) = environment
                            .tracked_engine()
                            .get_variant_associated_type(variant_id)
                            .await
                        else {
                            continue;
                        };

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
            let unpacked_position = tuple.unpacked_position();

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
                            after_unpacked_element_count: tuple.len()
                                - packed_position
                                - 1,
                        },
                    ));

                    for i in packed_position + 1..tuple.len() {
                        instructions.extend(
                            Box::pin(simplify_drop(
                                &Drop {
                                    address: Address::Tuple(address::Tuple {
                                        tuple_address: Box::new(
                                            drop.address.clone(),
                                        ),
                                        offset: address::Offset::FromEnd(
                                            tuple.len() - i - 1,
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
                    for index in 0..tuple.len() {
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

        Type::AssociatedSymbol(_)
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
                                span: span_of,
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

        Type::InstanceAssociated(_) | Type::Parameter(_) => {
            visited_types.remove(&ty);
            Ok(vec![Instruction::Drop(drop.clone())])
        }

        Type::Inference(infer) => {
            unreachable!("inference type should have been resolved: {infer:?}")
        }
    }
}
