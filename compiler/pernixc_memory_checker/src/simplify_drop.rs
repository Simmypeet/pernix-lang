//! Contains the logic for simplifying the drop instructions.
//!
//! By the word "simplify", it means to remove the unnecessary drop instructions
//! such as the drop instructions that are invoked on the values that do not
//! impleemnt the `Drop` trait. Moreover, it breaks down the drop instructions
//! on the product types into the drop instructions on the individual fields if
//! the product type itself does not implement the `Drop` trait.

use std::{collections::HashSet, sync::Arc};

use pernixc_abort::Abort;
use pernixc_component::function_signature::FunctionSignature;
use pernixc_handler::Handler;
use pernixc_ir::{
    address::{self, Address, Field, Memory, Variant},
    instruction::{Drop, DropUnpackTuple, Instruction},
    model, Values,
};
use pernixc_table::{
    component::{Member, SymbolKind},
    diagnostic::Diagnostic,
    GlobalID,
};
use pernixc_term::{
    generic_arguments::GenericArguments,
    predicate::{PositiveTrait, Predicate},
    r#type::Type,
};
use pernixc_type_system::{environment::Environment, normalizer::Normalizer};

pub(super) fn simplify_drops(
    drop_instructions: impl IntoIterator<Item = Instruction<model::Model>>,
    values: &Values<model::Model>,
    current_site: GlobalID,
    environment: &Environment<model::Model, impl Normalizer<model::Model>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Vec<Instruction<model::Model>>, Abort> {
    let mut results = Vec::new();

    for instruction in drop_instructions {
        if let Instruction::Drop(drop) = instruction {
            results.extend(simplify_drop(
                &drop,
                values,
                &mut HashSet::new(),
                current_site,
                environment,
                handler,
            )?);
        } else {
            results.push(instruction);
        }
    }

    Ok(results)
}

#[allow(clippy::uninhabited_references, clippy::too_many_lines)]
pub(super) fn simplify_drop(
    drop: &Drop<model::Model>,
    values: &Values<model::Model>,
    visited_types: &mut HashSet<Type<model::Model>>,
    current_site: GlobalID,
    environment: &Environment<model::Model, impl Normalizer<model::Model>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Vec<Instruction<model::Model>>, Abort> {
    let span_of = match drop.address.get_root_memory() {
        Memory::Parameter(id) => {
            let function_signature: Arc<FunctionSignature> =
                environment.table().query::<FunctionSignature>(current_site)?;

            function_signature.parameters[*id].span.clone().unwrap()
        }

        Memory::Alloca(id) => values.allocas[*id].span.clone().unwrap(),
    };

    let ty = values
        .type_of_address(&drop.address, current_site, environment)
        .map_err(|x| {
            x.report_overflow(|x| {
                x.report_as_type_calculating_overflow(span_of.clone(), handler)
            })
        })?
        .result;

    if !visited_types.insert(ty.clone()) {
        return Ok(Vec::new());
    }

    let drop_trait_id =
        environment.table().get_by_qualified_name(["core", "Drop"]).unwrap();

    match &ty {
        Type::Symbol(symbol) => {
            let symbol_kind = *environment.table().get::<SymbolKind>(symbol.id);

            match symbol_kind {
                SymbolKind::Struct => {
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
                        .query(&predicate)
                        .map_err(|x| {
                            x.report_overflow(|x| {
                                x.report_as_undecidable_predicate(
                                    Predicate::PositiveTrait(predicate.clone()),
                                    None,
                                    span_of,
                                    handler,
                                )
                            })
                        })?
                        .is_none()
                    {
                        // continue breaking down the drop instructions
                    } else {
                        visited_types.remove(&ty);
                        return Ok(vec![Instruction::Drop(drop.clone())]);
                    }

                    let fields = environment
                        .table()
                        .query::<pernixc_component::fields::Fields>(
                        symbol.id,
                    )?;
                    let mut instructions = Vec::new();

                    for field in fields.field_declaration_order.iter().copied()
                    {
                        // recursively simplify the drop instructions
                        instructions.extend(simplify_drop(
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
                            current_site,
                            environment,
                            handler,
                        )?);
                    }

                    visited_types.remove(&ty);

                    Ok(instructions)
                }
                SymbolKind::Enum => {
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
                        .query(&predicate)
                        .map_err(|x| {
                            x.report_overflow(|x| {
                                x.report_as_undecidable_predicate(
                                    Predicate::PositiveTrait(predicate.clone()),
                                    None,
                                    span_of,
                                    handler,
                                )
                            })
                        })?
                        .is_none()
                    {
                        // continue breaking if any variants requires drop
                    } else {
                        visited_types.remove(&ty);
                        return Ok(vec![Instruction::Drop(drop.clone())]);
                    }

                    let mut should_drop = false;
                    let member = environment.table().get::<Member>(symbol.id);

                    for variant_id in member
                        .values()
                        .copied()
                        .map(|x| GlobalID::new(symbol.id.target_id, x))
                    {
                        // recursively simplify the drop instructions
                        let variant_sym = environment
                            .table()
                            .query::<pernixc_component::variant::Variant>(
                            variant_id,
                        )?;

                        if variant_sym.associated_type.is_none() {
                            continue;
                        }

                        if !simplify_drop(
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
                            current_site,
                            environment,
                            handler,
                        )?
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
                        instructions.extend(simplify_drop(
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
                            current_site,
                            environment,
                            handler,
                        )?);
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
                        instructions.extend(simplify_drop(
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
                            current_site,
                            environment,
                            handler,
                        )?);
                    }
                }

                None => {
                    for (index, _) in tuple.elements.iter().enumerate() {
                        instructions.extend(simplify_drop(
                            &Drop {
                                address: Address::Tuple(address::Tuple {
                                    tuple_address: Box::new(
                                        drop.address.clone(),
                                    ),
                                    offset: address::Offset::FromStart(index),
                                }),
                            },
                            values,
                            visited_types,
                            current_site,
                            environment,
                            handler,
                        )?);
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

        Type::Array(_) | Type::TraitMember(_) | Type::Parameter(_) => {
            visited_types.remove(&ty);
            Ok(vec![Instruction::Drop(drop.clone())])
        }

        Type::Inference(never) => match *never {},
    }
}
