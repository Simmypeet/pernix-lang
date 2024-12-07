//! Contains the logic for simplifying the drop instructions.
//!
//! By the word "simplify", it means to remove the unnecessary drop instructions
//! such as the drop instructions that are invoked on the values that do not
//! impleemnt the `Drop` trait. Moreover, it breaks down the drop instructions
//! on the product types into the drop instructions on the individual fields if
//! the product type itself does not implement the `Drop` trait.

use crate::{
    error::{OverflowOperation, TypeSystemOverflow},
    ir::{
        self,
        address::{self, Address, Field, Memory},
        control_flow_graph::ControlFlowGraph,
        instruction::{Drop, DropUnpackTuple, Instruction},
        representation::Values,
    },
    symbol::{
        table::{self, representation::Index},
        AdtID, CallableID, GlobalID,
    },
    type_system::{
        environment::Environment,
        normalizer::Normalizer,
        observer::Observer,
        predicate::{PositiveTrait, Predicate},
        term::{
            r#type::{SymbolID, Type},
            GenericArguments,
        },
        Compute,
    },
};

fn simplify_drop<S: table::State>(
    drop: &Drop<ir::Model>,
    values: &Values<ir::Model>,
    current_site: GlobalID,
    environment: &Environment<
        ir::Model,
        S,
        impl Normalizer<ir::Model, S>,
        impl Observer<ir::Model, S>,
    >,
) -> Result<Vec<Instruction<ir::Model>>, TypeSystemOverflow<ir::Model>> {
    let get_span_of = || match drop.address.get_root_memory() {
        Memory::Parameter(id) => {
            let callable = CallableID::try_from(current_site).unwrap();

            environment
                .table()
                .get_callable(callable)
                .unwrap()
                .parameters()
                .get(*id)
                .unwrap()
                .span
                .clone()
                .unwrap()
        }

        Memory::Alloca(id) => values.allocas.get(*id).unwrap().span.clone(),
    };

    let ty = values
        .type_of_address(&drop.address, current_site, environment)
        .map_err(|x| TypeSystemOverflow {
            operation: OverflowOperation::TypeCheck,
            overflow_span: get_span_of(),
            overflow_error: x.into_overflow().unwrap(),
        })?
        .result;

    match &ty {
        Type::Symbol(symbol) => match symbol.id {
            SymbolID::Adt(AdtID::Struct(struct_id)) => {
                let drop_trait_id = environment
                    .table()
                    .get_by_qualified_name(["core", "Drop"])
                    .unwrap()
                    .into_trait()
                    .unwrap();

                let predicate = PositiveTrait::new(
                    drop_trait_id,
                    false, /* TODO: use correct boolean value */
                    GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![ty],
                        constants: Vec::new(),
                    },
                );

                match predicate.query(environment) {
                    Ok(None) => {
                        // continue breaking down the drop instructions
                    }

                    Ok(Some(_)) => {
                        return Ok(vec![Instruction::Drop(drop.clone())])
                    }

                    Err(overflow_error) => {
                        return Err(TypeSystemOverflow {
                            operation: OverflowOperation::Predicate(
                                Predicate::PositiveTrait(predicate),
                            ),
                            overflow_span: get_span_of(),
                            overflow_error,
                        });
                    }
                }

                let mut instructions = Vec::new();

                for field in environment
                    .table()
                    .get(struct_id)
                    .unwrap()
                    .field_declaration_order()
                    .iter()
                    .copied()
                {
                    // recursively simplify the drop instructions
                    instructions.extend(simplify_drop(
                        &Drop {
                            address: Address::Field(Field {
                                struct_address: Box::new(drop.address.clone()),
                                id: field,
                            }),
                        },
                        values,
                        current_site,
                        environment,
                    )?);
                }

                Ok(instructions)
            }

            SymbolID::Adt(AdtID::Enum(_)) => {
                Ok(vec![Instruction::Drop(drop.clone())])
            }

            SymbolID::Function(_) => Ok(Vec::new()),
        },

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
                            current_site,
                            environment,
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
                            current_site,
                            environment,
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
                            current_site,
                            environment,
                        )?);
                    }
                }
            }

            Ok(instructions)
        }

        Type::MemberSymbol(_)
        | Type::Reference(_)
        | Type::Pointer(_)
        | Type::Primitive(_)
        | Type::Error(_)
        | Type::Phantom(_) => Ok(Vec::new()),

        Type::Array(_) | Type::TraitMember(_) | Type::Parameter(_) => {
            Ok(vec![Instruction::Drop(drop.clone())])
        }

        Type::Inference(never) => match *never {},
    }
}

pub(super) fn simplfy_drop_in_cfg<S: table::State>(
    control_flow_graph: &mut ControlFlowGraph<ir::Model>,
    values: &Values<ir::Model>,
    current_site: GlobalID,
    environment: &Environment<
        ir::Model,
        S,
        impl Normalizer<ir::Model, S>,
        impl Observer<ir::Model, S>,
    >,
) -> Result<(), TypeSystemOverflow<ir::Model>> {
    for (_, block) in control_flow_graph.block_muts() {
        let mut i = 0;

        while i < block.instructions().len() {
            if let Instruction::Drop(inst) = &block.instructions()[i] {
                let instructions =
                    simplify_drop(inst, values, current_site, environment)?;

                let len = instructions.len();
                block.splice(i..=i, instructions);

                i += len;
            } else {
                i += 1;
            }
        }
    }

    Ok(())
}
