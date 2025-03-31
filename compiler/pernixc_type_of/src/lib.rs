//! Contains the extension traits for retrieving the type of the values in the
//! ir.

use std::{collections::BTreeSet, ops::Deref};

use pernixc_arena::ID;
use pernixc_semantic::{
    component::{
        derived::{
            fields::Fields,
            function_signature::FunctionSignature,
            generic_parameters::GenericParameters,
            ir::{
                address::{Address, Memory, Offset},
                value::{
                    register::{
                        Assignment, Binary, BinaryOperator, Borrow,
                        FunctionCall, Load, Prefix, PrefixOperator, Register,
                        Struct, Tuple, Variant, VariantNumber,
                    },
                    Value,
                },
                Values,
            },
            variant,
        },
        input::{Member, Parent},
    },
    table::{GlobalID, Table},
    term::{
        self,
        constant::Constant,
        instantiation::{self, Instantiation},
        r#type::{Primitive, Type},
        Symbol,
    },
};
use pernixc_type_system::{
    environment::Environment, normalizer::Normalizer, simplify::Simplify,
    Error, Succeeded,
};

/// An extension trait on [`Values`] taht provides the ability to get the type
/// of the values in the IR.
pub trait TypeOf<V> {
    /// The model in which the [`Values`] is in.
    type Model: term::Model;

    /// Gets the type of the value
    ///
    /// # Parameters
    ///
    /// - `value`: The value to get the type of.
    /// - `current_site`: The site where the IR binding is being taken place in.
    /// - `environment`: The environment to get the type from.
    ///
    /// # Errors
    ///
    /// See [`Error`] for the possible errors that can occur.
    fn type_of(
        &self,
        value: V,
        current_site: GlobalID,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> Result<Succeeded<Type<Self::Model>, Self::Model>, Error>;
}

impl<M: term::Model> TypeOf<&Value<M>> for Values<M> {
    type Model = M;

    fn type_of(
        &self,
        value: &Value<M>,
        current_site: GlobalID,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> Result<Succeeded<Type<Self::Model>, Self::Model>, Error> {
        match value {
            Value::Register(register) => {
                self.type_of(*register, current_site, environment)
            }
            Value::Literal(literal) => Ok(environment
                .query(&Simplify(literal.r#type()))?
                .map(|x| x.deref().clone())
                .unwrap()),
        }
    }
}

fn type_of_tuple_assignment<M: term::Model>(
    values: &Values<M>,
    tuple: &Tuple<M>,
    current_site: GlobalID,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<Succeeded<Type<M>, M>, Error> {
    let mut constraints = BTreeSet::new();
    let mut elements = Vec::new();

    for element in &tuple.elements {
        let Succeeded { result: ty, constraints: new_constraint } =
            values.type_of(&element.value, current_site, environment)?;

        constraints.extend(new_constraint);

        if element.is_unpacked {
            match ty {
                Type::Tuple(ty) => elements.extend(ty.elements),
                ty => elements
                    .push(term::TupleElement { term: ty, is_unpacked: true }),
            }
        } else {
            elements.push(term::TupleElement { term: ty, is_unpacked: false });
        }
    }

    Ok(Succeeded::with_constraints(
        Type::Tuple(term::Tuple { elements }),
        constraints,
    ))
}

fn type_of_load_assignment<M: term::Model>(
    values: &Values<M>,
    load: &Load<M>,
    current_site: GlobalID,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<Succeeded<Type<M>, M>, Error> {
    values.type_of(&load.address, current_site, environment)
}

fn type_of_reference_of_assignment<M: term::Model>(
    values: &Values<M>,
    reference_of: &Borrow<M>,
    current_site: GlobalID,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<Succeeded<Type<M>, M>, Error> {
    values.type_of(&reference_of.address, current_site, environment).map(|x| {
        x.map(|x| {
            Type::Reference(term::r#type::Reference {
                qualifier: reference_of.qualifier,
                lifetime: reference_of.lifetime.clone(),
                pointee: Box::new(x),
            })
        })
    })
}

fn type_of_prefix_assignment<M: term::Model>(
    values: &Values<M>,
    prefix: &Prefix<M>,
    current_site: GlobalID,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<Succeeded<Type<M>, M>, Error> {
    let operand_type =
        values.type_of(&prefix.operand, current_site, environment)?;

    match prefix.operator {
        PrefixOperator::Negate
        | PrefixOperator::LogicalNot
        | PrefixOperator::BitwiseNot => Ok(operand_type),
    }
}

fn type_of_struct_assignment<M: term::Model>(st: &Struct<M>) -> Type<M> {
    Type::Symbol(Symbol {
        id: st.struct_id,
        generic_arguments: st.generic_arguments.clone(),
    })
}

fn type_of_variant_assignment<M: term::Model>(
    variant: &Variant<M>,
    table: &Table,
) -> Type<M> {
    let enum_id = table.get::<Parent>(variant.variant_id).unwrap();

    Type::Symbol(Symbol {
        id: GlobalID::new(variant.variant_id.target_id, enum_id),
        generic_arguments: variant.generic_arguments.clone(),
    })
}

fn type_of_function_call_assignment<M: term::Model>(
    function_call: &FunctionCall<M>,
    table: &Table,
) -> Result<Type<M>, Error> {
    let callable =
        table.query::<FunctionSignature>(function_call.callable_id)?;

    let mut return_type = M::from_default_type(callable.return_type.clone());

    instantiation::instantiate(&mut return_type, &function_call.instantiation);

    Ok(return_type)
}

fn type_of_binary<M: term::Model>(
    values: &Values<M>,
    binary: &Binary<M>,
    current_site: GlobalID,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<Succeeded<Type<M>, M>, Error> {
    // the return type always based on the lhs field
    if let BinaryOperator::Relational(_) = binary.operator {
        Ok(Succeeded::new(Type::Primitive(term::r#type::Primitive::Bool)))
    } else {
        values.type_of(&binary.lhs, current_site, environment)
    }
}

fn type_of_variant_number<M: term::Model>(
    variant_number: &VariantNumber<M>,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Type<M> {
    let mut answer = Primitive::Bool;
    let variant_count =
        environment.table().get::<Member>(variant_number.enum_id).len();

    let int_capacity = |a: Primitive| match a {
        Primitive::Bool => 2u64,
        Primitive::Uint8 => 2u64.pow(8),
        Primitive::Uint16 => 2u64.pow(16),
        Primitive::Uint32 => 2u64.pow(32),
        Primitive::Uint64 => 2u64.pow(64),
        _ => unreachable!(),
    };

    let next = |a: Primitive| match a {
        Primitive::Bool => Primitive::Uint8,
        Primitive::Uint8 => Primitive::Uint16,
        Primitive::Uint16 => Primitive::Uint32,
        Primitive::Uint32 => Primitive::Uint64,
        _ => unreachable!(),
    };

    while int_capacity(answer) < variant_count as u64 {
        answer = next(answer);
    }

    Type::Primitive(answer)
}

impl<M: term::Model> TypeOf<ID<Register<M>>> for Values<M> {
    type Model = M;

    fn type_of(
        &self,
        id: ID<Register<M>>,
        current_site: GlobalID,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Succeeded<Type<M>, M>, Error> {
        let register = &self.registers[id];

        let ty = match &register.assignment {
            Assignment::Tuple(tuple) => {
                return type_of_tuple_assignment(
                    self,
                    tuple,
                    current_site,
                    environment,
                )
            }
            Assignment::Load(load) => {
                return type_of_load_assignment(
                    self,
                    load,
                    current_site,
                    environment,
                )
            }
            Assignment::Borrow(reference_of) => {
                return type_of_reference_of_assignment(
                    self,
                    reference_of,
                    current_site,
                    environment,
                )
            }
            Assignment::Prefix(prefix) => {
                return type_of_prefix_assignment(
                    self,
                    prefix,
                    current_site,
                    environment,
                )
            }
            Assignment::Struct(st) => Ok(type_of_struct_assignment(st)),
            Assignment::Variant(variant) => {
                Ok(type_of_variant_assignment(variant, environment.table()))
            }
            Assignment::FunctionCall(function_call) => {
                type_of_function_call_assignment(
                    function_call,
                    environment.table(),
                )
            }
            Assignment::Binary(binary) => {
                return type_of_binary(self, binary, current_site, environment);
            }
            Assignment::Phi(phi_node) => Ok(phi_node.r#type.clone()),
            Assignment::Array(array) => Ok(Type::Array(term::r#type::Array {
                r#type: Box::new(array.element_type.clone()),
                length: Constant::Primitive(term::constant::Primitive::Usize(
                    array.elements.len() as u64,
                )),
            })),
            Assignment::Cast(cast) => Ok(cast.r#type.clone()),
            Assignment::VariantNumber(variant) => {
                return Ok(Succeeded::new(type_of_variant_number(
                    variant,
                    environment,
                )))
            }
        }?;

        Ok(environment.simplify(ty)?.deref().clone())
    }
}

impl<M: term::Model> TypeOf<&Address<M>> for Values<M> {
    type Model = M;

    #[allow(clippy::too_many_lines)]
    fn type_of(
        &self,
        address: &Address<M>,
        current_site: GlobalID,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Succeeded<Type<M>, M>, Error> {
        match address {
            Address::Memory(Memory::Parameter(parameter)) => {
                let function_signature =
                    environment
                        .table()
                        .query::<FunctionSignature>(current_site)?;

                let ty = M::from_default_type(
                    function_signature.parameters[*parameter].r#type.clone(),
                );

                Ok(environment.simplify(ty)?.deref().clone())
            }

            Address::Memory(Memory::Alloca(parameter)) => {
                let alloca = &self.allocas[*parameter];

                Ok(environment.simplify(alloca.r#type.clone())?.deref().clone())
            }

            Address::Field(field_address) => {
                // the type of address should've been simplified
                let Succeeded { result, mut constraints } = self.type_of(
                    &*field_address.struct_address,
                    current_site,
                    environment,
                )?;
                let Type::Symbol(Symbol { id: struct_id, generic_arguments }) =
                    result
                else {
                    panic!("expected struct type");
                };

                let generic_parameters =
                    environment
                        .table()
                        .query::<GenericParameters>(struct_id)?;
                let fields = environment.table().query::<Fields>(struct_id)?;

                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments,
                    struct_id,
                    &generic_parameters,
                )
                .unwrap();

                let mut field_ty = M::from_default_type(
                    fields.fields[field_address.id].r#type.clone(),
                );

                instantiation::instantiate(&mut field_ty, &instantiation);
                let simplification = environment.simplify(field_ty)?;
                constraints.extend(simplification.constraints.iter().cloned());

                Ok(Succeeded {
                    result: simplification.result.clone(),
                    constraints,
                })
            }

            Address::Tuple(tuple) => {
                Ok(self
                    .type_of(&*tuple.tuple_address, current_site, environment)?
                    .map(|x| {
                        // extract tuple type
                        let Type::Tuple(mut tuple_ty) = x else {
                            panic!("expected tuple type");
                        };

                        match match tuple.offset {
                            Offset::FromStart(id) => Some(id),
                            Offset::FromEnd(id) => tuple_ty
                                .elements
                                .len()
                                .checked_sub(1)
                                .and_then(|x| x.checked_sub(id)),
                            Offset::Unpacked => {
                                let Some(unpacked_ty) =
                                    tuple_ty.elements.iter().find_map(|x| {
                                        x.is_unpacked.then_some(&x.term)
                                    })
                                else {
                                    panic!("expected unpacked tuple element");
                                };

                                return unpacked_ty.clone();
                            }
                        } {
                            Some(id) if id < tuple_ty.elements.len() => {
                                let element_ty = tuple_ty.elements.remove(id);

                                if element_ty.is_unpacked {
                                    Type::Tuple(term::Tuple {
                                        elements: vec![term::TupleElement {
                                            term: element_ty.term,
                                            is_unpacked: true,
                                        }],
                                    })
                                } else {
                                    element_ty.term
                                }
                            }

                            _ => panic!("invalid tuple offset"),
                        }
                    }))
            }

            Address::Index(index) => Ok(self
                .type_of(&*index.array_address, current_site, environment)?
                .map(|x| *x.into_array().unwrap().r#type)),

            Address::Variant(variant) => {
                let Succeeded { result, mut constraints } = self.type_of(
                    &*variant.enum_address,
                    current_site,
                    environment,
                )?;

                let Type::Symbol(Symbol { id: enum_id, generic_arguments }) =
                    result
                else {
                    panic!("expected enum type");
                };

                let enum_generic_params =
                    environment.table().query::<GenericParameters>(enum_id)?;

                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments,
                    enum_id,
                    &enum_generic_params,
                )
                .unwrap();

                let variant = environment
                    .table()
                    .query::<variant::Variant>(variant.id)?;

                let mut variant_ty = M::from_default_type(
                    variant.associated_type.clone().unwrap(),
                );

                instantiation::instantiate(&mut variant_ty, &instantiation);

                let simplification = environment.simplify(variant_ty)?;
                constraints.extend(simplification.constraints.iter().cloned());

                Ok(Succeeded::with_constraints(
                    simplification.result.clone(),
                    constraints,
                ))
            }

            Address::Reference(value) => Ok(self
                .type_of(&*value.reference_address, current_site, environment)?
                .map(|x| *x.into_reference().unwrap().pointee)),
        }
    }
}
