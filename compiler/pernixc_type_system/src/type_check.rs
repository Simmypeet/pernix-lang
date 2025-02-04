//! Module for type-checking the constant terms.

use std::{collections::BTreeSet, ops::Deref, sync::Arc};

use derive_new::new;
use pernixc_component::{fields::Fields, variant::Variant};
use pernixc_table::{
    component::{Parent, SymbolKind},
    GlobalID,
};
use pernixc_term::{
    constant::{self, Constant},
    generic_parameter::GenericParameters,
    instantiation::{self, Instantiation},
    r#type::{self, Type},
    variance::Variance,
    Model, ModelOf, Symbol, Tuple,
};

use crate::{
    environment::{Environment, Query},
    equality::Equality,
    normalizer::Normalizer,
    term::Term,
    AbruptError, ResultArc, Satisfied, Succeeded,
};

/// A query for checking the type of a compile-time constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct TypeCheck<M: Model> {
    /// The constant value to check.
    pub constant: Constant<M>,

    /// The type to check against.
    pub r#type: Type<M>,
}

trait Check<T>: ModelOf
where
    T: Term<Model = Self::Model>,
{
    fn type_check(
        &self,
        another: &T,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> ResultArc<Satisfied, Self::Model>;
}

impl<M: Model> Check<Constant<M>> for Type<M> {
    fn type_check(
        &self,
        another: &Constant<M>,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> ResultArc<Satisfied, M> {
        environment.query(&TypeCheck::new(another.clone(), self.clone()))
    }
}

impl<M: Model> Check<Type<M>> for Constant<M> {
    fn type_check(
        &self,
        another: &Type<M>,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> ResultArc<Satisfied, M> {
        environment.query(&TypeCheck::new(self.clone(), another.clone()))
    }
}

fn tuple_type_check_unpacked<T, U>(
    this: &Tuple<T>,
    another: &Tuple<U>,
    environment: &Environment<T::Model, impl Normalizer<T::Model>>,
) -> ResultArc<Satisfied, T::Model>
where
    T: Check<U> + Term,
    U: Term<Model = T::Model>,
    Tuple<T>: Into<T>,
    Tuple<U>: Into<U>,
{
    // list of all outlives constraints
    let mut constraints = BTreeSet::new();

    let this_unpacked_count =
        this.elements.iter().filter(|value| value.is_unpacked).count();

    if this_unpacked_count != 1 || another.elements.len() > this.elements.len()
    {
        return Ok(None);
    }

    // find the unpacked position
    let unpacked_position =
        this.elements.iter().position(|ty| ty.is_unpacked).unwrap();

    let head_range = 0..unpacked_position;
    let type_trail_range = (unpacked_position + 1)..this.elements.len();
    let value_trail_range = (another.elements.len() - type_trail_range.len())
        ..another.elements.len();
    let value_unpack_range = unpacked_position..value_trail_range.start;

    // check the head
    for (this, another) in this.elements[head_range.clone()]
        .iter()
        .zip(&another.elements[head_range])
    {
        // no unpacked
        if another.is_unpacked {
            return Ok(None);
        }

        let Some(result) = this.term.type_check(&another.term, environment)?
        else {
            return Ok(None);
        };

        constraints.extend(result.constraints.iter().cloned());
    }

    // check the trail
    for (this, another) in this.elements[type_trail_range]
        .iter()
        .zip(&another.elements[value_trail_range])
    {
        // no unpacked
        if another.is_unpacked {
            return Ok(None);
        }

        let Some(result) = this.term.type_check(&another.term, environment)?
        else {
            return Ok(None);
        };

        constraints.extend(result.constraints.iter().cloned());
    }

    // check the unpacked
    let packed_another =
        Tuple { elements: another.elements[value_unpack_range].to_vec() }
            .into();

    // check the unpacked
    let Some(result) = this
        .elements
        .get(unpacked_position)
        .unwrap()
        .term
        .type_check(&packed_another, environment)?
    else {
        return Ok(None);
    };
    constraints.extend(result.constraints.iter().cloned());

    Ok(Some(Arc::new(Succeeded::satisfied_with(constraints))))
}

impl<M: Model> Query for TypeCheck<M> {
    type Model = M;
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<Satisfied, M>;
    type Error = AbruptError;

    #[allow(clippy::too_many_lines)]
    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        let result = (|| -> ResultArc<Satisfied, M> {
            match (&self.r#type, &self.constant) {
                // check integer and boolean
                (_, Constant::Primitive(primitive_const)) => {
                    let this_constant_type =
                        Type::Primitive(match primitive_const {
                            constant::Primitive::Int8(_) => {
                                r#type::Primitive::Int8
                            }
                            constant::Primitive::Int16(_) => {
                                r#type::Primitive::Int16
                            }
                            constant::Primitive::Int32(_) => {
                                r#type::Primitive::Int32
                            }
                            constant::Primitive::Int64(_) => {
                                r#type::Primitive::Int64
                            }
                            constant::Primitive::Isize(_) => {
                                r#type::Primitive::Isize
                            }
                            constant::Primitive::Uint8(_) => {
                                r#type::Primitive::Uint8
                            }
                            constant::Primitive::Uint16(_) => {
                                r#type::Primitive::Uint16
                            }
                            constant::Primitive::Uint32(_) => {
                                r#type::Primitive::Uint32
                            }
                            constant::Primitive::Uint64(_) => {
                                r#type::Primitive::Uint64
                            }
                            constant::Primitive::Usize(_) => {
                                r#type::Primitive::Usize
                            }
                            constant::Primitive::Bool(_) => {
                                r#type::Primitive::Bool
                            }
                        });

                    match environment.compatible(
                        &this_constant_type,
                        &self.r#type,
                        Variance::Covariant,
                    )? {
                        Some(compatible) => {
                            if !compatible
                                .result
                                .forall_lifetime_errors
                                .is_empty()
                            {
                                return Ok(None);
                            }

                            Ok(Some(Arc::new(Succeeded::with_constraints(
                                Satisfied,
                                compatible.constraints,
                            ))))
                        }
                        None => Ok(None),
                    }
                }

                // check struct type
                (
                    Type::Symbol(Symbol { id: struct_id, generic_arguments }),
                    Constant::Struct(value),
                ) => {
                    // early return if the mismatched id
                    if *struct_id != value.id {
                        return Ok(None);
                    }

                    let generic_parameters =
                        environment
                            .table()
                            .query::<GenericParameters>(*struct_id)?;
                    let struct_fields =
                        environment.table().query::<Fields>(*struct_id)?;

                    // mismatched field count
                    assert_eq!(struct_fields.fields.len(), value.fields.len());

                    // create instantiation to check the fields
                    let instantiation = Instantiation::from_generic_arguments(
                        generic_arguments.clone(),
                        *struct_id,
                        &generic_parameters,
                    )
                    .unwrap();

                    let mut constraints = BTreeSet::new();

                    for (field_value, field_sym) in value.fields.iter().zip(
                        struct_fields
                            .field_declaration_order
                            .iter()
                            .copied()
                            .map(|x| &struct_fields.fields[x]),
                    ) {
                        // check the field type
                        let mut field_type =
                            M::from_default_type(field_sym.r#type.clone());

                        instantiation::instantiate(
                            &mut field_type,
                            &instantiation,
                        );

                        let Some(result) = environment.query(&Self::new(
                            field_value.clone(),
                            field_type,
                        ))?
                        else {
                            return Ok(None);
                        };

                        constraints.extend(result.constraints.iter().cloned());
                    }

                    Ok(Some(Arc::new(Succeeded::satisfied_with(constraints))))
                }

                // check enum type
                (
                    Type::Symbol(Symbol { id: enum_id, generic_arguments }),
                    Constant::Enum(enum_value),
                ) => {
                    let symbol_kind: SymbolKind =
                        *environment.table().get(*enum_id);

                    if symbol_kind != SymbolKind::Enum {
                        return Ok(None);
                    }

                    let enum_id_from_value = GlobalID::new(
                        enum_value.variant_id.target_id,
                        environment
                            .table()
                            .get::<Parent>(enum_value.variant_id)
                            .unwrap(),
                    );

                    if *enum_id != enum_id_from_value {
                        return Ok(None);
                    }

                    let enum_generic_params =
                        environment
                            .table()
                            .query::<GenericParameters>(*enum_id)?;

                    let instantiation = Instantiation::from_generic_arguments(
                        generic_arguments.clone(),
                        *enum_id,
                        &enum_generic_params,
                    )
                    .unwrap();

                    let variant = environment
                        .table()
                        .query::<Variant>(enum_value.variant_id)?;

                    // mismatched associated value
                    match (
                        &variant.associated_type,
                        &enum_value.associated_value,
                    ) {
                        (None, None) => {
                            Ok(Some(Arc::new(Succeeded::satisfied())))
                        }

                        (Some(ty), Some(value)) => {
                            let mut associated_ty =
                                M::from_default_type(ty.clone());

                            instantiation::instantiate(
                                &mut associated_ty,
                                &instantiation,
                            );

                            environment.query(&Self::new(
                                value.deref().clone(),
                                associated_ty,
                            ))
                        }

                        (ty, value) => {
                            panic!(
                                "Mismatched associated value: {ty:?} {value:?}"
                            );
                        }
                    }
                }

                // check array type
                (Type::Array(array_ty), Constant::Array(array_value)) => {
                    let len = Constant::Primitive(constant::Primitive::Usize(
                        array_value.elements.len() as u128,
                    ));

                    // check the length equals
                    let Some(result) = environment
                        .query(&Equality::new(len, array_ty.length.clone()))?
                    else {
                        return Ok(None);
                    };

                    let mut constraints = result.constraints.clone();

                    // check the type of each element
                    for value in &array_value.elements {
                        let Some(result) = environment.query(&Self::new(
                            value.clone(),
                            (*array_ty.r#type).clone(),
                        ))?
                        else {
                            return Ok(None);
                        };

                        constraints.extend(result.constraints.iter().cloned());
                    }

                    Ok(Some(Arc::new(Succeeded::satisfied_with(constraints))))
                }

                (ty, Constant::Parameter(constant_parameter)) => {
                    let generic_sym =
                        environment.table().query::<GenericParameters>(
                            constant_parameter.parent,
                        )?;

                    let constant_parameter_ty = M::from_default_type(
                        generic_sym.constants()[constant_parameter.id]
                            .r#type
                            .clone(),
                    );

                    let compatible = environment.compatible(
                        &constant_parameter_ty,
                        ty,
                        Variance::Covariant,
                    )?;

                    match compatible {
                        Some(compatible) => {
                            if !compatible
                                .result
                                .forall_lifetime_errors
                                .is_empty()
                            {
                                return Ok(None);
                            }

                            Ok(Some(Arc::new(Succeeded::with_constraints(
                                Satisfied,
                                compatible.constraints,
                            ))))
                        }
                        None => Ok(None),
                    }
                }

                (Type::Tuple(tuple_type), Constant::Tuple(tuple_const)) => {
                    'out: {
                        // check the element one by one
                        if tuple_type.elements.len()
                            == tuple_const.elements.len()
                        {
                            let mut constraints = BTreeSet::new();

                            for (ty, value) in tuple_type
                                .elements
                                .iter()
                                .zip(tuple_const.elements.iter())
                            {
                                // check the type of each element
                                if ty.is_unpacked != value.is_unpacked {
                                    break;
                                }

                                let Some(result) =
                                    environment.query(&Self::new(
                                        value.term.clone(),
                                        ty.term.clone(),
                                    ))?
                                else {
                                    break 'out;
                                };

                                constraints
                                    .extend(result.constraints.iter().cloned());
                            }

                            return Ok(Some(Arc::new(
                                Succeeded::satisfied_with(constraints),
                            )));
                        }
                    }

                    if let Some(result) = tuple_type_check_unpacked(
                        tuple_type,
                        tuple_const,
                        environment,
                    )? {
                        return Ok(Some(result));
                    }

                    if let Some(result) = tuple_type_check_unpacked(
                        tuple_const,
                        tuple_type,
                        environment,
                    )? {
                        return Ok(Some(result));
                    }

                    Ok(None)
                }

                (Type::Phantom(_), Constant::Phantom) => {
                    Ok(Some(Arc::new(Succeeded::satisfied())))
                }

                _ => Ok(None),
            }
        })();

        match result {
            Ok(Some(result)) => {
                return Ok(Some(result));
            }

            Ok(None) => {}

            Err(err) => {
                return Err(err);
            }
        }

        for Succeeded { result: constant_eq, mut constraints } in
            environment.get_equivalences(&self.constant)?
        {
            if let Some(result) = environment
                .query(&Self::new(constant_eq, self.r#type.clone()))?
            {
                constraints.extend(result.constraints.iter().cloned());

                return Ok(Some(result));
            }
        }

        Ok(None)
    }
}
