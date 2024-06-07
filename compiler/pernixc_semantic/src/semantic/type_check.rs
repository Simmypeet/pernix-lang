use super::{
    equality::equals_impl,
    instantiation::MismatchedGenericArgumentCountError,
    model::Model,
    normalizer::Normalizer,
    session::{self, Limit, Session},
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, ModelOf, Symbol,
        Term,
    },
    Environment, ExceedLimitError, Satisfied,
};
use crate::{
    arena::ID,
    semantic::{
        get_equivalences_impl,
        instantiation::{self, Instantiation},
        term::{constant, r#type, Tuple},
    },
    symbol::{
        self,
        table::{representation::Index, State},
        ConstantParameterID,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Query<'a, M: Model> {
    pub constant: &'a Constant<M>,
    pub r#type: &'a Type<M>,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum Error {
    #[error(transparent)]
    ExceedLimit(#[from] ExceedLimitError),

    #[error(
        "the struct value contains {actual} fields, but the struct expects \
         {expected} fields"
    )]
    StructFieldCountMismatch {
        struct_id: ID<symbol::Struct>,
        expected: usize,
        actual: usize,
    },

    #[error("found a struct ID that does not exist in the symbol table")]
    InvalidStructID(ID<symbol::Struct>),

    #[error("found a variant ID that does not exist in the symbol table")]
    InvalidVariantID(ID<symbol::Variant>),

    #[error(transparent)]
    MismatchedGenericArgumentCount(#[from] MismatchedGenericArgumentCountError),

    #[error(
        "the variant either expects an associated value or not but got the \
         opposite"
    )]
    VariantAssociatedValueMismatch {
        /// The variant ID.
        variant_id: ID<symbol::Variant>,

        /// The expected associated value.
        expected: bool,

        /// The actual associated value.
        actual: bool,
    },

    #[error("the given constant parameter ID is invalid")]
    InvalidConstantParameter(ConstantParameterID),
}

trait TypeCheck<T>: ModelOf
where
    T: Term<Model = Self::Model>,
{
    fn type_check(
        &self,
        another: &T,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        limit: &mut Limit<
            impl Session<Lifetime<Self::Model>>
                + Session<Type<Self::Model>>
                + Session<Constant<Self::Model>>,
        >,
    ) -> Result<bool, Error>;
}

impl<M: Model> TypeCheck<Type<M>> for Constant<M> {
    fn type_check(
        &self,
        another: &Type<M>,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        limit: &mut Limit<
            impl Session<Lifetime<Self::Model>>
                + Session<Type<Self::Model>>
                + Session<Constant<Self::Model>>,
        >,
    ) -> Result<bool, Error> {
        type_check_impl(self, another, environment, limit)
    }
}

fn tuple_type_check_unpacked<T: Term, U>(
    this: &Tuple<T>,
    another: &Tuple<U>,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    limit: &mut Limit<
        impl Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<U::Model>>,
    >,
) -> Result<bool, Error>
where
    T: TypeCheck<U>,
    U: Term<Model = T::Model>,
    Tuple<T>: Into<T>,
    Tuple<U>: Into<U>,
{
    let this_unpacked_count =
        this.elements.iter().filter(|value| value.is_unpacked).count();

    if this_unpacked_count != 1 || another.elements.len() > this.elements.len()
    {
        return Ok(false);
    }

    // find the unpacked position
    let unpacked_position =
        this.elements.iter().position(|ty| ty.is_unpacked).unwrap();

    let head_range = 0..unpacked_position;
    let type_trail_range = (unpacked_position + 1)..this.elements.len();
    let value_trail_range = (another.elements.len()
        - type_trail_range.clone().len())
        ..another.elements.len();
    let value_unpack_range = unpacked_position..value_trail_range.start;

    // check the head
    for (this, another) in this.elements[head_range.clone()]
        .iter()
        .zip(&another.elements[head_range.clone()])
    {
        // no unpacked
        if another.is_unpacked {
            return Ok(false);
        }

        if this.term.type_check(&another.term, environment, limit)? {
            return Ok(false);
        }
    }

    // check the trail
    for (this, another) in this.elements[type_trail_range.clone()]
        .iter()
        .zip(&another.elements[value_trail_range.clone()])
    {
        // no unpacked
        if another.is_unpacked {
            return Ok(false);
        }

        if this.term.type_check(&another.term, environment, limit)? {
            return Ok(false);
        }
    }

    // check the unpacked
    let packed_another = Tuple {
        elements: another.elements[value_unpack_range.clone()]
            .iter()
            .map(|value| value.clone())
            .collect::<Vec<_>>(),
    }
    .into();

    this.elements.get(unpacked_position).unwrap().term.type_check(
        &packed_another,
        environment,
        limit,
    )
}

impl<M: Model> TypeCheck<Constant<M>> for Type<M> {
    fn type_check(
        &self,
        another: &Constant<M>,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        limit: &mut Limit<
            impl Session<Lifetime<Self::Model>>
                + Session<Type<Self::Model>>
                + Session<Constant<Self::Model>>,
        >,
    ) -> Result<bool, Error> {
        type_check_impl(another, self, environment, limit)
    }
}

/// Performs the type check on the constant argument.
pub(super) fn type_check_impl<M: Model>(
    constant: &Constant<M>,
    ty: &Type<M>,
    environment: &Environment<M, impl State, impl Normalizer<M>>,
    limit: &mut Limit<
        impl Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
    >,
) -> Result<bool, Error> {
    match limit.mark_as_in_progress::<Type<_>, _>(
        Query { constant: &constant, r#type: &ty },
        (),
    )? {
        Some(session::Cached::Done(Satisfied)) => return Ok(true),
        Some(session::Cached::InProgress(())) => return Ok(false),
        None => {}
    }

    let result = (|| -> Result<bool, Error> {
        match (ty, constant) {
            // check integer and boolean
            (
                Type::Primitive(primitive_ty),
                Constant::Primitive(primitive_const),
            ) => match primitive_const {
                constant::Primitive::Integer(value) => match primitive_ty {
                    r#type::Primitive::Int8
                    | r#type::Primitive::Int16
                    | r#type::Primitive::Int32
                    | r#type::Primitive::Int64
                    | r#type::Primitive::Isize => Ok(true),

                    r#type::Primitive::Uint8
                    | r#type::Primitive::Uint16
                    | r#type::Primitive::Uint32
                    | r#type::Primitive::Uint64
                    | r#type::Primitive::Usize => Ok(value.is_positive()),

                    r#type::Primitive::Float32
                    | r#type::Primitive::Float64
                    | r#type::Primitive::Bool => Ok(false),
                },

                constant::Primitive::Bool(_) => {
                    Ok(*primitive_ty == r#type::Primitive::Bool)
                }
            },

            // check struct type
            (
                Type::Symbol(Symbol {
                    id: r#type::SymbolID::Struct(struct_id),
                    generic_arguments,
                }),
                Constant::Struct(value),
            ) => {
                // early return if the mismatched id
                if *struct_id != value.id {
                    return Ok(false);
                }

                let struct_sym = environment
                    .table
                    .get(*struct_id)
                    .ok_or(Error::InvalidStructID(*struct_id))?;

                // mismatched field count
                if struct_sym.fields().len() != value.fields.len() {
                    return Err(Error::StructFieldCountMismatch {
                        struct_id: *struct_id,
                        expected: struct_sym.fields().len(),
                        actual: value.fields.len(),
                    });
                }

                // create instantiation to check the fields
                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments.clone(),
                    (*struct_id).into(),
                    &struct_sym.generic_declaration.parameters,
                )?;

                for (field_value, (_, field_sym)) in
                    value.fields.iter().zip(struct_sym.fields_as_order())
                {
                    // check the field type
                    let mut field_type =
                        M::from_default_type(field_sym.r#type.clone());

                    instantiation::instantiate(&mut field_type, &instantiation);

                    if !type_check_impl(
                        field_value,
                        &field_type,
                        environment,
                        limit,
                    )? {
                        return Ok(false);
                    }
                }

                Ok(true)
            }

            // check enum type
            (
                Type::Symbol(Symbol {
                    id: r#type::SymbolID::Enum(enum_id),
                    generic_arguments,
                }),
                Constant::Enum(enum_value),
            ) => {
                let variant_sym = environment
                    .table
                    .get(enum_value.variant_id)
                    .ok_or(Error::InvalidVariantID(enum_value.variant_id))?;

                if *enum_id != variant_sym.parent_enum_id() {
                    return Ok(false);
                }

                let enum_sym = environment.table.get(*enum_id).unwrap();

                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments.clone(),
                    (*enum_id).into(),
                    &enum_sym.generic_declaration.parameters,
                )?;

                // mismatched associated value
                match (
                    &variant_sym.associated_type,
                    &enum_value.associated_value,
                ) {
                    (None, None) => Ok(true),

                    (Some(ty), Some(value)) => {
                        let mut associated_ty =
                            M::from_default_type(ty.clone());

                        instantiation::instantiate(
                            &mut associated_ty,
                            &instantiation,
                        );

                        type_check_impl(
                            &value,
                            &associated_ty,
                            environment,
                            limit,
                        )
                    }

                    (ty, value) => Err(Error::VariantAssociatedValueMismatch {
                        variant_id: enum_value.variant_id,
                        expected: ty.is_some(),
                        actual: value.is_some(),
                    }),
                }
            }

            // check array type
            (Type::Array(array_ty), Constant::Array(array_value)) => {
                let len = Constant::Primitive(constant::Primitive::Integer(
                    array_value.elements.len() as i128,
                ));

                // check the length equals
                if !equals_impl(&len, &array_ty.length, environment, limit)? {
                    return Ok(false);
                }

                // check the type of each element

                for value in &array_value.elements {
                    if !type_check_impl(
                        value,
                        &array_ty.r#type,
                        environment,
                        limit,
                    )? {
                        return Ok(false);
                    }
                }

                Ok(true)
            }

            (ty, Constant::Parameter(constant_parameter)) => {
                let generic_sym = environment
                    .table
                    .get_generic(constant_parameter.parent)
                    .ok_or(Error::InvalidConstantParameter(
                        *constant_parameter,
                    ))?;

                let constant_parameter_sym = generic_sym
                    .generic_declaration()
                    .parameters
                    .constants()
                    .get(constant_parameter.id)
                    .ok_or(Error::InvalidConstantParameter(
                        *constant_parameter,
                    ))?;

                Ok(equals_impl(
                    ty,
                    &M::from_default_type(
                        constant_parameter_sym.r#type.clone(),
                    ),
                    environment,
                    limit,
                )?)
            }

            // the local type and local constant
            (Type::Local(local_type), Constant::Local(local_const)) => {
                type_check_impl(
                    &local_const.0,
                    &local_type.0,
                    environment,
                    limit,
                )
            }

            (Type::Tuple(tuple_type), Constant::Tuple(tuple_const)) => {
                // check the element one by one
                if tuple_type.elements.len() == tuple_const.elements.len() {
                    for (ty, value) in tuple_type
                        .elements
                        .iter()
                        .zip(tuple_const.elements.iter())
                    {
                        // check the type of each element
                        if ty.is_unpacked != value.is_unpacked {
                            break;
                        }

                        if !type_check_impl(
                            &value.term,
                            &ty.term,
                            environment,
                            limit,
                        )? {
                            break;
                        }
                    }

                    return Ok(true);
                }

                if tuple_type_check_unpacked(
                    tuple_type,
                    tuple_const,
                    environment,
                    limit,
                )? {
                    return Ok(true);
                }

                if tuple_type_check_unpacked(
                    tuple_const,
                    tuple_type,
                    environment,
                    limit,
                )? {
                    return Ok(true);
                }

                Ok(false)
            }

            (Type::Phantom(_), Constant::Phantom) => Ok(true),

            _ => Ok(false),
        }
    })();

    match result {
        Ok(true) => {
            // done, return true
            limit.mark_as_done::<Type<_>, _>(
                Query { constant: &constant, r#type: &ty },
                Satisfied,
            );

            return Ok(true);
        }

        Ok(false) => {}

        Err(Error::ExceedLimit(err)) => return Err(err.into()),

        Err(err) => {
            limit.mark_as_done::<Type<_>, _>(
                Query { constant: &constant, r#type: &ty },
                Satisfied,
            );

            return Err(err);
        }
    }

    for constant_eq in get_equivalences_impl(constant, environment, limit)? {
        if type_check_impl(&constant_eq, ty, environment, limit)? {
            limit.mark_as_done::<Type<_>, _>(
                Query { constant: &constant, r#type: &ty },
                Satisfied,
            );

            return Ok(true);
        }
    }

    for type_eq in get_equivalences_impl(ty, environment, limit)? {
        if type_check_impl(constant, &type_eq, environment, limit)? {
            limit.mark_as_done::<Type<_>, _>(
                Query { constant: &constant, r#type: &ty },
                Satisfied,
            );

            return Ok(true);
        }
    }

    limit.clear_query::<Type<_>, _>(Query { constant: &constant, r#type: &ty });
    Ok(false)
}
