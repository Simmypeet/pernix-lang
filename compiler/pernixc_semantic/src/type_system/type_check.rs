//! Contains the code related to type checking the [`Constant`].

use std::collections::HashSet;

use super::{
    equality::Equality,
    instantiation::MismatchedGenericArgumentCountError,
    model::Model,
    normalizer::Normalizer,
    query::Context,
    term::{constant::Constant, r#type::Type, ModelOf, Symbol, Term},
    Compute, Environment, Output, OverflowError, Satisfied, Succeeded,
};
use crate::{
    arena::ID,
    type_system::{
        get_equivalences_with_context,
        instantiation::{self, Instantiation},
        term::{constant, r#type, Tuple},
    },
    symbol::{
        self,
        table::{representation::Index, State},
        ConstantParameterID,
    },
};

/// A query for checking the type of a compile-time constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeCheck<M: Model> {
    /// The constant value to check.
    pub constant: Constant<M>,

    /// The type to check against.
    pub r#type: Type<M>,
}

impl<M: Model> TypeCheck<M> {
    /// Creates a new type check query.
    #[must_use]
    pub fn new(constant: Constant<M>, r#type: Type<M>) -> Self {
        Self { constant, r#type }
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    Overflow(#[from] OverflowError),

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

trait Check<T>: ModelOf
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
        context: &mut Context<Self::Model>,
    ) -> Result<Output<Satisfied, T::Model>, Error>;
}

impl<M: Model> Check<Type<M>> for Constant<M> {
    fn type_check(
        &self,
        another: &Type<M>,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        context: &mut Context<Self::Model>,
    ) -> Result<Output<Satisfied, M>, Error> {
        TypeCheck::new(self.clone(), another.clone())
            .query_with_context(environment, context)
    }
}

fn tuple_type_check_unpacked<T, U>(
    this: &Tuple<T>,
    another: &Tuple<U>,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    context: &mut Context<T::Model>,
) -> Result<Output<Satisfied, T::Model>, Error>
where
    T: Check<U> + Term,
    U: Term<Model = T::Model>,
    Tuple<T>: Into<T>,
    Tuple<U>: Into<U>,
{
    // list of all outlives constraints
    let mut constraints = HashSet::new();

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
            return Ok(None);
        }

        let Some(result) =
            this.term.type_check(&another.term, environment, context)?
        else {
            return Ok(None);
        };

        constraints.extend(result.constraints);
    }

    // check the trail
    for (this, another) in this.elements[type_trail_range.clone()]
        .iter()
        .zip(&another.elements[value_trail_range.clone()])
    {
        // no unpacked
        if another.is_unpacked {
            return Ok(None);
        }

        let Some(result) =
            this.term.type_check(&another.term, environment, context)?
        else {
            return Ok(None);
        };

        constraints.extend(result.constraints);
    }

    // check the unpacked
    let packed_another = Tuple {
        elements: another.elements[value_unpack_range.clone()]
            .iter()
            .map(|value| value.clone())
            .collect::<Vec<_>>(),
    }
    .into();

    // check the unpacked
    let Some(result) = this
        .elements
        .get(unpacked_position)
        .unwrap()
        .term
        .type_check(&packed_another, environment, context)?
    else {
        return Ok(None);
    };
    constraints.extend(result.constraints);

    Ok(Some(Succeeded::with_constraints(Satisfied, constraints)))
}

impl<M: Model> Check<Constant<M>> for Type<M> {
    fn type_check(
        &self,
        another: &Constant<M>,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        context: &mut Context<M>,
    ) -> Result<Output<Satisfied, M>, Error> {
        TypeCheck::new(another.clone(), self.clone())
            .query_with_context(environment, context)
    }
}

impl<M: Model> Compute for TypeCheck<M> {
    type Error = Error;
    type Parameter = ();

    #[allow(private_bounds, private_interfaces)]
    fn implementation(
        &self,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        context: &mut Context<Self::Model>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Self::Result>, Self::Error> {
        let result = (|| -> Result<Output<Satisfied, M>, Error> {
            match (&self.r#type, &self.constant) {
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
                        | r#type::Primitive::Isize => {
                            Ok(Some(Succeeded::satisfied()))
                        }

                        r#type::Primitive::Uint8
                        | r#type::Primitive::Uint16
                        | r#type::Primitive::Uint32
                        | r#type::Primitive::Uint64
                        | r#type::Primitive::Usize => Ok(value
                            .is_positive()
                            .then_some(Succeeded::satisfied())),

                        r#type::Primitive::Float32
                        | r#type::Primitive::Float64
                        | r#type::Primitive::Bool => Ok(None),
                    },

                    constant::Primitive::Bool(_) => Ok((*primitive_ty
                        == r#type::Primitive::Bool)
                        .then_some(Succeeded::satisfied())),
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
                        return Ok(None);
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

                    let mut constraints = HashSet::new();

                    for (field_value, (_, field_sym)) in
                        value.fields.iter().zip(struct_sym.fields_as_order())
                    {
                        // check the field type
                        let mut field_type =
                            M::from_default_type(field_sym.r#type.clone());

                        instantiation::instantiate(
                            &mut field_type,
                            &instantiation,
                        );

                        let Some(result) =
                            TypeCheck::new(field_value.clone(), field_type)
                                .query_with_context(environment, context)?
                        else {
                            return Ok(None);
                        };

                        constraints.extend(result.constraints);
                    }

                    Ok(Some(Succeeded::with_constraints(
                        Satisfied,
                        constraints,
                    )))
                }

                // check enum type
                (
                    Type::Symbol(Symbol {
                        id: r#type::SymbolID::Enum(enum_id),
                        generic_arguments,
                    }),
                    Constant::Enum(enum_value),
                ) => {
                    let variant_sym =
                        environment.table.get(enum_value.variant_id).ok_or(
                            Error::InvalidVariantID(enum_value.variant_id),
                        )?;

                    if *enum_id != variant_sym.parent_enum_id() {
                        return Ok(None);
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
                        (None, None) => Ok(Some(Succeeded::satisfied())),

                        (Some(ty), Some(value)) => {
                            let mut associated_ty =
                                M::from_default_type(ty.clone());

                            instantiation::instantiate(
                                &mut associated_ty,
                                &instantiation,
                            );

                            TypeCheck::new((**value).clone(), associated_ty)
                                .query_with_context(environment, context)
                        }

                        (ty, value) => {
                            Err(Error::VariantAssociatedValueMismatch {
                                variant_id: enum_value.variant_id,
                                expected: ty.is_some(),
                                actual: value.is_some(),
                            })
                        }
                    }
                }

                // check array type
                (Type::Array(array_ty), Constant::Array(array_value)) => {
                    let len =
                        Constant::Primitive(constant::Primitive::Integer(
                            array_value.elements.len() as i128,
                        ));

                    // check the length equals
                    let Some(Succeeded { result: _, mut constraints }) =
                        Equality::new(len, array_ty.length.clone())
                            .query_with_context(environment, context)?
                    else {
                        return Ok(None);
                    };

                    // check the type of each element
                    for value in &array_value.elements {
                        let Some(result) = TypeCheck::new(
                            value.clone(),
                            (*array_ty.r#type).clone(),
                        )
                        .query_with_context(environment, context)?
                        else {
                            return Ok(None);
                        };

                        constraints.extend(result.constraints);
                    }

                    Ok(Some(Succeeded::with_constraints(
                        Satisfied,
                        constraints,
                    )))
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

                    Ok(Equality::new(
                        ty.clone(),
                        M::from_default_type(
                            constant_parameter_sym.r#type.clone(),
                        ),
                    )
                    .query_with_context(environment, context)?)
                }

                // the local type and local constant
                (Type::Local(local_type), Constant::Local(local_const)) => {
                    TypeCheck::new(
                        (*local_const.0).clone(),
                        (*local_type.0).clone(),
                    )
                    .query_with_context(environment, context)
                }

                (Type::Tuple(tuple_type), Constant::Tuple(tuple_const)) => {
                    'out: {
                        // check the element one by one
                        if tuple_type.elements.len()
                            == tuple_const.elements.len()
                        {
                            let mut constraints = HashSet::new();

                            for (ty, value) in tuple_type
                                .elements
                                .iter()
                                .zip(tuple_const.elements.iter())
                            {
                                // check the type of each element
                                if ty.is_unpacked != value.is_unpacked {
                                    break;
                                }

                                let Some(result) = TypeCheck::new(
                                    value.term.clone(),
                                    ty.term.clone(),
                                )
                                .query_with_context(environment, context)?
                                else {
                                    break 'out;
                                };

                                constraints.extend(result.constraints);
                            }

                            return Ok(Some(Succeeded::with_constraints(
                                Satisfied,
                                constraints,
                            )));
                        }
                    }

                    if let Some(result) = tuple_type_check_unpacked(
                        tuple_type,
                        tuple_const,
                        environment,
                        context,
                    )? {
                        return Ok(Some(result));
                    }

                    if let Some(result) = tuple_type_check_unpacked(
                        tuple_const,
                        tuple_type,
                        environment,
                        context,
                    )? {
                        return Ok(Some(result));
                    }

                    Ok(None)
                }

                (Type::Phantom(_), Constant::Phantom) => {
                    Ok(Some(Succeeded::satisfied()))
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

        for Succeeded { result: constant_eq, constraints } in
            get_equivalences_with_context(&self.constant, environment, context)?
        {
            if let Some(mut result) =
                TypeCheck::new(constant_eq, self.r#type.clone())
                    .query_with_context(environment, context)?
            {
                result.constraints.extend(constraints);

                return Ok(Some(result));
            }
        }

        for Succeeded { result: type_eq, constraints } in
            get_equivalences_with_context(&self.r#type, environment, context)?
        {
            if let Some(mut result) =
                TypeCheck::new(self.constant.clone(), type_eq)
                    .query_with_context(environment, context)?
            {
                result.constraints.extend(constraints);

                return Ok(Some(result));
            }
        }

        Ok(None)
    }
}
