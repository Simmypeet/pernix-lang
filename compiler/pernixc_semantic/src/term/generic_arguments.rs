//! Contains the definition of the [`GenericArguments`].

use serde::{Deserialize, Serialize};

use crate::{
    table,
    term::{
        constant::Constant,
        lifetime::Lifetime,
        r#type::Type,
        table::{DisplayObject, Table},
        Default, Model, ModelOf,
    },
};

mod arbitrary;

/// Represents a generic arguments supplied to a term (i.e., `type[ARGS]`).
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct GenericArguments<M: Model> {
    /// The lifetimes supplied to the term.
    pub lifetimes: Vec<Lifetime<M>>,

    /// The types supplied to the term.
    pub types: Vec<Type<M>>,

    /// The constants supplied to the term.
    pub constants: Vec<Constant<M>>,
}

impl<M: Model> table::Display for GenericArguments<M>
where
    Lifetime<M>: table::Display,
    Type<M>: table::Display,
    Constant<M>: table::Display,
{
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        if self.lifetimes.is_empty()
            && self.types.is_empty()
            && self.constants.is_empty()
        {
            return Ok(());
        }

        let mut lifetimes = self.lifetimes.iter().peekable();
        let mut types = self.types.iter().peekable();
        let mut constants = self.constants.iter().peekable();

        write!(f, "[")?;

        while let Some(lifetime) = lifetimes.next() {
            let is_last = lifetimes.peek().is_none()
                && self.types.is_empty()
                && self.constants.is_empty();

            write!(f, "{}", DisplayObject { table, display: lifetime })?;

            if !is_last {
                write!(f, ", ")?;
            }
        }

        while let Some(r#type) = types.next() {
            let is_last = types.peek().is_none() && self.constants.is_empty();

            write!(f, "{}", DisplayObject { table, display: r#type })?;

            if !is_last {
                write!(f, ", ")?;
            }
        }

        while let Some(constant) = constants.next() {
            let is_last = constants.peek().is_none();

            write!(f, "{}", DisplayObject { table, display: constant })?;

            if !is_last {
                write!(f, ", ")?;
            }
        }

        write!(f, "]")
    }
}

impl<M: Model> ModelOf for GenericArguments<M> {
    type Model = M;
    type Rebind<U: Model> = GenericArguments<U>;

    /// Converts a generic arguments with the model `U` into the model `M`.
    fn from_other_model<U: Model>(term: GenericArguments<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        Self {
            lifetimes: term
                .lifetimes
                .into_iter()
                .map(Lifetime::from_other_model)
                .collect(),
            types: term.types.into_iter().map(Type::from_other_model).collect(),
            constants: term
                .constants
                .into_iter()
                .map(Constant::from_other_model)
                .collect(),
        }
    }

    /// Tries to convert a generic arguments with the model `U` into the model
    /// `M`.
    ///
    /// # Errors
    ///
    /// Returns an error returned by the `TryFrom` implementation of the model.
    fn try_from_other_model<U: Model, E>(
        term: GenericArguments<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self {
            lifetimes: term
                .lifetimes
                .into_iter()
                .map(Lifetime::try_from_other_model)
                .collect::<Result<Vec<_>, _>>()?,
            types: term
                .types
                .into_iter()
                .map(Type::try_from_other_model)
                .collect::<Result<Vec<_>, _>>()?,
            constants: term
                .constants
                .into_iter()
                .map(Constant::try_from_other_model)
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

impl<M: Model> GenericArguments<M> {
    /// Checks if there's any errornous term in the generic arguments.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.lifetimes.iter().any(Lifetime::is_error)
            || self.types.iter().any(Type::is_error)
            || self.constants.iter().any(Constant::is_error)
    }

    /// Converts the generic arguments with the default model into the model `M`
    #[must_use]
    pub fn from_default_model(
        generic_arguments: GenericArguments<Default>,
    ) -> Self {
        Self {
            lifetimes: generic_arguments
                .lifetimes
                .into_iter()
                .map(|x| M::from_default_lifetime(x))
                .collect(),
            types: generic_arguments
                .types
                .into_iter()
                .map(|x| M::from_default_type(x))
                .collect(),
            constants: generic_arguments
                .constants
                .into_iter()
                .map(|x| M::from_default_constant(x))
                .collect(),
        }
    }
}

/// A trait for retrieving the arguments array from a generic arguments.
#[allow(missing_docs)]
pub trait Element<M: Model> {
    fn get(generic_arguments: &GenericArguments<M>) -> &[Self]
    where
        Self: Sized;

    fn get_mut(generic_arguments: &mut GenericArguments<M>) -> &mut Vec<Self>
    where
        Self: Sized;
}

impl<M: Model> Element<M> for Lifetime<M> {
    fn get(generic_arguments: &GenericArguments<M>) -> &[Self] {
        &generic_arguments.lifetimes
    }

    fn get_mut(generic_arguments: &mut GenericArguments<M>) -> &mut Vec<Self> {
        &mut generic_arguments.lifetimes
    }
}

impl<M: Model> Element<M> for Type<M> {
    fn get(generic_arguments: &GenericArguments<M>) -> &[Self] {
        &generic_arguments.types
    }

    fn get_mut(generic_arguments: &mut GenericArguments<M>) -> &mut Vec<Self> {
        &mut generic_arguments.types
    }
}

impl<M: Model> Element<M> for Constant<M> {
    fn get(generic_arguments: &GenericArguments<M>) -> &[Self] {
        &generic_arguments.constants
    }

    fn get_mut(generic_arguments: &mut GenericArguments<M>) -> &mut Vec<Self> {
        &mut generic_arguments.constants
    }
}
