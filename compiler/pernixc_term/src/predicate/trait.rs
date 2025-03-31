use derive_new::new;
use pernixc_semantic::{DisplayObject, GlobalID, Table};
use serde::{Deserialize, Serialize};

use super::contains_error;
use crate::{
    generic_arguments::GenericArguments, instantiation::Instantiation,
    lifetime::Lifetime, Model, ModelOf,
};

/// Represents a predicate stating that there exists an implementation for the
/// given trait and generic arguments
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    new,
)]
pub struct Positive<M: Model> {
    /// The trait to be implemented.
    pub trait_id: GlobalID,

    /// Whether the implementation is const.
    pub is_const: bool,

    /// The generic arguments supplied to the trait.
    pub generic_arguments: GenericArguments<M>,
}

impl<M: Model> ModelOf for Positive<M> {
    type Model = M;
    type Rebind<U: Model> = Positive<U>;

    fn from_other_model<U: Model>(term: Self::Rebind<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        Self {
            trait_id: term.trait_id,
            is_const: term.is_const,
            generic_arguments: GenericArguments::from_other_model(
                term.generic_arguments,
            ),
        }
    }

    fn try_from_other_model<U: Model, E>(
        term: Self::Rebind<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self {
            trait_id: term.trait_id,
            is_const: term.is_const,
            generic_arguments: GenericArguments::try_from_other_model(
                term.generic_arguments,
            )?,
        })
    }
}

impl<M: Model> Positive<M> {
    /// Checks if the trait contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.generic_arguments.lifetimes.iter().any(Lifetime::is_error)
            || self.generic_arguments.types.iter().any(contains_error)
            || self.generic_arguments.constants.iter().any(contains_error)
    }

    /// Applies an instantiation to the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation<M>) {
        self.generic_arguments.instantiate(instantiation);
    }
}

impl<M: Model> pernixc_semantic::Display for Positive<M>
where
    GenericArguments<M>: pernixc_semantic::Display,
{
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "trait ")?;

        if self.is_const {
            write!(f, "const ")?;
        }

        write!(
            f,
            "{}{}",
            table.get_qualified_name(self.trait_id),
            DisplayObject { display: &self.generic_arguments, table }
        )
    }
}

/// Represents a predicate stating that there exists no implementation for the
/// given trait and generic arguments
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    new,
)]
pub struct Negative<M: Model> {
    /// The trait in question.
    pub trait_id: GlobalID,

    /// The generic arguments supplied to the trait.
    pub generic_arguments: GenericArguments<M>,
}

impl<M: Model> ModelOf for Negative<M> {
    type Model = M;
    type Rebind<U: Model> = Negative<U>;

    fn from_other_model<U: Model>(term: Self::Rebind<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        Self {
            trait_id: term.trait_id,
            generic_arguments: GenericArguments::from_other_model(
                term.generic_arguments,
            ),
        }
    }

    fn try_from_other_model<U: Model, E>(
        term: Self::Rebind<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self {
            trait_id: term.trait_id,
            generic_arguments: GenericArguments::try_from_other_model(
                term.generic_arguments,
            )?,
        })
    }
}

impl<M: Model> Negative<M> {
    /// Checks if the trait contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.generic_arguments.lifetimes.iter().any(Lifetime::is_error)
            || self.generic_arguments.types.iter().any(contains_error)
            || self.generic_arguments.constants.iter().any(contains_error)
    }

    /// Applies an instantiation to the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation<M>) {
        self.generic_arguments.instantiate(instantiation);
    }
}

impl<M: Model> pernixc_semantic::Display for Negative<M>
where
    GenericArguments<M>: pernixc_semantic::Display,
{
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "trait !")?;

        write!(
            f,
            "{}{}",
            table.get_qualified_name(self.trait_id),
            DisplayObject { display: &self.generic_arguments, table }
        )
    }
}
