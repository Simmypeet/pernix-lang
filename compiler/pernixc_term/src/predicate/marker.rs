use derive_new::new;
use pernixc_semantic::{DisplayObject, GlobalID, Table};
use serde::{Deserialize, Serialize};

use crate::{
    generic_arguments::GenericArguments, instantiation::Instantiation, Model,
    ModelOf,
};

/// Predicates specifying that the marker is satisfied.
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
    /// The id of the marker.
    pub marker_id: GlobalID,

    /// The generic arguments supplied to the marker.
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
            marker_id: term.marker_id,
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

        Self: Sized,
    {
        Ok(Self {
            marker_id: term.marker_id,
            generic_arguments: GenericArguments::try_from_other_model(
                term.generic_arguments,
            )?,
        })
    }
}

impl<M: Model> Positive<M> {
    /// Checks if there's an error in the generic arguments.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.generic_arguments.contains_error()
    }

    /// Applies the instantiation to the generic arguments.
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
        write!(
            f,
            "marker {}{}",
            table.get_qualified_name(self.marker_id),
            DisplayObject { display: &self.generic_arguments, table }
        )
    }
}

/// The predicate specifying that the marker will never be satisfied.
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
    /// The id of the marker.
    pub marker_id: GlobalID,

    /// The generic arguments supplied to the marker.
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
            marker_id: term.marker_id,
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
            marker_id: term.marker_id,
            generic_arguments: GenericArguments::try_from_other_model(
                term.generic_arguments,
            )?,
        })
    }
}

impl<M: Model> Negative<M> {
    /// Checks if there's an error in the generic arguments.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.generic_arguments.contains_error()
    }

    /// Applies the instantiation to the generic arguments.
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
        write!(
            f,
            "marker !{}{}",
            table.get_qualified_name(self.marker_id),
            DisplayObject { display: &self.generic_arguments, table }
        )
    }
}
