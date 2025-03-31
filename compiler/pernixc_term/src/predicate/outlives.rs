use pernixc_semantic::{DisplayObject, Table};
use serde::{Deserialize, Serialize};

use super::contains_error;
use crate::{
    instantiation::{self, Instantiation},
    lifetime::Lifetime,
    visitor, Model, ModelOf,
};

/// A predicate that a term outlives a lifetime.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Outlives<T: ModelOf> {
    /// The term that must outlive the bound.
    pub operand: T,

    /// The lifetime that the term must outlive.
    pub bound: Lifetime<T::Model>,
}

impl<T: ModelOf> Outlives<T> {
    /// Creates a new outlives predicate.
    #[must_use]
    pub const fn new(operand: T, bound: Lifetime<T::Model>) -> Self {
        Self { operand, bound }
    }
}

impl<T: pernixc_semantic::Display + ModelOf> pernixc_semantic::Display
    for Outlives<T>
where
    Lifetime<T::Model>: pernixc_semantic::Display,
{
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}: {}",
            DisplayObject { display: &self.operand, table },
            DisplayObject { display: &self.bound, table },
        )
    }
}

impl<T: ModelOf> ModelOf for Outlives<T> {
    type Model = T::Model;
    type Rebind<U: Model> = Outlives<T::Rebind<U>>;

    fn from_other_model<U: Model>(term: Self::Rebind<U>) -> Self
    where
        <T::Model as Model>::LifetimeInference: From<U::LifetimeInference>,
        <T::Model as Model>::TypeInference: From<U::TypeInference>,
        <T::Model as Model>::ConstantInference: From<U::ConstantInference>,
    {
        Self {
            operand: T::from_other_model(term.operand),
            bound: Lifetime::from_other_model(term.bound),
        }
    }

    fn try_from_other_model<U: Model, E>(
        term: Self::Rebind<U>,
    ) -> Result<Self, E>
    where
        <T::Model as Model>::LifetimeInference:
            TryFrom<U::LifetimeInference, Error = E>,
        <T::Model as Model>::TypeInference:
            TryFrom<U::TypeInference, Error = E>,
        <T::Model as Model>::ConstantInference:
            TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self {
            operand: T::try_from_other_model(term.operand)?,
            bound: Lifetime::try_from_other_model(term.bound)?,
        })
    }
}

impl<T: ModelOf> Outlives<T> {
    /// Checks if the term contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool
    where
        T: visitor::Element,
    {
        contains_error(&self.operand) || self.bound.is_forall()
    }

    /// Applies a instantiation to the [`Outlives::operand`] and
    /// [`Outlives::bound`].
    pub fn instantiate(&mut self, instantiation: &Instantiation<T::Model>)
    where
        T: instantiation::Element + visitor::Element + Clone,
    {
        instantiation::instantiate(&mut self.operand, instantiation);
        instantiation::instantiate(&mut self.bound, instantiation);
    }
}
