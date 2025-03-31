use pernixc_semantic::{DisplayObject, Table};
use serde::{Deserialize, Serialize};

use super::contains_error;
use crate::{
    instantiation::{self, Instantiation},
    visitor, Model, ModelOf,
};

/// The predicate meaning that the term is a tuple and is unpackable.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Tuple<T>(pub T);

impl<T: pernixc_semantic::Display> pernixc_semantic::Display for Tuple<T> {
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "tuple {}", DisplayObject { display: &self.0, table })
    }
}

impl<T: ModelOf> ModelOf for Tuple<T> {
    type Model = T::Model;
    type Rebind<U: Model> = Tuple<T::Rebind<U>>;

    fn from_other_model<U: Model>(term: Self::Rebind<U>) -> Self
    where
        <Self::Model as Model>::LifetimeInference: From<U::LifetimeInference>,
        <Self::Model as Model>::TypeInference: From<U::TypeInference>,
        <Self::Model as Model>::ConstantInference: From<U::ConstantInference>,
    {
        Self(T::from_other_model(term.0))
    }

    fn try_from_other_model<U: Model, E>(
        term: Self::Rebind<U>,
    ) -> Result<Self, E>
    where
        <Self::Model as Model>::LifetimeInference:
            TryFrom<U::LifetimeInference, Error = E>,
        <Self::Model as Model>::TypeInference:
            TryFrom<U::TypeInference, Error = E>,
        <Self::Model as Model>::ConstantInference:
            TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self(T::try_from_other_model(term.0)?))
    }
}

impl<T> Tuple<T> {
    /// Checks if the term contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool
    where
        T: visitor::Element,
    {
        contains_error(&self.0)
    }

    /// Applies a instantiation to the [`Tuple`] term.
    pub fn instantiate(&mut self, instantiation: &Instantiation<T::Model>)
    where
        T: ModelOf + instantiation::Element + visitor::Element + Clone,
    {
        instantiation::instantiate(&mut self.0, instantiation);
    }
}
