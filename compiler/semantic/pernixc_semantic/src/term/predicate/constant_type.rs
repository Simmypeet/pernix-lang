use serde::{Deserialize, Serialize};

use super::contains_error;
use crate::{
    table::{self, DisplayObject, Table},
    term::{
        instantiation::{self, Instantiation},
        r#type::Type,
        Model, ModelOf,
    },
};

/// Represents a type can be used as a type of a compile-time constant value.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct ConstantType<M: Model>(pub Type<M>);

impl<M: Model> table::Display for ConstantType<M>
where
    Type<M>: table::Display,
{
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "const type {}", DisplayObject { display: &self.0, table })
    }
}

impl<M: Model> ModelOf for ConstantType<M> {
    type Model = M;
    type Rebind<U: Model> = ConstantType<U>;

    fn from_other_model<U: Model>(term: Self::Rebind<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        Self(Type::from_other_model(term.0))
    }

    fn try_from_other_model<U: Model, E>(
        term: Self::Rebind<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self(Type::try_from_other_model(term.0)?))
    }
}

impl<M: Model> ConstantType<M> {
    /// Checks if the type contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool { contains_error(&self.0) }

    /// Applies the instantiation to the type.
    pub fn instantiate(&mut self, instantiation: &Instantiation<M>) {
        instantiation::instantiate(&mut self.0, instantiation);
    }
}
