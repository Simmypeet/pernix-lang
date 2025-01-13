use derive_new::new;
use serde::{Deserialize, Serialize};

use crate::{Model, ModelOf};

/// A predicate representing compatible equality between two values.
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
#[allow(missing_docs)]
pub struct Compatible<T, U = T> {
    pub lhs: T,
    pub rhs: U,
}

impl<M: Model, T, U> ModelOf for Compatible<T, U>
where
    T: ModelOf<Model = M>,
    U: ModelOf<Model = M>,
{
    type Model = M;
    type Rebind<V: Model> = Compatible<T::Rebind<V>, U::Rebind<V>>;

    fn from_other_model<W: Model>(term: Self::Rebind<W>) -> Self
    where
        M::LifetimeInference: From<W::LifetimeInference>,
        M::TypeInference: From<W::TypeInference>,
        M::ConstantInference: From<W::ConstantInference>,
    {
        Self {
            lhs: T::from_other_model(term.lhs),
            rhs: U::from_other_model(term.rhs),
        }
    }

    fn try_from_other_model<W: Model, E>(
        term: Self::Rebind<W>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<W::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<W::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<W::ConstantInference, Error = E>,
    {
        Ok(Self {
            lhs: T::try_from_other_model(term.lhs)?,
            rhs: U::try_from_other_model(term.rhs)?,
        })
    }
}
