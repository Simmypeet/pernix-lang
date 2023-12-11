//! Contains the definition of [`Lifetime`].

use enum_as_inner::EnumAsInner;

use super::{Model, Term};
use crate::{
    semantic::model::{Entity, Forall},
    symbol::LifetimeParameterID,
};

/// Represents a particular variable lifetime
#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Lifetime<S: Model> {
    /// A static lifetime, denoted by `'static`.
    Static,

    /// A lifetime to a named lifetime parameter, denoted by `'a`.
    Parameter(LifetimeParameterID),

    /// The lifetime defined in the function scope.
    Scoped(S::ScopedLifetime),

    /// Quantified lifetime, denoted by `for<'a> 'a`.
    Forall(Forall),

    /// Inferred lifetime.
    Inference(S::LifetimeInference),

    /// Error lifetime
    Error,
}

impl<M: Model> Term for Lifetime<M> {
    type Model = M;
}

impl<S: Model> Entity for Lifetime<S> {
    type Model = S;
    type Rebind<A: Model> = Lifetime<A>;

    fn into_other_model<T: Model>(self) -> Self::Rebind<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LifetimeInference: Into<T::LifetimeInference>,
        S::ScopedLifetime: Into<T::ScopedLifetime>,
    {
        match self {
            Self::Static => Lifetime::Static,
            Self::Parameter(id) => Lifetime::Parameter(id),
            Self::Scoped(lifetime) => Lifetime::Scoped(lifetime.into()),
            Self::Forall(lifetime) => Lifetime::Forall(lifetime),
            Self::Inference(lifetime) => Lifetime::Inference(lifetime.into()),
            Self::Error => Lifetime::Error,
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::Rebind<T>>
    where
        S::ConstantInference: TryInto<T::ConstantInference>,
        S::TypeInference: TryInto<T::TypeInference>,
        S::LifetimeInference: TryInto<T::LifetimeInference>,
        S::ScopedLifetime: TryInto<T::ScopedLifetime>,
    {
        match self {
            Self::Static => Some(Lifetime::Static),
            Self::Parameter(id) => Some(Lifetime::Parameter(id)),
            Self::Forall(lifetime) => Some(Lifetime::Forall(lifetime)),
            Self::Scoped(lifetime) => lifetime.try_into().ok().map(Lifetime::Scoped),
            Self::Inference(lifetime) => lifetime.try_into().ok().map(Lifetime::Inference),
            Self::Error => Some(Lifetime::Error),
        }
    }
}
