//! Contains the definition of [`Lifetime`].

use enum_as_inner::EnumAsInner;

use super::{predicate::Forall, Entity, Model, Substitution};
use crate::symbol::LifetimeParameterID;

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
}

impl<S: Model> Entity<S> for Lifetime<S> {
    type This<A: Model> = Lifetime<A>;

    fn into_other_model<T: Model>(self) -> Self::This<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::ScopedLifetime: Into<T::ScopedLifetime>,
    {
        match self {
            Self::Static => Lifetime::Static,
            Self::Parameter(id) => Lifetime::Parameter(id),
            Self::Scoped(local) => Lifetime::Scoped(local.into()),
            Self::Forall(forall) => Lifetime::Forall(forall),
        }
    }

    fn apply(&mut self, substitution: &Substitution<S>) {
        if let Some(new) = substitution.lifetimes.get(self) {
            *self = new.clone();
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::This<T>>
    where
        <S as Model>::ConstantInference: TryInto<T::ConstantInference>,
        <S as Model>::TypeInference: TryInto<T::TypeInference>,
        <S as Model>::ScopedLifetime: TryInto<T::ScopedLifetime>,
    {
        match self {
            Self::Static => Some(Lifetime::Static),
            Self::Parameter(id) => Some(Lifetime::Parameter(id)),
            Self::Scoped(local) => local.try_into().ok().map(Lifetime::Scoped),
            Self::Forall(forall) => Some(Lifetime::Forall(forall)),
        }
    }
}
