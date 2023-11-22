//! Contains the definition of [`Region`].

use enum_as_inner::EnumAsInner;

use super::{Entity, Model, Substitution, predicate::Forall};
use crate::symbol::LifetimeParameterID;

/// Represents a particular variable region
#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Region<S: Model> {
    /// A static lifetime, denoted by `'static`.
    Static,

    /// A lifetime to a named lifetime parameter, denoted by `'a`.
    Named(LifetimeParameterID),

    /// The kind of region that depends on the particular context.
    Local(S::LocalRegion),

    /// Quantified region, denoted by `for<'a> 'a`.
    Forall(Forall),
}

impl<S: Model> Entity<S> for Region<S> {
    type This<A: Model> = Region<A>;

    fn into_other_model<T: Model>(self) -> Self::This<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LocalRegion: Into<T::LocalRegion>,
    {
        match self {
            Self::Static => Region::Static,
            Self::Named(id) => Region::Named(id),
            Self::Local(local) => Region::Local(local.into()),
            Self::Forall(forall) => Region::Forall(forall.into()),
        }
    }

    fn apply(&mut self, substitution: &Substitution<S>) {
        if let Some(new) = substitution.regions.get(self) {
            *self = new.clone();
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::This<T>>
    where
        <S as Model>::ConstantInference: TryInto<T::ConstantInference>,
        <S as Model>::TypeInference: TryInto<T::TypeInference>,
        <S as Model>::LocalRegion: TryInto<T::LocalRegion>,
    {
        match self {
            Self::Static => Some(Region::Static),
            Self::Named(id) => Some(Region::Named(id)),
            Self::Local(local) => local.try_into().ok().map(Region::Local),
            Self::Forall(forall) => forall.try_into().ok().map(Region::Forall),
        }
    }
}
