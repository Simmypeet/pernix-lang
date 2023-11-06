//! Contains the definition of [`Region`].

use enum_as_inner::EnumAsInner;

use super::{Model, Never};
use crate::symbol::LifetimeParameterID;

/// Represents a particular variable region
#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Region<S: Model> {
    /// A static lifetime, denoted by `'static`.
    Static,

    /// A lifetime to a named lifetime parameter, denoted by `'a`.
    Named(LifetimeParameterID),

    /// The kind of region that depends on the particular context.
    Context(S::RegionContext),
}

impl<T> Region<T>
where
    T: Model<TypeInference = Never, ConstantInference = Never, RegionContext = Never>,
{
    /// Converts this region into another model.
    #[must_use]
    pub fn into_other_model<S: Model>(self) -> Region<S> {
        match self {
            Self::Static => Region::Static,
            Self::Named(named) => Region::Named(named),
            Self::Context(never) => match never {},
        }
    }
}
