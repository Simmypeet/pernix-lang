//! Contains the definition of [`Region`].

use std::{
    cmp::{Eq, Ord, PartialEq, PartialOrd},
    sync::atomic::{AtomicUsize, Ordering},
};

use enum_as_inner::EnumAsInner;

use super::Model;
use crate::symbol::LifetimeParameterID;

/// Represents a particular variable region
#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Region<S: Model> {
    /// A static lifetime, denoted by `'static`.
    Static,

    /// A lifetime to a named lifetime parameter, denoted by `'a`.
    Named(LifetimeParameterID),

    /// The kind of region that depends on the particular context.
    Context(S::RegionContext),
}
