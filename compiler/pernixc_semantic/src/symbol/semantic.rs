//! Contains the semantic logic for the symbolic model.

use crate::semantic::{model::Model, term::Never};

/// A struct that implements [`Model`] which describes the system of the symbolic model.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Symbolic;

impl Model for Symbolic {
    type ConstantInference = Never;
    type ForallLifetime = Never;
    type LifetimeInference = Never;
    type ScopedLifetime = Never;
    type TypeInference = Never;
}
