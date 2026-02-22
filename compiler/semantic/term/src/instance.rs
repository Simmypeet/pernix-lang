//! Defines the [`Instance`] term

use qbice::{Decode, Encode, StableHash};

use crate::{
    generic_arguments::Symbol, generic_parameters::InstanceParameterID,
};

/// The `TraitRef` refers to a trait that an instance implements.
///
/// This is analogous to "expression" has a specific "type". In this case, the
/// "instance" implements a specific "trait".
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct TraitRef(Symbol);

/// An instance of a trait implementation.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub enum Instance {
    /// Directly refers to an `instance` symbol being defined on module level.
    Symbol(Symbol),

    /// Refers to an instance parameter denoted by `instance I: Trait` syntax.
    InstanceParameterID(InstanceParameterID),
}
