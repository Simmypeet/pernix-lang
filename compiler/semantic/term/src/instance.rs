//! Defines the [`Instance`] term

use pernixc_target::Global;
use qbice::{Decode, Encode, StableHash};

use crate::{
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::InstanceParameterID,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

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

/// Refers to a terrm that is associated with an instance.
///
/// This could be any term that is associated with an instance, such as "type"
/// or even "instance" associated with an instance.
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
pub struct InstanceAssociated {
    instance: Box<Instance>,
    trait_associated_symbol_id: Global<pernixc_symbol::ID>,
    trait_associated_symbol_generic_arguments: GenericArguments,
}

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
    Parameter(InstanceParameterID),

    /// Refers to an instance that is associated with another instance.
    ///
    /// # Example
    ///
    /// ```pnx
    /// public trait Fizz:
    ///     public instance Inner
    ///
    /// public struct Buzz[instance I: Fizz]:
    ///     public myField: Foo[I::Inner]
    /// ```
    ///
    /// In the above example, `I::Inner` is an instance associated with the
    /// instance parameter `I`.
    InstanceAssociated(InstanceAssociated),
}
