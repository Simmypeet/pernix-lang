//! Defines the various terms to be used with the type system.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

pub mod constant;
pub mod error;
pub mod generic_arguments;
pub mod generic_parameters;
pub mod inference;
pub mod lifetime;
pub mod matching;
pub mod sub_term;
pub mod tuple;
pub mod r#type;

/// Represents a type that can never be instantiated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Never {}

/// An enumeration of all kinds of terms in the type system.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
#[allow(missing_docs)]
pub enum Term {
    Constant(constant::Constant),
    Lifetime(lifetime::Lifetime),
    Type(r#type::Type),
}
