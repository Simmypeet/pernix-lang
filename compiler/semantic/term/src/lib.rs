//! Defines the various terms to be used with the type system.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

pub mod constant;
pub mod error;
pub mod generic_arguments;
pub mod generic_parameters;
pub mod inference;
pub mod lifetime;
pub mod sub_term;
pub mod tuple;
pub mod r#type;

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
