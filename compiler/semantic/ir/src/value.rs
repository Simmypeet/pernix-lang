//! Contains the definition of [`Value`].

use literal::Literal;
use pernixc_arena::ID;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::value::register::Register;
pub mod literal;
pub mod register;

/// An enumeration representing a value in the IR.
///
/// The value can be either a [`Register`] or a [`Literal`].
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    enum_as_inner::EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs, clippy::large_enum_variant)]
pub enum Value {
    Register(ID<Register>),
    Literal(Literal),
}
