//! Contains the definition of the [`Alloca`] struct.

use pernixc_arena::ID;
use pernixc_lexical::tree::RelativeSpan;
use qbice::{Decode, Encode};
use qbice::StableHash;
use pernixc_term::r#type::Type;

use crate::scope;

/// Represents a stack memory allocation.
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
pub struct Alloca {
    /// The type of the value being allocated.
    pub r#type: Type,

    /// The scope in which this alloca is declared.
    pub declared_in_scope_id: ID<scope::Scope>,

    /// The order in which this alloca is declared in the scope (starting from
    /// 0).
    pub declaration_order: usize,

    /// The span of the allocation.
    pub span: RelativeSpan,
}
