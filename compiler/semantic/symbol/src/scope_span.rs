//! Defines the scope span key for symbols.

use pernixc_lexical::tree::RelativeSpan;
use pernixc_target::Global;
use qbice::{Decode, Encode, Query, StableHash};

/// Points to the whole range in the source code where the given symbol is
/// scoped, typically used for determining the symbol scope at a particular
/// location.
///
/// This covers the entire span of the symbol, from signature to the end of its
/// body. This is different from the [`crate::span::Key`], which usually only
/// points to the identifier of the symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    Query,
    StableHash,
)]
#[extend(name = get_scope_span, by_val)]
#[value(Option<RelativeSpan>)]
pub struct Key {
    /// The global ID of the symbol to get the scope span for.
    pub symbol_id: Global<crate::ID>,
}
