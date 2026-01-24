//! Contains the definition of the [`Span`] query.

use derive_more::{Deref, DerefMut};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_target::Global;
use qbice::{Decode, Encode, Query, StableHash};

use crate::ID;

/// Points to the particular location span in the source code where the given
/// symbol is defined, typically used for diagnostics and error reporting.
///
/// It usually points to the identifier of the symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Deref,
    DerefMut,
    Encode,
    Decode,
    Query,
    StableHash,
)]
#[extend(name = get_span, by_val)]
#[value(Option<RelativeSpan>)]
pub struct Key {
    /// The global ID of the symbol to get the span for.
    pub symbol_id: Global<ID>,
}
