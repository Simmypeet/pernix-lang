//! Defines the scope span key for symbols.

use pernixc_lexical::tree::RelativeSpan;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

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
    Serialize,
    Deserialize,
    pernixc_query::Key,
    StableHash,
)]
#[extend(method(get_scope_span), no_cyclic)]
#[value(Option<RelativeSpan>)]
pub struct Key(pub Global<crate::ID>);
