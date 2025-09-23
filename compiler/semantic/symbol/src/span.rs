//! Contains the definition of the [`Span`] query.

use derive_more::{Deref, DerefMut};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

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
    Serialize,
    Deserialize,
    pernixc_query::Key,
    StableHash,
)]
#[extend(method(get_span), no_cyclic)]
#[value(Option<RelativeSpan>)]
pub struct Key(pub Global<ID>);
