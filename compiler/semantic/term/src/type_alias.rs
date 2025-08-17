//! Defines a query for retrieving type alias term from the type alias symbols.

use std::sync::Arc;

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::r#type::Type;

/// A key for retrieving the type alias of various kinds of type alias symbols.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    StableHash,
    Serialize,
    Deserialize,
    pernixc_query::Key,
)]
#[value(Arc<Type>)]
#[extend(method(get_type_alias))]
pub struct Key(pub Global<pernixc_symbol::ID>);
