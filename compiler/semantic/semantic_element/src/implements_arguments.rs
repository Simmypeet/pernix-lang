//! Contains the query definition for retrieving `implements` generic arguments.

use std::sync::Arc;

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::generic_arguments::GenericArguments;

/// A query for retrieving the generic arguments supplied to the `implements`
/// item (`implements[GENERIC_PARAMETERS] symbol[IMPLEMENTS_ARGUMENTS]`)
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    pernixc_query::Key,
)]
#[value(Option<Arc<GenericArguments>>)]
#[extend(method(get_implements_argument))]
pub struct Key(pub Global<pernixc_symbol::ID>);
