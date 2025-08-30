//!  Contains the definition of [`Key`]

use std::sync::Arc;

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::r#type::Type;

/// A query key for retrieving the optional associated type of a variant. Every
/// variant symbol shall have this key whether or not they have associated type.
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
    StableHash,
    pernixc_query::Key,
)]
#[value(Option<Arc<Type>>)]
#[extend(method(get_variant_associated_type))]
pub struct Key(pub Global<pernixc_symbol::ID>);
