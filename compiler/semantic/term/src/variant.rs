//!  Contains the definition of [`Key`]

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::r#type::Type;

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
#[value(Option<Type>)]
pub struct Key(pub Global<pernixc_symbol::ID>);
