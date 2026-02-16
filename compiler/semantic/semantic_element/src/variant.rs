//!  Contains the definition of [`Key`]

use pernixc_target::Global;
use pernixc_term::r#type::Type;
use qbice::{Decode, Encode, Query, StableHash, storage::intern::Interned};

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
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<Interned<Type>>)]
#[extend(name = get_variant_associated_type, by_val)]
pub struct Key {
    /// The global ID of the variant symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
}
