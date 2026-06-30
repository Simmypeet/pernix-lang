use pernixc_symbol::GlobalSymbolID;
use qbice::{Decode, Encode, Query, StableHash, storage::intern::Interned};

use crate::r#type::Type2;

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
    Encode,
    Decode,
    Query,
)]
#[value(Interned<Type2>)]
#[extend(name = get_instance_associated_type2, by_val)]
pub struct Key {
    pub symbol_id: GlobalSymbolID,
}
