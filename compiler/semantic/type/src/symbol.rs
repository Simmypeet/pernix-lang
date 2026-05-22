use pernixc_qbice::TrackedEngine;
use pernixc_symbol::GlobalSymbolID;
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::{instantiation::Instantiation, r#type::Type};

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Symbol {
    symbol_id: GlobalSymbolID,
    generic_arguments: Interned<[Interned<Type>]>,
}

impl Symbol {
    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> Instantiation {
        let mut inst = Instantiation::default();
        inst.append_generic_arguments(
            self.symbol_id,
            &self.generic_arguments,
            engine,
        )
        .await;

        inst
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct TraitRef(Symbol);
