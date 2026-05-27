use pernixc_qbice::TrackedEngine;
use pernixc_symbol::GlobalSymbolID;
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::{
    substitution::{Substitutable, Substitution},
    r#type::Type,
};

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

impl Substitutable for Symbol {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl pernixc_qbice::Interner,
    ) -> Option<Self>
    where
        Self: Sized,
    {
        self.generic_arguments.apply(subst, interner).map(|generic_arguments| {
            Self { symbol_id: self.symbol_id, generic_arguments }
        })
    }
}

impl Symbol {
    pub async fn create_substitution(
        &self,
        engine: &TrackedEngine,
    ) -> Substitution {
        let mut subst = Substitution::new();
        subst
            .append_generic_arguments(
                self.symbol_id,
                &self.generic_arguments,
                engine,
            )
            .await;

        subst
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

impl Substitutable for TraitRef {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl pernixc_qbice::Interner,
    ) -> Option<Self> {
        self.0.apply(subst, interner).map(TraitRef)
    }
}
