use std::collections::BTreeSet;

use pernixc_symbol::GlobalSymbolID;
use pernixc_type::predicate::Predicate;
use qbice::{Decode, Encode, Identifiable, StableHash};

/// This is equivalence to "given"s in Haskell terminologies. It represents a
/// set of predicates that are assumed to be true at a particular query site.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Encode,
    Decode,
    StableHash,
    Identifiable,
)]
pub struct Premise {
    predicates: BTreeSet<Predicate>,
    query_site: GlobalSymbolID,
}

impl Premise {
    pub fn insert(&mut self, predicate: Predicate) -> bool {
        self.predicates.insert(predicate)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Predicate> {
        self.predicates.iter()
    }
}
