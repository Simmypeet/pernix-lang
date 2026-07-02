use std::collections::BTreeSet;

use pernixc_symbol::GlobalSymbolID;
use pernixc_type::predicate::Predicate2;
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
    predicates: BTreeSet<Predicate2>,
    query_site: GlobalSymbolID,
}

impl Premise {
    #[must_use]
    pub const fn new(query_site: GlobalSymbolID) -> Self {
        Self { predicates: BTreeSet::new(), query_site }
    }

    #[must_use]
    pub const fn query_site(&self) -> GlobalSymbolID { self.query_site }

    pub fn insert(&mut self, predicate: Predicate2) -> bool {
        self.predicates.insert(predicate)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Predicate2> {
        self.predicates.iter()
    }
}
