/// Contains the premise of the semantic logic.
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
    /// List of predicates that will be considered as facts.
    pub predicates: BTreeSet<Predicate>,

    /// An optional [`Global<pernixc_symbol::ID>`] specifying the site where
    /// the queries will be taking place in.
    ///
    /// This can influence the result of resoliving the trait/marker
    /// implementations.
    pub query_site: Global<pernixc_symbol::SymbolID>,
}

pub struct Solver {}
