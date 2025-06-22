use pernixc_parser::{
    abstract_tree, expect,
    parser::{ast, Parser as _},
};
use pernixc_serialize::{
    extension::{SharedPointerDeserialize, SharedPointerSerialize},
    Deserialize, Serialize,
};
use pernixc_stable_hash::StableHash;

use crate::{predicate::Predicate, Keyword, Passable};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        Hash,
        StableHash,
        Serialize,
        Deserialize,
    )]
    #[serde(
        ser_extension(SharedPointerSerialize),
        de_extension(SharedPointerDeserialize)
    )]
    #{fragment = expect::Fragment::Indentation}
    pub struct Predicates {
        pub predicates: #[multi] Passable<Predicate>
            = ast::<Passable<Predicate>>().line().repeat_all()
    }
}

abstract_tree::abstract_tree! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        Hash,
        Serialize,
        Deserialize,
    )]
    #[serde(
        ser_extension(SharedPointerSerialize),
        de_extension(SharedPointerDeserialize)
    )]
    pub struct WhereClause {
        pub where_keyword: Keyword = expect::Keyword::Where,
        pub predicates: Predicates = ast::<Predicates>()
    }
}

#[cfg(test)]
mod test;
