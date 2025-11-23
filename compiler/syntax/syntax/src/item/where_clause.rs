use pernixc_parser::{
    abstract_tree, expect,
    parser::{Parser as _, ast},
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{Keyword, Passable, predicate::Predicate};

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
    pub struct WhereClause {
        pub where_keyword: Keyword = expect::Keyword::Where,
        pub predicates: Predicates = ast::<Predicates>()
    }
}

#[cfg(test)]
mod test;
