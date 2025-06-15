use pernixc_parser::{
    abstract_tree, expect,
    parser::{ast, Parser as _},
};

use crate::{predicate::Predicate, Keyword, Passable};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Indentation}
    pub struct Predicates {
        pub predicates: #[multi] Passable<Predicate>
            = ast::<Passable<Predicate>>().line().repeat_all()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct WhereClause {
        pub where_keyword: Keyword = expect::Keyword::Where,
        pub predicates: Predicates = ast::<Predicates>()
    }
}

#[cfg(test)]
mod test;
