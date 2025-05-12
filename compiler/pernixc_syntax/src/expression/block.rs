use enum_as_inner::EnumAsInner;
use pernixc_parser::{
    abstract_tree, expect,
    parser::{ast, Parser as _},
};

use crate::{statement::Statements, Keyword, Label};

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Block {
        Scope(Scope = ast::<Scope>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Scope {
        pub unsafe_keyword: Keyword = expect::Keyword::Unsafe.optional(),
        pub scope_keyword: Keyword = expect::Keyword::Scope,
        pub label: Label = ast::<Label>().optional(),
        pub statements: Statements = ast::<Statements>(),
    }
}
