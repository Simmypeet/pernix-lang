use pernixc_parser::{
    abstract_tree, expect,
    parser::{ast, Parser as _},
};

use crate::{
    item::{function, TrailingWhereClause},
    AccessModifier, Keyword, Passable,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Function {
        pub access_modifier: AccessModifier = ast::<AccessModifier>(),
        pub signature: function::Signature = ast::<function::Signature>(),
        pub trailing_where_clause: TrailingWhereClause
            = ast::<TrailingWhereClause>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Indentation}
    pub struct Body {
        pub functions: #[multi] Passable<Function>
            = ast::<Passable<Function>>().line().repeat_all(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Extern {
        pub extern_keyword: Keyword = expect::Keyword::Extern,
        pub convention: crate::String = expect::String,
        pub body: Body = ast::<Body>(),
    }
}

#[cfg(test)]
mod test;
