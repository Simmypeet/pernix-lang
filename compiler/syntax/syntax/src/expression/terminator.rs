use enum_as_inner::EnumAsInner;
use pernixc_parser::{
    abstract_tree, expect,
    parser::{ParserExt, ast},
};
use qbice::{Decode, Encode, StableHash};

use crate::{Keyword, Label, expression::binary::Binary};

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
        Encode,
        Decode,
        EnumAsInner
    )]
    pub enum Terminator {
        Return(Return = ast::<Return>()),
        Continue(Continue = ast::<Continue>()),
        Express(Express = ast::<Express>()),
        Break(Break = ast::<Break>()),
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
        StableHash,
        Encode,
        Decode,
    )]
    pub struct Return {
        pub return_keyword: Keyword = expect::Keyword::Return,
        pub binary: Binary = ast::<Binary>().optional(),
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
        StableHash,
        Encode,
        Decode,
    )]
    pub struct Continue {
        pub continue_keyword: Keyword = expect::Keyword::Continue,
        pub label: Label = ast::<Label>().optional(),
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
        StableHash,
        Encode,
        Decode,
    )]
    pub struct Express {
        pub express_keyword: Keyword = expect::Keyword::Express,
        pub label: Label = ast::<Label>().optional(),
        pub binary: Binary = ast::<Binary>().optional(),
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
        StableHash,
        Encode,
        Decode,
    )]
    pub struct Break {
        pub break_keyword: Keyword = expect::Keyword::Break,
        pub label: Label = ast::<Label>().optional(),
        pub binary: Binary = ast::<Binary>().optional(),
    }
}
