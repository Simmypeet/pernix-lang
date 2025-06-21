use enum_as_inner::EnumAsInner;
use pernixc_parser::{
    abstract_tree, expect,
    parser::{ast, Parser as _},
};
use pernixc_serialize::{
    extension::{SharedPointerDeserialize, SharedPointerSerialize},
    Deserialize, Serialize,
};

use crate::{expression::binary::Binary, Keyword, Label};

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
        Serialize,
        Deserialize,
        EnumAsInner
    )]
    #[serde(
        ser_extension(SharedPointerSerialize),
        de_extension(SharedPointerDeserialize)
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
        Serialize,
        Deserialize,
    )]
    #[serde(
        ser_extension(SharedPointerSerialize),
        de_extension(SharedPointerDeserialize)
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
        Serialize,
        Deserialize,
    )]
    #[serde(
        ser_extension(SharedPointerSerialize),
        de_extension(SharedPointerDeserialize)
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
        Serialize,
        Deserialize,
    )]
    #[serde(
        ser_extension(SharedPointerSerialize),
        de_extension(SharedPointerDeserialize)
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
        Serialize,
        Deserialize,
    )]
    #[serde(
        ser_extension(SharedPointerSerialize),
        de_extension(SharedPointerDeserialize)
    )]
    pub struct Break {
        pub break_keyword: Keyword = expect::Keyword::Break,
        pub label: Label = ast::<Label>().optional(),
        pub binary: Binary = ast::<Binary>().optional(),
    }
}
