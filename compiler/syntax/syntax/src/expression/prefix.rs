use enum_as_inner::EnumAsInner;
use pernixc_parser::{abstract_tree, expect, parser::ast};

use crate::{expression::postfix::Postfix, Keyword, Punctuation, ReferenceOf};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Prefixable {
        Postfix(Postfix = ast::<Postfix>()),
        Prefix(Prefix = ast::<Prefix>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Prefix {
        pub operator: Operator = ast::<Operator>(),
        pub prefixable: Prefixable = ast::<Prefixable>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Operator {
        LogicalNot(Keyword = expect::Keyword::Not),
        Negate(Punctuation = '-'),
        BitwiseNot(Punctuation = '~'),
        Dereference(Punctuation = '*'),
        ReferenceOf(ReferenceOf = ast::<ReferenceOf>()),
    }
}
