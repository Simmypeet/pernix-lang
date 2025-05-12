use enum_as_inner::EnumAsInner;
use pernixc_parser::{
    abstract_tree, expect,
    parser::{ast, Parser as _},
};

use crate::{
    expression::{binary::Binary, Expression},
    statement::Statements,
    Keyword, Label, Punctuation,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Block {
        Scope(Scope = ast::<Scope>()),
        IfElse(IfElse = ast::<IfElse>()),
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

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct IndentedGroup {
        pub unsafe_keyword: Keyword = expect::Keyword::Unsafe.optional(),
        pub label: Label = ast::<Label>().optional(),
        pub statements: Statements = ast::<Statements>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct InlineExpression {
        pub colon: Punctuation = ':',
        pub expression: Expression = ast::<Expression>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Group {
        Indented(IndentedGroup = ast::<IndentedGroup>()),
        Inline(InlineExpression = ast::<InlineExpression>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct IfElse {
        pub if_keyword: Keyword = expect::Keyword::If,
        pub binary: Binary = ast::<Binary>(),
        pub then: Group = ast::<Group>(),
        pub r#else: Else = ast::<Else>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum GroupOrIfElse {
        Group(Group = ast::<Group>()),
        IfElse(IfElse = ast::<IfElse>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Else {
        pub else_keyword: Keyword
            = expect::Keyword::Else.new_line_significant(false),
        pub group_or_if_else: GroupOrIfElse = ast::<GroupOrIfElse>(),
    }
}
