use enum_as_inner::EnumAsInner;
use pernixc_parser::{
    abstract_tree, expect,
    parser::{Parser as _, ast},
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    Keyword, Passable, Punctuation, expression::Expression,
    pattern::Irrefutable, r#type::Type,
};

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
    pub struct VariableDeclaration {
        pub let_keyword: Keyword = expect::Keyword::Let,
        pub irrefutable_pattern: Irrefutable = ast::<Irrefutable>(),
        pub type_annotation: TypeAnnotation = ast::<TypeAnnotation>().optional(),
        pub equals: Punctuation = '=',
        pub expression: Expression = ast::<Expression>()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct TypeAnnotation {
        pub colon: Punctuation = ':',
        pub r#type: Type = ast::<Type>()
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
        StableHash,
        EnumAsInner
    )]
    pub enum Statement {
        VariableDeclaration(VariableDeclaration = ast::<VariableDeclaration>()),
        Expression(Expression = ast::<Expression>())
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Indentation}
    pub struct Statements {
        pub statements: #[multi] Passable<Statement>
            = ast::<Passable<Statement>>().line().repeat_all()
    }
}
