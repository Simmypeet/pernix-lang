use enum_as_inner::EnumAsInner;
use pernixc_parser::{
    abstract_tree, expect,
    parser::{ast, Parser as _},
};

use crate::{
    expression::Expression, pattern::Irrefutable, r#type::Type, Keyword,
    Passable, Punctuation,
};

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
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
