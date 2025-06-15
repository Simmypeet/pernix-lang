use enum_as_inner::EnumAsInner;
use pernixc_lexical::tree::DelimiterKind;
use pernixc_parser::{
    abstract_tree,
    expect::{self, Ext as _},
    parser::{ast, Parser as _},
};

use crate::{
    expression::{unit::Unit, Expression},
    r#type::Type,
    GenericIdentifier, Keyword, Numeric, Punctuation,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Postfix {
        pub unit: Unit = ast::<Unit>(),
        pub operators: #[multi] Operator = ast::<Operator>().repeat(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Operator {
        Call(Call = ast::<Call>()),
        Cast(Cast = ast::<Cast>()),
        Access(Access = ast::<Access>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Parenthesis)}
    pub struct Call {
        pub expressions: #[multi] Expression
            = ast::<Expression>().repeat_all_with_separator(','),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Cast {
        pub as_keyword: Keyword = expect::Keyword::As,
        pub r#type: Type = ast::<Type>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Dot {
        pub dot: Punctuation = '.'
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Arrow {
        pub dash: Punctuation = '-',
        pub greater_than: Punctuation = '>'.no_prior_insignificant()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum AccessMode {
        Dot(Dot = ast::<Dot>()),
        Arrow(Arrow = ast::<Arrow>())
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct TupleIndex {
        pub minus: Punctuation = '-'.optional(),
        pub index: Numeric = expect::Numeric,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Bracket)}
    pub struct ArrayIndex {
        pub expression: Expression = ast::<Expression>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum AccessKind {
        GenericIdentifier(GenericIdentifier = ast::<GenericIdentifier>()),
        TupleIndex(TupleIndex = ast::<TupleIndex>()),
        ArrayIndex(ArrayIndex = ast::<ArrayIndex>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Access {
        pub mode: AccessMode = ast::<AccessMode>(),
        pub kind: AccessKind = ast::<AccessKind>(),
    }
}
