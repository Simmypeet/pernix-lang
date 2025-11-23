use enum_as_inner::EnumAsInner;
use pernixc_lexical::tree::DelimiterKind;
use pernixc_parser::{
    abstract_tree,
    expect::{self, Ext as _},
    parser::{Parser as _, ast},
};

use crate::{
    GenericIdentifier, Identifier, Keyword, Numeric, Punctuation,
    expression::{Call, Expression, unit::Unit},
    r#type::Type,
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
        MethodCall(MethodCall = ast::<MethodCall>()),
        Cast(Cast = ast::<Cast>()),
        Access(Access = ast::<Access>()),
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
        StructField(Identifier = expect::Identifier),
        TupleIndex(TupleIndex = ast::<TupleIndex>()),
        ArrayIndex(ArrayIndex = ast::<ArrayIndex>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Access {
        pub dot: Punctuation = '.',
        pub kind: AccessKind = ast::<AccessKind>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct MethodCall {
        pub dot: Punctuation = '.',
        pub generic_identifier: GenericIdentifier = ast::<GenericIdentifier>(),
        pub call: Call = ast::<Call>(),
    }
}
