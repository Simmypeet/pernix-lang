use enum_as_inner::EnumAsInner;
use pernixc_lexical::tree::DelimiterKind;
use pernixc_parser::{
    abstract_tree,
    expect::{self, Ext as _},
    parser::{Parser, ast},
};

use crate::{
    Ellipsis, Identifier, Keyword, Punctuation, QualifiedIdentifier,
    expression::{Call, Expression},
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Unit {
        Boolean(Boolean = ast::<Boolean>()),
        Numeric(Numeric = ast::<Numeric>()),
        Parenthesized(Parenthesized = ast::<Parenthesized>()),
        Struct(Struct = ast::<Struct>()),
        FunctionCall(FunctionCall = ast::<FunctionCall>()),
        ResumeCall(ResumeCall = ast::<ResumeCall>()),
        QualifiedIdentifier(QualifiedIdentifier = ast::<QualifiedIdentifier>()),
        Array(Array = ast::<Array>()),
        Phantom(Phantom = ast::<Phantom>()),
        String(String = ast::<String>()),
        Character(Character = ast::<Character>()),
        Panic(Panic = ast::<Panic>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ResumeCall {
        pub resume_keyword: Keyword = expect::Keyword::Resume,
        pub single_call: SingleCall = ast::<SingleCall>(),
    }
}

abstract_tree::abstract_tree! {

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Parenthesis)}
    pub struct SingleCall {
        pub expression: Expression = ast::<Expression>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct FunctionCall {
        pub qualified_identifier: QualifiedIdentifier
            = ast::<QualifiedIdentifier>(),
        pub call: Call = ast::<Call>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Decimal {
        pub point: Punctuation = '.'.no_prior_insignificant(),
        pub digits: crate::Numeric = expect::Numeric.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Numeric {
        pub numeric: crate::Numeric = expect::Numeric,
        pub decimal: Decimal = ast::<Decimal>().optional(),
        pub identifier: Identifier
            = expect::Identifier.no_prior_insignificant().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Unpackable {
        pub ellipsis: Ellipsis = ast::<Ellipsis>().optional(),
        pub expression: Expression = ast::<Expression>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Parenthesis)}
    pub struct Parenthesized {
        pub unpackables: #[multi] Unpackable
            = ast::<Unpackable>().repeat_all_with_separator(','),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Boolean {
        True(Keyword = expect::Keyword::True),
        False(Keyword = expect::Keyword::False),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct FieldInitializer {
        pub identifier: Identifier = expect::Identifier,
        pub colon: Punctuation = ':',
        pub expression: Expression = ast::<Expression>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Brace)}
    pub struct FieldInitializerBody {
        pub initializers: #[multi] FieldInitializer
            = ast::<FieldInitializer>().repeat_all_with_separator(','),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Struct {
        pub qualified_identifier: QualifiedIdentifier
            = ast::<QualifiedIdentifier>(),
        pub field_initializer_body: FieldInitializerBody
            = ast::<FieldInitializerBody>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Bracket)}
    pub struct Array {
        pub expressions: #[multi] Expression
            = ast::<Expression>().repeat_all_with_separator(','),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Phantom {
        pub phantom: Keyword = expect::Keyword::Phantom,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct String {
        pub string: crate::String = expect::String,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Character {
        pub character: crate::Character = expect::Character,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Panic {
        pub panic: Keyword = expect::Keyword::Panic,
    }
}
