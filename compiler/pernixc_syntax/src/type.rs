//! Contains the definitions of the syntax tree related to type terms

use pernixc_lexical::tree::DelimiterKind;
use pernixc_parser::{
    abstract_tree, expect,
    parser::{ast, Parser},
};

use crate::{
    Elided, Ellipsis, Identifier, Keyword, Lifetime, Numeric, Punctuation,
    QualifiedIdentifier,
};

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub enum Primitive {
        Bool(Keyword = expect::Keyword::Bool),
        Float32(Keyword = expect::Keyword::Float32),
        Float64(Keyword = expect::Keyword::Float64),
        Int8(Keyword = expect::Keyword::Int8),
        Int16(Keyword = expect::Keyword::Int16),
        Int32(Keyword = expect::Keyword::Int32),
        Int64(Keyword = expect::Keyword::Int64),
        Uint8(Keyword = expect::Keyword::Uint8),
        Uint16(Keyword = expect::Keyword::Uint16),
        Uint32(Keyword = expect::Keyword::Uint32),
        Uint64(Keyword = expect::Keyword::Uint64),
        Usize(Keyword = expect::Keyword::Usize),
        Isize(Keyword = expect::Keyword::Isize),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct Reference {
        pub ampersand: Punctuation = '&',
        pub lifetime: Lifetime = ast::<Lifetime>(),
        pub mut_keyword: Keyword = expect::Keyword::Mut.optional(),
        pub r#type: Type = ast::<Type>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct Unpackable {
        pub ellipsis: Ellipsis = ast::<Ellipsis>().optional(),
        pub r#type: Type = ast::<Type>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Parenthesis)}
    pub struct Tuple {
        pub types: #[multi] Unpackable
            = ast::<Unpackable>().repeat_all_with_separator(','),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Bracket)}
    pub struct Array {
        pub r#type: Type = ast::<Type>(),
        pub x: Identifier = expect::IdentifierValue("x"),
        pub numeric: Numeric = expect::Numeric,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct Pointer {
        pub asterisk: Punctuation = '*',
        pub mut_keyword: Keyword = expect::Keyword::Mut.optional(),
        pub r#type: Type = ast::<Type>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct Phantom {
        pub phantom: Keyword = expect::Keyword::Phantom,
        pub r#type: Type = ast::<Type>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub enum Type {
        Primitive(Primitive = ast::<Primitive>()),
        QualifiedIdentifier(QualifiedIdentifier = ast::<QualifiedIdentifier>()),
        Reference(Reference = ast::<Reference>()),
        Pointer(Pointer = ast::<Pointer>()),
        Tuple(Tuple = ast::<Tuple>()),
        Array(Array = ast::<Array>()),
        Phantom(Phantom = ast::<Phantom>()),
        Elided(Elided = ast::<Elided>())
    }
}
