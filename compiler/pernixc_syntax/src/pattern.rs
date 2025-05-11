use enum_as_inner::EnumAsInner;
use pernixc_lexical::tree::DelimiterKind;
use pernixc_parser::{
    abstract_tree::{self, AbstractTree},
    expect::{self, Ext as _},
    parser::{ast, Parser as _},
};

use crate::{Ellipsis, Identifier, Keyword, Numeric, Punctuation, ReferenceOf};

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct FieldAssociation<P: 'static + AbstractTree> {
        pub identifier: Identifier = expect::Identifier,
        pub colon: Punctuation = ':',
        pub pattern: P = ast::<P>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Wildcard {
        pub first_dot = '.',
        pub second_dot = '.'.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Named {
        pub mutable_keyword: Keyword = expect::Keyword::Mut.optional(),
        pub reference_of: ReferenceOf = ast::<ReferenceOf>().optional(),
        pub identifier: Identifier = expect::Identifier,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Field<P: 'static + AbstractTree> {
        FieldAssociation(FieldAssociation<P> = ast::<FieldAssociation<P>>()),
        Wildcard(Wildcard = ast::<Wildcard>()),
        Named(Named = ast::<Named>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Brace)}
    pub struct Struct<P: 'static + AbstractTree> {
        pub fields: #[multi] Field<P>
            = ast::<Field<P>>().repeat_all_with_separator(',')
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Parenthesis)}
    pub struct EnumAssociation {
        pub pattern: Refutable = ast::<Refutable>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Enum {
        pub identifier: Identifier = expect::Identifier,
        pub association: EnumAssociation = ast::<EnumAssociation>().optional(),
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
    pub struct Integer {
        pub minus: Punctuation = '-'.optional(),
        pub numeric: Numeric = expect::Numeric,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Unpackable<P: 'static + AbstractTree> {
        pub ellipsis: Ellipsis = ast::<Ellipsis>().optional(),
        pub pattern: P = ast::<P>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Parenthesis)}
    pub struct Tuple<P: 'static + AbstractTree> {
        pub types: #[multi] Unpackable<P>
            = ast::<Unpackable<P>>().repeat_all_with_separator(','),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Irrefutable {
        Struct(Struct<Self> = ast::<Struct<Self>>()),
        Named(Named = ast::<Named>()),
        Tuple(Tuple<Self> = ast::<Tuple<Self>>()),
        Wildcard(Wildcard = ast::<Wildcard>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Refutable {
        Boolean(Boolean = ast::<Boolean>()),
        Integer(Integer = ast::<Integer>()),
        Struct(Struct<Self> = ast::<Struct<Self>>()),
        Enum(Enum = ast::<Enum>()),
        Named(Named = ast::<Named>()),
        Tuple(Tuple<Self> = ast::<Tuple<Self>>()),
        Wildcard(Wildcard = ast::<Wildcard>()),
    }
}
