//! Contains all the definitions of the syntax tree

use enum_as_inner::EnumAsInner;
use pernixc_lexical::{
    kind,
    token::Token,
    tree::{DelimiterKind, RelativeLocation},
};
use pernixc_parser::{
    abstract_tree,
    expect::{self, Ext as _},
    parser::{ast, Parser as _},
};
use r#type::Type;

pub mod r#type;

/// Type alias for [`Token`] categorized as a [`kind::Keyword`].
pub type Keyword = Token<kind::Keyword, RelativeLocation>;

/// Type alias for [`Token`] categorized as a [`kind::NewLine`].
pub type NewLine = Token<kind::NewLine, RelativeLocation>;

/// Type alias for [`Token`] categorized as a [`kind::Character`].
pub type Character = Token<kind::Character, RelativeLocation>;

/// Type alias for [`Token`] categorized as a [`kind::String`].
pub type String = Token<kind::String, RelativeLocation>;

/// Type alias for [`Token`] categorized as a [`kind::Identifier`].
pub type Identifier = Token<kind::Identifier, RelativeLocation>;

/// Type alias for [`Token`] categorized as a [`kind::Punctuation`].
pub type Punctuation = Token<kind::Punctuation, RelativeLocation>;

/// Type alias for [`Token`] categorized as a [`kind::Numeric`].
pub type Numeric = Token<kind::Numeric, RelativeLocation>;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    #[allow(missing_docs)]
    pub enum AccessModifier {
        Public(Keyword = expect::Keyword::Public),
        Private(Keyword = expect::Keyword::Private),
        Internal(Keyword = expect::Keyword::Internal)
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct ScopeSeparator {
        pub first_colon = ':',
        pub second_colon = ':'.no_prior_insignificant()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct Elided {
        pub first_dot = '.',
        pub second_dot = '.'.no_prior_insignificant()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    #[allow(missing_docs)]
    pub enum LifetimeIdentifier {
        Identifier(Identifier = expect::Identifier),
        Static(Keyword = expect::Keyword::Static),
        Elided(Elided = ast::<Elided>())
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct Lifetime {
        pub apostrophe: Punctuation = '\'',
        pub identifier: LifetimeIdentifier = ast::<LifetimeIdentifier>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    #[allow(missing_docs)]
    pub enum GenericArgument {
        Lifetime(Lifetime = ast::<Lifetime>()),
        Type(Type = ast::<Type>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Bracket)}
    pub struct GenericArguments {
        pub argments: #[multi] GenericArgument
            = ast::<GenericArgument>().repeat_all_with_separator(',')
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct GenericIdentifier {
        pub identifier: Identifier = expect::Identifier,
        pub generic_arguments: GenericArguments
            = ast::<GenericArguments>().optional()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct LifetimeParameter {
        pub apostrophe: Punctuation = '\'',
        pub identifier: Identifier = expect::Identifier,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    #[allow(missing_docs)]
    pub enum SimplePathRoot {
        Target(Keyword = expect::Keyword::Target),
        Identifier(Identifier = expect::Identifier),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct SimplePathSubsequent {
        pub scope_separator: ScopeSeparator = ast::<ScopeSeparator>(),
        pub identifier: Identifier = expect::Identifier,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct SimplePath {
        pub root: SimplePathRoot = ast::<SimplePathRoot>(),
        pub subsequences: #[multi] SimplePathSubsequent
            = ast::<SimplePathSubsequent>().repeat()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    #[allow(missing_docs)]
    pub enum QualifiedIdentifierRoot {
        Target(Keyword = expect::Keyword::Target),
        This(Keyword = expect::Keyword::This),
        GenericIdentifier(GenericIdentifier = ast::<GenericIdentifier>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct QualifiedIdentifierSubsequent {
        pub scope_separator: ScopeSeparator = ast::<ScopeSeparator>(),
        pub generic_identifier: GenericIdentifier
            = ast::<GenericIdentifier>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct QualifiedIdentifier {
        pub root: QualifiedIdentifierRoot = ast::<QualifiedIdentifierRoot>(),
        pub subsequences: #[multi] QualifiedIdentifierSubsequent
            = ast::<QualifiedIdentifierSubsequent>().repeat()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct Label {
        pub apostrophe: Punctuation = '\'',
        pub identifier: Identifier = expect::Identifier,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct ReferenceOf {
        pub ampersand: Punctuation = '&',
        pub mut_keyword: Keyword = expect::Keyword::Mut.optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(missing_docs)]
    pub struct Ellipsis {
        pub first_dot: Punctuation = '.',
        pub second_dot: Punctuation = '.'.no_prior_insignificant(),
        pub third_dot: Punctuation = '.'.no_prior_insignificant(),
    }
}
