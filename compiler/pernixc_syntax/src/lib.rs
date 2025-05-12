#![allow(missing_docs)]

//! Contains all the definitions of the syntax tree

use enum_as_inner::EnumAsInner;
use expression::Expression;
use pernixc_lexical::{
    kind,
    token::Token,
    tree::{DelimiterKind, RelativeLocation},
};
use pernixc_parser::{
    abstract_tree::{self, AbstractTree},
    expect::{self, Ext as _},
    parser::{ast, Parser as _},
};
use r#type::Type;

pub mod expression;
pub mod pattern;
pub mod statement;
pub mod r#type;

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

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
    pub enum AccessModifier {
        Public(Keyword = expect::Keyword::Public),
        Private(Keyword = expect::Keyword::Private),
        Internal(Keyword = expect::Keyword::Internal)
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ScopeSeparator {
        pub first_colon = ':',
        pub second_colon = ':'.no_prior_insignificant()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Elided {
        pub first_dot = '.',
        pub second_dot = '.'.no_prior_insignificant()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum LifetimeIdentifier {
        Identifier(Identifier = expect::Identifier),
        Static(Keyword = expect::Keyword::Static),
        Elided(Elided = ast::<Elided>())
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Lifetime {
        pub apostrophe: Punctuation = '\'',
        pub identifier: LifetimeIdentifier = ast::<LifetimeIdentifier>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum GenericArgument {
        Lifetime(Lifetime = ast::<Lifetime>()),
        Type(Type = ast::<Type>()),
        Constant(ConstantArgument = ast::<ConstantArgument>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Bracket)}
    pub struct GenericArguments {
        pub arguments: #[multi] GenericArgument
            = ast::<GenericArgument>().repeat_all_with_separator(',')
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct GenericIdentifier {
        pub identifier: Identifier = expect::Identifier,
        pub generic_arguments: GenericArguments
            = ast::<GenericArguments>().optional()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct LifetimeParameter {
        pub apostrophe: Punctuation = '\'',
        pub identifier: Identifier = expect::Identifier,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum SimplePathRoot {
        Target(Keyword = expect::Keyword::Target),
        Identifier(Identifier = expect::Identifier),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct SimplePathSubsequent {
        pub scope_separator: ScopeSeparator = ast::<ScopeSeparator>(),
        pub identifier: Identifier = expect::Identifier,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct SimplePath {
        pub root: SimplePathRoot = ast::<SimplePathRoot>(),
        pub subsequences: #[multi] SimplePathSubsequent
            = ast::<SimplePathSubsequent>().repeat()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum QualifiedIdentifierRoot {
        Target(Keyword = expect::Keyword::Target),
        This(Keyword = expect::Keyword::This),
        GenericIdentifier(GenericIdentifier = ast::<GenericIdentifier>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct QualifiedIdentifierSubsequent {
        pub scope_separator: ScopeSeparator = ast::<ScopeSeparator>(),
        pub generic_identifier: GenericIdentifier
            = ast::<GenericIdentifier>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct QualifiedIdentifier {
        pub root: QualifiedIdentifierRoot = ast::<QualifiedIdentifierRoot>(),
        pub subsequences: #[multi] QualifiedIdentifierSubsequent
            = ast::<QualifiedIdentifierSubsequent>().repeat()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Label {
        pub apostrophe: Punctuation = '\'',
        pub identifier: Identifier = expect::Identifier,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ReferenceOf {
        pub ampersand: Punctuation = '&',
        pub mut_keyword: Keyword = expect::Keyword::Mut.optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Ellipsis {
        pub first_dot: Punctuation = '.',
        pub second_dot: Punctuation = '.'.no_prior_insignificant(),
        pub third_dot: Punctuation = '.'.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Brace)}
    pub enum ConstantArgument {
        Expression(Expression =  ast::<Expression>()),
        Elided(Elided = ast::<Elided>()),
    }
}

abstract_tree::abstract_tree! {
    pub enum Passable<T: 'static + AbstractTree> {
        Pass(Keyword = expect::Keyword::Pass),
        Line(T = ast::<T>())
    }
}

#[cfg(test)]
mod test;
