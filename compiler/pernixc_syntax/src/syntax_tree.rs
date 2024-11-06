//! Contains all the definition of syntax trees and their parsing functions.

#![allow(missing_docs)]

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};

use self::{expression::Expression, r#type::Type};
use crate::{
    error,
    parser::{
        self, delimited_tree, expect::Expect, DelimitedTree, ExpectIdentifier,
        Parser, Reading, Syntax,
    },
};

pub mod expression;
pub mod item;
pub mod json;
pub mod pattern;
pub mod predicate;
pub mod statement;
pub mod strategy;
pub mod target;
pub mod r#type;

/// Allows to create a [`Syntax`] from a syntax tree node.
pub trait Parse {
    /// Creates a [`Syntax`] from the syntax tree node.
    fn syntax() -> impl Syntax<Output = Self>;
}

impl<'a> Parser<'a> {
    /// Parses the given syntax tree at the current parser position.
    pub fn parse_syntax_tree<P: Parse>(
        &mut self,
        handler: &dyn Handler<error::Error>,
    ) -> Result<P, parser::Error> {
        self.parse(P::syntax(), handler)
    }

    /// Parses the given syntax tree at the current parser position without
    pub fn parse_syntax_tree_dont_skip<S: Parse>(
        &mut self,
        handler: &dyn Handler<error::Error>,
    ) -> Result<S, parser::Error> {
        self.parse_dont_skip(S::syntax(), handler)
    }
}

impl<T: Parse> Parse for Box<T> {
    fn syntax() -> impl Syntax<Output = Self> { T::syntax().map(Box::new) }
}

impl<T: Parse> Parse for Option<T> {
    fn syntax() -> impl Syntax<Output = Self> { T::syntax().or_none() }
}

impl Parse for Identifier {
    fn syntax() -> impl Syntax<Output = Self> { ExpectIdentifier }
}

/// Represents a syntax tree node with a pattern of syntax tree nodes separated
///
/// by a separator.
///
/// This struct is useful for representing syntax tree nodes that are separated
/// by a separator. For example, a comma separated list of expressions such as
/// `1, 2, 3` can be represented by a [`ConnectedList`] with the separator being
/// a comma token and the elements being the expressions.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ConnectedList<Element, Separator> {
    /// The first element of the list.
    #[get = "pub"]
    first: Element,

    /// The rest of the elements of the list.
    ///
    /// Each element of the list is a tuple containing the separator and the
    /// element. The separator is the token/syntax tree node that separates
    /// the current element from the prior one.
    #[get = "pub"]
    rest: Vec<(Separator, Element)>,

    /// The trailing separator of the list.
    #[get = "pub"]
    trailing_separator: Option<Separator>,
}

/// Represents a syntax tree pattern of a list of elements enclosed by a pair of
/// delimiters.
pub type DelimitedList<T> =
    DelimitedTree<Option<ConnectedList<T, Punctuation>>>;

/// Parses a list of elements enclosed by a pair of delimiters, separated by a
/// separator.
pub fn delimited_list<T: Parse>(
    delimiter: Delimiter,
    separator: char,
) -> impl Syntax<Output = DelimitedList<T>> {
    delimiter.verify_then_do(move |parser, handler| {
        fn skip_to_next_separator(
            this: &mut Parser,
            separator: char,
        ) -> Option<Punctuation> {
            if let Reading::Unit(Token::Punctuation(pun)) =
                this.stop_at(|token| {
                    matches!(
                        token, Reading::Unit(Token::Punctuation(pun))
                        if pun.punctuation == separator
                    )
                })
            {
                this.forward();
                Some(pun)
            } else {
                None
            }
        }

        let delimited_tree = parser.step_into(
            delimiter,
            |parser| {
                let mut first = None;
                let mut rest = Vec::new();
                let mut trailing_separator: Option<Punctuation> = None;

                while !parser.is_exhausted() {
                    let Ok(element) = parser.parse_syntax_tree::<T>(handler)
                    else {
                        skip_to_next_separator(parser, separator);
                        continue;
                    };

                    // adds new element
                    match (&first, &trailing_separator) {
                        (None, None) => {
                            first = Some(element);
                        }
                        (Some(_), Some(separator)) => {
                            rest.push((separator.clone(), element));
                            trailing_separator = None;
                        }
                        _ => unreachable!(),
                    }

                    // expect separator if not exhausted
                    if !parser.is_exhausted() {
                        let Ok(separator) = parser.parse(separator, handler)
                        else {
                            if let Some(punctuation) =
                                skip_to_next_separator(parser, separator)
                            {
                                trailing_separator = Some(punctuation);
                            }

                            continue;
                        };

                        trailing_separator = Some(separator);
                    }
                }

                Ok(first.map(|first| ConnectedList {
                    first,
                    rest,
                    trailing_separator,
                }))
            },
            handler,
        )?;

        Ok(DelimitedList {
            open: delimited_tree.open,
            tree: delimited_tree.tree.unwrap(),
            close: delimited_tree.close,
        })
    })
}

impl<Element: SourceElement, Separator: SourceElement> SourceElement
    for ConnectedList<Element, Separator>
{
    fn span(&self) -> Span {
        let end = self.trailing_separator.as_ref().map_or_else(
            || {
                self.rest.last().map_or_else(
                    || self.first.span(),
                    |(_, element)| element.span(),
                )
            },
            SourceElement::span,
        );

        self.first.span().join(&end).unwrap()
    }
}

impl<Element, Separator> ConnectedList<Element, Separator> {
    /// Returns an iterator over the elements of the list.
    pub fn elements(&self) -> impl Iterator<Item = &Element> {
        std::iter::once(&self.first)
            .chain(self.rest.iter().map(|(_, element)| element))
    }

    /// Returns an iterator over the elements of the list.
    pub fn into_elements(self) -> impl Iterator<Item = Element> {
        std::iter::once(self.first)
            .chain(self.rest.into_iter().map(|(_, element)| element))
    }

    /// Gets the number of elements in the list.
    pub fn len(&self) -> usize { self.rest.len() + 1 }

    /// Returns `true` if the list is empty.
    ///
    /// The function will never return `false`.
    pub const fn is_empty(&self) -> bool { false }
}

/// Syntax Synopsis:
/// ``` txt
/// AccessModifier:
///     'public'
///      | 'private'
///      | 'internal'
///      ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum AccessModifier {
    Public(Keyword),
    Private(Keyword),
    Internal(Keyword),
}

impl Parse for AccessModifier {
    fn syntax() -> impl Syntax<Output = Self> {
        KeywordKind::Public
            .map(AccessModifier::Public)
            .or_else(KeywordKind::Private.map(AccessModifier::Private))
            .or_else(KeywordKind::Internal.map(AccessModifier::Internal))
    }
}

impl SourceElement for AccessModifier {
    fn span(&self) -> Span {
        match self {
            Self::Public(k) | Self::Private(k) | Self::Internal(k) => {
                k.span.clone()
            }
        }
    }
}

/// Represents a syntax tree node of two consecutive colon tokens.
///
/// This syntax tree is used to represent the scope separator `::` in the
/// qualified identifier syntax
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct ScopeSeparator {
    #[get = "pub"]
    first: Punctuation,
    #[get = "pub"]
    second: Punctuation,
}

impl Parse for ScopeSeparator {
    fn syntax() -> impl Syntax<Output = Self> {
        ':'.then_do(|parser, first, handler| {
            Ok(ScopeSeparator {
                first,
                second: parser.parse_dont_skip(':', handler)?,
            })
        })
    }
}

impl SourceElement for ScopeSeparator {
    fn span(&self) -> Span { self.first.span.join(&self.second.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// LifetimeIdentifier:
///     Identifier
///     | 'static'
///     | '..'
///     ;
/// ```
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum LifetimeIdentifier {
    Identifier(Identifier),
    Static(Keyword),
    Elided(Elided),
}

impl SourceElement for LifetimeIdentifier {
    fn span(&self) -> Span {
        match self {
            Self::Identifier(ident) => ident.span.clone(),
            Self::Static(keyword) => keyword.span.clone(),
            Self::Elided(elided) => elided.span(),
        }
    }
}

impl Parse for LifetimeIdentifier {
    fn syntax() -> impl Syntax<Output = Self> {
        ExpectIdentifier
            .map(LifetimeIdentifier::Identifier)
            .or_else(KeywordKind::Static.map(LifetimeIdentifier::Static))
            .or_else('.'.then_do(|parser, first, handler| {
                Ok(LifetimeIdentifier::Elided(Elided {
                    first_dot: first,
                    second_dot: parser.parse_dont_skip('.', handler)?,
                }))
            }))
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Lifetime:
///     '/'' LifetimeIdentifier
///     ;
/// ``
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Lifetime {
    #[get = "pub"]
    apostrophe: Punctuation,
    #[get = "pub"]
    identifier: LifetimeIdentifier,
}

impl SourceElement for Lifetime {
    fn span(&self) -> Span {
        self.apostrophe.span.join(&self.identifier.span()).unwrap()
    }
}

impl Parse for Lifetime {
    fn syntax() -> impl Syntax<Output = Self> {
        '\''.then_do(|parser, apostrophe, handler| {
            Ok(Lifetime {
                apostrophe,
                identifier: parser.parse_syntax_tree(handler)?,
            })
        })
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Constant:
///     Expression
///     | Elided
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Constant {
    Expression(Box<Expression>),
    Elided(Elided),
}

impl Parse for Constant {
    fn syntax() -> impl Syntax<Output = Self> {
        Expression::syntax()
            .map(Box::new)
            .map(Constant::Expression)
            .or_else(Elided::syntax().map(Constant::Elided))
    }
}

impl SourceElement for Constant {
    fn span(&self) -> Span {
        match self {
            Self::Expression(expr) => expr.span(),
            Self::Elided(elided) => elided.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ConstantArgument:
///     '{' Constant '}'
///     ;
/// ```
pub type ConstantArgument = DelimitedTree<Constant>;

impl Parse for ConstantArgument {
    fn syntax() -> impl Syntax<Output = Self> {
        delimited_tree(Delimiter::Brace, Constant::syntax())
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Elided:
///     '..'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Elided {
    #[get = "pub"]
    first_dot: Punctuation,
    #[get = "pub"]
    second_dot: Punctuation,
}

impl Parse for Elided {
    fn syntax() -> impl Syntax<Output = Self> {
        '.'.then_do(|parser, f, handler| {
            Ok(Elided {
                first_dot: f,
                second_dot: parser.parse_dont_skip('.', handler)?,
            })
        })
    }
}

impl SourceElement for Elided {
    fn span(&self) -> Span {
        self.first_dot.span.join(&self.second_dot.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// GenericArgument:
///     Type
///     | ConstantArgument
///     | Lifetime
///     ;
/// ```
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum GenericArgument {
    Type(Box<Type>),
    Constant(ConstantArgument),
    Lifetime(Lifetime),
}

impl Parse for GenericArgument {
    fn syntax() -> impl Syntax<Output = Self> {
        Type::syntax()
            .map(Box::new)
            .map(GenericArgument::Type)
            .or_else(ConstantArgument::syntax().map(GenericArgument::Constant))
            .or_else(Lifetime::syntax().map(GenericArgument::Lifetime))
    }
}

impl SourceElement for GenericArgument {
    fn span(&self) -> Span {
        match self {
            Self::Type(type_specifier) => type_specifier.span(),
            Self::Lifetime(lifetime_argument) => lifetime_argument.span(),
            Self::Constant(const_argument) => const_argument.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// GenericArguments:
///     '[' (GenericArgument (',' GenericArgument)*)? ']'
///     ;
/// ```
pub type GenericArguments = DelimitedList<GenericArgument>;

impl Parse for GenericArguments {
    fn syntax() -> impl Syntax<Output = Self> {
        delimited_list(Delimiter::Bracket, ',')
    }
}

/// Syntax Synopsis:
/// ``` txt
/// GenericIdentifier:
///     Identifier GenericArguments?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct GenericIdentifier {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    generic_arguments: Option<GenericArguments>,
}

impl Parse for GenericIdentifier {
    fn syntax() -> impl Syntax<Output = Self> {
        ExpectIdentifier.then_do(|parser, identifier, handler| {
            Ok(GenericIdentifier {
                identifier,
                generic_arguments: parser.parse_syntax_tree(handler)?,
            })
        })
    }
}

impl SourceElement for GenericIdentifier {
    fn span(&self) -> Span {
        self.generic_arguments.as_ref().map_or_else(
            || self.identifier.span(),
            |generic_arguments| {
                self.identifier.span.join(&generic_arguments.span()).unwrap()
            },
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// LifetimeParameter:
///     '\'' Identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct LifetimeParameter {
    #[get = "pub"]
    apostrophe: Punctuation,
    #[get = "pub"]
    identifier: Identifier,
}

impl Parse for LifetimeParameter {
    fn syntax() -> impl Syntax<Output = Self> {
        '\''.then_do(|parser, apostrophe, handler| {
            Ok(LifetimeParameter {
                apostrophe,
                identifier: parser.parse_syntax_tree(handler)?,
            })
        })
    }
}

impl SourceElement for LifetimeParameter {
    fn span(&self) -> Span {
        self.apostrophe.span.join(&self.identifier.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ```txt
/// SimplePathRoot:
///     'target'
///     | Identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum SimplePathRoot {
    Target(Keyword),
    Identifier(Identifier),
}

impl Parse for SimplePathRoot {
    fn syntax() -> impl Syntax<Output = Self> {
        KeywordKind::Target
            .map(SimplePathRoot::Target)
            .or_else(ExpectIdentifier.map(SimplePathRoot::Identifier))
    }
}

impl SourceElement for SimplePathRoot {
    fn span(&self) -> Span {
        match self {
            Self::Target(target) => target.span.clone(),
            Self::Identifier(identifier) => identifier.span.clone(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// SimplePath:
///     SimplePathRoot ('::' Identifier)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct SimplePath {
    #[get = "pub"]
    root: SimplePathRoot,
    #[get = "pub"]
    rest: Vec<(ScopeSeparator, Identifier)>,
}

impl Parse for SimplePath {
    fn syntax() -> impl Syntax<Output = Self> {
        SimplePathRoot::syntax().then_do(|parser, root, handler| {
            let mut rest = Vec::new();

            while let Some(separator) = parser.parse_syntax_tree(handler)? {
                rest.push((separator, parser.parse_syntax_tree(handler)?));
            }

            Ok(SimplePath { root, rest })
        })
    }
}

impl SourceElement for SimplePath {
    fn span(&self) -> Span {
        self.rest.last().map_or_else(
            || self.root.span(),
            |last| self.root.span().join(&last.1.span).unwrap(),
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// QualifiedIdentifierRoot:
///     'target'
///     | 'this'
///     | 'super'
///     | GenericIdentifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum QualifiedIdentifierRoot {
    Target(Keyword),
    This(Keyword),
    GenericIdentifier(GenericIdentifier),
}

impl Parse for QualifiedIdentifierRoot {
    fn syntax() -> impl Syntax<Output = Self> {
        KeywordKind::Target
            .map(QualifiedIdentifierRoot::Target)
            .or_else(KeywordKind::This.map(QualifiedIdentifierRoot::This))
            .or_else(
                GenericIdentifier::syntax()
                    .map(QualifiedIdentifierRoot::GenericIdentifier),
            )
    }
}

impl SourceElement for QualifiedIdentifierRoot {
    fn span(&self) -> Span {
        match self {
            Self::Target(keyword) | Self::This(keyword) => keyword.span.clone(),
            Self::GenericIdentifier(ident) => ident.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// QualifiedIdentifier:
///     '::'? GenericIdentifier ('::' GenericIdentifier)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct QualifiedIdentifier {
    #[get = "pub"]
    root: QualifiedIdentifierRoot,
    #[get = "pub"]
    rest: Vec<(ScopeSeparator, GenericIdentifier)>,
}

impl Parse for QualifiedIdentifier {
    fn syntax() -> impl Syntax<Output = Self> {
        QualifiedIdentifierRoot::syntax().then_do(|parser, root, handler| {
            let mut rest = Vec::new();

            while let Some(separator) = parser.parse_syntax_tree(handler)? {
                rest.push((separator, parser.parse_syntax_tree(handler)?));
            }

            Ok(QualifiedIdentifier { root, rest })
        })
    }
}

impl SourceElement for QualifiedIdentifier {
    fn span(&self) -> Span {
        self.rest.last().map_or_else(
            || self.root.span(),
            |(_, identifier)| {
                self.root.span().join(&identifier.span()).unwrap()
            },
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Label:
///     '\'' Identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Label {
    #[get = "pub"]
    apostrophe: Punctuation,
    #[get = "pub"]
    identifier: Identifier,
}

impl Parse for Label {
    fn syntax() -> impl Syntax<Output = Self> {
        '\''.then_do(|parser, apostrophe, handler| {
            Ok(Label {
                apostrophe,
                identifier: parser.parse_syntax_tree(handler)?,
            })
        })
    }
}

impl SourceElement for Label {
    fn span(&self) -> Span {
        self.apostrophe.span.join(&self.identifier.span).unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// ReferenceOf:
///    '&' 'mutable'?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ReferenceOf {
    #[get = "pub"]
    ampersand: Punctuation,
    #[get = "pub"]
    mutable_keyword: Option<Keyword>,
}

impl Parse for ReferenceOf {
    fn syntax() -> impl Syntax<Output = Self> {
        '&'.then_do(|parser, ampersand, handler| {
            Ok(ReferenceOf {
                ampersand,
                mutable_keyword: parser
                    .parse(KeywordKind::Mutable.or_none(), handler)?,
            })
        })
    }
}

impl SourceElement for ReferenceOf {
    fn span(&self) -> Span {
        self.mutable_keyword.as_ref().map_or_else(
            || self.ampersand.span(),
            |keyword| self.ampersand.span().join(&keyword.span()).unwrap(),
        )
    }
}

/// Similar to [`ConnectedList`] but specifically for list of arguments
/// separated by plus sings and has no trailing separator.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct UnionList<T> {
    /// The first element of the list.
    #[get = "pub"]
    first: T,

    /// The rest of the elements of the list.
    #[get = "pub"]
    rest: Vec<(Punctuation, T)>,
}

impl<T: Parse> Parse for UnionList<T> {
    fn syntax() -> impl Syntax<Output = Self> {
        T::syntax().then_do(|parser, first, handler| {
            let mut rest = Vec::new();

            while let Some(plus) = parser.parse('+'.or_none(), handler)? {
                rest.push((plus, parser.parse_syntax_tree(handler)?));
            }

            Ok(UnionList { first, rest })
        })
    }
}

impl<T> UnionList<T> {
    /// Returns an iterator containing references to the elements of the list.
    pub fn elements(&self) -> impl Iterator<Item = &T> {
        std::iter::once(&self.first).chain(self.rest.iter().map(|(_, t)| t))
    }

    /// Returns an iterator containing the elements of the list.
    pub fn into_elements(self) -> impl Iterator<Item = T> {
        std::iter::once(self.first).chain(self.rest.into_iter().map(|(_, t)| t))
    }
}

impl<T: SourceElement> SourceElement for UnionList<T> {
    fn span(&self) -> Span {
        let first = self.first.span();
        match self.rest.last() {
            Some(last) => first.join(&last.1.span()).unwrap(),
            None => first,
        }
    }
}

#[cfg(test)]
mod test;
