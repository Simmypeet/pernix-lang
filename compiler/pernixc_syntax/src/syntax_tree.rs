//! Contains all the definition of syntax trees and their parsing functions.

#![allow(missing_docs)]

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    diagnostic::{Dummy, Handler},
    source_file::{SourceElement, Span},
};
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};

use self::{expression::Expression, r#type::Type};
use crate::{
    error::{self, Error, SyntaxKind},
    parser::{Parser, Reading},
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DelimitedList<T> {
    /// The opening delimiter.
    pub open: Punctuation,

    /// The list of elements inside the delimiter. If `None`, then the list is
    /// empty (immediately closed).
    pub list: Option<ConnectedList<T, Punctuation>>,

    /// The closing delimiter.
    pub close: Punctuation,
}

impl<'a> Parser<'a> {
    /// Parses a list of elements enclosed by a pair of delimiters, separated by
    /// a separator.
    ///
    /// The parser position must be at the delimited list of the given
    /// delimiter. It will consume the whole delimited list and move the
    /// next token after the list.
    ///
    /// # Errors
    /// - if the parser position is not at the delimited list of the given
    ///   delimiter.
    /// - any error returned by the given parser function.
    pub fn parse_delimited_list<T: std::fmt::Debug>(
        &mut self,
        delimiter: Delimiter,
        separator: char,
        mut f: impl FnMut(&mut Self) -> Option<T>,
        handler: &dyn Handler<error::Error>,
    ) -> Option<DelimitedList<T>> {
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

        let delimited_tree = self.step_into(
            delimiter,
            |parser| {
                let mut first = None;
                let mut rest = Vec::new();
                let mut trailing_separator: Option<Punctuation> = None;

                while !parser.is_exhausted() {
                    let Some(element) = f(parser) else {
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
                        (first, trailing_separator) => {
                            unreachable!("{first:?} {trailing_separator:?}")
                        }
                    }

                    // expect separator if not exhausted
                    if !parser.is_exhausted() {
                        let Some(separator) =
                            parser.parse_punctuation(separator, true, handler)
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

                Some(first.map(|first| ConnectedList {
                    first,
                    rest,
                    trailing_separator,
                }))
            },
            handler,
        )?;

        Some(DelimitedList {
            open: delimited_tree.open,
            list: delimited_tree.tree.unwrap(),
            close: delimited_tree.close,
        })
    }
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ConstantArgument {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    constant: Constant,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl SourceElement for ConstantArgument {
    fn span(&self) -> Span {
        self.left_brace.span.join(&self.right_brace.span).unwrap()
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
/// GenericArgumentList:
///     GenericArgument (',' GenericArgument)*
///     ;
/// ```
pub type GenericArgumentList = ConnectedList<GenericArgument, Punctuation>;

/// Syntax Synopsis:
/// ``` txt
/// GenericArguments:
///     '[' GenericArgumentList? ']'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct GenericArguments {
    #[get = "pub"]
    left_bracket: Punctuation,
    #[get = "pub"]
    argument_list: Option<GenericArgumentList>,
    #[get = "pub"]
    right_bracket: Punctuation,
}

impl SourceElement for GenericArguments {
    fn span(&self) -> Span {
        self.left_bracket.span.join(&self.right_bracket.span()).unwrap()
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

impl SourceElement for LifetimeParameter {
    fn span(&self) -> Span {
        self.apostrophe.span.join(&self.identifier.span).unwrap()
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
    leading_scope_separator: Option<ScopeSeparator>,
    #[get = "pub"]
    first: GenericIdentifier,
    #[get = "pub"]
    rest: Vec<(ScopeSeparator, GenericIdentifier)>,
}

impl QualifiedIdentifier {
    /// Returns an iterator over the generic identifiers in this qualified
    /// identifier.
    pub fn generic_identifiers(
        &self,
    ) -> impl Iterator<Item = &GenericIdentifier> {
        std::iter::once(&self.first)
            .chain(self.rest.iter().map(|(_, ident)| ident))
    }
}

impl SourceElement for QualifiedIdentifier {
    fn span(&self) -> Span {
        let start = self
            .leading_scope_separator
            .as_ref()
            .map_or_else(|| self.first.span(), SourceElement::span);

        let end = self
            .rest
            .last()
            .map_or_else(|| self.first.span(), |(_, ident)| ident.span());

        start.join(&end).unwrap()
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

impl SourceElement for Label {
    fn span(&self) -> Span {
        self.apostrophe.span.join(&self.identifier.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Qualifier:
///     'mutable'
///     | 'unique'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Qualifier {
    Mutable(Keyword),
    Unique(Keyword),
}

impl SourceElement for Qualifier {
    fn span(&self) -> Span {
        match self {
            Self::Mutable(token) | Self::Unique(token) => token.span.clone(),
        }
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

impl<'a> Parser<'a> {
    /// Parses a [`GenericIdentifier`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_generic_identifier(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<GenericIdentifier> {
        let identifier = self.parse_identifier(handler)?;

        self.stop_at_significant();
        let parse_generic_arguments = {
            self.peek()
                .into_into_delimited()
                .map_or(false, |pun| pun.0 == Delimiter::Bracket)
        };

        let generic_arguments = if parse_generic_arguments {
            Some(self.parse_generic_arguments(handler)?)
        } else {
            None
        };

        Some(GenericIdentifier { identifier, generic_arguments })
    }

    /// Parses a [`ScopeSeparator`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_scope_separator(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<ScopeSeparator> {
        let first = self.parse_punctuation(':', true, handler)?;
        let second = self.parse_punctuation(':', false, handler)?;

        Some(ScopeSeparator { first, second })
    }

    /// Parses a [`QualifiedIdentifier`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_qualified_identifier(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<QualifiedIdentifier> {
        // stop at significant tokens
        self.stop_at_significant();

        // leading scope separator
        let parse_leading_scope_separator =
            self.peek().into_unit().map_or(
                false,
                |token| matches!(token, Token::Punctuation(p) if p.punctuation == ':'),
            ) && self.peek_offset(1).and_then(|x| x.into_unit().ok()).map_or(
                false,
                |token| matches!(token, Token::Punctuation(p) if p.punctuation == ':'),
            );

        let leading_scope_separator = if parse_leading_scope_separator {
            Some(self.parse_scope_separator(handler)?)
        } else {
            None
        };

        let first = self.parse_generic_identifier(handler)?;
        let mut rest = Vec::new();

        // parses the identifier chain
        while let Some(token) =
            self.try_parse(|frame| frame.parse_scope_separator(&Dummy))
        {
            let another_identifier = self.parse_generic_identifier(handler)?;

            rest.push((token, another_identifier));
        }

        Some(QualifiedIdentifier { leading_scope_separator, first, rest })
    }

    /// Parses a [`LifetimeParameter`]
    pub fn parse_lifetime_parameter(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<LifetimeParameter> {
        let apostrophe = self.parse_punctuation('\'', true, handler)?;
        let identifier = self.parse_identifier(handler)?;

        Some(LifetimeParameter { apostrophe, identifier })
    }

    /// Parses a [`Lifetime`]
    pub fn parse_lifetime(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Lifetime> {
        let apostrophe = self.parse_punctuation('\'', true, handler)?;
        let identifier = match self.stop_at_significant() {
            Reading::Unit(Token::Identifier(identifier)) => {
                // eat identifier
                self.forward();

                LifetimeIdentifier::Identifier(identifier)
            }

            Reading::Unit(Token::Keyword(
                keyword @ Keyword { kind: KeywordKind::Static, .. },
            )) => {
                // eat keyword
                self.forward();

                LifetimeIdentifier::Static(keyword)
            }

            Reading::Unit(Token::Punctuation(
                first_dot @ Punctuation { punctuation: '.', .. },
            )) => {
                // eat first dot
                self.forward();

                let second_dot = self.parse_punctuation('.', false, handler)?;

                LifetimeIdentifier::Elided(Elided { first_dot, second_dot })
            }

            found => {
                handler.receive(Error {
                    expected: SyntaxKind::Identifier,
                    alternatives: vec![SyntaxKind::Keyword(
                        KeywordKind::Static,
                    )],
                    found: self.reading_to_found(found),
                });
                return None;
            }
        };

        Some(Lifetime { apostrophe, identifier })
    }

    /// Parses a [`ConstantArgument`]
    pub fn parse_constant_argument(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<ConstantArgument> {
        let delimited_tree = self.step_into(
            Delimiter::Brace,
            |parser| match parser.stop_at_significant() {
                Reading::Unit(Token::Punctuation(pun))
                    if pun.punctuation == '.' =>
                {
                    // eat the first dot
                    parser.forward();

                    Some(Constant::Elided(Elided {
                        first_dot: pun,
                        second_dot: parser
                            .parse_punctuation('.', false, handler)?,
                    }))
                }

                _ => Some(Constant::Expression(Box::new(
                    parser.parse_expression(handler)?,
                ))),
            },
            handler,
        )?;

        Some(ConstantArgument {
            left_brace: delimited_tree.open,
            constant: delimited_tree.tree?,
            right_brace: delimited_tree.close,
        })
    }

    /// Parses a [`GenericArgument`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_generic_argument(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<GenericArgument> {
        match self.stop_at_significant() {
            // parse lifetime argument
            Reading::Unit(Token::Punctuation(apostrophe))
                if apostrophe.punctuation == '\'' =>
            {
                Some(GenericArgument::Lifetime(self.parse_lifetime(handler)?))
            }

            // parse const argument
            Reading::IntoDelimited(Delimiter::Brace, _) => {
                Some(GenericArgument::Constant(
                    self.parse_constant_argument(handler)?,
                ))
            }

            // parse type argument
            _ => {
                Some(GenericArgument::Type(Box::new(self.parse_type(handler)?)))
            }
        }
    }

    fn parse_union_list<T>(
        &mut self,
        mut parser: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<UnionList<T>> {
        let first = parser(self)?;
        let mut rest = Vec::new();

        while let Some(plus) =
            self.try_parse(|parser| parser.parse_punctuation('+', true, &Dummy))
        {
            rest.push((plus, parser(self)?));
        }

        Some(UnionList { first, rest })
    }

    /// Parses a [`GenericArguments`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_generic_arguments(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<GenericArguments> {
        let arguments = self.parse_delimited_list(
            Delimiter::Bracket,
            ',',
            |parser| parser.parse_generic_argument(handler),
            handler,
        )?;

        Some(GenericArguments {
            left_bracket: arguments.open,
            argument_list: arguments.list,
            right_bracket: arguments.close,
        })
    }
}

#[cfg(test)]
mod test;
