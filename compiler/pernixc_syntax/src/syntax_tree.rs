//! Contains all the definition of syntax trees and their parsing functions.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};
use pernixc_source::{SourceElement, Span};
use pernixc_system::diagnostic::{Dummy, Handler};

use self::{expression::Expression, ty::Type};
use crate::{
    error::{
        self, Error, GenericArgumentParameterListCannotBeEmpty, IdentifierExpected,
        PunctuationExpected,
    },
    parser::Parser,
};

pub mod expression;
pub mod item;
pub mod pattern;
pub mod statement;
pub mod target;
pub mod ty;

/// Represents a syntax tree node with a pattern of syntax tree nodes separated by a separator.
///
/// This struct is useful for representing syntax tree nodes that are separated by a separator.
/// For example, a comma separated list of expressions such as `1, 2, 3` can be represented by a
/// [`ConnectedList`] with the separator being a comma token and the elements being the expressions.
#[derive(Debug, Clone, Getters)]
pub struct ConnectedList<Element, Separator> {
    /// The first element of the list.
    #[get = "pub"]
    first: Element,

    /// The rest of the elements of the list.
    ///
    /// Each element of the list is a tuple containing the separator and the element. The separator
    /// is the token/syntax tree node that separates the current element from the prior one.
    #[get = "pub"]
    rest: Vec<(Separator, Element)>,

    /// The trailing separator of the list.
    #[get = "pub"]
    trailing_separator: Option<Separator>,
}

#[derive(Debug, Clone)]
pub struct DelimitedList<T> {
    pub open: Punctuation,
    pub list: Option<ConnectedList<T, Punctuation>>,
    pub close: Punctuation,
}

impl<'a> Parser<'a> {
    /// Parses a list of items that are separated by a particular separator.
    ///
    /// This function is useful for parsing patterns of elements that are separated by a single
    /// character, such as comma-separated lists of expressions; where the list is enclosed in
    /// parentheses, brackets, or braces.
    pub(crate) fn parse_enclosed_list_manual<T>(
        &mut self,
        delimiter: char,
        separator: char,
        mut parse_item: impl FnMut(&mut Self) -> Option<T>,
        handler: &impl Handler<error::Error>,
    ) -> Option<(Option<ConnectedList<T, Punctuation>>, Punctuation)> {
        let mut first = None;
        let mut rest = Vec::new();
        let mut trailing_separator = None;

        // check for empty list
        match self.stop_at_significant() {
            Some(Token::Punctuation(punc)) if punc.punctuation == delimiter => {
                self.next_token();
                return Some((None, punc));
            }
            None => handler.receive(error::Error::PunctuationExpected(PunctuationExpected {
                expected: delimiter,
                found: self.get_actual_found_token(None),
            })),
            _ => (),
        }

        if let Some(value) = parse_item(self) {
            first = Some(value);
        } else {
            let token = self.stop_at(|token| match token {
                Token::Punctuation(punc) => {
                    punc.punctuation == delimiter || punc.punctuation == separator
                }
                _ => false,
            });

            // if found delimiter, return empty list
            if let Some(Token::Punctuation(token)) = token {
                if token.punctuation == delimiter {
                    self.next_token();
                    return Some((None, token));
                }
            }
        }

        let delimiter = loop {
            match self.stop_at_significant() {
                Some(Token::Punctuation(separator_token))
                    if separator_token.punctuation == separator =>
                {
                    // eat the separator
                    self.next_token();

                    match self.stop_at_significant() {
                        Some(Token::Punctuation(delimiter_token))
                            if delimiter_token.punctuation == delimiter =>
                        {
                            // eat the delimiter
                            self.next_token();

                            trailing_separator = Some(separator_token);
                            break delimiter_token;
                        }
                        _ => (),
                    }

                    parse_item(self).map_or_else(
                        || {
                            self.stop_at(|token| {
                                matches!(
                                    token,
                                    Token::Punctuation(punc) if punc.punctuation == delimiter ||
                                        punc.punctuation == separator
                                )
                            });
                        },
                        |value| {
                            if first.is_none() {
                                first = Some(value);
                            } else {
                                rest.push((separator_token.clone(), value));
                            }
                        },
                    );
                }
                Some(Token::Punctuation(delimiter_token))
                    if delimiter_token.punctuation == delimiter =>
                {
                    // eat the delimiter
                    self.next_token();

                    break delimiter_token;
                }
                found => {
                    handler.receive(error::Error::PunctuationExpected(PunctuationExpected {
                        expected: delimiter,
                        found,
                    }));
                    return None;
                }
            }
        };

        Some((
            first.map(|first| ConnectedList {
                first,
                rest,
                trailing_separator,
            }),
            delimiter,
        ))
    }
}

impl<'a> Parser<'a> {
    /// Parses a list of elements enclosed by a pair of delimiters, separated by a separator.
    ///
    /// The parser position must be at the delimited list of the given delimiter. It will
    /// consume the whole delimited list and move the next token after the list.
    ///
    /// # Errors
    /// - if the parser position is not at the delimited list of the given delimiter.
    /// - any error returned by the given parser function.
    pub fn parse_enclosed_list<T: std::fmt::Debug>(
        &mut self,
        delimiter: Delimiter,
        separator: char,
        mut f: impl FnMut(&mut Self) -> Option<T>,
        handler: &impl Handler<error::Error>,
    ) -> Option<DelimitedList<T>> {
        fn skip_to_next_separator(this: &mut Parser, separator: char) -> Option<Punctuation> {
            if let Some(Token::Punctuation(punc)) = this.stop_at(
                |token| matches!(token, Token::Punctuation(punc) if punc.punctuation == separator),
            ) {
                this.forward();
                Some(punc)
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
                        let Some(separator) = parser.parse_punctuation(separator, true, handler)
                        else {
                            if let Some(punctuation) = skip_to_next_separator(parser, separator) {
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
                self.rest
                    .last()
                    .map_or_else(|| self.first.span(), |(_, element)| element.span())
            },
            pernixc_source::SourceElement::span,
        );

        self.first.span().join(&end).unwrap()
    }
}

impl<Element, Separator> ConnectedList<Element, Separator> {
    /// Returns an iterator over the elements of the list.
    pub fn elements(&self) -> impl Iterator<Item = &Element> {
        std::iter::once(&self.first).chain(self.rest.iter().map(|(_, element)| element))
    }

    /// Returns an iterator over the elements of the list.
    pub fn into_elements(self) -> impl Iterator<Item = Element> {
        std::iter::once(self.first).chain(self.rest.into_iter().map(|(_, element)| element))
    }

    /// Gets the number of elements in the list.
    pub fn len(&self) -> usize { self.rest.len() + 1 }

    /// Returns `true` if the list is empty.
    ///
    /// The function will never return `false`.
    pub fn is_empty(&self) -> bool { false }
}

/// Syntax Synopsis:
/// ``` txt
/// AccessModifier:
///     'public'
///      | 'private'
///      | 'internal'
///      ;
/// ```
#[derive(Debug, Clone, EnumAsInner)]
#[allow(missing_docs)]
pub enum AccessModifier {
    Public(Keyword),
    Private(Keyword),
    Internal(Keyword),
}

impl SourceElement for AccessModifier {
    fn span(&self) -> Span {
        match self {
            Self::Public(k) | Self::Private(k) | Self::Internal(k) => k.span.clone(),
        }
    }
}

/// Represents a syntax tree node of two consecutive colon tokens.
///
/// This syntax tree is used to represent the scope separator `::` in the qualified identifier
/// syntax
#[derive(Debug, Clone, Getters)]
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
/// LifetimeArgumentIdentifier:
///     Identifier
///     | 'static'
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum LifetimeArgumentIdentifier {
    Identifier(Identifier),
    Static(Keyword),
}

impl SourceElement for LifetimeArgumentIdentifier {
    fn span(&self) -> Span {
        match self {
            Self::Identifier(ident) => ident.span.clone(),
            Self::Static(keyword) => keyword.span.clone(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// LifetimeArgument:
///     '/'' LifetimeArgumentIdentifier
///     ;
/// ``
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct LifetimeArgument {
    #[get = "pub"]
    apostrophe: Punctuation,
    #[get = "pub"]
    identifier: LifetimeArgumentIdentifier,
}

impl SourceElement for LifetimeArgument {
    fn span(&self) -> Span { self.apostrophe.span.join(&self.identifier.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// ConstArgument:
///     '{' Expression '}'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
pub struct ConstArgument {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    expression: Box<Expression>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl SourceElement for ConstArgument {
    fn span(&self) -> Span { self.left_brace.span.join(&self.right_brace.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// GenericArgument:
///     Type
///     | ConstArgument
///     | LifetimeArgument
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum GenericArgument {
    Type(Box<Type>),
    Const(ConstArgument),
    Lifetime(LifetimeArgument),
}

impl SourceElement for GenericArgument {
    fn span(&self) -> Span {
        match self {
            Self::Type(type_specifier) => type_specifier.span(),
            Self::Lifetime(lifetime_argument) => lifetime_argument.span(),
            Self::Const(const_argument) => const_argument.span(),
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
///     ':'? '<' GenericArgumentList '>'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct GenericArguments {
    #[get = "pub"]
    colon: Option<Punctuation>,
    #[get = "pub"]
    left_angle: Punctuation,
    #[get = "pub"]
    argument_list: GenericArgumentList,
    #[get = "pub"]
    right_angle: Punctuation,
}

impl SourceElement for GenericArguments {
    fn span(&self) -> Span {
        let start = self.colon.as_ref().map_or_else(
            || self.left_angle.span(),
            pernixc_source::SourceElement::span,
        );

        start.join(&self.right_angle.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// GenericIdentifier:
///     Identifier GenericArguments?
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
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
                self.identifier
                    .span
                    .join(&generic_arguments.span())
                    .unwrap()
            },
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// QualifiedIdentifier:
///     '::'? GenericIdentifier ('::' GenericIdentifier)*
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
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
    /// Returns an iterator over the generic identifiers in this qualified identifier.
    pub fn generic_identifiers(&self) -> impl Iterator<Item = &GenericIdentifier> {
        std::iter::once(&self.first).chain(self.rest.iter().map(|(_, ident)| ident))
    }
}

impl SourceElement for QualifiedIdentifier {
    fn span(&self) -> Span {
        let start = self
            .leading_scope_separator
            .as_ref()
            .map_or_else(|| self.first.span(), pernixc_source::SourceElement::span);

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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Label {
    #[get = "pub"]
    apostrophe: Punctuation,
    #[get = "pub"]
    identifier: Identifier,
}

impl SourceElement for Label {
    fn span(&self) -> Span { self.apostrophe.span.join(&self.identifier.span).unwrap() }
}

impl<'a> Parser<'a> {
    /// Parses a [`ScopeSeparator`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_scope_separator(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<ScopeSeparator> {
        let first = self.parse_punctuation(':', true, handler)?;
        let second = self.parse_punctuation(':', false, handler)?;

        Some(ScopeSeparator { first, second })
    }

    /// Parses a [`GenericIdentifier`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_generic_identifier(
        &mut self,
        use_turbo_fish: bool,
        handler: &impl Handler<Error>,
    ) -> Option<GenericIdentifier> {
        let identifier = self.parse_identifier(handler)?;

        self.stop_at_significant();
        let parse_generic_arguments = if use_turbo_fish {
            self.peek().as_ref().map_or(
                false,
                |token| matches!(token, Token::Punctuation(p) if p.punctuation == ':'),
            ) && self.peek_offset(1).as_ref().map_or(
                false,
                |token| matches!(token, Token::Punctuation(p) if p.punctuation == '<'),
            )
        } else {
            self.peek().as_ref().map_or(
                false,
                |token| matches!(token, Token::Punctuation(p) if p.punctuation == '<'),
            )
        };

        let generic_arguments = if parse_generic_arguments {
            Some(self.parse_generic_arguments(use_turbo_fish, handler)?)
        } else {
            None
        };

        Some(GenericIdentifier {
            identifier,
            generic_arguments,
        })
    }

    /// Parses a [`QualifiedIdentifier`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_qualified_identifier(
        &mut self,
        use_turbo_fish: bool,
        handler: &impl Handler<Error>,
    ) -> Option<QualifiedIdentifier> {
        // stop at significant tokens
        self.stop_at_significant();

        // leading scope separator
        let parse_leading_scope_separator = self.peek().as_ref().map_or(
            false,
            |token| matches!(token, Token::Punctuation(p) if p.punctuation == ':'),
        ) && self.peek_offset(1).as_ref().map_or(
            false,
            |token| matches!(token, Token::Punctuation(p) if p.punctuation == ':'),
        );

        let leading_scope_separator = if parse_leading_scope_separator {
            Some(self.parse_scope_separator(handler)?)
        } else {
            None
        };

        let first = self.parse_generic_identifier(use_turbo_fish, handler)?;
        let mut rest = Vec::new();

        // parses the identifier chain
        while let Some(token) = self.try_parse(|frame| frame.parse_scope_separator(&Dummy)) {
            let another_identifier = self.parse_generic_identifier(use_turbo_fish, handler)?;

            rest.push((token, another_identifier));
        }

        Some(QualifiedIdentifier {
            leading_scope_separator,
            first,
            rest,
        })
    }

    /// Parses a [`GenericArgument`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_generic_argument(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<GenericArgument> {
        match self.stop_at_significant() {
            // parse lifetime argument
            Some(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                // eat apostrophe
                self.next_token();

                let lifetime_argument_identifier =
                    self.parse_lifetime_argument_identifier(handler)?;

                Some(GenericArgument::Lifetime(LifetimeArgument {
                    apostrophe,
                    identifier: lifetime_argument_identifier,
                }))
            }

            // parse const argument
            Some(Token::Punctuation(left_brace)) if left_brace.punctuation == '{' => {
                let delimited_tree = self.step_into(
                    Delimiter::Brace,
                    |parser| parser.parse_expression(handler).map(Box::new),
                    handler,
                )?;

                Some(GenericArgument::Const(ConstArgument {
                    left_brace: delimited_tree.open,
                    expression: delimited_tree.tree?,
                    right_brace: delimited_tree.close,
                }))
            }

            // parse type argument
            _ => Some(GenericArgument::Type(Box::new(self.parse_type(handler)?))),
        }
    }

    /// Parses a [`GenericArguments`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_generic_arguments(
        &mut self,
        use_turbo_fish: bool,
        handler: &impl Handler<Error>,
    ) -> Option<GenericArguments> {
        let colon = if use_turbo_fish {
            Some(self.parse_punctuation(':', true, handler)?)
        } else {
            None
        };

        let left_angle = self.parse_punctuation('<', !use_turbo_fish, handler)?;

        let (argument_list, right_angle) = self.parse_enclosed_list_manual(
            '>',
            ',',
            |this| this.parse_generic_argument(handler),
            handler,
        )?;

        // cannot be empty
        let Some(argument_list) = argument_list else {
            handler.receive(Error::GenericArgumentParameterListCannotBeEmpty(
                GenericArgumentParameterListCannotBeEmpty {
                    span: left_angle.span.join(&right_angle.span).unwrap(),
                },
            ));
            return None;
        };

        Some(GenericArguments {
            colon,
            left_angle,
            argument_list,
            right_angle,
        })
    }

    fn parse_lifetime_argument_identifier(
        &mut self,
        handler: &impl Handler<error::Error>,
    ) -> Option<LifetimeArgumentIdentifier> {
        match self.next_significant_token() {
            // static
            Some(Token::Keyword(static_keyword))
                if static_keyword.keyword == KeywordKind::Static =>
            {
                Some(LifetimeArgumentIdentifier::Static(static_keyword))
            }

            // identifier
            Some(Token::Identifier(identifier)) => {
                Some(LifetimeArgumentIdentifier::Identifier(identifier))
            }

            // error: lifetime argument identifier expected
            found => {
                handler.receive(Error::IdentifierExpected(IdentifierExpected { found }));

                None
            }
        }
    }
}

#[cfg(test)]
pub(crate) mod tests;
