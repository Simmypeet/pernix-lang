//! Contains the definition of the syntax tree nodes.
//!
//! The syntax tree nodes can be classified into three categories:
//! - *Item*: A syntax tree node that represents a top-level item in the source code.
//! - *Statement*: A syntax tree node that represents a statement in the function body.
//! - *Expression*: A syntax tree node that represents an expression in the function body.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::TokenStream,
};
use pernixc_source::{SourceElement, Span};
use pernixc_system::error_handler::ErrorHandler;

use self::item::Item;
use crate::{
    error::{
        Error, GenericArgumentParameterListCannotBeEmpty, PunctuationExpected,
        TypeSpecifierExpected,
    },
    parser::Parser,
};

pub mod expression;
pub mod item;
pub mod statement;

/// Represents a syntax tree node with a pattern of syntax tree nodes separated by a separator.
///
/// This struct is useful for representing syntax tree nodes that are separated by a separator.
/// For example, a comma separated list of expressions such as `1, 2, 3` can be represented by a
/// [`ConnectedList`] with the separator being a comma token and the elements being the expressions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct ConnectedList<Element, Separator> {
    /// The first element of the list.
    #[get = "pub"]
    pub(crate) first: Element,

    /// The rest of the elements of the list.
    ///
    /// Each element of the list is a tuple containing the separator and the element. The separator
    /// is the token/syntax tree node that separates the current element from the prior one.
    #[get = "pub"]
    pub(crate) rest: Vec<(Separator, Element)>,

    /// The trailing separator of the list.
    #[get = "pub"]
    pub(crate) trailing_separator: Option<Separator>,
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

/// Represents a syntax tree node of two consecutive colon tokens.
///
/// This syntax tree is used to represent the scope separator `::` in the qualified identifier
/// syntax
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ScopeSeparator(pub(crate) Punctuation, pub(crate) Punctuation);

impl SourceElement for ScopeSeparator {
    fn span(&self) -> Span { self.0.span().join(&self.1.span).unwrap() }
}

/// Represents a syntax tree node of a lifetime argument identifier.
///
/// Syntax Synopsis:
/// ``` txt
/// LifetimeArgumentIdentifier:
///     Identifier
///     | 'static'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum LifetimeArgumentIdentifier {
    Identifier(Identifier),
    StaticKeyword(Keyword),
}

impl SourceElement for LifetimeArgumentIdentifier {
    fn span(&self) -> Span {
        match self {
            Self::Identifier(ident) => ident.span(),
            Self::StaticKeyword(keyword) => keyword.span(),
        }
    }
}

/// Represents a syntax tree node of a lifetime argument.
///
/// Syntax Synopsis:
/// ``` txt
/// LifetimeArgument:
///     '/'' LifetimeArgumentIdentifier
///     ;
/// ``
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct LifetimeArgument {
    #[get = "pub"]
    apostrophe: Punctuation,
    #[get = "pub"]
    lifetime_identifier: LifetimeArgumentIdentifier,
}

impl SourceElement for LifetimeArgument {
    fn span(&self) -> Span {
        self.apostrophe
            .span()
            .join(&self.lifetime_identifier.span())
            .unwrap()
    }
}

/// Represents a syntax tree node of a generic argument.
///
/// Syntax Synopsis:
/// ``` txt
/// GenericArgument:
///     TypeSpecifier
///     | LifetimeArgument
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum GenericArgument {
    TypeSpecifier(Box<TypeSpecifier>),
    LifetimeArgument(LifetimeArgument),
}

impl SourceElement for GenericArgument {
    fn span(&self) -> Span {
        match self {
            Self::TypeSpecifier(type_specifier) => type_specifier.span(),
            Self::LifetimeArgument(lifetime_argument) => lifetime_argument.span(),
        }
    }
}

/// Represents a syntax tree node of a list of generic arguments separated by commas.
///
/// Syntax Synopsis:
/// ``` txt
/// GenericArgumentList:
///     GenericArgument (',' GenericArgument)*
///     ;
/// ```
pub type GenericArgumentList = ConnectedList<GenericArgument, Punctuation>;

/// Represents a syntax tree node of a list of generic arguments.
///
/// Syntax Synopsis:
/// ``` txt
/// GenericArguments:
///     ':'? '<' GenericArgumentList '>'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
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
        let start = self
            .colon
            .as_ref()
            .map_or_else(|| self.left_angle.span(), |colon| colon.span());

        start.join(&self.right_angle.span()).unwrap()
    }
}

/// Represents a syntax tree node of an identifier with optional generic arguments.
///
/// Syntax Synopsis:
/// ``` txt
/// GenericIdentifier:
///     Identifier GenericArguments?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct GenericIdentifier {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    generic_arguments: Option<GenericArguments>,
}

impl SourceElement for GenericIdentifier {
    fn span(&self) -> Span {
        match self.generic_arguments {
            Some(generic_arguments) => self
                .identifier
                .span()
                .join(&generic_arguments.span())
                .unwrap(),
            None => self.identifier.span(),
        }
    }
}

/// Represents a syntax tree node of identifiers separated by scope separators.
///
/// Syntax Synopsis:
/// ``` txt
/// QualifiedIdentifier:
///     '::'? GenericIdentifier ('::' GenericIdentifier)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct QualifiedIdentifier {
    #[get = "pub"]
    leading_separator: Option<ScopeSeparator>,
    #[get = "pub"]
    identifiers: ConnectedList<GenericIdentifier, ScopeSeparator>,
}

impl SourceElement for QualifiedIdentifier {
    fn span(&self) -> Span {
        let start = self.leading_separator.as_ref().map_or_else(
            || self.identifiers.first.span().clone(),
            pernixc_source::SourceElement::span,
        );

        start.join(&self.identifiers.span()).unwrap()
    }
}

/// Represents a syntax tree node of primitive type specifier.
///
/// Syntax Synopsis:
/// ``` txt
/// PrimitiveTypeSpecifier:
///     'bool'
///     | 'void'
///     | 'float32'
///     | 'float64'
///     | 'int8'
///     | 'int16'
///     | 'int32'
///     | 'int64'
///     | 'uint8'
///     | 'uint16'
///     | 'uint32'
///     | 'uint64'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum PrimitiveTypeSpecifier {
    Bool(Keyword),
    Void(Keyword),
    Float32(Keyword),
    Float64(Keyword),
    Int8(Keyword),
    Int16(Keyword),
    Int32(Keyword),
    Int64(Keyword),
    Uint8(Keyword),
    Uint16(Keyword),
    Uint32(Keyword),
    Uint64(Keyword),
}

impl SourceElement for PrimitiveTypeSpecifier {
    fn span(&self) -> Span {
        match self {
            Self::Bool(token)
            | Self::Void(token)
            | Self::Float32(token)
            | Self::Float64(token)
            | Self::Int8(token)
            | Self::Int16(token)
            | Self::Int32(token)
            | Self::Int64(token)
            | Self::Uint8(token)
            | Self::Uint16(token)
            | Self::Uint32(token)
            | Self::Uint64(token) => token.span.clone(),
        }
    }
}

/// Represents a syntax tree node of type specifier.
///
/// The type specifier is used to annotate the type of various symbols in the syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// TypeSpecifier:
///     PrimitiveTypeIdentifier
///     | QualifiedIdentifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum TypeSpecifier {
    PrimitiveTypeSpecifier(PrimitiveTypeSpecifier),
    QualifiedIdentifier(QualifiedIdentifier),
}

impl SourceElement for TypeSpecifier {
    fn span(&self) -> Span {
        match self {
            Self::PrimitiveTypeSpecifier(primitive) => primitive.span(),
            Self::QualifiedIdentifier(qualified) => qualified.span(),
        }
    }
}

/// Is a syntax tree node that represents a label.
///
/// Syntax Synopsis:
/// ``` txt
/// Label:
///     '\'' Identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Label {
    #[get = "pub"]
    pub(crate) single_quote: Punctuation,
    #[get = "pub"]
    pub(crate) identifier: Identifier,
}

impl SourceElement for Label {
    fn span(&self) -> Span {
        self.single_quote
            .span()
            .join(&self.identifier.span)
            .unwrap()
    }
}

/// Represents a type annotation used to annotate the type of a symbol.
///
/// Syntax Synopsis:
/// ``` txt
/// TypeAnnotation:
///     ':' TypeSpecifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct TypeAnnotation {
    #[get = "pub"]
    pub(crate) colon: Punctuation,
    #[get = "pub"]
    pub(crate) type_specifier: TypeSpecifier,
}

impl TypeAnnotation {
    /// Dissolves the struct into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, TypeSpecifier) { (self.colon, self.type_specifier) }
}

impl SourceElement for TypeAnnotation {
    fn span(&self) -> Span { self.colon.span().join(&self.type_specifier.span()).unwrap() }
}

impl<'a> Parser<'a> {
    /// Parses a [`QualifiedIdentifier`]
    pub fn parse_qualified_identifier(
        &mut self,
        handler: &impl ErrorHandler<Error>,
        use_turbofish: bool,
    ) -> Option<QualifiedIdentifier> {
        // found a leading separator
        let leading_separator = match self.peek_significant_token().cloned() {
            Some(Token::Punctuation(first_colon)) if first_colon.punctuation == ':' => {
                // eat the first colon
                self.next_significant_token();

                // expect the second colon
                let second_colon = match self.next_token().cloned() {
                    Some(Token::Punctuation(second_colon)) if second_colon.punctuation == ':' => {
                        second_colon
                    }
                    found => {
                        handler.recieve(
                            PunctuationExpected {
                                expected: ':',
                                found,
                            }
                            .into(),
                        );
                        return None;
                    }
                };

                Some(ScopeSeparator(first_colon, second_colon))
            }
            _ => None,
        };

        // expect the first identifier
        let first_identifier = self.expect_identifier(handler)?.clone();

        let mut rest = Vec::new();
        let mut cursor = self.cursor;

        // check if the next token is a scope separator '::'
        while let Some((first_colon, second_colon)) = {
            let first_colon = match self.next_significant_token() {
                Some(Token::Punctuation(first_colon)) if first_colon.punctuation == ':' => {
                    Some(first_colon)
                }
                _ => None,
            };
            let second_colon = match self.next_token() {
                Some(Token::Punctuation(second_colon)) if second_colon.punctuation == ':' => {
                    Some(second_colon)
                }
                _ => None,
            };

            // check if two consecutive colons are found
            if let Some(x) = first_colon.zip(second_colon) {
                Some(x)
            } else {
                self.cursor = cursor;
                None
            }
        } {
            let scope_separator = ScopeSeparator(first_colon.clone(), second_colon.clone());

            // must be followed by an identifier
            let identifier = self.expect_identifier(handler)?.clone();

            rest.push((scope_separator, identifier));

            cursor = self.cursor;
        }

        Some(QualifiedIdentifier {
            leading_separator,
            identifiers: ConnectedList {
                first: first_identifier,
                rest,
                trailing_separator: None,
            },
        })
    }

    /// Parses a [`GenericArguments`]
    pub fn parse_generic_arguments(
        &mut self,
        handler: &impl ErrorHandler<Error>,
        use_turbofish: bool,
    ) -> Option<GenericArguments> {
        // colon, if use turbofish syntax
        let colon = if use_turbofish {
            Some(self.expect_punctuation(':', handler).cloned()?)
        } else {
            None
        };

        // left angle bracket
        let left_angle = self.expect_punctuation('<', handler).cloned()?;
        let (argument_list, right_angle) = self.parse_enclosed_list(
            '>',
            ',',
            |this| match self.peek_significant_token() {
                Some(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                    // eat the apostrophe
                    this.next_token();

                    let lifetime_identifier = match this.peek_significant_token() {
                        Some(Token::Keyword(static_keyword))
                            if static_keyword.keyword == KeywordKind::Static =>
                        {
                            // eat the static keyword
                            this.next_token();

                            LifetimeArgumentIdentifier::StaticKeyword(static_keyword.clone())
                        }
                        _ => {
                            let identifier = this.expect_identifier(handler)?;
                            LifetimeArgumentIdentifier::Identifier(identifier.clone())
                        }
                    };

                    Some(GenericArgument::LifetimeArgument(LifetimeArgument {
                        apostrophe: apostrophe.clone(),
                        lifetime_identifier,
                    }))
                }
                _ => Some(GenericArgument::TypeSpecifier(Box::new(
                    this.parse_type_specifier(handler)?,
                ))),
            },
            handler,
        )?;

        let Some(argument_list) = argument_list else {
            handler.recieve(Error::GenericArgumentParameterListCannotBeEmpty(
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

    /// Parses a [`TypeSpecifier`]
    pub fn parse_type_specifier(
        &mut self,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<TypeSpecifier> {
        if let Some(token) = self.peek_significant_token() {
            match token {
                Token::Punctuation(punc) if punc.punctuation == ':' => {
                    Some(TypeSpecifier::QualifiedIdentifier(
                        self.parse_qualified_identifier(handler, false)?,
                    ))
                }
                Token::Identifier(..) => Some(TypeSpecifier::QualifiedIdentifier(
                    self.parse_qualified_identifier(handler, false)?,
                )),
                Token::Keyword(keyword) => {
                    // eat the token right away
                    self.next_token();

                    let primitive_type = match keyword.keyword {
                        KeywordKind::Bool => PrimitiveTypeSpecifier::Bool(keyword.clone()),
                        KeywordKind::Void => PrimitiveTypeSpecifier::Void(keyword.clone()),
                        KeywordKind::Float32 => PrimitiveTypeSpecifier::Float32(keyword.clone()),
                        KeywordKind::Float64 => PrimitiveTypeSpecifier::Float64(keyword.clone()),
                        KeywordKind::Int8 => PrimitiveTypeSpecifier::Int8(keyword.clone()),
                        KeywordKind::Int16 => PrimitiveTypeSpecifier::Int16(keyword.clone()),
                        KeywordKind::Int32 => PrimitiveTypeSpecifier::Int32(keyword.clone()),
                        KeywordKind::Int64 => PrimitiveTypeSpecifier::Int64(keyword.clone()),
                        KeywordKind::Uint8 => PrimitiveTypeSpecifier::Uint8(keyword.clone()),
                        KeywordKind::Uint16 => PrimitiveTypeSpecifier::Uint16(keyword.clone()),
                        KeywordKind::Uint32 => PrimitiveTypeSpecifier::Uint32(keyword.clone()),
                        KeywordKind::Uint64 => PrimitiveTypeSpecifier::Uint64(keyword.clone()),
                        _ => return None,
                    };

                    Some(primitive_type.into())
                }
                token => {
                    // eat the token, make progress
                    self.next_token();

                    handler.recieve(
                        TypeSpecifierExpected {
                            found: Some(token.clone()),
                        }
                        .into(),
                    );
                    None
                }
            }
        } else {
            // make progress
            self.next_token();

            handler.recieve(TypeSpecifierExpected { found: None }.into());
            None
        }
    }

    /// Parses a [`TypeAnnotation`]
    pub fn parse_type_annotation(
        &mut self,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<TypeAnnotation> {
        let colon = self.expect_punctuation(':', handler)?;
        let type_specifier = self.parse_type_specifier(handler)?;

        Some(TypeAnnotation {
            colon: colon.clone(),
            type_specifier,
        })
    }
}

/// Is a syntax tree node that represents a list of items in a file.
///
/// Syntax Synopsis:
/// ``` txt
/// File:
///     Item*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct File {
    /// Is a list of item declarations in the file.
    #[get = "pub"]
    items: Vec<Item>,
}

impl File {
    /// Dissolves the struct into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> Vec<Item> { self.items }
}

impl File {
    /// Parses a [`File`] from a token stream.
    #[must_use]
    pub fn parse(tokens: &TokenStream, handler: &impl ErrorHandler<Error>) -> Self {
        // empty token stream
        if tokens.is_empty() {
            return Self { items: Vec::new() };
        }

        let mut cursor = tokens.cursor();
        cursor.next_token();

        // create a parser
        let mut parser = Parser::new(cursor).expect("should be no problem");

        let mut items = Vec::new();

        // parse items
        while parser.peek_significant_token().is_some() {
            parser.parse_item(handler).map_or_else(
                || {
                    // look for the next access modifier
                    parser.forward_until(|token| {
                        matches!(token, Token::Keyword(keyword) if keyword.keyword == KeywordKind::Public
                            || keyword.keyword == KeywordKind::Private)
                    });
                },
                |item| items.push(item),
            );
        }

        // return the syntax tree and the errors
        Self { items }
    }
}

#[cfg(test)]
mod tests;
