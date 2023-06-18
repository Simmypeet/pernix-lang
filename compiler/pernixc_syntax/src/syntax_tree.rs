//! Contains all the definition of syntax trees and their parsing functions.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};
use pernixc_source::{SourceElement, Span, SpanError};
use pernixc_system::diagnostic::{Dummy, Handler};

use crate::{
    error::{
        Error, GenericArgumentParameterListCannotBeEmpty, IdentifierExpected, TypeSpecifierExpected,
    },
    parser::{Error as ParserError, Parser, Result as ParserResult},
};

pub mod expression;
pub mod input;
pub mod item;
pub mod statement;
pub mod target;

/// Represents a syntax tree node with a pattern of syntax tree nodes separated by a separator.
///
/// This struct is useful for representing syntax tree nodes that are separated by a separator.
/// For example, a comma separated list of expressions such as `1, 2, 3` can be represented by a
/// [`ConnectedList`] with the separator being a comma token and the elements being the expressions.
#[derive(Debug, Clone)]
pub struct ConnectedList<Element, Separator> {
    /// The first element of the list.
    pub first: Element,

    /// The rest of the elements of the list.
    ///
    /// Each element of the list is a tuple containing the separator and the element. The separator
    /// is the token/syntax tree node that separates the current element from the prior one.
    pub rest: Vec<(Separator, Element)>,

    /// The trailing separator of the list.
    pub trailing_separator: Option<Separator>,
}

/// Is the result of [`Parser::parse_enclosed_frame()`].
#[derive(Debug, Clone)]
pub struct EnclosedList<T> {
    /// The open delimiter of the list.
    pub open: Punctuation,

    /// The list of elements.
    pub list: Option<ConnectedList<T, Punctuation>>,

    /// The close delimiter of the list.
    pub close: Punctuation,
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
    pub fn parse_enclosed_tree<T, H: Handler<Error>>(
        &mut self,
        delimiter: Delimiter,
        separator: char,
        mut f: impl FnMut(&mut Self, &H) -> ParserResult<T>,
        handler: &H,
    ) -> ParserResult<EnclosedList<T>> {
        fn skip_to_next_separator(this: &mut Parser, separator: char) {
            // skip to next separator
            if this
                .stop_at(|token| {
                    token
                        .as_punctuation()
                        .map_or(false, |x| x.punctuation == separator)
                })
                .map_or(false, |x| {
                    x.as_punctuation()
                        .map_or(false, |x| x.punctuation == separator)
                })
            {
                this.forward();
            }
        }

        // step into the delimited
        let open = self.step_into(delimiter, handler)?;

        let mut first = None;
        let mut rest = Vec::new();
        let mut trailing_separator: Option<Punctuation> = None;

        while !self.is_exhausted() {
            let Ok(element) = f(self, handler) else {
                skip_to_next_separator(self, separator);
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
            if !self.is_exhausted() {
                let Ok(separator) = self.parse_punctuation(separator, true, handler) else {
                    skip_to_next_separator(self, separator);
                    continue;
                };

                trailing_separator = Some(separator);
            }
        }

        // step out from the delimited list
        let close = self
            .step_out(handler)
            .expect("must be able to step out, the list is exhausted");

        Ok(EnclosedList {
            open,
            list: first.map(|first| ConnectedList {
                first,
                rest,
                trailing_separator,
            }),
            close,
        })
    }
}

impl<Element: SourceElement, Separator: SourceElement> SourceElement
    for ConnectedList<Element, Separator>
{
    fn span(&self) -> Result<Span, SpanError> {
        let end = self.trailing_separator.as_ref().map_or_else(
            || {
                self.rest
                    .last()
                    .map_or_else(|| self.first.span(), |(_, element)| element.span())
            },
            pernixc_source::SourceElement::span,
        )?;

        self.first.span()?.join(&end)
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

/// Represents a syntax tree for an access modifier.
///
/// Syntax Synopsis:
/// ```text
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
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Public(k) | Self::Private(k) | Self::Internal(k) => Ok(k.span.clone()),
        }
    }
}

/// Represents a syntax tree node of two consecutive colon tokens.
///
/// This syntax tree is used to represent the scope separator `::` in the qualified identifier
/// syntax
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ScopeSeparator {
    pub first: Punctuation,
    pub second: Punctuation,
}

impl SourceElement for ScopeSeparator {
    fn span(&self) -> Result<Span, SpanError> { self.first.span.join(&self.second.span) }
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
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum LifetimeArgumentIdentifier {
    Identifier(Identifier),
    Static(Keyword),
}

impl SourceElement for LifetimeArgumentIdentifier {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Identifier(ident) => Ok(ident.span.clone()),
            Self::Static(keyword) => Ok(keyword.span.clone()),
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
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct LifetimeArgument {
    pub apostrophe: Punctuation,
    pub identifier: LifetimeArgumentIdentifier,
}

impl SourceElement for LifetimeArgument {
    fn span(&self) -> Result<Span, SpanError> {
        self.apostrophe.span.join(&self.identifier.span()?)
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
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum GenericArgument {
    TypeSpecifier(Box<TypeSpecifier>),
    Lifetime(LifetimeArgument),
}

impl SourceElement for GenericArgument {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::TypeSpecifier(type_specifier) => type_specifier.span(),
            Self::Lifetime(lifetime_argument) => lifetime_argument.span(),
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
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct GenericArguments {
    pub colon: Option<Punctuation>,
    pub left_angle: Punctuation,
    pub argument_list: GenericArgumentList,
    pub right_angle: Punctuation,
}

impl SourceElement for GenericArguments {
    fn span(&self) -> Result<Span, SpanError> {
        let start = self.colon.as_ref().map_or_else(
            || self.left_angle.span(),
            pernixc_source::SourceElement::span,
        )?;

        start.join(&self.right_angle.span)
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
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct GenericIdentifier {
    pub identifier: Identifier,
    pub generic_arguments: Option<GenericArguments>,
}

impl SourceElement for GenericIdentifier {
    fn span(&self) -> Result<Span, SpanError> {
        self.generic_arguments.as_ref().map_or_else(
            || self.identifier.span(),
            |generic_arguments| self.identifier.span.join(&generic_arguments.span()?),
        )
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
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct QualifiedIdentifier {
    pub leading_scope_separator: Option<ScopeSeparator>,
    pub first: GenericIdentifier,
    pub rest: Vec<(ScopeSeparator, GenericIdentifier)>,
}

impl QualifiedIdentifier {
    /// Returns an iterator over the generic identifiers in this qualified identifier.
    pub fn generic_identifiers(&self) -> impl Iterator<Item = &GenericIdentifier> {
        std::iter::once(&self.first).chain(self.rest.iter().map(|(_, ident)| ident))
    }
}

impl SourceElement for QualifiedIdentifier {
    fn span(&self) -> Result<Span, SpanError> {
        let start = self
            .leading_scope_separator
            .as_ref()
            .map_or_else(|| self.first.span(), pernixc_source::SourceElement::span)?;

        let end = self
            .rest
            .last()
            .map_or_else(|| self.first.span(), |(_, ident)| ident.span())?;

        start.join(&end)
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
#[derive(Debug, Clone, EnumAsInner)]
#[allow(missing_docs)]
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
    fn span(&self) -> Result<Span, SpanError> {
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
            | Self::Uint64(token) => Ok(token.span.clone()),
        }
    }
}

/// Represents a syntax tree node of reference qualifier.
///
/// Syntax Synopsis:
/// ``` txt
/// ReferenceQualifier:
///     'mutable'
///     | 'restrict'
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner)]
#[allow(missing_docs)]
pub enum ReferenceQualifier {
    Mutable(Keyword),
    Restrict(Keyword),
}

impl SourceElement for ReferenceQualifier {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Mutable(token) | Self::Restrict(token) => Ok(token.span.clone()),
        }
    }
}

/// Represents a syntax tree node of reference type specifier.
///
/// Syntax Synopsis:
/// ``` txt
/// ReferenceTypeSpecifier:
///     '&' LifetimeArgument? ReferenceQualifier? TypeSpecifier
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ReferenceTypeSpecifier {
    pub ampersand: Punctuation,
    pub lifetime_argument: Option<LifetimeArgument>,
    pub qualifier: Option<ReferenceQualifier>,
    pub operand_type: Box<TypeSpecifier>,
}

impl SourceElement for ReferenceTypeSpecifier {
    fn span(&self) -> Result<Span, SpanError> {
        self.ampersand.span.join(&self.operand_type.span()?)
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
///     | ReferenceTypeSpecifier
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum TypeSpecifier {
    Primitive(PrimitiveTypeSpecifier),
    QualifiedIdentifier(QualifiedIdentifier),
    Reference(ReferenceTypeSpecifier),
}

impl SourceElement for TypeSpecifier {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Primitive(primitive) => primitive.span(),
            Self::QualifiedIdentifier(qualified) => qualified.span(),
            Self::Reference(reference) => reference.span(),
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
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Label {
    pub apostrophe: Punctuation,
    pub identifier: Identifier,
}

impl SourceElement for Label {
    fn span(&self) -> Result<Span, SpanError> { self.apostrophe.span.join(&self.identifier.span) }
}

/// Represents a type annotation used to annotate the type of a symbol.
///
/// Syntax Synopsis:
/// ``` txt
/// TypeAnnotation:
///     ':' TypeSpecifier
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct TypeAnnotation {
    pub colon: Punctuation,
    pub type_specifier: TypeSpecifier,
}

impl SourceElement for TypeAnnotation {
    fn span(&self) -> Result<Span, SpanError> { self.colon.span.join(&self.type_specifier.span()?) }
}

impl<'a> Parser<'a> {
    /// Parses a [`ScopeSeparator`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_scope_separator(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<ScopeSeparator> {
        let first = self.parse_punctuation(':', true, handler)?;
        let second = self.parse_punctuation(':', false, handler)?;

        Ok(ScopeSeparator { first, second })
    }

    /// Parses a [`GenericIdentifier`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_generic_identifier(
        &mut self,
        use_turbo_fish: bool,
        handler: &impl Handler<Error>,
    ) -> ParserResult<GenericIdentifier> {
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

        Ok(GenericIdentifier {
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
    ) -> ParserResult<QualifiedIdentifier> {
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
        while let Ok(token) = self.try_parse(|frame| frame.parse_scope_separator(&Dummy)) {
            let another_identifier = self.parse_generic_identifier(use_turbo_fish, handler)?;

            rest.push((token, another_identifier));
        }

        Ok(QualifiedIdentifier {
            leading_scope_separator,
            first,
            rest,
        })
    }

    fn parse_lifetime_argument_identifier(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<LifetimeArgumentIdentifier> {
        match self.next_significant_token() {
            // static
            Some(Token::Keyword(static_keyword))
                if static_keyword.keyword == KeywordKind::Static =>
            {
                Ok(LifetimeArgumentIdentifier::Static(static_keyword))
            }

            // identifier
            Some(Token::Identifier(identifier)) => {
                Ok(LifetimeArgumentIdentifier::Identifier(identifier))
            }

            // error: lifetime argument identifier expected
            found => {
                handler.recieve(Error::IdentifierExpected(IdentifierExpected { found }));

                Err(ParserError)
            }
        }
    }

    fn parse_reference_type_specifier(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<ReferenceTypeSpecifier> {
        let ampersand = self.parse_punctuation('&', true, handler)?;
        let lifetime_argument = match self.stop_at_significant() {
            Some(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                // eat apostrophe
                self.forward();

                let lifetime_argument_identifier =
                    self.parse_lifetime_argument_identifier(handler)?;

                Some(LifetimeArgument {
                    apostrophe,
                    identifier: lifetime_argument_identifier,
                })
            }

            _ => None,
        };
        let reference_qualifier = match self.stop_at_significant() {
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Mutable => {
                self.forward();
                Some(ReferenceQualifier::Mutable(k))
            }

            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Restrict => {
                self.forward();
                Some(ReferenceQualifier::Restrict(k))
            }

            _ => None,
        };
        let operand_type = Box::new(self.parse_type_specifier(handler)?);

        Ok(ReferenceTypeSpecifier {
            ampersand,
            lifetime_argument,
            qualifier: reference_qualifier,
            operand_type,
        })
    }

    /// Parses a [`TypeSpecifier`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_type_specifier(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<TypeSpecifier> {
        match self.stop_at_significant() {
            // parse qualified identifier
            Some(Token::Punctuation(first_colon))
                if first_colon.punctuation == ':'
                    && self.peek_offset(1).map_or(
                        false,
                        |x| matches!(x, Token::Punctuation(p) if p.punctuation == ':'),
                    ) =>
            {
                Ok(TypeSpecifier::QualifiedIdentifier(
                    self.parse_qualified_identifier(false, handler)?,
                ))
            }

            // parse qualified identifier
            Some(Token::Identifier(..)) => Ok(TypeSpecifier::QualifiedIdentifier(
                self.parse_qualified_identifier(false, handler)?,
            )),

            // parse reference
            Some(Token::Punctuation(p)) if p.punctuation == '&' => self
                .parse_reference_type_specifier(handler)
                .map(TypeSpecifier::Reference),

            // primitive type
            Some(Token::Keyword(keyword))
                if matches!(
                    keyword.keyword,
                    KeywordKind::Int8
                        | KeywordKind::Int16
                        | KeywordKind::Int32
                        | KeywordKind::Int64
                        | KeywordKind::Uint8
                        | KeywordKind::Uint16
                        | KeywordKind::Uint32
                        | KeywordKind::Uint64
                        | KeywordKind::Float32
                        | KeywordKind::Float64
                        | KeywordKind::Bool
                        | KeywordKind::Void
                ) =>
            {
                // eat primitive type token
                self.next_token();

                let primitive_type = match keyword.keyword {
                    KeywordKind::Void => PrimitiveTypeSpecifier::Void(keyword),
                    KeywordKind::Bool => PrimitiveTypeSpecifier::Bool(keyword),
                    KeywordKind::Int8 => PrimitiveTypeSpecifier::Int8(keyword),
                    KeywordKind::Int16 => PrimitiveTypeSpecifier::Int16(keyword),
                    KeywordKind::Int32 => PrimitiveTypeSpecifier::Int32(keyword),
                    KeywordKind::Int64 => PrimitiveTypeSpecifier::Int64(keyword),
                    KeywordKind::Uint8 => PrimitiveTypeSpecifier::Uint8(keyword),
                    KeywordKind::Uint16 => PrimitiveTypeSpecifier::Uint16(keyword),
                    KeywordKind::Uint32 => PrimitiveTypeSpecifier::Uint32(keyword),
                    KeywordKind::Uint64 => PrimitiveTypeSpecifier::Uint64(keyword),
                    KeywordKind::Float32 => PrimitiveTypeSpecifier::Float32(keyword),
                    KeywordKind::Float64 => PrimitiveTypeSpecifier::Float64(keyword),
                    _ => unreachable!(),
                };

                Ok(TypeSpecifier::Primitive(primitive_type))
            }

            found => {
                // eat the current token / make progress
                self.forward();

                handler.recieve(Error::TypeSpecifierExpected(TypeSpecifierExpected {
                    found,
                }));

                Err(ParserError)
            }
        }
    }

    /// Parses a [`GenericArgument`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_generic_argument(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<GenericArgument> {
        match self.stop_at_significant() {
            // parse lifetime argument
            Some(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                // eat apostrophe
                self.next_token();

                let lifetime_argument_identifier =
                    self.parse_lifetime_argument_identifier(handler)?;

                Ok(GenericArgument::Lifetime(LifetimeArgument {
                    apostrophe,
                    identifier: lifetime_argument_identifier,
                }))
            }

            // parse type argument
            _ => Ok(GenericArgument::TypeSpecifier(Box::new(
                self.parse_type_specifier(handler)?,
            ))),
        }
    }

    /// Parses a [`GenericArguments`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_generic_arguments(
        &mut self,
        use_turbo_fish: bool,
        handler: &impl Handler<Error>,
    ) -> ParserResult<GenericArguments> {
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
            handler.recieve(Error::GenericArgumentParameterListCannotBeEmpty(GenericArgumentParameterListCannotBeEmpty {
                span: left_angle.span.join(&right_angle.span).unwrap(),
            }));
            return Err(ParserError);
        };

        Ok(GenericArguments {
            colon,
            left_angle,
            argument_list,
            right_angle,
        })
    }

    /// Parsers a [`TypeAnnotation`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_type_annotation(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<TypeAnnotation> {
        let colon = self.parse_punctuation(':', true, handler)?;

        let type_specifier = self.parse_type_specifier(handler)?;

        Ok(TypeAnnotation {
            colon,
            type_specifier,
        })
    }
}

#[cfg(test)]
mod tests;
