//! Contains all the definition of syntax trees and their parsing functions.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::{Identifier, Keyword, Punctuation};
use pernixc_source::{SourceElement, Span, SpanError};
use pernixc_system::diagnostic::{Dummy, Handler};

use crate::{
    error::Error,
    parser::{Frame, Parser, Result as ParserResult},
};

/// Represents a syntax tree node with a pattern of syntax tree nodes separated by a separator.
///
/// This struct is useful for representing syntax tree nodes that are separated by a separator.
/// For example, a comma separated list of expressions such as `1, 2, 3` can be represented by a
/// [`ConnectedList`] with the separator being a comma token and the elements being the expressions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

/// Represents a syntax tree node of two consecutive colon tokens.
///
/// This syntax tree is used to represent the scope separator `::` in the qualified identifier
/// syntax
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum LifetimeArgumentIdentifier {
    Identifier(Identifier),
    StaticKeyword(Keyword),
}

impl SourceElement for LifetimeArgumentIdentifier {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Identifier(ident) => Ok(ident.span.clone()),
            Self::StaticKeyword(keyword) => Ok(keyword.span.clone()),
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LifetimeArgument {
    pub apostrophe: Punctuation,
    pub lifetime_argument_identifier: LifetimeArgumentIdentifier,
}

impl SourceElement for LifetimeArgument {
    fn span(&self) -> Result<Span, SpanError> {
        self.apostrophe
            .span
            .join(&self.lifetime_argument_identifier.span()?)
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
    fn span(&self) -> Result<Span, SpanError> {
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericIdentifier {
    identifier: Identifier,
    generic_arguments: Option<GenericArguments>,
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QualifiedIdentifier {
    pub leading_separator: Option<ScopeSeparator>,
    pub identifiers: ConnectedList<GenericIdentifier, ScopeSeparator>,
}

impl SourceElement for QualifiedIdentifier {
    fn span(&self) -> Result<Span, SpanError> {
        if let Some(leading_separator) = &self.leading_separator {
            leading_separator.first.span.join(&self.identifiers.span()?)
        } else {
            self.identifiers.span()
        }
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReferenceTypeSpecifier {
    pub ampersand: Punctuation,
    pub lifetime_argument: Option<LifetimeArgument>,
    pub reference_qualifier: Option<ReferenceQualifier>,
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum TypeSpecifier {
    PrimitiveTypeSpecifier(PrimitiveTypeSpecifier),
    QualifiedIdentifier(QualifiedIdentifier),
    ReferenceTypeSpecifier(ReferenceTypeSpecifier),
}

impl SourceElement for TypeSpecifier {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::PrimitiveTypeSpecifier(primitive) => primitive.span(),
            Self::QualifiedIdentifier(qualified) => qualified.span(),
            Self::ReferenceTypeSpecifier(reference) => reference.span(),
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeAnnotation {
    pub colon: Punctuation,
    pub type_specifier: TypeSpecifier,
}

impl SourceElement for TypeAnnotation {
    fn span(&self) -> Result<Span, SpanError> { self.colon.span.join(&self.type_specifier.span()?) }
}

/// Represents a moulde path in used in `module` declarations and `using` statements.
///
/// Syntax Synopsis:
/// ``` txt
/// ModulePath:
///     Identifier ('::' Identifier)*
///     ;
/// ```
pub type ModulePath = ConnectedList<Identifier, ScopeSeparator>;

impl<'a> Frame<'a> {
    /// Parses a [`ModulePath`]
    pub fn parse_module_path(&mut self, handler: &impl Handler<Error>) -> ParserResult<ModulePath> {
        let first = self.parse_identifier(handler)?;
        let mut rest = Vec::new();

        while let Ok(scope_separator) = self.try_parse(|this| this.parse_scope_separator(&Dummy)) {
            let identifier = self.parse_identifier(handler)?;
            rest.push((scope_separator, identifier));
        }

        Ok(ConnectedList {
            first,
            rest,
            trailing_separator: None,
        })
    }

    /// Parses a [`ScopeSeparator`]
    pub fn parse_scope_separator(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<ScopeSeparator> {
        let first = self.parse_punctuation(':', true, handler)?;
        let second = self.parse_punctuation(':', false, handler)?;

        Ok(ScopeSeparator { first, second })
    }
}

impl<'a> Parser<'a> {
    /// Parses a [`GenericIdentifier`]
    pub fn parse_generic_identifier(
        &mut self,
        use_turbofish: bool,
        handler: &impl Handler<Error>,
    ) -> Option<GenericIdentifier> {
        let identifier = self.expect_identifier(handler)?;

        // parse the generic argument list (if any)
        let generic_arguments = if use_turbofish {
            let result = self.try_parse(|this| {
                let colon = this.expect_punctuation(':', &Dummy)?;
                match this.next_token() {
                    Some(Token::Punctuation(punc)) if punc.punctuation == '<' => {
                        Some((colon, punc))
                    }
                    _ => None,
                }
            });

            match result {
                Some((colon, left_angle)) => {
                    Some(self.parse_generic_arguments(Some(colon), left_angle, handler)?)
                }
                None => None,
            }
        } else {
            let left_angle = self.try_parse(|this| this.expect_punctuation('<', &Dummy));

            match left_angle {
                Some(left_angle) => Some(self.parse_generic_arguments(None, left_angle, handler)?),
                None => None,
            }
        };
        Some(GenericIdentifier {
            identifier,
            generic_arguments,
        })
    }

    /// Parses a [`QualifiedIdentifier`]
    pub fn parse_qualified_identifier(
        &mut self,
        handler: &impl Handler<Error>,
        use_turbofish: bool,
    ) -> Option<QualifiedIdentifier> {
        // found a leading separator
        let leading_separator = self.try_parse(|this| this.parse_scope_separator(&Dummy));

        // expect the first identifier
        let first_identifier = self.parse_generic_identifier(use_turbofish, handler)?;

        let mut rest = Vec::new();

        // check if the next token is a scope separator '::'
        while let Some(scope_separator) = self.try_parse(|this| this.parse_scope_separator(&Dummy))
        {
            // must be followed by an identifier
            let identifier = self.parse_generic_identifier(use_turbofish, handler)?;

            rest.push((scope_separator, identifier));
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
        colon: Option<Punctuation>,
        left_angle: Punctuation,
        handler: &impl Handler<Error>,
    ) -> Option<GenericArguments> {
        let (argument_list, right_angle) = self.parse_enclosed_list(
            '>',
            ',',
            |this| match this.peek_significant_token() {
                Some(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                    // eat the apostrophe
                    this.next_token();
                    let lifetime_identifier = this.parse_lifetime_argument_identifier(handler)?;

                    Some(GenericArgument::LifetimeArgument(LifetimeArgument {
                        apostrophe,
                        lifetime_argument_identifier: lifetime_identifier,
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

    fn parse_lifetime_argument_identifier(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<LifetimeArgumentIdentifier> {
        match self.peek_significant_token() {
            Some(Token::Keyword(static_keyword))
                if static_keyword.keyword == KeywordKind::Static =>
            {
                // eat the static keyword
                self.next_token();

                Some(LifetimeArgumentIdentifier::StaticKeyword(static_keyword))
            }
            _ => {
                let identifier = self.expect_identifier(handler)?;
                Some(LifetimeArgumentIdentifier::Identifier(identifier))
            }
        }
    }

    fn parse_reference_type_specifier(
        &mut self,
        ampersand: Punctuation,
        handler: &impl Handler<Error>,
    ) -> Option<ReferenceTypeSpecifier> {
        // parse additional lifetime argument
        let lifetime_argument = match self.peek_significant_token() {
            Some(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                // eat the apostrophe
                self.next_token();

                let lifetime_argument_identifier =
                    self.parse_lifetime_argument_identifier(handler)?;

                Some(LifetimeArgument {
                    apostrophe,
                    lifetime_argument_identifier,
                })
            }
            _ => None,
        };

        // parse additional reference qualifiers
        let reference_qualifier = match self.peek_significant_token() {
            Some(Token::Keyword(mutable_keyword))
                if mutable_keyword.keyword == KeywordKind::Mutable =>
            {
                // eat the mut keyword
                self.next_token();

                Some(ReferenceQualifier::Mutable(mutable_keyword))
            }

            Some(Token::Keyword(restrict_keyword))
                if restrict_keyword.keyword == KeywordKind::Restrict =>
            {
                // eat the restrict keyword
                self.next_token();

                Some(ReferenceQualifier::Restrict(restrict_keyword))
            }
            _ => None,
        };

        let operand_type = Box::new(self.parse_type_specifier(handler)?);

        Some(ReferenceTypeSpecifier {
            ampersand,
            lifetime_argument,
            reference_qualifier,
            operand_type,
        })
    }

    /// Parses a [`TypeSpecifier`]
    pub fn parse_type_specifier(&mut self, handler: &impl Handler<Error>) -> Option<TypeSpecifier> {
        if let Some(token) = self.peek_significant_token() {
            match token {
                Token::Punctuation(punc) if punc.punctuation == ':' => {
                    Some(TypeSpecifier::QualifiedIdentifier(
                        self.parse_qualified_identifier(handler, false)?,
                    ))
                }
                Token::Punctuation(ampersand) if ampersand.punctuation == '&' => {
                    // eat the ampersand
                    self.next_token();

                    self.parse_reference_type_specifier(ampersand, handler)
                        .map(TypeSpecifier::ReferenceTypeSpecifier)
                }
                Token::Identifier(..) => Some(TypeSpecifier::QualifiedIdentifier(
                    self.parse_qualified_identifier(handler, false)?,
                )),
                Token::Keyword(keyword) => {
                    // eat the token right away
                    self.next_token();

                    let primitive_type = match keyword.keyword {
                        KeywordKind::Bool => PrimitiveTypeSpecifier::Bool(keyword),
                        KeywordKind::Void => PrimitiveTypeSpecifier::Void(keyword),
                        KeywordKind::Float32 => PrimitiveTypeSpecifier::Float32(keyword),
                        KeywordKind::Float64 => PrimitiveTypeSpecifier::Float64(keyword),
                        KeywordKind::Int8 => PrimitiveTypeSpecifier::Int8(keyword),
                        KeywordKind::Int16 => PrimitiveTypeSpecifier::Int16(keyword),
                        KeywordKind::Int32 => PrimitiveTypeSpecifier::Int32(keyword),
                        KeywordKind::Int64 => PrimitiveTypeSpecifier::Int64(keyword),
                        KeywordKind::Uint8 => PrimitiveTypeSpecifier::Uint8(keyword),
                        KeywordKind::Uint16 => PrimitiveTypeSpecifier::Uint16(keyword),
                        KeywordKind::Uint32 => PrimitiveTypeSpecifier::Uint32(keyword),
                        KeywordKind::Uint64 => PrimitiveTypeSpecifier::Uint64(keyword),
                        _ => return None,
                    };

                    Some(primitive_type.into())
                }
                token => {
                    // eat the token, make progress
                    self.next_token();

                    handler.recieve(TypeSpecifierExpected { found: Some(token) }.into());
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
        handler: &impl Handler<Error>,
    ) -> Option<TypeAnnotation> {
        let colon = self.expect_punctuation(':', handler)?;
        let type_specifier = self.parse_type_specifier(handler)?;

        Some(TypeAnnotation {
            colon,
            type_specifier,
        })
    }
}
