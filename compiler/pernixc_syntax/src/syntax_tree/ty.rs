use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};
use pernixc_source::{SourceElement, Span};
use pernixc_system::diagnostic::Handler;

use super::{expression::Expression, ConnectedList, LifetimeArgument, QualifiedIdentifier};
use crate::{
    error::{Error, SyntaxKind, UnexpectedSyntax},
    parser::Parser,
};

/// Syntax Synopsis:
/// ``` txt
/// Primitive:
///     'bool'
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Primitive {
    Bool(Keyword),
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

impl SourceElement for Primitive {
    fn span(&self) -> Span {
        match self {
            Self::Bool(token)
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

/// Syntax Synopsis:
/// ``` txt
/// ReferenceQualifier:
///     'mutable'
///     | 'restrict'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum ReferenceQualifier {
    Mutable(Keyword),
    Restrict(Keyword),
}

impl SourceElement for ReferenceQualifier {
    fn span(&self) -> Span {
        match self {
            Self::Mutable(token) | Self::Restrict(token) => token.span.clone(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Reference:
///     '&' LifetimeArgument? ReferenceQualifier? Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Reference {
    #[get = "pub"]
    ampersand: Punctuation,
    #[get = "pub"]
    lifetime_argument: Option<LifetimeArgument>,
    #[get = "pub"]
    qualifier: Option<ReferenceQualifier>,
    #[get = "pub"]
    operand: Box<Type>,
}

impl SourceElement for Reference {
    fn span(&self) -> Span { self.ampersand.span.join(&self.operand.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// UnpackableList:
///     Unpackable (',' Unpackable)* ','?
///     ;
/// ```
pub type UnpackableList = ConnectedList<Unpackable, Punctuation>;

/// Syntax Synopsis:
/// ``` txt
/// Tuple:
///     '(' UnpackableList? ')'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Tuple {
    #[get = "pub"]
    pub(super) left_paren: Punctuation,
    #[get = "pub"]
    pub(super) unpackable_list: Option<UnpackableList>,
    #[get = "pub"]
    pub(super) right_paren: Punctuation,
}

impl SourceElement for Tuple {
    fn span(&self) -> Span { self.left_paren.span.join(&self.right_paren.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// Array:
///     '[' Type ':' Expression ']'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Array {
    #[get = "pub"]
    left_bracket: Punctuation,
    #[get = "pub"]
    operand: Box<Type>,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    expression: Box<Expression>,
    #[get = "pub"]
    right_bracket: Punctuation,
}

impl SourceElement for Array {
    fn span(&self) -> Span {
        self.left_bracket
            .span
            .join(&self.right_bracket.span)
            .unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Pointer:
///     '*' 'mutable'? Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Pointer {
    #[get = "pub"]
    asterisk: Punctuation,
    #[get = "pub"]
    mutable_keyword: Option<Keyword>,
    #[get = "pub"]
    operand: Box<Type>,
}

impl SourceElement for Pointer {
    fn span(&self) -> Span { self.asterisk.span.join(&self.operand.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// Type:
///     Primitive
///     | QualifiedIdentifier
///     | Reference
///     | Pointer
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From)]
#[allow(missing_docs)]
pub enum Type {
    Primitive(Primitive),
    QualifiedIdentifier(QualifiedIdentifier),
    Reference(Reference),
    Pointer(Pointer),
    Tuple(Tuple),
    Array(Array),
}

impl SourceElement for Type {
    fn span(&self) -> Span {
        match self {
            Self::Primitive(primitive) => primitive.span(),
            Self::QualifiedIdentifier(qualified) => qualified.span(),
            Self::Reference(reference) => reference.span(),
            Self::Pointer(pointer) => pointer.span(),
            Self::Tuple(tuple) => tuple.span(),
            Self::Array(array) => array.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Unpackable:
///     '...'? Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Unpackable {
    #[get = "pub"]
    pub(super) ellipsis: Option<(Punctuation, Punctuation, Punctuation)>,
    #[get = "pub"]
    pub(super) ty: Box<Type>,
}

impl SourceElement for Unpackable {
    fn span(&self) -> Span {
        match &self.ellipsis {
            Some((left, ..)) => left.span.join(&self.ty.span()).unwrap(),
            None => self.ty.span(),
        }
    }
}

impl<'a> Parser<'a> {
    fn parse_reference_type(&mut self, handler: &impl Handler<Error>) -> Option<Reference> {
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
        let operand = Box::new(self.parse_type(handler)?);

        Some(Reference {
            ampersand,
            lifetime_argument,
            qualifier: reference_qualifier,
            operand,
        })
    }

    fn parse_pointer_type(&mut self, handler: &impl Handler<Error>) -> Option<Pointer> {
        let asterisk = self.parse_punctuation('*', true, handler)?;

        let mutable_keyword = match self.stop_at_significant() {
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Mutable => {
                self.forward();
                Some(k)
            }
            _ => None,
        };

        let operand_type = Box::new(self.parse_type(handler)?);

        Some(Pointer {
            asterisk,
            mutable_keyword,
            operand: operand_type,
        })
    }

    fn parse_array_type(&mut self, handler: &impl Handler<Error>) -> Option<Array> {
        let delimited_tree = self.step_into(
            Delimiter::Bracket,
            |parser| {
                let ty = parser.parse_type(handler)?;
                let colon = parser.parse_punctuation(':', true, handler)?;
                let expression = parser.parse_expression(handler)?;
                Some((ty, colon, expression))
            },
            handler,
        )?;

        let tree = delimited_tree.tree?;

        Some(Array {
            left_bracket: delimited_tree.open,
            operand: Box::new(tree.0),
            colon: tree.1,
            expression: Box::new(tree.2),
            right_bracket: delimited_tree.close,
        })
    }

    fn parse_tuple_type(&mut self, handler: &impl Handler<Error>) -> Option<Tuple> {
        let type_specifiers = self.parse_enclosed_list(
            Delimiter::Parenthesis,
            ',',
            |parser| {
                // stop at significant token
                parser.stop_at_significant();

                let ellipsis = match (parser.peek(), parser.peek_offset(1), parser.peek_offset(2)) {
                    (
                        Some(Token::Punctuation(p1)),
                        Some(Token::Punctuation(p2)),
                        Some(Token::Punctuation(p3)),
                    ) if p1.punctuation == '.'
                        && p2.punctuation == '.'
                        && p3.punctuation == '.' =>
                    {
                        // eat three dots (ellipsis)
                        parser.forward();
                        parser.forward();
                        parser.forward();

                        Some((p1, p2, p3))
                    }

                    _ => None,
                };

                let ty = parser.parse_type(handler)?;

                Some(Unpackable {
                    ellipsis,
                    ty: Box::new(ty),
                })
            },
            handler,
        )?;

        Some(Tuple {
            left_paren: type_specifiers.open,
            unpackable_list: type_specifiers.list,
            right_paren: type_specifiers.close,
        })
    }

    /// Parses a [`Type`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_type(&mut self, handler: &impl Handler<Error>) -> Option<Type> {
        match self.stop_at_significant() {
            // parse qualified identifier
            Some(Token::Punctuation(first_colon))
                if first_colon.punctuation == ':'
                    && self.peek_offset(1).map_or(
                        false,
                        |x| matches!(x, Token::Punctuation(p) if p.punctuation == ':'),
                    ) =>
            {
                Some(Type::QualifiedIdentifier(
                    self.parse_qualified_identifier(false, handler)?,
                ))
            }

            // parse qualified identifier
            Some(Token::Identifier(..)) => Some(Type::QualifiedIdentifier(
                self.parse_qualified_identifier(false, handler)?,
            )),

            // parse pointer type
            Some(Token::Punctuation(p)) if p.punctuation == '*' => {
                self.parse_pointer_type(handler).map(Type::Pointer)
            }

            // parse reference
            Some(Token::Punctuation(p)) if p.punctuation == '&' => {
                self.parse_reference_type(handler).map(Type::Reference)
            }

            // parse array type
            Some(Token::Punctuation(p)) if p.punctuation == '[' => {
                self.parse_array_type(handler).map(Type::Array)
            }

            // parse tuple type
            Some(Token::Punctuation(p)) if p.punctuation == '(' => {
                self.parse_tuple_type(handler).map(Type::Tuple)
            }

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
                ) =>
            {
                // eat primitive type token
                self.next_token();

                let primitive_type = match keyword.keyword {
                    KeywordKind::Bool => Primitive::Bool(keyword),
                    KeywordKind::Int8 => Primitive::Int8(keyword),
                    KeywordKind::Int16 => Primitive::Int16(keyword),
                    KeywordKind::Int32 => Primitive::Int32(keyword),
                    KeywordKind::Int64 => Primitive::Int64(keyword),
                    KeywordKind::Uint8 => Primitive::Uint8(keyword),
                    KeywordKind::Uint16 => Primitive::Uint16(keyword),
                    KeywordKind::Uint32 => Primitive::Uint32(keyword),
                    KeywordKind::Uint64 => Primitive::Uint64(keyword),
                    KeywordKind::Float32 => Primitive::Float32(keyword),
                    KeywordKind::Float64 => Primitive::Float64(keyword),
                    _ => unreachable!(),
                };

                Some(Type::Primitive(primitive_type))
            }

            found => {
                // eat the current token / make progress
                self.forward();

                handler.receive(Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::TypeSpecifier,
                    found,
                }));

                None
            }
        }
    }
}

#[cfg(test)]
pub(super) mod tests;
