//! Contains all definition of type syntax trees.

#![allow(missing_docs)]

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};

use super::{ConnectedList, Constant, Elided, Lifetime, QualifiedIdentifier};
use crate::{
    error::{Error, SyntaxKind},
    parser::{Parser, Reading},
};

pub mod strategy;

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
    Usize(Keyword),
    Isize(Keyword),
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
            | Self::Uint64(token)
            | Self::Usize(token)
            | Self::Isize(token) => token.span.clone(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Reference:
///      Qualifier Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Reference {
    #[get = "pub"]
    ampersand: Punctuation,
    #[get = "pub"]
    lifetime: Option<Lifetime>,
    #[get = "pub"]
    mutable_keyword: Option<Keyword>,
    #[get = "pub"]
    operand: Box<Type>,
}

impl SourceElement for Reference {
    fn span(&self) -> Span {
        self.ampersand.span().join(&self.operand.span()).unwrap()
    }
}

impl Reference {
    /// Destructs the `Reference` into its components.
    pub fn destruct(
        self,
    ) -> (Punctuation, Option<Lifetime>, Option<Keyword>, Box<Type>) {
        (self.ampersand, self.lifetime, self.mutable_keyword, self.operand)
    }
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
    fn span(&self) -> Span {
        self.left_paren.span.join(&self.right_paren.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Array:
///     '[' Type ':' Constant ']'
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
    constant: Constant,
    #[get = "pub"]
    right_bracket: Punctuation,
}

impl SourceElement for Array {
    fn span(&self) -> Span {
        self.left_bracket.span.join(&self.right_bracket.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Pointer:
///     '*' mutable? Type
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
    fn span(&self) -> Span {
        self.asterisk.span.join(&self.operand.span()).unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Phantom:
///     'phantom' Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Phantom {
    #[get = "pub"]
    phantom_keyword: Keyword,
    #[get = "pub"]
    r#type: Box<Type>,
}

impl SourceElement for Phantom {
    fn span(&self) -> Span {
        self.phantom_keyword.span.join(&self.r#type.span()).unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Local:
///     'local' Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Local {
    #[get = "pub"]
    local_keyword: Keyword,
    #[get = "pub"]
    ty: Box<Type>,
}

impl SourceElement for Local {
    fn span(&self) -> Span {
        self.local_keyword.span.join(&self.ty.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Type:
///     Primitive
///     | QualifiedIdentifier
///     | Reference
///     | Pointer
///     | Tuple
///     | Local
///     | Array
///     | Phantom
///     | Elided
///     ;
/// ```
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Type {
    Primitive(Primitive),
    QualifiedIdentifier(QualifiedIdentifier),
    Reference(Reference),
    Pointer(Pointer),
    Tuple(Tuple),
    Local(Local),
    Array(Array),
    Phantom(Phantom),
    Elided(Elided),
}

impl SourceElement for Type {
    fn span(&self) -> Span {
        match self {
            Self::Primitive(primitive) => primitive.span(),
            Self::QualifiedIdentifier(qualified) => qualified.span(),
            Self::Local(local) => local.span(),
            Self::Reference(reference) => reference.span(),
            Self::Pointer(pointer) => pointer.span(),
            Self::Tuple(tuple) => tuple.span(),
            Self::Array(array) => array.span(),
            Self::Phantom(phantom) => phantom.span(),
            Self::Elided(elided) => elided.span(),
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
    fn parse_reference_type(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Reference> {
        match self.stop_at_significant() {
            Reading::Unit(Token::Punctuation(
                ampersand @ Punctuation { punctuation: '&', .. },
            )) => {
                // eat the ampersand
                self.forward();

                let lifetime = self.try_parse_lifetime(handler)?;
                let mutable = if let Reading::Unit(Token::Keyword(
                    mutable_keyowrd @ Keyword {
                        kind: KeywordKind::Mutable, ..
                    },
                )) = self.stop_at_significant()
                {
                    self.forward();
                    Some(mutable_keyowrd)
                } else {
                    None
                };

                let operand = Box::new(self.parse_type(handler)?);

                Some(Reference {
                    ampersand,
                    lifetime,
                    mutable_keyword: mutable,
                    operand,
                })
            }

            found => {
                handler.receive(Error {
                    expected: SyntaxKind::Punctuation('&'),
                    alternatives: Vec::new(),
                    found: self.reading_to_found(found),
                });

                // eat the current token / make progress
                self.forward();

                None
            }
        }
    }

    fn parse_pointer_type(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Pointer> {
        let asterisk = self.parse_punctuation('*', true, handler)?;

        let mutable_keyword = match self.stop_at_significant() {
            Reading::Unit(Token::Keyword(k))
                if k.kind == KeywordKind::Mutable =>
            {
                self.forward();
                Some(k)
            }

            _ => None,
        };

        let operand_type = Box::new(self.parse_type(handler)?);

        Some(Pointer { asterisk, mutable_keyword, operand: operand_type })
    }

    fn parse_array_type(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Array> {
        let delimited_tree = self.step_into(
            Delimiter::Bracket,
            |parser| {
                let ty = parser.parse_type(handler)?;
                let colon = parser.parse_punctuation(':', true, handler)?;
                let expression = match parser.stop_at_significant() {
                    Reading::Unit(Token::Punctuation(
                        first_dot @ Punctuation { punctuation: '.', .. },
                    )) => {
                        // eat the first dot
                        parser.forward();

                        let second_dot =
                            parser.parse_punctuation('.', false, handler)?;

                        Constant::Elided(Elided { first_dot, second_dot })
                    }

                    _ => parser
                        .parse_expression(handler)
                        .map(|x| Constant::Expression(Box::new(x)))?,
                };
                Some((ty, colon, expression))
            },
            handler,
        )?;

        let tree = delimited_tree.tree?;

        Some(Array {
            left_bracket: delimited_tree.open,
            operand: Box::new(tree.0),
            colon: tree.1,
            constant: tree.2,
            right_bracket: delimited_tree.close,
        })
    }

    fn parse_tuple_type(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Tuple> {
        let type_specifiers = self.parse_delimited_list(
            Delimiter::Parenthesis,
            ',',
            |parser| {
                // stop at significant token
                parser.stop_at_significant();

                let ellipsis = match (
                    parser.peek(),
                    parser.peek_offset(1),
                    parser.peek_offset(2),
                ) {
                    (
                        Reading::Unit(Token::Punctuation(p1)),
                        Some(Reading::Unit(Token::Punctuation(p2))),
                        Some(Reading::Unit(Token::Punctuation(p3))),
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

                Some(Unpackable { ellipsis, ty: Box::new(ty) })
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
    #[allow(clippy::missing_errors_doc, clippy::too_many_lines)]
    pub fn parse_type(&mut self, handler: &dyn Handler<Error>) -> Option<Type> {
        match self.stop_at_significant() {
            // parse elided type
            Reading::Unit(Token::Punctuation(
                first_dot @ Punctuation { punctuation: '.', .. },
            )) => {
                // eat the first dot
                self.forward();

                let second_dot = self.parse_punctuation('.', false, handler)?;

                Some(Type::Elided(Elided { first_dot, second_dot }))
            }

            // parse local type
            Reading::Unit(Token::Keyword(keyword))
                if keyword.kind == KeywordKind::Local =>
            {
                // eat local keyword
                self.forward();

                let identifier = self.parse_type(handler)?;

                Some(Type::Local(Local {
                    local_keyword: keyword,
                    ty: Box::new(identifier),
                }))
            }

            // parse qualified identifier
            Reading::Unit(
                Token::Identifier(..)
                | Token::Keyword(Keyword {
                    kind:
                        KeywordKind::This | KeywordKind::Super | KeywordKind::Target,
                    ..
                }),
            ) => Some(Type::QualifiedIdentifier(
                self.parse_qualified_identifier(handler)?,
            )),

            // parse pointer type
            Reading::Unit(Token::Punctuation(p)) if p.punctuation == '*' => {
                self.parse_pointer_type(handler).map(Type::Pointer)
            }

            // parse reference
            Reading::Unit(Token::Punctuation(Punctuation {
                punctuation: '&' | '@',
                ..
            })) => self.parse_reference_type(handler).map(Type::Reference),

            // parse array type
            Reading::IntoDelimited(Delimiter::Bracket, _) => {
                self.parse_array_type(handler).map(Type::Array)
            }

            // parse tuple type
            Reading::IntoDelimited(Delimiter::Parenthesis, _) => {
                self.parse_tuple_type(handler).map(Type::Tuple)
            }

            Reading::Unit(Token::Keyword(phantom_keyword))
                if phantom_keyword.kind == KeywordKind::Phantom =>
            {
                // eat phantom keyword
                self.forward();

                let ty = self.parse_type(handler)?;

                Some(Type::Phantom(Phantom {
                    phantom_keyword,
                    r#type: Box::new(ty),
                }))
            }

            // primitive type
            Reading::Unit(Token::Keyword(keyword))
                if matches!(
                    keyword.kind,
                    KeywordKind::Int8
                        | KeywordKind::Int16
                        | KeywordKind::Int32
                        | KeywordKind::Int64
                        | KeywordKind::Uint8
                        | KeywordKind::Uint16
                        | KeywordKind::Uint32
                        | KeywordKind::Uint64
                        | KeywordKind::Usize
                        | KeywordKind::Isize
                        | KeywordKind::Float32
                        | KeywordKind::Float64
                        | KeywordKind::Bool
                ) =>
            {
                // eat primitive type token
                self.forward();

                let primitive_type = match keyword.kind {
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
                    KeywordKind::Usize => Primitive::Usize(keyword),
                    KeywordKind::Isize => Primitive::Isize(keyword),
                    _ => unreachable!(),
                };

                Some(Type::Primitive(primitive_type))
            }

            found => {
                handler.receive(Error {
                    expected: SyntaxKind::TypeSpecifier,
                    alternatives: Vec::new(),
                    found: self.reading_to_found(found),
                });
                // eat the current token / make progress
                self.forward();

                None
            }
        }
    }
}

#[cfg(test)]
mod test;
