//! Contains all definition of type syntax trees.

#![allow(missing_docs)]

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::source_file::{SourceElement, Span};
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation},
    token_stream::Delimiter,
};

use super::{
    delimited_list, Constant, DelimitedList, Elided, Lifetime, Parse,
    QualifiedIdentifier,
};
use crate::parser::{expect::Expect, StepIntoTree, Syntax};

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

impl Parse for Primitive {
    fn syntax() -> impl Syntax<Output = Self> {
        KeywordKind::Bool
            .map(Primitive::Bool)
            .or_else(KeywordKind::Float32.map(Primitive::Float32))
            .or_else(KeywordKind::Float64.map(Primitive::Float64))
            .or_else(KeywordKind::Int8.map(Primitive::Int8))
            .or_else(KeywordKind::Int16.map(Primitive::Int16))
            .or_else(KeywordKind::Int32.map(Primitive::Int32))
            .or_else(KeywordKind::Int64.map(Primitive::Int64))
            .or_else(KeywordKind::Uint8.map(Primitive::Uint8))
            .or_else(KeywordKind::Uint16.map(Primitive::Uint16))
            .or_else(KeywordKind::Uint32.map(Primitive::Uint32))
            .or_else(KeywordKind::Uint64.map(Primitive::Uint64))
            .or_else(KeywordKind::Usize.map(Primitive::Usize))
            .or_else(KeywordKind::Isize.map(Primitive::Isize))
    }
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

impl Parse for Reference {
    fn syntax() -> impl Syntax<Output = Self> {
        ('&', Lifetime::syntax().or_none(), KeywordKind::Mutable.or_none())
            .then_do(
                |parser, (ampersand, lifetime, mutable_keyword), handler| {
                    Ok(Reference {
                        ampersand,
                        lifetime,
                        mutable_keyword,
                        operand: parser.parse_syntax_tree(handler)?,
                    })
                },
            )
    }
}

impl SourceElement for Reference {
    fn span(&self) -> Span {
        self.ampersand.span().join(&self.operand.span()).unwrap()
    }
}

impl Reference {
    /// Destructs the `Reference` into its components.
    #[must_use]
    pub fn destruct(
        self,
    ) -> (Punctuation, Option<Lifetime>, Option<Keyword>, Box<Type>) {
        (self.ampersand, self.lifetime, self.mutable_keyword, self.operand)
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
    pub(super) r#type: Box<Type>,
}

impl Parse for Unpackable {
    fn syntax() -> impl Syntax<Output = Self> {
        (
            '.'.then_do(|parser, f, handler| {
                Ok((
                    f,
                    parser.parse_dont_skip('.', handler)?,
                    parser.parse_dont_skip('.', handler)?,
                ))
            })
            .or_none(),
            Type::syntax().map(Box::new),
        )
            .map(|(ellipsis, r#type)| Self { ellipsis, r#type })
    }
}

impl SourceElement for Unpackable {
    fn span(&self) -> Span {
        match &self.ellipsis {
            Some((left, ..)) => left.span.join(&self.r#type.span()).unwrap(),
            None => self.r#type.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Tuple:
///     '(' UnpackableList? ')'
///     ;
/// ```
pub type Tuple = DelimitedList<Unpackable>;

impl Parse for Tuple {
    fn syntax() -> impl Syntax<Output = Self> {
        delimited_list(Delimiter::Parenthesis, ',')
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

impl Parse for Array {
    fn syntax() -> impl Syntax<Output = Self> {
        '['.verify_then_do(|parser, handler| {
            let StepIntoTree { open, tree, close } = parser.step_into(
                Delimiter::Bracket,
                |parser| {
                    Ok((
                        parser.parse_syntax_tree(handler)?,
                        parser.parse(':', handler)?,
                        parser.parse_syntax_tree(handler)?,
                    ))
                },
                handler,
            )?;

            let tree = tree?;

            Ok(Array {
                left_bracket: open,
                operand: tree.0,
                colon: tree.1,
                constant: tree.2,
                right_bracket: close,
            })
        })
    }
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

impl Parse for Pointer {
    fn syntax() -> impl Syntax<Output = Self> {
        '*'.then_do(|parser, asterisk, handler| {
            Ok(Pointer {
                asterisk,
                mutable_keyword: parser
                    .parse(KeywordKind::Mutable.or_none(), handler)?,
                operand: parser.parse_syntax_tree(handler)?,
            })
        })
    }
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

impl Parse for Phantom {
    fn syntax() -> impl Syntax<Output = Self> {
        KeywordKind::Phantom.then_do(|parser, phantom_keyword, handler| {
            Ok(Phantom {
                phantom_keyword,
                r#type: parser.parse_syntax_tree(handler)?,
            })
        })
    }
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
    r#type: Box<Type>,
}

impl Parse for Local {
    fn syntax() -> impl Syntax<Output = Self> {
        KeywordKind::Local.then_do(|parser, local_keyword, handler| {
            Ok(Local {
                local_keyword,
                r#type: parser.parse_syntax_tree(handler)?,
            })
        })
    }
}

impl SourceElement for Local {
    fn span(&self) -> Span {
        self.local_keyword.span.join(&self.r#type.span()).unwrap()
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

impl Parse for Type {
    fn syntax() -> impl Syntax<Output = Self> {
        Primitive::syntax()
            .map(Type::Primitive)
            .or_else(
                QualifiedIdentifier::syntax().map(Type::QualifiedIdentifier),
            )
            .or_else(Reference::syntax().map(Type::Reference))
            .or_else(Pointer::syntax().map(Type::Pointer))
            .or_else(Tuple::syntax().map(Type::Tuple))
            .or_else(Local::syntax().map(Type::Local))
            .or_else(Array::syntax().map(Type::Array))
            .or_else(Phantom::syntax().map(Type::Phantom))
            .or_else(Elided::syntax().map(Type::Elided))
    }
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

#[cfg(test)]
mod test;
