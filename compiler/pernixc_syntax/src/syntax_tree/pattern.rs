//! Contains all definition of pattern syntax trees.

use std::{fmt::Debug, option::Option};

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_lexical::{
    token::{self, Identifier, Keyword, KeywordKind, Punctuation},
    token_stream::Delimiter,
};

use super::{
    expression::Boolean, ConnectedList, EnclosedConnectedList, EnclosedTree,
    Parse, ParseExt, ReferenceOf, SyntaxTree,
};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, ExpectExt},
        StateMachine,
    },
};

// pub mod strategy;

/// Syntax Synopsis:
/// ``` txt
/// FieldAssociation:
///     Identifier ':' Pattern
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct FieldAssociation<Pattern> {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    pattern: Box<Pattern>,
}

impl<Pattern: SyntaxTree + 'static> SyntaxTree for FieldAssociation<Pattern> {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            expect::Identifier.to_owned(),
            ':'.to_owned(),
            Pattern::parse.map(Box::new),
        )
            .map(|(identifier, colon, pattern)| Self {
                identifier,
                colon,
                pattern,
            })
            .parse(state_machine, handler)
    }
}

impl<Pattern: SourceElement> SourceElement for FieldAssociation<Pattern> {
    fn span(&self) -> Span {
        self.identifier.span().join(&self.pattern().span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Field:
///     FieldAssociation
///     | Named
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Field<Pattern> {
    Association(FieldAssociation<Pattern>),
    Named(Named),
}

impl<Pattern: SyntaxTree + 'static> SyntaxTree for Field<Pattern> {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        FieldAssociation::parse
            .map(Self::Association)
            .or_else(Named::parse.map(Self::Named))
            .parse(state_machine, handler)
    }
}

impl<Pattern: SourceElement> SourceElement for Field<Pattern> {
    fn span(&self) -> Span {
        match self {
            Self::Association(field_with_association) => {
                field_with_association.span()
            }
            Self::Named(field_without_association) => {
                field_without_association.span()
            }
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Structural:
///     '{' (Field (',' Field)* ','?)? '..'? '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Structural<Pattern> {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    fields: Option<ConnectedList<Field<Pattern>, Punctuation>>,
    #[get = "pub"]
    wildcard: Option<Wildcard>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl<Pattern: SyntaxTree + 'static> SyntaxTree for Structural<Pattern> {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (Field::parse.connected_list(','.to_owned()), Wildcard::parse.or_none())
            .step_into(Delimiter::Brace)
            .map(|(open, (fields, wildcard), close)| Self {
                left_brace: open.clone(),
                fields,
                wildcard,
                right_brace: close.clone(),
            })
            .parse(state_machine, handler)
    }
}

impl<Pattern> SourceElement for Structural<Pattern> {
    fn span(&self) -> Span {
        self.left_brace.span.join(&self.right_brace.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// EnumAssociation:
///     '(' Pattern ')'
///     ;
/// ```
pub type EnumAssociation = EnclosedTree<Box<Refutable>>;

impl SyntaxTree for EnumAssociation {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Refutable::parse
            .map(Box::new)
            .enclosed_tree(Delimiter::Parenthesis)
            .parse(state_machine, handler)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Enum:
///     'case' Identifier EnumAssociation?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Enum {
    #[get = "pub"]
    case_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    association: Option<EnumAssociation>,
}

impl SyntaxTree for Enum {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Case.to_owned(),
            expect::Identifier.to_owned(),
            EnumAssociation::parse.or_none(),
        )
            .map(|(case_keyword, identifier, association)| Self {
                case_keyword,
                identifier,
                association,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Enum {
    fn span(&self) -> Span {
        self.association.as_ref().map_or_else(
            || self.case_keyword.span().join(&self.identifier.span()).unwrap(),
            |association| {
                self.case_keyword.span().join(&association.span()).unwrap()
            },
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Wildcard:
///     '..'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Wildcard(Punctuation, Punctuation);

impl SyntaxTree for Wildcard {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ('.'.to_owned(), '.'.no_skip().to_owned())
            .map(|(f, s)| Self(f, s))
            .parse(state_machine, handler)
    }
}

impl SourceElement for Wildcard {
    fn span(&self) -> Span { self.0.span.join(&self.1.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// TupleElement:
///     '...'? Pattern
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TupleElement<Pattern> {
    #[get = "pub"]
    ellipsis: Option<(Punctuation, Punctuation, Punctuation)>,
    #[get = "pub"]
    pattern: Box<Pattern>,
}

impl<Pattern: SyntaxTree + 'static> SyntaxTree for TupleElement<Pattern> {
    fn parse(
        parser: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            (
                '.'.to_owned(),
                '.'.no_skip().to_owned(),
                '.'.no_skip().to_owned(),
            )
                .or_none(),
            Pattern::parse.map(Box::new),
        )
            .map(|(ellipsis, pattern)| Self { ellipsis, pattern })
            .parse(parser, handler)
    }
}

impl<Pattern: SourceElement> SourceElement for TupleElement<Pattern> {
    fn span(&self) -> Span {
        self.ellipsis.as_ref().map_or_else(
            || self.pattern.span(),
            |(dots, _, _)| dots.span().join(&self.pattern.span()).unwrap(),
        )
    }
}

impl<Pattern> TupleElement<Pattern> {
    /// Dissolves the tuple element into its components.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (Option<(Punctuation, Punctuation, Punctuation)>, Box<Pattern>) {
        (self.ellipsis, self.pattern)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Tuple:
///     '(' (TupleElement (',' TupleElement)* ','?)? ')'
///     ;
/// ```
pub type Tuple<Pattern> =
    EnclosedConnectedList<TupleElement<Pattern>, Punctuation>;

impl<Pattern: SyntaxTree + 'static> SyntaxTree for Tuple<Pattern> {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        TupleElement::parse
            .enclosed_connected_list(','.to_owned(), Delimiter::Parenthesis)
            .parse(state_machine, handler)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Named:
///     'mutable'? Qualifier? Identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Named {
    #[get = "pub"]
    mutable_keyword: Option<Keyword>,
    #[get = "pub"]
    reference_of: Option<ReferenceOf>,
    #[get = "pub"]
    identifier: Identifier,
}

impl SyntaxTree for Named {
    fn parse(
        parser: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Mutable.to_owned().or_none(),
            ReferenceOf::parse.or_none(),
            expect::Identifier.to_owned(),
        )
            .map(|(mutable_keyword, reference_of, identifier)| Self {
                mutable_keyword,
                reference_of,
                identifier,
            })
            .parse(parser, handler)
    }
}

impl SourceElement for Named {
    fn span(&self) -> Span {
        self.mutable_keyword.as_ref().map_or_else(
            || {
                self.reference_of.as_ref().map_or_else(
                    || self.identifier.span(),
                    |qualifier| {
                        qualifier.span().join(&self.identifier.span()).unwrap()
                    },
                )
            },
            |mutable_keyword| {
                mutable_keyword.span().join(&self.identifier.span()).unwrap()
            },
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Integer:
///     '-'?
///     NumericToken
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Integer {
    #[get = "pub"]
    minus: Option<Punctuation>,
    #[get = "pub"]
    numeric: token::Numeric,
}

impl SyntaxTree for Integer {
    fn parse(
        parser: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ('-'.to_owned().or_none(), expect::Numeric.to_owned())
            .map(|(minus, numeric)| Self { minus, numeric })
            .parse(parser, handler)
    }
}

impl SourceElement for Integer {
    fn span(&self) -> Span {
        self.minus.as_ref().map_or_else(
            || self.numeric.span(),
            |minus| minus.span().join(&self.numeric.span()).unwrap(),
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Refutable:
///     Boolean
///     | Numeric
///     | Structural
///     | Enum
///     | Named
///     | Tuple
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
pub enum Refutable {
    Boolean(Boolean),
    Integer(Integer),
    Structural(Structural<Self>),
    Enum(Enum),
    Named(Named),
    Tuple(Tuple<Self>),
    Wildcard(Wildcard),
}

impl SyntaxTree for Refutable {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Boolean::parse
            .map(Self::Boolean)
            .or_else(Integer::parse.map(Self::Integer))
            .or_else(Structural::parse.map(Self::Structural))
            .or_else(Enum::parse.map(Self::Enum))
            .or_else(Named::parse.map(Self::Named))
            .or_else(Tuple::parse.map(Self::Tuple))
            .or_else(Wildcard::parse.map(Self::Wildcard))
            .parse(state_machine, handler)
    }
}

impl SourceElement for Refutable {
    fn span(&self) -> Span {
        match self {
            Self::Boolean(boolean_literal) => boolean_literal.span(),
            Self::Integer(numeric_literal) => numeric_literal.span(),
            Self::Structural(structural) => structural.span(),
            Self::Enum(associated_enum) => associated_enum.span(),
            Self::Named(identifier) => identifier.span(),
            Self::Tuple(tuple_pattern) => tuple_pattern.span(),
            Self::Wildcard(wildcard) => wildcard.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Irrefutable:
///     Structural
///     | Enum
///     | Named
///     | Tuple
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
pub enum Irrefutable {
    Structural(Structural<Self>),
    Named(Named),
    Tuple(Tuple<Self>),
    Wildcard(Wildcard),
}

impl SyntaxTree for Irrefutable {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Structural::parse
            .map(Self::Structural)
            .or_else(Named::parse.map(Self::Named))
            .or_else(Tuple::parse.map(Self::Tuple))
            .or_else(Wildcard::parse.map(Self::Wildcard))
            .parse(state_machine, handler)
    }
}
impl Irrefutable {
    /// Returns `true` if the pattern contains a named pattern.
    pub fn contains_named(&self) -> bool {
        match self {
            Self::Structural(structural) => {
                structural.fields.iter().flat_map(ConnectedList::elements).any(
                    |x| match x {
                        Field::Association(pattern) => {
                            pattern.pattern.contains_named()
                        }
                        Field::Named(_) => true,
                    },
                )
            }
            Self::Named(_) => true,
            Self::Tuple(tuple) => tuple
                .connected_list
                .as_ref()
                .into_iter()
                .flat_map(ConnectedList::elements)
                .any(|x| x.pattern.contains_named()),
            Self::Wildcard(_) => false,
        }
    }
}

impl SourceElement for Irrefutable {
    fn span(&self) -> Span {
        match self {
            Self::Structural(structural) => structural.span(),
            Self::Named(identifier) => identifier.span(),
            Self::Tuple(tuple_pattern) => tuple_pattern.span(),
            Self::Wildcard(wildcard) => wildcard.span(),
        }
    }
}

#[cfg(test)]
mod test;
