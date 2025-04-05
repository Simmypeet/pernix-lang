use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{self, Character, Identifier, Keyword, KeywordKind, Punctuation},
    token_stream::DelimiterKind,
};
use pernixc_source_file::{SourceElement, GlobalSpan};

use super::Expression;
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Branch, ExpectExt, Parse},
        StateMachine,
    },
    syntax_tree::{
        EnclosedConnectedList, ParseExt, QualifiedIdentifier, SyntaxTree,
    },
};

pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Unit {
    Boolean(Boolean),
    Numeric(Numeric),
    QualifiedIdentifier(QualifiedIdentifier),
    Parenthesized(Parenthesized),
    Struct(Struct),
    Array(Array),
    Phantom(Phantom),
    String(token::String),
    Character(Character),
    Panic(Panic),
}

impl SyntaxTree for Unit {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Struct::parse.map(Self::Struct),
            QualifiedIdentifier::parse.map(Self::QualifiedIdentifier),
            Array::parse.map(Self::Array),
            Phantom::parse.map(Self::Phantom),
            expect::String.to_owned().map(Self::String),
            expect::Character.to_owned().map(Self::Character),
            Numeric::parse.map(Self::Numeric),
            Boolean::parse.map(Self::Boolean),
            Parenthesized::parse.map(Self::Parenthesized),
            Panic::parse.map(Self::Panic),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Unit {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Boolean(unit) => unit.span(),
            Self::Numeric(unit) => unit.span(),
            Self::QualifiedIdentifier(unit) => unit.span(),
            Self::Parenthesized(unit) => unit.span(),
            Self::Struct(unit) => unit.span(),
            Self::Array(unit) => unit.span(),
            Self::Phantom(unit) => unit.span(),
            Self::String(unit) => unit.span(),
            Self::Character(unit) => unit.span(),
            Self::Panic(unit) => unit.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Boolean {
    True(Keyword),
    False(Keyword),
}

impl SyntaxTree for Boolean {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::True.to_owned().map(Self::True),
            KeywordKind::False.to_owned().map(Self::False),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Boolean {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::True(keyword) | Self::False(keyword) => keyword.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Decimal {
    pub dot: Punctuation,
    pub numeric: token::Numeric,
}

impl SyntaxTree for Decimal {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ('.'.to_owned(), expect::Numeric.no_skip().to_owned())
            .map(|(dot, numeric)| Self { dot, numeric })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Decimal {
    fn span(&self) -> GlobalSpan { self.dot.span().join(&self.numeric.span) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric {
    pub numeric: token::Numeric,
    pub decimal: Option<Decimal>,
    pub suffix: Option<Identifier>,
}

impl SyntaxTree for Numeric {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            expect::Numeric.to_owned(),
            ('.'.no_skip().to_owned(), expect::Numeric.no_skip().to_owned())
                .map(|(dot, numeric)| Decimal { dot, numeric })
                .commit_in(2)
                .or_none(),
            expect::Identifier.no_skip().to_owned().or_none(),
        )
            .map(|(numeric, decimal, suffix)| Self { numeric, decimal, suffix })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Numeric {
    fn span(&self) -> GlobalSpan {
        let end = self.suffix.as_ref().map_or_else(
            || {
                self.decimal.as_ref().map_or_else(
                    || self.numeric.span.clone(),
                    SourceElement::span,
                )
            },
            SourceElement::span,
        );

        self.numeric.span().join(&end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unpackable {
    pub ellipsis: Option<(Punctuation, Punctuation, Punctuation)>,
    pub expression: Box<Expression>,
}

impl SyntaxTree for Unpackable {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            (
                '.'.to_owned(),
                '.'.no_skip().to_owned(),
                '.'.no_skip().to_owned(),
            )
                .or_none(),
            Expression::parse.map(Box::new),
        )
            .map(|(ellipsis, expression)| Self { ellipsis, expression })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Unpackable {
    fn span(&self) -> GlobalSpan {
        match &self.ellipsis {
            Some((start, ..)) => start.span().join(&self.expression.span()),
            None => self.expression.span(),
        }
    }
}

pub type Parenthesized = EnclosedConnectedList<Unpackable, Punctuation>;

impl SyntaxTree for Parenthesized {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Unpackable::parse
            .enclosed_connected_list(','.to_owned(), DelimiterKind::Parenthesis)
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldInitializer {
    pub identifier: Identifier,
    pub colon: Punctuation,
    pub expression: Box<Expression>,
}

impl SyntaxTree for FieldInitializer {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            expect::Identifier.to_owned(),
            ':'.to_owned(),
            Expression::parse.map(Box::new),
        )
            .map(|(identifier, colon, expression)| Self {
                identifier,
                colon,
                expression,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for FieldInitializer {
    fn span(&self) -> GlobalSpan {
        self.identifier.span().join(&self.expression.span())
    }
}

pub type FieldInitializers =
    EnclosedConnectedList<FieldInitializer, Punctuation>;

impl SyntaxTree for FieldInitializers {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        FieldInitializer::parse
            .enclosed_connected_list(','.to_owned(), DelimiterKind::Brace)
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub qualified_identifier: QualifiedIdentifier,
    pub field_initializers: FieldInitializers,
}

impl SyntaxTree for Struct {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (QualifiedIdentifier::parse, FieldInitializers::parse)
            .map(|(qualified_identifier, field_initializers)| Self {
                qualified_identifier,
                field_initializers,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Struct {
    fn span(&self) -> GlobalSpan {
        self.qualified_identifier.span().join(&self.field_initializers.span())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array {
    pub arguments: EnclosedConnectedList<Box<Expression>, Punctuation>,
}

impl SyntaxTree for Array {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Expression::parse
            .map(Box::new)
            .enclosed_connected_list(','.to_owned(), DelimiterKind::Bracket)
            .map(|arguments| Self { arguments })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Array {
    fn span(&self) -> GlobalSpan { self.arguments.span() }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Phantom {
    pub phantom_keyword: Keyword,
}

impl SyntaxTree for Phantom {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        KeywordKind::Phantom
            .to_owned()
            .map(|phantom_keyword| Self { phantom_keyword })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Phantom {
    fn span(&self) -> GlobalSpan { self.phantom_keyword.span() }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Panic {
    pub panic_keyword: Keyword,
}

impl SyntaxTree for Panic {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        KeywordKind::Panic
            .to_owned()
            .map(|panic_keyword| Self { panic_keyword })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Panic {
    fn span(&self) -> GlobalSpan { self.panic_keyword.span() }
}
