//! Contains all definition of expression syntax trees.

#![allow(missing_docs)]

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_lexical::{
    token::{self, Character, Identifier, Keyword, KeywordKind, Punctuation},
    token_stream::Delimiter,
};

use super::{
    pattern::Refutable, r#type::Type, statement::Statements,
    EnclosedConnectedList, EnclosedTree, GenericIdentifier, Label, Parse,
    ParseExt, QualifiedIdentifier, ReferenceOf, SyntaxTree,
};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Branch, ExpectExt},
        StateMachine,
    },
};

pub mod strategy;

/// Syntax Synopsis:
///
/// ``` txt
/// Expression:
///     Binary
///     | Terminator
///     | Brace
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Expression {
    Binary(Binary),
    Terminator(Terminator),
}

impl SyntaxTree for Expression {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Binary::parse.map(Self::Binary),
            Terminator::parse.map(Self::Terminator),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Expression {
    fn span(&self) -> Span {
        match self {
            Self::Binary(syn) => syn.span(),
            Self::Terminator(syn) => syn.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Brace:
///     Block
///     | IfElse
///     | Loop
///     | Match
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Brace {
    Block(Block),
    IfElse(IfElse),
    Loop(Loop),
    Match(Match),
    While(While),
}

impl SyntaxTree for Brace {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Block::parse.map(Self::Block),
            IfElse::parse.map(Self::IfElse),
            Loop::parse.map(Self::Loop),
            Match::parse.map(Self::Match),
            While::parse.map(Self::While),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Brace {
    fn span(&self) -> Span {
        match self {
            Self::Block(syn) => syn.span(),
            Self::IfElse(syn) => syn.span(),
            Self::Loop(syn) => syn.span(),
            Self::Match(syn) => syn.span(),
            Self::While(syn) => syn.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// LabelSpecifier:
///     Label ':'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct LabelSpecifier {
    #[get = "pub"]
    label: Label,
    #[get = "pub"]
    colon: Punctuation,
}

impl SyntaxTree for LabelSpecifier {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (Label::parse, ':'.to_owned())
            .map(|(label, colon)| Self { label, colon })
            .parse(state_machine, handler)
    }
}

impl SourceElement for LabelSpecifier {
    fn span(&self) -> Span { self.label.span().join(&self.colon.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// MatchArm:
///     RefutablePattern ':' Block
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct MatchArm {
    #[get = "pub"]
    refutable_pattern: Refutable,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    expression: Box<Expression>,
}

impl SyntaxTree for MatchArm {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (Refutable::parse, ':'.to_owned(), Expression::parse.map(Box::new))
            .map(|(refutable_pattern, colon, expression)| Self {
                refutable_pattern,
                colon,
                expression,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for MatchArm {
    fn span(&self) -> Span {
        self.refutable_pattern.span().join(&self.expression.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// MatchArms:
///     '{' (MatchArm (',' MatchArm)* ','? )? '}'
///     ;
/// ```
pub type MatchArms = EnclosedConnectedList<MatchArm, Punctuation>;

impl SyntaxTree for MatchArms {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        MatchArm::parse
            .enclosed_connected_list(','.to_owned(), Delimiter::Brace)
            .parse(state_machine, handler)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Match:
///     'match' Parenthesized MatchArms
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Match {
    #[get = "pub"]
    match_keyword: Keyword,
    #[get = "pub"]
    parenthesized: Parenthesized,
    #[get = "pub"]
    arms: MatchArms,
}

impl SyntaxTree for Match {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Match.to_owned(), Parenthesized::parse, MatchArms::parse)
            .map(|(match_keyword, parenthesized, arms)| Self {
                match_keyword,
                parenthesized,
                arms,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Match {
    fn span(&self) -> Span {
        self.match_keyword.span.join(&self.arms.span()).unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Block:
///     LabelSpecifier? 'unsafe'? Statements
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Block {
    #[get = "pub"]
    label_specifier: Option<LabelSpecifier>,
    #[get = "pub"]
    unsafe_keyword: Option<Keyword>,
    #[get = "pub"]
    statements: Statements,
}

impl SyntaxTree for Block {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            LabelSpecifier::parse.or_none(),
            KeywordKind::Unsafe.to_owned().or_none(),
            Statements::parse,
        )
            .map(|(label_specifier, unsafe_keyword, statements)| Self {
                label_specifier,
                unsafe_keyword,
                statements,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Block {
    fn span(&self) -> Span {
        let start = self.label_specifier.as_ref().map_or_else(
            || {
                self.unsafe_keyword
                    .as_ref()
                    .map_or_else(|| self.statements.span(), SourceElement::span)
            },
            SourceElement::span,
        );
        let end = self.statements.span();
        start.join(&end).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// BlockOrIfElse:
///     Block
///     | IfElse
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs, clippy::large_enum_variant /*false positive*/)]
pub enum BlockOrIfElse {
    Block(Block),
    IfElse(IfElse),
}

impl SyntaxTree for BlockOrIfElse {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (Block::parse.map(Self::Block), IfElse::parse.map(Self::IfElse))
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for BlockOrIfElse {
    fn span(&self) -> Span {
        match self {
            Self::Block(block) => block.span(),
            Self::IfElse(if_else) => if_else.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Else:
///     'else' BlockOrIfElse
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Else {
    #[get = "pub"]
    else_keyword: Keyword,
    #[get = "pub"]
    expression: Box<BlockOrIfElse>,
}

impl SyntaxTree for Else {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Else.to_owned(), BlockOrIfElse::parse.map(Box::new))
            .map(|(else_keyword, expression)| Self { else_keyword, expression })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Else {
    fn span(&self) -> Span {
        self.else_keyword.span().join(&self.expression.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// IfElse:
///     'if' Parenthesized Block Else?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct IfElse {
    #[get = "pub"]
    if_keyword: Keyword,
    #[get = "pub"]
    parenthesized: Parenthesized,
    #[get = "pub"]
    then_expression: Block,
    #[get = "pub"]
    else_expression: Option<Else>,
}

impl SyntaxTree for IfElse {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::If.to_owned(),
            Parenthesized::parse,
            Block::parse,
            Else::parse.or_none(),
        )
            .map(
                |(
                    if_keyword,
                    parenthesized,
                    then_expression,
                    else_expression,
                )| {
                    Self {
                        if_keyword,
                        parenthesized,
                        then_expression,
                        else_expression,
                    }
                },
            )
            .parse(state_machine, handler)
    }
}

impl SourceElement for IfElse {
    fn span(&self) -> Span {
        let start = self.if_keyword.span();
        let end = self
            .else_expression
            .as_ref()
            .map_or(self.then_expression.span(), |else_expression| {
                else_expression.span()
            });

        start.join(&end).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// While:
///     'while' Parenthesized Block
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct While {
    #[get = "pub"]
    while_keyword: Keyword,
    #[get = "pub"]
    parenthesized: Parenthesized,
    #[get = "pub"]
    block: Block,
}

impl SyntaxTree for While {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::While.to_owned(), Parenthesized::parse, Block::parse)
            .map(|(while_keyword, parenthesized, block)| Self {
                while_keyword,
                parenthesized,
                block,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for While {
    fn span(&self) -> Span {
        self.while_keyword.span().join(&self.block.span()).unwrap()
    }
}

impl While {
    /// Destructs the while into its components
    #[must_use]
    pub fn destruct(self) -> (Keyword, Parenthesized, Block) {
        (self.while_keyword, self.parenthesized, self.block)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Loop:
///     'loop' Block
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Loop {
    #[get = "pub"]
    loop_keyword: Keyword,
    #[get = "pub"]
    block: Block,
}

impl SyntaxTree for Loop {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Loop.to_owned(), Block::parse)
            .map(|(loop_keyword, block)| Self { loop_keyword, block })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Loop {
    fn span(&self) -> Span {
        self.loop_keyword.span.join(&self.block.span()).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TerminatorTarget(Option<Label>, Option<Binary>);

impl SyntaxTree for TerminatorTarget {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            (
                LabelSpecifier::parse,
                KeywordKind::Unsafe.to_owned().or_none(),
                Statements::parse,
                (BinaryOperator::parse, BinaryNode::parse).keep_take(),
            )
                .map(
                    |(label_specifier, unsafe_keyword, statements, chain)| {
                        TerminatorTarget(
                            None,
                            Some(Binary {
                                first: BinaryNode::Brace(Brace::Block(Block {
                                    label_specifier: Some(label_specifier),
                                    unsafe_keyword,
                                    statements,
                                })),
                                chain,
                            }),
                        )
                    },
                ),
            (Label::parse.or_none(), Binary::parse.or_none())
                .map(|(label, binary)| TerminatorTarget(label, binary)),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Terminator:
///     Return
///     | Continue
///     | Express
///     | Break
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Terminator {
    Return(Return),
    Continue(Continue),
    Express(Express),
    Break(Break),
}

impl SyntaxTree for Terminator {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Return::parse.map(Terminator::Return),
            Continue::parse.map(Terminator::Continue),
            Express::parse.map(Terminator::Express),
            Break::parse.map(Terminator::Break),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Terminator {
    fn span(&self) -> Span {
        match self {
            Self::Return(syn) => syn.span(),
            Self::Continue(syn) => syn.span(),
            Self::Express(syn) => syn.span(),
            Self::Break(syn) => syn.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ```txt
/// Return:
///     'return' Binary?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Return {
    #[get = "pub"]
    return_keyword: Keyword,
    #[get = "pub"]
    binary: Option<Binary>,
}

impl SyntaxTree for Return {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Return.to_owned(), Binary::parse.or_none())
            .map(|(return_keyword, binary)| Self { return_keyword, binary })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Return {
    fn span(&self) -> Span {
        self.binary.as_ref().map_or_else(
            || self.return_keyword.span(),
            |expression| {
                self.return_keyword.span().join(&expression.span()).unwrap()
            },
        )
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Continue:
///     'continue' Label?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Continue {
    #[get = "pub"]
    continue_keyword: Keyword,
    #[get = "pub"]
    label: Option<Label>,
}

impl SyntaxTree for Continue {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Continue.to_owned(), Label::parse.or_none())
            .map(|(continue_keyword, label)| Self { continue_keyword, label })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Continue {
    fn span(&self) -> Span {
        self.label.as_ref().map_or_else(
            || self.continue_keyword.span(),
            |label| self.continue_keyword.span().join(&label.span()).unwrap(),
        )
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Express:
///     'express' Label? Binary?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Express {
    #[get = "pub"]
    express_keyword: Keyword,
    #[get = "pub"]
    label: Option<Label>,
    #[get = "pub"]
    binary: Option<Binary>,
}

impl SyntaxTree for Express {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Express.to_owned(), TerminatorTarget::parse)
            .map(|(express_keyword, TerminatorTarget(label, binary))| Self {
                express_keyword,
                label,
                binary,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Express {
    fn span(&self) -> Span {
        self.binary.as_ref().map_or_else(
            || {
                self.label.as_ref().map_or_else(
                    || self.express_keyword.span(),
                    |label| {
                        self.express_keyword.span().join(&label.span()).unwrap()
                    },
                )
            },
            |expression| {
                self.express_keyword.span().join(&expression.span()).unwrap()
            },
        )
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Break:
///     'break' Label? Binary?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Break {
    #[get = "pub"]
    break_keyword: Keyword,
    #[get = "pub"]
    label: Option<Label>,
    #[get = "pub"]
    binary: Option<Binary>,
}

impl SyntaxTree for Break {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Break.to_owned(), TerminatorTarget::parse)
            .map(|(break_keyword, TerminatorTarget(label, binary))| Self {
                break_keyword,
                label,
                binary,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Break {
    fn span(&self) -> Span {
        self.binary.as_ref().map_or_else(
            || {
                self.label.as_ref().map_or_else(
                    || self.break_keyword.span(),
                    |label| {
                        self.break_keyword.span().join(&label.span()).unwrap()
                    },
                )
            },
            |expression| {
                self.break_keyword.span().join(&expression.span()).unwrap()
            },
        )
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// BooleanLiteral:
///     'true'
///     | 'false'
///     ;
/// ```
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
    fn span(&self) -> Span {
        match self {
            Self::True(keyword) | Self::False(keyword) => keyword.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Decimal:
///     '.' NumericToken
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Decimal {
    #[get = "pub"]
    pub(super) dot: Punctuation,
    #[get = "pub"]
    pub(super) numeric: token::Numeric,
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
    fn span(&self) -> Span { self.dot.span().join(&self.numeric.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// Numeric:
///     NumericToken Decimal? Identifier?
///     ;
/// ````
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Numeric {
    #[get = "pub"]
    pub(super) numeric: token::Numeric,
    #[get = "pub"]
    pub(super) decimal: Option<Decimal>,
    #[get = "pub"]
    pub(super) suffix: Option<Identifier>,
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
    fn span(&self) -> Span {
        let end = self.suffix.as_ref().map_or_else(
            || {
                self.decimal.as_ref().map_or_else(
                    || self.numeric.span.clone(),
                    SourceElement::span,
                )
            },
            SourceElement::span,
        );

        self.numeric.span().join(&end).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Unpackable:
///     '...'? Expression
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Unpackable {
    #[get = "pub"]
    ellipsis: Option<(Punctuation, Punctuation, Punctuation)>,
    #[get = "pub"]
    expression: Box<Expression>,
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
    fn span(&self) -> Span {
        match &self.ellipsis {
            Some((start, ..)) => {
                start.span().join(&self.expression.span()).unwrap()
            }
            None => self.expression.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Parenthesized:
///     '(' (Unpackable (',' Unpackable)* ','? )? ')'
///     ;
/// ```
#[allow(missing_docs)]
pub type Parenthesized = EnclosedConnectedList<Unpackable, Punctuation>;

impl SyntaxTree for Parenthesized {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Unpackable::parse
            .enclosed_connected_list(','.to_owned(), Delimiter::Parenthesis)
            .parse(state_machine, handler)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// FieldInitializer:
///     Identifier ':' Expression
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct FieldInitializer {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    expression: Box<Expression>,
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
    fn span(&self) -> Span {
        self.identifier.span().join(&self.expression.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// FieldInitializers:
///     '{' FieldInitializer (',' FieldInitializer)* '}'
///     ;
/// ```
pub type FieldInitializers =
    EnclosedConnectedList<FieldInitializer, Punctuation>;

impl SyntaxTree for FieldInitializers {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        FieldInitializer::parse
            .enclosed_connected_list(','.to_owned(), Delimiter::Brace)
            .parse(state_machine, handler)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// StructLiteral:
///     QualifiedIdentifier FieldInitializers
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Struct {
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
    #[get = "pub"]
    field_initializers: FieldInitializers,
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
    fn span(&self) -> Span {
        self.qualified_identifier
            .span()
            .join(&self.field_initializers().span())
            .unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Array:
///     '[' (Expression (',' Expression)* ','? )? ']'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Array {
    #[get = "pub"]
    arguments: EnclosedConnectedList<Box<Expression>, Punctuation>,
}

impl SyntaxTree for Array {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Expression::parse
            .map(Box::new)
            .enclosed_connected_list(','.to_owned(), Delimiter::Bracket)
            .map(|arguments| Self { arguments })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Array {
    fn span(&self) -> Span { self.arguments.span() }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Phantom:
///     'phantom'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Phantom {
    #[get = "pub"]
    phantom_keyword: Keyword,
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
    fn span(&self) -> Span { self.phantom_keyword.span() }
}

/// Syntax Synopsis:
/// ``` txt
/// Panic:
///     "panic"
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Panic {
    #[get = "pub"]
    panic_keyword: Keyword,
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
    fn span(&self) -> Span { self.panic_keyword.span() }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Unit:
///     Boolean
///     | Numeric
///     | QualifiedIdentifier
///     | Parenthesized
///     | Struct
///     | Array
///     | Phantom
///     | String
///     | Character
///     | "panic"
///    ;
/// ```
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
    fn span(&self) -> Span {
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

/// Syntax Synopsis:
///
/// ``` txt
/// Call:
///     '(' ArgumentList? ')'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Call {
    #[get = "pub"]
    arguments: EnclosedConnectedList<Box<Expression>, Punctuation>,
}

impl SyntaxTree for Call {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Expression::parse
            .map(Box::new)
            .enclosed_connected_list(','.to_owned(), Delimiter::Parenthesis)
            .map(|arguments| Self { arguments })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Call {
    fn span(&self) -> Span { self.arguments.span() }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Cast:
///     'as' Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Cast {
    #[get = "pub"]
    as_keyword: Keyword,
    #[get = "pub"]
    r#type: Type,
}

impl SyntaxTree for Cast {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::As.to_owned(), Type::parse)
            .map(|(as_keyword, r#type)| Self { as_keyword, r#type })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Cast {
    fn span(&self) -> Span {
        self.as_keyword.span().join(&self.r#type.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// AccessOperator:
///     '.'
///     | '->'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum AccessOperator {
    Dot(Punctuation),
    Arrow(Punctuation, Punctuation),
}

impl SyntaxTree for AccessOperator {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            '.'.to_owned().map(Self::Dot),
            ('-'.to_owned(), '>'.no_skip().to_owned())
                .map(|(start, end)| Self::Arrow(start, end))
                .commit_in(2),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for AccessOperator {
    fn span(&self) -> Span {
        match self {
            Self::Dot(punctuation) => punctuation.span(),
            Self::Arrow(start, end) => start.span().join(&end.span).unwrap(),
        }
    }
}

/// Syntax Synopsis:
/// ```txt
/// Index:
///     '[' Expression ']'
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Index {
    #[get = "pub"]
    expression: EnclosedTree<Box<Expression>>,
}

impl SyntaxTree for Index {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Expression::parse
            .map(Box::new)
            .enclosed_tree(Delimiter::Bracket)
            .map(|expression| Self { expression })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Index {
    fn span(&self) -> Span { self.expression.span() }
}

/// Syntax Synopsis:
/// ``` txt
/// TupleIndex:
///     '-'? Numeric
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TupleIndex {
    #[get = "pub"]
    minus: Option<Punctuation>,
    #[get = "pub"]
    index: token::Numeric,
}

impl SyntaxTree for TupleIndex {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ('-'.to_owned().or_none(), expect::Numeric.to_owned())
            .map(|(minus, index)| Self { minus, index })
            .parse(state_machine, handler)
    }
}

impl SourceElement for TupleIndex {
    fn span(&self) -> Span {
        self.minus.as_ref().map_or_else(
            || self.index.span(),
            |minus| minus.span.join(&self.index.span).unwrap(),
        )
    }
}

impl TupleIndex {
    /// Destructs the tuple index into its components
    #[must_use]
    pub fn destruct(self) -> (Option<Punctuation>, token::Numeric) {
        (self.minus, self.index)
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// AccessKind:
///     Identifier
///     | Numeric
///     | Index
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum AccessKind {
    GenericIdentifier(GenericIdentifier),
    Tuple(TupleIndex),
    Index(Index),
}

impl SyntaxTree for AccessKind {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            GenericIdentifier::parse.map(Self::GenericIdentifier),
            TupleIndex::parse.map(Self::Tuple),
            Index::parse.map(Self::Index),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for AccessKind {
    fn span(&self) -> Span {
        match self {
            Self::GenericIdentifier(identifier) => identifier.span(),
            Self::Tuple(index) => index.span(),
            Self::Index(index) => index.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Access:
///    AccessOperator AccessKind
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Access {
    #[get = "pub"]
    operator: AccessOperator,
    #[get = "pub"]
    kind: AccessKind,
}

impl SyntaxTree for Access {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (AccessOperator::parse, AccessKind::parse)
            .map(|(operator, kind)| Self { operator, kind })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Access {
    fn span(&self) -> Span {
        self.operator.span().join(&self.kind.span()).unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// PostfixOperator:
///     Call
///     | Cast
///     | Access
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum PostfixOperator {
    Call(Call),
    Cast(Cast),
    Access(Access),
}

impl SyntaxTree for PostfixOperator {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Call::parse.map(Self::Call),
            Cast::parse.map(Self::Cast),
            Access::parse.map(Self::Access),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for PostfixOperator {
    fn span(&self) -> Span {
        match self {
            Self::Call(operator) => operator.span(),
            Self::Cast(operator) => operator.span(),
            Self::Access(operator) => operator.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Postfix:
///     Postfixable PostfixOperator
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Postfix {
    #[get = "pub"]
    postfixable: Box<Postfixable>,
    #[get = "pub"]
    operator: PostfixOperator,
}

impl SourceElement for Postfix {
    fn span(&self) -> Span {
        self.postfixable.span().join(&self.operator.span()).unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Postfix:
///     Unit
///     | Postfix
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Postfixable {
    Unit(Unit),
    Postfix(Postfix),
}

impl SyntaxTree for Postfixable {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Unit::parse
            .map(Self::Unit)
            .keep_fold(PostfixOperator::parse, |operand, operator| {
                Self::Postfix(Postfix {
                    postfixable: Box::new(operand),
                    operator,
                })
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Postfixable {
    fn span(&self) -> Span {
        match self {
            Self::Unit(unit) => unit.span(),
            Self::Postfix(postfix) => postfix.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// PrefixOperator:
///     '!'
///     | '-'
///     | '~'
///     | '*'
///     | ReferenceOf
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrefixOperator {
    LogicalNot(Punctuation),
    Negate(Punctuation),
    BitwiseNot(Punctuation),
    Dereference(Punctuation),
    ReferenceOf(ReferenceOf),
}

impl SyntaxTree for PrefixOperator {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            '!'.to_owned().map(Self::LogicalNot),
            '-'.to_owned().map(Self::Negate),
            '~'.to_owned().map(Self::BitwiseNot),
            '*'.to_owned().map(Self::Dereference),
            ReferenceOf::parse.map(Self::ReferenceOf),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for PrefixOperator {
    fn span(&self) -> Span {
        match self {
            Self::Negate(p)
            | Self::BitwiseNot(p)
            | Self::Dereference(p)
            | Self::LogicalNot(p) => p.span.clone(),
            Self::ReferenceOf(k) => k.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Prefix:
///     PrefixOperator Prefixable
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Prefix {
    #[get = "pub"]
    operator: PrefixOperator,
    #[get = "pub"]
    prefixable: Box<Prefixable>,
}

impl SourceElement for Prefix {
    fn span(&self) -> Span {
        self.operator.span().join(&self.prefixable.span()).unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Prefixable:
///     Postfixable
///     | Prefix
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Prefixable {
    Postfixable(Postfixable),
    Prefix(Prefix),
}

impl SyntaxTree for Prefixable {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            (PrefixOperator::parse, Self::parse.map(Box::new))
                .map(|(operator, prefixable)| Prefix { operator, prefixable })
                .map(Self::Prefix),
            Postfixable::parse.map(Self::Postfixable),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Prefixable {
    fn span(&self) -> Span {
        match self {
            Self::Postfixable(postfixable) => postfixable.span(),
            Self::Prefix(prefix) => prefix.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// BinaryOperator:
///     '+'
///     | '-'
///     | '*'
///     | '/'
///     | '%'
///     | '='
///     | '+='
///     | '-='
///     | '*='
///     | '/='
///     | '%='
///     | '=='
///     | '!='
///     | '<'
///     | '<='
///     | '>'
///     | '>='
///     | ':='
///     | 'and'
///     | 'or'
///     | '&'
///     | '&='
///     | '|'
///     | '|='
///     | '^'
///     | '^='
///     | '<<'
///     | '<<='
///     | '>>'
///     | '>>='
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum BinaryOperator {
    Add(Punctuation),
    Subtract(Punctuation),
    Multiply(Punctuation),
    Divide(Punctuation),
    Modulo(Punctuation),
    Assign(Punctuation),
    CompoundAdd(Punctuation, Punctuation),
    CompoundSubtract(Punctuation, Punctuation),
    CompoundMultiply(Punctuation, Punctuation),
    CompoundDivide(Punctuation, Punctuation),
    CompoundModulo(Punctuation, Punctuation),
    Equal(Punctuation, Punctuation),
    NotEqual(Punctuation, Punctuation),
    LessThan(Punctuation),
    LessThanOrEqual(Punctuation, Punctuation),
    GreaterThan(Punctuation),
    GreaterThanOrEqual(Punctuation, Punctuation),
    LogicalAnd(Keyword),
    LogicalOr(Keyword),
    BitwiseAnd(Punctuation),
    CompoundBitwiseAnd(Punctuation, Punctuation),
    BitwiseOr(Punctuation),
    CompoundBitwiseOr(Punctuation, Punctuation),
    BitwiseXor(Punctuation),
    CompoundBitwiseXor(Punctuation, Punctuation),
    BitwiseLeftShift(Punctuation, Punctuation),
    CompoundBitwiseLeftShift(Punctuation, Punctuation, Punctuation),
    BitwiseRightShift(Punctuation, Punctuation),
    CompoundBitwiseRightShift(Punctuation, Punctuation, Punctuation),
}

impl BinaryOperator {
    fn parse_arithmetic_and_compound(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            ('+'.to_owned(), '='.no_skip().to_owned())
                .map(|(start, end)| Self::CompoundAdd(start, end)),
            ('-'.to_owned(), '='.no_skip().to_owned())
                .map(|(start, end)| Self::CompoundSubtract(start, end)),
            ('*'.to_owned(), '='.no_skip().to_owned())
                .map(|(start, end)| Self::CompoundMultiply(start, end)),
            ('/'.to_owned(), '='.no_skip().to_owned())
                .map(|(start, end)| Self::CompoundDivide(start, end)),
            ('%'.to_owned(), '='.no_skip().to_owned())
                .map(|(start, end)| Self::CompoundModulo(start, end)),
            '+'.to_owned().map(Self::Add),
            '-'.to_owned().map(Self::Subtract),
            '*'.to_owned().map(Self::Multiply),
            '/'.to_owned().map(Self::Divide),
            '%'.to_owned().map(Self::Modulo),
        )
            .branch()
            .parse(state_machine, handler)
    }

    fn parse_left_angle(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            (
                '<'.to_owned(),
                '<'.no_skip().to_owned(),
                '='.no_skip().to_owned(),
            )
                .map(|(start, end, assign)| {
                    Self::CompoundBitwiseLeftShift(start, end, assign)
                }),
            ('<'.to_owned(), '<'.no_skip().to_owned())
                .map(|(start, end)| Self::BitwiseLeftShift(start, end)),
            ('<'.to_owned(), '='.no_skip().to_owned())
                .map(|(start, end)| Self::LessThanOrEqual(start, end)),
            '<'.to_owned().map(Self::LessThan),
        )
            .branch()
            .parse(state_machine, handler)
    }

    fn parse_right_angle(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            (
                '>'.to_owned(),
                '>'.no_skip().to_owned(),
                '='.no_skip().to_owned(),
            )
                .map(|(start, end, assign)| {
                    Self::CompoundBitwiseRightShift(start, end, assign)
                }),
            ('>'.to_owned(), '>'.no_skip().to_owned())
                .map(|(start, end)| Self::BitwiseRightShift(start, end)),
            ('>'.to_owned(), '='.no_skip().to_owned())
                .map(|(start, end)| Self::GreaterThanOrEqual(start, end)),
            '>'.to_owned().map(Self::GreaterThan),
        )
            .branch()
            .parse(state_machine, handler)
    }

    fn parse_bitwise_and_or_xor(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            ('&'.to_owned(), '='.no_skip().to_owned())
                .map(|(start, end)| Self::CompoundBitwiseAnd(start, end)),
            '&'.to_owned().map(Self::BitwiseAnd),
            ('|'.to_owned(), '='.no_skip().to_owned())
                .map(|(start, end)| Self::CompoundBitwiseOr(start, end)),
            '|'.to_owned().map(Self::BitwiseOr),
            ('^'.to_owned(), '='.no_skip().to_owned())
                .map(|(start, end)| Self::CompoundBitwiseXor(start, end)),
            '^'.to_owned().map(Self::BitwiseXor),
        )
            .branch()
            .parse(state_machine, handler)
    }

    fn parse_equal(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            ('='.to_owned(), '='.no_skip().to_owned())
                .map(|(start, end)| Self::Equal(start, end)),
            ('!'.to_owned(), '='.no_skip().to_owned())
                .map(|(start, end)| Self::NotEqual(start, end)),
            '='.to_owned().map(Self::Assign),
        )
            .branch()
            .parse(state_machine, handler)
    }

    fn parse_logical_and_or(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::And.to_owned().map(Self::LogicalAnd),
            KeywordKind::Or.to_owned().map(Self::LogicalOr),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SyntaxTree for BinaryOperator {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Self::parse_arithmetic_and_compound,
            Self::parse_left_angle,
            Self::parse_right_angle,
            Self::parse_bitwise_and_or_xor,
            Self::parse_equal,
            Self::parse_logical_and_or,
        )
            .branch()
            .parse(state_machine, handler)
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// BinaryNode:
///     Prefixable
///     | Brace
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum BinaryNode {
    Prefixable(Prefixable),
    Brace(Brace),
}

impl SourceElement for BinaryNode {
    fn span(&self) -> Span {
        match self {
            Self::Prefixable(prefixable) => prefixable.span(),
            Self::Brace(brace) => brace.span(),
        }
    }
}

impl SyntaxTree for BinaryNode {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self>
    where
        Self: Sized,
    {
        (Prefixable::parse.map(Self::Prefixable), Brace::parse.map(Self::Brace))
            .branch()
            .parse(state_machine, handler)
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Binary:
///     Prefixable (BinaryOperator Prefixable)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Binary {
    #[get = "pub"]
    first: BinaryNode,
    #[get = "pub"]
    chain: Vec<(BinaryOperator, BinaryNode)>,
}

impl SyntaxTree for Binary {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self>
    where
        Self: Sized,
    {
        (
            BinaryNode::parse,
            (BinaryOperator::parse, BinaryNode::parse).keep_take(),
        )
            .map(|(first, chain)| Self { first, chain })
            .parse(state_machine, handler)
    }
}

impl Binary {
    /// Destructs the binary into its components
    #[must_use]
    pub fn destruct(self) -> (BinaryNode, Vec<(BinaryOperator, BinaryNode)>) {
        (self.first, self.chain)
    }
}

impl SourceElement for Binary {
    fn span(&self) -> Span {
        self.chain.last().map_or_else(
            || self.first.span(),
            |(_, right)| self.first.span().join(&right.span()).unwrap(),
        )
    }
}

#[cfg(test)]
mod test;
