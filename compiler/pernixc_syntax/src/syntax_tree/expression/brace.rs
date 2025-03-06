use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::token::{Keyword, KeywordKind, Punctuation};
use pernixc_source_file::{SourceElement, Span};

use super::{unit::Parenthesized, Expression, LabelSpecifier};
use crate::{
    error,
    state_machine::{
        parse::{self, Branch, Parse, Passable},
        StateMachine,
    },
    syntax_tree::{pattern::Refutable, statement::Statements, SyntaxTree},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(clippy::large_enum_variant)]
pub enum Brace {
    Scope(Scope),
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
            Scope::parse.map(Self::Scope),
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
            Self::Scope(syn) => syn.span(),
            Self::IfElse(syn) => syn.span(),
            Self::Loop(syn) => syn.span(),
            Self::Match(syn) => syn.span(),
            Self::While(syn) => syn.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MatchArm {
    pub refutable_pattern: Refutable,
    pub block: Block,
}

impl SyntaxTree for MatchArm {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (Refutable::parse, Block::parse)
            .map(|(refutable_pattern, block)| Self { refutable_pattern, block })
            .parse(state_machine, handler)
    }
}

impl SourceElement for MatchArm {
    fn span(&self) -> Span {
        self.refutable_pattern.span().join(&self.block.span())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MatchBody {
    colon: Punctuation,
    arms: Vec<Passable<MatchArm>>,
}

impl SyntaxTree for MatchBody {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        MatchArm::parse
            .indentation_item()
            .keep_take_all()
            .step_into_indentation()
            .map(|(colon, arms)| Self { colon: colon.clone(), arms })
            .parse(state_machine, handler)
    }
}

impl SourceElement for MatchBody {
    fn span(&self) -> Span {
        let start = self.colon.span();
        let end = self
            .arms
            .iter()
            .fold(start.clone(), |span, arm| span.join(&arm.span()));

        start.join(&end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Match {
    pub match_keyword: Keyword,
    pub expression: Box<Expression>,
    pub body: MatchBody,
}

impl SyntaxTree for Match {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Match.to_owned(),
            Expression::parse.map(Box::new),
            MatchBody::parse,
        )
            .map(|(match_keyword, expression, body)| Self {
                match_keyword,
                expression,
                body,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Match {
    fn span(&self) -> Span { self.match_keyword.span.join(&self.body.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scope {
    pub unsafe_keyword: Option<Keyword>,
    pub scope_keyword: Keyword,
    pub label_specifier: Option<LabelSpecifier>,
    pub statements: Statements,
}

impl SyntaxTree for Scope {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Unsafe.to_owned().or_none(),
            KeywordKind::Scope.to_owned(),
            LabelSpecifier::parse.or_none(),
            Statements::parse,
        )
            .map(
                |(
                    unsafe_keyword,
                    scope_keyword,
                    label_specifier,
                    statements,
                )| Self {
                    unsafe_keyword,
                    scope_keyword,
                    label_specifier,
                    statements,
                },
            )
            .parse(state_machine, handler)
    }
}

impl SourceElement for Scope {
    fn span(&self) -> Span {
        let start = self
            .unsafe_keyword
            .as_ref()
            .map_or_else(|| self.scope_keyword.span(), SourceElement::span);

        start.join(&self.statements.span())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IndentedBlock {
    pub unsafe_keyword: Option<Keyword>,
    pub label_specifier: Option<LabelSpecifier>,
    pub statements: Statements,
}

impl SyntaxTree for IndentedBlock {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Unsafe.to_owned().or_none(),
            LabelSpecifier::parse.or_none(),
            Statements::parse,
        )
            .map(|(unsafe_keyword, label_specifier, statements)| Self {
                unsafe_keyword,
                label_specifier,
                statements,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for IndentedBlock {
    fn span(&self) -> Span {
        let end = self.statements.span();
        let start = self.unsafe_keyword.as_ref().map_or_else(
            || {
                self.label_specifier
                    .as_ref()
                    .map_or_else(|| end.clone(), SourceElement::span)
            },
            SourceElement::span,
        );

        start.join(&self.statements.span())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InlineBlock {
    pub colon: Punctuation,
    pub expression: Box<Expression>,
}

impl SyntaxTree for InlineBlock {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (':'.to_owned(), Expression::parse.map(Box::new))
            .map(|(colon, expression)| Self { colon, expression })
            .parse(state_machine, handler)
    }
}

impl SourceElement for InlineBlock {
    fn span(&self) -> Span { self.colon.span().join(&self.expression.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Block {
    Indented(IndentedBlock),
    Inline(InlineBlock),
}

impl SyntaxTree for Block {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            IndentedBlock::parse.map(Self::Indented),
            InlineBlock::parse.map(Self::Inline),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Block {
    fn span(&self) -> Span {
        match self {
            Self::Indented(block) => block.span(),
            Self::Inline(block) => block.span(),
        }
    }
}

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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Else {
    pub else_keyword: Keyword,
    pub expression: Box<BlockOrIfElse>,
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
        self.else_keyword.span().join(&self.expression.span())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IfElse {
    pub if_keyword: Keyword,
    pub parenthesized: Parenthesized,
    pub then_expression: Block,
    pub else_expression: Option<Else>,
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

        start.join(&end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct While {
    pub while_keyword: Keyword,
    pub parenthesized: Parenthesized,
    pub block: Block,
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
        self.while_keyword.span().join(&self.block.span())
    }
}

impl While {
    /// Destructs the while into its components
    #[must_use]
    pub fn destruct(self) -> (Keyword, Parenthesized, Block) {
        (self.while_keyword, self.parenthesized, self.block)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Loop {
    pub loop_keyword: Keyword,
    pub block: Block,
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
    fn span(&self) -> Span { self.loop_keyword.span.join(&self.block.span()) }
}
