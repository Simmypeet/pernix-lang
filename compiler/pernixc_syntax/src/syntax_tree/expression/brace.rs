use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation},
    token_stream::DelimiterKind,
};
use pernixc_source_file::{SourceElement, Span};

use super::{unit::Parenthesized, Expression, LabelSpecifier};
use crate::{
    error,
    state_machine::{
        parse::{self, Branch, Parse},
        StateMachine,
    },
    syntax_tree::{
        pattern::Refutable, statement::Statements, EnclosedConnectedList,
        ParseExt, SyntaxTree,
    },
};

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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MatchArm {
    pub refutable_pattern: Refutable,
    pub colon: Punctuation,
    pub expression: Box<Expression>,
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
        self.refutable_pattern.span().join(&self.expression.span())
    }
}

pub type MatchArms = EnclosedConnectedList<MatchArm, Punctuation>;

impl SyntaxTree for MatchArms {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        MatchArm::parse
            .enclosed_connected_list(','.to_owned(), DelimiterKind::Brace)
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Match {
    pub match_keyword: Keyword,
    pub parenthesized: Parenthesized,
    pub arms: MatchArms,
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
    fn span(&self) -> Span { self.match_keyword.span.join(&self.arms.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Block {
    pub label_specifier: Option<LabelSpecifier>,
    pub unsafe_keyword: Option<Keyword>,
    pub statements: Statements,
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
        start.join(&end)
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
