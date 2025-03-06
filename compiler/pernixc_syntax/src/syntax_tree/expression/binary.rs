use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::token::{Keyword, KeywordKind, Punctuation};
use pernixc_source_file::{SourceElement, Span};

use super::{block::Block, prefix::Prefixable};
use crate::{
    error,
    state_machine::{
        parse::{self, Branch, ExpectExt, Parse},
        StateMachine,
    },
    syntax_tree::SyntaxTree,
};

pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
                .commit_in(2)
                .map(|(start, end)| Self::CompoundSubtract(start, end)),
            ('*'.to_owned(), '='.no_skip().to_owned())
                .commit_in(2)
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
                .commit_in(2)
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
                .commit_in(2)
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum BinaryNode {
    Prefixable(Prefixable),
    Brace(Block),
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
    ) -> parse::Result<Self> {
        (Prefixable::parse.map(Self::Prefixable), Block::parse.map(Self::Brace))
            .branch()
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Binary {
    pub first: BinaryNode,
    pub chain: Vec<(BinaryOperator, BinaryNode)>,
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

impl SourceElement for Binary {
    fn span(&self) -> Span {
        self.chain.last().map_or_else(
            || self.first.span(),
            |(_, right)| self.first.span().join(&right.span()),
        )
    }
}
