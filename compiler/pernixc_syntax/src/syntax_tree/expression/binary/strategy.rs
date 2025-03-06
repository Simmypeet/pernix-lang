use std::fmt::{Display, Write};

use enum_as_inner::EnumAsInner;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy, TestCaseError},
    prop_oneof,
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    expression::{
        block::strategy::Block, prefix::strategy::Prefixable,
        strategy::Expression,
    },
    r#type::strategy::Type,
    statement::strategy::Statement,
    strategy::{IndentDisplay, QualifiedIdentifier},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Assign,
    CompoundAdd,
    CompoundSubtract,
    CompoundMultiply,
    CompoundDivide,
    CompoundModulo,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    CompoundBitwiseAnd,
    BitwiseOr,
    CompoundBitwiseOr,
    BitwiseXor,
    CompoundBitwiseXor,
    BitwiseLeftShift,
    CompoundBitwiseLeftShift,
    BitwiseRightShift,
    CompoundBitwiseRightShift,
}

impl Input<&super::BinaryOperator> for &BinaryOperator {
    fn assert(self, output: &super::BinaryOperator) -> TestCaseResult {
        match (self, output) {
            (BinaryOperator::Add, super::BinaryOperator::Add(_))
            | (BinaryOperator::Subtract, super::BinaryOperator::Subtract(_))
            | (BinaryOperator::Multiply, super::BinaryOperator::Multiply(_))
            | (BinaryOperator::Divide, super::BinaryOperator::Divide(_))
            | (BinaryOperator::Modulo, super::BinaryOperator::Modulo(_))
            | (BinaryOperator::Assign, super::BinaryOperator::Assign(_))
            | (
                BinaryOperator::CompoundAdd,
                super::BinaryOperator::CompoundAdd(..),
            )
            | (
                BinaryOperator::CompoundSubtract,
                super::BinaryOperator::CompoundSubtract(..),
            )
            | (
                BinaryOperator::CompoundMultiply,
                super::BinaryOperator::CompoundMultiply(..),
            )
            | (
                BinaryOperator::CompoundDivide,
                super::BinaryOperator::CompoundDivide(..),
            )
            | (
                BinaryOperator::CompoundModulo,
                super::BinaryOperator::CompoundModulo(..),
            )
            | (BinaryOperator::Equal, super::BinaryOperator::Equal(..))
            | (BinaryOperator::NotEqual, super::BinaryOperator::NotEqual(..))
            | (BinaryOperator::LessThan, super::BinaryOperator::LessThan(_))
            | (
                BinaryOperator::LessThanOrEqual,
                super::BinaryOperator::LessThanOrEqual(..),
            )
            | (
                BinaryOperator::GreaterThan,
                super::BinaryOperator::GreaterThan(_),
            )
            | (
                BinaryOperator::GreaterThanOrEqual,
                super::BinaryOperator::GreaterThanOrEqual(..),
            )
            | (
                BinaryOperator::LogicalAnd,
                super::BinaryOperator::LogicalAnd(_),
            )
            | (
                BinaryOperator::LogicalOr,
                super::BinaryOperator::LogicalOr(_),
            )
            | (
                BinaryOperator::BitwiseAnd,
                super::BinaryOperator::BitwiseAnd(_),
            )
            | (
                BinaryOperator::CompoundBitwiseAnd,
                super::BinaryOperator::CompoundBitwiseAnd(..),
            )
            | (
                BinaryOperator::BitwiseOr,
                super::BinaryOperator::BitwiseOr(_),
            )
            | (
                BinaryOperator::CompoundBitwiseOr,
                super::BinaryOperator::CompoundBitwiseOr(..),
            )
            | (
                BinaryOperator::BitwiseXor,
                super::BinaryOperator::BitwiseXor(_),
            )
            | (
                BinaryOperator::CompoundBitwiseXor,
                super::BinaryOperator::CompoundBitwiseXor(..),
            )
            | (
                BinaryOperator::BitwiseLeftShift,
                super::BinaryOperator::BitwiseLeftShift(..),
            )
            | (
                BinaryOperator::CompoundBitwiseLeftShift,
                super::BinaryOperator::CompoundBitwiseLeftShift(..),
            )
            | (
                BinaryOperator::BitwiseRightShift,
                super::BinaryOperator::BitwiseRightShift(..),
            )
            | (
                BinaryOperator::CompoundBitwiseRightShift,
                super::BinaryOperator::CompoundBitwiseRightShift(..),
            ) => Ok(()),

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for BinaryOperator {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Add),
            Just(Self::Subtract),
            Just(Self::Multiply),
            Just(Self::Divide),
            Just(Self::Modulo),
            Just(Self::Assign),
            Just(Self::CompoundAdd),
            Just(Self::CompoundSubtract),
            Just(Self::CompoundMultiply),
            Just(Self::CompoundDivide),
            Just(Self::CompoundModulo),
            Just(Self::Equal),
            Just(Self::NotEqual),
            Just(Self::LessThan),
            Just(Self::LessThanOrEqual),
            Just(Self::GreaterThan),
            Just(Self::GreaterThanOrEqual),
            Just(Self::LogicalAnd),
            Just(Self::LogicalOr),
            Just(Self::BitwiseAnd),
            Just(Self::CompoundBitwiseAnd),
            Just(Self::BitwiseOr),
            Just(Self::CompoundBitwiseOr),
            Just(Self::BitwiseXor),
            Just(Self::CompoundBitwiseXor),
            Just(Self::BitwiseLeftShift),
            Just(Self::CompoundBitwiseLeftShift),
            Just(Self::BitwiseRightShift),
            Just(Self::CompoundBitwiseRightShift),
        ]
        .boxed()
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => f.write_char('+'),
            Self::Subtract => f.write_char('-'),
            Self::Multiply => f.write_char('*'),
            Self::Divide => f.write_char('/'),
            Self::Modulo => f.write_char('%'),
            Self::Assign => f.write_char('='),
            Self::CompoundAdd => f.write_str("+="),
            Self::CompoundSubtract => f.write_str("-="),
            Self::CompoundMultiply => f.write_str("*="),
            Self::CompoundDivide => f.write_str("/="),
            Self::CompoundModulo => f.write_str("%="),
            Self::Equal => f.write_str("=="),
            Self::NotEqual => f.write_str("!="),
            Self::LessThan => f.write_char('<'),
            Self::LessThanOrEqual => f.write_str("<="),
            Self::GreaterThan => f.write_char('>'),
            Self::GreaterThanOrEqual => f.write_str(">="),
            Self::LogicalAnd => f.write_str("and"),
            Self::LogicalOr => f.write_str("or"),
            Self::BitwiseAnd => f.write_char('&'),
            Self::CompoundBitwiseAnd => f.write_str("&="),
            Self::BitwiseOr => f.write_char('|'),
            Self::CompoundBitwiseOr => f.write_str("|="),
            Self::BitwiseXor => f.write_char('^'),
            Self::CompoundBitwiseXor => f.write_str("^="),
            Self::BitwiseLeftShift => f.write_str("<<"),
            Self::CompoundBitwiseLeftShift => f.write_str("<<="),
            Self::BitwiseRightShift => f.write_str(">>"),
            Self::CompoundBitwiseRightShift => f.write_str(">>="),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum BinaryNode {
    Prefixable(Prefixable),
    Block(Block),
}

impl Arbitrary for BinaryNode {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Prefixable::arbitrary_with((
                args.0.clone(),
                args.1.clone(),
                args.2.clone(),
            ))
            .prop_map(BinaryNode::Prefixable),
            Block::arbitrary_with(args).prop_map(BinaryNode::Block),
        ]
        .boxed()
    }
}

impl Input<&super::BinaryNode> for &BinaryNode {
    fn assert(self, output: &super::BinaryNode) -> TestCaseResult {
        match (self, output) {
            (
                BinaryNode::Prefixable(input),
                super::BinaryNode::Prefixable(output),
            ) => input.assert(output),
            (BinaryNode::Block(input), super::BinaryNode::Brace(output)) => {
                input.assert(output)
            }

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl IndentDisplay for BinaryNode {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Prefixable(prefixable) => prefixable.indent_fmt(f, indent),
            Self::Block(block) => block.indent_fmt(f, indent),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Binary {
    pub first: BinaryNode,
    pub chain: Vec<(BinaryOperator, BinaryNode)>,
}

impl Arbitrary for Binary {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let prefixable = BinaryNode::arbitrary_with(args);
        (
            prefixable.clone(),
            proptest::collection::vec(
                (BinaryOperator::arbitrary(), prefixable),
                0..=3,
            ),
        )
            .prop_map(|(first, chain)| Self { first, chain })
            .boxed()
    }
}

impl Input<&super::Binary> for &Binary {
    fn assert(self, output: &super::Binary) -> TestCaseResult {
        self.first.assert(&output.first)?;
        self.chain.assert(&output.chain)?;

        Ok(())
    }
}

impl IndentDisplay for Binary {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.first.indent_fmt(f, indent)?;

        for (operator, prefixable) in &self.chain {
            write!(f, " {operator} ")?;
            prefixable.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}
