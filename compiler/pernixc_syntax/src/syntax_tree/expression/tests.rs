use std::fmt::{Display, Write};

use pernixc_tests::input::Input;
use proptest::{
    arbitrary::Arbitrary,
    prelude::prop::test_runner::TestCaseResult,
    prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::TestCaseError,
};

use crate::syntax_tree::{
    self,
    pattern::tests::Refutable,
    r#type::tests::Type,
    statement::tests::Statement,
    tests::{
        parse, ConnectedList, ConstantPunctuation, Identifier, Label,
        QualifiedIdentifier, Qualifier,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expression {
    Binary(Binary),
    Terminator(Terminator),
    Brace(Brace),
}

impl Input<&super::Expression> for &Expression {
    fn assert(self, output: &super::Expression) -> TestCaseResult {
        match (self, output) {
            (Expression::Binary(input), super::Expression::Binary(output)) => {
                input.assert(output)
            }
            (
                Expression::Terminator(input),
                super::Expression::Terminator(output),
            ) => input.assert(output),
            (Expression::Brace(input), super::Expression::Brace(output)) => {
                input.assert(output)
            }

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Expression {
    type Parameters = (
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            Boolean::arbitrary().prop_map(|x| {
                Self::Binary(Binary {
                    first: Box::new(Prefixable::Postfixable(
                        Postfixable::Unit(Unit::Boolean(x)),
                    )),
                    chain: Vec::new(),
                })
            }),
            Numeric::arbitrary().prop_map(|x| {
                Self::Binary(Binary {
                    first: Box::new(Prefixable::Postfixable(
                        Postfixable::Unit(Unit::Numeric(x)),
                    )),
                    chain: Vec::new(),
                })
            }),
        ];

        leaf.prop_recursive(4, 64, 16, move |inner| {
            prop_oneof![
                Binary::arbitrary_with((
                    Some(inner.clone()),
                    args.0.clone(),
                    args.1.clone()
                ))
                .prop_map(Expression::Binary),
                Terminator::arbitrary_with((
                    Some(inner.clone()),
                    args.0.clone(),
                    args.1.clone()
                ))
                .prop_map(Expression::Terminator),
                Brace::arbitrary_with((
                    Some(inner),
                    args.0.clone(),
                    args.1.clone(),
                    args.2.clone()
                ))
                .prop_map(Expression::Brace),
            ]
        })
        .boxed()
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Binary(binary) => Display::fmt(binary, f),
            Self::Terminator(terminator) => Display::fmt(terminator, f),
            Self::Brace(brace) => Display::fmt(brace, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LabelSpecifier {
    pub label: Label,
}

impl Input<&super::LabelSpecifier> for &LabelSpecifier {
    fn assert(self, output: &super::LabelSpecifier) -> TestCaseResult {
        self.label.assert(output.label())
    }
}

impl Arbitrary for LabelSpecifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        Label::arbitrary().prop_map(|label| Self { label }).boxed()
    }
}

impl Display for LabelSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: ", self.label)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Statements {
    pub statements: Vec<Statement>,
}

impl Input<&super::Statements> for &Statements {
    fn assert(self, output: &super::Statements) -> TestCaseResult {
        self.statements.assert(output.statements())
    }
}

impl Arbitrary for Statements {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let statement = args
            .3
            .unwrap_or_else(|| Statement::arbitrary_with((args.0, args.1)));

        proptest::collection::vec(statement, 0..=6)
            .prop_map(|statements| Self { statements })
            .boxed()
    }
}

impl Display for Statements {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('{')?;
        for statement in &self.statements {
            Display::fmt(statement, f)?;
        }
        f.write_char('}')
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Block {
    pub label_specifier: Option<LabelSpecifier>,
    pub is_unsafe: bool,
    pub statements: Statements,
}

impl Input<&super::Block> for &Block {
    fn assert(self, output: &super::Block) -> TestCaseResult {
        self.label_specifier
            .as_ref()
            .assert(output.label_specifier().as_ref())?;
        prop_assert_eq!(self.is_unsafe, output.unsafe_keyword.is_some());
        self.statements.assert(output.statements())
    }
}

impl Arbitrary for Block {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(LabelSpecifier::arbitrary()),
            proptest::bool::ANY,
            Statements::arbitrary_with(args),
        )
            .prop_map(|(label_specifier, is_unsafe, statements)| Self {
                label_specifier,
                is_unsafe,
                statements,
            })
            .boxed()
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(label_specifier) = &self.label_specifier {
            Display::fmt(label_specifier, f)?;
        }

        if self.is_unsafe {
            f.write_str(" unsafe ")?;
        }

        Display::fmt(&self.statements, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loop {
    pub block: Block,
}

impl Input<&super::Loop> for &Loop {
    fn assert(self, output: &super::Loop) -> TestCaseResult {
        self.block.assert(output.block())
    }
}

impl Arbitrary for Loop {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        Block::arbitrary_with(args).prop_map(|block| Self { block }).boxed()
    }
}

impl Display for Loop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("loop ")?;

        Display::fmt(&self.block, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IfElse {
    pub condition: Box<Expression>,
    pub then_expression: Block,
    pub else_expression: Option<Else>,
}

impl Arbitrary for IfElse {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expression = args.0.clone().unwrap_or_else(|| {
            Expression::arbitrary_with((
                args.1.clone(),
                args.2.clone(),
                args.3.clone(),
            ))
        });

        let leaf = (expression.clone(), Block::arbitrary_with(args.clone()))
            .prop_map(|(condition, then_expression)| Self {
                condition: Box::new(condition),
                then_expression,
                else_expression: None,
            });

        leaf.prop_recursive(4, 24, 6, move |inner| {
            (
                expression.clone(),
                Block::arbitrary_with(args.clone()),
                proptest::option::of(prop_oneof![
                    Block::arbitrary_with(args.clone()).prop_map(|x| {
                        Else { expression: Box::new(BlockOrIfElse::Block(x)) }
                    }),
                    inner.prop_map(|x| {
                        Else { expression: Box::new(BlockOrIfElse::IfElse(x)) }
                    })
                ]),
            )
                .prop_map(
                    |(condition, then_expression, else_expression)| Self {
                        condition: Box::new(condition),
                        then_expression,
                        else_expression,
                    },
                )
        })
        .boxed()
    }
}

impl Input<&super::IfElse> for &IfElse {
    fn assert(self, output: &super::IfElse) -> TestCaseResult {
        self.condition.assert(output.condition())?;
        self.then_expression.assert(output.then_expression())?;
        self.else_expression.as_ref().assert(output.else_expression().as_ref())
    }
}

impl Display for IfElse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if ({}) {}", self.condition, self.then_expression)?;

        if let Some(else_expression) = &self.else_expression {
            write!(f, " {else_expression}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum BlockOrIfElse {
    Block(Block),
    IfElse(IfElse),
}

impl Input<&super::BlockOrIfElse> for &BlockOrIfElse {
    fn assert(self, output: &super::BlockOrIfElse) -> TestCaseResult {
        match (self, output) {
            (BlockOrIfElse::Block(i), super::BlockOrIfElse::Block(o)) => {
                i.assert(o)
            }
            (BlockOrIfElse::IfElse(i), super::BlockOrIfElse::IfElse(o)) => {
                i.assert(o)
            }
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?} to be {output:?}"
            ))),
        }
    }
}

impl Display for BlockOrIfElse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Block(block) => Display::fmt(block, f),
            Self::IfElse(if_else) => Display::fmt(if_else, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Else {
    expression: Box<BlockOrIfElse>,
}

impl Input<&super::Else> for &Else {
    fn assert(self, output: &super::Else) -> TestCaseResult {
        self.expression.assert(output.expression())
    }
}

impl Display for Else {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("else ")?;

        Display::fmt(&self.expression, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MatchArmGuard {
    pub expression: Box<Expression>,
}

impl Arbitrary for MatchArmGuard {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        args.0
            .unwrap_or_else(|| {
                Expression::arbitrary_with((args.1, args.2, args.3))
            })
            .prop_map(|expression| Self { expression: Box::new(expression) })
            .boxed()
    }
}

impl Input<&super::MatchArmGuard> for &MatchArmGuard {
    fn assert(self, output: &super::MatchArmGuard) -> TestCaseResult {
        self.expression.assert(output.expression())
    }
}

impl Display for MatchArmGuard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if ({})", self.expression)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MatchArm {
    pub refutable_pattern: Refutable,
    pub guard: Option<MatchArmGuard>,
    pub block: Block,
}

impl Arbitrary for MatchArm {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            Refutable::arbitrary(),
            proptest::option::of(MatchArmGuard::arbitrary_with(args.clone())),
            Block::arbitrary_with(args),
        )
            .prop_map(|(pattern, guard, block)| Self {
                refutable_pattern: pattern,
                guard,
                block,
            })
            .boxed()
    }
}

impl Input<&super::MatchArm> for &MatchArm {
    fn assert(self, output: &super::MatchArm) -> TestCaseResult {
        self.refutable_pattern.assert(output.refutable_pattern())?;
        self.guard.as_ref().assert(output.guard().as_ref())?;
        self.block.assert(output.block())
    }
}

impl Display for MatchArm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.refutable_pattern)?;

        if let Some(guard) = &self.guard {
            write!(f, " {guard}")?;
        }

        write!(f, ": {}", self.block)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Match {
    pub expression: Box<Expression>,
    pub arms: Vec<MatchArm>,
}

impl Input<&super::Match> for &Match {
    fn assert(self, output: &super::Match) -> TestCaseResult {
        self.expression.assert(output.expression())?;
        self.arms.assert(output.arms())
    }
}

impl Arbitrary for Match {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expression = args.0.clone().unwrap_or_else(|| {
            Expression::arbitrary_with((
                args.1.clone(),
                args.2.clone(),
                args.3.clone(),
            ))
        });

        (
            expression,
            proptest::collection::vec(MatchArm::arbitrary_with(args), 1..=6),
        )
            .prop_map(|(expression, arms)| Self {
                expression: Box::new(expression),
                arms,
            })
            .boxed()
    }
}

impl Display for Match {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "match ({}) {{", self.expression)?;
        for arm in &self.arms {
            write!(f, "{arm}")?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Brace {
    Block(Block),
    Loop(Loop),
    IfElse(IfElse),
    Match(Match),
}

impl Input<&super::Brace> for &Brace {
    fn assert(self, output: &super::Brace) -> TestCaseResult {
        match (self, output) {
            (Brace::Block(input), super::Brace::Block(output)) => {
                input.assert(output)
            }
            (Brace::Loop(input), super::Brace::Loop(output)) => {
                input.assert(output)
            }
            (Brace::IfElse(input), super::Brace::IfElse(output)) => {
                input.assert(output)
            }
            (Brace::Match(input), super::Brace::Match(output)) => {
                input.assert(output)
            }

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Brace {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Block::arbitrary_with(args.clone()).prop_map(Brace::Block),
            Loop::arbitrary_with(args.clone()).prop_map(Brace::Loop),
            IfElse::arbitrary_with(args.clone()).prop_map(Brace::IfElse),
            Match::arbitrary_with(args).prop_map(Brace::Match),
        ]
        .boxed()
    }
}

impl Display for Brace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Block(block) => Display::fmt(block, f),
            Self::Loop(loop_) => Display::fmt(loop_, f),
            Self::IfElse(if_else) => Display::fmt(if_else, f),
            Self::Match(match_) => Display::fmt(match_, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Terminator {
    Return(Return),
    Continue(Continue),
    Express(Express),
    Break(Break),
}

impl Input<&super::Terminator> for &Terminator {
    fn assert(self, output: &super::Terminator) -> TestCaseResult {
        match (self, output) {
            (Terminator::Return(input), super::Terminator::Return(output)) => {
                input.assert(output)
            }
            (
                Terminator::Continue(input),
                super::Terminator::Continue(output),
            ) => input.assert(output),
            (
                Terminator::Express(input),
                super::Terminator::Express(output),
            ) => input.assert(output),
            (Terminator::Break(input), super::Terminator::Break(output)) => {
                input.assert(output)
            }

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Terminator {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Return::arbitrary_with(args.clone()).prop_map(Terminator::Return),
            Continue::arbitrary_with(()).prop_map(Terminator::Continue),
            Express::arbitrary_with(args.clone()).prop_map(Terminator::Express),
            Break::arbitrary_with(args).prop_map(Terminator::Break),
        ]
        .boxed()
    }
}

impl Display for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Return(r#return) => Display::fmt(r#return, f),
            Self::Continue(continue_) => Display::fmt(continue_, f),
            Self::Express(express) => Display::fmt(express, f),
            Self::Break(break_) => Display::fmt(break_, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Return {
    pub binary: Option<Binary>,
}

impl Input<&super::Return> for &Return {
    fn assert(self, output: &super::Return) -> TestCaseResult {
        self.binary.as_ref().assert(output.binary().as_ref())
    }
}

impl Arbitrary for Return {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::option::of(Binary::arbitrary_with(args))
            .prop_map(|expression| Self { binary: expression })
            .boxed()
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("return")?;

        if let Some(expression) = &self.binary {
            write!(f, " {expression}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Continue {
    pub label: Option<Label>,
}

impl Input<&super::Continue> for &Continue {
    fn assert(self, output: &super::Continue) -> TestCaseResult {
        self.label.as_ref().assert(output.label().as_ref())
    }
}

impl Arbitrary for Continue {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::option::of(Label::arbitrary())
            .prop_map(|label| Self { label })
            .boxed()
    }
}

impl Display for Continue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("continue")?;

        if let Some(label) = &self.label {
            write!(f, " {label}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Express {
    pub label: Option<Label>,
    pub binary: Option<Binary>,
}

impl Input<&super::Express> for &Express {
    fn assert(self, output: &super::Express) -> TestCaseResult {
        self.label.as_ref().assert(output.label().as_ref())?;
        self.binary.as_ref().assert(output.binary().as_ref())
    }
}

impl Arbitrary for Express {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(Label::arbitrary()),
            proptest::option::of(Binary::arbitrary_with(args)),
        )
            .prop_map(|(label, binary)| Self { label, binary })
            .boxed()
    }
}

impl Display for Express {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("express")?;

        if let Some(label) = &self.label {
            write!(f, " {label}")?;
        }

        if let Some(binary) = &self.binary {
            write!(f, " {binary}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Break {
    pub label: Option<Label>,
    pub binary: Option<Binary>,
}

impl Input<&super::Break> for &Break {
    fn assert(self, output: &super::Break) -> TestCaseResult {
        self.label.as_ref().assert(output.label().as_ref())?;
        self.binary.as_ref().assert(output.binary().as_ref())
    }
}

impl Arbitrary for Break {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(Label::arbitrary()),
            proptest::option::of(Binary::arbitrary_with(args)),
        )
            .prop_map(|(label, binary)| Self { label, binary })
            .boxed()
    }
}

impl Display for Break {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("break")?;

        if let Some(label) = &self.label {
            write!(f, " {label}")?;
        }

        if let Some(binary) = &self.binary {
            write!(f, " {binary}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Boolean {
    pub value: bool,
}

impl Arbitrary for Boolean {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::bool::ANY.prop_map(|value| Self { value }).boxed()
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", if self.value { "true" } else { "false" })
    }
}

impl Input<&super::Boolean> for &Boolean {
    fn assert(self, output: &super::Boolean) -> TestCaseResult {
        match (self.value, output) {
            (true, super::Boolean::True(_))
            | (false, super::Boolean::False(_)) => Ok(()),

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Decimal {
    pub numeric: syntax_tree::tests::Numeric,
}

impl Input<&super::Decimal> for &Decimal {
    fn assert(self, output: &super::Decimal) -> TestCaseResult {
        self.numeric.assert(&output.numeric)
    }
}

impl Arbitrary for Decimal {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        syntax_tree::tests::Numeric::arbitrary_with(())
            .prop_map(|numeric| Self { numeric })
            .boxed()
    }
}

impl Display for Decimal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ".{}", self.numeric)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(clippy::struct_field_names)]
pub struct Numeric {
    pub numeric: syntax_tree::tests::Numeric,
    pub decimal: Option<Decimal>,
    pub suffix: Option<Identifier>,
}

impl Input<&super::Numeric> for &Numeric {
    fn assert(self, output: &super::Numeric) -> TestCaseResult {
        self.numeric.assert(&output.numeric)?;
        self.decimal.as_ref().assert(output.decimal.as_ref())?;
        self.suffix.as_ref().assert(output.suffix.as_ref())
    }
}

impl Arbitrary for Numeric {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            syntax_tree::tests::Numeric::arbitrary_with(()),
            proptest::option::of(Decimal::arbitrary_with(())),
            proptest::option::of(Identifier::arbitrary_with(())),
        )
            .prop_map(|(numeric, decimal, suffix)| Self {
                numeric,
                decimal,
                suffix,
            })
            .boxed()
    }
}

impl Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.numeric)?;

        if let Some(decimal) = &self.decimal {
            write!(f, "{decimal}")?;
        }

        if let Some(suffix) = &self.suffix {
            write!(f, "{suffix}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unpackable {
    pub ellipsis: bool,
    pub expression: Box<Expression>,
}

impl Input<&super::Unpackable> for &Unpackable {
    fn assert(self, output: &super::Unpackable) -> TestCaseResult {
        prop_assert_eq!(self.ellipsis, output.ellipsis().is_some());
        self.expression.assert(output.expression())
    }
}

impl Display for Unpackable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ellipsis {
            f.write_str("...")?;
        }

        Display::fmt(&self.expression, f)
    }
}

impl Arbitrary for Unpackable {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expression_strategy = args.unwrap_or_else(Expression::arbitrary);

        (proptest::bool::ANY, expression_strategy.prop_map(Box::new))
            .prop_map(|(ellipsis, expression)| Self { ellipsis, expression })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parenthesized {
    pub expressions:
        Option<ConnectedList<Unpackable, ConstantPunctuation<','>>>,
}

impl Input<&super::Parenthesized> for &Parenthesized {
    fn assert(self, output: &super::Parenthesized) -> TestCaseResult {
        self.expressions.as_ref().assert(output.expression().as_ref())
    }
}

impl Arbitrary for Parenthesized {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            Unpackable::arbitrary_with(args),
            ConstantPunctuation::<','>::arbitrary(),
        ))
        .prop_map(|expressions| Self { expressions })
        .boxed()
    }
}

impl Display for Parenthesized {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;

        if let Some(expressions) = &self.expressions {
            write!(f, "{expressions}")?;
        }

        write!(f, ")")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldInitializer {
    pub identifier: Identifier,
    pub expression: Box<Expression>,
}

impl Input<&super::FieldInitializer> for &FieldInitializer {
    fn assert(self, output: &super::FieldInitializer) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.expression.assert(output.expression())
    }
}

impl Arbitrary for FieldInitializer {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expression = args.unwrap_or_else(Expression::arbitrary);

        (Identifier::arbitrary(), expression)
            .prop_map(|(identifier, expression)| Self {
                identifier,
                expression: Box::new(expression),
            })
            .boxed()
    }
}

impl Display for FieldInitializer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.identifier, self.expression)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub qualified_identifier: QualifiedIdentifier,
    pub field_initializers:
        Option<ConnectedList<FieldInitializer, ConstantPunctuation<','>>>,
}

impl Input<&super::Struct> for &Struct {
    fn assert(self, output: &super::Struct) -> TestCaseResult {
        self.qualified_identifier.assert(output.qualified_identifier())?;
        self.field_initializers
            .as_ref()
            .assert(output.field_initializers().as_ref())
    }
}

impl Arbitrary for Struct {
    type Parameters = (
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            args.2.clone().unwrap_or_else(|| {
                QualifiedIdentifier::arbitrary_with((
                    args.0.clone(),
                    args.1.clone(),
                ))
            }),
            proptest::option::of(ConnectedList::arbitrary_with(
                FieldInitializer::arbitrary_with(args.1),
                ConstantPunctuation::arbitrary(),
            )),
        )
            .prop_map(|(qualified_identifier, field_initializers)| Self {
                qualified_identifier,
                field_initializers,
            })
            .boxed()
    }
}

impl Display for Struct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.qualified_identifier, f)?;

        f.write_char('{')?;
        if let Some(field_initializers) = &self.field_initializers {
            Display::fmt(field_initializers, f)?;
        }
        f.write_char('}')?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array {
    pub expressions:
        Option<ConnectedList<Box<Expression>, ConstantPunctuation<','>>>,
}

impl Input<&super::Array> for &Array {
    fn assert(self, output: &super::Array) -> TestCaseResult {
        self.expressions.as_ref().assert(output.arguments().as_ref())
    }
}

impl Arbitrary for Array {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expression_strategy = args.unwrap_or_else(Expression::arbitrary);

        proptest::option::of(ConnectedList::arbitrary_with(
            expression_strategy.prop_map(Box::new),
            ConstantPunctuation::arbitrary(),
        ))
        .prop_map(|expressions| Self { expressions })
        .boxed()
    }
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('[')?;

        if let Some(expressions) = &self.expressions {
            write!(f, "{expressions}")?;
        }

        f.write_char(']')
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Phantom;

impl Arbitrary for Phantom {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::strategy::Just(Self).boxed()
    }
}

impl Display for Phantom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "phantom")?;

        Ok(())
    }
}

impl Input<&super::Phantom> for &Phantom {
    fn assert(self, _: &super::Phantom) -> TestCaseResult { Ok(()) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Unit {
    Boolean(Boolean),
    Numeric(Numeric),
    QualifiedIdentifier(QualifiedIdentifier),
    Parenthesized(Parenthesized),
    Struct(Struct),
    Array(Array),
    Phantom(Phantom),
}

impl Arbitrary for Unit {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(
        (expr_strategy, ty_strategy, qualified_strategy): Self::Parameters,
    ) -> Self::Strategy {
        prop_oneof![
            Boolean::arbitrary_with(()).prop_map(Unit::Boolean),
            Numeric::arbitrary_with(()).prop_map(Unit::Numeric),
            qualified_strategy
                .clone()
                .unwrap_or_else(|| {
                    QualifiedIdentifier::arbitrary_with((
                        ty_strategy.clone(),
                        expr_strategy.clone(),
                    ))
                })
                .prop_map(Unit::QualifiedIdentifier),
            Parenthesized::arbitrary_with(expr_strategy.clone())
                .prop_map(Unit::Parenthesized),
            Struct::arbitrary_with((
                ty_strategy,
                expr_strategy.clone(),
                qualified_strategy
            ))
            .prop_map(Unit::Struct),
            Array::arbitrary_with(expr_strategy).prop_map(Unit::Array),
            Just(Self::Phantom(Phantom)),
        ]
        .boxed()
    }
}

impl Input<&super::Unit> for &Unit {
    fn assert(self, output: &super::Unit) -> TestCaseResult {
        match (self, output) {
            (Unit::Boolean(input), super::Unit::Boolean(output)) => {
                input.assert(output)
            }
            (Unit::Numeric(input), super::Unit::Numeric(output)) => {
                input.assert(output)
            }
            (
                Unit::QualifiedIdentifier(input),
                super::Unit::QualifiedIdentifier(output),
            ) => input.assert(output),
            (
                Unit::Parenthesized(input),
                super::Unit::Parenthesized(output),
            ) => input.assert(output),
            (Unit::Struct(input), super::Unit::Struct(output)) => {
                input.assert(output)
            }
            (Unit::Array(input), super::Unit::Array(output)) => {
                input.assert(output)
            }
            (Unit::Phantom(input), super::Unit::Phantom(output)) => {
                input.assert(output)
            }

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean(boolean_literal) => Display::fmt(boolean_literal, f),
            Self::Numeric(numeric_literal) => Display::fmt(numeric_literal, f),
            Self::QualifiedIdentifier(qualified_identifier) => {
                Display::fmt(qualified_identifier, f)
            }
            Self::Parenthesized(parenthesized) => {
                Display::fmt(parenthesized, f)
            }
            Self::Struct(struct_literal) => Display::fmt(struct_literal, f),
            Self::Array(array_literal) => Display::fmt(array_literal, f),
            Self::Phantom(phantom) => Display::fmt(phantom, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Call {
    pub arguments:
        Option<ConnectedList<Box<Expression>, ConstantPunctuation<','>>>,
}

impl Input<&super::Call> for &Call {
    fn assert(self, output: &super::Call) -> TestCaseResult {
        self.arguments.as_ref().assert(output.arguments().as_ref())
    }
}

impl Arbitrary for Call {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expression_strategy = args.unwrap_or_else(Expression::arbitrary);

        proptest::option::of(ConnectedList::arbitrary_with(
            expression_strategy.prop_map(Box::new),
            ConstantPunctuation::arbitrary(),
        ))
        .prop_map(|arguments| Self { arguments })
        .boxed()
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('(')?;

        if let Some(arguments) = &self.arguments {
            write!(f, "{arguments}")?;
        }

        f.write_char(')')
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cast {
    pub r#type: Type,
}

impl Input<&super::Cast> for &Cast {
    fn assert(self, output: &super::Cast) -> TestCaseResult {
        self.r#type.assert(output.r#type())
    }
}

impl Arbitrary for Cast {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let type_strategy =
            args.1.unwrap_or_else(|| Type::arbitrary_with((args.0, args.2)));

        type_strategy.prop_map(|r#type| Self { r#type }).boxed()
    }
}

impl Display for Cast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, " as {}", self.r#type)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AccessOperator {
    Dot,
    Arrow,
}

impl Input<&super::AccessOperator> for &AccessOperator {
    fn assert(self, output: &super::AccessOperator) -> TestCaseResult {
        match (self, output) {
            (AccessOperator::Dot, super::AccessOperator::Dot(_))
            | (AccessOperator::Arrow, super::AccessOperator::Arrow(_, _)) => {
                Ok(())
            }

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for AccessOperator {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![Just(Self::Dot), Just(Self::Arrow)].boxed()
    }
}

impl Display for AccessOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dot => f.write_char('.'),
            Self::Arrow => f.write_str("->"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AccessKind {
    Identifier(Identifier),
    Tuple(syntax_tree::tests::Numeric),
}

impl Input<&super::AccessKind> for &AccessKind {
    fn assert(self, output: &super::AccessKind) -> TestCaseResult {
        match (self, output) {
            (
                AccessKind::Identifier(input),
                super::AccessKind::Identifier(output),
            ) => input.assert(output),
            (AccessKind::Tuple(input), super::AccessKind::Tuple(output)) => {
                input.assert(output)
            }

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for AccessKind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Identifier::arbitrary().prop_map(AccessKind::Identifier),
            syntax_tree::tests::Numeric::arbitrary()
                .prop_map(AccessKind::Tuple),
        ]
        .boxed()
    }
}

impl Display for AccessKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(identifier) => Display::fmt(identifier, f),
            Self::Tuple(numeric) => Display::fmt(numeric, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Access {
    pub operator: AccessOperator,
    pub kind: AccessKind,
}

impl Input<&super::Access> for &Access {
    fn assert(self, output: &super::Access) -> TestCaseResult {
        self.operator.assert(output.operator())?;
        self.kind.assert(output.kind())
    }
}

impl Arbitrary for Access {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (AccessOperator::arbitrary_with(()), AccessKind::arbitrary_with(()))
            .prop_map(|(operator, kind)| Self { operator, kind })
            .boxed()
    }
}

impl Display for Access {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.operator, self.kind)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PostfixOperator {
    Copy,
    Call(Call),
    Cast(Cast),
    Access(Access),
}

impl Input<&super::PostfixOperator> for &PostfixOperator {
    fn assert(self, output: &super::PostfixOperator) -> TestCaseResult {
        match (self, output) {
            (PostfixOperator::Copy, super::PostfixOperator::Copy(_)) => Ok(()),
            (
                PostfixOperator::Call(input),
                super::PostfixOperator::Call(output),
            ) => input.assert(output),
            (
                PostfixOperator::Cast(input),
                super::PostfixOperator::Cast(output),
            ) => input.assert(output),
            (
                PostfixOperator::Access(input),
                super::PostfixOperator::Access(output),
            ) => input.assert(output),

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for PostfixOperator {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Copy),
            Call::arbitrary_with(args.0.clone()).prop_map(Self::Call),
            Cast::arbitrary_with(args).prop_map(Self::Cast),
            Access::arbitrary_with(()).prop_map(Self::Access),
        ]
        .boxed()
    }
}

impl Display for PostfixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Copy => f.write_char('\''),
            Self::Call(call) => Display::fmt(call, f),
            Self::Cast(cast) => Display::fmt(cast, f),
            Self::Access(access) => Display::fmt(access, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Postfix {
    pub postfixable: Box<Postfixable>,
    pub operator: PostfixOperator,
}

impl Input<&super::Postfix> for &Postfix {
    fn assert(self, output: &super::Postfix) -> TestCaseResult {
        self.postfixable.assert(output.postfixable())?;
        self.operator.assert(output.operator())
    }
}

impl Arbitrary for Postfix {
    type Parameters = (
        Option<BoxedStrategy<Postfixable>>,
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            args.0.clone().unwrap_or_else(|| {
                Postfixable::arbitrary_with((
                    args.1.clone(),
                    args.2.clone(),
                    args.3.clone(),
                ))
            }),
            PostfixOperator::arbitrary_with((args.1, args.2, args.3)),
        )
            .prop_map(|(postfixable, operator)| Self {
                postfixable: Box::new(postfixable),
                operator,
            })
            .prop_filter("filter ambiguous syntax", |x| {
                !matches!(
                    (&*x.postfixable, &x.operator),
                    (
                        Postfixable::Unit(Unit::Numeric(Numeric {
                            decimal: None,
                            suffix: None,
                            ..
                        })),
                        PostfixOperator::Access(Access {
                            operator: AccessOperator::Dot,
                            kind: AccessKind::Tuple(..)
                        })
                    )
                )
            })
            .boxed()
    }
}

impl Display for Postfix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.postfixable, f)?;
        Display::fmt(&self.operator, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Postfixable {
    Unit(Unit),
    Postfix(Postfix),
}

impl Input<&super::Postfixable> for &Postfixable {
    fn assert(self, output: &super::Postfixable) -> TestCaseResult {
        match (self, output) {
            (Postfixable::Unit(input), super::Postfixable::Unit(output)) => {
                input.assert(output)
            }
            (
                Postfixable::Postfix(input),
                super::Postfixable::Postfix(output),
            ) => input.assert(output),

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Postfixable {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        Unit::arbitrary_with(args.clone())
            .prop_map(Postfixable::Unit)
            .prop_recursive(3, 3, 3, move |inner| {
                Postfix::arbitrary_with((
                    Some(inner),
                    args.0.clone(),
                    args.1.clone(),
                    args.2.clone(),
                ))
                .prop_map(Postfixable::Postfix)
            })
            .boxed()
    }
}

impl Display for Postfixable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit(unit) => Display::fmt(unit, f),
            Self::Postfix(postfix) => Display::fmt(postfix, f),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReferenceOfKind {
    Local,
    Regular,
}

impl Input<&super::ReferenceOfKind> for &ReferenceOfKind {
    fn assert(self, output: &super::ReferenceOfKind) -> TestCaseResult {
        match (self, output) {
            (ReferenceOfKind::Local, super::ReferenceOfKind::Local(_))
            | (ReferenceOfKind::Regular, super::ReferenceOfKind::Regular(_)) => {
                Ok(())
            }

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for ReferenceOfKind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![Just(Self::Local), Just(Self::Regular)].boxed()
    }
}

impl Display for ReferenceOfKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local => f.write_char('@'),
            Self::Regular => f.write_char('&'),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReferenceOf {
    pub kind: ReferenceOfKind,
    pub qualifier: Option<Qualifier>,
}

impl Input<&super::ReferenceOf> for &ReferenceOf {
    fn assert(self, output: &super::ReferenceOf) -> TestCaseResult {
        self.kind.assert(output.kind())?;
        self.qualifier.as_ref().assert(output.qualifier().as_ref())
    }
}

impl Arbitrary for ReferenceOf {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            ReferenceOfKind::arbitrary(),
            proptest::option::of(Qualifier::arbitrary_with(())),
        )
            .prop_map(|(kind, qualifier)| Self { kind, qualifier })
            .boxed()
    }
}

impl Display for ReferenceOf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.kind, f)?;

        if let Some(qualifier) = &self.qualifier {
            write!(f, "{qualifier} ")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrefixOperator {
    LogicalNot,
    Negate,
    BitwiseNot,
    Dereference,
    Local,
    Unlocal,
    ReferenceOf(ReferenceOf),
}

impl Input<&super::PrefixOperator> for &PrefixOperator {
    fn assert(self, output: &super::PrefixOperator) -> TestCaseResult {
        match (self, output) {
            (
                PrefixOperator::LogicalNot,
                super::PrefixOperator::LogicalNot(_),
            )
            | (PrefixOperator::Negate, super::PrefixOperator::Negate(_))
            | (
                PrefixOperator::BitwiseNot,
                super::PrefixOperator::BitwiseNot(_),
            )
            | (
                PrefixOperator::Dereference,
                super::PrefixOperator::Dereference(_),
            )
            | (PrefixOperator::Local, super::PrefixOperator::Local(_))
            | (PrefixOperator::Unlocal, super::PrefixOperator::Unlocal(_)) => {
                Ok(())
            }

            (
                PrefixOperator::ReferenceOf(input),
                super::PrefixOperator::ReferenceOf(output),
            ) => input.assert(output),

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for PrefixOperator {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::LogicalNot),
            Just(Self::Negate),
            Just(Self::BitwiseNot),
            Just(Self::Dereference),
            Just(Self::Local),
            Just(Self::Unlocal),
            ReferenceOf::arbitrary_with(()).prop_map(Self::ReferenceOf),
        ]
        .boxed()
    }
}

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LogicalNot => f.write_char('!'),
            Self::Negate => f.write_char('-'),
            Self::BitwiseNot => f.write_char('~'),
            Self::Dereference => f.write_char('*'),
            Self::Local => f.write_str("local "),
            Self::Unlocal => f.write_str("unlocal "),
            Self::ReferenceOf(reference_of) => Display::fmt(reference_of, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prefix {
    pub prefixable: Box<Prefixable>,
    pub operator: PrefixOperator,
}

impl Input<&super::Prefix> for &Prefix {
    fn assert(self, output: &super::Prefix) -> TestCaseResult {
        self.prefixable.assert(output.prefixable())?;
        self.operator.assert(output.operator())
    }
}

impl Arbitrary for Prefix {
    type Parameters = (
        Option<BoxedStrategy<Prefixable>>,
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            args.0.clone().unwrap_or_else(|| {
                Prefixable::arbitrary_with((
                    args.1.clone(),
                    args.2.clone(),
                    args.3.clone(),
                ))
            }),
            PrefixOperator::arbitrary(),
        )
            .prop_map(|(prefixable, operator)| Self {
                prefixable: Box::new(prefixable),
                operator,
            })
            .boxed()
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.operator, f)?;
        Display::fmt(&self.prefixable, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Prefixable {
    Postfixable(Postfixable),
    Prefix(Prefix),
}

impl Input<&super::Prefixable> for &Prefixable {
    fn assert(self, output: &super::Prefixable) -> TestCaseResult {
        match (self, output) {
            (
                Prefixable::Postfixable(input),
                super::Prefixable::Postfixable(output),
            ) => input.assert(output),
            (Prefixable::Prefix(input), super::Prefixable::Prefix(output)) => {
                input.assert(output)
            }

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Prefixable {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        Postfixable::arbitrary_with(args.clone())
            .prop_map(Prefixable::Postfixable)
            .prop_recursive(3, 3, 3, move |inner| {
                Prefix::arbitrary_with((
                    Some(inner),
                    args.0.clone(),
                    args.1.clone(),
                    args.2.clone(),
                ))
                .prop_map(Prefixable::Prefix)
            })
            .boxed()
    }
}

impl Display for Prefixable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Postfixable(postfixable) => Display::fmt(postfixable, f),
            Self::Prefix(prefix) => Display::fmt(prefix, f),
        }
    }
}

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
    UniqueAssign,
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
                BinaryOperator::UniqueAssign,
                super::BinaryOperator::UniqueAssign(..),
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
            Just(Self::UniqueAssign),
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
            Self::UniqueAssign => f.write_str(":="),
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Binary {
    pub first: Box<Prefixable>,
    pub chain: Vec<(BinaryOperator, Prefixable)>,
}

impl Arbitrary for Binary {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let prefixable = Prefixable::arbitrary_with((args.0, args.1, args.2));
        (
            prefixable.clone(),
            proptest::collection::vec(
                (BinaryOperator::arbitrary(), prefixable),
                0..=3,
            ),
        )
            .prop_map(|(first, chain)| Self { first: Box::new(first), chain })
            .boxed()
    }
}

impl Input<&super::Binary> for &Binary {
    fn assert(self, output: &super::Binary) -> TestCaseResult {
        self.first.assert(output.first())?;
        self.chain.assert(output.chain())?;

        Ok(())
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.first, f)?;

        for (operator, prefixable) in &self.chain {
            write!(f, " {operator} {prefixable}")?;
        }

        Ok(())
    }
}

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 4096,
        ..proptest::test_runner::Config::default()
    })]
    #[allow(clippy::ignored_unit_patterns)]
    #[test]
    fn expression_test(
        expression_input in Expression::arbitrary(),
    ) {
        let source = expression_input.to_string();
        println!("{source}");
        let expression = parse(
            &source,
            |parser, handler| parser.parse_expression(handler)
        )?;

        expression_input.assert(&expression)?;
    }
}

