use std::fmt::Write;

use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy, TestCaseError},
    prop_oneof,
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    expression::{binary::strategy::Binary, strategy::Expression},
    r#type::strategy::Type,
    statement::strategy::Statement,
    strategy::{IndentDisplay, Label, QualifiedIdentifier},
};

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
        Option<BoxedStrategy<Statement>>,
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

impl IndentDisplay for Terminator {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Return(return_) => return_.indent_fmt(f, indent),
            Self::Continue(continue_) => continue_.indent_fmt(f, indent),
            Self::Express(express) => express.indent_fmt(f, indent),
            Self::Break(break_) => break_.indent_fmt(f, indent),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Return {
    pub binary: Option<Binary>,
}

impl Input<&super::Return> for &Return {
    fn assert(self, output: &super::Return) -> TestCaseResult {
        self.binary.as_ref().assert(output.binary.as_ref())
    }
}

impl Arbitrary for Return {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::option::of(Binary::arbitrary_with(args))
            .prop_map(|expression| Self { binary: expression })
            .boxed()
    }
}

impl IndentDisplay for Return {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("return")?;

        if let Some(expression) = &self.binary {
            f.write_char(' ')?;
            expression.indent_fmt(f, indent)?;
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
        self.label.as_ref().assert(output.label.as_ref())
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

impl IndentDisplay for Continue {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _indent: usize,
    ) -> std::fmt::Result {
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
        self.label.as_ref().assert(output.label.as_ref())?;
        self.binary.as_ref().assert(output.binary.as_ref())
    }
}

impl Arbitrary for Express {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
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

impl IndentDisplay for Express {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("express")?;

        if let Some(label) = &self.label {
            write!(f, " {label}")?;
        }

        if let Some(binary) = &self.binary {
            f.write_char(' ')?;
            binary.indent_fmt(f, indent)?;
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
        self.label.as_ref().assert(output.label.as_ref())?;
        self.binary.as_ref().assert(output.binary.as_ref())
    }
}

impl Arbitrary for Break {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
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

impl IndentDisplay for Break {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("break")?;

        if let Some(label) = &self.label {
            write!(f, " {label}")?;
        }

        if let Some(binary) = &self.binary {
            f.write_char(' ')?;
            binary.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}
