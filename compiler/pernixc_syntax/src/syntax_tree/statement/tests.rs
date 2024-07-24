use std::fmt::{Display, Write};

use pernixc_tests::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::syntax_tree::{
    self,
    expression::tests::{Binary, Brace, Expression, Terminator},
    pattern::tests::Irrefutable,
    r#type::tests::Type,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Statement {
    Expressive(Expressive),
    VariableDeclaration(VariableDeclaration),
}

impl Input<&super::Statement> for &Statement {
    fn assert(self, output: &super::Statement) -> TestCaseResult {
        match (self, output) {
            (
                Statement::VariableDeclaration(i),
                super::Statement::VariableDeclaration(o),
            ) => i.assert(o),
            (Statement::Expressive(i), super::Statement::Expressive(o)) => {
                i.assert(o)
            }
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, found {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Statement {
    type Parameters =
        (Option<BoxedStrategy<Expression>>, Option<BoxedStrategy<Type>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Expressive::arbitrary_with(args.clone()).prop_map(Self::Expressive),
            VariableDeclaration::arbitrary_with(args)
                .prop_map(Self::VariableDeclaration),
        ]
        .boxed()
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VariableDeclaration(v) => Display::fmt(v, f),
            Self::Expressive(e) => Display::fmt(e, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableDeclaration {
    pub irrefutable_pattern: Irrefutable,
    pub ty: Option<Type>,
    pub expression: Expression,
}

impl Input<&super::VariableDeclaration> for &VariableDeclaration {
    fn assert(self, output: &super::VariableDeclaration) -> TestCaseResult {
        self.irrefutable_pattern.assert(output.irrefutable_pattern())?;
        self.ty
            .as_ref()
            .assert(output.type_annotation.as_ref().map(|x| &x.ty))?;
        self.expression.assert(output.expression())
    }
}

impl Arbitrary for VariableDeclaration {
    type Parameters =
        (Option<BoxedStrategy<Expression>>, Option<BoxedStrategy<Type>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expression = args.0.clone().unwrap_or_else(|| {
            Expression::arbitrary_with((args.1.clone(), None, None))
        });

        (
            Irrefutable::arbitrary(),
            proptest::option::of(args.1.unwrap_or_else(|| {
                Type::arbitrary_with((args.0.clone(), None))
            })),
            expression,
        )
            .prop_map(|(irrefutable_pattern, ty, expression)| Self {
                irrefutable_pattern,
                ty,
                expression,
            })
            .boxed()
    }
}

impl Display for VariableDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("let ")?;

        Display::fmt(&self.irrefutable_pattern, f)?;

        if let Some(type_annotation) = &self.ty {
            write!(f, ": {type_annotation}")?;
        }

        write!(f, " = {}", self.expression)?;

        f.write_char(';')
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Expressive {
    Brace(Brace),
    Semi(Semi),
}

impl Input<&super::Expressive> for &Expressive {
    fn assert(self, output: &super::Expressive) -> TestCaseResult {
        match (self, output) {
            (Expressive::Brace(i), super::Expressive::Brace(o)) => i.assert(o),
            (Expressive::Semi(i), super::Expressive::Semi(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, found {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Expressive {
    type Parameters =
        (Option<BoxedStrategy<Expression>>, Option<BoxedStrategy<Type>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expr_strategy = args.0.unwrap_or_else(|| {
            Expression::arbitrary_with((args.1, None, None))
        });

        expr_strategy
            .prop_map(|x| match x {
                Expression::Binary(x) => {
                    Self::Semi(Semi { expression: SemiExpression::Binary(x) })
                }
                Expression::Terminator(x) => Self::Semi(Semi {
                    expression: SemiExpression::Terminator(x),
                }),
                Expression::Brace(x) => Self::Brace(x),
            })
            .boxed()
    }
}

impl Display for Expressive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Brace(t) => Display::fmt(t, f),
            Self::Semi(t) => Display::fmt(t, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum SemiExpression {
    Binary(Binary),
    Terminator(Terminator),
}

impl Input<&super::SemiExpression> for &SemiExpression {
    fn assert(self, output: &super::SemiExpression) -> TestCaseResult {
        match (self, output) {
            (SemiExpression::Binary(i), super::SemiExpression::Binary(o)) => {
                i.assert(o)
            }
            (
                SemiExpression::Terminator(i),
                super::SemiExpression::Terminator(o),
            ) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, found {output:?}",
            ))),
        }
    }
}

impl Display for SemiExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Binary(t) => Display::fmt(t, f),
            Self::Terminator(t) => Display::fmt(t, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Semi {
    pub expression: SemiExpression,
}

impl Input<&super::Semi> for &Semi {
    fn assert(self, output: &super::Semi) -> TestCaseResult {
        self.expression.assert(output.expression())
    }
}

impl Display for Semi {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.expression, f)?;
        f.write_str(";")
    }
}

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls, clippy::ignored_unit_patterns)]
    fn statement(
        statement_input in Statement::arbitrary()
    ) {
        let source = statement_input.to_string();

        let statement = syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_statement(handler)
        )?;

        statement_input.assert(&statement)?;
    }
}
