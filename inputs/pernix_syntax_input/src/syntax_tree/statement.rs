//! Contains the definition of various inputs that correspond to the definitions in defined
//! [`pernixc_syntax::syntax_tree::statement`] module.

use std::fmt::Display;

use pernix_input::Input;
use pernix_lexical_input::token::Identifier;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use super::{
    expression::{Expression, Functional, Imperative, Terminator},
    TypeAnnotation,
};

/// Represents an input for the [`pernixc_syntax::syntax_tree::statement::Statement`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Statement {
    Expressive(Expressive),
    VariableDeclaration(VariableDeclaration),
}

impl Input for Statement {
    type Output = pernixc_syntax::syntax_tree::statement::Statement;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (
                Self::VariableDeclaration(i),
                pernixc_syntax::syntax_tree::statement::Statement::VariableDeclaration(o),
            ) => i.assert(o),
            (
                Self::Expressive(i),
                pernixc_syntax::syntax_tree::statement::Statement::Expressive(o),
            ) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, found {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Statement {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let args = args.unwrap_or_else(Expression::arbitrary);
        prop_oneof![
            VariableDeclaration::arbitrary_with(Some(args.clone()))
                .prop_map(Statement::VariableDeclaration),
            args.prop_map(|expr| match expr {
                Expression::Imperative(a) => Self::Expressive(Expressive::Imperative(a)),
                Expression::Terminator(..) | Expression::Functional(..) => {
                    let expression = match expr {
                        Expression::Terminator(a) => SemiExpression::Terminator(a),
                        Expression::Functional(a) => SemiExpression::Functional(a),
                        Expression::Imperative(..) => unreachable!(),
                    };

                    Self::Expressive(Expressive::Semi(Semi { expression }))
                }
            })
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

/// Represents an input for the [`pernixc_syntax::syntax_tree::statement::VariableDeclaration`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableDeclaration {
    /// Whether the variable is mutable.
    pub mutable: bool,

    /// The identifier of the variable.
    pub identifier: pernix_lexical_input::token::Identifier,

    /// The type specifier of the variable.
    pub type_annotation: Option<TypeAnnotation>,

    /// The initalizer expression of the variable.
    pub expression: Expression,
}

impl Input for VariableDeclaration {
    type Output = pernixc_syntax::syntax_tree::statement::VariableDeclaration;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.mutable, output.mutable_keyword().is_some());
        self.identifier.assert(output.identifier())?;
        self.type_annotation.assert(output.type_annotation())?;
        self.expression.assert(output.expression())
    }
}

impl Arbitrary for VariableDeclaration {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expression = args.unwrap_or_else(Expression::arbitrary);

        (
            proptest::bool::ANY,
            Identifier::arbitrary(),
            proptest::option::of(TypeAnnotation::arbitrary()),
            expression,
        )
            .prop_map(|(mutable, identifier, type_annotation, expression)| Self {
                mutable,
                identifier,
                type_annotation,
                expression,
            })
            .boxed()
    }
}

impl Display for VariableDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("let")?;
        if self.mutable {
            f.write_str(" mutable")?;
        }

        write!(f, " {}", self.identifier)?;

        if let Some(type_annotation) = &self.type_annotation {
            Display::fmt(type_annotation, f)?;
        }

        write!(f, " = {};", self.expression)
    }
}

/// Represents an input for the [`pernixc_syntax::syntax_tree::statement::Expressive`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Expressive {
    Imperative(Imperative),
    Semi(Semi),
}

impl Input for Expressive {
    type Output = pernixc_syntax::syntax_tree::statement::Expressive;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (
                Self::Imperative(i),
                pernixc_syntax::syntax_tree::statement::Expressive::Imperative(o),
            ) => i.assert(o),
            (Self::Semi(i), pernixc_syntax::syntax_tree::statement::Expressive::Semi(o)) => {
                i.assert(o)
            }
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, found {output:?}",
            ))),
        }
    }
}

impl Display for Expressive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Imperative(t) => Display::fmt(t, f),
            Self::Semi(t) => Display::fmt(t, f),
        }
    }
}

/// Represents an input for the [`pernixc_syntax::syntax_tree::statement::SemiExpression`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum SemiExpression {
    Functional(Functional),
    Terminator(Terminator),
}

impl Input for SemiExpression {
    type Output = pernixc_syntax::syntax_tree::statement::SemiExpression;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (
                Self::Functional(i),
                pernixc_syntax::syntax_tree::statement::SemiExpression::Functional(o),
            ) => i.assert(o),
            (
                Self::Terminator(i),
                pernixc_syntax::syntax_tree::statement::SemiExpression::Terminator(o),
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
            Self::Functional(t) => Display::fmt(t, f),
            Self::Terminator(t) => Display::fmt(t, f),
        }
    }
}

/// Represents an input for the [`pernixc_syntax::syntax_tree::statement::Semi`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Semi {
    /// The expression of the semi-statement.
    pub expression: SemiExpression,
}

impl Input for Semi {
    type Output = pernixc_syntax::syntax_tree::statement::Semi;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.expression.assert(output.expression())
    }
}

impl Display for Semi {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.expression, f)?;
        f.write_str(";")
    }
}
