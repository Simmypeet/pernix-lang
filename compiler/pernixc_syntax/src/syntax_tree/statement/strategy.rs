use std::fmt::{Display, Write};

use enum_as_inner::EnumAsInner;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, Just},
    prop_oneof,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::syntax_tree::{
    expression::strategy::Expression,
    pattern::strategy::Irrefutable,
    r#type::strategy::Type,
    strategy::{
        write_indent_line, write_indent_line_for_indent_display, IndentDisplay,
        Passable, QualifiedIdentifier,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Statement {
    Expression(Expression),
    VariableDeclaration(VariableDeclaration),
}

impl Input<&super::Statement> for &Statement {
    fn assert(self, output: &super::Statement) -> TestCaseResult {
        match (self, output) {
            (
                Statement::VariableDeclaration(i),
                super::Statement::VariableDeclaration(o),
            ) => i.assert(o),
            (Statement::Expression(i), super::Statement::Expression(o)) => {
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
            Expression::arbitrary_with((args.1.clone(), None, None))
                .prop_map(Self::Expression),
            VariableDeclaration::arbitrary_with(args)
                .prop_map(Self::VariableDeclaration),
        ]
        .boxed()
    }
}

impl IndentDisplay for Statement {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::VariableDeclaration(v) => v.indent_fmt(f, indent),
            Self::Expression(e) => e.indent_fmt(f, indent),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableDeclaration {
    pub irrefutable_pattern: Irrefutable,
    pub r#type: Option<Type>,
    pub expression: Expression,
}

impl Input<&super::VariableDeclaration> for &VariableDeclaration {
    fn assert(self, output: &super::VariableDeclaration) -> TestCaseResult {
        self.irrefutable_pattern.assert(&output.irrefutable_pattern)?;
        self.r#type
            .as_ref()
            .assert(output.type_annotation.as_ref().map(|x| &x.r#type))?;
        self.expression.assert(&output.expression)
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
                r#type: ty,
                expression,
            })
            .boxed()
    }
}

impl IndentDisplay for VariableDeclaration {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("let ")?;

        Display::fmt(&self.irrefutable_pattern, f)?;

        if let Some(type_annotation) = &self.r#type {
            f.write_str(": ")?;
            type_annotation.indent_fmt(f, indent)?;
        }

        f.write_str(" = ")?;
        self.expression.indent_fmt(f, indent)?;

        f.write_char(';')
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Statements {
    pub statements: Vec<Passable<Statement>>,
}

impl Input<&super::Statements> for &Statements {
    fn assert(self, output: &super::Statements) -> TestCaseResult {
        self.statements.assert(&output.statements)
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

        proptest::collection::vec(
            prop_oneof![
                8 => statement.prop_map(Passable::SyntaxTree),
                1 => Just(Passable::Pass)
            ],
            1..=10,
        )
        .prop_map(|statements| Self { statements })
        .boxed()
    }
}

impl IndentDisplay for Statements {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        write_indent_line(f, &":", indent)?;
        for statement in &self.statements {
            write_indent_line_for_indent_display(f, statement, indent)?;
        }

        Ok(())
    }
}
