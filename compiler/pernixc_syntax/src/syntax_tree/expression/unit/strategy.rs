use std::fmt::{Display, Write};

use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy, TestCaseError},
    prop_assert_eq, prop_oneof,
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    self,
    expression::strategy::Expression,
    r#type::strategy::Type,
    strategy::{
        ConnectedList, ConstantPunctuation, Identifier, IndentDisplay,
        QualifiedIdentifier,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Unit {
    Boolean(Boolean),
    Numeric(Numeric),
    QualifiedIdentifier(QualifiedIdentifier),
    Parenthesized(Parenthesized),
    Struct(Struct),
    Array(Array),
    Phantom(Phantom),
    Panic,
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
            Just(Self::Panic)
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

            (Unit::Panic, super::Unit::Panic(_)) => Ok(()),

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl IndentDisplay for Unit {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Boolean(boolean_literal) => Display::fmt(boolean_literal, f),
            Self::Numeric(numeric_literal) => Display::fmt(numeric_literal, f),
            Self::QualifiedIdentifier(qualified_identifier) => {
                qualified_identifier.indent_fmt(f, indent)
            }
            Self::Parenthesized(parenthesized) => {
                parenthesized.indent_fmt(f, indent)
            }
            Self::Struct(struct_literal) => {
                struct_literal.indent_fmt(f, indent)
            }
            Self::Array(array_literal) => array_literal.indent_fmt(f, indent),
            Self::Phantom(phantom) => Display::fmt(phantom, f),
            Self::Panic => f.write_str("panic"),
        }
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
    pub numeric: syntax_tree::strategy::Numeric,
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
        syntax_tree::strategy::Numeric::arbitrary_with(())
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
    pub numeric: syntax_tree::strategy::Numeric,
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
            syntax_tree::strategy::Numeric::arbitrary_with(()),
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
        prop_assert_eq!(self.ellipsis, output.ellipsis.is_some());
        self.expression.assert(&output.expression)
    }
}

impl IndentDisplay for Unpackable {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if self.ellipsis {
            f.write_str("...")?;
        }

        self.expression.indent_fmt(f, indent)
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
        self.expressions.as_ref().assert(output.connected_list.as_ref())
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

impl IndentDisplay for Parenthesized {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "(")?;

        if let Some(expressions) = &self.expressions {
            expressions.indent_fmt(f, indent)?;
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
        self.identifier.assert(&output.identifier)?;
        self.expression.assert(&output.expression)
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

impl IndentDisplay for FieldInitializer {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.identifier.fmt(f)?;
        f.write_str(": ")?;
        self.expression.indent_fmt(f, indent)
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
        self.qualified_identifier.assert(&output.qualified_identifier)?;
        self.field_initializers
            .as_ref()
            .assert(output.field_initializers.connected_list.as_ref())
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

impl IndentDisplay for Struct {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.qualified_identifier.indent_fmt(f, indent)?;

        f.write_char('{')?;
        if let Some(field_initializers) = &self.field_initializers {
            field_initializers.indent_fmt(f, indent)?;
        }
        f.write_char('}')?;

        Ok(())
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
pub struct Array {
    pub expressions:
        Option<ConnectedList<Box<Expression>, ConstantPunctuation<','>>>,
}

impl Input<&super::Array> for &Array {
    fn assert(self, output: &super::Array) -> TestCaseResult {
        self.expressions
            .as_ref()
            .assert(output.arguments.connected_list.as_ref())
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

impl IndentDisplay for Array {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_char('[')?;

        if let Some(expressions) = &self.expressions {
            expressions.indent_fmt(f, indent)?;
        }

        f.write_char(']')
    }
}
