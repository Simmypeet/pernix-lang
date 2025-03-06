use std::fmt::{Display, Write};

use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy, TestCaseError},
    prop_oneof,
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    self,
    expression::{
        strategy::Expression,
        unit::strategy::{Numeric, Unit},
    },
    r#type::strategy::Type,
    strategy::{
        ConnectedList, ConstantPunctuation, GenericIdentifier, IndentDisplay,
        QualifiedIdentifier,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Postfix {
    pub postfixable: Box<Postfixable>,
    pub operator: PostfixOperator,
}

impl Input<&super::Postfix> for &Postfix {
    fn assert(self, output: &super::Postfix) -> TestCaseResult {
        self.postfixable.assert(&output.postfixable)?;
        self.operator.assert(&output.operator)
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

impl IndentDisplay for Postfix {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.postfixable.indent_fmt(f, indent)?;
        self.operator.indent_fmt(f, indent)
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

impl IndentDisplay for Postfixable {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Unit(unit) => unit.indent_fmt(f, indent),
            Self::Postfix(postfix) => postfix.indent_fmt(f, indent),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PostfixOperator {
    Call(Call),
    Cast(Cast),
    Access(Access),
}

impl Input<&super::PostfixOperator> for &PostfixOperator {
    fn assert(self, output: &super::PostfixOperator) -> TestCaseResult {
        match (self, output) {
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
            Call::arbitrary_with(args.0.clone()).prop_map(Self::Call),
            Access::arbitrary_with((args.0.clone(), args.1.clone()))
                .prop_map(Self::Access),
            Cast::arbitrary_with(args).prop_map(Self::Cast),
        ]
        .boxed()
    }
}

impl IndentDisplay for PostfixOperator {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Call(call) => call.indent_fmt(f, indent),
            Self::Cast(cast) => cast.indent_fmt(f, indent),
            Self::Access(access) => access.indent_fmt(f, indent),
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
        self.arguments.as_ref().assert(output.arguments.connected_list.as_ref())
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

impl IndentDisplay for Call {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_char('(')?;

        if let Some(arguments) = &self.arguments {
            arguments.indent_fmt(f, indent)?;
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
        self.r#type.assert(&output.r#type)
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

impl IndentDisplay for Cast {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str(" as ")?;
        self.r#type.indent_fmt(f, indent)
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
    GenericIdentifier(GenericIdentifier),
    Tuple(bool, syntax_tree::strategy::Numeric),
    Index(Box<Expression>),
}

impl Input<&super::AccessKind> for &AccessKind {
    fn assert(self, output: &super::AccessKind) -> TestCaseResult {
        match (self, output) {
            (
                AccessKind::GenericIdentifier(input),
                super::AccessKind::GenericIdentifier(output),
            ) => input.assert(output),

            (
                AccessKind::Tuple(is_neg, input),
                super::AccessKind::Tuple(output),
            ) => {
                assert_eq!(*is_neg, output.minus.is_some());
                input.assert(&output.index)
            }

            (AccessKind::Index(input), super::AccessKind::Index(output)) => {
                input.assert(&output.expression.tree)
            }

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for AccessKind {
    type Parameters =
        (Option<BoxedStrategy<Expression>>, Option<BoxedStrategy<Type>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(strat: Self::Parameters) -> Self::Strategy {
        let expr_strat = strat.0.unwrap_or_else(Expression::arbitrary);
        prop_oneof![
            GenericIdentifier::arbitrary_with((
                strat.1,
                Some(expr_strat.clone()),
            ))
            .prop_map(Self::GenericIdentifier),
            (proptest::bool::ANY, syntax_tree::strategy::Numeric::arbitrary())
                .prop_map(|(is_neg, index)| Self::Tuple(is_neg, index)),
            expr_strat.prop_map(Box::new).prop_map(Self::Index),
        ]
        .boxed()
    }
}

impl IndentDisplay for AccessKind {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::GenericIdentifier(identifier) => {
                identifier.indent_fmt(f, indent)
            }
            Self::Tuple(is_neg, numeric) => {
                if *is_neg {
                    f.write_char('-')?;
                }

                Display::fmt(numeric, f)
            }
            Self::Index(expression) => {
                f.write_char('[')?;
                expression.indent_fmt(f, indent)?;
                f.write_char(']')
            }
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
        self.operator.assert(&output.operator)?;
        self.kind.assert(&output.kind)
    }
}

impl Arbitrary for Access {
    type Parameters =
        (Option<BoxedStrategy<Expression>>, Option<BoxedStrategy<Type>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(strat: Self::Parameters) -> Self::Strategy {
        (AccessOperator::arbitrary_with(()), AccessKind::arbitrary_with(strat))
            .prop_map(|(operator, kind)| Self { operator, kind })
            .boxed()
    }
}

impl IndentDisplay for Access {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.operator.fmt(f)?;
        self.kind.indent_fmt(f, indent)
    }
}
