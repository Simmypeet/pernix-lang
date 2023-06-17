//! Contains the definition of various inputs that correspond to the definitions in defined
//! [`crate::syntax_tree::expression`] module.

use std::fmt::{Display, Write};

use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::{self, input::Identifier, KeywordKind};
use pernixc_system::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::syntax_tree::{
    input::{ConnectedList, ConstantPunctuation, QualifiedIdentifier, TypeSpecifier},
    statement::input::Statement,
};

/// Represents an input for the [`super::NumericLiteral`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NumericLiteral {
    /// The token representing the numeric literal.
    pub numeric_literal_token: token::input::NumericLiteral,
}

impl Arbitrary for NumericLiteral {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        token::input::NumericLiteral::arbitrary()
            .prop_map(|x| Self {
                numeric_literal_token: x,
            })
            .boxed()
    }
}

impl Input for NumericLiteral {
    type Output = super::NumericLiteral;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.numeric_literal_token
            .assert(&output.numeric_literal_token)
    }
}

impl Display for NumericLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.numeric_literal_token, f)
    }
}

/// Represents an input for the [`super::BooleanLiteral`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BooleanLiteral {
    /// The value of the boolean literal input.
    pub boolean_value: bool,
}

impl Arbitrary for BooleanLiteral {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        proptest::bool::ANY
            .prop_map(|x| Self { boolean_value: x })
            .boxed()
    }
}

impl Input for BooleanLiteral {
    type Output = super::BooleanLiteral;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self.boolean_value, output) {
            (true, super::BooleanLiteral::True(k)) => prop_assert_eq!(k.keyword, KeywordKind::True),
            (false, super::BooleanLiteral::False(k)) => {
                prop_assert_eq!(k.keyword, KeywordKind::False);
            }
            _ => {
                return Err(TestCaseError::fail(format!(
                    "expected boolean literal to be {:?}, found {:?}",
                    self.boolean_value, output
                )))
            }
        }

        Ok(())
    }
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.boolean_value {
            f.write_str("true")
        } else {
            f.write_str("false")
        }
    }
}

/// Represents an input for the [`super::Named`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Named {
    /// The qualified identifier representing the name of the named expression.
    pub qualified_identifier: QualifiedIdentifier,
}

impl Input for Named {
    type Output = super::Named;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.qualified_identifier
            .assert(&output.qualified_identifier)
    }
}

impl Display for Named {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.qualified_identifier, f)
    }
}

impl Arbitrary for Named {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        QualifiedIdentifier::arbitrary()
            .prop_map(|x| Self {
                qualified_identifier: x,
            })
            .boxed()
    }
}

/// Represents an input for the [`super::PrefixOperator`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum PrefixOperator {
    LogicalNot,
    Negate,
    ReferenceOf,
    Dereference,
}

impl Arbitrary for PrefixOperator {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::LogicalNot),
            Just(Self::Negate),
            Just(Self::ReferenceOf),
            Just(Self::Dereference),
        ]
        .boxed()
    }
}

impl Input for PrefixOperator {
    type Output = super::PrefixOperator;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::LogicalNot, super::PrefixOperator::LogicalNot(k)) => {
                prop_assert_eq!(k.punctuation, '!');
            }
            (Self::Negate, super::PrefixOperator::Negate(k)) => {
                prop_assert_eq!(k.punctuation, '-');
            }
            (Self::ReferenceOf, super::PrefixOperator::ReferenceOf(k)) => {
                prop_assert_eq!(k.punctuation, '&');
            }
            (Self::Dereference, super::PrefixOperator::Dereference(k)) => {
                prop_assert_eq!(k.punctuation, '*');
            }
            _ => {
                return Err(TestCaseError::fail(format!(
                    "expected prefix operator to be {self:?}, found {output:?}",
                )))
            }
        }

        Ok(())
    }
}

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LogicalNot => f.write_char('!'),
            Self::Negate => f.write_char('-'),
            Self::ReferenceOf => f.write_char('&'),
            Self::Dereference => f.write_char('*'),
        }
    }
}

/// Represents an input for the [`super::Prefix`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prefix {
    /// The prefix operator.
    pub operator: PrefixOperator,

    /// The operand of the prefix operator.
    pub operand: Box<Functional>,
}

impl Input for Prefix {
    type Output = super::Prefix;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.operator.assert(&output.operator)?;
        self.operand.assert(&output.operand)
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.operator, f)?;
        Display::fmt(&self.operand, f)
    }
}

impl Arbitrary for Prefix {
    type Parameters = Option<BoxedStrategy<Functional>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let prefix = args
            .unwrap_or_else(|| filter_functional_variant(Expression::arbitrary()))
            .prop_filter("filter out binary", |x| {
                !matches!(x, Functional::Binary(..))
            });

        // TODO: Filter out binary variants.
        (prefix, PrefixOperator::arbitrary())
            .prop_map(|(operand, operator)| Self {
                operator,
                operand: Box::new(operand),
            })
            .boxed()
    }
}

/// Represents an input for the [`super::MemberAccess`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberAccess {
    /// The operand of the member access.
    pub operand: Box<Functional>,

    /// The member name.
    pub identifier: Identifier,
}

impl Input for MemberAccess {
    type Output = super::MemberAccess;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.operand.assert(&output.operand)?;
        self.identifier.assert(&output.identifier)
    }
}

impl Display for MemberAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.operand, f)?;
        f.write_char('.')?;
        Display::fmt(&self.identifier, f)
    }
}

impl Arbitrary for MemberAccess {
    type Parameters = Option<BoxedStrategy<Functional>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let operand = args
            .unwrap_or_else(|| filter_functional_variant(Expression::arbitrary()))
            .prop_filter("filter out `prefix` and `binary`", |x| {
                !matches!(x, Functional::Prefix(..) | Functional::Binary(..))
            });

        (operand, Identifier::arbitrary())
            .prop_map(|(operand, identifier)| Self {
                operand: Box::new(operand),
                identifier,
            })
            .boxed()
    }
}

/// Represents an input for the [`super::BinaryOperator`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
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
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    LogicalAnd,
    LogicalOr,
}

impl Input for BinaryOperator {
    type Output = super::BinaryOperator;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Add, super::BinaryOperator::Add(k)) => {
                prop_assert_eq!(k.punctuation, '+');
            }
            (Self::Subtract, super::BinaryOperator::Subtract(k)) => {
                prop_assert_eq!(k.punctuation, '-');
            }
            (Self::Multiply, super::BinaryOperator::Multiply(k)) => {
                prop_assert_eq!(k.punctuation, '*');
            }
            (Self::Divide, super::BinaryOperator::Divide(k)) => {
                prop_assert_eq!(k.punctuation, '/');
            }
            (Self::Modulo, super::BinaryOperator::Modulo(k)) => {
                prop_assert_eq!(k.punctuation, '%');
            }
            (Self::Assign, super::BinaryOperator::Assign(k)) => {
                prop_assert_eq!(k.punctuation, '=');
            }
            (Self::CompoundAdd, super::BinaryOperator::CompoundAdd(f, s)) => {
                prop_assert_eq!(f.punctuation, '+');
                prop_assert_eq!(s.punctuation, '=');
            }
            (Self::CompoundSubtract, super::BinaryOperator::CompoundSubtract(f, s)) => {
                prop_assert_eq!(f.punctuation, '-');
                prop_assert_eq!(s.punctuation, '=');
            }
            (Self::CompoundMultiply, super::BinaryOperator::CompoundMultiply(f, s)) => {
                prop_assert_eq!(f.punctuation, '*');
                prop_assert_eq!(s.punctuation, '=');
            }
            (Self::CompoundDivide, super::BinaryOperator::CompoundDivide(f, s)) => {
                prop_assert_eq!(f.punctuation, '/');
                prop_assert_eq!(s.punctuation, '=');
            }
            (Self::CompoundModulo, super::BinaryOperator::CompoundModulo(f, s)) => {
                prop_assert_eq!(f.punctuation, '%');
                prop_assert_eq!(s.punctuation, '=');
            }
            (Self::Equal, super::BinaryOperator::Equal(f, s)) => {
                prop_assert_eq!(f.punctuation, '=');
                prop_assert_eq!(s.punctuation, '=');
            }
            (Self::NotEqual, super::BinaryOperator::NotEqual(f, s)) => {
                prop_assert_eq!(f.punctuation, '!');
                prop_assert_eq!(s.punctuation, '=');
            }
            (Self::GreaterThan, super::BinaryOperator::GreaterThan(f)) => {
                prop_assert_eq!(f.punctuation, '>');
            }
            (Self::GreaterThanOrEqual, super::BinaryOperator::GreaterThanOrEqual(f, s)) => {
                prop_assert_eq!(f.punctuation, '>');
                prop_assert_eq!(s.punctuation, '=');
            }
            (Self::LessThan, super::BinaryOperator::LessThan(f)) => {
                prop_assert_eq!(f.punctuation, '<');
            }
            (Self::LessThanOrEqual, super::BinaryOperator::LessThanOrEqual(f, s)) => {
                prop_assert_eq!(f.punctuation, '<');
                prop_assert_eq!(s.punctuation, '=');
            }
            (Self::LogicalAnd, super::BinaryOperator::LogicalAnd(k)) => {
                prop_assert_eq!(k.keyword, KeywordKind::And);
            }
            (Self::LogicalOr, super::BinaryOperator::LogicalOr(k)) => {
                prop_assert_eq!(k.keyword, KeywordKind::Or);
            }
            _ => {
                return Err(TestCaseError::fail(format!(
                    "expected `{self:?}`, found `{output:?}`",
                )))
            }
        }

        Ok(())
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
            Self::Assign => write!(f, "="),
            Self::CompoundAdd => write!(f, "+="),
            Self::CompoundSubtract => write!(f, "-="),
            Self::CompoundMultiply => write!(f, "*="),
            Self::CompoundDivide => write!(f, "/="),
            Self::CompoundModulo => write!(f, "%="),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::GreaterThan => write!(f, ">"),
            Self::GreaterThanOrEqual => write!(f, ">="),
            Self::LessThan => write!(f, "<"),
            Self::LessThanOrEqual => write!(f, "<="),
            Self::LogicalAnd => write!(f, "and"),
            Self::LogicalOr => write!(f, "or"),
        }
    }
}

impl Arbitrary for BinaryOperator {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
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
            Just(Self::GreaterThan),
            Just(Self::GreaterThanOrEqual),
            Just(Self::LessThan),
            Just(Self::LessThanOrEqual),
            Just(Self::LogicalAnd),
            Just(Self::LogicalOr),
        ]
        .boxed()
    }
}

impl BinaryOperator {
    /// Returns `true` if the operator is assignment (including compound assignment)
    #[must_use]
    pub fn is_assignment(&self) -> bool {
        matches!(
            self,
            Self::Assign
                | Self::CompoundAdd
                | Self::CompoundSubtract
                | Self::CompoundMultiply
                | Self::CompoundDivide
                | Self::CompoundModulo
        )
    }

    /// Gets the precedence of the operator (the higher the number, the first it will be evaluated)
    ///
    /// The least operator has precedence 1.
    #[must_use]
    pub fn get_precedence(&self) -> u32 {
        match self {
            Self::Assign
            | Self::CompoundAdd
            | Self::CompoundSubtract
            | Self::CompoundMultiply
            | Self::CompoundDivide
            | Self::CompoundModulo => 1,
            Self::LogicalOr => 2,
            Self::LogicalAnd => 3,
            Self::Equal | Self::NotEqual => 4,
            Self::LessThan
            | Self::LessThanOrEqual
            | Self::GreaterThan
            | Self::GreaterThanOrEqual => 5,
            Self::Add | Self::Subtract => 6,
            Self::Multiply | Self::Divide | Self::Modulo => 7,
        }
    }
}

/// Represents an input for the [`super::Binary`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Binary {
    /// The left operand of the binary expression.
    pub left_operand: Box<Functional>,

    /// The operator of the binary expression.
    pub operator: BinaryOperator,

    /// The right operand of the binary expression.
    pub right_operand: Box<Functional>,
}

impl Input for Binary {
    type Output = super::Binary;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.operator.assert(&output.operator)?;

        self.left_operand.assert(&output.left_operand)?;
        self.right_operand.assert(&output.right_operand)?;

        Ok(())
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.left_operand, self.operator, self.right_operand
        )
    }
}

impl Arbitrary for Binary {
    type Parameters = Option<BoxedStrategy<Functional>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        fn create_binary(
            operator: BinaryOperator,
            binary_input: Binary,
            functional_input: Functional,
            swap: bool,
        ) -> Option<Binary> {
            match operator
                .get_precedence()
                .cmp(&binary_input.operator.get_precedence())
            {
                std::cmp::Ordering::Less => {
                    let mut left_operand = Box::new(Functional::Binary(binary_input));
                    let mut right_operand = Box::new(functional_input);

                    if swap {
                        std::mem::swap(&mut left_operand, &mut right_operand);
                    }

                    Some(Binary {
                        left_operand,
                        operator,
                        right_operand,
                    })
                }
                std::cmp::Ordering::Equal => {
                    let mut left_operand = Box::new(Functional::Binary(binary_input));
                    let mut right_operand = Box::new(functional_input);

                    if operator.is_assignment() {
                        std::mem::swap(&mut left_operand, &mut right_operand);
                    }

                    Some(Binary {
                        left_operand,
                        operator,
                        right_operand,
                    })
                }
                std::cmp::Ordering::Greater => None,
            }
        }
        let functional_strategy =
            args.unwrap_or_else(|| filter_functional_variant(Expression::arbitrary()));

        (
            functional_strategy.clone(),
            BinaryOperator::arbitrary(),
            functional_strategy,
        )
            .prop_filter_map("disambiguate the syntax", |(left, operator, right)| match (
                left, right,
            ) {
                (Functional::Binary(left_operand), Functional::Binary(right_operand)) => {
                    if operator.get_precedence() >= left_operand.operator.get_precedence()
                        || operator.get_precedence() >= right_operand.operator.get_precedence()
                    {
                        None
                    } else {
                        Some(Self {
                            left_operand: Box::new(Functional::Binary(left_operand)),
                            operator,
                            right_operand: Box::new(Functional::Binary(right_operand)),
                        })
                    }
                }
                (Functional::Binary(left_operand), right_operand) => {
                    create_binary(operator, left_operand, right_operand, false)
                }
                (left_operand, Functional::Binary(right_operand)) => {
                    create_binary(operator, right_operand, left_operand, true)
                }
                (left_operand, right_operand) => Some(Self {
                    left_operand: Box::new(left_operand),
                    operator,
                    right_operand: Box::new(right_operand),
                }),
            })
            .boxed()
    }
}

/// Represents an input for the [`super::FunctionCall`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionCall {
    /// The name of the function.
    pub qualified_identifier: QualifiedIdentifier,

    /// The arguments of the function.
    pub arguments: Option<ConnectedList<Box<Expression>, ConstantPunctuation<','>>>,
}

impl Input for FunctionCall {
    type Output = super::FunctionCall;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.qualified_identifier
            .assert(&output.qualified_identifier)?;

        self.arguments.assert(&output.arguments)?;

        Ok(())
    }
}

impl Display for FunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.qualified_identifier)?;

        if let Some(arguments) = &self.arguments {
            write!(f, "{arguments}")?;
        }

        write!(f, ")")
    }
}

impl Arbitrary for FunctionCall {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expression_strategy = args.unwrap_or_else(Expression::arbitrary);
        (
            QualifiedIdentifier::arbitrary(),
            proptest::option::of(ConnectedList::arbitrary_with(
                expression_strategy.prop_map(Box::new),
                ConstantPunctuation::arbitrary(),
            )),
        )
            .prop_map(|(qualified_identifier, arguments)| Self {
                qualified_identifier,
                arguments,
            })
            .boxed()
    }
}

/// Represents an input for the [`super::Parenthesized`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parenthesized {
    /// The expression inside the parentheses.
    pub expression: Box<Expression>,
}

impl Input for Parenthesized {
    type Output = super::Parenthesized;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.expression.assert(&output.expression)
    }
}

impl Arbitrary for Parenthesized {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expression_strategy = args.unwrap_or_else(Expression::arbitrary);

        expression_strategy
            .prop_map(|expression| Self {
                expression: Box::new(expression),
            })
            .boxed()
    }
}

impl Display for Parenthesized {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.expression)
    }
}

/// Represents an input for the [`super::FieldInitializer`]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldInitializer {
    /// The name of the field.
    pub identifier: Identifier,

    /// The expression of the field.
    pub expression: Box<Expression>,
}

impl Input for FieldInitializer {
    type Output = super::FieldInitializer;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
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

impl Display for FieldInitializer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.identifier, self.expression)
    }
}

/// Represents an input for the [`super::StructLiteral`]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructLiteral {
    /// The name of the struct
    pub qualified_identifier: QualifiedIdentifier,

    /// The field initializers of the struct.
    pub field_initializers: Option<ConnectedList<FieldInitializer, ConstantPunctuation<','>>>,
}

impl Input for StructLiteral {
    type Output = super::StructLiteral;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.qualified_identifier
            .assert(&output.qualified_identifier)?;
        self.field_initializers.assert(&output.field_initializers)
    }
}

impl Arbitrary for StructLiteral {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expression_strategy = args.unwrap_or_else(Expression::arbitrary);
        (
            QualifiedIdentifier::arbitrary(),
            proptest::option::of(ConnectedList::arbitrary_with(
                FieldInitializer::arbitrary_with(Some(expression_strategy)),
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

impl Display for StructLiteral {
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

/// Represents an input for the [`super::Cast`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cast {
    /// The expression to cast.
    pub operand: Box<Functional>,

    /// The type to cast to.
    pub type_specifier: TypeSpecifier,
}

impl Input for Cast {
    type Output = super::Cast;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.operand.assert(&output.operand)?;
        self.type_specifier.assert(&output.type_specifier)
    }
}

impl Arbitrary for Cast {
    type Parameters = Option<BoxedStrategy<Functional>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let operand = args
            .unwrap_or_else(|| filter_functional_variant(Expression::arbitrary()))
            .prop_filter("filter out `prefix` and `binary`", |x| {
                !matches!(x, Functional::Prefix(..) | Functional::Binary(..))
            });

        (operand, TypeSpecifier::arbitrary())
            .prop_map(|(operand, type_specifier)| Self {
                operand: Box::new(operand),
                type_specifier,
            })
            .boxed()
    }
}

impl Display for Cast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} as ({})", self.operand, self.type_specifier)
    }
}

/// Represents an input for the [`super::Label`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label {
    /// The identifier of the label.
    pub identifier: Identifier,
}

impl Input for Label {
    type Output = super::Label;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(&output.identifier)
    }
}

impl Arbitrary for Label {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        Identifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.identifier)
    }
}

/// Represents an input for the [`super::LabelSpecifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LabelSpecifier {
    /// The identifier of the label.
    pub label: Label,
}

impl Input for LabelSpecifier {
    type Output = super::LabelSpecifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult { self.label.assert(&output.label) }
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

/// Represents an input for the [`super::BlockWithoutLabel`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockWithoutLabel {
    /// List of statements in the block.
    pub statements: Vec<Statement>,
}

impl Input for BlockWithoutLabel {
    type Output = super::BlockWithoutLabel;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.statements.len(), output.statements.len());

        for (input, output) in self.statements.iter().zip(output.statements.iter()) {
            input.assert(output)?;
        }

        Ok(())
    }
}

impl Arbitrary for BlockWithoutLabel {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let statement = Statement::arbitrary_with(args);

        proptest::collection::vec(statement, 0..=8)
            .prop_map(|statements| Self { statements })
            .boxed()
    }
}

impl Display for BlockWithoutLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('{')?;
        for statement in &self.statements {
            Display::fmt(statement, f)?;
        }
        f.write_char('}')
    }
}

/// Represents an input for the [`super::Block`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Block {
    /// The optional label specifier.
    pub label_specifier: Option<LabelSpecifier>,

    /// The list of statements in the block.
    pub block_without_label: BlockWithoutLabel,
}

impl Input for Block {
    type Output = super::Block;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.label_specifier.assert(&output.label_specifier)?;
        self.block_without_label.assert(&output.block_without_label)
    }
}

impl Arbitrary for Block {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(LabelSpecifier::arbitrary()),
            BlockWithoutLabel::arbitrary_with(args),
        )
            .prop_map(|(label_specifier, block_without_label)| Self {
                label_specifier,
                block_without_label,
            })
            .boxed()
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(label_specifier) = &self.label_specifier {
            Display::fmt(label_specifier, f)?;
        }

        Display::fmt(&self.block_without_label, f)
    }
}

/// Represents an input for the [`super::Loop`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loop {
    /// The optional label specifier.
    pub label_specifier: Option<LabelSpecifier>,

    /// The list of statements in the loop.
    pub block_without_label: BlockWithoutLabel,
}

impl Input for Loop {
    type Output = super::Loop;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.label_specifier.assert(&output.label_specifier)?;
        self.block_without_label.assert(&output.block_without_label)
    }
}

impl Arbitrary for Loop {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(LabelSpecifier::arbitrary()),
            BlockWithoutLabel::arbitrary_with(args),
        )
            .prop_map(|(label_specifier, block_without_label)| Self {
                label_specifier,
                block_without_label,
            })
            .boxed()
    }
}

impl Display for Loop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(label_specifier) = &self.label_specifier {
            Display::fmt(label_specifier, f)?;
        }

        f.write_str("loop ")?;

        Display::fmt(&self.block_without_label, f)
    }
}

/// Represents an input for the [`super::IfElse`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IfElse {
    /// The condition of the if-else statement.
    pub condition: Box<Expression>,

    /// The then block of the if-else statement.
    pub then_expression: Block,

    /// The else block of the if-else statement.
    pub else_expression: Option<Else>,
}

impl Arbitrary for IfElse {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expression = args.unwrap_or_else(Expression::arbitrary);

        let leaf = (
            expression.clone(),
            Block::arbitrary_with(Some(expression.clone())),
        )
            .prop_map(|(condition, then_expression)| Self {
                condition: Box::new(condition),
                then_expression,
                else_expression: None,
            });

        leaf.prop_recursive(4, 16, 4, move |inner| {
            (
                expression.clone(),
                Block::arbitrary_with(Some(expression.clone())),
                proptest::option::of(prop_oneof![
                    Block::arbitrary_with(Some(expression.clone())).prop_map(|x| Else {
                        expression: Box::new(BlockOrIfElse::Block(x))
                    }),
                    inner.prop_map(|x| Else {
                        expression: Box::new(BlockOrIfElse::IfElse(x))
                    })
                ]),
            )
                .prop_map(|(condition, then_expression, else_expression)| Self {
                    condition: Box::new(condition),
                    then_expression,
                    else_expression,
                })
        })
        .boxed()
    }
}

impl Input for IfElse {
    type Output = super::IfElse;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.condition.assert(&output.condition)?;
        self.then_expression.assert(&output.then_expression)?;
        self.else_expression.assert(&output.else_expression)
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

/// Represents an input for the [`super::BlockOrIfElse`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum BlockOrIfElse {
    Block(Block),
    IfElse(IfElse),
}

impl Input for BlockOrIfElse {
    type Output = super::BlockOrIfElse;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Block(i), super::BlockOrIfElse::Block(o)) => i.assert(o),
            (Self::IfElse(i), super::BlockOrIfElse::IfElse(o)) => i.assert(o),
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

/// Represents an input for the [`super::Else`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Else {
    /// The content of the else statement.
    expression: Box<BlockOrIfElse>,
}

impl Input for Else {
    type Output = super::Else;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.expression.assert(&output.expression)
    }
}

impl Display for Else {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("else ")?;

        Display::fmt(&self.expression, f)
    }
}

/// Represents an input for the [`super::Imperative`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Imperative {
    Block(Block),
    Loop(Loop),
    IfElse(IfElse),
}

impl Input for Imperative {
    type Output = super::Imperative;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Block(i), super::Imperative::Block(o)) => i.assert(o),
            (Self::Loop(i), super::Imperative::Loop(o)) => i.assert(o),
            (Self::IfElse(i), super::Imperative::IfElse(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}",
            ))),
        }
    }
}

impl Display for Imperative {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Block(i) => Display::fmt(i, f),
            Self::Loop(i) => Display::fmt(i, f),
            Self::IfElse(i) => Display::fmt(i, f),
        }
    }
}

/// Represents an input for the [`super::Functional`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Functional {
    NumericLiteral(NumericLiteral),
    BooleanLiteral(BooleanLiteral),
    Named(Named),
    Prefix(Prefix),
    MemberAccess(MemberAccess),
    Binary(Binary),
    FunctionCall(FunctionCall),
    Parenthesized(Parenthesized),
    StructLiteral(StructLiteral),
    ArrowOperator(ArrowOperator),
    Cast(Cast),
}

/// Represents an input for the [`super::ArrowOperator`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArrowOperator {
    /// The operand expression of the arrow operator
    pub operand: Box<Functional>,

    /// The name of the field to access
    pub identifier: Identifier,
}

impl Input for ArrowOperator {
    type Output = super::ArrowOperator;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.operand.assert(&output.operand)?;
        self.identifier.assert(&output.identifier)
    }
}

impl Arbitrary for ArrowOperator {
    type Parameters = Option<BoxedStrategy<Functional>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let operand = args
            .unwrap_or_else(|| filter_functional_variant(Expression::arbitrary()))
            .prop_filter_map("filter out grammar ambiguity", |x| {
                if matches!(x, Functional::Prefix(..) | Functional::Binary(..)) {
                    None
                } else {
                    Some(Box::new(x))
                }
            });

        (operand, Identifier::arbitrary())
            .prop_map(|(operand, identifier)| Self {
                operand,
                identifier,
            })
            .boxed()
    }
}

impl Display for ArrowOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}->{}", self.operand, self.identifier)
    }
}

/// Represents an input for the [`super::Return`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Return {
    /// The expression of the `return`.
    pub expression: Option<Functional>,
}

impl Input for Return {
    type Output = super::Return;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.expression.assert(&output.expression)
    }
}

impl Arbitrary for Return {
    type Parameters = Option<BoxedStrategy<Functional>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let functional = args.unwrap_or_else(|| filter_functional_variant(Expression::arbitrary()));

        proptest::option::of(functional)
            .prop_map(|expression| Self { expression })
            .boxed()
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("return")?;

        if let Some(expression) = &self.expression {
            write!(f, " {expression}")?;
        }

        Ok(())
    }
}

/// Represents an input for the [`super::Continue`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Continue {
    /// The label of the `continue`.
    pub label: Option<Label>,
}

impl Input for Continue {
    type Output = super::Continue;

    fn assert(&self, output: &Self::Output) -> TestCaseResult { self.label.assert(&output.label) }
}

impl Arbitrary for Continue {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
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

/// Represents an input for the [`super::Express`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Express {
    /// The label of the `express`.
    pub label: Option<Label>,

    /// The expression of the `express`.
    pub expression: Option<Functional>,
}

impl Input for Express {
    type Output = super::Express;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.label.assert(&output.label)?;
        self.expression.assert(&output.expression)
    }
}

impl Display for Express {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("express")?;

        if let Some(label) = &self.label {
            write!(f, " {label}")?;
        }

        if let Some(expression) = &self.expression {
            write!(f, " {expression}")?;
        }

        Ok(())
    }
}

impl Arbitrary for Express {
    type Parameters = Option<BoxedStrategy<Functional>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expression = args.unwrap_or_else(|| filter_functional_variant(Expression::arbitrary()));
        (
            proptest::option::of(Label::arbitrary()),
            proptest::option::of(expression),
        )
            .prop_map(|(label, expression)| Self { label, expression })
            .boxed()
    }
}

/// Represents an input for the [`super::Break`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Break {
    /// The label of the `break`.
    pub label: Option<Label>,

    /// The expression of the `break`.
    pub expression: Option<Functional>,
}

impl Input for Break {
    type Output = super::Break;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.label.assert(&output.label)?;
        self.expression.assert(&output.expression)
    }
}

impl Display for Break {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("break")?;

        if let Some(label) = &self.label {
            write!(f, " {label}")?;
        }

        if let Some(expression) = &self.expression {
            write!(f, " {expression}")?;
        }

        Ok(())
    }
}

impl Arbitrary for Break {
    type Parameters = Option<BoxedStrategy<Functional>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expression = args.unwrap_or_else(|| filter_functional_variant(Expression::arbitrary()));
        (
            proptest::option::of(Label::arbitrary()),
            proptest::option::of(expression),
        )
            .prop_map(|(label, expression)| Self { label, expression })
            .boxed()
    }
}

/// Represents an input for the [`super::Terminator`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Terminator {
    Return(Return),
    Continue(Continue),
    Express(Express),
    Break(Break),
}

impl Input for Terminator {
    type Output = super::Terminator;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Return(i), super::Terminator::Return(o)) => i.assert(o),
            (Self::Continue(i), super::Terminator::Continue(o)) => i.assert(o),
            (Self::Express(i), super::Terminator::Express(o)) => i.assert(o),
            (Self::Break(i), super::Terminator::Break(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "expected {self:?}, got {output:?}"
            ))),
        }
    }
}

impl Display for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Return(return_) => Display::fmt(return_, f),
            Self::Continue(continue_) => Display::fmt(continue_, f),
            Self::Express(express) => Display::fmt(express, f),
            Self::Break(break_) => Display::fmt(break_, f),
        }
    }
}

impl Input for Functional {
    type Output = super::Functional;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::NumericLiteral(i), super::Functional::NumericLiteral(o)) => i.assert(o),
            (Self::BooleanLiteral(i), super::Functional::BooleanLiteral(o)) => i.assert(o),
            (Self::Named(i), super::Functional::Named(o)) => i.assert(o),
            (Self::Prefix(i), super::Functional::Prefix(o)) => i.assert(o),
            (Self::MemberAccess(i), super::Functional::MemberAccess(o)) => i.assert(o),
            (Self::Binary(i), super::Functional::Binary(o)) => i.assert(o),
            (Self::FunctionCall(i), super::Functional::FunctionCall(o)) => i.assert(o),
            (Self::Parenthesized(i), super::Functional::Parenthesized(o)) => i.assert(o),
            (Self::StructLiteral(i), super::Functional::StructLiteral(o)) => i.assert(o),
            (Self::Cast(i), super::Functional::Cast(o)) => i.assert(o),
            (Self::ArrowOperator(i), super::Functional::ArrowOperator(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "expected functional to be {self:?}, found {output:?}",
            ))),
        }
    }
}

impl Display for Functional {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NumericLiteral(i) => Display::fmt(i, f),
            Self::BooleanLiteral(i) => Display::fmt(i, f),
            Self::Named(i) => Display::fmt(i, f),
            Self::Prefix(i) => Display::fmt(i, f),
            Self::MemberAccess(i) => Display::fmt(i, f),
            Self::Binary(i) => Display::fmt(i, f),
            Self::FunctionCall(i) => Display::fmt(i, f),
            Self::Parenthesized(i) => Display::fmt(i, f),
            Self::StructLiteral(i) => Display::fmt(i, f),
            Self::Cast(i) => Display::fmt(i, f),
            Self::ArrowOperator(i) => Display::fmt(i, f),
        }
    }
}

/// Represents an input for the [`super::Expression`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Expression {
    Functional(Functional),
    Imperative(Imperative),
    Terminator(Terminator),
}

impl Input for Expression {
    type Output = super::Expression;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Functional(i), super::Expression::Functional(o)) => i.assert(o),
            (Self::Imperative(i), super::Expression::Imperative(o)) => i.assert(o),
            (Self::Terminator(i), super::Expression::Terminator(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "expected expression to be {self:?}, found {output:?}",
            ))),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Functional(i) => Display::fmt(i, f),
            Self::Imperative(i) => Display::fmt(i, f),
            Self::Terminator(i) => Display::fmt(i, f),
        }
    }
}

fn filter_functional_variant(
    x: impl Strategy<Value = Expression> + 'static,
) -> BoxedStrategy<Functional> {
    x.prop_filter_map("filter functional variant", |x| match x {
        Expression::Functional(f) => Some(f),
        Expression::Imperative(..) | Expression::Terminator(..) => None,
    })
    .boxed()
}

impl Arbitrary for Expression {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        let leaf = proptest::prop_oneof![
            NumericLiteral::arbitrary()
                .prop_map(|x| Self::Functional(Functional::NumericLiteral(x))),
            BooleanLiteral::arbitrary()
                .prop_map(|x| Self::Functional(Functional::BooleanLiteral(x))),
            Named::arbitrary().prop_map(|x| Self::Functional(Functional::Named(x))),
        ];

        leaf.prop_recursive(8, 256, 8, |inner| {
            prop_oneof![
                Prefix::arbitrary_with(Some(filter_functional_variant(inner.clone())))
                    .prop_map(|x| Self::Functional(Functional::Prefix(x))),
                MemberAccess::arbitrary_with(Some(filter_functional_variant(inner.clone())))
                    .prop_map(|x| Self::Functional(Functional::MemberAccess(x))),
                Binary::arbitrary_with(Some(filter_functional_variant(inner.clone())))
                    .prop_map(|x| Self::Functional(Functional::Binary(x))),
                FunctionCall::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Self::Functional(Functional::FunctionCall(x))),
                Parenthesized::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Self::Functional(Functional::Parenthesized(x))),
                StructLiteral::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Self::Functional(Functional::StructLiteral(x))),
                Cast::arbitrary_with(Some(filter_functional_variant(inner.clone())))
                    .prop_map(|x| Self::Functional(Functional::Cast(x))),
                ArrowOperator::arbitrary_with(Some(filter_functional_variant(inner.clone())))
                    .prop_map(|x| Self::Functional(Functional::ArrowOperator(x))),
                Block::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Self::Imperative(Imperative::Block(x))),
                Loop::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Self::Imperative(Imperative::Loop(x))),
                IfElse::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Self::Imperative(Imperative::IfElse(x))),
                Return::arbitrary_with(Some(filter_functional_variant(inner.clone())))
                    .prop_map(|x| Self::Terminator(Terminator::Return(x))),
                Continue::arbitrary().prop_map(|x| Self::Terminator(Terminator::Continue(x))),
                Express::arbitrary_with(Some(filter_functional_variant(inner.clone())))
                    .prop_map(|x| Self::Terminator(Terminator::Express(x))),
                Break::arbitrary_with(Some(filter_functional_variant(inner)))
                    .prop_map(|x| Self::Terminator(Terminator::Break(x))),
            ]
        })
        .boxed()
    }
}
