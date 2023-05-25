//! Contains various definition of `proptest` strategies for creating expression inputs.

use std::string::ToString;

use enum_as_inner::EnumAsInner;
use proptest::{
    prop_assert_eq, prop_oneof,
    strategy::{Just, Strategy},
    test_runner::TestCaseError,
};

use super::{
    BooleanLiteral, Expression, FunctionCall, Functional, MemberAccess, Named, NumericLiteral,
    Prefix, PrefixOperator, StructLiteral,
};
use crate::syntax_tree::{strategy::QualifiedIdentifierInput, ConnectedList};

/// Represents an input for [`super::NumericLiteral`]
#[derive(Debug, Clone, Copy)]
pub struct NumericLiteralInput {
    /// The number
    pub number: u64,
}

impl NumericLiteralInput {
    /// Validates the input against the [`NumericLiteral`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &NumericLiteral) -> Result<(), TestCaseError> {
        let output_number = output
            .numeric_literal_token
            .value_span
            .str()
            .parse::<u64>()?;

        prop_assert_eq!(self.number, output_number);

        Ok(())
    }
}

// Represents an input for [`super::FieldInitializer`]
#[derive(Debug, Clone)]
pub struct FieldInitializerInput {
    /// The identifier of the field
    pub identifier: String,

    /// The expression of the field
    pub expression: ExpressionInput,
}

impl ToString for FieldInitializerInput {
    fn to_string(&self) -> String {
        format!("{}: {}", self.identifier, self.expression.to_string())
    }
}

/// Represents an input for [`super::StructLiteral`]
#[derive(Debug, Clone)]
pub struct StructLiteralInput {
    /// The qualified identifier of the struct
    pub qualified_identifier: QualifiedIdentifierInput,

    /// The field initializers of the struct
    pub field_initializers: Vec<FieldInitializerInput>,

    /// Whether the struct literal has a trailing comma
    pub trailing_comma: bool,
}

impl ToString for StructLiteralInput {
    fn to_string(&self) -> String {
        let mut field_initializers = self
            .field_initializers
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");

        if !field_initializers.is_empty() && self.trailing_comma {
            field_initializers.push(',');
        }

        format!(
            "{} {{{}}}",
            self.qualified_identifier.to_string(),
            field_initializers
        )
    }
}

impl StructLiteralInput {
    /// Validates the input against the [`StructLiteral`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &StructLiteral) -> Result<(), TestCaseError> {
        self.qualified_identifier
            .validate(&output.qualified_identifier)?;

        prop_assert_eq!(
            self.field_initializers.len(),
            output
                .field_initializers
                .as_ref()
                .map_or(0, ConnectedList::len)
        );

        if !self.field_initializers.is_empty() {
            let Some(field_initializers) = &output.field_initializers else {
                return Err(TestCaseError::fail("Expected arguments"));
            };

            prop_assert_eq!(
                self.trailing_comma,
                field_initializers.trailing_separator.is_some()
            );
        }

        for (input, output) in self.field_initializers.iter().zip(
            output
                .field_initializers
                .iter()
                .flat_map(ConnectedList::elements),
        ) {
            prop_assert_eq!(&input.identifier, output.identifier.span.str());

            input.expression.validate(&output.expression)?;
        }

        Ok(())
    }
}

/// Represents an input for [`super::FunctionCall`]
#[derive(Debug, Clone)]
pub struct FunctionCallInput {
    /// The qualified identifier of the function
    pub qualified_identifier: QualifiedIdentifierInput,

    /// The arguments of the function
    pub arguments: Vec<ExpressionInput>,

    /// Whether the function call has a trailing comma
    pub trailing_comma: bool,
}

impl FunctionCallInput {
    /// Validates the input against the [`FunctionCall`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &FunctionCall) -> Result<(), TestCaseError> {
        self.qualified_identifier
            .validate(&output.qualified_identifier)?;

        prop_assert_eq!(
            self.arguments.len(),
            output.arguments.as_ref().map_or(0, ConnectedList::len)
        );

        if !self.arguments.is_empty() {
            let Some(arguments) = &output.arguments else {
                return Err(TestCaseError::fail("Expected arguments"));
            };

            prop_assert_eq!(self.trailing_comma, arguments.trailing_separator.is_some());
        }

        for (input, output) in self
            .arguments
            .iter()
            .zip(output.arguments.iter().flat_map(ConnectedList::elements))
        {
            input.validate(output)?;
        }

        Ok(())
    }
}

impl ToString for FunctionCallInput {
    fn to_string(&self) -> String {
        let mut arguments = self
            .arguments
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");

        if !arguments.is_empty() && self.trailing_comma {
            arguments.push(',');
        }

        format!("{}({})", self.qualified_identifier.to_string(), arguments)
    }
}

impl ToString for NumericLiteralInput {
    fn to_string(&self) -> String { self.number.to_string() }
}

/// Represents an input for [`super::MemberAccess`]
#[derive(Debug, Clone)]
pub struct MemberAccessInput {
    /// The identifier
    pub identifier: String,

    /// The operand
    pub operand: Box<ExpressionInput>,
}

impl MemberAccessInput {
    /// Validates the input against the [`MemberAccess`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &MemberAccess) -> Result<(), TestCaseError> {
        prop_assert_eq!(&self.identifier, output.identifier.span.str());

        self.operand.validate(&output.operand)
    }
}

impl ToString for MemberAccessInput {
    fn to_string(&self) -> String { format!("{}.{}", self.operand.to_string(), self.identifier) }
}

/// Represents an input for [`super::BooleanLiteral`]
#[derive(Debug, Clone, Copy)]
pub struct BooleanLiteralInput {
    /// The value
    pub value: bool,
}

impl BooleanLiteralInput {
    /// Validates the input against the [`BooleanLiteral`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &BooleanLiteral) -> Result<(), TestCaseError> {
        match (self.value, output) {
            (true, BooleanLiteral::True(output)) => {
                prop_assert_eq!(output.span.str(), "true");
                Ok(())
            }
            (false, BooleanLiteral::False(output)) => {
                prop_assert_eq!(output.span.str(), "false");
                Ok(())
            }
            _ => Err(TestCaseError::fail("Boolean literal mismatch")),
        }
    }
}

impl ToString for BooleanLiteralInput {
    fn to_string(&self) -> String { self.value.to_string() }
}

/// Represents an input for [`super::PrefixOperator`]
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum PrefixOperatorInput {
    LogicalNot,
    Negate,
}

impl PrefixOperatorInput {
    /// Validates the input against the [`PrefixOperator`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &PrefixOperator) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::LogicalNot, PrefixOperator::LogicalNot(output)) => {
                prop_assert_eq!(output.span.str(), "!");
                Ok(())
            }
            (Self::Negate, PrefixOperator::Negate(output)) => {
                prop_assert_eq!(output.span.str(), "-");
                Ok(())
            }
            _ => Err(TestCaseError::fail("Prefix operator mismatch")),
        }
    }
}

impl ToString for PrefixOperatorInput {
    fn to_string(&self) -> String {
        match self {
            Self::LogicalNot => "!",
            Self::Negate => "-",
        }
        .to_string()
    }
}

/// Represents an input for [`super::Prefix`]
#[derive(Debug, Clone)]
pub struct PrefixInput {
    /// The prefix operator
    pub prefix_operator: PrefixOperatorInput,

    /// The operand
    pub operand: Box<ExpressionInput>,
}

impl PrefixInput {
    /// Validates the input against the [`Prefix`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Prefix) -> Result<(), TestCaseError> {
        self.prefix_operator.validate(&output.prefix_operator)?;
        self.operand.validate(&output.operand)?;
        Ok(())
    }
}

impl ToString for PrefixInput {
    fn to_string(&self) -> String {
        format!(
            "{}{}",
            self.prefix_operator.to_string(),
            self.operand.to_string()
        )
    }
}

/// Represents an input for [`crate::syntax_tree::QualifiedIdentifier`]
#[derive(Debug, Clone)]
pub struct NamedInput {
    /// The qualified identifier
    pub qualified_identifier: QualifiedIdentifierInput,
}

impl NamedInput {
    /// Validates the input against the [`Named`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Named) -> Result<(), TestCaseError> {
        self.qualified_identifier
            .validate(&output.qualified_identifier)
    }
}

impl ToString for NamedInput {
    fn to_string(&self) -> String { self.qualified_identifier.to_string() }
}

/// Represents an input for [`super::Functional`]
#[derive(Debug, Clone, EnumAsInner)]
#[allow(missing_docs)]
pub enum FunctionalInput {
    NumericLiteral(NumericLiteralInput),
    Prefix(PrefixInput),
    BooleanLiteral(BooleanLiteralInput),
    MemberAccess(MemberAccessInput),
    FunctionCall(FunctionCallInput),
    StructLiteral(StructLiteralInput),
    Named(NamedInput),
}

impl FunctionalInput {
    /// Validates the input against the [`Functional`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Functional) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::NumericLiteral(i), Functional::NumericLiteral(o)) => i.validate(o),
            (Self::Prefix(i), Functional::Prefix(o)) => i.validate(o),
            (Self::BooleanLiteral(i), Functional::BooleanLiteral(o)) => i.validate(o),
            (Self::MemberAccess(i), Functional::MemberAccess(o)) => i.validate(o),
            (Self::FunctionCall(i), Functional::FunctionCall(o)) => i.validate(o),
            (Self::StructLiteral(i), Functional::StructLiteral(o)) => i.validate(o),
            (Self::Named(i), Functional::Named(o)) => i.validate(o),
            _ => Err(TestCaseError::fail("Functional type mismatch")),
        }
    }
}

impl ToString for FunctionalInput {
    fn to_string(&self) -> String {
        match self {
            Self::NumericLiteral(n) => n.to_string(),
            Self::Prefix(p) => p.to_string(),
            Self::BooleanLiteral(p) => p.to_string(),
            Self::MemberAccess(p) => p.to_string(),
            Self::FunctionCall(p) => p.to_string(),
            Self::StructLiteral(p) => p.to_string(),
            Self::Named(p) => p.to_string(),
        }
    }
}

/// Represents an input for [`super::Expression`]
#[derive(Debug, Clone, EnumAsInner)]
#[allow(missing_docs)]
pub enum ExpressionInput {
    Functional(FunctionalInput),
}

impl ExpressionInput {
    /// Validates the input against the [`Expression`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Expression) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Functional(i), Expression::Functional(o)) => i.validate(o),
            _ => Err(TestCaseError::fail("Expression type mismatch")),
        }
    }
}

impl ToString for ExpressionInput {
    fn to_string(&self) -> String {
        match self {
            Self::Functional(f) => f.to_string(),
        }
    }
}

/// Represents a [`Strategy`] for [`NumericLiteralInput`]
pub fn numeric_literal() -> impl Strategy<Value = NumericLiteralInput> {
    proptest::num::u64::ANY.prop_map(|number| NumericLiteralInput { number })
}

/// Represents a [`Strategy`] for [`PrefixInput`]
pub fn boolean_literal() -> impl Strategy<Value = BooleanLiteralInput> {
    proptest::bool::ANY.prop_map(|value| BooleanLiteralInput { value })
}

/// Returns a [`Strategy`] for [`ExpressionInput`]
pub fn base_expression() -> impl Strategy<Value = ExpressionInput> {
    prop_oneof![
        numeric_literal()
            .prop_map(|x| ExpressionInput::Functional(FunctionalInput::NumericLiteral(x))),
        boolean_literal()
            .prop_map(|x| ExpressionInput::Functional(FunctionalInput::BooleanLiteral(x))),
    ]
}

/// Returns a [`Strategy`] for [`PrefixInput`]
pub fn prefix_with(
    expression_strategy: impl Strategy<Value = ExpressionInput>,
) -> impl Strategy<Value = PrefixInput> {
    // prefix
    (
        prop_oneof![
            Just(PrefixOperatorInput::LogicalNot),
            Just(PrefixOperatorInput::Negate)
        ],
        expression_strategy,
    )
        .prop_filter_map(
            "prefix input cannot have `binary` as its operand",
            |(prefix_operator, expression)| {
                Some(PrefixInput {
                    prefix_operator,
                    operand: Box::new(expression),
                })
            },
        )
}

/// Returns a [`Strategy`] for [`MemberAccessInput`]
pub fn member_access_with(
    expression_strategy: impl Strategy<Value = ExpressionInput>,
) -> impl Strategy<Value = MemberAccessInput> {
    (
        pernixc_lexical::token::strategy::identifier(),
        expression_strategy,
    )
        .prop_filter_map(
            "member access input cannot have `prefix` and `binary` as its operand",
            |(identifier, operand)| {
                if operand
                    .as_functional()
                    .and_then(FunctionalInput::as_prefix)
                    .is_some()
                {
                    return None;
                }

                Some(MemberAccessInput {
                    identifier,
                    operand: Box::new(operand),
                })
            },
        )
}

/// Returns a [`Strategy`] for [`FunctionCallInput`]
pub fn function_call_with(
    expression_strategy: impl Strategy<Value = ExpressionInput>,
) -> impl Strategy<Value = FunctionCallInput> {
    (
        crate::syntax_tree::strategy::qualified_identifier(),
        proptest::collection::vec(expression_strategy, 0..=10),
        proptest::bool::ANY,
    )
        .prop_map(
            |(qualified_identifier, arguments, trailing_comma)| FunctionCallInput {
                qualified_identifier,
                arguments,
                trailing_comma,
            },
        )
}

/// Returns a [`Strategy`] for [`StructLiteralInput`]
pub fn struct_literal_with(
    expression_strategy: impl Strategy<Value = ExpressionInput>,
) -> impl Strategy<Value = StructLiteralInput> {
    (
        crate::syntax_tree::strategy::qualified_identifier(),
        proptest::collection::vec(
            (
                pernixc_lexical::token::strategy::identifier(),
                expression_strategy,
            ),
            0..=10,
        ),
        proptest::bool::ANY,
    )
        .prop_map(
            |(qualified_identifier, field_initializers, trailing_comma)| StructLiteralInput {
                qualified_identifier,
                field_initializers: field_initializers
                    .into_iter()
                    .map(|(identifier, expression)| FieldInitializerInput {
                        identifier,
                        expression,
                    })
                    .collect(),
                trailing_comma,
            },
        )
}

/// Returns a [`Strategy`] for [`ExpressionInput`]
pub fn expression() -> impl Strategy<Value = ExpressionInput> {
    base_expression().prop_recursive(8, 64, 10, |inner| {
        prop_oneof![
            prefix_with(inner.clone())
                .prop_map(|x| ExpressionInput::Functional(FunctionalInput::Prefix(x))),
            member_access_with(inner.clone())
                .prop_map(|x| ExpressionInput::Functional(FunctionalInput::MemberAccess(x))),
            function_call_with(inner.clone())
                .prop_map(|x| ExpressionInput::Functional(FunctionalInput::FunctionCall(x))),
            struct_literal_with(inner)
                .prop_map(|x| ExpressionInput::Functional(FunctionalInput::StructLiteral(x))),
            crate::syntax_tree::strategy::qualified_identifier().prop_map(|qualified_identifier| {
                ExpressionInput::Functional(FunctionalInput::Named(NamedInput {
                    qualified_identifier,
                }))
            })
        ]
    })
}

