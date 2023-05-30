//! Contains various definition of `proptest` strategies for creating expression inputs.

use std::string::ToString;

use enum_as_inner::EnumAsInner;
use proptest::{
    prop_assert_eq, prop_oneof,
    strategy::{Just, Strategy},
    test_runner::TestCaseError,
};

use super::{
    Binary, BinaryOperator, Block, BooleanLiteral, Break, Continue, Express, Expression,
    FunctionCall, Functional, Imperative, Loop, MemberAccess, Named, NumericLiteral, Parenthesized,
    Prefix, PrefixOperator, Return, StructLiteral, Terminator,
};
use crate::syntax_tree::{
    statement::strategy::StatementInput, strategy::QualifiedIdentifierInput, ConnectedList,
};

/// Represents an input for [`super::Loop`]
#[derive(Debug, Clone)]
pub struct LoopInput {
    /// The label of the block
    pub label: Option<String>,

    /// The list of statements inside the block
    pub statements: Vec<StatementInput>,
}

impl ToString for LoopInput {
    fn to_string(&self) -> String {
        let mut string = String::new();

        if let Some(label) = &self.label {
            string.push_str(&format!("'{label}: "));
        }

        string.push_str("loop {");

        for statement in &self.statements {
            string.push_str(&statement.to_string());
        }

        string.push('}');

        string
    }
}

impl LoopInput {
    /// Validates the input against the [`Loop`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Loop) -> Result<(), TestCaseError> {
        match (&self.label, &output.label_specifier) {
            (Some(label), Some(output_label)) => {
                prop_assert_eq!(label, &output_label.label.identifier.span.str());
            }
            (None, None) => (),
            _ => return Err(TestCaseError::fail("Label mismatch")),
        }

        if self.statements.len() != output.block_without_label.statements.len() {
            dbg!(&self.statements, &output.block_without_label.statements);
            return Err(TestCaseError::fail("Statement count mismatch"));
        }

        for (input, output) in self
            .statements
            .iter()
            .zip(output.block_without_label.statements.iter())
        {
            input.validate(output)?;
        }

        Ok(())
    }
}

/// Represents an input for [`super::Block`]
#[derive(Debug, Clone)]
pub struct BlockInput {
    /// The label of the block
    pub label: Option<String>,

    /// The list of statements inside the block
    pub statements: Vec<StatementInput>,
}

impl ToString for BlockInput {
    fn to_string(&self) -> String {
        let mut string = String::new();

        if let Some(label) = &self.label {
            string.push_str(&format!("'{label}: "));
        }

        string.push('{');

        for statement in &self.statements {
            string.push_str(&statement.to_string());
        }

        string.push('}');

        string
    }
}

impl BlockInput {
    /// Validates the input against the [`Block`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Block) -> Result<(), TestCaseError> {
        match (&self.label, &output.label_specifier) {
            (Some(label), Some(output_label)) => {
                prop_assert_eq!(label, &output_label.label.identifier.span.str());
            }
            (None, None) => (),
            _ => return Err(TestCaseError::fail("Label mismatch")),
        }

        if self.statements.len() != output.block_without_label.statements.len() {
            return Err(TestCaseError::fail(format!(
                "Statement count mismatch: {:#?}\n{:#?}",
                &self.statements, &output.block_without_label.statements
            )));
        }

        for (input, output) in self
            .statements
            .iter()
            .zip(output.block_without_label.statements.iter())
        {
            input.validate(output)?;
        }

        Ok(())
    }
}

/// Represents an input for [`super::Parenthesized`]
#[derive(Debug, Clone)]
pub struct ParenthesizedInput {
    /// The expression inside the parentheses
    pub expression: Box<ExpressionInput>,
}

impl ToString for ParenthesizedInput {
    fn to_string(&self) -> String { format!("({})", self.expression.to_string()) }
}

impl ParenthesizedInput {
    /// Validates the input against the [`Parenthesized`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Parenthesized) -> Result<(), TestCaseError> {
        self.expression.validate(&output.expression)
    }
}

/// Represents an input for [`super::BinaryOperator`]
#[derive(Debug, Clone, Copy)]
pub enum BinaryOperatorInput {
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

impl ToString for BinaryOperatorInput {
    fn to_string(&self) -> String {
        match self {
            Self::Add => "+",
            Self::Subtract => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::Modulo => "%",
            Self::Assign => "=",
            Self::CompoundAdd => "+=",
            Self::CompoundSubtract => "-=",
            Self::CompoundMultiply => "*=",
            Self::CompoundDivide => "/=",
            Self::CompoundModulo => "%=",
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::GreaterThan => ">",
            Self::GreaterThanOrEqual => ">=",
            Self::LessThan => "<",
            Self::LessThanOrEqual => "<=",
            Self::LogicalAnd => "and",
            Self::LogicalOr => "or",
        }
        .to_string()
    }
}

impl BinaryOperatorInput {
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

    /// Validates the input against the [`BinaryOperator`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &BinaryOperator) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Add, BinaryOperator::Add(..))
            | (Self::Subtract, BinaryOperator::Subtract(..))
            | (Self::Multiply, BinaryOperator::Multiply(..))
            | (Self::Divide, BinaryOperator::Divide(..))
            | (Self::Modulo, BinaryOperator::Modulo(..))
            | (Self::Assign, BinaryOperator::Assign(..))
            | (Self::CompoundAdd, BinaryOperator::CompoundAdd(..))
            | (Self::CompoundSubtract, BinaryOperator::CompoundSubtract(..))
            | (Self::CompoundMultiply, BinaryOperator::CompoundMultiply(..))
            | (Self::CompoundDivide, BinaryOperator::CompoundDivide(..))
            | (Self::CompoundModulo, BinaryOperator::CompoundModulo(..))
            | (Self::Equal, BinaryOperator::Equal(..))
            | (Self::NotEqual, BinaryOperator::NotEqual(..))
            | (Self::GreaterThan, BinaryOperator::GreaterThan(..))
            | (Self::GreaterThanOrEqual, BinaryOperator::GreaterThanOrEqual(..))
            | (Self::LessThan, BinaryOperator::LessThan(..))
            | (Self::LessThanOrEqual, BinaryOperator::LessThanOrEqual(..))
            | (Self::LogicalAnd, BinaryOperator::LogicalAnd(..))
            | (Self::LogicalOr, BinaryOperator::LogicalOr(..)) => Ok(()),
            (_, _) => Err(TestCaseError::fail("Binary operator mismatch")),
        }
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

/// Represents an input for [`super::Binary`]
#[derive(Debug, Clone)]
pub struct BinaryInput {
    /// The left operand of the binary expression.
    pub left_operand: Box<FunctionalInput>,

    /// The binary operator.
    pub binary_operator: BinaryOperatorInput,

    /// The right operand of the binary expression.
    pub right_operand: Box<FunctionalInput>,
}

impl BinaryInput {
    /// Validates the input against the [`Binary`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Binary) -> Result<(), TestCaseError> {
        self.left_operand.validate(&output.left_operand)?;
        self.right_operand.validate(&output.right_operand)?;
        self.binary_operator.validate(&output.binary_operator)
    }
}

impl ToString for BinaryInput {
    fn to_string(&self) -> String {
        format!(
            "{} {} {}",
            self.left_operand.to_string(),
            self.binary_operator.to_string(),
            self.right_operand.to_string()
        )
    }
}

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
    pub operand: Box<FunctionalInput>,
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
    pub operand: Box<FunctionalInput>,
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
    Binary(BinaryInput),
    Named(NamedInput),
    Parenthesized(ParenthesizedInput),
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
            (Self::Binary(i), Functional::Binary(o)) => i.validate(o),
            (Self::Parenthesized(i), Functional::Parenthesized(o)) => i.validate(o),
            _ => Err(TestCaseError::fail(format!(
                "{self:#?}\n{output:#?} mismatch"
            ))),
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
            Self::Binary(p) => p.to_string(),
            Self::Parenthesized(p) => p.to_string(),
        }
    }
}

/// Represents an input for [`super::Return`]
#[derive(Debug, Clone)]
pub struct ReturnInput {
    /// The return expression of the return statement
    pub expression: Option<FunctionalInput>,
}

impl ToString for ReturnInput {
    fn to_string(&self) -> String {
        self.expression.as_ref().map_or_else(
            || "return".to_string(),
            |e| format!("return {}", e.to_string()),
        )
    }
}

impl ReturnInput {
    /// Validates the input against the [`Return`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Return) -> Result<(), TestCaseError> {
        match (&self.expression, &output.expression) {
            (None, None) => Ok(()),
            (Some(i), Some(o)) => i.validate(o),
            _ => Err(TestCaseError::fail(format!(
                "{self:#?}\n{output:#?} mismatch"
            ))),
        }
    }
}

/// Represents an input for [`super::Break`]
#[derive(Debug, Clone)]
pub struct BreakInput {
    /// The label of the break statement
    pub label: Option<String>,

    /// The expression of the break statement
    pub expression: Option<FunctionalInput>,
}

impl ToString for BreakInput {
    fn to_string(&self) -> String {
        let mut string = "break".to_string();

        if let Some(label) = &self.label {
            string.push_str(&format!(" '{label}"));
        }

        if let Some(expression) = &self.expression {
            string.push_str(&format!(" {}", expression.to_string()));
        }

        string
    }
}

impl BreakInput {
    /// Validates the input against the [`Break`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Break) -> Result<(), TestCaseError> {
        match (self.label.as_ref(), output.label.as_ref()) {
            (Some(i), Some(o)) => {
                prop_assert_eq!(i, o.identifier.span.str());
            }
            (None, None) => {}
            _ => return Err(TestCaseError::fail("label mismatch")),
        }

        match (self.expression.as_ref(), output.expression.as_ref()) {
            (Some(i), Some(o)) => i.validate(o)?,
            (None, None) => {}
            _ => return Err(TestCaseError::fail("expression mismatch")),
        }

        Ok(())
    }
}

/// Represents an input for [`super::Express`]
#[derive(Debug, Clone)]
pub struct ExpressInput {
    /// The label of the express statement
    pub label: Option<String>,

    /// The expression of the express statement
    pub expression: Option<FunctionalInput>,
}

impl ToString for ExpressInput {
    fn to_string(&self) -> String {
        let mut string = String::from("express");

        if let Some(label) = &self.label {
            string.push_str(&format!(" '{label}"));
        }

        if let Some(expression) = &self.expression {
            string.push_str(&format!(" {}", expression.to_string()));
        }

        string
    }
}

impl ExpressInput {
    /// Validates the input against the [`Express`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Express) -> Result<(), TestCaseError> {
        match (self.label.as_ref(), output.label.as_ref()) {
            (Some(i), Some(o)) => {
                prop_assert_eq!(i, o.identifier.span.str());
            }
            (None, None) => {}
            _ => return Err(TestCaseError::fail("label mismatch")),
        }

        match (self.expression.as_ref(), output.expression.as_ref()) {
            (Some(i), Some(o)) => i.validate(o)?,
            (None, None) => {}
            _ => return Err(TestCaseError::fail("expression mismatch")),
        }

        Ok(())
    }
}

/// Represents an input for [`super::Continue`]
#[derive(Debug, Clone)]
pub struct ContinueInput {
    /// The label of the continue statement
    pub label: Option<String>,
}

impl ToString for ContinueInput {
    fn to_string(&self) -> String {
        let mut string = String::from("continue");

        if let Some(label) = &self.label {
            string.push_str(&format!(" '{label}"));
        }

        string
    }
}

impl ContinueInput {
    /// Validates the input against the [`Continue`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Continue) -> Result<(), TestCaseError> {
        match (self.label.as_ref(), output.label.as_ref()) {
            (Some(i), Some(o)) => {
                prop_assert_eq!(i, o.identifier.span.str());
            }
            (None, None) => {}
            _ => return Err(TestCaseError::fail("label mismatch")),
        }

        Ok(())
    }
}

/// Represents an input for [`super::Terminator`]
#[derive(Debug, Clone, EnumAsInner)]
pub enum TerminatorInput {
    Return(ReturnInput),
    Break(BreakInput),
    Continue(ContinueInput),
    Express(ExpressInput),
}

impl ToString for TerminatorInput {
    fn to_string(&self) -> String {
        match self {
            Self::Return(r) => r.to_string(),
            Self::Break(b) => b.to_string(),
            Self::Continue(c) => c.to_string(),
            Self::Express(e) => e.to_string(),
        }
    }
}

impl TerminatorInput {
    /// Validates the input against the [`Terminator`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Terminator) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Return(i), Terminator::Return(o)) => i.validate(o),
            (Self::Break(i), Terminator::Break(o)) => i.validate(o),
            (Self::Continue(i), Terminator::Continue(o)) => i.validate(o),
            (Self::Express(i), Terminator::Express(o)) => i.validate(o),
            _ => Err(TestCaseError::fail(format!(
                "{self:#?}\n{output:#?} mismatch"
            ))),
        }
    }
}

/// Represents an input for [`super::Block`]
#[derive(Debug, Clone, EnumAsInner)]
pub enum ImperativeInput {
    Block(BlockInput),
    Loop(LoopInput),
}

impl ToString for ImperativeInput {
    fn to_string(&self) -> String {
        match self {
            Self::Block(b) => b.to_string(),
            Self::Loop(l) => l.to_string(),
        }
    }
}

impl ImperativeInput {
    /// Validates the input against the output
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Imperative) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Block(i), Imperative::Block(o)) => i.validate(o),
            (Self::Loop(i), Imperative::Loop(o)) => i.validate(o),
            _ => Err(TestCaseError::fail(format!(
                "{self:#?}\n{output:#?} mismatch"
            ))),
        }
    }
}

/// Represents an input for [`super::Expression`]
#[derive(Debug, Clone, EnumAsInner)]
#[allow(missing_docs)]
pub enum ExpressionInput {
    Functional(FunctionalInput),
    Imperative(ImperativeInput),
    Terminator(TerminatorInput),
}

impl ExpressionInput {
    /// Validates the input against the [`Expression`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Expression) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Functional(i), Expression::Functional(o)) => i.validate(o),
            (Self::Imperative(i), Expression::Imperative(o)) => i.validate(o),
            (Self::Terminator(i), Expression::Terminator(o)) => i.validate(o),
            _ => Err(TestCaseError::fail("Expression type mismatch")),
        }
    }
}

impl ToString for ExpressionInput {
    fn to_string(&self) -> String {
        match self {
            Self::Functional(f) => f.to_string(),
            Self::Imperative(i) => i.to_string(),
            Self::Terminator(t) => t.to_string(),
        }
    }
}

fn numeric_literal() -> impl Strategy<Value = NumericLiteralInput> {
    proptest::num::u64::ANY.prop_map(|number| NumericLiteralInput { number })
}

fn boolean_literal() -> impl Strategy<Value = BooleanLiteralInput> {
    proptest::bool::ANY.prop_map(|value| BooleanLiteralInput { value })
}

fn base_functional() -> impl Strategy<Value = FunctionalInput> {
    prop_oneof![
        numeric_literal().prop_map(FunctionalInput::NumericLiteral),
        boolean_literal().prop_map(FunctionalInput::BooleanLiteral),
        crate::syntax_tree::strategy::qualified_identifier(true).prop_map(|qualified_identifier| {
            FunctionalInput::Named(NamedInput {
                qualified_identifier,
            })
        })
    ]
}

fn prefix_with(
    functional_strategy: impl Strategy<Value = FunctionalInput>,
) -> impl Strategy<Value = PrefixInput> {
    // prefix
    (
        prop_oneof![
            Just(PrefixOperatorInput::LogicalNot),
            Just(PrefixOperatorInput::Negate)
        ],
        functional_strategy,
    )
        .prop_filter_map(
            "prefix input cannot have `binary` as its operand",
            |(prefix_operator, expression)| {
                if matches!(expression, FunctionalInput::Binary(..)) {
                    return None;
                }

                Some(PrefixInput {
                    prefix_operator,
                    operand: Box::new(expression),
                })
            },
        )
}

fn member_access_with(
    functional_strategy: impl Strategy<Value = FunctionalInput>,
) -> impl Strategy<Value = MemberAccessInput> {
    (
        pernixc_lexical::token::strategy::identifier(),
        functional_strategy,
    )
        .prop_filter_map(
            "member access input cannot have `prefix` and `binary` as its operand",
            |(identifier, operand)| {
                if matches!(
                    operand,
                    FunctionalInput::Prefix(..) | FunctionalInput::Binary(..)
                ) {
                    return None;
                }

                Some(MemberAccessInput {
                    identifier,
                    operand: Box::new(operand),
                })
            },
        )
}

fn binary_operator() -> impl Strategy<Value = BinaryOperatorInput> {
    prop_oneof![
        Just(BinaryOperatorInput::Add),
        Just(BinaryOperatorInput::Subtract),
        Just(BinaryOperatorInput::Multiply),
        Just(BinaryOperatorInput::Divide),
        Just(BinaryOperatorInput::Modulo),
        Just(BinaryOperatorInput::CompoundAdd),
        Just(BinaryOperatorInput::CompoundSubtract),
        Just(BinaryOperatorInput::CompoundMultiply),
        Just(BinaryOperatorInput::CompoundDivide),
        Just(BinaryOperatorInput::CompoundModulo),
        Just(BinaryOperatorInput::Assign),
        Just(BinaryOperatorInput::Equal),
        Just(BinaryOperatorInput::NotEqual),
        Just(BinaryOperatorInput::LessThan),
        Just(BinaryOperatorInput::LessThanOrEqual),
        Just(BinaryOperatorInput::GreaterThan),
        Just(BinaryOperatorInput::GreaterThanOrEqual),
        Just(BinaryOperatorInput::LogicalAnd),
        Just(BinaryOperatorInput::LogicalOr),
    ]
}

fn create_binary(
    binary_operator: BinaryOperatorInput,
    binary_input: BinaryInput,
    functional_input: FunctionalInput,
    swap: bool,
) -> Option<BinaryInput> {
    match binary_operator
        .get_precedence()
        .cmp(&binary_input.binary_operator.get_precedence())
    {
        std::cmp::Ordering::Less => {
            let mut left_operand = Box::new(FunctionalInput::Binary(binary_input));
            let mut right_operand = Box::new(functional_input);

            if swap {
                std::mem::swap(&mut left_operand, &mut right_operand);
            }

            Some(BinaryInput {
                left_operand,
                binary_operator,
                right_operand,
            })
        }
        std::cmp::Ordering::Equal => {
            let mut left_operand = Box::new(FunctionalInput::Binary(binary_input));
            let mut right_operand = Box::new(functional_input);

            if binary_operator.is_assignment() {
                std::mem::swap(&mut left_operand, &mut right_operand);
            }

            Some(BinaryInput {
                left_operand,
                binary_operator,
                right_operand,
            })
        }
        std::cmp::Ordering::Greater => None,
    }
}

fn binary_with(
    functional_strategy: impl Strategy<Value = FunctionalInput> + Clone,
) -> impl Strategy<Value = BinaryInput> {
    (
        functional_strategy.clone(),
        binary_operator(),
        functional_strategy,
    )
        .prop_filter_map(
            "disambiguate the syntax",
            |(left, binary_operator, right)| match (left, right) {
                (FunctionalInput::Binary(..), FunctionalInput::Binary(..)) => None,
                (FunctionalInput::Binary(left_operand), right_operand) => {
                    create_binary(binary_operator, left_operand, right_operand, false)
                }
                (left_operand, FunctionalInput::Binary(right_operand)) => {
                    create_binary(binary_operator, right_operand, left_operand, true)
                }
                (left_operand, right_operand) => Some(BinaryInput {
                    left_operand: Box::new(left_operand),
                    binary_operator,
                    right_operand: Box::new(right_operand),
                }),
            },
        )
}

fn function_call_with(
    expression_strategy: impl Strategy<Value = ExpressionInput>,
) -> impl Strategy<Value = FunctionCallInput> {
    (
        crate::syntax_tree::strategy::qualified_identifier(true),
        proptest::collection::vec(expression_strategy, 0..=4),
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

fn struct_literal_with(
    expression_strategy: impl Strategy<Value = ExpressionInput>,
) -> impl Strategy<Value = StructLiteralInput> {
    (
        crate::syntax_tree::strategy::qualified_identifier(true),
        proptest::collection::vec(
            (
                pernixc_lexical::token::strategy::identifier(),
                expression_strategy,
            ),
            0..=4,
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

fn block_with(
    expression_strategy: impl Strategy<Value = ExpressionInput> + Clone,
) -> impl Strategy<Value = BlockInput> {
    (
        proptest::option::of(pernixc_lexical::token::strategy::identifier()),
        proptest::collection::vec(
            crate::syntax_tree::statement::strategy::statement_with(expression_strategy),
            0..=4,
        ),
    )
        .prop_map(|(label, statements)| BlockInput { label, statements })
}

fn loop_with(
    expression_strategy: impl Strategy<Value = ExpressionInput> + Clone,
) -> impl Strategy<Value = LoopInput> {
    (
        proptest::option::of(pernixc_lexical::token::strategy::identifier()),
        proptest::collection::vec(
            crate::syntax_tree::statement::strategy::statement_with(expression_strategy),
            0..=4,
        ),
    )
        .prop_map(|(label, statements)| LoopInput { label, statements })
}

fn functional() -> impl Strategy<Value = FunctionalInput> {
    base_functional().prop_recursive(4, 16, 4, |inner| {
        prop_oneof![
            prefix_with(inner.clone()).prop_map(FunctionalInput::Prefix),
            member_access_with(inner.clone()).prop_map(FunctionalInput::MemberAccess),
            function_call_with(expression_with(inner.clone()))
                .prop_map(FunctionalInput::FunctionCall),
            struct_literal_with(expression_with(inner.clone()))
                .prop_map(FunctionalInput::StructLiteral),
            binary_with(inner.clone()).prop_map(FunctionalInput::Binary),
            expression_with(inner).prop_map(|x| (FunctionalInput::Parenthesized(
                ParenthesizedInput {
                    expression: Box::new(x)
                }
            )))
        ]
    })
}

fn return_with(
    functional_strategy: impl Strategy<Value = FunctionalInput>,
) -> impl Strategy<Value = ReturnInput> {
    proptest::option::of(functional_strategy).prop_map(|expression| ReturnInput { expression })
}

fn break_with(
    functional_strategy: impl Strategy<Value = FunctionalInput>,
) -> impl Strategy<Value = BreakInput> {
    (
        proptest::option::of(pernixc_lexical::token::strategy::identifier()),
        proptest::option::of(functional_strategy),
    )
        .prop_map(|(label, expression)| BreakInput { label, expression })
}

fn express_with(
    functional_strategy: impl Strategy<Value = FunctionalInput>,
) -> impl Strategy<Value = ExpressInput> {
    (
        proptest::option::of(pernixc_lexical::token::strategy::identifier()),
        proptest::option::of(functional_strategy),
    )
        .prop_map(|(label, expression)| ExpressInput { label, expression })
}

fn continue_with() -> impl Strategy<Value = ContinueInput> {
    proptest::option::of(pernixc_lexical::token::strategy::identifier())
        .prop_map(|label| ContinueInput { label })
}

fn expression_with(
    functional_strategy: impl Strategy<Value = FunctionalInput> + 'static,
) -> impl Strategy<Value = ExpressionInput> + Clone {
    functional_strategy
        .prop_map(ExpressionInput::Functional)
        .prop_recursive(4, 16, 4, move |inner| {
            prop_oneof![
                loop_with(inner.clone())
                    .prop_map(|x| ExpressionInput::Imperative(ImperativeInput::Loop(x))),
                block_with(inner.clone())
                    .prop_map(|x| ExpressionInput::Imperative(ImperativeInput::Block(x))),
                return_with(
                    inner
                        .clone()
                        .prop_filter_map("extract functional", |x| x.into_functional().ok())
                )
                .prop_map(|x| ExpressionInput::Terminator(TerminatorInput::Return(x))),
                break_with(
                    inner
                        .clone()
                        .prop_filter_map("extract functional", |x| x.into_functional().ok())
                )
                .prop_map(|x| ExpressionInput::Terminator(TerminatorInput::Break(x))),
                express_with(
                    inner.prop_filter_map("extract functional", |x| x.into_functional().ok())
                )
                .prop_map(|x| ExpressionInput::Terminator(TerminatorInput::Express(x))),
                continue_with()
                    .prop_map(|x| ExpressionInput::Terminator(TerminatorInput::Continue(x)))
            ]
        })
}

pub fn expression() -> impl Strategy<Value = ExpressionInput> + Clone {
    expression_with(functional())
}
