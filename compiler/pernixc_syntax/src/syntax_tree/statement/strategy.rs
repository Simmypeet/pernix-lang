use proptest::{prop_assert_eq, prop_oneof, strategy::Strategy, test_runner::TestCaseError};

use super::{Declarative, Expressive, Semi, Statement, VariableDeclaration};
use crate::syntax_tree::{
    expression::strategy::{ExpressionInput, FunctionalInput, ImperativeInput},
    strategy::TypeSpecifierInput,
};

/// Represents an input for [`super::Statement`]
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum StatementInput {
    Expressive(ExpressiveInput),
    Declarative(DeclarativeInput),
}

impl ToString for StatementInput {
    fn to_string(&self) -> String {
        match self {
            Self::Expressive(expressive) => expressive.to_string(),
            Self::Declarative(declarative) => declarative.to_string(),
        }
    }
}

impl StatementInput {
    /// Validates the input against the [`Statement`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Statement) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Expressive(i), Statement::Expressive(o)) => i.validate(o),
            (Self::Declarative(i), Statement::Declarative(o)) => i.validate(o),
            _ => Err(TestCaseError::fail("Invalid statement input")),
        }
    }
}

/// Represents an input for [`super::Expressive`]
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum ExpressiveInput {
    Semi(SemiInput),
    Imperative(ImperativeInput),
}

impl ToString for ExpressiveInput {
    fn to_string(&self) -> String {
        match self {
            Self::Semi(semi) => semi.to_string(),
            Self::Imperative(imperative) => imperative.to_string(),
        }
    }
}

impl ExpressiveInput {
    /// Validates the input against the [`Expressive`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Expressive) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Semi(i), Expressive::Semi(o)) => i.validate(o),
            (Self::Imperative(i), Expressive::Imperative(o)) => i.validate(o),
            _ => Err(TestCaseError::fail("Invalid expressive input")),
        }
    }
}

/// Represents an input for [`super::Declarative`]
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum DeclarativeInput {
    VariableDeclaration(VariableDeclarationInput),
}

impl ToString for DeclarativeInput {
    fn to_string(&self) -> String {
        match self {
            Self::VariableDeclaration(variable_declaration) => variable_declaration.to_string(),
        }
    }
}

impl DeclarativeInput {
    /// Validates the input against the [`Declarative`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Declarative) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::VariableDeclaration(i), Declarative::VariableDeclaration(o)) => i.validate(o),
        }
    }
}

/// Represents an input for [`super::VariableDeclaration`]
#[derive(Debug, Clone)]
pub struct VariableDeclarationInput {
    /// Whether the variable is mutable or not
    pub mutable: bool,

    /// The identifier of the variable
    pub identifier: String,

    /// The optional type annotation for the variable
    pub type_specifier: Option<TypeSpecifierInput>,

    /// The expression initializer for the variable
    pub expression: ExpressionInput,
}

impl ToString for VariableDeclarationInput {
    fn to_string(&self) -> String {
        let mut string = String::from("let ");

        if self.mutable {
            string.push_str("mutable ");
        }

        string.push_str(&self.identifier);

        if let Some(type_specifier) = &self.type_specifier {
            string.push_str(": ");
            string.push_str(&type_specifier.to_string());
        }

        string.push_str(" = ");
        string.push_str(&self.expression.to_string());
        string.push(';');

        string
    }
}

impl VariableDeclarationInput {
    /// Validates the input against the [`VariableDeclaration`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &VariableDeclaration) -> Result<(), TestCaseError> {
        prop_assert_eq!(self.mutable, output.mutable_keyword.is_some());
        prop_assert_eq!(&self.identifier, output.identifier.span.str());

        match (&self.type_specifier, &output.type_annotation) {
            (Some(input), Some(output)) => input.validate(&output.type_specifier)?,
            (None, None) => {}
            _ => return Err(TestCaseError::fail("type specifier mismatch")),
        }

        self.expression.validate(&output.expression)
    }
}

/// Represents an input for [`super::Semi`]
#[derive(Debug, Clone)]
pub struct SemiInput {
    /// The expression to be evaluated
    pub expression: FunctionalInput,
}

impl ToString for SemiInput {
    fn to_string(&self) -> String { format!("{};", self.expression.to_string()) }
}

impl SemiInput {
    /// Validates the input against the [`Semi`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Semi) -> Result<(), TestCaseError> {
        self.expression.validate(&output.expression)
    }
}

pub(crate) fn variable_declaration_with(
    expression_strategy: impl Strategy<Value = ExpressionInput>,
) -> impl Strategy<Value = VariableDeclarationInput> {
    (
        proptest::bool::ANY,
        pernixc_lexical::token::strategy::identifier(),
        proptest::option::of(crate::syntax_tree::strategy::type_specifier()),
        expression_strategy,
    )
        .prop_map(|(mutable, identifier, type_specifier, expression)| {
            VariableDeclarationInput {
                mutable,
                identifier,
                type_specifier,
                expression,
            }
        })
}

pub(crate) fn expressive_with(
    expression_strategy: impl Strategy<Value = ExpressionInput>,
) -> impl Strategy<Value = ExpressiveInput> {
    expression_strategy.prop_map(|expression| match expression {
        ExpressionInput::Functional(expression) => ExpressiveInput::Semi(SemiInput { expression }),
        ExpressionInput::Imperative(expression) => ExpressiveInput::Imperative(expression),
    })
}

pub(crate) fn statement_with(
    expression_strategy: impl Strategy<Value = ExpressionInput> + Clone,
) -> impl Strategy<Value = StatementInput> {
    prop_oneof![
        variable_declaration_with(expression_strategy.clone())
            .prop_map(|x| StatementInput::Declarative(DeclarativeInput::VariableDeclaration(x))),
        expressive_with(expression_strategy).prop_map(StatementInput::Expressive),
    ]
}

/// Returns a strategy that generates a random [`StatementInput`].
pub fn statement() -> impl Strategy<Value = StatementInput> {
    statement_with(crate::syntax_tree::expression::strategy::expression())
}
