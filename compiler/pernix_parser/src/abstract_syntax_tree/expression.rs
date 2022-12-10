use pernix_lexer::token::LiteralConstantType;

use super::{BinaryOperator, PositiionWrapper, UnaryOperator};

/// Enumeration containing all possible expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression<'a> {
    /// Represents an expression of the form `left operator right`
    BinaryExpression {
        left: Box<PositiionWrapper<Expression<'a>>>,
        operator: PositiionWrapper<BinaryOperator>,
        right: Box<PositiionWrapper<Expression<'a>>>,
    },

    /// Represents an expression of the form `operator operand`
    UnaryExpression {
        operator: PositiionWrapper<UnaryOperator>,
        operand: Box<PositiionWrapper<Expression<'a>>>,
    },

    /// Represents an expression of the form `literal`
    LiteralExpression(LiteralConstantType<'a>),

    /// Represents an expression of the form `identifier`
    IdentifierExpression {
        identifier: PositiionWrapper<String>,
    },

    /// Represents an expression of the form `function_name(arguments)`
    FunctionCallExpression {
        function_name: PositiionWrapper<String>,
        arguments: Vec<PositiionWrapper<Expression<'a>>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// Represents a scope statement of the form `{ statements* }`
pub struct ScopeStatement<'a> {
    pub statements: Vec<PositiionWrapper<Statement<'a>>>,
}

/// Enumeration containing all kinds of statements
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement<'a> {
    /// Represents a statement of the form `return expression;`
    ReturnStatement {
        expression: PositiionWrapper<Expression<'a>>,
    },

    /// Represents a statement of the form `expression;`
    ExpressionStatement {
        expression: PositiionWrapper<Expression<'a>>,
    },

    /// Represents a statement of the form `let identifier = expression;`
    VariableDeclarationStatement {
        identifier: PositiionWrapper<String>,
        expression: PositiionWrapper<Expression<'a>>,
    },

    /// Represents an if statement of the form `if (condition) then_statement
    /// else else_statement`
    IfStatement {
        condition: PositiionWrapper<Expression<'a>>,
        then_statement: Box<PositiionWrapper<Statement<'a>>>,
        else_statement: Option<Box<PositiionWrapper<Statement<'a>>>>,
    },

    ScopeStatement(ScopeStatement<'a>),
}
