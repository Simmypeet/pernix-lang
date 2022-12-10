use super::{BinaryOperator, PositiionWrapper, UnaryOperator};

/// Enumeration containing all possible expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    /// Represents an expression of the form `left operator right`
    BinaryExpression {
        left: Box<PositiionWrapper<Expression>>,
        operator: PositiionWrapper<BinaryOperator>,
        right: Box<PositiionWrapper<Expression>>,
    },

    /// Represents an expression of the form `operator operand`
    UnaryExpression {
        operator: PositiionWrapper<UnaryOperator>,
        operand: Box<PositiionWrapper<Expression>>,
    },

    /// Represents an expression of the form `literal`
    LiteralExpression(PositiionWrapper<String>),

    /// Represents an expression of the form `identifier`
    IdentifierExpression {
        identifier: PositiionWrapper<String>,
    },

    /// Represents an expression of the form `function_name(arguments)`
    FunctionCallExpression {
        function_name: PositiionWrapper<String>,
        arguments: Vec<PositiionWrapper<Expression>>,
    },

    /// Represents an expression of the form `(expression)`
    ParenthesizedExpression {
        expression: Box<PositiionWrapper<Expression>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// Represents a scope statement of the form `{ statements* }`
pub struct ScopeStatement {
    pub statements: Vec<PositiionWrapper<Statement>>,
}

/// Enumeration containing all kinds of statements
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    /// Represents a statement of the form `return expression;`
    ReturnStatement {
        expression: PositiionWrapper<Expression>,
    },

    /// Represents a statement of the form `expression;`
    ExpressionStatement {
        expression: PositiionWrapper<Expression>,
    },

    /// Represents a statement of the form `let identifier = expression;`
    VariableDeclarationStatement {
        identifier: PositiionWrapper<String>,
        expression: PositiionWrapper<Expression>,
    },

    /// Represents an if statement of the form `if (condition) then_statement
    /// else else_statement`
    IfStatement {
        condition: PositiionWrapper<Expression>,
        then_statement: Box<PositiionWrapper<Statement>>,
        else_statement: Option<Box<PositiionWrapper<Statement>>>,
    },

    ScopeStatement(ScopeStatement),
}
