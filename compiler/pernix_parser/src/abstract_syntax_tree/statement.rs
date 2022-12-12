use super::{declaration::Type, expression::Expression, PositionWrapper};

/// Represent a statement that declares a variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableDeclarationStatement<'a> {
    pub type_annotation: Option<PositionWrapper<Type<'a>>>,
    pub is_mutable: bool,
    pub identifier: PositionWrapper<&'a str>,
    pub expression: PositionWrapper<Expression<'a>>,
}

/// Represent an if-else statement controlling the flow of execution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStatement<'a> {
    pub condition: PositionWrapper<Expression<'a>>,
    pub then_statement: Box<PositionWrapper<Statement<'a>>>,
    pub else_statement: Option<Box<PositionWrapper<Statement<'a>>>>,
}

/// Represent a scope delimited by curly braces containing statements.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope<'a> {
    pub statements: Vec<PositionWrapper<Statement<'a>>>,
}

/// Represent an enumeration containing all kinds of statements.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement<'a> {
    ReturnStatement(Option<PositionWrapper<Expression<'a>>>),
    ExpressionStatement(PositionWrapper<Expression<'a>>),
    VariableDeclarationStatement(VariableDeclarationStatement<'a>),
    IfStatement(IfStatement<'a>),
    Scope(Scope<'a>),
}
