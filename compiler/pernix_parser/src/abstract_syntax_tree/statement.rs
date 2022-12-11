use super::{expression::Expression, PositionWrapper};

/// Represent a statement that declares a variable
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableDeclarationStatement<'a> {
    pub identifier: PositionWrapper<&'a str>,
    pub expression: PositionWrapper<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// Represent an if-else statement controlling the flow of execution
pub struct IfStatement<'a> {
    pub condition: PositionWrapper<Expression<'a>>,
    pub then_statement: Vec<PositionWrapper<Statement<'a>>>,
    pub else_statement: Option<Vec<PositionWrapper<Statement<'a>>>>,
}

/// Represent an enumeration containing all kinds of statements
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement<'a> {
    ReturnStatement(Option<PositionWrapper<Expression<'a>>>),
    ExpressionStatement(PositionWrapper<Expression<'a>>),
    VariableDeclarationStatement(VariableDeclarationStatement<'a>),
    IfStatement(IfStatement<'a>),
}
