use super::{
    declaration::TypeAnnotation, expression::Expression, PositionWrapper,
};

/// Represent a statement that declares a variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableDeclarationStatement<'a> {
    pub type_annotation: Option<PositionWrapper<TypeAnnotation<'a>>>,
    pub is_mutable: bool,
    pub identifier: PositionWrapper<&'a str>,
    pub expression: PositionWrapper<Expression<'a>>,
}

/// Represent an if-else statement controlling the flow of execution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfElseStatement<'a> {
    pub condition: PositionWrapper<Expression<'a>>,
    pub then_statement: Box<PositionWrapper<Statement<'a>>>,
    pub else_statement: Option<Box<PositionWrapper<Statement<'a>>>>,
}

/// Represent a scope delimited by curly braces containing statements.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockScopeStatement<'a> {
    pub statements: Vec<PositionWrapper<Statement<'a>>>,
}

/// Represent a return statement that returns an expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnStatement<'a> {
    pub expression: Option<PositionWrapper<Expression<'a>>>,
}

/// Represent a while loop statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhileStatement<'a> {
    pub condition: PositionWrapper<Expression<'a>>,
    pub loop_statement: Box<PositionWrapper<Statement<'a>>>,
}

/// Represent an enumeration containing all kinds of statements.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement<'a> {
    ReturnStatement(ReturnStatement<'a>),
    ExpressionStatement(PositionWrapper<Expression<'a>>),
    VariableDeclarationStatement(VariableDeclarationStatement<'a>),
    IfElseStatement(IfElseStatement<'a>),
    BlockScopeStatement(BlockScopeStatement<'a>),
    WhileStatement(WhileStatement<'a>),
    BreakStatement,
    ContinueStatement,
}
