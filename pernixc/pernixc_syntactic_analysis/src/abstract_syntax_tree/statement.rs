use super::{expression::Expression, PositionWrapper};

/// Reprsent an if-else statement abstract syntax tree node.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// IfElseStatement:
///     'if' '(' Expression ')' Statement ('else' Statement)?;
/// ```
pub struct IfElseStatement<'src> {
    pub condition: PositionWrapper<Expression<'src>>,
    pub then_statement: Box<PositionWrapper<Statement<'src>>>,
    pub else_statement: Option<Box<PositionWrapper<Statement<'src>>>>,
}

/// Reprsent a while statement abstract syntax tree node.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// WhileStatement:
///     'while' '(' Expression ')' Statement;
/// ```
pub struct WhileStatement<'src> {
    pub condition: PositionWrapper<Expression<'src>>,
    pub statement: Box<PositionWrapper<Statement<'src>>>,
}

/// Reprsent a variable declaration statement abstract syntax tree node.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// VariableDeclarationStatement:
///     ('mutable' | 'let') Identifier '=' Expression ';';
pub struct VariableDeclarationStatement<'src> {
    pub variable_name: PositionWrapper<&'src str>,
    pub is_mutable: bool,
    pub expression: PositionWrapper<Expression<'src>>,
}

/// Reprsent a return statement abstract syntax tree node.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// ReturnStatement:
///     'return' Expression? ';';
pub struct ReturnStatement<'src> {
    pub expression: PositionWrapper<Expression<'src>>,
}

/// Reprsent a block statement abstract syntax tree node contain
///
/// ANTLR4 grammar:
///
/// ``` txt
/// BlockScopeStatement:
///     '{' Statement* '}';
/// ```
pub struct BlockScopeStatement<'src> {
    pub statements: Vec<PositionWrapper<Statement<'src>>>,
}

/// Represent an enumeration of all possible statement abstract syntax tree nodes.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// Statement:
///     IfElseStatement
///     | WhileStatement
///     | (Expression ';')
///     | VariableDeclarationStatement
///     | ReturnStatement
///     | BlockScopeStatement
///     | ('break' ';')
///     | ('continue' ';');
/// ```
pub enum Statement<'src> {
    IfElseStatement(IfElseStatement<'src>),
    WhileStatement(WhileStatement<'src>),
    ExpressionStatement(Expression<'src>),
    VariableDeclarationStatement(VariableDeclarationStatement<'src>),
    ReturnStatement(ReturnStatement<'src>),
    BlockScopeStatement(BlockScopeStatement<'src>),
    BreakStatement,
    ContinueStatement,
}
