use super::{expression::ExpressionAST, PositionWrapper};

/// Reprsent an if-else statement abstract syntax tree node.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// IfElseStatement:
///     'if' '(' Expression ')' Statement ('else' Statement)?;
/// ```
#[derive(Clone)]
pub struct IfElseStatementAST<'src> {
    pub condition: PositionWrapper<ExpressionAST<'src>>,
    pub then_statement: Box<PositionWrapper<StatementAST<'src>>>,
    pub else_statement: Option<Box<PositionWrapper<StatementAST<'src>>>>,
}

/// Reprsent a while statement abstract syntax tree node.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// WhileStatement:
///     'while' '(' Expression ')' Statement;
/// ```
#[derive(Clone)]
pub struct WhileStatementAST<'src> {
    pub condition: PositionWrapper<ExpressionAST<'src>>,
    pub statement: Box<PositionWrapper<StatementAST<'src>>>,
}

/// Reprsent a variable declaration statement abstract syntax tree node.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// VariableDeclarationStatement:
///     ('mutable' | 'let') Identifier '=' Expression ';';
/// ```
#[derive(Clone)]
pub struct VariableDeclarationStatementAST<'src> {
    pub variable_name: PositionWrapper<&'src str>,
    pub is_mutable: bool,
    pub expression: PositionWrapper<ExpressionAST<'src>>,
}

/// Reprsent a return statement abstract syntax tree node.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// ReturnStatement:
///     'return' Expression? ';';
/// ```
#[derive(Clone)]
pub struct ReturnStatementAST<'src> {
    pub expression: PositionWrapper<ExpressionAST<'src>>,
}

/// Reprsent a block statement abstract syntax tree node contain
///
/// ANTLR4 grammar:
///
/// ``` txt
/// BlockScopeStatement:
///     '{' Statement* '}';
/// ```
#[derive(Clone)]
pub struct BlockScopeStatementAST<'src> {
    pub statements: Vec<PositionWrapper<StatementAST<'src>>>,
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
#[derive(Clone)]
pub enum StatementAST<'src> {
    IfElseStatement(IfElseStatementAST<'src>),
    WhileStatement(WhileStatementAST<'src>),
    ExpressionStatement(ExpressionAST<'src>),
    VariableDeclarationStatement(VariableDeclarationStatementAST<'src>),
    ReturnStatement(ReturnStatementAST<'src>),
    BlockScopeStatement(BlockScopeStatementAST<'src>),
    BreakStatement,
    ContinueStatement,
}
