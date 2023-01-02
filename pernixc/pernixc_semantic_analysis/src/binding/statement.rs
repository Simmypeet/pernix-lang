use pernixc_syntactic_analysis::abstract_syntax_tree::PositionWrapper;

use crate::symbol_table::VariableSymbol;

use super::expression::Expression;

/// Represent an enumeration containing all the possible bound statements.
#[derive(Clone)]
pub enum Statement<'src> {
    ExpressionStatement(Expression<'src>),
    BlockScopeStatement(BlockScopeStatement<'src>),
    ReturnStatement(ReturnStatement<'src>),
    VariableDeclarationStatement(VariableDeclarationStatement<'src>),
    IfElseStatement(IfElseStatement<'src>),
    WhileStatement(WhileStatement<'src>),
    BreakStatement,
    ContinueStatement,
}

/// Represent a bound version of the
/// ['BlockScopeStatementAST`](pernixc_syntactic_analysis::abstract_syntax_tree::statement::BlockScopeStatementAST)
/// AST.
#[derive(Clone)]
pub struct BlockScopeStatement<'src> {
    pub statements: Vec<PositionWrapper<Statement<'src>>>,
}

/// Represent a bound version of the
/// ['ReturnStatementAST`](pernixc_syntactic_analysis::abstract_syntax_tree::statement::ReturnStatementAST)
/// AST.
#[derive(Clone)]
pub struct ReturnStatement<'src> {
    pub expression: Option<PositionWrapper<Expression<'src>>>,
}

/// Represent a bound version of the
/// ['VariableDeclarationStatementAST`](pernixc_syntactic_analysis::abstract_syntax_tree::statement::VariableDeclarationStatementAST)
/// AST.
#[derive(Clone)]
pub struct VariableDeclarationStatement<'src> {
    pub variable_name: PositionWrapper<&'src str>,
    pub variable_symbol: VariableSymbol,
    pub expression: PositionWrapper<Expression<'src>>,
}

/// Represent a bound version of the
/// ['IfElseStatementAST`](pernixc_syntactic_analysis::abstract_syntax_tree::statement::IfElseStatementAST)
/// AST.
#[derive(Clone)]
pub struct IfElseStatement<'src> {
    pub condition: PositionWrapper<Expression<'src>>,
    pub then_statement: Box<PositionWrapper<Statement<'src>>>,
    pub else_statement: Option<Box<PositionWrapper<Statement<'src>>>>,
}

/// Represent a bound version of the
/// ['WhileStatementAST`](pernixc_syntactic_analysis::abstract_syntax_tree::statement::WhileStatementAST)
/// AST.
#[derive(Clone)]
pub struct WhileStatement<'src> {
    pub condition: PositionWrapper<Expression<'src>>,
    pub loop_statement: Box<Statement<'src>>,
}
