use std::sync::Arc;

use pernix_parser::abstract_syntax_tree::{
    statement::{
        BlockScopeStatement, IfElseStatement, ReturnStatement,
        VariableDeclarationStatement, WhileStatement,
    },
    PositionWrapper,
};

use crate::{
    error::Error,
    symbol::{
        table::{FunctionSymbolTable, TypeSymbolTable},
        FunctionSymbol, VariableSymbol,
    },
};

use super::{binder::Binder, bound_expression::BoundExpression};

/// Represent an enumeration containing all the possible bound statements.
#[derive(Clone, Debug)]
pub enum BoundStatement<'table, 'ast> {
    BoundExpressionStatement(BoundExpression<'table, 'ast>),
    BoundBlockScopeStatement(BoundBlockScopeStatement<'table, 'ast>),
    BoundReturnStatement(BoundReturnStatement<'table, 'ast>),
    BoundVariableDeclarationStatement(
        BoundVariableDeclarationStatement<'table, 'ast>,
    ),
    BoundIfElseStatement(BoundIfElseStatement<'table, 'ast>),
    BoundWhileStatement(BoundWhileStatement<'table, 'ast>),
    BoundBreakStatement,
    BoundContinueStatement,
}

impl<'table: 'ast, 'ast> BoundStatement<'table, 'ast> {
    /// Bind the given function symbol to a [`BoundStatement`].
    pub fn bind(
        function_symbol: &'table FunctionSymbol<'table, 'ast>,
        function_table: &'table FunctionSymbolTable<'table, 'ast>,
        type_table: &'table TypeSymbolTable,
    ) -> Result<BoundStatement<'table, 'ast>, Vec<Error<'table, 'ast>>> {
        Binder::bind(function_symbol, function_table, type_table)
    }
}

/// Represent a bound version of a [`ScopeStatement`] AST.
#[derive(Clone, Debug)]
pub struct BoundBlockScopeStatement<'table, 'ast> {
    pub ast: PositionWrapper<&'ast BlockScopeStatement<'ast>>,
    pub statements: Vec<BoundStatement<'table, 'ast>>,
}

/// Represent a bound version of a [`Statement::ReturnStatement`] AST.
#[derive(Clone, Debug)]
pub struct BoundReturnStatement<'table, 'ast> {
    pub ast: PositionWrapper<&'ast ReturnStatement<'ast>>,
    pub expression: Option<BoundExpression<'table, 'ast>>,
}

/// Represent a bound version of a [`VariableDeclarationStatement`] AST.
#[derive(Clone, Debug)]
pub struct BoundVariableDeclarationStatement<'table, 'ast> {
    pub ast: PositionWrapper<&'ast VariableDeclarationStatement<'ast>>,
    pub variable_symbol: Arc<VariableSymbol<'table, 'ast>>,
    pub expression: BoundExpression<'table, 'ast>,
}

/// Represent a bound version of a [`IfElseStatement`] AST.
#[derive(Clone, Debug)]
pub struct BoundIfElseStatement<'table, 'ast> {
    pub ast: PositionWrapper<&'ast IfElseStatement<'ast>>,
    pub condition: BoundExpression<'table, 'ast>,
    pub then_statement: Box<BoundStatement<'table, 'ast>>,
    pub else_statement: Option<Box<BoundStatement<'table, 'ast>>>,
}

/// Represent a bound version of a [`WhileStatement`] AST.
#[derive(Clone, Debug)]
pub struct BoundWhileStatement<'table, 'ast> {
    pub ast: PositionWrapper<&'ast WhileStatement<'ast>>,
    pub condition: BoundExpression<'table, 'ast>,
    pub loop_statement: Box<BoundStatement<'table, 'ast>>,
}
