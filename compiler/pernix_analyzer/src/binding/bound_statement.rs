use std::rc::Rc;

use pernix_parser::abstract_syntax_tree::{
    statement::{
        BlockScopeStatement, IfElseStatement, ReturnStatement,
        VariableDeclarationStatement,
    },
    PositionWrapper,
};

use crate::symbol::VariableSymbol;

use super::bound_expression::BoundExpression;

/// Represent an enumeration containing all the possible bound statements.
#[derive(Clone, Debug)]
pub enum BoundStatement<'table, 'parser, 'ast> {
    BoundExpressionStatement(BoundExpression<'table, 'parser, 'ast>),
    BoundBlockScopeStatement(BoundBlockScopeStatement<'table, 'parser, 'ast>),
    BoundReturnStatement(BoundReturnStatement<'table, 'parser, 'ast>),
    BoundVariableDeclarationStatement(
        BoundVariableDeclarationStatement<'table, 'parser, 'ast>,
    ),
    BoundIfElseStatement(BoundIfElseStatement<'table, 'parser, 'ast>),
}

/// Represent a bound version of a [`ScopeStatement`] AST.
#[derive(Clone, Debug)]
pub struct BoundBlockScopeStatement<'table, 'parser, 'ast> {
    pub ast: PositionWrapper<&'parser BlockScopeStatement<'ast>>,
    pub statements: Vec<BoundStatement<'table, 'parser, 'ast>>,
}

/// Represent a bound version of a [`Statement::ReturnStatement`] AST.
#[derive(Clone, Debug)]
pub struct BoundReturnStatement<'table, 'parser, 'ast> {
    pub ast: PositionWrapper<&'parser ReturnStatement<'ast>>,
    pub expression: Option<BoundExpression<'table, 'parser, 'ast>>,
}

/// Represent a bound version of a [`VariableDeclarationStatement`] AST.
#[derive(Clone, Debug)]
pub struct BoundVariableDeclarationStatement<'table, 'parser, 'ast> {
    pub ast: PositionWrapper<&'parser VariableDeclarationStatement<'ast>>,
    pub variable_symbol: Rc<VariableSymbol<'table, 'ast>>,
    pub expression: BoundExpression<'table, 'parser, 'ast>,
}

/// Represent a bound version of a [`IfElseStatement`] AST.
#[derive(Clone, Debug)]
pub struct BoundIfElseStatement<'table, 'parser, 'ast> {
    pub ast: PositionWrapper<&'parser IfElseStatement<'ast>>,
    pub condition: BoundExpression<'table, 'parser, 'ast>,
    pub then_statement: Box<BoundStatement<'table, 'parser, 'ast>>,
    pub else_statement: Option<Box<BoundStatement<'table, 'parser, 'ast>>>,
}
