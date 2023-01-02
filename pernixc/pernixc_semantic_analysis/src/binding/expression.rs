use std::collections::HashMap;

use pernixc_lexical_analysis::token::LiteralConstantToken;
use pernixc_syntactic_analysis::abstract_syntax_tree::{
    expression::{BinaryOperator, UnaryOperator},
    PositionWrapper,
};

use crate::symbol_table::{SymbolID, TypeAnnotationSymbol, TypeUnitSymbol, VariableSymbol};

/// Represent a bound version of the
/// [`UnaryOperator`](pernixc_syntactic_analysis::abstract_syntax_tree::expression::UnaryOperator)
/// AST.
#[derive(Clone)]
pub struct UnaryExpression<'src> {
    pub operator: PositionWrapper<UnaryOperator>,
    pub operand: Box<PositionWrapper<Expression<'src>>>,
    pub expression_type: ExpressionTypeAnnotation,
}

/// Represent a bound version of the
/// [`BinaryOperator`](pernixc_syntactic_analysis::abstract_syntax_tree::expression::BinaryOperator)
/// AST.
#[derive(Clone)]
pub struct BinaryExpression<'src> {
    pub operator: PositionWrapper<BinaryOperator>,
    pub left: Box<PositionWrapper<Expression<'src>>>,
    pub right: Box<PositionWrapper<Expression<'src>>>,
    pub expression_type: ExpressionTypeAnnotation,
}

/// Represent a bound version of the
/// [`LiteralExpressionAST`](pernixc_syntactic_analysis::abstract_syntax_tree::expression::LiteralExpressionAST)
/// AST.
#[derive(Clone)]
pub struct LiteralExpression<'src> {
    pub literal: PositionWrapper<LiteralConstantToken<'src>>,
    pub type_annotation_symbol: TypeAnnotationSymbol,
}

/// Represent an expression that yields the value by referencing a particular variable by its name.
#[derive(Clone)]
pub struct VariableLoadExpression {
    pub variable_symbol: VariableSymbol,
}

/// Represent a bound version of the
/// [`FunctionCallExpressionAST`](pernixc_syntactic_analysis::abstract_syntax_tree::expression::FunctionCallExpressionAST)
/// AST.
#[derive(Clone)]
pub struct FunctionCallExpression<'src> {
    pub arguments: Vec<PositionWrapper<Expression<'src>>>,
    pub function_overload_id: FunctionOverloadID,
    pub type_annotation_symbol: TypeAnnotationSymbol,
}

/// Represent a bound version of the
/// [`ClassInstantiationExpressionAST`](pernixc_syntactic_analysis::abstract_syntax_tree::expression::ClassInstantiationExpressionAST)
/// AST.
#[derive(Clone)]
pub struct ClassInstantiationExpression<'src> {
    pub class_symbol_id: SymbolID,
    pub instantiation_arguments: HashMap<&'src str, Expression<'src>>,
}

/// Represent a bound version of the
/// [`MemberFieldAccessExpressionAST`](pernixc_syntactic_analysis::abstract_syntax_tree::expression::MemberFieldAccessExpressionAST)
/// AST.
#[derive(Clone)]
pub struct MemberFieldAccessExpression<'src> {
    pub expression: Box<PositionWrapper<Expression<'src>>>,
    pub field_name: PositionWrapper<&'src str>,
    pub field_type_annotation_symbol: TypeAnnotationSymbol,
}

/// Is a unique identifier of a particular function overload.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct FunctionOverloadID {
    pub overload_set_symbol_id: SymbolID,
    pub overload_index: usize,
}

/// Indicate the type of a particular expression.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct ExpressionTypeAnnotation {
    pub type_annotation_symbol: TypeAnnotationSymbol,
    pub expression_category: ExpressionCategory,
}

/// Represent an enumeration containing all the possible categories of an expression.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub enum ExpressionCategory {
    /// Represent an expression that doesn't have the memory storage.
    RValue,

    /// Represent an expression that has the memory storage.
    LValue { is_mutable: bool },
}

/// Represent an expression AST node with additional semantic information attached to it.
#[derive(Clone)]
pub enum Expression<'src> {
    UnaryExpression(UnaryExpression<'src>),
    BinaryExpression(BinaryExpression<'src>),
    LiteralExpression(LiteralExpression<'src>),
    VariableLoadExpression(VariableLoadExpression),
    FunctionCallExpression(FunctionCallExpression<'src>),
    ClassInstantiationExpression(ClassInstantiationExpression<'src>),
    MemberFieldAccessExpression(MemberFieldAccessExpression<'src>),
}

impl<'src> Expression<'src> {
    /// Get the expression type annotation of this expression.
    pub fn get_expression_type(&self) -> ExpressionTypeAnnotation {
        match self {
            Expression::UnaryExpression(expr) => expr.expression_type,
            Expression::BinaryExpression(expr) => expr.expression_type,
            Expression::LiteralExpression(expr) => ExpressionTypeAnnotation {
                type_annotation_symbol: expr.type_annotation_symbol,
                expression_category: ExpressionCategory::RValue,
            },
            Expression::VariableLoadExpression(expr) => ExpressionTypeAnnotation {
                type_annotation_symbol: expr
                    .variable_symbol
                    .qualified_type_annotation_symbol
                    .type_annotation_symbol,
                expression_category: ExpressionCategory::LValue {
                    is_mutable: expr
                        .variable_symbol
                        .qualified_type_annotation_symbol
                        .is_mutable,
                },
            },
            Expression::FunctionCallExpression(expr) => ExpressionTypeAnnotation {
                type_annotation_symbol: expr.type_annotation_symbol,
                expression_category: ExpressionCategory::RValue,
            },
            Expression::ClassInstantiationExpression(expr) => ExpressionTypeAnnotation {
                type_annotation_symbol: TypeAnnotationSymbol::TypeUnitSymbol(
                    TypeUnitSymbol::UserDefinedTypeUnit(expr.class_symbol_id),
                ),
                expression_category: ExpressionCategory::RValue,
            },
            Expression::MemberFieldAccessExpression(expr) => ExpressionTypeAnnotation {
                type_annotation_symbol: expr.field_type_annotation_symbol,
                expression_category: ExpressionCategory::LValue {
                    is_mutable: match expr
                        .expression
                        .value
                        .get_expression_type()
                        .expression_category
                    {
                        // temporary storage for the result of the expression
                        ExpressionCategory::RValue => true,

                        // the expression is already a memory storage
                        ExpressionCategory::LValue { is_mutable } => is_mutable,
                    },
                },
            },
        }
    }
}
