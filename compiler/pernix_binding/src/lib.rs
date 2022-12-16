use bound_ast::{
    bound_expression::{
        BoundExpression, BoundUnaryExpression, ExpressionCategory,
        ExpressionType,
    },
    bound_statement::{BoundBlockScopeStatement, BoundStatement},
};
use error::Error;
use pernix_parser::abstract_syntax_tree::{
    expression::{Expression, FunctionCallExpression, UnaryExpression},
    statement::{BlockScopeStatement, Statement},
    PositionWrapper, UnaryOperator,
};
use symbol::{
    table::{FunctionSymbolTable, TypeSymbolTable},
    FunctionSymbol, PrimitiveType, TypeSymbol,
};

use crate::bound_ast::bound_expression::BoundImplicitCastExpression;

pub mod bound_ast;
pub mod error;
pub mod scope;
pub mod symbol;

/// Represent a bound function definition with its body. The body is the list
/// of bound statements that will be executed when the function is called.
pub struct FunctionDefinition<'table, 'parser, 'ast> {
    function_symbol: FunctionSymbol<'table, 'parser, 'ast>,
    body: BoundBlockScopeStatement<'table, 'parser, 'ast>,
}

impl<'table, 'parser, 'ast> FunctionDefinition<'table, 'parser, 'ast> {
    /// Returns a reference to the function symbol of this [`FunctionDefinition`].
    pub fn function_symbol(&self) -> &FunctionSymbol<'table, 'parser, 'ast> {
        &self.function_symbol
    }

    /// Returns a reference to the body of this [`FunctionDefinition`].
    pub fn body(&self) -> &BoundBlockScopeStatement<'table, 'parser, 'ast> {
        &self.body
    }
}

/// Represent a binder that will bind the [`FunctionSymbol`] to the
/// [`FunctionDefinition`].
pub struct FunctionBinder<'sym, 'table, 'parser, 'ast> {
    type_table: &'table TypeSymbolTable,
    function_table: &'table FunctionSymbolTable<'table, 'parser, 'ast>,
    func_symbol: &'sym FunctionSymbol<'table, 'parser, 'ast>,
    errors: Vec<Error<'table, 'parser, 'ast>>,
}

impl<'sym, 'table, 'parser: 'ast, 'ast>
    FunctionBinder<'sym, 'table, 'parser, 'ast>
{
    /// Bind the [`FunctionSymbol`] to the [`FunctionDefinition`].
    pub fn bind(
        type_table: &'table TypeSymbolTable,
        function_table: &'table FunctionSymbolTable<'table, 'parser, 'ast>,
        func_symbol: &'sym FunctionSymbol<'table, 'parser, 'ast>,
    ) -> Result<
        FunctionDefinition<'table, 'parser, 'ast>,
        Vec<Error<'table, 'parser, 'ast>>,
    > {
        let mut binder = Self {
            type_table,
            function_table,
            func_symbol,
            errors: Vec::new(),
        };

        match binder.bind_block_scope_statement(PositionWrapper {
            position: func_symbol.ast().value.body.position.clone(),
            value: &func_symbol.ast().value.body.value,
        }) {
            Some(value) => {
                if binder.errors.is_empty() {
                    Ok(FunctionDefinition {
                        function_symbol: func_symbol.clone(),
                        body: value,
                    })
                } else {
                    Err(binder.errors)
                }
            }
            None => Err(binder.errors),
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    /// STATEMENT
    ////////////////////////////////////////////////////////////////////////////
    fn bind_statement(
        &mut self,
        statement: &'ast PositionWrapper<Statement<'parser>>,
    ) -> Option<BoundStatement<'table, 'parser, 'ast>> {
        todo!();
    }

    fn bind_block_scope_statement(
        &mut self,
        statement: PositionWrapper<&'ast BlockScopeStatement<'parser>>,
    ) -> Option<BoundBlockScopeStatement<'table, 'parser, 'ast>> {
        todo!();
    }

    ////////////////////////////////////////////////////////////////////////////
    /// EXPRESSION
    ////////////////////////////////////////////////////////////////////////////
    fn bind_expression(
        &mut self,
        expression: &'ast PositionWrapper<Expression<'parser>>,
    ) -> Option<BoundExpression<'table, 'parser, 'ast>> {
        todo!();
    }

    fn bind_call_expression(
        &mut self,
        expression: PositionWrapper<&'parser FunctionCallExpression<'ast>>,
    ) -> Option<BoundExpression<'table, 'parser, 'ast>> {
        
        let fn_call_symbol = self.function_table.lookup(
            self.func_symbol.scope_info(),
            expression.value.function_name.value,
        )?;

        todo!();
    }

    fn bind_unary_expression(
        &mut self,
        expression: PositionWrapper<&'ast UnaryExpression<'parser>>,
    ) -> Option<BoundExpression<'table, 'parser, 'ast>> {
        let bound_operand = self.bind_expression(&expression.value.operand)?;

        let expression_type = &match bound_operand.get_type().type_symbol {
            TypeSymbol::PrimitiveType(primitive_type) => match primitive_type {
                PrimitiveType::Bool => {
                    if expression.value.operator.value
                        != UnaryOperator::LogicalNot
                    {
                        self.errors.push(Error::InvalidUnaryOperation {
                            operand: &expression.value.operand,
                            operator: expression.value.operator.clone(),
                            operand_type: bound_operand.get_type().type_symbol,
                        });
                        return None;
                    }

                    self.type_table.get("bool")
                }
                PrimitiveType::Int8
                | PrimitiveType::Int16
                | PrimitiveType::Int32
                | PrimitiveType::Int64
                | PrimitiveType::Float32
                | PrimitiveType::Float64 => {
                    if expression.value.operator.value != UnaryOperator::Minus
                        && expression.value.operator.value
                            != UnaryOperator::Plus
                    {
                        self.errors.push(Error::InvalidUnaryOperation {
                            operand: &expression.value.operand,
                            operator: expression.value.operator.clone(),
                            operand_type: bound_operand.get_type().type_symbol,
                        });
                        return None;
                    }

                    match primitive_type {
                        PrimitiveType::Int8 => self.type_table.get("int8"),
                        PrimitiveType::Int16 => self.type_table.get("int16"),
                        PrimitiveType::Int32 => self.type_table.get("int32"),
                        PrimitiveType::Int64 => self.type_table.get("int64"),
                        PrimitiveType::Float32 => {
                            self.type_table.get("float32")
                        }
                        PrimitiveType::Float64 => {
                            self.type_table.get("float64")
                        }
                        _ => unreachable!(),
                    }
                }
                PrimitiveType::Uint8
                | PrimitiveType::Uint16
                | PrimitiveType::Uint32
                | PrimitiveType::Uint64 => {
                    if expression.value.operator.value != UnaryOperator::Plus {
                        self.errors.push(Error::InvalidUnaryOperation {
                            operand: &expression.value.operand,
                            operator: expression.value.operator.clone(),
                            operand_type: bound_operand.get_type().type_symbol,
                        });
                        return None;
                    }

                    match primitive_type {
                        PrimitiveType::Uint8 => self.type_table.get("uint8"),
                        PrimitiveType::Uint16 => self.type_table.get("uint16"),
                        PrimitiveType::Uint32 => self.type_table.get("uint32"),
                        PrimitiveType::Uint64 => self.type_table.get("uint64"),
                        _ => unreachable!(),
                    }
                }
                _ => {
                    self.errors.push(Error::InvalidUnaryOperation {
                        operand: &expression.value.operand,
                        operator: expression.value.operator.clone(),
                        operand_type: bound_operand.get_type().type_symbol,
                    });
                    return None;
                }
            },
        }
        .unwrap()
        .value;

        Some(BoundExpression::BoundUnaryExpression(
            BoundUnaryExpression {
                operand: Box::new(bound_operand),
                ast: expression,
                expression_type: ExpressionType {
                    type_symbol: expression_type,
                    category: ExpressionCategory::RValue,
                },
            },
        ))
    }

    ////////////////////////////////////////////////////////////////////////////
    /// UTILITY
    ////////////////////////////////////////////////////////////////////////////
    fn is_implicitly_castable(
        from: &'table TypeSymbol,
        to: &'table TypeSymbol,
    ) -> bool {
        if std::ptr::eq(from, to) {
            return true;
        }

        if from.is_numeric() && to.is_numeric() {
            /*
            RULES:
                - any integer types can be implicitly casted to any floating
                  point types
                - signed integer types can't be implicitly casted to unsigned
                  integer types
                - casting from a larger type to a smaller type is not allowed
                - casting from a floating point type to an integer type is not
                  allowed
             */

            if from.is_floating_point() && to.is_floating_point() {
                return true;
            }

            let from_is_signed = match from {
                TypeSymbol::PrimitiveType(PrimitiveType::Int8)
                | TypeSymbol::PrimitiveType(PrimitiveType::Int16)
                | TypeSymbol::PrimitiveType(PrimitiveType::Int32)
                | TypeSymbol::PrimitiveType(PrimitiveType::Int64)
                | TypeSymbol::PrimitiveType(PrimitiveType::Float32)
                | TypeSymbol::PrimitiveType(PrimitiveType::Float64) => true,

                _ => false,
            };

            let to_is_signed = match to {
                TypeSymbol::PrimitiveType(PrimitiveType::Int8)
                | TypeSymbol::PrimitiveType(PrimitiveType::Int16)
                | TypeSymbol::PrimitiveType(PrimitiveType::Int32)
                | TypeSymbol::PrimitiveType(PrimitiveType::Int64)
                | TypeSymbol::PrimitiveType(PrimitiveType::Float32)
                | TypeSymbol::PrimitiveType(PrimitiveType::Float64) => true,

                _ => false,
            };

            if from_is_signed && !to_is_signed {
                return false;
            }

            let from_size = match from {
                TypeSymbol::PrimitiveType(PrimitiveType::Int8)
                | TypeSymbol::PrimitiveType(PrimitiveType::Uint8) => 8,

                TypeSymbol::PrimitiveType(PrimitiveType::Int16)
                | TypeSymbol::PrimitiveType(PrimitiveType::Uint16) => 16,

                TypeSymbol::PrimitiveType(PrimitiveType::Int32)
                | TypeSymbol::PrimitiveType(PrimitiveType::Uint32)
                | TypeSymbol::PrimitiveType(PrimitiveType::Float32) => 32,

                TypeSymbol::PrimitiveType(PrimitiveType::Int64)
                | TypeSymbol::PrimitiveType(PrimitiveType::Uint64)
                | TypeSymbol::PrimitiveType(PrimitiveType::Float64) => 64,

                _ => unreachable!(),
            };

            let to_size = match to {
                TypeSymbol::PrimitiveType(PrimitiveType::Int8)
                | TypeSymbol::PrimitiveType(PrimitiveType::Uint8) => 8,

                TypeSymbol::PrimitiveType(PrimitiveType::Int16)
                | TypeSymbol::PrimitiveType(PrimitiveType::Uint16) => 16,

                TypeSymbol::PrimitiveType(PrimitiveType::Int32)
                | TypeSymbol::PrimitiveType(PrimitiveType::Uint32)
                | TypeSymbol::PrimitiveType(PrimitiveType::Float32) => 32,

                TypeSymbol::PrimitiveType(PrimitiveType::Int64)
                | TypeSymbol::PrimitiveType(PrimitiveType::Uint64)
                | TypeSymbol::PrimitiveType(PrimitiveType::Float64) => 64,

                _ => unreachable!(),
            };

            if from.is_integer() && to.is_integer() {
                return from_size < to_size;
            } else if from.is_floating_point() && to.is_floating_point() {
                return from_size < to_size;
            }

            false
        } else {
            false
        }
    }

    fn pass_expression_to(
        &mut self,
        expression: &'parser PositionWrapper<Expression<'ast>>,
        expected_type: &'table TypeSymbol,
    ) -> Option<BoundExpression<'table, 'parser, 'ast>> {
        let bound_expression = self.bind_expression(expression)?;

        if std::ptr::eq(bound_expression.get_type().type_symbol, expected_type)
        {
            return Some(bound_expression);
        } else if Self::is_implicitly_castable(
            bound_expression.get_type().type_symbol,
            expected_type,
        ) {
            return Some(BoundExpression::BoundImplicitCastExpression(
                BoundImplicitCastExpression {
                    ast: expression,
                    operand: Box::new(bound_expression),
                    expression_type: ExpressionType {
                        type_symbol: expected_type,
                        category: ExpressionCategory::RValue,
                    },
                },
            ));
        }

        self.errors.push(Error::TypeMismatched {
            expected_type,
            expression_type: bound_expression.get_type().type_symbol,
            expression,
        });

        None
    }
}

#[cfg(test)]
mod test;
