use std::rc::Rc;

use pernix_lexer::token::LiteralConstantToken;
use pernix_parser::abstract_syntax_tree::{
    expression::{
        BinaryExpression, Expression, FunctionCallExpression,
        IdentifierExpression, LiteralExpression, UnaryExpression,
    },
    statement::{
        BlockScopeStatement, IfElseStatement, ReturnStatement, Statement,
        VariableDeclarationStatement, WhileStatement,
    },
    BinaryOperator, PositionWrapper, UnaryOperator,
};

use crate::{
    error::Error,
    scope::LockScopeStack,
    symbol::{
        table::{FunctionSymbolTable, TypeSymbolTable},
        FunctionSymbol, PrimitiveType, TypeSymbol, VariableSymbol,
    },
};

use super::{
    bound_expression::{
        BoundBinaryExpression, BoundCastExpression, BoundExpression,
        BoundFunctionCallExpression, BoundIdentifierExpression,
        BoundLiteralExpression, BoundUnaryExpression, ExpressionCategory,
        ExpressionType,
    },
    bound_statement::{
        BoundBlockScopeStatement, BoundIfElseStatement, BoundReturnStatement,
        BoundStatement, BoundVariableDeclarationStatement, BoundWhileStatement,
    },
};

pub(crate) struct Binder<'table, 'ast> {
    type_table: &'table TypeSymbolTable,
    function_table: &'table FunctionSymbolTable<'table, 'ast>,
    function_symbol: &'table FunctionSymbol<'table, 'ast>,
    local_scope_stack: LockScopeStack<'ast, Rc<VariableSymbol<'table, 'ast>>>,
    errors: Vec<Error<'table, 'ast>>,
}

impl<'table: 'ast, 'ast> Binder<'table, 'ast> {
    /// Bind the given function symbol to a [`BoundStatement`]. The bound
    /// statement returned by the function is the [`BoundStatement::BoundBlockScopeStatement`]
    /// variant.
    pub(crate) fn bind(
        function_symbol: &'table FunctionSymbol<'table, 'ast>,
        function_table: &'table FunctionSymbolTable<'table, 'ast>,
        type_table: &'table TypeSymbolTable,
    ) -> Result<BoundStatement<'table, 'ast>, Vec<Error<'table, 'ast>>> {
        let mut binder =
            Binder::new(type_table, function_table, &function_symbol);
        let mut statements = Vec::new();
        binder.local_scope_stack.push();

        for argument in function_symbol.parameters() {
            binder
                .local_scope_stack
                .declare_variable(argument.name, argument.clone());
        }

        for statement in &function_symbol.ast().value.body.value.statements {
            statements.push(match binder.bind_statement(statement, false) {
                Some(val) => val,
                None => continue,
            });
        }

        binder.local_scope_stack.pop();

        if !binder.errors.is_empty() {
            return Err(binder.pop_errors());
        } else {
            Ok(BoundStatement::BoundBlockScopeStatement(
                BoundBlockScopeStatement {
                    ast: PositionWrapper {
                        position: function_symbol
                            .ast()
                            .value
                            .body
                            .position
                            .clone(),
                        value: &function_symbol.ast().value.body.value,
                    },
                    statements,
                },
            ))
        }
    }

    /// Create a new [`Binder`] instance.
    fn new(
        type_table: &'table TypeSymbolTable,
        function_table: &'table FunctionSymbolTable<'table, 'ast>,
        function_symbol: &'table FunctionSymbol<'table, 'ast>,
    ) -> Self {
        Self {
            type_table,
            function_table,
            function_symbol,
            local_scope_stack: LockScopeStack::new(),
            errors: Vec::new(),
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    /// STATEMENT
    ////////////////////////////////////////////////////////////////////////////

    /// Bind the given statement AST to a [`BoundStatement`].
    fn bind_statement(
        &mut self,
        statement: &'ast PositionWrapper<Statement<'ast>>,
        is_in_loop: bool,
    ) -> Option<BoundStatement<'table, 'ast>> {
        match &statement.value {
            Statement::ReturnStatement(return_statement) => self
                .bind_return_statement(PositionWrapper {
                    value: return_statement,
                    position: statement.position.clone(),
                }),
            Statement::ExpressionStatement(expr_statement) => {
                let bound_expression = self.bind_expression(expr_statement)?;
                Some(BoundStatement::BoundExpressionStatement(bound_expression))
            }
            Statement::VariableDeclarationStatement(
                variable_decl_statement,
            ) => self.bind_variable_declaration_statement(PositionWrapper {
                value: variable_decl_statement,
                position: statement.position.clone(),
            }),
            Statement::IfElseStatement(if_else_statement) => self
                .bind_if_else_statement(
                    PositionWrapper {
                        value: if_else_statement,
                        position: statement.position.clone(),
                    },
                    is_in_loop,
                ),
            Statement::BlockScopeStatement(block_scope_statement) => self
                .bind_block_scope_statement(
                    PositionWrapper {
                        value: &block_scope_statement,
                        position: statement.position.clone(),
                    },
                    is_in_loop,
                ),
            Statement::WhileStatement(while_statement) => self
                .bind_while_statement(PositionWrapper {
                    value: while_statement,
                    position: statement.position.clone(),
                }),
            Statement::BreakStatement | Statement::ContinueStatement => {
                let bound_statement = match &statement.value {
                    Statement::BreakStatement => {
                        BoundStatement::BoundBreakStatement
                    }
                    Statement::ContinueStatement => {
                        BoundStatement::BoundContinueStatement
                    }
                    _ => unreachable!(),
                };

                if is_in_loop {
                    Some(bound_statement)
                } else {
                    self.errors.push(Error::InvalidLoopFlowStatement {
                        invalid_statement: statement,
                    });
                    None
                }
            }
        }
    }

    fn bind_while_statement(
        &mut self,
        while_statement: PositionWrapper<&'ast WhileStatement<'ast>>,
    ) -> Option<BoundStatement<'table, 'ast>> {
        let bound_condition = self.pass_expression_to(
            &while_statement.value.condition,
            &self
                .type_table
                .get_primitive_type(PrimitiveType::Bool)
                .value,
        )?;

        let bound_loop_statement =
            self.bind_statement(&while_statement.value.loop_statement, true)?;

        Some(BoundStatement::BoundWhileStatement(BoundWhileStatement {
            ast: while_statement,
            condition: bound_condition,
            loop_statement: Box::new(bound_loop_statement),
        }))
    }

    fn bind_if_else_statement(
        &mut self,
        if_else_statement: PositionWrapper<&'ast IfElseStatement<'ast>>,
        is_in_loop: bool,
    ) -> Option<BoundStatement<'table, 'ast>> {
        let bound_condition = self.pass_expression_to(
            &if_else_statement.value.condition,
            &self
                .type_table
                .get_primitive_type(PrimitiveType::Bool)
                .value,
        )?;

        let bound_then_statement = self.bind_statement(
            &if_else_statement.value.then_statement,
            is_in_loop,
        )?;

        let bound_else_statement = if let Some(ref else_statement) =
            if_else_statement.value.else_statement
        {
            Some(self.bind_statement(else_statement, is_in_loop)?)
        } else {
            None
        };

        Some(BoundStatement::BoundIfElseStatement(BoundIfElseStatement {
            ast: if_else_statement,
            condition: bound_condition,
            then_statement: Box::new(bound_then_statement),
            else_statement: match bound_else_statement {
                Some(val) => Some(Box::new(val)),
                None => None,
            },
        }))
    }

    /// Bind the given variable declaration statement AST to a [`BoundStatement`].
    fn bind_variable_declaration_statement(
        &mut self,
        variable_declaration_statement: PositionWrapper<
            &'ast VariableDeclarationStatement<'ast>,
        >,
    ) -> Option<BoundStatement<'table, 'ast>> {
        let bound_expression = if let Some(ref type_annotation) =
            variable_declaration_statement.value.type_annotation
        {
            let variable_type =
                match self.type_table.lookup_from_type_annotation(
                    self.function_symbol.scope_info(),
                    type_annotation,
                ) {
                    Ok(variable_type) => {
                        if variable_type.value.is_void() {
                            self.errors.push(Error::DefinedVoidVariable {
                                position: variable_declaration_statement
                                    .position
                                    .clone(),
                            });
                            return None;
                        }

                        &variable_type.value
                    }
                    Err(err) => {
                        self.errors.push(err);
                        return None;
                    }
                };

            self.pass_expression_to(
                &variable_declaration_statement.value.expression,
                variable_type,
            )?
        } else {
            let expr = self.bind_expression(
                &variable_declaration_statement.value.expression,
            )?;

            if expr.get_type().type_symbol.is_void() {
                self.errors.push(Error::DefinedVoidVariable {
                    position: variable_declaration_statement.position.clone(),
                });
                return None;
            }

            expr
        };

        let symbol = Rc::new(VariableSymbol {
            variable_type: bound_expression.get_type().type_symbol,
            name: variable_declaration_statement.value.identifier.value,
            is_mutable: variable_declaration_statement.value.is_mutable,
        });

        self.local_scope_stack
            .declare_variable(symbol.name, symbol.clone());

        Some(BoundStatement::BoundVariableDeclarationStatement(
            BoundVariableDeclarationStatement {
                ast: variable_declaration_statement.clone(),
                variable_symbol: symbol,
                expression: bound_expression,
            },
        ))
    }

    /// Bind the given block scope statement AST to a [`BoundStatement`].
    fn bind_block_scope_statement(
        &mut self,
        block_scope_statement: PositionWrapper<&'ast BlockScopeStatement<'ast>>,
        is_in_loop: bool,
    ) -> Option<BoundStatement<'table, 'ast>> {
        self.local_scope_stack.push();
        let mut statements = Vec::new();

        for statement in &block_scope_statement.value.statements {
            if let Some(bound_statement) =
                self.bind_statement(statement, is_in_loop)
            {
                statements.push(bound_statement);
            }
        }

        self.local_scope_stack.pop();

        Some(BoundStatement::BoundBlockScopeStatement(
            BoundBlockScopeStatement {
                ast: block_scope_statement.clone(),
                statements,
            },
        ))
    }

    /// Bind the given return statement AST to a [`BOundStatement`].
    fn bind_return_statement(
        &mut self,
        return_statement: PositionWrapper<&'ast ReturnStatement<'ast>>,
    ) -> Option<BoundStatement<'table, 'ast>> {
        match &return_statement.value.expression {
            Some(val) => {
                let bound_expression = self.bind_expression(val)?;

                if !std::ptr::eq(
                    self.function_symbol.return_type(),
                    bound_expression.get_type().type_symbol,
                ) {
                    self.errors.push(Error::TypeMismatched {
                        expected_type: self.function_symbol.return_type(),
                        expression_type: bound_expression
                            .get_type()
                            .type_symbol,
                        expression: val,
                    });

                    None
                } else {
                    Some(BoundStatement::BoundReturnStatement(
                        BoundReturnStatement {
                            ast: return_statement.clone(),
                            expression: Some(bound_expression),
                        },
                    ))
                }
            }
            None => {
                // return type must be void
                match self.function_symbol.return_type() {
                    TypeSymbol::PrimitiveType(PrimitiveType::Void) => {
                        Some(BoundStatement::BoundReturnStatement(
                            BoundReturnStatement {
                                ast: return_statement.clone(),
                                expression: None,
                            },
                        ))
                    }
                    _ => {
                        self.errors.push(
                            Error::ReturnStatementMustReturnAValue {
                                return_statement: return_statement.clone(),
                            },
                        );
                        None
                    }
                }
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    /// EXPRESSION
    ////////////////////////////////////////////////////////////////////////////

    /// Bind the given expression AST to a [`BoundExpression`].
    fn bind_expression(
        &mut self,
        expression: &'ast PositionWrapper<Expression<'ast>>,
    ) -> Option<BoundExpression<'table, 'ast>> {
        match &expression.value {
            Expression::BinaryExpression(binary_expr) => self
                .bind_binary_expression(PositionWrapper {
                    value: &binary_expr,
                    position: expression.position.clone(),
                }),
            Expression::UnaryExpression(unary_expr) => self
                .bind_unary_expression(PositionWrapper {
                    value: &unary_expr,
                    position: expression.position.clone(),
                }),
            Expression::LiteralExpression(lit_expr) => self
                .bind_literal_expression(PositionWrapper {
                    value: &lit_expr,
                    position: expression.position.clone(),
                }),
            Expression::IdentifierExpression(ident_expr) => self
                .bind_identifier_expression(PositionWrapper {
                    value: &ident_expr,
                    position: expression.position.clone(),
                }),
            Expression::FunctionCallExpression(call_expr) => self
                .bind_function_call_expression(PositionWrapper {
                    value: &call_expr,
                    position: expression.position.clone(),
                }),
        }
    }

    /// Bind the given binary expression AST to a [`BoundExpression`].
    fn bind_binary_expression(
        &mut self,
        expression: PositionWrapper<&'ast BinaryExpression<'ast>>,
    ) -> Option<BoundExpression<'table, 'ast>> {
        let mut left = self.bind_expression(&expression.value.left)?;
        let mut right = self.bind_expression(&expression.value.right)?;

        if Self::is_assignment(expression.value.operator.value) {
            match left.get_type().category {
                ExpressionCategory::LValue { is_mutable: true } => {}
                _ => {
                    match left.get_type().category {
                        ExpressionCategory::RValue => {
                            self.errors.push(Error::RValueAssignment {
                                rvalue_expression: &expression.value.left,
                            })
                        }
                        ExpressionCategory::LValue { .. } => {
                            self.errors.push(Error::IsNotMutable {
                                lvalue_expression: &expression.value.left,
                                mutate_expression: &expression.value.right,
                            })
                        }
                    }
                    return None;
                }
            }

            // if the left and right types are not the same, we need to perform
            // an implicit cast (if possible)
            if !std::ptr::eq(
                left.get_type().type_symbol,
                right.get_type().type_symbol,
            ) {
                let implicit_castable = Self::is_implicitly_castable(
                    right.get_type().type_symbol,
                    left.get_type().type_symbol,
                );

                if !implicit_castable {
                    self.errors.push(Error::TypeMismatched {
                        expected_type: left.get_type().type_symbol,
                        expression_type: right.get_type().type_symbol,
                        expression: &expression.value.right,
                    });
                    return None;
                }

                right =
                    BoundExpression::BoundCastExpression(BoundCastExpression {
                        ast: &expression.value.right,
                        operand: Box::new(right),
                        expression_type: ExpressionType {
                            type_symbol: left.get_type().type_symbol,
                            category: ExpressionCategory::RValue,
                        },
                    });
            }

            Some(BoundExpression::BoundBinaryExpression(
                BoundBinaryExpression {
                    expression_type: ExpressionType {
                        type_symbol: left.get_type().type_symbol,
                        category: ExpressionCategory::LValue {
                            is_mutable: true,
                        },
                    },
                    ast: expression.clone(),
                    left: Box::new(left),
                    right: Box::new(right),
                },
            ))
        } else if left.get_type().type_symbol.is_numeric()
            && right.get_type().type_symbol.is_numeric()
        {
            if std::ptr::eq(
                left.get_type().type_symbol,
                right.get_type().type_symbol,
            ) {
                Some(BoundExpression::BoundBinaryExpression(
                    BoundBinaryExpression {
                        ast: expression.clone(),
                        expression_type: ExpressionType {
                            type_symbol: left.get_type().type_symbol,
                            category: ExpressionCategory::RValue,
                        },
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                ))
            } else {
                // if the left and right types are not the same, we need to perform
                // usual arithmetic conversion
                let left_type = match &left.get_type().type_symbol {
                    TypeSymbol::PrimitiveType(prim) => prim,
                };
                let right_type = match &right.get_type().type_symbol {
                    TypeSymbol::PrimitiveType(prim) => prim,
                };

                let left_rank = Self::get_operand_rank(*left_type);
                let right_rank = Self::get_operand_rank(*right_type);

                let highest_rank = std::cmp::max(left_rank, right_rank);
                let convert_type = match highest_rank {
                    0 | 1 => PrimitiveType::Int32,
                    2 => PrimitiveType::Uint32,
                    3 => PrimitiveType::Int64,
                    4 => PrimitiveType::Uint64,
                    5 => PrimitiveType::Float32,
                    6 => PrimitiveType::Float64,
                    _ => unreachable!(),
                };

                if *left_type != convert_type {
                    left = BoundExpression::BoundCastExpression(
                        BoundCastExpression {
                            ast: &expression.value.left,
                            operand: Box::new(left),
                            expression_type: ExpressionType {
                                type_symbol: &self
                                    .type_table
                                    .get_primitive_type(convert_type)
                                    .value,
                                category: ExpressionCategory::RValue,
                            },
                        },
                    );
                }

                if *right_type != convert_type {
                    right = Self::cast_unchecked(
                        right,
                        &expression.value.right,
                        &self.type_table.get_primitive_type(convert_type).value,
                    )
                }

                if *left_type != convert_type {
                    left = Self::cast_unchecked(
                        left,
                        &expression.value.left,
                        &self.type_table.get_primitive_type(convert_type).value,
                    )
                }

                Some(BoundExpression::BoundBinaryExpression(
                    BoundBinaryExpression {
                        ast: expression.clone(),
                        expression_type: ExpressionType {
                            type_symbol: &self
                                .type_table
                                .get_primitive_type(convert_type)
                                .value,
                            category: ExpressionCategory::RValue,
                        },
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                ))
            }
        } else if left.get_type().type_symbol.is_bool()
            && right.get_type().type_symbol.is_bool()
        {
            match expression.value.operator.value {
                BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => {
                    Some(BoundExpression::BoundBinaryExpression(
                        BoundBinaryExpression {
                            ast: expression.clone(),
                            expression_type: ExpressionType {
                                type_symbol: left.get_type().type_symbol,
                                category: ExpressionCategory::RValue,
                            },
                            left: Box::new(left),
                            right: Box::new(right),
                        },
                    ))
                }
                _ => {
                    self.errors.push(Error::InvalidBinaryOperation {
                        binary_expression: expression.clone(),
                    });
                    None
                }
            }
        } else {
            self.errors.push(Error::InvalidBinaryOperation {
                binary_expression: expression.clone(),
            });
            None
        }
    }

    fn is_assignment(operator: BinaryOperator) -> bool {
        match operator {
            BinaryOperator::Assignment
            | BinaryOperator::CompoundAddition
            | BinaryOperator::CompoundSubtraction
            | BinaryOperator::CompoundMultiplication
            | BinaryOperator::CompoundDivision
            | BinaryOperator::CompoundRemainder => true,
            _ => false,
        }
    }

    /// Bind the given literal expression AST to a [`BoundExpression`].
    fn bind_literal_expression(
        &mut self,
        expression: PositionWrapper<&'ast LiteralExpression<'ast>>,
    ) -> Option<BoundExpression<'table, 'ast>> {
        match expression.value.literal_expression {
            LiteralConstantToken::Number {
                value: _,
                literal_suffix,
                is_decimal,
            } => {
                if literal_suffix.is_none() {
                    let primitive_type = if is_decimal {
                        PrimitiveType::Float64
                    } else {
                        PrimitiveType::Int32
                    };

                    Some(BoundExpression::BoundLiteralExpression(
                        BoundLiteralExpression {
                            ast: expression.clone(),
                            expression_type: ExpressionType {
                                type_symbol: &self
                                    .type_table
                                    .get_primitive_type(primitive_type)
                                    .value,
                                category: ExpressionCategory::RValue,
                            },
                        },
                    ))
                } else {
                    let type_symbol = &self
                        .type_table
                        .get_primitive_type(match literal_suffix.unwrap() {
                            "u8" => PrimitiveType::Uint8,
                            "u16" => PrimitiveType::Uint16,
                            "u32" => PrimitiveType::Uint32,
                            "u64" => PrimitiveType::Uint64,
                            "i8" => PrimitiveType::Int8,
                            "i16" => PrimitiveType::Int16,
                            "i32" => PrimitiveType::Int32,
                            "i64" => PrimitiveType::Int64,
                            "f32" => PrimitiveType::Float32,
                            "f64" => PrimitiveType::Float64,
                            _ => {
                                self.errors.push(
                                    Error::UndefinedLiteralSuffix {
                                        literal_expression: expression.clone(),
                                    },
                                );
                                return None;
                            }
                        })
                        .value;

                    // decimal literals can't have integer suffixes
                    if is_decimal && type_symbol.is_integer() {
                        self.errors.push(Error::UndefinedLiteralSuffix {
                            literal_expression: expression.clone(),
                        });
                        return None;
                    }

                    Some(BoundExpression::BoundLiteralExpression(
                        BoundLiteralExpression {
                            ast: expression.clone(),
                            expression_type: ExpressionType {
                                type_symbol,
                                category: ExpressionCategory::RValue,
                            },
                        },
                    ))
                }
            }

            LiteralConstantToken::Boolean(_) => {
                Some(BoundExpression::BoundLiteralExpression(
                    BoundLiteralExpression {
                        ast: expression.clone(),
                        expression_type: ExpressionType {
                            type_symbol: &self
                                .type_table
                                .get_primitive_type(PrimitiveType::Bool)
                                .value,
                            category: ExpressionCategory::RValue,
                        },
                    },
                ))
            }
        }
    }

    /// Bind the given unary expression AST to a [`BoundExpression`].
    fn bind_unary_expression(
        &mut self,
        expression: PositionWrapper<&'ast UnaryExpression<'ast>>,
    ) -> Option<BoundExpression<'table, 'ast>> {
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

                    self.type_table.get_primitive_type(PrimitiveType::Bool)
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
                        PrimitiveType::Int8 => self
                            .type_table
                            .get_primitive_type(PrimitiveType::Int8),
                        PrimitiveType::Int16 => self
                            .type_table
                            .get_primitive_type(PrimitiveType::Int16),
                        PrimitiveType::Int32 => self
                            .type_table
                            .get_primitive_type(PrimitiveType::Int32),
                        PrimitiveType::Int64 => self
                            .type_table
                            .get_primitive_type(PrimitiveType::Int64),
                        PrimitiveType::Float32 => self
                            .type_table
                            .get_primitive_type(PrimitiveType::Float32),
                        PrimitiveType::Float64 => self
                            .type_table
                            .get_primitive_type(PrimitiveType::Float64),
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
                        PrimitiveType::Uint8 => self
                            .type_table
                            .get_primitive_type(PrimitiveType::Uint8),
                        PrimitiveType::Uint16 => self
                            .type_table
                            .get_primitive_type(PrimitiveType::Uint16),
                        PrimitiveType::Uint32 => self
                            .type_table
                            .get_primitive_type(PrimitiveType::Uint32),
                        PrimitiveType::Uint64 => self
                            .type_table
                            .get_primitive_type(PrimitiveType::Uint64),
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

    /// Bind the given identifier expression AST to a [`BoundExpression`].
    fn bind_identifier_expression(
        &mut self,
        expression: PositionWrapper<&'ast IdentifierExpression<'ast>>,
    ) -> Option<BoundExpression<'table, 'ast>> {
        match self
            .local_scope_stack
            .lookup_variable(expression.value.identifier)
        {
            Some(variable) => {
                let expression_type = ExpressionType {
                    type_symbol: variable.variable_type,
                    category: ExpressionCategory::LValue {
                        is_mutable: variable.is_mutable,
                    },
                };

                Some(BoundExpression::BoundIdentifierExpression(
                    BoundIdentifierExpression {
                        ast: expression,
                        variable_symbol: variable.clone(),
                        expression_type,
                    },
                ))
            }
            None => {
                self.errors.push(Error::UndefinedVariable {
                    variable_name: expression.clone(),
                });
                None
            }
        }
    }

    /// Bind the given function call expression AST to a [`BoundExpression`].
    fn bind_function_call_expression(
        &mut self,
        expression: PositionWrapper<&'ast FunctionCallExpression<'ast>>,
    ) -> Option<BoundExpression<'table, 'ast>> {
        // Lookup the function in the function table by its name.
        let fn_call_symbol = match self.function_table.lookup(
            self.function_symbol.scope_info(),
            expression.value.function_name.value,
        ) {
            Some(val) => &val.value,
            None => {
                self.errors.push(Error::UndefinedFunctionName {
                    function_call_expression: expression.clone(),
                });
                return None;
            }
        };

        // Check the number of arguments in the function call matches the number
        // of parameters in the function declaration.
        let expected_argument_count = fn_call_symbol.parameters().len();
        let supplied_argument_count = expression.value.arguments.len();
        if expected_argument_count != supplied_argument_count {
            self.errors.push(Error::InvalidArgumentCount {
                function_declaration: fn_call_symbol.ast().clone(),
                function_call_expression: expression.clone(),
                expected_argument_count,
                supplied_argument_count,
            });
            return None;
        }

        // Check each argument type matches the parameter type.
        let mut bound_arguments = Vec::new();
        for (idx, expected_type) in
            fn_call_symbol.parameters().iter().enumerate()
        {
            // perform any valid implicit conversions (if needed)
            let bound_expression = self.pass_expression_to(
                &expression.value.arguments[idx],
                expected_type.variable_type,
            )?;

            bound_arguments.push(bound_expression);
        }

        Some(BoundExpression::BoundFunctionCallExpression(
            BoundFunctionCallExpression {
                ast: expression.clone(),
                arguments: bound_arguments,
                function: fn_call_symbol,
                expression_type: ExpressionType {
                    type_symbol: fn_call_symbol.return_type(),
                    category: ExpressionCategory::RValue,
                },
            },
        ))
    }

    ////////////////////////////////////////////////////////////////////////////
    /// UTILITY
    ////////////////////////////////////////////////////////////////////////////
    fn pass_expression_to(
        &mut self,
        expression: &'ast PositionWrapper<Expression<'ast>>,
        expected_type: &'table TypeSymbol,
    ) -> Option<BoundExpression<'table, 'ast>> {
        let bound_expression = self.bind_expression(expression)?;

        if std::ptr::eq(bound_expression.get_type().type_symbol, expected_type)
        {
            return Some(bound_expression);
        }

        // by default, an integer literal with no literal suffix is an int32.
        // however, if the expected type is not int32, we can just straight up
        // change the bound expression's type to the expected type. The same
        // goes for decimal literals
        if let BoundExpression::BoundLiteralExpression(literal) =
            &bound_expression
        {
            if let LiteralConstantToken::Number {
                value: _,
                literal_suffix,
                is_decimal,
            } = literal.ast.value.literal_expression
            {
                if literal_suffix.is_none()
                    && ((expected_type.is_numeric() && !is_decimal)
                        || (expected_type.is_floating_point()) && is_decimal)
                {
                    return Some(BoundExpression::BoundLiteralExpression(
                        BoundLiteralExpression {
                            ast: literal.ast.clone(),
                            expression_type: ExpressionType {
                                type_symbol: expected_type,
                                category: ExpressionCategory::RValue,
                            },
                        },
                    ));
                }
            }
        }
        if Self::is_implicitly_castable(
            bound_expression.get_type().type_symbol,
            expected_type,
        ) {
            return Some(BoundExpression::BoundCastExpression(
                BoundCastExpression {
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

    /// Get the rank of an operand in the context of usual arithmetic conversions.
    fn get_operand_rank(prim_type: PrimitiveType) -> i32 {
        match prim_type {
            PrimitiveType::Int32 => 1,
            PrimitiveType::Uint32 => 2,
            PrimitiveType::Int64 => 3,
            PrimitiveType::Uint64 => 4,
            PrimitiveType::Float32 => 5,
            PrimitiveType::Float64 => 6,
            _ => 0,
        }
    }

    /// Check if a type can be implicitly casted to another type.
    fn is_implicitly_castable(from: &TypeSymbol, to: &TypeSymbol) -> bool {
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

    fn cast_unchecked(
        bound_expression: BoundExpression<'table, 'ast>,
        expression_ast: &'ast PositionWrapper<Expression<'ast>>,
        target_type: &'table TypeSymbol,
    ) -> BoundExpression<'table, 'ast> {
        if let BoundExpression::BoundLiteralExpression(literal) =
            &bound_expression
        {
            if let LiteralConstantToken::Number {
                value: _,
                literal_suffix,
                is_decimal,
            } = literal.ast.value.literal_expression
            {
                if literal_suffix.is_none()
                    && ((target_type.is_numeric() && !is_decimal)
                        || (target_type.is_floating_point()) && is_decimal)
                {
                    return BoundExpression::BoundLiteralExpression(
                        BoundLiteralExpression {
                            ast: literal.ast.clone(),
                            expression_type: ExpressionType {
                                type_symbol: target_type,
                                category: ExpressionCategory::RValue,
                            },
                        },
                    );
                }
            }
        }

        return BoundExpression::BoundCastExpression(BoundCastExpression {
            ast: expression_ast,
            operand: Box::new(bound_expression),
            expression_type: ExpressionType {
                type_symbol: target_type,
                category: ExpressionCategory::RValue,
            },
        });
    }

    fn pop_errors(&mut self) -> Vec<Error<'table, 'ast>> {
        std::mem::take(&mut self.errors)
    }
}
