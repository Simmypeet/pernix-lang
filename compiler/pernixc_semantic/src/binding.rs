use std::sync::Arc;

use derive_more::From;
use pernixc_common::source_file::SourceFile;
use pernixc_syntax::syntax_tree::{
    expression::{
        Binary, BinaryOperatorSyntaxTree, BooleanLiteral, ExpressionSyntaxTree, FunctionCall,
        Functional, Imperative, Named, NumericLiteralSyntaxTree, Prefix, PrefixOperator,
        StructLiteral,
    },
    SourceElement,
};
use thiserror::Error;

use self::{
    expression::{
        BinaryOperator, BooleanLiteralBinding, CastBinding, ExpressionBinding,
        ExpressionBindingTrait, ExpressionCategory, FunctionCallBinding,
        IdentifierExpressionBinding, PrefixExpressionBinding, PrefixOperator, StructLiteralBinding,
    },
    name_stack::NameStack,
};
use crate::{
    binding::expression::{
        BinaryExpressionBinding, ExpressionType, IdentifierExpressionBindingKind,
        NumericLiteralBinding, NumericLiteralValue,
    },
    errors::{
        AmbiguousConversionInBinaryOperation, ArgumentCountMismatch, AssignToImmutable,
        AssignToRValue, ExpressionExpected, InvalidBinaryOperation, InvalidNumericLiteralSuffix,
        InvalidPrefixOperation, OutOfRangeNumericLiteral, SemanticError, StructExpected,
        SymbolIsNotCallable, TypeMismatch,
    },
    symbol::{
        ty::{PrimitiveType, Type},
        FunctionSymbol, GlobalSymbol, GlobalSymbolTable, VariableSymbol,
    },
    SourceSpan,
};

pub mod expression;
pub mod name_stack;
pub mod statement;

#[derive(Debug, Clone, Error, From)]
pub enum FunctionBindError {
    #[error("Invalid `function_symbol_index` argument.")]
    InvalidFunctionSymbolIndex,

    #[error("{0}")]
    Semantic(SemanticError),
}

pub struct FunctionBinding {}

pub struct FunctionBinder<'a> {
    symbol_table: &'a GlobalSymbolTable,
    function_symbol: &'a FunctionSymbol,
    current_block_id: usize,
    variable_symbols: Vec<VariableSymbol>,
    variable_name_stack: NameStack<usize>,
    errors: Vec<SemanticError>,
}

impl<'a> FunctionBinder<'a> {
    fn bind_expression(
        &mut self,
        exression_syntax: &ExpressionSyntaxTree,
    ) -> Option<ExpressionBinding> {
        match exression_syntax {
            ExpressionSyntaxTree::Functional(expr) => match expr {
                Functional::NumericLiteralSyntaxTree(expr) => self.bind_numeric_literal(expr),
                Functional::BooleanLiteral(expr) => self.bind_boolean_literal(expr),
                Functional::Binary(expr) => self.bind_binary(expr),
                Functional::Prefix(expr) => self.bind_prefix(expr),
                Functional::Named(expr) => self.bind_identifier(expr),
                Functional::FunctionCall(expr) => self.bind_function_call(expr),
                Functional::Parenthesized(expr) => self.bind_expression(&expr.expression),
                Functional::StructLiteral(expr) => todo!(),
                Functional::MemberAccess(..) => todo!(),
                Functional::Continue(..) => todo!(),
                Functional::Break(..) => todo!(),
                Functional::Return(..) => todo!(),
                Functional::Express(..) => todo!(),
                Functional::Cast(..) => todo!(),
            },
            ExpressionSyntaxTree::Imperative(expr) => match expr {
                Imperative::Block(..) => todo!(),
                Imperative::IfElse(..) => todo!(),
                Imperative::Loop(..) => todo!(),
            },
        }
    }

    fn expect_expression(
        &mut self,
        expression_syntax: &ExpressionSyntaxTree,
        expected_type: Type,
    ) -> Option<ExpressionBinding> {
        let expression = self.bind_expression(expression_syntax)?;

        // try to perform implicit cast
        if expression.expression_type().ty != expected_type {
            if Self::is_implicitly_castable(expression.expression_type().ty, expected_type) {
                Some(ExpressionBinding::CastBinding(CastBinding {
                    span: expression.span().clone(),
                    expression: Box::new(expression),
                    target_type: expected_type,
                }))
            } else {
                self.errors.push(SemanticError::TypeMismatch(TypeMismatch {
                    expected: expected_type,
                    actual: expression.expression_type().ty,
                    span: expression.span().clone(),
                }));
                None
            }
        } else {
            Some(expression)
        }
    }

    fn is_implicitly_castable(from: Type, to: Type) -> bool {
        fn is_signed(ty: PrimitiveType) -> bool {
            matches!(
                ty,
                PrimitiveType::Int8
                    | PrimitiveType::Int16
                    | PrimitiveType::Int32
                    | PrimitiveType::Int64
                    | PrimitiveType::Float32
                    | PrimitiveType::Float64
            )
        }

        fn primitive_rank(ty: PrimitiveType) -> u32 {
            match ty {
                PrimitiveType::Int8 | PrimitiveType::Uint8 => 0,
                PrimitiveType::Int16 | PrimitiveType::Uint16 => 1,
                PrimitiveType::Int32 | PrimitiveType::Uint32 => 2,
                PrimitiveType::Int64 | PrimitiveType::Uint64 => 3,
                PrimitiveType::Float32 => 4,
                PrimitiveType::Float64 => 5,
                _ => 0,
            }
        }

        match (from, to) {
            (Type::Primitive(from_prim), Type::Primitive(to_prim))
                if from.is_arithmetic() && to.is_arithmetic() =>
            {
                let from_is_signed = is_signed(from_prim);
                let to_is_signed = is_signed(to_prim);

                // conversion from signed to unsigned is not allowed
                if from_is_signed && !to_is_signed {
                    return false;
                }

                let from_rank = primitive_rank(from_prim);
                let to_rank = primitive_rank(to_prim);

                // can convert from lower rank to higher rank to prevent overflow
                from_rank < to_rank
            }
            _ => false,
        }
    }

    fn source_file(&self) -> &'a Arc<SourceFile> { &self.function_symbol.syntax_tree.source_file }

    fn bind_struct_literal(
        &mut self,
        expression_syntax: &StructLiteral,
    ) -> Option<StructLiteralBinding> {
        let (struct_symbol, symbol_index) = self
            .symbol_table
            .resolve_symbol(
                self.function_symbol.parent_index,
                &self.function_symbol.syntax_tree.source_file,
                expression_syntax.qualified_identifier.elements(),
            )
            .map_or_else(
                |err| {
                    self.errors.push(err);
                    None
                },
                |idx| Some((self.symbol_table.get_by_index(idx).unwrap(), idx)),
            )?;

        let struct_symbol = if let GlobalSymbol::StructSymbol(struct_symbol) = struct_symbol {
            struct_symbol
        } else {
            self.errors
                .push(SemanticError::StructExpected(StructExpected {
                    span: SourceSpan::new(
                        self.function_symbol.syntax_tree.source_file.clone(),
                        expression_syntax.qualified_identifier.span(),
                    ),
                }));
            return None;
        };

        todo!()
    }

    fn bind_function_call(
        &mut self,
        expression_syntax: &FunctionCall,
    ) -> Option<ExpressionBinding> {
        let (symbol, symbol_idx) = self
            .symbol_table
            .resolve_symbol(
                self.function_symbol.parent_index,
                &self.function_symbol.syntax_tree.source_file,
                expression_syntax.qualified_identifier.elements(),
            )
            .map_or_else(
                |err| {
                    self.errors.push(err);
                    None
                },
                |idx| Some((self.symbol_table.get_by_index(idx).unwrap(), idx)),
            )?;

        // expect function symbol
        let function_symbol = if let GlobalSymbol::FunctionSymbol(func) = symbol {
            func
        } else {
            self.errors
                .push(SemanticError::SymbolIsNotCallable(SymbolIsNotCallable {
                    span: SourceSpan::new(
                        self.source_file().clone(),
                        expression_syntax.qualified_identifier.span(),
                    ),
                }));
            return None;
        };
        let arg_count = expression_syntax
            .arguments
            .as_ref()
            .map_or(0, |arg| arg.len());

        // check argument count
        if function_symbol.parameters.len() != arg_count {
            self.errors.push(SemanticError::ArgumentCountMismatch(
                ArgumentCountMismatch {
                    expected: function_symbol.parameters.len(),
                    found: arg_count,
                    span: SourceSpan::new(
                        self.source_file().clone(),
                        expression_syntax.qualified_identifier.span(),
                    ),
                },
            ));
            return None;
        }

        let arguments = if let Some(argument_syntaxes) = &expression_syntax.arguments {
            let mut found_error = false;
            let mut arguments = Vec::with_capacity(argument_syntaxes.len());

            for (argument_syntax, argument_type) in argument_syntaxes.elements().zip(
                function_symbol
                    .parameters
                    .iter()
                    .map(|x| x.type_binding_specifier.ty),
            ) {
                if let Some(argument) = self.expect_expression(argument_syntax, argument_type) {
                    arguments.push(argument);
                } else {
                    found_error = true;
                }
            }

            if found_error {
                return None;
            } else {
                arguments
            }
        } else {
            Vec::new()
        };

        Some(ExpressionBinding::FunctionCallBinding(
            FunctionCallBinding {
                span: SourceSpan::new(
                    self.source_file().clone(),
                    expression_syntax.qualified_identifier.span(),
                ),
                function_symbol_index: symbol_idx,
                arguments,
                expression_type: ExpressionType {
                    ty: function_symbol.return_type,
                    category: ExpressionCategory::RValue,
                },
            },
        ))
    }

    fn bind_identifier(&mut self, expression_syntax: &Named) -> Option<ExpressionBinding> {
        if expression_syntax.0.rest.is_empty() {
            // try to find variable symbol first
            let variable = self
                .variable_name_stack
                .get(&self.source_file()[expression_syntax.0.first.span])
                .copied()
                .map(|index| (index, self.variable_symbols.get(index).unwrap()));

            if let Some((variable_index, variable_symbol)) = variable {
                return Some(ExpressionBinding::IdentifierExpressionBinding(
                    IdentifierExpressionBinding {
                        identifier_expression_binding_kind:
                            IdentifierExpressionBindingKind::LocalVariableIndex {
                                local_variable_index: variable_index,
                            },
                        expression_type: ExpressionType {
                            ty: variable_symbol.type_binding_specifier.ty,
                            category: ExpressionCategory::LValue {
                                is_mutable: variable_symbol.type_binding_specifier.is_mutable,
                            },
                        },
                        span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                    },
                ));
            }

            // try to find parameter symbol
            let parameter = self
                .function_symbol
                .parameters
                .map_name_to_index(&self.source_file()[expression_syntax.0.first.span])
                .map(|index| {
                    (
                        index,
                        self.function_symbol.parameters.get_by_index(index).unwrap(),
                    )
                });

            if let Some((parameter_index, parameter_symbol)) = parameter {
                return Some(ExpressionBinding::IdentifierExpressionBinding(
                    IdentifierExpressionBinding {
                        identifier_expression_binding_kind:
                            IdentifierExpressionBindingKind::LocalArgumentIndex {
                                local_argument_index: parameter_index,
                            },
                        expression_type: ExpressionType {
                            ty: parameter_symbol.type_binding_specifier.ty,
                            category: ExpressionCategory::RValue,
                        },
                        span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                    },
                ));
            }
        }

        // try to find enum member
        let result = self
            .symbol_table
            .resolve_symbol(
                self.function_symbol.parent_index,
                &self.function_symbol.syntax_tree.source_file,
                expression_syntax.0.elements(),
            )
            .map_or_else(
                |err| {
                    self.errors.push(err);
                    None
                },
                |symbol_idx| Some(self.symbol_table.get_by_index(symbol_idx).unwrap()),
            )?;

        match result {
            GlobalSymbol::EnumVariantSymbol(enum_sym) => Some(
                ExpressionBinding::IdentifierExpressionBinding(IdentifierExpressionBinding {
                    identifier_expression_binding_kind:
                        IdentifierExpressionBindingKind::EnumVariantIndex {
                            enum_symbol_index: enum_sym.parent_index,
                            variant_number: enum_sym.variant_number,
                        },
                    span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                    expression_type: ExpressionType {
                        ty: Type::UserDefinedSymbolIndex(enum_sym.parent_index),
                        category: ExpressionCategory::RValue,
                    },
                }),
            ),
            _ => {
                self.errors
                    .push(SemanticError::ExpressionExpected(ExpressionExpected {
                        span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                    }));
                None
            }
        }
    }

    fn bind_prefix(&mut self, expression_syntax: &Prefix) -> Option<ExpressionBinding> {
        match expression_syntax.operator {
            PrefixOperator::LogicalNot(..) => {
                let expression = self.expect_expression(
                    expression_syntax.operand.as_ref(),
                    Type::Primitive(PrimitiveType::Bool),
                )?;
                Some(ExpressionBinding::PrefixExpressionBinding(
                    PrefixExpressionBinding {
                        span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                        operator: PrefixOperator::LogicalNot,
                        operand: Box::new(expression),
                        expression_type: ExpressionType {
                            ty: Type::Primitive(PrimitiveType::Bool),
                            category: ExpressionCategory::RValue,
                        },
                    },
                ))
            }
            PrefixOperator::Negate(..) => {
                let mut expression = self.bind_expression(&expression_syntax.operand)?;

                if !expression.expression_type().ty.is_arithmetic() {
                    self.errors.push(SemanticError::InvalidPrefixOperation(
                        InvalidPrefixOperation {
                            span: SourceSpan::new(
                                self.source_file().clone(),
                                expression_syntax.span(),
                            ),
                            operator: PrefixOperator::Negate,
                            operand_type: expression.expression_type(),
                        },
                    ));
                    return None;
                }

                // if not signed type, try to promote to higher-rank signed type
                if !matches!(
                    expression.expression_type().ty,
                    Type::Primitive(PrimitiveType::Int8)
                        | Type::Primitive(PrimitiveType::Int16)
                        | Type::Primitive(PrimitiveType::Int32)
                        | Type::Primitive(PrimitiveType::Int64)
                        | Type::Primitive(PrimitiveType::Float32)
                        | Type::Primitive(PrimitiveType::Float64)
                ) {
                    match expression.expression_type().ty {
                        Type::Primitive(PrimitiveType::Uint8) => {
                            expression = ExpressionBinding::CastBinding(CastBinding {
                                span: expression.span().clone(),
                                expression: Box::new(expression),
                                target_type: Type::Primitive(PrimitiveType::Int8),
                            })
                        }
                        Type::Primitive(PrimitiveType::Uint16) => {
                            expression = ExpressionBinding::CastBinding(CastBinding {
                                span: expression.span().clone(),
                                expression: Box::new(expression),
                                target_type: Type::Primitive(PrimitiveType::Int16),
                            })
                        }
                        Type::Primitive(PrimitiveType::Uint32) => {
                            expression = ExpressionBinding::CastBinding(CastBinding {
                                span: expression.span().clone(),
                                expression: Box::new(expression),
                                target_type: Type::Primitive(PrimitiveType::Int32),
                            })
                        }
                        _ => {
                            self.errors.push(SemanticError::InvalidPrefixOperation(
                                InvalidPrefixOperation {
                                    span: SourceSpan::new(
                                        self.source_file().clone(),
                                        expression_syntax.span(),
                                    ),
                                    operator: PrefixOperator::Negate,
                                    operand_type: expression.expression_type(),
                                },
                            ));
                            return None;
                        }
                    }
                }

                Some(ExpressionBinding::PrefixExpressionBinding(
                    PrefixExpressionBinding {
                        span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                        operator: PrefixOperator::Negate,
                        expression_type: ExpressionType {
                            ty: expression.expression_type().ty,
                            category: ExpressionCategory::RValue,
                        },
                        operand: Box::new(expression),
                    },
                ))
            }
        }
    }

    fn bind_binary(&mut self, expression_syntax: &Binary) -> Option<ExpressionBinding> {
        let operator = match expression_syntax.operator {
            BinaryOperatorSyntaxTree::Add(..) => BinaryOperator::Add,
            BinaryOperatorSyntaxTree::Subtract(..) => BinaryOperator::Subtract,
            BinaryOperatorSyntaxTree::Multiply(..) => BinaryOperator::Multiply,
            BinaryOperatorSyntaxTree::Divide(..) => BinaryOperator::Divide,
            BinaryOperatorSyntaxTree::Modulo(..) => BinaryOperator::Modulo,
            BinaryOperatorSyntaxTree::Equal(..) => BinaryOperator::Equal,
            BinaryOperatorSyntaxTree::NotEqual(..) => BinaryOperator::NotEqual,
            BinaryOperatorSyntaxTree::LessThan(..) => BinaryOperator::LessThan,
            BinaryOperatorSyntaxTree::LessThanOrEqual(..) => BinaryOperator::LessThanOrEqual,
            BinaryOperatorSyntaxTree::GreaterThan(..) => BinaryOperator::GreaterThan,
            BinaryOperatorSyntaxTree::GreaterThanOrEqual(..) => BinaryOperator::GreaterThanOrEqual,
            BinaryOperatorSyntaxTree::LogicalAnd(..) => BinaryOperator::LogicalAnd,
            BinaryOperatorSyntaxTree::LogicalOr(..) => BinaryOperator::LogicalOr,
            BinaryOperatorSyntaxTree::Assign(..) => BinaryOperator::Assign,
            BinaryOperatorSyntaxTree::CompoundAdd(..) => BinaryOperator::CompoundAdd,
            BinaryOperatorSyntaxTree::CompoundSubtract(..) => BinaryOperator::CompoundSubtract,
            BinaryOperatorSyntaxTree::CompoundMultiply(..) => BinaryOperator::CompoundMultiply,
            BinaryOperatorSyntaxTree::CompoundDivide(..) => BinaryOperator::CompoundDivide,
            BinaryOperatorSyntaxTree::CompoundModulo(..) => BinaryOperator::CompoundModulo,
        };

        match operator {
            BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::Multiply
            | BinaryOperator::Divide
            | BinaryOperator::Modulo
            | BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::LessThan
            | BinaryOperator::LessThanOrEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanOrEqual => {
                let mut left = self.bind_expression(&expression_syntax.left)?;
                let mut right = self.bind_expression(&expression_syntax.right)?;

                // binary operator is not allowed for non-primitive types
                if left.expression_type().ty.as_primitive().is_none()
                    || right.expression_type().ty.as_primitive().is_none()
                {
                    self.errors.push(SemanticError::InvalidBinaryOperation(
                        InvalidBinaryOperation {
                            span: SourceSpan::new(
                                self.source_file().clone(),
                                expression_syntax.span(),
                            ),
                            operator,
                            lhs_type: left.expression_type(),
                            rhs_type: right.expression_type(),
                        },
                    ));
                    return None;
                }

                let is_arithmetic_operation = matches!(
                    operator,
                    BinaryOperator::Add
                        | BinaryOperator::Subtract
                        | BinaryOperator::Multiply
                        | BinaryOperator::Divide
                        | BinaryOperator::Modulo
                        | BinaryOperator::GreaterThan
                        | BinaryOperator::GreaterThanOrEqual
                        | BinaryOperator::LessThan
                        | BinaryOperator::LessThanOrEqual
                );

                // if left and right types aren't the same then try to perform implicit cast
                if left.expression_type().ty != right.expression_type().ty {
                    // if left is implicitly castable to right and vice versa, we got a
                    // ambiguity
                    let is_left_to_right_castable = Self::is_implicitly_castable(
                        left.expression_type().ty,
                        right.expression_type().ty,
                    );
                    let is_right_to_left_castable = Self::is_implicitly_castable(
                        right.expression_type().ty,
                        left.expression_type().ty,
                    );

                    match (is_left_to_right_castable, is_right_to_left_castable) {
                        // ambiguous cast
                        (true, true) => {
                            self.errors
                                .push(SemanticError::AmbiguousConversionInBinaryOperation(
                                    AmbiguousConversionInBinaryOperation {
                                        lhs_type: left.expression_type(),
                                        rhs_type: right.expression_type(),
                                        operator,
                                        span: SourceSpan::new(
                                            self.source_file().clone(),
                                            expression_syntax.span(),
                                        ),
                                    },
                                ))
                        }
                        // left is implicitly castable to right
                        (true, false) => {
                            left = ExpressionBinding::CastBinding(CastBinding {
                                expression: Box::new(left),
                                target_type: right.expression_type().ty,
                                span: SourceSpan::new(
                                    self.source_file().clone(),
                                    expression_syntax.span(),
                                ),
                            })
                        }
                        // right is implicitly castable to left
                        (false, true) => {
                            right = ExpressionBinding::CastBinding(CastBinding {
                                expression: Box::new(right),
                                target_type: left.expression_type().ty,
                                span: SourceSpan::new(
                                    self.source_file().clone(),
                                    expression_syntax.span(),
                                ),
                            })
                        }
                        // mismatched types
                        (false, false) => {
                            self.errors.push(SemanticError::TypeMismatch(TypeMismatch {
                                expected: left.expression_type().ty,
                                actual: right.expression_type().ty,
                                span: SourceSpan::new(
                                    self.source_file().clone(),
                                    expression_syntax.span(),
                                ),
                            }))
                        }
                    }
                }

                // can't perform modulo on floating point types
                if matches!(operator, BinaryOperator::Modulo)
                    && left.expression_type().ty.is_floating_point()
                {
                    self.errors.push(SemanticError::InvalidBinaryOperation(
                        InvalidBinaryOperation {
                            span: SourceSpan::new(
                                self.source_file().clone(),
                                expression_syntax.span(),
                            ),
                            operator,
                            lhs_type: left.expression_type(),
                            rhs_type: right.expression_type(),
                        },
                    ));
                    return None;
                }

                // arithmetic operation can only be performed on arithmetic types
                if is_arithmetic_operation
                    && !left.expression_type().ty.is_arithmetic()
                    && !right.expression_type().ty.is_arithmetic()
                {
                    self.errors.push(SemanticError::InvalidBinaryOperation(
                        InvalidBinaryOperation {
                            span: SourceSpan::new(
                                self.source_file().clone(),
                                expression_syntax.span(),
                            ),
                            operator,
                            lhs_type: left.expression_type(),
                            rhs_type: right.expression_type(),
                        },
                    ));
                    return None;
                }

                Some(ExpressionBinding::BinaryExpressionBinding(
                    BinaryExpressionBinding {
                        operator,
                        right: Box::new(right),
                        span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                        expression_type: ExpressionType {
                            ty: match operator {
                                BinaryOperator::Equal
                                | BinaryOperator::NotEqual
                                | BinaryOperator::LessThan
                                | BinaryOperator::LessThanOrEqual
                                | BinaryOperator::GreaterThan
                                | BinaryOperator::GreaterThanOrEqual => {
                                    Type::Primitive(PrimitiveType::Bool)
                                }
                                _ => left.expression_type().ty,
                            },
                            category: ExpressionCategory::RValue,
                        },
                        left: Box::new(left),
                    },
                ))
            }

            BinaryOperator::Assign
            | BinaryOperator::CompoundAdd
            | BinaryOperator::CompoundSubtract
            | BinaryOperator::CompoundMultiply
            | BinaryOperator::CompoundDivide
            | BinaryOperator::CompoundModulo => {
                let left = self.bind_expression(&expression_syntax.left)?;

                // assignment is only allowed for lvalues
                if matches!(left.expression_type().category, ExpressionCategory::RValue) {
                    self.errors
                        .push(SemanticError::AssignToRValue(AssignToRValue {
                            span: SourceSpan::new(
                                self.source_file().clone(),
                                expression_syntax.span(),
                            ),
                        }));
                    return None;
                }

                // assignment is only allowed for mutable lvalues
                if matches!(
                    left.expression_type().category,
                    ExpressionCategory::LValue { is_mutable: false }
                ) {
                    self.errors
                        .push(SemanticError::AssignToImmutable(AssignToImmutable {
                            span: SourceSpan::new(
                                self.source_file().clone(),
                                expression_syntax.span(),
                            ),
                        }));
                    return None;
                }

                let right =
                    self.expect_expression(&expression_syntax.right, left.expression_type().ty)?;

                let is_arithmetic = matches!(
                    operator,
                    BinaryOperator::CompoundAdd
                        | BinaryOperator::CompoundSubtract
                        | BinaryOperator::CompoundMultiply
                        | BinaryOperator::CompoundDivide
                        | BinaryOperator::CompoundModulo
                );

                if is_arithmetic {
                    if !left.expression_type().ty.is_arithmetic() {
                        self.errors.push(SemanticError::InvalidBinaryOperation(
                            InvalidBinaryOperation {
                                lhs_type: left.expression_type(),
                                operator,
                                rhs_type: right.expression_type(),
                                span: SourceSpan::new(
                                    self.source_file().clone(),
                                    expression_syntax.span(),
                                ),
                            },
                        ))
                    }

                    // float modulo is not allowed
                    if left.expression_type().ty.is_floating_point()
                        && matches!(operator, BinaryOperator::CompoundModulo)
                    {
                        self.errors.push(SemanticError::InvalidBinaryOperation(
                            InvalidBinaryOperation {
                                lhs_type: left.expression_type(),
                                operator,
                                rhs_type: right.expression_type(),
                                span: SourceSpan::new(
                                    self.source_file().clone(),
                                    expression_syntax.span(),
                                ),
                            },
                        ))
                    }
                }

                Some(ExpressionBinding::BinaryExpressionBinding(
                    BinaryExpressionBinding {
                        span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                        operator,
                        right: Box::new(right),
                        expression_type: ExpressionType {
                            ty: left.expression_type().ty,
                            category: ExpressionCategory::LValue { is_mutable: true },
                        },
                        left: Box::new(left),
                    },
                ))
            }

            BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => {
                let left = self.expect_expression(
                    &expression_syntax.left,
                    Type::Primitive(PrimitiveType::Bool),
                )?;
                let right = self.expect_expression(
                    &expression_syntax.right,
                    Type::Primitive(PrimitiveType::Bool),
                )?;

                Some(ExpressionBinding::BinaryExpressionBinding(
                    BinaryExpressionBinding {
                        span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                        expression_type: ExpressionType {
                            ty: Type::Primitive(PrimitiveType::Bool),
                            category: ExpressionCategory::RValue,
                        },
                    },
                ))
            }
        }
    }

    fn bind_boolean_literal(
        &mut self,
        expression_syntax: &BooleanLiteral,
    ) -> Option<ExpressionBinding> {
        Some(ExpressionBinding::BooleanLiteralBinding(
            BooleanLiteralBinding {
                span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                value: expression_syntax.as_true().is_some(),
            },
        ))
    }

    fn bind_numeric_literal(
        &mut self,
        expression_syntax: &NumericLiteralSyntaxTree,
    ) -> Option<ExpressionBinding> {
        let numeric_string = &self.source_file()[expression_syntax.0.value_span];
        let suffix_string = expression_syntax
            .0
            .suffix_span
            .map(|span| &self.source_file()[span]);
        let is_float = numeric_string.contains('.');

        // the numeric literal has a suffix type specifier
        if let Some(suffix_string) = suffix_string {
            let suffix_type = match suffix_string {
                "i8" => PrimitiveType::Int8,
                "i16" => PrimitiveType::Int16,
                "i32" => PrimitiveType::Int32,
                "i64" => PrimitiveType::Int64,
                "u8" => PrimitiveType::Uint8,
                "u16" => PrimitiveType::Uint16,
                "u32" => PrimitiveType::Uint32,
                "u64" => PrimitiveType::Uint64,
                "f32" => PrimitiveType::Float32,
                "f64" => PrimitiveType::Float64,
                _ => {
                    self.errors.push(SemanticError::InvalidNumericLiteralSuffix(
                        InvalidNumericLiteralSuffix {
                            span: SourceSpan::new(
                                self.source_file().clone(),
                                expression_syntax.0.suffix_span.unwrap(),
                            ),
                        },
                    ));
                    return None;
                }
            };

            let suffix_type_is_float =
                matches!(suffix_type, PrimitiveType::Float32 | PrimitiveType::Float64);

            // the numeric literal is a float but the suffix type is not a float
            if is_float && !suffix_type_is_float {
                self.errors.push(SemanticError::InvalidNumericLiteralSuffix(
                    InvalidNumericLiteralSuffix {
                        span: SourceSpan::new(
                            self.source_file().clone(),
                            expression_syntax.0.suffix_span.unwrap(),
                        ),
                    },
                ));
                return None;
            }

            let numeric_value = match suffix_type {
                PrimitiveType::Float32 => numeric_string
                    .parse::<f32>()
                    .ok()
                    .map(NumericLiteralValue::Float32),
                PrimitiveType::Float64 => numeric_string
                    .parse::<f64>()
                    .ok()
                    .map(NumericLiteralValue::Float64),
                PrimitiveType::Int8 => numeric_string
                    .parse::<i8>()
                    .ok()
                    .map(NumericLiteralValue::Int8),
                PrimitiveType::Int16 => numeric_string
                    .parse::<i16>()
                    .ok()
                    .map(NumericLiteralValue::Int16),
                PrimitiveType::Int32 => numeric_string
                    .parse::<i32>()
                    .ok()
                    .map(NumericLiteralValue::Int32),
                PrimitiveType::Int64 => numeric_string
                    .parse::<i64>()
                    .ok()
                    .map(NumericLiteralValue::Int64),
                PrimitiveType::Uint8 => numeric_string
                    .parse::<u8>()
                    .ok()
                    .map(NumericLiteralValue::Uint8),
                PrimitiveType::Uint16 => numeric_string
                    .parse::<u16>()
                    .ok()
                    .map(NumericLiteralValue::Uint16),
                PrimitiveType::Uint32 => numeric_string
                    .parse::<u32>()
                    .ok()
                    .map(NumericLiteralValue::Uint32),
                PrimitiveType::Uint64 => numeric_string
                    .parse::<u64>()
                    .ok()
                    .map(NumericLiteralValue::Uint64),
                _ => unreachable!(),
            };

            let numeric_value = numeric_value.map_or_else(
                || {
                    self.errors.push(SemanticError::OutOfRangeNumericLiteral(
                        OutOfRangeNumericLiteral {
                            span: SourceSpan::new(
                                self.source_file().clone(),
                                expression_syntax.span(),
                            ),
                        },
                    ));
                    None
                },
                Some,
            )?;

            Some(ExpressionBinding::NumericLiteralBinding(
                NumericLiteralBinding {
                    value: numeric_value,
                    span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                },
            ))
        } else if is_float {
            let numeric_value = numeric_string
                .parse::<f64>()
                .ok()
                .map(NumericLiteralValue::Float64)
                .map_or_else(
                    || {
                        self.errors.push(SemanticError::OutOfRangeNumericLiteral(
                            OutOfRangeNumericLiteral {
                                span: SourceSpan::new(
                                    self.source_file().clone(),
                                    expression_syntax.span(),
                                ),
                            },
                        ));
                        None
                    },
                    Some,
                )?;

            Some(ExpressionBinding::NumericLiteralBinding(
                NumericLiteralBinding {
                    value: numeric_value,
                    span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                },
            ))
        } else {
            let numeric_value = numeric_string.parse::<i64>().ok().map_or_else(
                || {
                    self.errors.push(SemanticError::OutOfRangeNumericLiteral(
                        OutOfRangeNumericLiteral {
                            span: SourceSpan::new(
                                self.source_file().clone(),
                                expression_syntax.span(),
                            ),
                        },
                    ));
                    None
                },
                Some,
            )?;

            if numeric_value < i32::MAX as i64 {
                // use i32 if possible
                Some(ExpressionBinding::NumericLiteralBinding(
                    NumericLiteralBinding {
                        value: NumericLiteralValue::Int32(numeric_value as i32),
                        span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                    },
                ))
            } else {
                Some(ExpressionBinding::NumericLiteralBinding(
                    NumericLiteralBinding {
                        value: NumericLiteralValue::Int64(numeric_value),
                        span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                    },
                ))
            }
        }
    }
}

pub fn bind(
    symbol_table: &GlobalSymbolTable,
    function_symbol_index: usize,
) -> Result<FunctionBinding, FunctionBindError> {
    // Find the function symbol
    let function_symbol = symbol_table
        .get_by_index(function_symbol_index)
        .map_or_else(|| Err(FunctionBindError::InvalidFunctionSymbolIndex), Ok)?;

    let function_symbol = function_symbol
        .as_function_symbol()
        .map_or_else(|| Err(FunctionBindError::InvalidFunctionSymbolIndex), Ok)?;

    let mut function_binder = FunctionBinder {
        symbol_table,
        function_symbol,
        current_block_id: 0,
        errors: Vec::new(),
        variable_symbols: Vec::new(),
        variable_name_stack: NameStack::default(),
    };

    function_binder.bind_expression(&function_symbol.syntax_tree.syntax_tree.expression);

    todo!()
}
