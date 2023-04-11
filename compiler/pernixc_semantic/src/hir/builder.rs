//! Contains the definition [`Builder`].

use std::{collections::HashSet, hash::Hash, sync::Arc};

use pernixc_common::source_file::{SourceElement, SourceFile, Span};
use pernixc_lexical::token::NumericLiteral as NumericLiteralToken;
use pernixc_syntax::syntax_tree::{
    expression::{
        Binary as BinarySyntaxTree, BinaryOperator as BinaryOperatorSyntaxTree,
        Block as BlockSyntaxTree, BlockOrIfElse, BooleanLiteral as BooleanLiteralSyntaxTree,
        Express as ExpressSyntaxTree, Expression as ExpressionSyntaxTree, FieldInitializerList,
        FunctionCall as FunctionCallSyntaxTree, Functional as FunctionalSyntaxTree,
        IfElse as IfElseSyntaxTree, Imperative as ImperativeSyntaxTree,
        MemberAccess as MemberAccessSyntaxTree, Named as NamedSyntaxTree,
        Prefix as PrefixSyntaxTree, PrefixOperator as PrefixOperatorSyntaxTree,
        Return as ReturnSyntaxTree, StructLiteral as StructLiteralSyntaxTree,
    },
    statement::{
        Declarative as DeclarativeSyntaxTree, Expressive as ExpressiveSyntaxTree,
        Statement as StatementSyntaxTree, VariableDeclaration as VariableDeclarationSyntaxTree,
    },
};

use self::{
    block_manager::{BlockInfo, BlockManager},
    variable_manager::VariableManager,
};
use super::{
    instruction::{Initialization, Instruction, StructInitialization},
    value::{
        ArgumentAddress, Binary, BinaryOperator, Block, BooleanLiteral, Category, EnumLiteral,
        Express, FunctionCall, IfElse, IfElseLoad, ImplicitConversion, InferrableType,
        IntermediateType, LValue, Load, MemberAccess, Named, NumericLiteral, Prefix, Return,
        StructLiteral, TypeBinding, Value, ValueTrait, Variable, VariableAddress, VariableID,
    },
};
use crate::{
    control_flow_graph::{
        BasicBlockID, ControlFlowGraph, Instruction as CFGInstruction, TerminalInstruction,
    },
    errors::{
        ArgumentCountMismatch, AssignToImmutable, AssignToRValue, DuplicateFieldInitialization,
        ExpressOutsideBlock, ExpressionExpected, FieldIsNotAccessible, FieldNotFound,
        IncompleteFieldInitialization, InvalidBinaryOperation, InvalidNumericLiteralSuffix,
        InvalidPrefixOperation, LabelNotFound, MemberAccessOnNonStruct, NotAllPathExpressTheValue,
        ReturnRequiredValue, SemanticError, StructExpected, SymbolIsNotCallable, TypeMismatch,
    },
    hir::{instruction::Store, value::PrefixOperator},
    infer::{Constraint, InferenceContext, InferenceID, Infererence, UnificationError},
    symbol::{
        ty::{PrimitiveType, Type},
        AccessModifier, FieldID, Function, FunctionID, ResolveError, StructID, Table,
        TypeResolveError, TypedID, ID,
    },
    SemanticResult, SourceSpan,
};

mod block_manager;
mod variable_manager;

/// Represents a build for the [`crate::hir::HIR`].
#[derive(Debug, Clone)]
pub struct Builder<'a> {
    table: &'a Table,
    function: &'a Function,
    function_symbol_id: FunctionID,
    control_flow_graph: ControlFlowGraph<Instruction<IntermediateType>, Value<IntermediateType>>,
    inference_context: InferenceContext,
    errors: Vec<SemanticError>,
    produce_error: bool,
    current_basic_block_id: BasicBlockID,

    variable_manager: VariableManager<'a>,
    block_manager: BlockManager<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum InitializeCheckResult {
    Uninitialized,
    Initialized,
    Unreachable,
}

impl<'a> Builder<'a> {
    /// Creates a new [`Builder`] instance targeting the function in the [`Table`] with the given
    /// [`FunctionID`].
    ///
    /// # Panics
    /// Panics if the function with the given [`FunctionID`] does not exist in the [`Table`].
    #[must_use]
    pub fn new(table: &'a Table, function_symbol_id: FunctionID) -> Self {
        let control_flow_graph = ControlFlowGraph::new();
        let current_basic_block_id = control_flow_graph.entry_block_id();
        Self {
            table,
            function: table.get(function_symbol_id),
            function_symbol_id,
            inference_context: InferenceContext::new(),
            control_flow_graph,
            current_basic_block_id,
            errors: Vec::new(),
            produce_error: true,
            variable_manager: VariableManager::new(),
            block_manager: BlockManager::new(),
        }
    }

    /// Binds the given statement syntax tree into an instruction and adds it to the current basic
    /// block.
    #[must_use]
    pub fn bind_statement(&mut self, syntax: &StatementSyntaxTree) -> bool {
        match syntax {
            StatementSyntaxTree::Declarative(DeclarativeSyntaxTree::VariableDeclaration(var)) => {
                self.bind_variable_declaration(var).is_some()
            }
            StatementSyntaxTree::Expressive(expressive) => {
                let value = match expressive {
                    ExpressiveSyntaxTree::Semi(semi) => {
                        self.bind_functional(&semi.expression, None)
                    }
                    ExpressiveSyntaxTree::Imperative(imperative) => {
                        self.bind_imperative(imperative, None)
                    }
                };

                // expect a valid value
                let Some(value) = value else { return false; };

                // These are list of evaluations that will not be added into the cfg
                //
                // 1.) Block Load
                // 2.) IfElse Load
                // 3.) Express
                // 4.) Return
                if !matches!(
                    value,
                    Value::Block(..) | Value::IfElse(..) | Value::Express(..) | Value::Return(..)
                ) {
                    // add value evalutation instruction
                    self.control_flow_graph[self.current_basic_block_id]
                        .add_instruction(value.into());
                }

                true
            }
        }
    }

    /// Binds the variable declaration statement into an appropriate [`Instruction`] and adds it to
    /// the current basic block.
    pub fn bind_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclarationSyntaxTree,
    ) -> Option<VariableAddress> {
        let variable_id = VariableID::fresh();
        let initializer = match &variable_declaration.type_annotation {
            Some(type_annotation) => {
                let ty = self.table.resolve_type(
                    self.function_symbol_id.into(),
                    &type_annotation.type_specifier,
                    self.source_file(),
                );

                let ty = self.handle_type_resolve_result(ty)?;

                // binds the expression with the expected type
                self.expect_value(
                    &variable_declaration.expression,
                    ty.into(),
                    Some(variable_id),
                )?
            }
            None => self.bind_expression(&variable_declaration.expression, Some(variable_id))?,
        };

        // add variable into the list
        let variable_name = &self.source_file()[variable_declaration.identifier.span];
        self.variable_manager.add_variable_with_name(
            variable_id,
            Variable {
                is_mutable: variable_declaration.mutable_keyword.is_some(),
                ty: initializer.type_binding().ty,
                name: Some(variable_name.to_string()),
            },
            &self.source_file()[variable_declaration.identifier.span],
        );

        // add store instruction to the current basic block
        if !matches!(
            initializer,
            Value::StructLiteral(..) | Value::Block(..) | Value::IfElse(..)
        ) {
            self.control_flow_graph[self.current_basic_block_id].add_instruction(
                Store {
                    variable_id,
                    initialization: Initialization::Value(initializer),
                }
                .into(),
            );
        }

        Some(VariableAddress { variable_id })
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// SECTION - Helper Functions
////////////////////////////////////////////////////////////////////////////////////////////////////

impl<'a> Builder<'a> {
    /// Gets the [`InferrableType`] of the given [`IntermediateType`] based on the current inference
    /// context state.
    #[must_use]
    pub fn get_inferrable_type(&self, ty: &IntermediateType) -> InferrableType {
        match ty {
            IntermediateType::Inference(ty) => match self.inference_context.get_inference(*ty) {
                Infererence::Inferred(concrete) => InferrableType::Type(*concrete),
                Infererence::TypeVariable(type_var) => {
                    InferrableType::Inferring(*type_var.constraint())
                }
            },
            IntermediateType::Type(ty) => InferrableType::Type(*ty),
        }
    }

    /// Gets the [`SourceFile`] of the current function.
    fn source_file(&self) -> &'a Arc<SourceFile> { &self.function.syntax_tree.source_file }

    /// Gets the [`SourceSpan`] of the given [`Span`].
    fn source_span(&self, span: Span) -> SourceSpan {
        SourceSpan {
            source_file: self.function.syntax_tree.source_file.clone(),
            span,
        }
    }

    /// Unwraps the result from the [`Table::resolve_type()`] function and adds the errors to the
    /// [`Builder`].
    fn handle_type_resolve_result(
        &mut self,
        resolve_result: SemanticResult<Type, TypeResolveError, SemanticError>,
    ) -> Option<Type> {
        match resolve_result {
            Ok(mut ok) => {
                if self.produce_error {
                    self.errors.append(&mut ok.errors);
                }

                Some(ok.value)
            }
            Err(errors) => {
                if self.produce_error {
                    self.errors.extend(
                        errors
                            .into_iter()
                            .filter_map(|err| err.into_semantic_error().ok()),
                    );
                }
                None
            }
        }
    }

    /// Unwraps the result from the [`Table::resolve_symbol()`] function and adds the errors to the
    /// [`Builder`].
    fn handle_symbol_resolve_result(
        &mut self,
        resolve_result: SemanticResult<ID, ResolveError, SemanticError>,
    ) -> Option<ID> {
        match resolve_result {
            Ok(mut ok) => {
                if self.produce_error {
                    self.errors.append(&mut ok.errors);
                }

                Some(ok.value)
            }
            Err(errors) => {
                if self.produce_error {
                    self.errors.extend(
                        errors
                            .into_iter()
                            .filter_map(|err| err.into_semantic_error().ok()),
                    );
                }
                None
            }
        }
    }

    /// Adds the semantic error to the list of errors.
    pub fn add_error(&mut self, error: SemanticError) {
        if self.produce_error {
            self.errors.push(error);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// SECTION - Bind Expressions
////////////////////////////////////////////////////////////////////////////////////////////////////

impl<'a> Builder<'a> {
    ////////////////////////////////////////////////////////////////////////////////////////////////
    /// SECTION - Top Level Bind Expression
    ////////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`ExpressionSyntaxTree`] into the [`Value<IntermediateType>`].
    pub fn bind_expression(
        &mut self,
        syntax: &ExpressionSyntaxTree,
        variable_id: Option<VariableID>,
    ) -> Option<Value<IntermediateType>> {
        match syntax {
            ExpressionSyntaxTree::Functional(syntax) => self.bind_functional(syntax, variable_id),
            ExpressionSyntaxTree::Imperative(syntax) => self.bind_imperative(syntax, variable_id),
        }
    }

    /// Binds the given [`FunctionalSyntaxTree`] into the [`Value<IntermediateType>`].
    pub fn bind_functional(
        &mut self,
        syntax: &FunctionalSyntaxTree,
        variable_id: Option<VariableID>,
    ) -> Option<Value<IntermediateType>> {
        match syntax {
            FunctionalSyntaxTree::NumericLiteral(syntax) => {
                self.bind_numeric_literal(syntax).map(Value::NumericLiteral)
            }
            FunctionalSyntaxTree::BooleanLiteral(syntax) => {
                Some(Value::BooleanLiteral(self.bind_boolean_literal(syntax)))
            }
            FunctionalSyntaxTree::Binary(syntax) => self.bind_binary(syntax).map(Value::Binary),
            FunctionalSyntaxTree::Prefix(syntax) => self.bind_prefix(syntax).map(Value::Prefix),
            FunctionalSyntaxTree::Named(syntax) => self.bind_named(syntax).map(Value::Named),
            FunctionalSyntaxTree::FunctionCall(syntax) => {
                self.bind_function_call(syntax).map(Value::FunctionCall)
            }
            FunctionalSyntaxTree::Parenthesized(syntax) => {
                self.bind_expression(&syntax.expression, None)
            }
            FunctionalSyntaxTree::StructLiteral(syntax) => self
                .bind_struct_literal(syntax, variable_id)
                .map(Value::StructLiteral),
            FunctionalSyntaxTree::MemberAccess(syntax) => {
                self.bind_member_access(syntax).map(Value::MemberAccess)
            }
            FunctionalSyntaxTree::Continue(_) => todo!(),
            FunctionalSyntaxTree::Break(_) => todo!(),
            FunctionalSyntaxTree::Return(ret) => self.bind_return(ret).map(Value::Return),
            FunctionalSyntaxTree::Express(express) => {
                self.bind_express(express).map(Value::Express)
            }
            FunctionalSyntaxTree::Cast(_) => todo!(),
        }
    }

    /// Binds the given [`ImperativeSyntaxTree`] into the [`Value<IntermediateType>`].
    pub fn bind_imperative(
        &mut self,
        syntax: &ImperativeSyntaxTree,
        variable_id: Option<VariableID>,
    ) -> Option<Value<IntermediateType>> {
        match syntax {
            ImperativeSyntaxTree::Block(block) => {
                self.bind_block(block, variable_id).map(Value::Block)
            }
            ImperativeSyntaxTree::IfElse(if_else) => {
                self.bind_if_else(if_else, variable_id).map(Value::IfElse)
            }
            ImperativeSyntaxTree::Loop(_) => todo!(),
        }
    }

    /// Binds the given [`ExpressionSyntaxTree`] into the [`Value<IntermediateType>`] with
    /// additional type verification.
    pub fn expect_value(
        &mut self,
        syntax: &ExpressionSyntaxTree,
        ty: IntermediateType,
        variable_id: Option<VariableID>,
    ) -> Option<Value<IntermediateType>> {
        let value = self.bind_expression(syntax, variable_id)?;
        self.verify_value(value, ty).ok()
    }

    fn check_inference_and_concrete(
        &mut self,
        value: Value<IntermediateType>,
        inference_id: InferenceID,
        concrete_type: Type,
        reversed: bool,
    ) -> Result<Value<IntermediateType>, Value<IntermediateType>> {
        let Err(err) = self.inference_context.unify_with_concrete(inference_id, concrete_type) else {
            return Ok(value);
        };

        match err {
            UnificationError::TypeMismatch {
                mut left,
                mut right,
            } => {
                if reversed {
                    std::mem::swap(&mut left, &mut right);
                }

                if Self::castable(left, right) {
                    return Ok(ImplicitConversion {
                        value: Box::new(value),
                        ty: right.into(),
                    }
                    .into());
                }
                self.add_error(
                    TypeMismatch {
                        source_span: value.source_span(),
                        expected: right.into(),
                        actual: left.into(),
                    }
                    .into(),
                );
                Err(value)
            }
            UnificationError::ConstraintNotSatisfied {
                constraint,
                concrete_type,
            } => {
                let mut left = InferrableType::Inferring(constraint);
                let mut right = InferrableType::Type(concrete_type);

                if reversed {
                    std::mem::swap(&mut left, &mut right);
                }

                self.add_error(
                    TypeMismatch {
                        source_span: value.source_span(),
                        actual: left,
                        expected: right,
                    }
                    .into(),
                );

                Err(value)
            }
        }
    }

    fn castable(from: Type, to: Type) -> bool {
        matches!(
            (from, to),
            (Type::Never, Type::Primitive(PrimitiveType::Void))
                | (Type::Primitive(PrimitiveType::Void), Type::Never)
        )
    }

    /// Verfies whether the given [`Value<IntermediateType>`] is/satisfies the given
    /// [`InferrableType`].
    #[allow(clippy::missing_errors_doc)]
    pub fn verify_value(
        &mut self,
        value: Value<IntermediateType>,
        expected: IntermediateType,
    ) -> Result<Value<IntermediateType>, Value<IntermediateType>> {
        match (value.type_binding().ty, expected) {
            (IntermediateType::Inference(left), IntermediateType::Inference(right)) => 'result: {
                let Err(err) = self.inference_context.unify(left, right) else {
                    break 'result Ok(value);
                };

                match err {
                    UnificationError::TypeMismatch { left, right } => {
                        if Self::castable(left, right) {
                            Ok(ImplicitConversion {
                                value: Box::new(value),
                                ty: right.into(),
                            }
                            .into())
                        } else {
                            self.add_error(
                                TypeMismatch {
                                    source_span: value.source_span(),
                                    expected: InferrableType::Type(left),
                                    actual: InferrableType::Type(right),
                                }
                                .into(),
                            );
                            Err(value)
                        }
                    }
                    UnificationError::ConstraintNotSatisfied {
                        constraint,
                        concrete_type,
                    } => {
                        let left_is_constraint = self
                            .inference_context
                            .get_inference(left)
                            .as_type_variable()
                            .is_some();

                        if left_is_constraint {
                            self.add_error(
                                TypeMismatch {
                                    source_span: value.source_span(),
                                    expected: InferrableType::Type(concrete_type),
                                    actual: InferrableType::Inferring(constraint),
                                }
                                .into(),
                            );
                        } else {
                            self.add_error(
                                TypeMismatch {
                                    source_span: value.source_span(),
                                    expected: InferrableType::Inferring(constraint),
                                    actual: InferrableType::Type(concrete_type),
                                }
                                .into(),
                            );
                        }
                        Err(value)
                    }
                }
            }
            (IntermediateType::Inference(left), IntermediateType::Type(right)) => {
                self.check_inference_and_concrete(value, left, right, false)
            }
            (IntermediateType::Type(left), IntermediateType::Inference(right)) => {
                self.check_inference_and_concrete(value, right, left, true)
            }
            (IntermediateType::Type(left), IntermediateType::Type(right)) => {
                if left == right {
                    Ok(value)
                } else if Self::castable(left, right) {
                    Ok(ImplicitConversion {
                        value: Box::new(value),
                        ty: right.into(),
                    }
                    .into())
                } else {
                    self.add_error(
                        TypeMismatch {
                            source_span: value.source_span(),
                            expected: InferrableType::Type(right),
                            actual: InferrableType::Type(left),
                        }
                        .into(),
                    );
                    Err(value)
                }
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    /// SECTION - Bindings
    ////////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`NumericLiteralToken`] into the [`NumericLiteral`].
    pub fn bind_numeric_literal(
        &mut self,
        token: &NumericLiteralToken,
    ) -> Option<NumericLiteral<IntermediateType>> {
        let is_float = self.function.syntax_tree.source_file[token.value_span].contains('.');
        if let Some(suffix_span) = &token.suffix_span {
            let suffix = &self.function.syntax_tree.source_file[*suffix_span];

            // gets the type constraint of numeric literal
            let primitive_type = match suffix {
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
                      self.add_error(
                        InvalidNumericLiteralSuffix {
                            source_span: self.source_span(*suffix_span),
                        }
                        .into(),
                    );
                    return None;
                }
            };

            if is_float
                && !matches!(
                    primitive_type,
                    PrimitiveType::Float32 | PrimitiveType::Float64
                )
            {
                self.add_error(
                    InvalidNumericLiteralSuffix {
                        source_span: self.source_span(token.span),
                    }
                    .into(),
                );
                return None;
            }

            Some(NumericLiteral {
                source_span: self.source_span(token.value_span),
                ty: Type::Primitive(primitive_type).into(),
            })
        } else {
            // gets the type constraint of numeric literal
            let constraint = if is_float {
                Constraint::Float
            } else {
                Constraint::Number
            };

            Some(NumericLiteral {
                source_span: self.source_span(token.value_span),
                ty: self.inference_context.add_inference(constraint).into(),
            })
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`BooleanLiteralSyntaxTree`] into the [`BooleanLiteral`].
    pub fn bind_boolean_literal(&mut self, syntax: &BooleanLiteralSyntaxTree) -> BooleanLiteral {
        BooleanLiteral {
            source_span: self.source_span(syntax.span()),
            value: syntax.as_true().is_some(),
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn handle_assignment(
        &mut self,
        syntax: &BinarySyntaxTree,
        binary_operator: BinaryOperator,
    ) -> Option<Binary<IntermediateType>> {
        let left = self.bind_expression(&syntax.left_operand, None)?;
        let right = self.expect_value(&syntax.right_operand, left.type_binding().ty, None)?;

        self.check_lvalue_assignment(&left.type_binding(), syntax.span())?;

        Some(Binary {
            type_binding: left.type_binding(),
            source_span: self.source_span(syntax.span()),
            left_operand: Box::new(left),
            right_operand: Box::new(right),
            binary_operator,
        })
    }

    fn handle_comparison(
        &mut self,
        syntax: &BinarySyntaxTree,
        binary_operator: BinaryOperator,
    ) -> Option<Binary<IntermediateType>> {
        let left = self.bind_expression(&syntax.left_operand, None)?;
        let right = self.expect_value(&syntax.right_operand, left.type_binding().ty, None)?;

        let is_arithmetic_comparison = matches!(
            binary_operator,
            BinaryOperator::LessThan
                | BinaryOperator::LessThanOrEqual
                | BinaryOperator::GreaterThan
                | BinaryOperator::GreaterThanOrEqual
        );

        let type_or_constraint = self.get_inferrable_type(&left.type_binding().ty);
        match type_or_constraint {
            InferrableType::Inferring(_) => {
                // NOTE: Currently, all the constraints are arithmetic constraints.
            }
            InferrableType::Type(ty) => {
                // must be primitive
                if !matches!(ty, Type::Primitive(_)) {
                    self.add_error(
                        InvalidBinaryOperation {
                            source_span: self.source_span(syntax.span()),
                            binary_operator,
                            left_operand_type: self.get_inferrable_type(&left.type_binding().ty),
                            right_operand_type: self.get_inferrable_type(&right.type_binding().ty),
                        }
                        .into(),
                    );
                }

                if is_arithmetic_comparison && !Self::is_arithmetic_type(ty) {
                    self.add_error(
                        InvalidBinaryOperation {
                            source_span: self.source_span(syntax.span()),
                            binary_operator,
                            left_operand_type: self.get_inferrable_type(&left.type_binding().ty),
                            right_operand_type: self.get_inferrable_type(&right.type_binding().ty),
                        }
                        .into(),
                    );
                }
            }
        }

        Some(Binary {
            type_binding: TypeBinding {
                ty: IntermediateType::Type(PrimitiveType::Bool.into()),
                category: Category::RValue,
            },
            source_span: self.source_span(syntax.span()),
            left_operand: Box::new(left),
            right_operand: Box::new(right),
            binary_operator,
        })
    }

    fn handle_logical(
        &mut self,
        syntax: &BinarySyntaxTree,
        binary_operator: BinaryOperator,
    ) -> Option<Binary<IntermediateType>> {
        // must be bool
        let left = self.bind_expression(&syntax.left_operand, None)?;
        let right = self.expect_value(&syntax.right_operand, left.type_binding().ty, None)?;
        let ty = self.get_inferrable_type(&left.type_binding().ty);

        if matches!(ty, InferrableType::Inferring(_))
            || matches!(ty, InferrableType::Type(ty)
                        if ty != PrimitiveType::Bool.into())
        {
            self.add_error(
                InvalidBinaryOperation {
                    left_operand_type: self.get_inferrable_type(&left.type_binding().ty),
                    right_operand_type: self.get_inferrable_type(&right.type_binding().ty),
                    binary_operator,
                    source_span: self.source_span(syntax.span()),
                }
                .into(),
            );
            return None;
        }

        Some(Binary {
            type_binding: TypeBinding {
                ty: IntermediateType::Type(PrimitiveType::Bool.into()),
                category: Category::RValue,
            },
            source_span: self.source_span(syntax.span()),
            left_operand: Box::new(left),
            right_operand: Box::new(right),
            binary_operator,
        })
    }

    fn is_arithmetic_type(concrete_type: Type) -> bool {
        matches!(
            concrete_type,
            Type::Primitive(
                PrimitiveType::Float32
                    | PrimitiveType::Float64
                    | PrimitiveType::Int8
                    | PrimitiveType::Int16
                    | PrimitiveType::Int32
                    | PrimitiveType::Int64
                    | PrimitiveType::Uint8
                    | PrimitiveType::Uint16
                    | PrimitiveType::Uint32
                    | PrimitiveType::Uint64
            )
        )
    }

    #[must_use]
    fn check_arithmetic_operation(
        &mut self,
        left: &Value<IntermediateType>,
        right: &Value<IntermediateType>,
        binary_operator: BinaryOperator,
        span: Span,
    ) -> Option<()> {
        let is_modulo = matches!(
            binary_operator,
            BinaryOperator::Modulo | BinaryOperator::CompoundModulo
        );

        let arithmetic_type_check = |this: &mut Self, concrete: Type| -> Option<()> {
            if !Self::is_arithmetic_type(concrete) {
                this.add_error(
                    InvalidBinaryOperation {
                        source_span: this.source_span(span),
                        binary_operator,
                        left_operand_type: this.get_inferrable_type(&left.type_binding().ty),
                        right_operand_type: this.get_inferrable_type(&right.type_binding().ty),
                    }
                    .into(),
                );
                return None;
            }

            // can't perform modulo on floating point types
            if is_modulo
                && matches!(
                    concrete,
                    Type::Primitive(PrimitiveType::Float32 | PrimitiveType::Float64)
                )
            {
                this.add_error(
                    InvalidBinaryOperation {
                        source_span: this.source_span(span),
                        binary_operator,
                        left_operand_type: this.get_inferrable_type(&left.type_binding().ty),
                        right_operand_type: this.get_inferrable_type(&right.type_binding().ty),
                    }
                    .into(),
                );
                return None;
            }

            Some(())
        };

        // must be arithmetic type
        match left.type_binding().ty {
            IntermediateType::Inference(infer) => {
                match self.inference_context.get_inference(infer) {
                    Infererence::Inferred(infer) => arithmetic_type_check(self, *infer),
                    Infererence::TypeVariable(ty_var) => {
                        // can't perform modulo on floating point types
                        if *ty_var.constraint() == Constraint::Float && is_modulo {
                            self.add_error(
                                InvalidBinaryOperation {
                                    source_span: self.source_span(span),
                                    binary_operator,
                                    left_operand_type: self
                                        .get_inferrable_type(&left.type_binding().ty),
                                    right_operand_type: self
                                        .get_inferrable_type(&right.type_binding().ty),
                                }
                                .into(),
                            );
                            None
                        } else {
                            Some(())
                        }
                    }
                }
            }
            IntermediateType::Type(concrete) => arithmetic_type_check(self, concrete),
        }
    }

    fn handle_arithmetic(
        &mut self,
        syntax: &BinarySyntaxTree,
        binary_operator: BinaryOperator,
    ) -> Option<Binary<IntermediateType>> {
        let left = self.bind_expression(&syntax.left_operand, None)?;
        let right = self.expect_value(&syntax.right_operand, left.type_binding().ty, None)?;

        self.check_arithmetic_operation(&left, &right, binary_operator, syntax.span())?;

        Some(Binary {
            type_binding: TypeBinding {
                ty: left.type_binding().ty,
                category: Category::RValue,
            },
            source_span: self.source_span(syntax.span()),
            left_operand: Box::new(left),
            right_operand: Box::new(right),
            binary_operator,
        })
    }

    fn handle_compound_arithmetic(
        &mut self,
        syntax: &BinarySyntaxTree,
        binary_operator: BinaryOperator,
    ) -> Option<Binary<IntermediateType>> {
        let left = self.bind_expression(&syntax.left_operand, None)?;
        let right = self.expect_value(&syntax.right_operand, left.type_binding().ty, None)?;

        self.check_lvalue_assignment(&left.type_binding(), syntax.span())?;
        self.check_arithmetic_operation(&left, &right, binary_operator, syntax.span())?;

        Some(Binary {
            type_binding: left.type_binding(),
            source_span: self.source_span(syntax.span()),
            left_operand: Box::new(left),
            right_operand: Box::new(right),
            binary_operator,
        })
    }

    #[must_use]
    fn check_lvalue_assignment(
        &mut self,
        type_binding: &TypeBinding<IntermediateType>,
        span: Span,
    ) -> Option<()> {
        let Category::LValue(lvalue) = &type_binding.category else {
                    self.add_error(
                        AssignToRValue {
                            source_span: self.source_span(span),
                        }.into()
                    );
                    return None;
                };

        // if the lvalue is mutable, we can assign to it
        if lvalue.is_mutable {
            Some(())
        } else {
            self.add_error(
                AssignToImmutable {
                    source_span: self.source_span(span),
                }
                .into(),
            );
            None
        }
    }

    fn bind_binary_operator(syntax: &BinaryOperatorSyntaxTree) -> BinaryOperator {
        match syntax {
            BinaryOperatorSyntaxTree::Add(_) => BinaryOperator::Add,
            BinaryOperatorSyntaxTree::Subtract(_) => BinaryOperator::Subtract,
            BinaryOperatorSyntaxTree::Multiply(_) => BinaryOperator::Multiply,
            BinaryOperatorSyntaxTree::Divide(_) => BinaryOperator::Divide,
            BinaryOperatorSyntaxTree::Modulo(_) => BinaryOperator::Modulo,
            BinaryOperatorSyntaxTree::LogicalAnd(_) => BinaryOperator::LogicalAnd,
            BinaryOperatorSyntaxTree::LogicalOr(_) => BinaryOperator::LogicalOr,
            BinaryOperatorSyntaxTree::Assign(..) => BinaryOperator::Assign,
            BinaryOperatorSyntaxTree::Equal(..) => BinaryOperator::Equal,
            BinaryOperatorSyntaxTree::NotEqual(..) => BinaryOperator::NotEqual,
            BinaryOperatorSyntaxTree::LessThan(..) => BinaryOperator::LessThan,
            BinaryOperatorSyntaxTree::LessThanOrEqual(..) => BinaryOperator::LessThanOrEqual,
            BinaryOperatorSyntaxTree::GreaterThan(..) => BinaryOperator::GreaterThan,
            BinaryOperatorSyntaxTree::GreaterThanOrEqual(..) => BinaryOperator::GreaterThanOrEqual,
            BinaryOperatorSyntaxTree::CompoundAdd(..) => BinaryOperator::CompoundAdd,
            BinaryOperatorSyntaxTree::CompoundSubtract(..) => BinaryOperator::CompoundSubtract,
            BinaryOperatorSyntaxTree::CompoundMultiply(..) => BinaryOperator::CompoundMultiply,
            BinaryOperatorSyntaxTree::CompoundDivide(..) => BinaryOperator::CompoundDivide,
            BinaryOperatorSyntaxTree::CompoundModulo(..) => BinaryOperator::CompoundModulo,
        }
    }

    /// Binds the given [`BinarySyntaxTree`] into the [`Binary`].
    pub fn bind_binary(&mut self, syntax: &BinarySyntaxTree) -> Option<Binary<IntermediateType>> {
        let binary_operator = Self::bind_binary_operator(&syntax.binary_operator);

        match binary_operator {
            BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::Multiply
            | BinaryOperator::Divide
            | BinaryOperator::Modulo => self.handle_arithmetic(syntax, binary_operator),
            BinaryOperator::CompoundAdd
            | BinaryOperator::CompoundSubtract
            | BinaryOperator::CompoundMultiply
            | BinaryOperator::CompoundDivide
            | BinaryOperator::CompoundModulo => {
                self.handle_compound_arithmetic(syntax, binary_operator)
            }
            BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::LessThan
            | BinaryOperator::LessThanOrEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanOrEqual => self.handle_comparison(syntax, binary_operator),
            BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => {
                self.handle_logical(syntax, binary_operator)
            }
            BinaryOperator::Assign => self.handle_assignment(syntax, binary_operator),
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`BooleanLiteralSyntaxTree`] into the [`BooleanLiteral`].
    pub fn bind_prefix(&mut self, syntax: &PrefixSyntaxTree) -> Option<Prefix<IntermediateType>> {
        match syntax.prefix_operator {
            PrefixOperatorSyntaxTree::LogicalNot(_) => {
                let value = self.bind_expression(&syntax.operand, None)?;
                let ty = self.get_inferrable_type(&value.type_binding().ty);
                if matches!(ty, InferrableType::Inferring(_))
                    || matches!(ty, InferrableType::Type(ty)
                        if ty != PrimitiveType::Bool.into())
                {
                    self.add_error(
                        InvalidPrefixOperation {
                            source_span: self.source_span(syntax.span()),
                            prefix_operator: PrefixOperator::LogicalNot,
                            operand_type: ty,
                        }
                        .into(),
                    );
                    return None;
                }

                Some(Prefix {
                    source_span: self.source_span(syntax.span()),
                    prefix_operator: PrefixOperator::LogicalNot,
                    operand: Box::new(value),
                    ty: IntermediateType::Type(PrimitiveType::Bool.into()),
                })
            }
            PrefixOperatorSyntaxTree::Negate(_) => {
                let value = self.bind_expression(&syntax.operand, None)?;

                match value.type_binding().ty {
                    IntermediateType::Inference(ty) => {
                        if let Err(error) = self
                            .inference_context
                            .add_constraint(ty, Constraint::Signed)
                        {
                            self.add_error(
                                InvalidPrefixOperation {
                                    source_span: self.source_span(syntax.span()),
                                    prefix_operator: PrefixOperator::Negate,
                                    operand_type: InferrableType::Type(error.0),
                                }
                                .into(),
                            );
                            return None;
                        }

                        Some(Prefix {
                            source_span: self.source_span(syntax.span()),
                            prefix_operator: PrefixOperator::Negate,
                            operand: Box::new(value),
                            ty: IntermediateType::Inference(ty),
                        })
                    }
                    IntermediateType::Type(ty) => {
                        if matches!(
                            ty,
                            Type::Primitive(
                                PrimitiveType::Float32
                                    | PrimitiveType::Float64
                                    | PrimitiveType::Int16
                                    | PrimitiveType::Int32
                                    | PrimitiveType::Int64
                            )
                        ) {
                            Some(Prefix {
                                source_span: self.source_span(syntax.span()),
                                prefix_operator: PrefixOperator::Negate,
                                operand: Box::new(value),
                                ty: IntermediateType::Type(ty),
                            })
                        } else {
                            self.add_error(
                                InvalidPrefixOperation {
                                    source_span: self.source_span(syntax.span()),
                                    prefix_operator: PrefixOperator::Negate,
                                    operand_type: InferrableType::Type(ty),
                                }
                                .into(),
                            );
                            None
                        }
                    }
                }
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn create_field_initialization(
        &mut self,
        struct_id: StructID,
        field_initialization: &Option<FieldInitializerList>,
    ) -> Vec<(FieldID, Option<Value<IntermediateType>>)> {
        let mut initialized_fields = HashSet::new();
        let mut initializations = Vec::new();
        for initialization in field_initialization
            .iter()
            .flat_map(pernixc_syntax::syntax_tree::ConnectedList::elements)
        {
            // get the field id of the initialization
            let field_id = self
                .table
                .get(struct_id)
                .fields
                .map_name_to_id(&self.source_file()[initialization.identifier.span]);

            // check if field exists
            let Some(field_id) = field_id else {
                self.add_error(FieldNotFound {
                    source_span: self.source_span(initialization.identifier.span),
                    struct_id,
                }.into());
                continue;
            };
            let field = self
                .table
                .get(struct_id)
                .fields
                .get_by_id(field_id)
                .unwrap();

            // check if field is already initialized
            if initialized_fields.contains(&field_id) {
                self.add_error(
                    DuplicateFieldInitialization {
                        source_span: self.source_span(initialization.span()),
                    }
                    .into(),
                );
                continue;
            };

            // check if field is accessible
            if field.access_modifier == AccessModifier::Private
                && !self
                    .table
                    .is_parent(self.function.parent_id.into(), struct_id.into())
            {
                self.add_error(
                    FieldIsNotAccessible {
                        source_span: self.source_span(initialization.identifier.span),
                        struct_id,
                        field_id,
                    }
                    .into(),
                );
                continue;
            }

            // bind the initializer
            let initializer = self.expect_value(&initialization.expression, field.ty.into(), None);

            // put the field id into the initialized field map
            initialized_fields.insert(field_id);
            initializations.push((field_id, initializer));
        }

        initializations
    }

    /// Binds the given [`StructLiteralSyntaxTree`] to a [`StructLiteral`].
    pub fn bind_struct_literal(
        &mut self,
        syntax: &StructLiteralSyntaxTree,
        variable_id: Option<VariableID>,
    ) -> Option<StructLiteral> {
        let (generated, variable_id) = variable_id.map_or_else(
            || (true, VariableID::fresh()),
            |variable_id| (false, variable_id),
        );

        // search for the symbol
        let symbol_id = self.table.resolve_symbol(
            self.function_symbol_id.into(),
            syntax.qualified_identifier.elements(),
            self.source_file(),
        );

        let symbol_id = self.handle_symbol_resolve_result(symbol_id)?;

        // expect struct id
        let ID::Struct(struct_id) = symbol_id else {
            self.add_error(
               StructExpected {
                    source_span: self.source_span(syntax.span()),
                }.into()
            );
            return None;
        };

        if generated {
            self.variable_manager.add_variable(variable_id, Variable {
                ty: Type::TypedID(struct_id.into()).into(),
                is_mutable: true,
                name: None,
            });
        }

        let initialized_field =
            self.create_field_initialization(struct_id, &syntax.field_initializations);

        if initialized_field.len() != self.table.get(struct_id).fields.len() {
            self.add_error(
                IncompleteFieldInitialization {
                    source_span: self.source_span(syntax.span()),
                    struct_id,
                }
                .into(),
            );
        }

        // generates the store instructions to each field initialization
        let mut field_initializations = Vec::new();
        for (field_id, initializer) in initialized_field {
            let Some(initializer) = initializer else {
                continue;
            };
            field_initializations.push((field_id, initializer));
        }

        self.control_flow_graph[self.current_basic_block_id].add_instruction(
            Store {
                variable_id,
                initialization: StructInitialization {
                    field_initializations,
                }
                .into(),
            }
            .into(),
        );

        Some(StructLiteral {
            source_span: self.source_span(syntax.span()),
            struct_id,
            variable_id,
        })
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`MemberAccessSyntaxTree`] into the [`MemberAccess`].
    pub fn bind_member_access(
        &mut self,
        syntax: &MemberAccessSyntaxTree,
    ) -> Option<MemberAccess<IntermediateType>> {
        let value = self.bind_expression(&syntax.operand, None)?;

        // must be struct
        let IntermediateType::Type(Type::TypedID(TypedID::Struct(struct_id)))
            = value.type_binding().ty else {
            self.add_error(
                MemberAccessOnNonStruct {
                    source_span: self.source_span(syntax.span()),
                }.into()
            );
            return None;
        };

        // get the field of the struct
        let struct_symbol = self.table.get(struct_id);
        let Some(field_id) =
            struct_symbol.fields.map_name_to_id(&self.source_file()[syntax.identifier.span]) else {
            self.add_error(
                FieldNotFound {
                    source_span: self.source_span(syntax.span()),
                    struct_id
                }.into()
            );
            return None;
        };

        Some(MemberAccess {
            source_span: self.source_span(syntax.span()),
            operand: Box::new(value),
            field_id,
            struct_id,
            field_ty: struct_symbol.fields[field_id].ty.into(),
        })
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`FunctionCallSyntaxTree`] into the [`FunctionCall`].
    pub fn bind_function_call(
        &mut self,
        syntax: &FunctionCallSyntaxTree,
    ) -> Option<FunctionCall<IntermediateType>> {
        let symbol_id = self.table.resolve_symbol(
            self.function_symbol_id.into(),
            syntax.qualified_identifier.elements(),
            self.source_file(),
        );

        // Handle errors
        let symbol_id = self.handle_symbol_resolve_result(symbol_id)?;

        // expect function id
        let ID::Function(function_id) = symbol_id else {
            self.add_error(
                SymbolIsNotCallable {
                    source_span: self.source_span(syntax.span()),
                }.into()
            );
            return None;
        };

        let argument_count = syntax
            .arguments
            .as_ref()
            .map_or(0, pernixc_syntax::syntax_tree::ConnectedList::len);

        // argument count mismatch
        if argument_count != self.table.get(function_id).parameters.len() {
            self.add_error(
                ArgumentCountMismatch {
                    expected: self.table.get(function_id).parameters.len(),
                    found: argument_count,
                    source_span: self.source_span(syntax.span()),
                }
                .into(),
            );
        }

        let mut arguments = Vec::with_capacity(argument_count);

        for argument in syntax
            .arguments
            .iter()
            .flat_map(pernixc_syntax::syntax_tree::ConnectedList::elements)
        {
            arguments.push(self.bind_expression(argument, None));
        }

        let mut arguments_checked = Vec::with_capacity(arguments.len());

        for (argument, parameter) in arguments
            .into_iter()
            .zip(self.table.get(function_id).parameters.iter())
        {
            // only accepts arguments that are not errors
            let Some(argument) = argument else {
                continue;
            };

            // check if argument type is compatible with parameter type
            match self.verify_value(argument, parameter.type_binding.ty.into()) {
                Err(argument) | Ok(argument) => arguments_checked.push(argument),
            }
        }

        Some(FunctionCall {
            source_span: self.source_span(syntax.span()),
            function_id,
            arguments: arguments_checked,
            return_type: IntermediateType::Type(self.table.get(function_id).return_type),
        })
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`NamedSyntaxTree`] to a [`Named`].
    pub fn bind_named(&mut self, syntax: &NamedSyntaxTree) -> Option<Named<IntermediateType>> {
        if syntax.0.rest.is_empty() {
            // try to find variable symbol first
            let variable = self
                .variable_manager
                .map_name_to_id(&self.source_file()[syntax.0.first.span])
                .map(|index| (index, &self.variable_manager[index]));

            if let Some((variable_id, variable_symbol)) = variable {
                return Some(
                    Load {
                        source_span: self.source_span(syntax.span()),
                        ty: variable_symbol.ty,
                        lvalue: LValue {
                            is_mutable: variable_symbol.is_mutable,
                            address: VariableAddress { variable_id }.into(),
                        },
                    }
                    .into(),
                );
            }

            // try to find parameter symbol
            let parameter = self
                .function
                .parameters
                .map_name_to_id(&self.source_file()[syntax.0.first.span])
                .map(|index| (index, self.function.parameters.get_by_id(index).unwrap()));

            if let Some((parameter_id, parameter)) = parameter {
                return Some(
                    Load {
                        source_span: self.source_span(syntax.span()),
                        ty: parameter.type_binding.ty.into(),
                        lvalue: LValue {
                            is_mutable: parameter.type_binding.is_mutable,
                            address: ArgumentAddress { parameter_id }.into(),
                        },
                    }
                    .into(),
                );
            }
        }

        let symbol_id = self.table.resolve_symbol(
            self.function_symbol_id.into(),
            syntax.0.elements(),
            self.source_file(),
        );

        // Handle errors
        let symbol_id = self.handle_symbol_resolve_result(symbol_id)?;

        let ID::EnumVariant(enum_variant_id) = symbol_id else {
            self.add_error(
                ExpressionExpected {
                    source_span: self.source_span(syntax.span()),
                }.into()
            );
            return None;
        };

        Some(
            EnumLiteral {
                source_span: self.source_span(syntax.span()),
                variant_number: self.table.get(enum_variant_id).variant_number,
                enum_id: self.table.get(enum_variant_id).parent_id,
                enum_variant_id,
            }
            .into(),
        )
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`BlockSyntaxTree`] into the [`Value<IntermediateType>`].
    pub fn bind_block(
        &mut self,
        syntax: &BlockSyntaxTree,
        variable_id: Option<VariableID>,
    ) -> Option<Block<IntermediateType>> {
        let (generated, variable_id) = variable_id.map_or_else(
            || (true, VariableID::fresh()),
            |variable_id| (false, variable_id),
        );

        // allocate basic blocks
        let begin_basic_block_id = self.current_basic_block_id;
        let successor_basic_block_id = self.control_flow_graph.create_new_block();

        // add new scope
        self.variable_manager.new_scope();
        let new_block_id = self.block_manager.new_scope(
            BlockInfo::new(begin_basic_block_id, successor_basic_block_id, variable_id),
            syntax
                .label_specifier
                .map(|span| &self.source_file()[span.label.identifier.span]),
        );

        // bind statements
        for statement in &syntax.block_without_label.statements {
            if !self.bind_statement(statement) {
                return None;
            }
        }

        // connect current basic block to successor basic block
        self.control_flow_graph
            .add_jump(self.current_basic_block_id, successor_basic_block_id);

        // update current basic block id
        self.current_basic_block_id = successor_basic_block_id;

        // pop scope
        self.variable_manager.pop_scope();
        self.block_manager.pop_scope();

        // If the `ty` field in the `BlockInfo` is not `None`, it means that the block has an
        // `express` statement and is required to generate a new variable
        let (ty, has_variable) = if let Some(ty) = self.block_manager[new_block_id].ty() {
            if generated {
                self.variable_manager.add_variable(variable_id, Variable {
                    is_mutable: false,
                    ty,
                    name: None,
                });
            }

            // Ensures that the variable is initialized in every path.
            let result = self.initialize_check(
                begin_basic_block_id,
                successor_basic_block_id,
                HashSet::new(),
                variable_id,
            );

            (
                match result {
                    InitializeCheckResult::Uninitialized => {
                        self.add_error(
                            NotAllPathExpressTheValue {
                                source_span: self.source_span(syntax.span()),
                            }
                            .into(),
                        );
                        ty
                    }
                    InitializeCheckResult::Initialized => ty,
                    InitializeCheckResult::Unreachable => Type::Never.into(),
                },
                true,
            )
        } else {
            let reachable = if self.is_reachable(
                begin_basic_block_id,
                successor_basic_block_id,
                HashSet::new(),
            ) {
                Type::Primitive(PrimitiveType::Bool).into()
            } else {
                Type::Never.into()
            };

            (reachable, false)
        };

        Some(Block {
            source_span: self.source_span(syntax.span()),
            ty,
            variable_id: if has_variable {
                Some(variable_id)
            } else {
                None
            },
        })
    }

    fn is_reachable(
        &self,
        start: BasicBlockID,
        target: BasicBlockID,
        mut visited: HashSet<BasicBlockID>,
    ) -> bool {
        if visited.contains(&start) {
            // this path is terminated by a loop
            return true;
        }

        visited.insert(start);

        for instruction in self.control_flow_graph[start].instructions() {
            match instruction {
                CFGInstruction::TerminalInstruction(TerminalInstruction::JumpInstruction(
                    jump_instruction,
                )) => {
                    // found the target, return true
                    if jump_instruction.basic_block_id == target {
                        return true;
                    }

                    // continue to search
                    return self.is_reachable(jump_instruction.basic_block_id, target, visited);
                }
                CFGInstruction::TerminalInstruction(
                    TerminalInstruction::ConditionalJumpInstruction(cond_jump),
                ) => {
                    let true_reachable =
                        self.is_reachable(cond_jump.true_basic_block_id, target, visited.clone());
                    let false_reachable =
                        self.is_reachable(cond_jump.false_basic_block_id, target, visited);

                    return true_reachable || false_reachable;
                }
                _ => (),
            }
        }

        false
    }

    fn initialize_check(
        &self,
        current: BasicBlockID,
        successor: BasicBlockID,
        mut visited: HashSet<BasicBlockID>,
        target_variabl_id: VariableID,
    ) -> InitializeCheckResult {
        if visited.contains(&current) {
            // this path is terminated by a loop
            return InitializeCheckResult::Unreachable;
        }

        visited.insert(current);

        for instruction in self.control_flow_graph[current].instructions() {
            match instruction {
                // if the store instruction stores the target variable, then the target variable is
                // initialized in this path
                CFGInstruction::IRInstruction(Instruction::Store(store)) => {
                    if store.variable_id == target_variabl_id {
                        return InitializeCheckResult::Initialized;
                    }
                }
                CFGInstruction::TerminalInstruction(terminal) => match terminal {
                    // the return instruction terminates the path completely
                    TerminalInstruction::ReturnInstruction(_) => {
                        return InitializeCheckResult::Unreachable
                    }
                    TerminalInstruction::JumpInstruction(jump_instruction) => {
                        // if jump to the successor, then the target variable is not initialized in
                        // this path
                        if jump_instruction.basic_block_id == successor {
                            println!("UNREACHABLE FROM JUMP");
                            return InitializeCheckResult::Uninitialized;
                        }

                        // check the successor of the jump instruction
                        return self.initialize_check(
                            jump_instruction.basic_block_id,
                            successor,
                            visited,
                            target_variabl_id,
                        );
                    }
                    TerminalInstruction::ConditionalJumpInstruction(
                        conditional_jump_instruction,
                    ) => {
                        // if any of the successors is the target, then the target variable is not
                        // initialized in this path
                        if conditional_jump_instruction.true_basic_block_id == successor
                            || conditional_jump_instruction.false_basic_block_id == successor
                        {
                            println!("UNREACHABLE FROM CONDITIONAL JUMP");
                            return InitializeCheckResult::Uninitialized;
                        }

                        // both successors must be checked
                        let true_result = self.initialize_check(
                            conditional_jump_instruction.true_basic_block_id,
                            successor,
                            visited.clone(),
                            target_variabl_id,
                        );
                        let false_result = self.initialize_check(
                            conditional_jump_instruction.false_basic_block_id,
                            successor,
                            visited,
                            target_variabl_id,
                        );

                        println!("true: {true_result:?}, false: {false_result:?}");

                        // the result is the combination of the two results
                        return match (true_result, false_result) {
                            (InitializeCheckResult::Unreachable, other)
                            | (other, InitializeCheckResult::Unreachable) => other,
                            (InitializeCheckResult::Uninitialized, _)
                            | (_, InitializeCheckResult::Uninitialized) => {
                                InitializeCheckResult::Uninitialized
                            }
                            (
                                InitializeCheckResult::Initialized,
                                InitializeCheckResult::Initialized,
                            ) => InitializeCheckResult::Initialized,
                        };
                    }
                },
                CFGInstruction::IRInstruction(_) => (),
            }
        }

        InitializeCheckResult::Unreachable
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`ExpressSyntaxTree`] to an [`Express`].
    pub fn bind_express(&mut self, syntax: &ExpressSyntaxTree) -> Option<Express> {
        // gets the block id of the express keyword
        let block_id = if let Some(label) = syntax.label {
            // search for the block
            let Some(block_id) = self
                .block_manager
                .search_scope(&self.source_file()[label.identifier.span]) else {
                self.add_error(LabelNotFound {
                    source_span: self.source_span(label.span()),
                }.into());
                return None;
            };

            block_id
        } else {
            // gets the topmost block
            let Some(block_id) = self.block_manager.get_last_scope() else {
                self.add_error(ExpressOutsideBlock {
                    span: self.source_span(syntax.span()),
                }.into());
                return None;
            };

            block_id
        };

        match &syntax.expression {
            Some(expression) => {
                let value = if let Some(ty) = self.block_manager[block_id].ty() {
                    self.expect_value(
                        expression,
                        ty,
                        Some(self.block_manager[block_id].variable_id()),
                    )?
                } else {
                    // binds the expression
                    let value = self.bind_expression(
                        expression,
                        Some(self.block_manager[block_id].variable_id()),
                    )?;

                    // updates the type of the block
                    self.block_manager[block_id].set_ty(value.type_binding().ty);

                    value
                };

                // adds the store instruction
                if !matches!(value, Value::StructLiteral(..) | Value::Block(..)) {
                    self.control_flow_graph[self.current_basic_block_id].add_instruction(
                        Store {
                            variable_id: self.block_manager[block_id].variable_id(),
                            initialization: Initialization::Value(value),
                        }
                        .into(),
                    );
                }
            }
            None => self.control_flow_graph[self.current_basic_block_id].add_instruction(
                Store {
                    variable_id: self.block_manager[block_id].variable_id(),
                    initialization: Initialization::None,
                }
                .into(),
            ),
        }

        // add jump instruction
        self.control_flow_graph.add_jump(
            self.current_basic_block_id,
            self.block_manager[block_id].successor_basic_block_id(),
        );

        Some(Express {
            source_span: self.source_span(syntax.span()),
        })
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`IfElseSyntaxTree`] to an [`IfElse`].
    pub fn bind_if_else(
        &mut self,
        syntax: &IfElseSyntaxTree,
        variable_id: Option<VariableID>,
    ) -> Option<IfElse<IntermediateType>> {
        let (variable_id, generated) =
            variable_id.map_or_else(|| (VariableID::fresh(), true), |id| (id, false));

        // binds the condition
        let condition = self.expect_value(
            &syntax.condition,
            Type::Primitive(PrimitiveType::Bool).into(),
            None,
        )?;

        let then_block = self.control_flow_graph.create_new_block();
        let if_else_load = if let Some(else_expression) = &syntax.else_expression {
            // if with else variant
            let else_block = self.control_flow_graph.create_new_block();
            let con_block = self.control_flow_graph.create_new_block();

            // adds the jump instruction
            self.control_flow_graph.add_conditional_jump(
                self.current_basic_block_id,
                then_block,
                else_block,
                condition,
            );

            // binds the then expression
            self.current_basic_block_id = then_block;
            let then_result = self.bind_block(&syntax.then_expression, Some(variable_id))?;
            // adds the jump instruction
            self.control_flow_graph
                .add_jump(self.current_basic_block_id, con_block);

            // binds the else expression
            self.current_basic_block_id = else_block;
            let else_result = match else_expression.expression.as_ref() {
                BlockOrIfElse::Block(block) => {
                    self.bind_block(block, Some(variable_id)).map(Value::Block)
                }
                BlockOrIfElse::IfElse(if_else) => self
                    .bind_if_else(if_else, Some(variable_id))
                    .map(Value::IfElse),
            }?;
            // adds the jump instruction
            self.control_flow_graph
                .add_jump(self.current_basic_block_id, con_block);

            // current basic block is the continutation block
            self.current_basic_block_id = con_block;

            let left_ty = self.get_inferrable_type(&then_result.ty);
            let right_ty = self.get_inferrable_type(&else_result.type_binding().ty);

            let ty = match (left_ty, right_ty) {
                (InferrableType::Type(Type::Never), _) => else_result.type_binding().ty,
                (_, InferrableType::Type(Type::Never)) => then_result.ty,
                _ => {
                    // unifies the types
                    let _ = self.verify_value(else_result, then_result.ty);
                    then_result.ty
                }
            };

            // if the variable id was generated, we need to store it
            if generated {
                self.variable_manager.add_variable(variable_id, Variable {
                    ty,
                    is_mutable: false,
                    name: None,
                });
            }

            Some(IfElseLoad { variable_id, ty })
        } else {
            // if without else variant
            let con_block = self.control_flow_graph.create_new_block();

            // adds the jump instruction
            self.control_flow_graph.add_conditional_jump(
                self.current_basic_block_id,
                then_block,
                con_block,
                condition,
            );

            // binds the then expression
            self.current_basic_block_id = then_block;
            self.bind_block(&syntax.then_expression, Some(variable_id))?;
            // adds the jump instruction
            self.control_flow_graph
                .add_jump(self.current_basic_block_id, con_block);

            // current basic block is the continutation block
            self.current_basic_block_id = con_block;

            None
        };

        Some(IfElse {
            source_span: self.source_span(syntax.span()),
            if_else_load,
        })
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`ReturnSyntaxTree`] to a [`Return`].
    pub fn bind_return(&mut self, syntax: &ReturnSyntaxTree) -> Option<Return> {
        match &syntax.expression {
            Some(value) => {
                let value = self.expect_value(value, self.function.return_type.into(), None)?;
                self.control_flow_graph[self.current_basic_block_id].return_value(Some(value));
            }
            None => {
                // must be void return type only
                if self.function.return_type != PrimitiveType::Void.into() {
                    self.add_error(
                        ReturnRequiredValue {
                            source_span: self.source_span(syntax.span()),
                        }
                        .into(),
                    );

                    return None;
                }

                self.control_flow_graph[self.current_basic_block_id].return_value(None);
            }
        }

        Some(Return {
            source_span: self.source_span(syntax.span()),
        })
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
}

#[cfg(test)]
mod tests;
