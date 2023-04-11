//! Contains the definition [`Builder`].

use std::{
    borrow::Borrow,
    collections::HashMap,
    hash::Hash,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use pernixc_common::source_file::{SourceElement, SourceFile, Span};
use pernixc_lexical::token::NumericLiteral as NumericLiteralToken;
use pernixc_syntax::syntax_tree::{
    expression::{
        Binary as BinarySyntaxTree, BinaryOperator as BinaryOperatorSyntaxTree,
        BooleanLiteral as BooleanLiteralSyntaxTree, Expression as ExpressionSyntaxTree,
        FieldInitializerList, FunctionCall as FunctionCallSyntaxTree,
        Functional as FunctionalSyntaxTree, Imperative as ImperativeSyntaxTree,
        MemberAccess as MemberAccessSyntaxTree, Named as NamedSyntaxTree,
        Prefix as PrefixSyntaxTree, PrefixOperator as PrefixOperatorSyntaxTree,
        StructLiteral as StructLiteralSyntaxTree,
    },
    statement::{
        Declarative as DeclarativeSyntaxTree, Expressive as ExpressiveSyntaxTree,
        Statement as StatementSyntaxTree, VariableDeclaration as VariableDeclarationSyntaxTree,
        VariableTypeBindingSpecifier,
    },
};

use super::{
    instruction::Instruction,
    value::{
        ArgumentAddress, Binary, BinaryOperator, BooleanLiteral, Category, EnumLiteral,
        FunctionCall, InferrableType, IntermediateType, LValue, Load, MemberAccess, Named,
        NumericLiteral, Prefix, StructLiteral, TypeBinding, Value, ValueTrait, Variable,
        VariableAddress, VariableID,
    },
};
use crate::{
    control_flow_graph::{BasicBlockID, ControlFlowGraph, Instruction as CFGInstruction},
    errors::{
        ArgumentCountMismatch, AssignToImmutable, AssignToRValue, DuplicateFieldInitialization,
        ExpressionExpected, FieldIsNotAccessible, FieldNotFound, IncompleteFieldInitialization,
        InvalidBinaryOperation, InvalidNumericLiteralSuffix, InvalidPrefixOperation,
        MemberAccessOnNonStruct, SemanticError, StructExpected, SymbolIsNotCallable, TypeMismatch,
    },
    hir::{
        instruction::Store,
        value::{PrefixOperator, StructFieldAddress},
    },
    infer::{Constraint, InferenceContext, InferenceID, Infererence, UnificationError},
    symbol::{
        ty::{PrimitiveType, Type},
        AccessModifier, FieldID, Function, FunctionID, ResolveError, StructID, Table,
        TypeResolveError, TypedID, ID,
    },
    SemanticResult, SourceSpan,
};

/// Is a data structure that emulates a stack of scopes in the form of a vector of [`HashMap`]s.
///
/// The scopes are represented as [`HashMap`]s and the vector is used to emulate a stack of scopes.
/// The search for a value in the [`ScopeMap`] is performed from the last scope to the first scope.
#[derive(Debug, Clone)]
pub struct ScopeMap<K, V> {
    scopes: Vec<HashMap<K, V>>,
}

impl<K, V> ScopeMap<K, V> {
    /// Creates a new [`ScopeMap`] with a single scope.
    #[must_use]
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    /// Starts a new scope.
    pub fn push(&mut self) { self.scopes.push(HashMap::new()); }

    /// Pops the last scope.
    pub fn pop(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Inserts a new value in the last scope. If the key already exists in the last scope, the
    /// previous value is returned (shadowing).
    pub fn insert(&mut self, key: K, value: V) -> Option<V>
    where
        K: Eq + Hash,
    {
        self.scopes.last_mut().unwrap().insert(key, value)
    }

    /// Searches for a value in the scopes. The search is performed from the last scope to the first
    /// scope. If the value is found, it is returned. Otherwise, `None` is returned.
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q> + Eq + Hash,
        Q: Eq + Hash + ?Sized,
    {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(key) {
                return Some(value);
            }
        }

        None
    }
}

impl<K, V> Default for ScopeMap<K, V> {
    fn default() -> Self { Self::new() }
}

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
    current_block_id: BasicBlockID,

    variables: HashMap<VariableID, Variable<IntermediateType>>,
    scope_map: ScopeMap<&'a str, VariableID>,
}

impl<'a> Builder<'a> {
    /// Creates a new [`Builder`] instance targeting the function in the [`Table`] with the given
    /// [`FunctionID`].
    ///
    /// # Panics
    /// Panics if the function with the given [`FunctionID`] does not exist in the [`Table`].
    #[must_use]
    pub fn new(table: &'a Table, function_symbol_id: FunctionID) -> Self {
        Self {
            table,
            function: table.get(function_symbol_id),
            function_symbol_id,
            control_flow_graph: ControlFlowGraph::new(),
            inference_context: InferenceContext::new(),
            current_block_id: BasicBlockID::ENTRY_BLOCK_ID,
            errors: Vec::new(),
            produce_error: true,
            variables: HashMap::new(),
            scope_map: ScopeMap::new(),
        }
    }

    /// Binds the given statement syntax tree into an instruction and adds it to the current basic
    /// block.
    pub fn bind_statement(&mut self, syntax: &StatementSyntaxTree) {
        match syntax {
            StatementSyntaxTree::Declarative(DeclarativeSyntaxTree::VariableDeclaration(var)) => {
                self.bind_variable_declaration(var);
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
                let Some(value) = value else { return; };

                // add value evalutation instruction
                self.control_flow_graph[self.current_block_id].add_instruction(
                    CFGInstruction::IRInstruction(Instruction::EvaluateValue(value)),
                );
            }
        }
    }

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

    fn new_variable_id() -> VariableID {
        // use atomic variable to generate unique variable ids
        static NEXT_VARIABLE_ID: AtomicUsize = AtomicUsize::new(0);

        VariableID(NEXT_VARIABLE_ID.fetch_add(1, Ordering::SeqCst))
    }

    /// Binds the variable declaration statement into an appropriate [`Instruction`] and adds it to
    /// the current basic block.
    pub fn bind_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclarationSyntaxTree,
    ) -> Option<VariableAddress> {
        let (initializer, variable_id, ty, is_mutable) =
            match &variable_declaration.variable_type_binding_specifier {
                VariableTypeBindingSpecifier::TypeBindingSpecifier(type_binding) => {
                    let ty = self.table.resolve_type(
                        self.function_symbol_id.into(),
                        &type_binding.type_specifier,
                        self.source_file(),
                    );
                    let ty = self.handle_type_resolve_result(ty)?;

                    let variable_id = Self::new_variable_id();
                    let initializer_value = self.expect_value(
                        &variable_declaration.expression,
                        ty.into(),
                        Some(VariableAddress { variable_id }),
                    )?;

                    (
                        initializer_value,
                        variable_id,
                        ty.into(),
                        type_binding.mutable_keyword.is_some(),
                    )
                }
                VariableTypeBindingSpecifier::LetBindingSpecifier(let_binding) => {
                    let variable_id = Self::new_variable_id();
                    let initializer_value = self.bind_expression(
                        &variable_declaration.expression,
                        Some(VariableAddress { variable_id }),
                    )?;

                    let ty = initializer_value.type_binding().ty;
                    (
                        initializer_value,
                        variable_id,
                        ty,
                        let_binding.mutable_keyword.is_some(),
                    )
                }
            };

        let variable_name = &self.source_file()[variable_declaration.identifier.span];
        let variable = Variable {
            name: Some(variable_name.to_string()),
            ty,
            is_mutable,
        };

        // insert variable into variable map
        self.variables.insert(variable_id, variable);
        self.scope_map.insert(variable_name, variable_id);

        // if the value is not struct literal, add a store instruction
        if !matches!(initializer, Value::StructLiteral(_)) {
            self.control_flow_graph[self.current_block_id].add_instruction(
                CFGInstruction::IRInstruction(
                    Store {
                        store_address: VariableAddress { variable_id }.into(),
                        value: initializer,
                    }
                    .into(),
                ),
            );
        }

        Some(VariableAddress { variable_id })
    }

    /// Binds the given [`ExpressionSyntaxTree`] into the [`Value<IntermediateType>`].
    pub fn bind_expression(
        &mut self,
        syntax: &ExpressionSyntaxTree,
        variable_address: Option<VariableAddress>,
    ) -> Option<Value<IntermediateType>> {
        match syntax {
            ExpressionSyntaxTree::Functional(syntax) => {
                self.bind_functional(syntax, variable_address)
            }
            ExpressionSyntaxTree::Imperative(syntax) => {
                self.bind_imperative(syntax, variable_address)
            }
        }
    }

    /// Binds the given [`FunctionalSyntaxTree`] into the [`Value<IntermediateType>`].
    pub fn bind_functional(
        &mut self,
        syntax: &FunctionalSyntaxTree,
        variable_address: Option<VariableAddress>,
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
                .bind_struct_literal(syntax, variable_address)
                .map(Value::StructLiteral),
            FunctionalSyntaxTree::MemberAccess(syntax) => {
                self.bind_member_access(syntax).map(Value::MemberAccess)
            }
            FunctionalSyntaxTree::Continue(_) => todo!(),
            FunctionalSyntaxTree::Break(_) => todo!(),
            FunctionalSyntaxTree::Return(_) => todo!(),
            FunctionalSyntaxTree::Express(_) => todo!(),
            FunctionalSyntaxTree::Cast(_) => todo!(),
        }
    }

    /// Binds the given [`ImperativeSyntaxTree`] into the [`Value<IntermediateType>`].
    pub fn bind_imperative(
        &mut self,
        syntax: &ImperativeSyntaxTree,
        _variable_address: Option<VariableAddress>,
    ) -> Option<Value<IntermediateType>> {
        match syntax {
            ImperativeSyntaxTree::Block(_) => todo!(),
            ImperativeSyntaxTree::IfElse(_) => todo!(),
            ImperativeSyntaxTree::Loop(_) => todo!(),
        }
    }

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
            let Some(argument) = self.verify_value(argument, parameter.type_binding.ty.into()) else {
                continue;
            };

            arguments_checked.push(argument);
        }

        Some(FunctionCall {
            source_span: self.source_span(syntax.span()),
            function_id,
            arguments: arguments_checked,
            return_type: IntermediateType::Type(self.table.get(function_id).return_type),
        })
    }

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

    fn source_file(&self) -> &'a Arc<SourceFile> { &self.function.syntax_tree.source_file }

    /// Binds the given [`NamedSyntaxTree`] to a [`Named`].
    pub fn bind_named(&mut self, syntax: &NamedSyntaxTree) -> Option<Named<IntermediateType>> {
        if syntax.0.rest.is_empty() {
            // try to find variable symbol first
            let variable = self
                .scope_map
                .get(&self.source_file()[syntax.0.first.span])
                .copied()
                .map(|index| (index, self.variables.get(&index).unwrap()));

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

    fn create_field_initialization(
        &mut self,
        struct_id: StructID,
        field_initialization: &Option<FieldInitializerList>,
    ) -> HashMap<FieldID, Option<Value<IntermediateType>>> {
        let mut initialized_field = HashMap::new();
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
            if initialized_field.contains_key(&field_id) {
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
            initialized_field.insert(field_id, initializer);
        }

        initialized_field
    }

    /// Binds the given [`StructLiteralSyntaxTree`] to a [`StructLiteral`].
    pub fn bind_struct_literal(
        &mut self,
        syntax: &StructLiteralSyntaxTree,
        variable_address: Option<VariableAddress>,
    ) -> Option<StructLiteral> {
        let (generated, variable_address) = variable_address.map_or_else(
            || {
                (true, VariableAddress {
                    variable_id: Self::new_variable_id(),
                })
            },
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
            self.variables
                .insert(variable_address.variable_id, Variable {
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
        for (field_id, initializer) in initialized_field {
            let Some(initializer) = initializer else {
                continue;
            };

            self.control_flow_graph[self.current_block_id].add_instruction(
                CFGInstruction::IRInstruction(
                    Store {
                        store_address: StructFieldAddress {
                            struct_address: Box::new(variable_address.into()),
                            field_id,
                        }
                        .into(),
                        value: initializer,
                    }
                    .into(),
                ),
            );
        }

        Some(StructLiteral {
            source_span: self.source_span(syntax.span()),
            struct_id,
            struct_load_address: variable_address,
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

    /// Binds the given [`ExpressionSyntaxTree`] into the [`Value<IntermediateType>`] with
    /// additional type verification.
    pub fn expect_value(
        &mut self,
        syntax: &ExpressionSyntaxTree,
        ty: IntermediateType,
        variable_address: Option<VariableAddress>,
    ) -> Option<Value<IntermediateType>> {
        let value = self.bind_expression(syntax, variable_address)?;
        self.verify_value(value, ty)
    }

    /// Binds the given [`BooleanLiteralSyntaxTree`] into the [`BooleanLiteral`].
    pub fn bind_boolean_literal(&mut self, syntax: &BooleanLiteralSyntaxTree) -> BooleanLiteral {
        BooleanLiteral {
            source_span: self.source_span(syntax.span()),
            value: syntax.as_true().is_some(),
        }
    }

    /// Adds the semantic error to the list of errors.
    pub fn add_error(&mut self, error: SemanticError) {
        if self.produce_error {
            self.errors.push(error);
        }
    }

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

    fn check_inference_and_concrete(
        &mut self,
        inference_id: InferenceID,
        concrete_type: Type,
        span: SourceSpan,
        reversed: bool,
    ) -> bool {
        let Err(err) = self.inference_context.unify_with_concrete(inference_id, concrete_type) else {
                    return true;
                };

        match err {
            UnificationError::TypeMismatch { left, right } => {
                self.add_error(
                    TypeMismatch {
                        source_span: span,
                        expected: if reversed {
                            InferrableType::Type(left)
                        } else {
                            InferrableType::Type(right)
                        },
                        actual: if reversed {
                            InferrableType::Type(right)
                        } else {
                            InferrableType::Type(left)
                        },
                    }
                    .into(),
                );
            }
            UnificationError::ConstraintNotSatisfied {
                constraint,
                concrete_type,
            } => {
                self.add_error(
                    TypeMismatch {
                        source_span: span,
                        expected: if reversed {
                            InferrableType::Inferring(constraint)
                        } else {
                            InferrableType::Type(concrete_type)
                        },
                        actual: if reversed {
                            InferrableType::Type(concrete_type)
                        } else {
                            InferrableType::Inferring(constraint)
                        },
                    }
                    .into(),
                );
            }
        }

        false
    }

    /// Verfies whether the given [`Value<IntermediateType>`] is/satisfies the given
    /// [`InferrableType`].
    pub fn verify_value(
        &mut self,
        value: Value<IntermediateType>,
        expected: IntermediateType,
    ) -> Option<Value<IntermediateType>> {
        let result = match (value.type_binding().ty, expected) {
            (IntermediateType::Inference(left), IntermediateType::Inference(right)) => 'result: {
                let Err(err) = self.inference_context.unify(left, right) else {
                    break 'result true;
                };

                match err {
                    UnificationError::TypeMismatch { left, right } => {
                        self.add_error(
                            TypeMismatch {
                                source_span: value.source_span(),
                                expected: InferrableType::Type(left),
                                actual: InferrableType::Type(right),
                            }
                            .into(),
                        );
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
                    }
                }

                false
            }
            (IntermediateType::Inference(left), IntermediateType::Type(right)) => {
                Self::check_inference_and_concrete(self, left, right, value.source_span(), false)
            }
            (IntermediateType::Type(left), IntermediateType::Inference(right)) => {
                Self::check_inference_and_concrete(self, right, left, value.source_span(), true)
            }
            (IntermediateType::Type(left), IntermediateType::Type(right)) => {
                if left == right {
                    true
                } else {
                    self.add_error(
                        TypeMismatch {
                            source_span: value.source_span(),
                            expected: InferrableType::Type(right),
                            actual: InferrableType::Type(left),
                        }
                        .into(),
                    );
                    false
                }
            }
        };

        if result {
            Some(value)
        } else {
            None
        }
    }

    fn source_span(&self, span: Span) -> SourceSpan {
        SourceSpan {
            source_file: self.function.syntax_tree.source_file.clone(),
            span,
        }
    }
}

#[cfg(test)]
mod tests;