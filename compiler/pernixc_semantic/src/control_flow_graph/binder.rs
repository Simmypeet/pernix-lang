use std::{collections::HashMap, sync::Arc};

use pernixc_common::source_file::{SourceElement, SourceFile, Span};
use pernixc_lexical::token::NumericLiteral as NumericLiteralToken;
use pernixc_syntax::syntax_tree::{
    expression::{
        BooleanLiteral as BooleanLiteralSyntaxTree, Expression as ExpressionSyntaxTree,
        Functional as FunctionalSyntaxTree, Imperative as ImperativeSyntaxTree,
        Prefix as PrefixSyntaxTree, PrefixOperator as PrefixOperatorSyntaxTree,
    },
    statement::{
        VariableDeclaration as VariableDeclarationSyntaxTree, VariableTypeBindingSpecifier,
    },
};

use super::{
    BasicBlock, BasicBlockID, ControlFlowGraph, FunctionAnalyzeError, StoreAddress,
    StoreInstruction, TemporaryVariable,
};
use crate::{
    binding::{
        Binding, BooleanLiteral, Cast, Expression, NumericLiteral, NumericLiteralValue, Prefix,
        PrefixOperator, ValueCategory, ValueType,
    },
    errors::{
        InvalidNumericLiteralSuffix, InvalidPrefixOperation, OutOfRangeNumericLiteral,
        SemanticError, TypeMismatch,
    },
    symbol::{
        ty::{PrimitiveType, Type, TypeBinding},
        Function, FunctionID, LocalVariable, Table,
    },
    SourceSpan,
};

/// Is a data structure that implements a stack of scopes.
///
/// Each scope is a [`HashMap`] that maps a name to a value. It's used to represent the scope rules
/// of the language.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct NameStack<T> {
    stack: Vec<HashMap<String, T>>,
}

impl<T> NameStack<T> {
    /// Creates a new [`NameStack`]
    ///
    /// The stack will contain a single scope.
    #[must_use]
    fn new() -> Self {
        Self {
            stack: vec![HashMap::new()],
        }
    }

    /// Pushes a new scope onto the stack.
    ///
    /// The underneath names will be hidden by the new scope.
    fn push(&mut self) { self.stack.push(HashMap::new()); }

    /// Pops the last scope from the stack.
    ///
    /// If there is only one scope left, it will not be popped.
    fn pop(&mut self) {
        if self.stack.len() > 1 {
            self.stack.pop();
        }
    }

    /// Inserts a new name and its value pair into the current scope.
    fn insert(&mut self, name: String, value: T) {
        self.stack.last_mut().unwrap().insert(name, value);
    }

    /// Returns the value of a name if it exists in the stack.
    ///
    /// The search is performed from the last scope to the first.
    #[must_use]
    fn get(&self, name: &str) -> Option<&T> {
        for scope in self.stack.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }

        None
    }

    /// Returns the value of a name if it exists in the stack.
    ///
    /// The search is performed from the last scope to the first.
    #[must_use]
    fn get_mut(&mut self, name: &str) -> Option<&mut T> {
        for scope in self.stack.iter_mut().rev() {
            if let Some(value) = scope.get_mut(name) {
                return Some(value);
            }
        }

        None
    }
}

/// Is a data structure that emulates a stack of scopes that maps names to values.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct ScopeMap<T> {
    values: Vec<T>,
    name_stack: NameStack<usize>,
}

impl<T> ScopeMap<T> {
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            name_stack: NameStack::new(),
        }
    }

    pub fn push_new_scope(&mut self) { self.name_stack.push(); }

    pub fn pop_scope(&mut self) { self.name_stack.pop() }

    pub fn push_value(&mut self, name: String, value: T) -> usize {
        let index = self.values.len();
        self.values.push(value);
        self.name_stack.insert(name, index);
        index
    }

    pub fn get(&self, name: &str) -> Option<&T> {
        let index = *self.name_stack.get(name)?;
        Some(&self.values[index])
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut T> {
        let index = *self.name_stack.get(name)?;
        Some(&mut self.values[index])
    }
}

#[derive(Debug, Clone)]
pub(super) struct ControlFlowGraphBinder<'a> {
    ids_by_basic_block: HashMap<BasicBlockID, BasicBlock>,
    pub(super) function_symbol: &'a Function,
    function_symbol_id: FunctionID,
    symbol_table: &'a Table,
    errors: Vec<SemanticError>,
    current_block_id: BasicBlockID,
    current_block: BasicBlockID,
    terminal_block_ids: Vec<BasicBlockID>,
    variable_map: ScopeMap<LocalVariable>,
    temporary_variable_map: ScopeMap<TemporaryVariable>,
}

impl<'a> ControlFlowGraphBinder<'a> {
    pub(super) fn new(symbol_table: &'a Table, function_id: FunctionID) -> Option<Self> {
        let function_symbol = symbol_table.get(function_id)?;

        let mut binder = Self {
            ids_by_basic_block: HashMap::new(),
            function_symbol,
            function_symbol_id: function_id,
            symbol_table,
            errors: Vec::new(),
            current_block_id: BasicBlockID(0),
            current_block: BasicBlockID(0),
            terminal_block_ids: Vec::new(),
            variable_map: ScopeMap::new(),
            temporary_variable_map: ScopeMap::new(),
        };

        binder.push_block();

        Some(binder)
    }

    /// Creates a new default basic block and returns a reference to it.
    ///
    /// The default basic block has only its identifier set.
    fn push_block(&mut self) -> &mut BasicBlock {
        let basic_block_id = self.current_block_id;
        self.current_block_id.0 += 1;
        self.ids_by_basic_block.insert(basic_block_id, BasicBlock {
            basic_block_id,
            predecessor_block_ids: Vec::new(),
            successor_block_ids: Vec::new(),
            instructions: Vec::new(),
        });
        self.current_block = basic_block_id;
        self.ids_by_basic_block.get_mut(&basic_block_id).unwrap()
    }

    fn source_file(&self) -> &'a Arc<SourceFile> { &self.function_symbol.syntax_tree.source_file }

    fn bind_expression(
        &mut self,
        expression_syntax: &ExpressionSyntaxTree,
        type_hint: Option<Type>,
    ) -> Option<Expression> {
        match expression_syntax {
            ExpressionSyntaxTree::Imperative(expression_syntax) => {
                self.bind_imperative(expression_syntax, type_hint)
            }
            ExpressionSyntaxTree::Functional(expression_syntax) => {
                self.bind_functional(expression_syntax, type_hint)
            }
        }
    }

    pub(super) fn bind_imperative(
        &mut self,
        expression_syntax: &ImperativeSyntaxTree,
        type_hint: Option<Type>,
    ) -> Option<Expression> {
        todo!()
    }

    pub(super) fn bind_functional(
        &mut self,
        expression_syntax: &FunctionalSyntaxTree,
        _type_hint: Option<Type>, // will be used for the future, keep the api
    ) -> Option<Expression> {
        match expression_syntax {
            FunctionalSyntaxTree::NumericLiteral(numeric_literal) => {
                self.bind_numeric_literal(numeric_literal)
            }
            FunctionalSyntaxTree::BooleanLiteral(boolean_literal) => {
                Some(self.bind_boolean_literal(boolean_literal))
            }
            FunctionalSyntaxTree::Binary(_) => todo!(),
            FunctionalSyntaxTree::Prefix(prefix) => self.bind_prefix(prefix),
            FunctionalSyntaxTree::Named(named) => todo!(),
            FunctionalSyntaxTree::FunctionCall(_) => todo!(),
            FunctionalSyntaxTree::Parenthesized(_) => todo!(),
            FunctionalSyntaxTree::StructLiteral(_) => todo!(),
            FunctionalSyntaxTree::MemberAccess(_) => todo!(),
            FunctionalSyntaxTree::Continue(_) => todo!(),
            FunctionalSyntaxTree::Break(_) => todo!(),
            FunctionalSyntaxTree::Return(_) => todo!(),
            FunctionalSyntaxTree::Express(_) => todo!(),
            FunctionalSyntaxTree::Cast(_) => todo!(),
        }
    }

    fn numeric_literal_suffix_to_numeric_type(
        &mut self,
        suffix_span: &Span,
    ) -> Option<PrimitiveType> {
        match &self.source_file()[*suffix_span] {
            "i8" => Some(PrimitiveType::Int8),
            "i16" => Some(PrimitiveType::Int16),
            "i32" => Some(PrimitiveType::Int32),
            "i64" => Some(PrimitiveType::Int64),
            "u8" => Some(PrimitiveType::Uint8),
            "u16" => Some(PrimitiveType::Uint16),
            "u32" => Some(PrimitiveType::Uint32),
            "u64" => Some(PrimitiveType::Uint64),
            "f32" => Some(PrimitiveType::Float32),
            "f64" => Some(PrimitiveType::Float64),
            _ => {
                self.errors.push(
                    InvalidNumericLiteralSuffix {
                        span: SourceSpan::new(self.source_file().clone(), *suffix_span),
                    }
                    .into(),
                );
                None
            }
        }
    }

    fn parse_numeric_literal_value(
        &mut self,
        span: &Span,
        suffix_type: PrimitiveType,
    ) -> Option<NumericLiteralValue> {
        let numeric_string = &self.source_file()[*span];
        let is_float = numeric_string.contains('.');
        let suffix_type_is_float =
            matches!(suffix_type, PrimitiveType::Float32 | PrimitiveType::Float64);

        // the numeric literal is a float but the suffix type is not a float
        if is_float && !suffix_type_is_float {
            self.errors.push(SemanticError::InvalidNumericLiteralSuffix(
                InvalidNumericLiteralSuffix {
                    span: SourceSpan::new(self.source_file().clone(), *span),
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

        numeric_value.map_or_else(
            || {
                self.errors.push(
                    OutOfRangeNumericLiteral {
                        span: SourceSpan::new(self.source_file().clone(), *span),
                    }
                    .into(),
                );
                None
            },
            Some,
        )
    }

    fn bind_numeric_literal(
        &mut self,
        expression_syntax: &NumericLiteralToken,
    ) -> Option<Expression> {
        let numeric_string = &self.source_file()[expression_syntax.value_span];
        let is_float = numeric_string.contains('.');

        // the numeric literal has a suffix type specifier
        if let Some(suffix_span) = expression_syntax.suffix_span {
            let suffix_type = self.numeric_literal_suffix_to_numeric_type(&suffix_span)?;
            let numeric_value =
                self.parse_numeric_literal_value(&expression_syntax.value_span, suffix_type)?;

            Some(
                NumericLiteral {
                    value: numeric_value,
                    span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                }
                .into(),
            )
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

            Some(
                NumericLiteral {
                    value: numeric_value,
                    span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                }
                .into(),
            )
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

            i32::try_from(numeric_value).map_or_else(
                |_| {
                    Some(
                        NumericLiteral {
                            value: NumericLiteralValue::Int64(numeric_value),
                            span: SourceSpan::new(
                                self.source_file().clone(),
                                expression_syntax.span(),
                            ),
                        }
                        .into(),
                    )
                },
                |i32_value| {
                    Some(
                        NumericLiteral {
                            value: NumericLiteralValue::Int32(i32_value),
                            span: SourceSpan::new(
                                self.source_file().clone(),
                                expression_syntax.span(),
                            ),
                        }
                        .into(),
                    )
                },
            )
        }
    }

    fn expect_expression(
        &mut self,
        expression_syntax: &ExpressionSyntaxTree,
        expected_type: Type,
    ) -> Option<Expression> {
        let expression = self.bind_expression(expression_syntax, Some(expected_type))?;
        self.verify_expression(expression, expected_type)
    }

    fn verify_expression(
        &mut self,
        expression: Expression,
        expected_type: Type,
    ) -> Option<Expression> {
        // try to perform implicit cast
        if expression.value_type().ty == expected_type {
            Some(expression)
        } else if Self::is_implicitly_castable(expression.value_type().ty, expected_type) {
            Some(
                Cast {
                    span: expression.span().clone(),
                    expression: Box::new(expression),
                    target_type: expected_type,
                }
                .into(),
            )
        } else {
            self.errors.push(
                TypeMismatch {
                    expected: expected_type,
                    actual: expression.value_type().ty,
                    span: expression.span().clone(),
                }
                .into(),
            );
            None
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
                PrimitiveType::Int8 | PrimitiveType::Uint8 => 1,
                PrimitiveType::Int16 | PrimitiveType::Uint16 => 2,
                PrimitiveType::Int32 | PrimitiveType::Uint32 => 3,
                PrimitiveType::Int64 | PrimitiveType::Uint64 => 4,
                PrimitiveType::Float32 => 5,
                PrimitiveType::Float64 => 6,
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

    fn bind_prefix(&mut self, expression_syntax: &PrefixSyntaxTree) -> Option<Expression> {
        match expression_syntax.operator {
            PrefixOperatorSyntaxTree::LogicalNot(..) => {
                let expression = self.expect_expression(
                    expression_syntax.operand.as_ref(),
                    Type::Primitive(PrimitiveType::Bool),
                )?;
                Some(
                    Prefix {
                        span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                        operator: PrefixOperator::LogicalNot,
                        operand: Box::new(expression),
                        value_type: ValueType::new(
                            Type::Primitive(PrimitiveType::Bool),
                            ValueCategory::RValue,
                        ),
                    }
                    .into(),
                )
            }
            PrefixOperatorSyntaxTree::Negate(..) => {
                let mut expression = self.bind_expression(&expression_syntax.operand, None)?;

                if !expression.value_type().ty.is_arithmetic() {
                    self.errors.push(
                        InvalidPrefixOperation {
                            span: SourceSpan::new(
                                self.source_file().clone(),
                                expression_syntax.span(),
                            ),
                            operator: PrefixOperator::Negate,
                            operand_type: expression.value_type(),
                        }
                        .into(),
                    );
                    return None;
                }

                // if not signed type, try to promote to higher-rank signed type
                if !matches!(
                    expression.value_type().ty,
                    Type::Primitive(
                        PrimitiveType::Int8
                            | PrimitiveType::Int16
                            | PrimitiveType::Int32
                            | PrimitiveType::Int64
                            | PrimitiveType::Float32
                            | PrimitiveType::Float64
                    )
                ) {
                    match expression.value_type().ty {
                        Type::Primitive(PrimitiveType::Uint8) => {
                            expression = Expression::Cast(Cast {
                                span: expression.span().clone(),
                                expression: Box::new(expression),
                                target_type: Type::Primitive(PrimitiveType::Int8),
                            });
                        }
                        Type::Primitive(PrimitiveType::Uint16) => {
                            expression = Expression::Cast(Cast {
                                span: expression.span().clone(),
                                expression: Box::new(expression),
                                target_type: Type::Primitive(PrimitiveType::Int16),
                            });
                        }
                        Type::Primitive(PrimitiveType::Uint32) => {
                            expression = Expression::Cast(Cast {
                                span: expression.span().clone(),
                                expression: Box::new(expression),
                                target_type: Type::Primitive(PrimitiveType::Int32),
                            });
                        }
                        _ => {
                            self.errors.push(SemanticError::InvalidPrefixOperation(
                                InvalidPrefixOperation {
                                    span: SourceSpan::new(
                                        self.source_file().clone(),
                                        expression_syntax.span(),
                                    ),
                                    operator: PrefixOperator::Negate,
                                    operand_type: expression.value_type(),
                                },
                            ));
                            return None;
                        }
                    }
                }

                Some(
                    Prefix {
                        span: SourceSpan::new(self.source_file().clone(), expression_syntax.span()),
                        operator: PrefixOperator::Negate,
                        value_type: ValueType {
                            ty: expression.value_type().ty,
                            category: ValueCategory::RValue,
                        },
                        operand: Box::new(expression),
                    }
                    .into(),
                )
            }
        }
    }

    pub(super) fn bind_boolean_literal(
        &mut self,
        expression_syntax: &BooleanLiteralSyntaxTree,
    ) -> Expression {
        let span = SourceSpan::new(self.source_file().clone(), expression_syntax.span());
        match expression_syntax {
            BooleanLiteralSyntaxTree::True(_) => BooleanLiteral { value: true, span }.into(),
            BooleanLiteralSyntaxTree::False(_) => BooleanLiteral { value: false, span }.into(),
        }
    }

    pub(super) fn bind_variable_declaration(
        &mut self,
        syntax_tree: &VariableDeclarationSyntaxTree,
    ) -> Option<StoreInstruction> {
        let variable_name = self.source_file()[syntax_tree.identifier.span].to_string();

        // bind the variable type binding specifier
        let (initializer, type_binding) = {
            match &syntax_tree.variable_type_binding_specifier {
                VariableTypeBindingSpecifier::TypeBindingSpecifier(type_binding) => {
                    // resolve type specifier syntax
                    let ty = self
                        .symbol_table
                        .resolve_type(
                            self.function_symbol_id.into(),
                            &type_binding.type_specifier,
                            self.source_file(),
                        )
                        .map_or_else(
                            |err| {
                                self.errors.push(err.into_semantic_error().unwrap());
                                None
                            },
                            Some,
                        )?;

                    // bind the initializer expression
                    let initializer = self.bind_expression(&syntax_tree.expression, Some(ty))?;

                    (initializer, TypeBinding {
                        ty,
                        is_mutable: type_binding.mutable_keyword.is_some(),
                    })
                }
                VariableTypeBindingSpecifier::LetBindingSpecifier(let_binding) => {
                    // bind the initializer expression
                    let initializer = self.bind_expression(&syntax_tree.expression, None)?;

                    // infer the type of the initializer expression
                    let type_binding = TypeBinding {
                        ty: initializer.value_type().ty,
                        is_mutable: let_binding.mutable_keyword.is_some(),
                    };
                    (initializer, type_binding)
                }
            }
        };

        // create variable
        let variable = LocalVariable {
            name: variable_name.clone(),
            type_binding,
        };

        // insert variable into scope
        let local_variable_index = self.variable_map.push_value(variable_name, variable);

        Some(StoreInstruction {
            store_address: StoreAddress::LocalVariableSymbolIndex(local_variable_index),
            expression: initializer,
        })
    }

    pub(super) fn get_current_block(&self) -> &BasicBlock {
        self.ids_by_basic_block.get(&self.current_block).unwrap()
    }

    pub(super) fn get_current_block_mut(&mut self) -> &mut BasicBlock {
        self.ids_by_basic_block
            .get_mut(&self.current_block)
            .unwrap()
    }

    pub(super) fn into_control_flow_graph(self) -> Result<ControlFlowGraph, FunctionAnalyzeError> {
        if self.errors.is_empty() {
            Ok(ControlFlowGraph {
                ids_by_basic_block: self.ids_by_basic_block,
                variables: self.variable_map.values,
                terminal_block_ids: self.terminal_block_ids,
                temporary_variables: self.temporary_variable_map.values,
            })
        } else {
            Err(FunctionAnalyzeError::SemanticErrors(self.errors))
        }
    }

    pub(super) fn terminate(self) -> FunctionAnalyzeError {
        FunctionAnalyzeError::SemanticErrors(self.errors)
    }
}
