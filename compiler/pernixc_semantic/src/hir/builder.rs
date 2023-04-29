use std::{convert::Into, sync::Arc};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::{SourceElement, Span};
use pernixc_lexical::token::NumericLiteral as NumericLiteralToken;
use pernixc_syntax::syntax_tree::{
    expression::{
        BooleanLiteral as BooleanLiteralSyntaxTree, Expression as ExpressionSyntaxTree,
        FunctionCall as FunctionCallSyntaxTree, Functional as FunctionalSyntaxTree,
        Imperative as ImperativeSyntaxTree, Prefix as PrefixSyntaxTree, PrefixOperator,
    },
    ConnectedList,
};

use super::{
    binding::{Binding, FunctionCall, Prefix},
    errors::{
        AmbiguousFunctionCall, NoAccessibleOverload, NoOverloadWithMatchingArgumentTypes,
        SemanticError, SymbolNotCallable, TypeMismatch,
    },
    instruction::Basic,
    Address, AllocaID, BooleanLiteral, Hir, Inspectable, Literal, NumericLiteral, Reachability,
    SsaValue, SsaVariable, SsaVariableData, SsaVariableID,
};
use crate::{
    cfg::BasicBlockID,
    hir::{
        errors::{
            FloatingPointLiteralHasIntegralSuffix, InvalidNumericLiteralSuffix,
            NoOverloadWithMatchingNumberOfArguments,
        },
        ValueTypeID,
    },
    infer::{
        Constraint, ConstraintNotSatisfiedError, InferableType, TypeMismatchError, UnificationError,
    },
    symbol::{
        errors::SymbolError,
        table::Table,
        ty::{PrimitiveType, Type},
        GlobalID, Overload, OverloadID, OverloadSetID,
    },
    OkWithErr,
};

/// Specifies the [`Builder`] output of the binding process.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum BindTarget {
    /// Binds the syntax tree for the address.
    ///
    /// This is typically used to get the address of the l-value expression.
    ForAddress,

    /// Binds the syntax tree for the value.
    ///
    /// For a non-literal r-value expression (e.g. function call, arithmetic expression, etc.)
    /// the builder will bind the syntax tree to [`Binding`], generate a new [`SsaVariable`],
    /// assign the [`Binding`] to the [`SsaVariable`] and return the [`SsaVariableID`] as the
    /// value.
    ///
    /// For a literal r-value expression (e.g. integer literal, boolean literal, etc.) the builder
    /// will directly return the [`Literal`] as the value.
    #[default]
    ForValue,
}

/// Specifies how the [`Builder`] should bind the syntax tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct BindOption {
    /// This field is used to avoid redundant [`CompilerAlloca`] generated by the compiler.
    /// [`Builder`] will use the given [`AllocaID`] specified in this field to store the result
    /// that need temporary storage.
    pub existing_alloca: Option<AllocaID>,

    /// Specifies the [`BindTarget`] of the binding process.
    pub bind_target: BindTarget,
}

#[derive(Debug)]
pub struct Builder {
    hir: Hir,
    current_basic_block: BasicBlockID,
    errors: Vec<SemanticError>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum BindingError {
    FatalSemanticError,
    TargetNotApplicable,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum BindingResult {
    Value(SsaValue),
    Address(Address),
}

impl Inspectable<BindingResult> for Hir {
    fn get_value_type_id(&self, value: &BindingResult) -> ValueTypeID {
        match value {
            BindingResult::Value(value) => self.get_value_type_id(value),
            BindingResult::Address(address) => self.get_value_type_id(address),
        }
    }

    fn get_reachability(&self, value: &BindingResult) -> Reachability {
        match value {
            BindingResult::Value(value) => self.get_reachability(value),
            BindingResult::Address(address) => self.get_reachability(address),
        }
    }

    fn get_span(&self, value: &BindingResult) -> Span {
        match value {
            BindingResult::Value(value) => self.get_span(value),
            BindingResult::Address(address) => self.get_span(address),
        }
    }
}

impl Builder {
    /// Creates a new [`Builder`].
    #[must_use]
    pub fn new(table: Arc<Table>, overload_id: OverloadID) -> Self {
        let hir = Hir::new(table, overload_id);
        let current_block = hir.control_flow_graph.entry_block();
        Self {
            hir,
            current_basic_block: current_block,
            errors: Vec::new(),
        }
    }
}

impl Builder {
    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn bind_expression(
        &mut self,
        syntax_tree: &ExpressionSyntaxTree,
        bind_option: BindOption,
    ) -> Result<BindingResult, BindingError> {
        match syntax_tree {
            ExpressionSyntaxTree::Functional(syntax_tree) => {
                self.bind_functional(syntax_tree, bind_option)
            }
            ExpressionSyntaxTree::Imperative(syntax_tree) => {
                self.bind_imperative(syntax_tree, bind_option)
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn bind_functional(
        &mut self,
        syntax_tree: &FunctionalSyntaxTree,
        bind_option: BindOption,
    ) -> Result<BindingResult, BindingError> {
        match syntax_tree {
            FunctionalSyntaxTree::NumericLiteral(syntax_tree) => {
                self.bind_numeric_literal(syntax_tree, bind_option)
            }
            FunctionalSyntaxTree::BooleanLiteral(syntax_tree) => {
                Self::bind_boolean_literal(syntax_tree, bind_option)
            }
            FunctionalSyntaxTree::FunctionCall(syntax_tree) => {
                self.bind_function_call(syntax_tree, bind_option)
            }
            FunctionalSyntaxTree::Binary(_) => todo!(),
            FunctionalSyntaxTree::Prefix(syntax_tree) => self.bind_prefix(syntax_tree, bind_option),
            FunctionalSyntaxTree::Named(_) => todo!(),
            FunctionalSyntaxTree::Parenthesized(syntax_tree) => {
                self.bind_expression(syntax_tree.expression(), bind_option)
            }
            FunctionalSyntaxTree::StructLiteral(_) => todo!(),
            FunctionalSyntaxTree::MemberAccess(_) => todo!(),
            FunctionalSyntaxTree::Continue(_) => todo!(),
            FunctionalSyntaxTree::Break(_) => todo!(),
            FunctionalSyntaxTree::Return(_) => todo!(),
            FunctionalSyntaxTree::Express(_) => todo!(),
            FunctionalSyntaxTree::Cast(_) => todo!(),
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn bind_imperative(
        &mut self,
        syntax_tree: &ImperativeSyntaxTree,
        bind_option: BindOption,
    ) -> Result<BindingResult, BindingError> {
        match syntax_tree {
            ImperativeSyntaxTree::Block(_) => todo!(),
            ImperativeSyntaxTree::IfElse(_) => todo!(),
            ImperativeSyntaxTree::Loop(_) => todo!(),
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn bind_prefix(
        &mut self,
        syntax_tree: &PrefixSyntaxTree,
        bind_option: BindOption,
    ) -> Result<BindingResult, BindingError> {
        let result = match syntax_tree.prefix_operator() {
            PrefixOperator::LogicalNot(..) => {
                let ssa_value = self
                    .expect_expression(
                        syntax_tree.operand(),
                        BindOption::default(),
                        Type::PrimitiveType(PrimitiveType::Bool),
                    )?
                    .into_value()
                    .unwrap();

                Prefix {
                    prefix_operator: syntax_tree.prefix_operator().clone(),
                    operand: ssa_value,
                    span: syntax_tree.span(),
                }
            }

            PrefixOperator::Negate(..) => {
                let ssa_value = self
                    .bind_expression(syntax_tree.operand(), BindOption::default())?
                    .into_value()
                    .unwrap();

                let ssa_value = self
                    .check_value(ssa_value.into(), Constraint::Signed.into())?
                    .into_value()
                    .unwrap();

                Prefix {
                    prefix_operator: syntax_tree.prefix_operator().clone(),
                    operand: ssa_value,
                    span: syntax_tree.span(),
                }
            }
        };

        if bind_option.bind_target != BindTarget::ForValue {
            return Err(BindingError::TargetNotApplicable);
        }

        let id = self.new_ssa(result.into());

        Ok(BindingResult::Value(SsaValue::SsaVariableID(id)))
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn bind_boolean_literal(
        syntax_tree: &BooleanLiteralSyntaxTree,
        bind_option: BindOption,
    ) -> Result<BindingResult, BindingError> {
        // can't bind to address
        if bind_option.bind_target == BindTarget::ForAddress {
            return Err(BindingError::TargetNotApplicable);
        }

        let value = match syntax_tree {
            BooleanLiteralSyntaxTree::True(..) => true,
            BooleanLiteralSyntaxTree::False(..) => false,
        };

        Ok(BindingResult::Value(SsaValue::Literal(
            Literal::BooleanLiteral(BooleanLiteral {
                value,
                span: syntax_tree.span(),
            }),
        )))
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn bind_numeric_literal(
        &mut self,
        syntax_tree: &NumericLiteralToken,
        bind_option: BindOption,
    ) -> Result<BindingResult, BindingError> {
        // determine the type of the literal
        let inferable_type_id = if let Some(suffix) = &syntax_tree.suffix_span {
            // the literal type is specified, so we don't need to infer the type

            let primitive_type = match suffix.str() {
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
                    self.errors.push(
                        InvalidNumericLiteralSuffix {
                            suffix_span: suffix.clone(),
                        }
                        .into(),
                    );
                    return Err(BindingError::FatalSemanticError);
                }
            };

            let primitive_type_is_integral = matches!(
                primitive_type,
                PrimitiveType::Int8
                    | PrimitiveType::Int16
                    | PrimitiveType::Int32
                    | PrimitiveType::Int64
                    | PrimitiveType::Uint8
                    | PrimitiveType::Uint16
                    | PrimitiveType::Uint32
                    | PrimitiveType::Uint64
            );

            let has_dot = syntax_tree.value_span.str().contains('.');

            if primitive_type_is_integral && has_dot {
                self.errors.push(
                    FloatingPointLiteralHasIntegralSuffix {
                        floating_point_span: syntax_tree.span.clone(),
                    }
                    .into(),
                );
                return Err(BindingError::FatalSemanticError);
            }

            // we check the bind_target here because we want to generate as much errors as possible
            // before returning
            if bind_option.bind_target == BindTarget::ForAddress {
                return Err(BindingError::TargetNotApplicable);
            }

            ValueTypeID::Type(Type::PrimitiveType(primitive_type))
        } else {
            // the literal type is not specified, so we need to infer the type

            // can't bind to address
            if bind_option.bind_target == BindTarget::ForAddress {
                return Err(BindingError::TargetNotApplicable);
            }

            let has_dot = syntax_tree.value_span.str().contains('.');

            if has_dot {
                ValueTypeID::Inferring(self.hir.inference_context.add_inference(Constraint::Float))
            } else {
                ValueTypeID::Inferring(self.hir.inference_context.add_inference(Constraint::Number))
            }
        };

        // create the literal
        Ok(BindingResult::Value(SsaValue::Literal(
            Literal::NumericLiteral(NumericLiteral {
                numeric_literal_token: syntax_tree.clone(),
                value_type_id: inferable_type_id,
            }),
        )))
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn handle_resolve_symbol_result(
        &mut self,
        result: Result<OkWithErr<GlobalID, SymbolError>, Vec<SymbolError>>,
    ) -> Option<GlobalID> {
        match result {
            Ok(ok) => {
                self.errors.extend(ok.errors.into_iter().map(Into::into));
                Some(ok.value)
            }
            Err(errors) => {
                self.errors.extend(errors.into_iter().map(Into::into));
                None
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn filter_overload_candidate(
        &self,
        overload_candidates: &mut Vec<OverloadID>,
        filter: impl Fn(&Overload) -> bool,
    ) {
        let mut index = 0;
        while index < overload_candidates.len() {
            let overload_id = overload_candidates[index];
            let overload = &self.hir.table[overload_id];

            if filter(overload) {
                index += 1;
            } else {
                overload_candidates.remove(index);
            }
        }
    }

    fn create_candidates(
        &mut self,
        overload_set_id: OverloadSetID,
        syntax_tree: &FunctionCallSyntaxTree,
    ) -> Option<Vec<OverloadID>> {
        let overload_set = &self.hir.table[overload_set_id];
        let mut overload_candidates = overload_set.overloads().clone();

        // filter out the overloads that are not accessible
        self.filter_overload_candidate(&mut overload_candidates, |overload| {
            self.hir.table.symbol_accessible(
                self.hir.parent_module_id.into(),
                overload_set_id.into(),
                overload.accessibility(),
            )
        });

        if overload_candidates.is_empty() {
            self.errors.push(
                NoAccessibleOverload {
                    overload_set_id,
                    symbol_span: syntax_tree.qualified_identifier().span(),
                }
                .into(),
            );
            return None;
        }

        Some(overload_candidates)
    }

    fn handle_overload_candidates(
        &mut self,
        arguments: Vec<SsaValue>,
        overload_candidates: Vec<OverloadID>,
        bind_option: BindOption,
        syntax_tree: &FunctionCallSyntaxTree,
        overload_set_id: OverloadSetID,
    ) -> Result<BindingResult, BindingError> {
        // must be exactly one overload
        match overload_candidates.len() {
            // no overload matches
            0 => {
                self.errors.push(
                    NoOverloadWithMatchingArgumentTypes {
                        overload_set_id,
                        symbol_span: syntax_tree.qualified_identifier().span(),
                    }
                    .into(),
                );

                Err(BindingError::FatalSemanticError)
            }
            // found the overload
            1 => {
                // unify the argument types with the parameter types
                for (argument, parameter) in arguments
                    .iter()
                    .zip(self.hir.table[overload_candidates[0]].parameter_order())
                    .map(|(argument, parameter)| (argument, &self.hir.table[*parameter]))
                {
                    let inference_id = if self
                        .hir
                        .get_bound_type(argument)
                        .inferable_type
                        .as_constraint()
                        .is_some()
                    {
                        *self.hir.get_value_type_id(argument).as_inferring().unwrap()
                    } else {
                        continue;
                    };

                    self.hir
                        .inference_context
                        .unify_with_concrete(inference_id, parameter.type_binding().ty)
                        .expect("unification failed");
                }

                let function_call = FunctionCall {
                    span: syntax_tree.span(),
                    overload_id: overload_candidates[0],
                    arguments,
                };

                let id = self.new_ssa(function_call.into());

                if bind_option.bind_target != BindTarget::ForValue {
                    return Err(BindingError::TargetNotApplicable);
                }

                Ok(BindingResult::Value(SsaValue::SsaVariableID(id)))
            }
            // ambiguous function call
            _ => {
                self.errors.push(
                    AmbiguousFunctionCall {
                        candidate_overloads: overload_candidates,
                        function_call_span: syntax_tree.span(),
                    }
                    .into(),
                );

                Err(BindingError::FatalSemanticError)
            }
        }
    }

    fn bind_function_call(
        &mut self,
        syntax_tree: &FunctionCallSyntaxTree,
        bind_option: BindOption,
    ) -> Result<BindingResult, BindingError> {
        // resolve the symbol
        let symbol = self.hir.table.resolve_symbol(
            syntax_tree.qualified_identifier(),
            self.hir.parent_module_id.into(),
        );

        let mut arguments = Vec::new();

        // binds argument
        for argument in syntax_tree
            .arguments()
            .iter()
            .flat_map(ConnectedList::elements)
        {
            let argument = self.bind_expression(argument, BindOption::default());
            arguments.push(argument);
        }

        // unwrap the symbol
        let symbol = self
            .handle_resolve_symbol_result(symbol)
            .ok_or(BindingError::FatalSemanticError)?;

        let GlobalID::OverloadSet(overload_set_id) = symbol else {
            self.errors.push(
                SymbolNotCallable {
                    found_id: symbol,
                    symbol_span: syntax_tree.span()
                }
                .into()
            );
            return Err(BindingError::FatalSemanticError);
        };

        // create accessible overload candidates
        let mut overload_candidates = self
            .create_candidates(overload_set_id, syntax_tree)
            .ok_or(BindingError::FatalSemanticError)?;

        // get the overload that matches the number of arguments
        self.filter_overload_candidate(&mut overload_candidates, |overload| {
            overload.parameter_order().len() == arguments.len()
        });
        if overload_candidates.is_empty() {
            self.errors.push(
                NoOverloadWithMatchingNumberOfArguments {
                    overload_set_id,
                    argument_count: arguments.len(),
                    symbol_span: syntax_tree.qualified_identifier().span(),
                }
                .into(),
            );
            return Err(BindingError::FatalSemanticError);
        }

        // unwrap the result first
        let arguments = arguments
            .into_iter()
            .map(|x| {
                x.map_err(|_| BindingError::FatalSemanticError)
                    .map(|x| x.into_value().unwrap())
            })
            .collect::<Result<Vec<_>, _>>()?;

        // get the overload that matches the argument types
        self.filter_overload_candidate(&mut overload_candidates, |overload| {
            for (parameter, argument) in overload
                .parameter_order()
                .iter()
                .map(|x| &self.hir.table[*x])
                .zip(arguments.iter())
            {
                let bound_type = self.hir.get_bound_type(argument);
                let ty_match = match bound_type.inferable_type {
                    InferableType::Type(ty) => ty == parameter.type_binding().ty,
                    InferableType::Constraint(constraint) => {
                        constraint.satisfies(parameter.type_binding().ty)
                    }
                };

                if !ty_match {
                    return false;
                }
            }

            true
        });

        self.handle_overload_candidates(
            arguments,
            overload_candidates,
            bind_option,
            syntax_tree,
            overload_set_id,
        )
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn check_value(
        &mut self,
        value: BindingResult,
        inferable_type: InferableType,
    ) -> Result<BindingResult, BindingError> {
        fn handle_unification_error(expression_span: Span, err: UnificationError) -> SemanticError {
            let (left, right) = match err {
                UnificationError::TypeMismatchError(TypeMismatchError { left, right }) => {
                    (left.into(), right.into())
                }
                UnificationError::ConstraintNotSatisfiedError(ConstraintNotSatisfiedError {
                    constraint,
                    concrete_type,
                }) => (constraint.into(), concrete_type.into()),
            };

            TypeMismatch {
                expression_span,
                found: left,
                expected: right,
            }
            .into()
        }

        let err = match (self.hir.get_value_type_id(&value), inferable_type) {
            (ValueTypeID::Inferring(found), InferableType::Type(expected)) => {
                let Err(err) = self
                    .hir
                    .inference_context
                    .unify_with_concrete(found, expected) else {
                    return Ok(value);
                };
                err
            }
            (ValueTypeID::Inferring(found), InferableType::Constraint(expected)) => {
                let Err(err) = self
                    .hir
                    .inference_context
                    .add_constraint(found, expected) else {
                    return Ok(value);
                };
                err.into()
            }
            (ValueTypeID::Type(found), InferableType::Constraint(expected)) => {
                if expected.satisfies(found) {
                    return Ok(value);
                }

                self.errors.push(
                    TypeMismatch {
                        expression_span: self.hir.get_span(&value),
                        found: found.into(),
                        expected: expected.into(),
                    }
                    .into(),
                );

                return Err(BindingError::FatalSemanticError);
            }
            (ValueTypeID::Type(found), InferableType::Type(expected)) => {
                if found == expected {
                    return Ok(value);
                }

                self.errors.push(
                    TypeMismatch {
                        expression_span: self.hir.get_span(&value),
                        found: found.into(),
                        expected: expected.into(),
                    }
                    .into(),
                );
                return Err(BindingError::FatalSemanticError);
            }
        };

        self.errors
            .push(handle_unification_error(self.hir.get_span(&value), err));

        Err(BindingError::FatalSemanticError)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn expect_expression(
        &mut self,
        syntax_tree: &ExpressionSyntaxTree,
        bind_option: BindOption,
        expected_type: Type,
    ) -> Result<BindingResult, BindingError> {
        let value = self.bind_expression(syntax_tree, bind_option)?;
        self.check_value(value, expected_type.into())
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn new_ssa(&mut self, binding: Binding) -> SsaVariableID {
        let ssa = SsaVariable::new(SsaVariableData { binding });
        let id = ssa.id();
        self.hir.variables_by_id.insert(id, ssa);

        self.hir.control_flow_graph[self.current_basic_block]
            .add_basic_instruction(Basic::SsaVariableAssignment(id));

        id
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
}

#[cfg(test)]
mod tests;
