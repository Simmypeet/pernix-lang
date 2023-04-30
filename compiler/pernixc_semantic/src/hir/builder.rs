//! Contains the definition of [`Builder`] -- the main interface for building the HIR.

use std::sync::Arc;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_source::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{
    expression::{
        BooleanLiteral as BooleanLiteralSyntaxTree, Expression as ExpressionSyntaxTree,
        FunctionCall as FunctionCallSyntaxTree, Functional as FunctionalSyntaxTree,
        Imperative as ImperativeSyntaxTree, NumericLiteral as NumericLiteralSyntaxTree,
    },
    ConnectedList,
};
use pernixc_system::arena::InvalidIDError;
use thiserror::Error;

use super::{
    error::{
        FloatingPointLiteralHasIntegralSuffix, HirError, InvalidNumericLiteralSuffix,
        NoAccessibleOverload, NoOverloadWithMatchingArgumentTypes,
        NoOverloadWithMatchingNumberOfArguments, SymbolNotCallable,
    },
    instruction::Backend,
    value::{Address, BooleanLiteral, Constant, NumericLiteral, PlaceHolder, Value},
    BindingErrorHandler, Hir, TypeSystem,
};
use crate::{
    cfg::ControlFlowGraph,
    infer::{Constraint, InferableType, InferenceContext, InferenceID},
    symbol::{
        table::Table,
        ty::{PrimitiveType, Type},
        GlobalID, ModuleID, Overload, OverloadID, OverloadSetID,
    },
};

/// Is an enumeration flag that specifies how the builder should bind the syntax tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum BindingTarget {
    /// Binds the syntax tree for a value.
    ///
    /// This is the default binding target.
    #[default]
    ForValue,

    /// Binds the syntax tree for the underlying address of the value.
    ///
    /// This is used for obtaining the address of r-values.
    ForAddress,

    /// Specifes that the expression is bound at a statement level.
    ///
    /// This is useful for avoiding allocating a register for some expressions.
    ForStatement,
}

/// Is a data passed to every `bind_*` method that specifies how the binding should be done.
///
/// This struct is just a request to the builder, and the builder may ignore it if it is not
/// applicable. The caller should check for the result of the binding and see if it satisfies the
/// needs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct BindingOption {
    /// Specifies the binding target.
    bind_kind: BindingTarget,
}

/// Is a [`TypeSystem`] used for building the [`Hir`].
///
/// While building the [`Hir`], the type of the value might not be known right away. Therefore, the
/// builder uses this [`TypeID`] to represent the type of the value that might be inferred later.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum TypeID {
    /// The type might be inferred later by the inference context.
    InferenceID(InferenceID),

    /// The type is determined right away at the time of the creation of the value.
    Type(Type),
}

impl TypeSystem for TypeID {
    fn from_type(ty: Type) -> Self {
        ty.into()
    }
}

/// Is a builder that builds the [`Hir`] by inputting the various
/// [`StatementSyntaxTree`](pernixc_syntax::syntax_tree::statement::Statement) to it.
#[derive(Debug, Getters)]
pub struct Builder {
    control_flow_graph: ControlFlowGraph<Backend<TypeID>>,
    /// Gets the [`InferenceContext`] used by the builder.
    #[get = "pub"]
    inference_context: InferenceContext,

    /// Gets the [`Table`] that was used for symbol resolution and variable lookup.
    #[get = "pub"]
    table: Arc<Table>,
    overload_id: OverloadID,
    overload_set_id: OverloadSetID,
    parent_module_id: ModuleID,
}

impl Builder {
    /// Creates a new [`Builder`].
    ///
    /// # Parameters
    /// - `table`: The [`Table`] that will be used for symbol resolution and various lookups.
    /// - `overload_id`: The context in which the [`Builder`] will be building the [`Hir`].
    ///
    /// # Errors
    /// - [`InvalidIDError`] if the `overload_id` is invalid for the `table`.
    pub fn new(table: Arc<Table>, overload_id: OverloadID) -> Result<Self, InvalidIDError> {
        let overload_set_id = table.get_overload(overload_id)?.parent_overload_set_id();
        let parent_module_id = table.get_overload_set(overload_set_id)?.parent_module_id();

        Ok(Self {
            control_flow_graph: ControlFlowGraph::new(),
            inference_context: InferenceContext::new(),
            table,
            overload_id,
            overload_set_id,
            parent_module_id,
        })
    }

    /// Finishes the building process and returns the [`Hir`].
    pub fn build(self, handler: &impl BindingErrorHandler) -> Hir {
        todo!()
    }
}

/// Is an error occurred during the binding process.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Error)]
#[error("Encountered a fatal semantic error that cause the binding process to terminate.")]
pub struct BindingError(Span);

/// Is the result of various `bind_*` functions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum BindingResult {
    /// The binding process returns a value.
    Value(Value<TypeID>),

    /// The binding process returns an address to a value.
    Address(Address),

    /// The binding process doesn't return anything but does update the control flow graph.
    None,
}

impl Builder {
    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`ExpressionSyntaxTree`] and returns the [`BindingResult`].
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_expression(
        &mut self,
        syntax_tree: &ExpressionSyntaxTree,
        binding_option: &BindingOption,
        handler: &impl BindingErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        match syntax_tree {
            ExpressionSyntaxTree::Functional(syn) => {
                self.bind_functional(syn, binding_option, handler)
            }
            ExpressionSyntaxTree::Imperative(syn) => {
                self.bind_imperative(syn, binding_option, handler)
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`ImperativeSyntaxTree`] and returns the [`BindingResult`].
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_functional(
        &mut self,
        syntax_tree: &FunctionalSyntaxTree,
        binding_option: &BindingOption,
        handler: &impl BindingErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        match syntax_tree {
            FunctionalSyntaxTree::NumericLiteral(syn) => self
                .bind_numeric_literal(syn, handler)
                .map(|f| BindingResult::Value(Value::Constant(Constant::NumericLiteral(f)))),
            FunctionalSyntaxTree::BooleanLiteral(syn) => Ok(BindingResult::Value(Value::Constant(
                Constant::BooleanLiteral(Self::bind_boolean_literal(syn)),
            ))),
            FunctionalSyntaxTree::Binary(_) => todo!(),
            FunctionalSyntaxTree::Prefix(_) => todo!(),
            FunctionalSyntaxTree::Named(_) => todo!(),
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

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`ImperativeSyntaxTree`] and returns the [`BindingResulft`].
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_imperative(
        &mut self,
        syntax_tree: &ImperativeSyntaxTree,
        binding_option: &BindingOption,
        handler: &impl BindingErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        todo!()
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`NumericLiteralSyntaxTree`] and returns the [`NumericLiteral`].
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_numeric_literal(
        &mut self,
        syntax_tree: &NumericLiteralSyntaxTree,
        handler: &impl BindingErrorHandler,
    ) -> Result<NumericLiteral<TypeID>, BindingError> {
        // determine the type of the literal
        let type_id = if let Some(suffix) = &syntax_tree.numeric_literal_token().suffix_span {
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
                    handler.recieve(HirError::InvalidNumericLiteralSuffix(
                        InvalidNumericLiteralSuffix {
                            suffix_span: suffix.clone(),
                        },
                    ));
                    return Err(BindingError(syntax_tree.span()));
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

            let has_dot = syntax_tree
                .numeric_literal_token()
                .value_span
                .str()
                .contains('.');

            if primitive_type_is_integral && has_dot {
                handler.recieve(HirError::FloatingPointLiteralHasIntegralSuffix(
                    FloatingPointLiteralHasIntegralSuffix {
                        floating_point_span: syntax_tree.numeric_literal_token().span.clone(),
                    },
                ));
                return Err(BindingError(syntax_tree.span()));
            }

            TypeID::Type(Type::PrimitiveType(primitive_type))
        } else {
            // the literal type is not specified, so we need to infer the type
            let has_dot = syntax_tree
                .numeric_literal_token()
                .value_span
                .str()
                .contains('.');

            if has_dot {
                TypeID::InferenceID(self.inference_context.new_inference(Constraint::Float))
            } else {
                TypeID::InferenceID(self.inference_context.new_inference(Constraint::Number))
            }
        };

        // create the literal
        Ok(NumericLiteral {
            numeric_literal_syntax_tree: syntax_tree.clone(),
            ty: type_id,
        })
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the give [`BooleanLiteralSyntaxTree`] to a [`BooleanLiteral`].
    #[must_use]
    pub fn bind_boolean_literal(syntax_tree: &BooleanLiteralSyntaxTree) -> BooleanLiteral {
        let value = match syntax_tree {
            BooleanLiteralSyntaxTree::True(..) => true,
            BooleanLiteralSyntaxTree::False(..) => false,
        };

        BooleanLiteral {
            value,
            span: syntax_tree.span(),
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    fn filter_overload_candidate(
        &self,
        overload_candidates: &mut Vec<OverloadID>,
        filter: impl Fn(&Overload) -> bool,
    ) {
        let mut index = 0;
        while index < overload_candidates.len() {
            let overload_id = overload_candidates[index];
            let overload = self.table.get_overload(overload_id).unwrap();

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
        handler: &impl BindingErrorHandler,
    ) -> Result<Vec<OverloadID>, BindingError> {
        let overload_set = self.table.get_overload_set(overload_set_id).unwrap();
        let mut overload_candidates = overload_set.overloads().clone();

        // filter out the overloads that are not accessible
        self.filter_overload_candidate(&mut overload_candidates, |overload| {
            self.table
                .symbol_accessible(
                    self.parent_module_id.into(),
                    overload_set_id.into(),
                    overload.accessibility(),
                )
                .unwrap()
        });

        if overload_candidates.is_empty() {
            handler.recieve(HirError::NoAccessibleOverload(NoAccessibleOverload {
                overload_set_id,
                symbol_span: syntax_tree.qualified_identifier().span(),
            }));
            return Err(BindingError(syntax_tree.span()));
        }

        Ok(overload_candidates)
    }

    fn handle_overload_candidates(
        &mut self,
        arguments: Vec<Value<TypeID>>,
        overload_candidates: Vec<OverloadID>,
        has_placeholders: bool,
        syntax_tree: &FunctionCallSyntaxTree,
        overload_set_id: OverloadSetID,
        handler: &impl BindingErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        // must be exactly one overload
        match overload_candidates.len() {
            // no overload matches
            0 => {
                handler.recieve(HirError::NoOverloadWithMatchingArgumentTypes(
                    NoOverloadWithMatchingArgumentTypes {
                        overload_set_id,
                        symbol_span: syntax_tree.qualified_identifier().span(),
                    },
                ));

                Err(BindingError(syntax_tree.span()))
            }
            // found the overload
            1 => {
                // unify the argument types with the parameter types
                for (argument, parameter) in arguments
                    .iter()
                    .zip(
                        self.table
                            .get_overload(overload_candidates[0])
                            .unwrap()
                            .parameter_order(),
                    )
                    .map(|(argument, parameter)| {
                        (argument, self.table.get_parameter(*parameter).unwrap())
                    })
                {
                    let inference_id = if self
                        .get_type_binding(argument)
                        .unwrap()
                        .ty
                        .as_constraint()
                        .is_some()
                    {
                        *self.hir.get_value_type_id(argument).as_inferring().unwrap()
                    } else {
                        continue;
                    };

                    self.inference_context
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

    /// Binds the given [`FunctionCallSyntaxTree`] to a [`SsaValue`].
    pub fn bind_function_call(
        &mut self,
        syntax_tree: &FunctionCallSyntaxTree,
        handler: &impl BindingErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        // resolve the symbol
        let symbol = self.table.resolve_symbol(
            self.parent_module_id.into(),
            syntax_tree.qualified_identifier(),
            handler,
        );

        let mut arguments = Vec::new();

        // binds argument
        for argument in syntax_tree
            .arguments()
            .iter()
            .flat_map(ConnectedList::elements)
        {
            let argument = self.bind_expression(argument, &BindingOption::default(), handler);
            arguments.push(argument);
        }

        // unwrap the symbol
        let symbol = symbol.map_err(|_| BindingError(syntax_tree.span()))?;

        let GlobalID::OverloadSet(overload_set_id) = symbol else {
            handler.recieve(
                HirError::SymbolNotCallable(SymbolNotCallable {
                    found_id: symbol,
                    symbol_span: syntax_tree.span()
                })
            );
            return Err(BindingError(syntax_tree.span()));
        };

        // create accessible overload candidates
        let mut overload_candidates =
            self.create_candidates(overload_set_id, syntax_tree, handler)?;

        // get the overload that matches the number of arguments
        self.filter_overload_candidate(&mut overload_candidates, |overload| {
            overload.parameter_order().len() == arguments.len()
        });
        if overload_candidates.is_empty() {
            handler.recieve(HirError::NoOverloadWithMatchingNumberOfArguments(
                NoOverloadWithMatchingNumberOfArguments {
                    overload_set_id,
                    argument_count: arguments.len(),
                    symbol_span: syntax_tree.qualified_identifier().span(),
                },
            ));
            return Err(BindingError(syntax_tree.span()));
        }

        // unwrap the result first
        let mut has_placeholders = false;
        let arguments = arguments
            .into_iter()
            .map(|x| {
                // every expression can be bound as value if requested
                x.unwrap_or_else(|err| {
                    has_placeholders = true;
                    BindingResult::Value(Value::PlaceHolder(PlaceHolder {
                        span: err.0,
                        ty: TypeID::InferenceID(
                            self.inference_context.new_inference(Constraint::All),
                        ),
                    }))
                })
                .into_value()
                .unwrap()
            })
            .collect::<Vec<_>>();

        // get the overload that matches the argument types
        self.filter_overload_candidate(&mut overload_candidates, |overload| {
            for (parameter, argument) in overload
                .parameter_order()
                .iter()
                .map(|x| self.table.get_parameter(*x).unwrap())
                .zip(arguments.iter())
            {
                let bound_type = self.get_type_binding(argument).unwrap();
                let ty_match = match bound_type.ty {
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
            syntax_tree,
            overload_set_id,
        )
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////
}
