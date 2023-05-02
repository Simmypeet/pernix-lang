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
        Imperative as ImperativeSyntaxTree, Named as NamedSyntaxTree,
        NumericLiteral as NumericLiteralSyntaxTree, Prefix as PrefixSyntaxTree, PrefixOperator,
    },
    statement::{
        Declarative, Expressive, Statement as StatementSyntaxTree,
        VariableDeclaration as VariableDeclarationSyntaxTree,
    },
    ConnectedList,
};
use pernixc_system::arena::InvalidIDError;
use thiserror::Error;

use self::scope::Stack;
use super::{
    error::{
        AmbiguousFunctionCall, Error, FloatingPointLiteralHasIntegralSuffix,
        InvalidNumericLiteralSuffix, NoAccessibleOverload, NoOverloadWithMatchingArgumentTypes,
        NoOverloadWithMatchingNumberOfArguments, SymbolNotCallable, ValueExpected,
    },
    instruction::RegisterAssignment,
    value::{
        binding::{FunctionCall, Prefix},
        Address, BooleanLiteral, Constant, EnumLiteral, NumericLiteral, Placeholder, Value,
    },
    Container, ErrorHandler, InvalidValueError, Register, RegisterID, TypeBinding, TypeSystem,
    ValueInspect,
};
use crate::{
    cfg::BasicBlockID,
    hir::{
        error::TypeMismatch,
        instruction::{Store, VariableDeclaration},
        value::binding::{LoadType, NamedLoad},
        Alloca,
    },
    infer::{
        Constraint, ConstraintNotSatisfiedError, InferableType, InferenceContext, InferenceID,
        TypeMismatchError, UnificationError,
    },
    symbol::{
        table::Table,
        ty::{PrimitiveType, Type},
        GlobalID, Overload, OverloadID, OverloadSetID,
    },
};

pub mod scope;

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
    binding_target: BindingTarget,
}

/// Is a [`TypeSystem`] used for building the [`Hir`].
///
/// While building the [`Hir`], the type of the value might not be known right away. Therefore, the
/// builder uses this [`IntermediateTypeID`] to represent the type of the value that might be
/// inferred later.
///
/// ## Misonceptions
///
/// This doesn't represent the final type of the value. Therefore, do not use this to check for
/// type equality. Instead, retrieve the [`TypeBinding<InferableType>`] from [`Builder`] then first
/// check for type equality.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum IntermediateTypeID {
    /// The type might be inferred later by the inference context.
    InferenceID(InferenceID),

    /// The type is determined right away at the time of the creation of the value.
    Type(Type),
}

impl TypeSystem for IntermediateTypeID {
    fn from_type(ty: Type) -> Self { ty.into() }
}

/// Is a builder that builds the [`Hir`] by inputting the various
/// [`StatementSyntaxTree`](pernixc_syntax::syntax_tree::statement::Statement) to it.
#[derive(Debug, Getters)]
pub struct Builder {
    container: Container<IntermediateTypeID>,
    current_block: BasicBlockID,
    inference_context: InferenceContext,
    stack: Stack,
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
        let container = Container::new(table, overload_id)?;

        Ok(Self {
            current_block: container.control_flow_graph.entry_block(),
            container,
            inference_context: InferenceContext::new(),
            stack: Stack::new(),
        })
    }

    /// Obtains [`TypeBinding`] for the given value.
    ///
    /// # Errors
    /// - If the given value wasn't created from this [`Builder`].
    pub fn get_type_binding<T>(
        &self,
        value: &T,
    ) -> Result<TypeBinding<InferableType>, InvalidValueError>
    where
        Container<IntermediateTypeID>: ValueInspect<IntermediateTypeID, T>,
    {
        let intermediate_ty_id = self.get_intermediate_type_id(value)?;
        let reachability = self.container.get_reachability(value)?;
        let ty = match intermediate_ty_id {
            IntermediateTypeID::InferenceID(inference_id) => self
                .inference_context
                .get_inferable_type(inference_id)
                .map_err(|_| InvalidValueError)?,
            IntermediateTypeID::Type(ty) => InferableType::Type(ty),
        };

        Ok(TypeBinding { ty, reachability })
    }

    /// Obtains [`IntermediateTypeID`] for the given value.
    ///
    /// This [`IntermediateTypeID`] doesn't represent the final type of the value. Therefore, do
    /// not use this method for type checking.
    ///
    /// # Errors
    /// - If the given value wasn't created from this [`Builder`].
    pub fn get_intermediate_type_id<T>(
        &self,
        value: &T,
    ) -> Result<IntermediateTypeID, InvalidValueError>
    where
        Container<IntermediateTypeID>: ValueInspect<IntermediateTypeID, T>,
    {
        self.container.get_type(value)
    }

    /// Obtains [`Span`] for the given value.
    ///
    /// # Errors
    /// - If the given value wasn't created from this [`Builder`].
    pub fn get_span<T>(&self, value: &T) -> Result<Span, InvalidValueError>
    where
        Container<IntermediateTypeID>: ValueInspect<IntermediateTypeID, T>,
    {
        self.container.get_span(value)
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
    Value(Value<IntermediateTypeID>),

    /// The binding process returns an address to a value.
    Address(Address),

    /// The binding process doesn't return anything but does update the control flow graph.
    None,
}

impl Builder {
    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds [`StatementSyntaxTree`] into an instruction that will be inserted into the control
    /// flow graph in this builder.
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_statement(
        &mut self,
        syntax_tree: &StatementSyntaxTree,
        handler: &impl ErrorHandler,
    ) -> Result<(), BindingError> {
        match syntax_tree {
            StatementSyntaxTree::Declarative(Declarative::VariableDeclaration(syntax_tree)) => {
                self.bind_variable_declaration_statement(syntax_tree, handler)
            }
            StatementSyntaxTree::Expressive(syntax_tree) => {
                let binding_option = BindingOption {
                    binding_target: BindingTarget::ForStatement,
                };
                match syntax_tree {
                    Expressive::Semi(syntax_tree) => self
                        .bind_functional(syntax_tree.expression(), binding_option, handler)
                        .map(|_| ()),
                    Expressive::Imperative(syntax_tree) => self
                        .bind_imperative(syntax_tree, binding_option, handler)
                        .map(|_| ()),
                }
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds [`VariableDeclarationSyntaxTree`] into an instruction that will be inserted into the
    /// control flow graph in this builder.
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_variable_declaration_statement(
        &mut self,
        syntax_tree: &VariableDeclarationSyntaxTree,
        handler: &impl ErrorHandler,
    ) -> Result<(), BindingError> {
        // gets the type of the variable
        let ty = if let Some(type_annotation) = syntax_tree.type_annotation() {
            // gets the type of the variable
            self.container
                .table
                .resolve_type(
                    self.container.parent_module_id.into(),
                    type_annotation.type_specifier(),
                    handler,
                )
                .map(InferableType::Type)
                .unwrap_or(InferableType::Constraint(Constraint::All))
        } else {
            InferableType::Constraint(Constraint::All)
        };

        // gets the value of the variable
        let value = self
            .expect_expression(
                syntax_tree.expression(),
                BindingOption::default(),
                ty,
                handler,
            )
            .map(|x| x.into_value().unwrap())
            .unwrap_or_else(|err| {
                Value::Placeholder(Placeholder {
                    span: err.0,
                    ty: self.inference_context.new_inference(Constraint::All).into(),
                })
            });

        // gets the type of the variable
        let ty = self.get_intermediate_type_id(&value).unwrap();

        // inserts a new alloca
        let alloca_id = self.container.allocas.insert(Alloca {
            identifier_token: syntax_tree.identifier().clone(),
            is_mutable: syntax_tree.mutable_keyword().is_some(),
            ty,
        });

        self.container
            .control_flow_graph
            .get_mut(self.current_block)
            .unwrap()
            .add_basic_instruction(VariableDeclaration { alloca_id }.into());

        // inserts a new store
        self.container
            .control_flow_graph
            .get_mut(self.current_block)
            .unwrap()
            .add_basic_instruction(
                Store {
                    address: Address::AllocaID(alloca_id),
                    value,
                }
                .into(),
            );

        Ok(())
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////
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
        binding_option: BindingOption,
        handler: &impl ErrorHandler,
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
        binding_option: BindingOption,
        handler: &impl ErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        match syntax_tree {
            FunctionalSyntaxTree::NumericLiteral(syn) => self
                .bind_numeric_literal(syn, handler)
                .map(|f| BindingResult::Value(Value::Constant(Constant::NumericLiteral(f)))),
            FunctionalSyntaxTree::BooleanLiteral(syn) => Ok(BindingResult::Value(Value::Constant(
                Constant::BooleanLiteral(Self::bind_boolean_literal(syn)),
            ))),
            FunctionalSyntaxTree::Binary(_) => todo!(),
            FunctionalSyntaxTree::Prefix(syn) => self
                .bind_prefix(syn, handler)
                .map(|x| BindingResult::Value(Value::Register(x))),
            FunctionalSyntaxTree::Named(_) => todo!(),
            FunctionalSyntaxTree::FunctionCall(syn) => self
                .bind_function_call(syn, handler)
                .map(|x| BindingResult::Value(Value::Register(x))),
            FunctionalSyntaxTree::Parenthesized(syn) => {
                self.bind_expression(syn.expression(), binding_option, handler)
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

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`ImperativeSyntaxTree`] and returns the [`BindingResulft`].
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_imperative(
        &mut self,
        _syntax_tree: &ImperativeSyntaxTree,
        _binding_option: BindingOption,
        _handler: &impl ErrorHandler,
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
        handler: &impl ErrorHandler,
    ) -> Result<NumericLiteral<IntermediateTypeID>, BindingError> {
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
                    handler.recieve(Error::InvalidNumericLiteralSuffix(
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
                handler.recieve(Error::FloatingPointLiteralHasIntegralSuffix(
                    FloatingPointLiteralHasIntegralSuffix {
                        floating_point_span: syntax_tree.numeric_literal_token().span.clone(),
                    },
                ));
                return Err(BindingError(syntax_tree.span()));
            }

            IntermediateTypeID::Type(Type::PrimitiveType(primitive_type))
        } else {
            // the literal type is not specified, so we need to infer the type
            let has_dot = syntax_tree
                .numeric_literal_token()
                .value_span
                .str()
                .contains('.');

            if has_dot {
                IntermediateTypeID::InferenceID(
                    self.inference_context.new_inference(Constraint::Float),
                )
            } else {
                IntermediateTypeID::InferenceID(
                    self.inference_context.new_inference(Constraint::Number),
                )
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
            let overload = self.container.table.get_overload(overload_id).unwrap();

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
        handler: &impl ErrorHandler,
    ) -> Result<Vec<OverloadID>, BindingError> {
        let overload_set = self
            .container
            .table
            .get_overload_set(overload_set_id)
            .unwrap();
        let mut overload_candidates = overload_set.overloads().clone();

        // filter out the overloads that are not accessible
        self.filter_overload_candidate(&mut overload_candidates, |overload| {
            self.container
                .table
                .symbol_accessible(
                    self.container.parent_module_id.into(),
                    overload_set_id.into(),
                    overload.accessibility(),
                )
                .unwrap()
        });

        if overload_candidates.is_empty() {
            handler.recieve(Error::NoAccessibleOverload(NoAccessibleOverload {
                overload_set_id,
                symbol_span: syntax_tree.qualified_identifier().span(),
            }));
            return Err(BindingError(syntax_tree.span()));
        }

        Ok(overload_candidates)
    }

    fn handle_overload_candidates(
        &mut self,
        arguments: Vec<Value<IntermediateTypeID>>,
        overload_candidates: Vec<OverloadID>,
        has_placeholders: bool,
        syntax_tree: &FunctionCallSyntaxTree,
        overload_set_id: OverloadSetID,
        handler: &impl ErrorHandler,
    ) -> Result<RegisterID, BindingError> {
        // must be exactly one overload
        match overload_candidates.len() {
            // no overload matches
            0 => {
                handler.recieve(Error::NoOverloadWithMatchingArgumentTypes(
                    NoOverloadWithMatchingArgumentTypes {
                        overload_set_id,
                        symbol_span: syntax_tree.qualified_identifier().span(),
                        argument_types: arguments
                            .iter()
                            .map(|a| self.get_type_binding(a).unwrap().ty)
                            .collect(),
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
                        self.container
                            .table
                            .get_overload(overload_candidates[0])
                            .unwrap()
                            .parameter_order(),
                    )
                    .map(|(argument, parameter)| {
                        (
                            argument,
                            self.container.table.get_parameter(*parameter).unwrap(),
                        )
                    })
                {
                    let inference_id = if self
                        .get_type_binding(argument)
                        .unwrap()
                        .ty
                        .as_constraint()
                        .is_some()
                    {
                        self.container
                            .get_type(argument)
                            .unwrap()
                            .into_inference_id()
                            .unwrap()
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

                let id = self.container.registers.insert(Register {
                    binding: function_call.into(),
                });

                // create assignment
                self.container
                    .control_flow_graph
                    .get_mut(self.current_block)
                    .unwrap()
                    .add_basic_instruction(RegisterAssignment { register_id: id }.into());

                Ok(id)
            }
            // ambiguous function call
            _ => {
                // since placeholders can match multiple types, it might produce multiple candidates
                if !has_placeholders {
                    handler.recieve(Error::AmbiguousFunctionCall(AmbiguousFunctionCall {
                        candidate_overloads: overload_candidates,
                        function_call_span: syntax_tree.span(),
                        overload_set_id,
                    }));
                }

                Err(BindingError(syntax_tree.span()))
            }
        }
    }

    /// Binds the given [`FunctionCallSyntaxTree`] and returns the [`RegisterID`] where the result
    /// is stored.
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_function_call(
        &mut self,
        syntax_tree: &FunctionCallSyntaxTree,
        handler: &impl ErrorHandler,
    ) -> Result<RegisterID, BindingError> {
        // resolve the symbol
        let symbol = self.container.table.resolve_symbol(
            self.container.parent_module_id.into(),
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
            let argument = self.bind_expression(argument, BindingOption::default(), handler);
            arguments.push(argument);
        }

        // unwrap the symbol
        let symbol = symbol.map_err(|_| BindingError(syntax_tree.span()))?;

        let GlobalID::OverloadSet(overload_set_id) = symbol else {
            handler.recieve(
                Error::SymbolNotCallable(SymbolNotCallable {
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
            handler.recieve(Error::NoOverloadWithMatchingNumberOfArguments(
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
                    BindingResult::Value(Value::Placeholder(Placeholder {
                        span: err.0,
                        ty: IntermediateTypeID::InferenceID(
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
                .map(|x| self.container.table.get_parameter(*x).unwrap())
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
            has_placeholders,
            syntax_tree,
            overload_set_id,
            handler,
        )
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`PrefixSyntaxTree`] and returns the [`RegisterID`] where the result of the
    /// expression is stored.
    ///
    /// # Errors
    /// - If encounters a fatal semantic error.
    pub fn bind_prefix(
        &mut self,
        syntax_tree: &PrefixSyntaxTree,
        handler: &impl ErrorHandler,
    ) -> Result<RegisterID, BindingError> {
        // get the expected type of the operand based on the operator
        let expected_type = match syntax_tree.prefix_operator() {
            PrefixOperator::LogicalNot(..) => InferableType::Type(PrimitiveType::Bool.into()),
            PrefixOperator::Negate(..) => InferableType::Constraint(Constraint::Signed),
        };

        // bind the operand
        let operand = self
            .expect_expression(
                syntax_tree.operand(),
                BindingOption::default(),
                expected_type,
                handler,
            )
            // default to a placeholder if fails to bind the operand
            .unwrap_or(BindingResult::Value(Value::Placeholder(Placeholder {
                span: syntax_tree.span(),
                ty: match expected_type {
                    InferableType::Type(ty) => ty.into(),
                    InferableType::Constraint(constraint) => {
                        self.inference_context.new_inference(constraint).into()
                    }
                },
            })))
            .into_value()
            .unwrap();

        let binding = Prefix {
            span: syntax_tree.span(),
            prefix_operator: syntax_tree.prefix_operator().clone(),
            operand,
        }
        .into();

        let register_id = self.container.registers.insert(Register { binding });
        self.container
            .control_flow_graph
            .get_mut(self.current_block)
            .unwrap()
            .add_basic_instruction(RegisterAssignment { register_id }.into());

        Ok(register_id)
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the [`NamedSyntaxTree`] into a [`BindingResult`]
    ///
    /// # Errors
    /// - If encounters a fatal semantic error
    pub fn bind_named(
        &mut self,
        syntax_tree: &NamedSyntaxTree,
        binding_option: BindingOption,
        handler: &impl ErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        // check for locals
        if syntax_tree
            .qualified_identifier()
            .leading_separator()
            .is_none()
            && syntax_tree.qualified_identifier().identifiers().len() == 1
        {
            let local_ident = syntax_tree
                .qualified_identifier()
                .identifiers()
                .first()
                .span
                .str();

            // search for local variables then parameters
            let address = self
                .stack
                .serach(local_ident)
                .map(Address::AllocaID)
                .map_or_else(
                    || {
                        self.container
                            .table
                            .get_overload(self.container.overload_id)
                            .unwrap()
                            .parameter_ids_by_name()
                            .get(local_ident)
                            .copied()
                            .map(Address::ParameterID)
                    },
                    Some,
                );

            if let Some(address) = address {
                match binding_option.binding_target {
                    BindingTarget::ForValue => {
                        // load the value
                        let binding = NamedLoad {
                            span: syntax_tree.span(),
                            load_type: LoadType::Copy, // auto move will be applied later
                            address,
                        }
                        .into();

                        let register_id = self.container.registers.insert(Register { binding });
                        self.container
                            .control_flow_graph
                            .get_mut(self.current_block)
                            .unwrap()
                            .add_basic_instruction(RegisterAssignment { register_id }.into());

                        return Ok(BindingResult::Value(Value::Register(register_id)));
                    }
                    BindingTarget::ForAddress => return Ok(BindingResult::Address(address)),
                    BindingTarget::ForStatement => return Ok(BindingResult::None),
                }
            }
        }

        // search in gloal scope
        let symbol = self
            .container
            .table
            .resolve_symbol(
                self.container.parent_module_id.into(),
                syntax_tree.qualified_identifier(),
                handler,
            )
            .map_err(|_| BindingError(syntax_tree.span()))?;

        // only enum literal can be used as a value
        let GlobalID::EnumVariant(enum_variant_id) = symbol else {
            handler.recieve(Error::ValueExpected(ValueExpected {
                expression_span: syntax_tree.span(),
                found_symbol: symbol
            }));

            return Err(BindingError(syntax_tree.span()));
        };

        Ok(BindingResult::Value(Value::Constant(
            Constant::EnumLiteral(EnumLiteral {
                span: syntax_tree.span(),
                enum_variant_id,
            }),
        )))
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////
}

impl Builder {
    fn get_address_intermediate_type_id(&self, address: &Address) -> IntermediateTypeID {
        match address {
            Address::AllocaID(id) => self.container.allocas.get(*id).unwrap().ty,
            Address::ParameterID(id) => self
                .container
                .table
                .get_parameter(*id)
                .unwrap()
                .type_binding()
                .ty
                .into(),
            Address::FieldAddress(address) => IntermediateTypeID::Type(Type::TypedID(
                self.container
                    .table
                    .get_field(address.field_id())
                    .unwrap()
                    .parent_struct_id()
                    .into(),
            )),
        }
    }

    /// Similar to `bind_expression` but with additional type checking.
    fn expect_expression(
        &mut self,
        syntax_tree: &ExpressionSyntaxTree,
        binding_option: BindingOption,
        expect: InferableType,
        handler: &impl ErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        assert!(
            binding_option.binding_target != BindingTarget::ForStatement,
            "`BindingTarget::ForStatement` with type checking doesn't seem applicable since the \
             expression might not return anything when binding"
        );

        let value = self.bind_expression(syntax_tree, binding_option, handler)?;

        match &value {
            BindingResult::Value(value) => self.type_check(
                syntax_tree.span(),
                self.get_intermediate_type_id(value).unwrap(),
                expect,
                handler,
            )?,
            BindingResult::Address(address) => {
                self.type_check(
                    syntax_tree.span(),
                    self.get_address_intermediate_type_id(address),
                    expect,
                    handler,
                )?;
            }
            BindingResult::None => todo!(),
        }

        Ok(value)
    }

    fn type_check(
        &mut self,
        span: Span,
        found: IntermediateTypeID,
        expect: InferableType,
        handler: &impl ErrorHandler,
    ) -> Result<(), BindingError> {
        fn handle_unification_error(expression_span: Span, err: UnificationError) -> Error {
            let (left, right) = match err {
                UnificationError::TypeMismatchError(TypeMismatchError { left, right }) => {
                    (left.into(), right.into())
                }
                UnificationError::ConstraintNotSatisfiedError(ConstraintNotSatisfiedError {
                    constraint,
                    concrete_type,
                }) => (constraint.into(), concrete_type.into()),
                UnificationError::InvalidIDError(..) => unreachable!(
                    "Internal Compiler Error: InvalidIDError should never be returned from unify"
                ),
            };

            TypeMismatch {
                expression_span,
                found: left,
                expect: right,
            }
            .into()
        }

        let err = match (found, expect) {
            (IntermediateTypeID::InferenceID(found), InferableType::Type(expected)) => {
                let Err(err) = self
                    .inference_context
                    .unify_with_concrete(found, expected) else {
                    return Ok(());
                };
                err
            }
            (IntermediateTypeID::InferenceID(found), InferableType::Constraint(expected)) => {
                let Err(err) = self
                    .inference_context
                    .unify_with_constraint(found, expected) else {
                    return Ok(());
                };
                err
            }
            (IntermediateTypeID::Type(found), InferableType::Constraint(expected)) => {
                if expected.satisfies(found) {
                    return Ok(());
                }

                handler.recieve(Error::TypeMismatch(TypeMismatch {
                    expression_span: span.clone(),
                    found: found.into(),
                    expect: expected.into(),
                }));

                return Err(BindingError(span));
            }
            (IntermediateTypeID::Type(found), InferableType::Type(expected)) => {
                if found == expected {
                    return Ok(());
                }

                handler.recieve(Error::TypeMismatch(TypeMismatch {
                    expression_span: span.clone(),
                    found: found.into(),
                    expect: expected.into(),
                }));
                return Err(BindingError(span));
            }
        };

        handler.recieve(handle_unification_error(span.clone(), err));

        Err(BindingError(span))
    }
}

#[cfg(test)]
mod tests;
