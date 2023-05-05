//! Contains the definition of [`Builder`] -- the main interface for building the HIR.

use std::{collections::HashMap, sync::Arc};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_source::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{
    expression::{
        Binary as BinarySyntaxTree, BinaryOperator as BinaryOperatorSyntaxTree,
        BooleanLiteral as BooleanLiteralSyntaxTree, Expression as ExpressionSyntaxTree,
        FunctionCall as FunctionCallSyntaxTree, Functional as FunctionalSyntaxTree,
        Imperative as ImperativeSyntaxTree, MemberAccess as MemberAccessSyntaxTree,
        Named as NamedSyntaxTree, NumericLiteral as NumericLiteralSyntaxTree,
        Prefix as PrefixSyntaxTree, PrefixOperator, StructLiteral as StructLiteralSyntaxTree,
    },
    statement::{
        Declarative, Expressive, Statement as StatementSyntaxTree,
        VariableDeclaration as VariableDeclarationSyntaxTree,
    },
    ConnectedList,
};
use pernixc_system::arena::{InvalidIDError, Arena};
use thiserror::Error;

use self::scope::{Locals, Stack, BlockPointer, Block};
use super::{
    error::{
        AmbiguousFunctionCall, Error, FloatingPointLiteralHasIntegralSuffix,
        InvalidNumericLiteralSuffix, LValueExpected, NoAccessibleOverload,
        NoOverloadWithMatchingArgumentTypes, NoOverloadWithMatchingNumberOfArguments,
        SymbolNotCallable, ValueExpected,
    },
    instruction::{Basic, RegisterAssignment},
    value::{
        binding::{
            ArithmeticOperator, Binary, BinaryOperator, Binding, ComparisonOperator,
            EqualityOperator, FunctionCall, MemberAccess, Prefix, StructLiteral,
        },
        Address, AddressWithSpan, BooleanLiteral, Constant, EnumLiteral, FieldAddress,
        NumericLiteral, Placeholder, Value,
    },
    AllocaID, Container, ErrorHandler, InvalidValueError, Register, RegisterID, TypeSystem,
    ValueInspect,
};
use crate::{
    cfg::BasicBlockID,
    hir::{
        error::{
            DuplicateFieldInitialization, FieldInaccessible, MutableLValueExpected, NoFieldOnType,
            StructExpected, TypeMismatch, UninitializedFields, UnknownField,
        },
        instruction::{ConditionalJump, Jump, Store, VariableDeclaration},
        value::binding::{Load, LoadType, PhiNode, PhiNodeSource},
        Alloca,
    },
    infer::{
        Constraint, ConstraintNotSatisfiedError, InferableType, InferenceContext, InferenceID,
        TypeMismatchError, UnificationError,
    },
    symbol::{
        table::Table,
        ty::{PrimitiveType, Type},
        FieldID, GlobalID, Overload, OverloadID, OverloadSetID, StructID, TypedID,
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

/// Is a [`TypeSystem`] used for building the [`super::Hir`].
///
/// While building the [`super::Hir`], the type of the value might not be known right away.
/// Therefore, the builder uses this [`IntermediateTypeID`] to represent the type of the value that
/// might be inferred later.
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

/// Is a builder that builds the [`super::Hir`] by inputting the various
/// [`StatementSyntaxTree`](pernixc_syntax::syntax_tree::statement::Statement) to it.
#[derive(Debug, Getters)]
pub struct Builder {
    container: Container<IntermediateTypeID>,
    current_basic_block_id: BasicBlockID,
    inference_context: InferenceContext,
    local_stack: Stack<Locals>,
    block_pointer_stack: Stack<BlockPointer>,
    blocks: Arena<Block>,
}

impl Builder {
    /// Creates a new [`Builder`].
    ///
    /// # Parameters
    /// - `table`: The [`Table`] that will be used for symbol resolution and various lookups.
    /// - `overload_id`: The context in which the [`Builder`] will be building the [`super::Hir`].
    ///
    /// # Errors
    /// - [`InvalidIDError`] if the `overload_id` is invalid for the `table`.
    pub fn new(table: Arc<Table>, overload_id: OverloadID) -> Result<Self, InvalidIDError> {
        let container = Container::new(table, overload_id)?;
        let mut local_stack = Stack::default();
        local_stack.push(Locals::default());

        Ok(Self {
            current_basic_block_id: container.control_flow_graph.entry_block(),
            container,
            inference_context: InferenceContext::new(),
            local_stack,
            blocks: Arena::new(),
            block_pointer_stack: Stack::default(),
        })
    }

    /// Obtains [`InferableType`] for the given value.
    ///
    /// # Errors
    /// - If the given value wasn't created from this [`Builder`].
    pub fn get_inferable_type<T>(&self, value: &T) -> Result<InferableType, InvalidValueError>
    where
        Container<IntermediateTypeID>: ValueInspect<IntermediateTypeID, T>,
    {
        Ok(match self.get_value_intermediate_type_id(value)? {
            IntermediateTypeID::InferenceID(inference_id) => self
                .inference_context
                .get_inferable_type(inference_id)
                .map_err(|_| InvalidValueError)?,
            IntermediateTypeID::Type(ty) => InferableType::Type(ty),
        })
    }

    /// Obtains [`IntermediateTypeID`] for the given value.
    ///
    /// This [`IntermediateTypeID`] doesn't represent the final type of the value. Therefore, do
    /// not use this method for type checking.
    ///
    /// # Errors
    /// - If the given value wasn't created from this [`Builder`].
    pub fn get_value_intermediate_type_id<T>(
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
    AddressWithSpan(AddressWithSpan),
}

/// Is an enumeration used in type checking/inference process.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum ExpectedType {
    /// Unifies the type of the value with another inference variable.
    InferenceID(InferenceID),

    /// Unifies the type of the value with another type.
    Type(Type),

    /// Checks the if the type of the value satisfies the given constraint and unifies the
    /// constraint if possible.
    Constraint(Constraint),
}

impl From<IntermediateTypeID> for ExpectedType {
    fn from(id: IntermediateTypeID) -> Self {
        match id {
            IntermediateTypeID::InferenceID(id) => Self::InferenceID(id),
            IntermediateTypeID::Type(ty) => Self::Type(ty),
        }
    }
}

impl From<InferableType> for ExpectedType {
    fn from(ty: InferableType) -> Self {
        match ty {
            InferableType::Constraint(constraint) => Self::Constraint(constraint),
            InferableType::Type(ty) => Self::Type(ty),
        }
    }
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
            StatementSyntaxTree::Declarative(Declarative::VariableDeclaration(syntax_tree)) => self
                .bind_variable_declaration_statement(syntax_tree, handler)
                .map(|_| ()),
            StatementSyntaxTree::Expressive(syntax_tree) => {
                let binding_option = BindingOption {
                    binding_target: BindingTarget::ForAddress,
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
    ) -> Result<AllocaID, BindingError> {
        // gets the type of the variable
        let ty = if let Some(type_annotation) = syntax_tree.type_annotation() {
            // gets the type of the variable
            self.container
                .table
                .resolve_type(
                    self.container.parent_scoped_id,
                    type_annotation.type_specifier(),
                    handler,
                )
                .map(ExpectedType::Type)
                .unwrap_or(ExpectedType::Constraint(Constraint::All))
        } else {
            ExpectedType::Constraint(Constraint::All)
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
        let ty = self.get_value_intermediate_type_id(&value).unwrap();

        // inserts a new alloca
        let alloca_id = self.container.allocas.insert(Alloca {
            identifier_token: syntax_tree.identifier().clone(),
            is_mutable: syntax_tree.mutable_keyword().is_some(),
            ty,
        });

        self.container
            .control_flow_graph
            .get_mut(self.current_basic_block_id)
            .unwrap()
            .add_basic_instruction(VariableDeclaration { alloca_id }.into());

        // inserts a new store
        self.container
            .control_flow_graph
            .get_mut(self.current_basic_block_id)
            .unwrap()
            .add_basic_instruction(
                Store {
                    address: Address::AllocaID(alloca_id),
                    value,
                    span: syntax_tree.span(),
                }
                .into(),
            );

        // insert into stack
        self.local_stack
            .top_mut()
            .unwrap()
            .insert(syntax_tree.identifier().span.str().to_owned(), alloca_id);

        Ok(alloca_id)
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
            FunctionalSyntaxTree::Binary(syn) => self.bind_binary(syn, binding_option, handler),
            FunctionalSyntaxTree::Prefix(syn) => self
                .bind_prefix(syn, handler)
                .map(|x| BindingResult::Value(Value::Register(x))),
            FunctionalSyntaxTree::Named(syn) => self.bind_named(syn, binding_option, handler),
            FunctionalSyntaxTree::FunctionCall(syn) => self
                .bind_function_call(syn, handler)
                .map(|x| BindingResult::Value(Value::Register(x))),
            FunctionalSyntaxTree::Parenthesized(syn) => {
                self.bind_expression(syn.expression(), binding_option, handler)
            }
            FunctionalSyntaxTree::StructLiteral(syntax_tree) => self
                .bind_struct_literal(syntax_tree, handler)
                .map(|x| BindingResult::Value(Value::Register(x))),
            FunctionalSyntaxTree::MemberAccess(syntax_tree) => {
                self.bind_member_access(syntax_tree, binding_option, handler)
            }
            FunctionalSyntaxTree::Continue(_) => todo!(),
            FunctionalSyntaxTree::Break(_) => todo!(),
            FunctionalSyntaxTree::Return(_) => todo!(),
            FunctionalSyntaxTree::Express(_) => todo!(),
            FunctionalSyntaxTree::Cast(_) => todo!(),
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`ImperativeSyntaxTree`] and returns the [`BindingResult`].
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
                    self.container.parent_scoped_id,
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
                            .map(|a| self.get_inferable_type(a).unwrap())
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
                        .get_inferable_type(argument)
                        .unwrap()
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

                Ok(self.assign_new_register_binding(function_call.into()))
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
            self.container.parent_scoped_id,
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
                let bound_type = self.get_inferable_type(argument).unwrap();
                let ty_match = match bound_type {
                    InferableType::Type(ty) => ty == parameter.type_binding().ty,
                    InferableType::Constraint(constraint) => {
                        constraint.check_satisfy(parameter.type_binding().ty)
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
            PrefixOperator::LogicalNot(..) => ExpectedType::Type(PrimitiveType::Bool.into()),
            PrefixOperator::Negate(..) => ExpectedType::Constraint(Constraint::Signed),
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
                    ExpectedType::Type(ty) => ty.into(),
                    ExpectedType::Constraint(constraint) => {
                        self.inference_context.new_inference(constraint).into()
                    }
                    ExpectedType::InferenceID(..) => unreachable!(),
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

        Ok(self.assign_new_register_binding(binding))
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
                .local_stack
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
                        let binding = Load {
                            span: syntax_tree.span(),
                            load_type: LoadType::Copy, // auto move will be applied later
                            address,
                        }
                        .into();

                        return Ok(BindingResult::Value(Value::Register(
                            self.assign_new_register_binding(binding),
                        )));
                    }
                    BindingTarget::ForAddress => {
                        return Ok(BindingResult::AddressWithSpan(AddressWithSpan {
                            address,
                            span: syntax_tree.span(),
                        }))
                    }
                }
            }
        }

        // search in gloal scope
        let symbol = self
            .container
            .table
            .resolve_symbol(
                self.container.parent_scoped_id,
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

    fn get_struct_id(
        &self,
        syntax_tree: &StructLiteralSyntaxTree,
        handler: &impl ErrorHandler,
    ) -> Result<StructID, BindingError> {
        let found_id = self
            .container
            .table
            .resolve_symbol(
                self.container.parent_scoped_id,
                syntax_tree.qualified_identifier(),
                handler,
            )
            .map_err(|_| BindingError(syntax_tree.span()))?;

        // resolve the struct id
        let struct_id = match found_id {
            GlobalID::Struct(struct_id) => Some(struct_id),
            GlobalID::TypeAlias(type_alias_id) => self
                .container
                .table
                .get_type_alias(type_alias_id)
                .unwrap()
                .alias()
                .into_typed_id()
                .ok()
                .and_then(|x| x.into_struct().ok()),
            _ => None,
        };

        // expect struct id
        let Some(struct_id) = struct_id else {
            handler.recieve(Error::StructExpected(StructExpected {
                found_id,
                symbol_span: syntax_tree.qualified_identifier().span()
            }));
            return Err(BindingError(syntax_tree.span()));
        };

        Ok(struct_id)
    }

    /// Binds the given [`StructLiteralSyntaxTree`] and returns the [`RegisterID`] where the result
    /// is stored.
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    #[allow(clippy::too_many_lines)]
    pub fn bind_struct_literal(
        &mut self,
        syntax_tree: &StructLiteralSyntaxTree,
        handler: &impl ErrorHandler,
    ) -> Result<RegisterID, BindingError> {
        // get struct id
        let struct_id = self.get_struct_id(syntax_tree, handler)?;
        let table = self.container.table.clone();
        let struct_sym = table.get_struct(struct_id).unwrap();
        let mut initializations: HashMap<FieldID, (Span, Value<IntermediateTypeID>)> =
            HashMap::new();
        let mut field_initialize_order = Vec::new();

        for initialization in syntax_tree
            .field_initializations()
            .iter()
            .flat_map(ConnectedList::elements)
        {
            let value = self
                .bind_expression(
                    initialization.expression(),
                    BindingOption::default(),
                    handler,
                )?
                .into_value()
                .unwrap();

            let Some(field_id) = struct_sym
                .field_ids_by_name()
                .get(initialization.identifier().span.str()).copied() else {
                handler.recieve(Error::UnknownField(UnknownField {
                    struct_id,
                    field_name_span: initialization.identifier().span.clone(),
                }));
                continue;
            };

            let field_sym = table.get_field(field_id).unwrap();

            // check if the symbol accessible
            if !self
                .container
                .table
                .symbol_accessible(
                    self.container.parent_scoped_id,
                    struct_id.into(),
                    field_sym.accessibility(),
                )
                .unwrap()
            {
                handler.recieve(Error::FieldInaccessible(FieldInaccessible {
                    field_id,
                    struct_id,
                    field_span: initialization.identifier().span.clone(),
                    current_scope: self.container.parent_scoped_id,
                }));
            }

            let redefined = if let Some((span, _)) = initializations.get(&field_id) {
                // field initialization duplication
                handler.recieve(Error::DuplicateFieldInitialization(
                    DuplicateFieldInitialization {
                        duplicate_initialization_span: initialization.span(),
                        previous_initialization_span: span.clone(),
                        struct_id,
                        field_id,
                    },
                ));
                true
            } else {
                false
            };

            if !redefined {
                initializations.insert(
                    field_id,
                    (
                        initialization.span(),
                        match self.type_check(
                            self.get_span(&value).unwrap(),
                            self.get_value_intermediate_type_id(&value).unwrap(),
                            field_sym.ty().into(),
                            handler,
                        ) {
                            Ok(..) => value,
                            Err(..) => Value::Placeholder(Placeholder {
                                span: initialization.expression().span(),
                                ty: field_sym.ty().into(),
                            }),
                        },
                    ),
                );
                field_initialize_order.push(field_id);
            }
        }

        // check for uninitialized fields
        let mut uninitialized_fields = Vec::new();
        for field in struct_sym.field_order() {
            if !initializations.contains_key(field) {
                uninitialized_fields.push(*field);
            }
        }

        if !uninitialized_fields.is_empty() {
            handler.recieve(Error::UninitializedFields(UninitializedFields {
                struct_literal_span: syntax_tree.span(),
                struct_id,
                uninitialized_fields,
            }));
        }

        let binding = StructLiteral {
            span: syntax_tree.span(),
            struct_id,
            initializations: field_initialize_order
                .into_iter()
                .map(|field_id| (field_id, initializations.remove(&field_id).unwrap().1))
                .collect(),
        }
        .into();

        Ok(self.assign_new_register_binding(binding))
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the [`MemberAccessSyntaxTree`] into a [`BindingResult`]
    ///
    /// # Errors
    /// - If encounters a fatal semantic error
    pub fn bind_member_access(
        &mut self,
        syntax_tree: &MemberAccessSyntaxTree,
        binding_option: BindingOption,
        handler: &impl ErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let operand = self.bind_expression(syntax_tree.operand(), binding_option, handler)?;
        let ty = match &operand {
            BindingResult::Value(value) => self.get_inferable_type(value).unwrap(),
            BindingResult::AddressWithSpan(address) => self.get_address_inferable_type(&address.address),
        };

        // expect the type to be a struct
        let InferableType::Type(Type::TypedID(TypedID::Struct(struct_id))) = ty else {
            handler.recieve(Error::NoFieldOnType(NoFieldOnType {
                operand_span: syntax_tree.operand().span(),
                operand_type: ty
            }));
            return Err(BindingError(syntax_tree.span()));
        };

        // search for the field
        let table_arc = self.container.table.clone();
        let struct_sym = table_arc.get_struct(struct_id).unwrap();

        // search for the field
        let Some(field_id) = struct_sym.field_ids_by_name().get(
            syntax_tree.identifier().span.str()
        ).copied() else {
            handler.recieve(Error::UnknownField(UnknownField {
                struct_id,
                field_name_span: syntax_tree.identifier().span.clone()
            }));
            return Err(BindingError(syntax_tree.span()));
        };

        let field_sym = table_arc.get_field(field_id).unwrap();

        if !table_arc
            .symbol_accessible(
                self.container.parent_scoped_id(),
                struct_id.into(),
                field_sym.accessibility(),
            )
            .unwrap()
        {
            handler.recieve(Error::FieldInaccessible(FieldInaccessible {
                field_id,
                struct_id,
                field_span: syntax_tree.identifier().span.clone(),
                current_scope: self.container.parent_scoped_id(),
            }));
        }

        Ok(match operand {
            BindingResult::Value(value) => {
                let binding = MemberAccess {
                    span: syntax_tree.span(),
                    operand: value,
                    field_id,
                }
                .into();

                BindingResult::Value(Value::Register(self.assign_new_register_binding(binding)))
            }
            BindingResult::AddressWithSpan(address) => {
                BindingResult::AddressWithSpan(AddressWithSpan {
                    address: FieldAddress {
                        operand_address: Box::new(address.address),
                        field_id,
                    }
                    .into(),
                    span: syntax_tree.span(),
                })
            }
        })
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    fn as_arithmetic_operator(binary_operator: &BinaryOperatorSyntaxTree) -> ArithmeticOperator {
        match binary_operator {
            BinaryOperatorSyntaxTree::Add(_) => ArithmeticOperator::Add,
            BinaryOperatorSyntaxTree::Subtract(_) => ArithmeticOperator::Subtract,
            BinaryOperatorSyntaxTree::Multiply(_) => ArithmeticOperator::Multiply,
            BinaryOperatorSyntaxTree::Divide(_) => ArithmeticOperator::Divide,
            BinaryOperatorSyntaxTree::Modulo(_) => ArithmeticOperator::Modulo,
            _ => unreachable!(),
        }
    }

    fn as_comparison_operator(binary_operator: &BinaryOperatorSyntaxTree) -> ComparisonOperator {
        match binary_operator {
            BinaryOperatorSyntaxTree::LessThan(..) => ComparisonOperator::LessThan,
            BinaryOperatorSyntaxTree::LessThanOrEqual(..) => ComparisonOperator::LessThanOrEqual,
            BinaryOperatorSyntaxTree::GreaterThan(..) => ComparisonOperator::GreaterThan,
            BinaryOperatorSyntaxTree::GreaterThanOrEqual(..) => {
                ComparisonOperator::GreaterThanOrEqual
            }
            _ => unreachable!(),
        }
    }

    fn as_equality_operator(binary_operator: &BinaryOperatorSyntaxTree) -> EqualityOperator {
        match binary_operator {
            BinaryOperatorSyntaxTree::Equal(..) => EqualityOperator::Equal,
            BinaryOperatorSyntaxTree::NotEqual(..) => EqualityOperator::NotEqual,
            _ => unreachable!(),
        }
    }

    fn compound_bianry_operator_syntax_tree_to_arithmetic_operator(
        binary_operator: &BinaryOperatorSyntaxTree,
    ) -> ArithmeticOperator {
        match binary_operator {
            BinaryOperatorSyntaxTree::CompoundAdd(..) => ArithmeticOperator::Add,
            BinaryOperatorSyntaxTree::CompoundSubtract(..) => ArithmeticOperator::Subtract,
            BinaryOperatorSyntaxTree::CompoundMultiply(..) => ArithmeticOperator::Multiply,
            BinaryOperatorSyntaxTree::CompoundDivide(..) => ArithmeticOperator::Divide,
            BinaryOperatorSyntaxTree::CompoundModulo(..) => ArithmeticOperator::Modulo,
            _ => unreachable!(),
        }
    }

    fn bind_left_and_right(
        &mut self,
        syntax_tree: &BinarySyntaxTree,
        is_assign: bool,
        expected_type: Option<ExpectedType>,
        handler: &impl ErrorHandler,
    ) -> Result<(BindingResult, Value<IntermediateTypeID>), BindingError> {
        let left_binding_option = {
            let binding_target = if is_assign {
                BindingTarget::ForAddress
            } else {
                BindingTarget::ForValue
            };
            BindingOption { binding_target }
        };

        let left = if let Some(inferable_type_check) = expected_type {
            self.expect_expression(
                syntax_tree.left_operand(),
                left_binding_option,
                inferable_type_check,
                handler,
            )
        } else {
            self.bind_expression(syntax_tree.left_operand(), left_binding_option, handler)
        };

        let right = self.bind_expression(
            syntax_tree.right_operand(),
            BindingOption::default(),
            handler,
        );

        match (left, right) {
            (Ok(left), Ok(right)) => {
                let mut right = right.into_value().unwrap();

                // unifies the types
                let left_ty = self.get_binding_result_intermediate_type_id(&left);
                let right_ty = self.get_value_intermediate_type_id(&right).unwrap();

                if let Err(err) = self.type_check(
                    syntax_tree.right_operand().span(),
                    left_ty,
                    right_ty.into(),
                    handler,
                ) {
                    // right must match the left. if not, change right to placeholder value that has
                    // the same type as the left
                    right = Value::Placeholder(Placeholder {
                        span: err.0,
                        ty: left_ty,
                    });
                }

                Ok((left, right))
            }
            (Ok(left), Err(_)) => {
                let left_ty = self.get_binding_result_intermediate_type_id(&left);
                Ok((
                    left,
                    Value::Placeholder(Placeholder {
                        span: syntax_tree.right_operand().span(),
                        ty: left_ty,
                    }),
                ))
            }
            (Err(_), Ok(right)) => {
                if is_assign {
                    Err(BindingError(syntax_tree.span()))
                } else {
                    Ok((
                        BindingResult::Value(Value::Placeholder(Placeholder {
                            span: syntax_tree.left_operand().span(),
                            ty: self.get_binding_result_intermediate_type_id(&right),
                        })),
                        right.into_value().unwrap(),
                    ))
                }
            }
            (Err(_), Err(_)) => Err(BindingError(syntax_tree.span())),
        }
    }

    fn handle_normal_binary(
        &mut self,
        syntax_tree: &BinarySyntaxTree,
        binary_operator: BinaryOperator,
        inferable_type_check: Option<ExpectedType>,
        handler: &impl ErrorHandler,
    ) -> Result<RegisterID, BindingError> {
        let (left, rhs) =
            self.bind_left_and_right(syntax_tree, false, inferable_type_check, handler)?;
        let left = left.into_value().unwrap();

        let binding = Binary {
            left_operand: left,
            right_operand: rhs,
            span: syntax_tree.span(),
            binary_operator,
        };

        Ok(self.assign_new_register_binding(binding.into()))
    }

    fn handle_assign(
        &mut self,
        syntax_tree: &BinarySyntaxTree,
        binding_option: BindingOption,
        handler: &impl ErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let (lhs, rhs) = self.bind_left_and_right(syntax_tree, true, None, handler)?;

        // lhs must be address
        let BindingResult::AddressWithSpan(address) = lhs else {
            handler.recieve(Error::LValueExpected(LValueExpected {
                expression_span: syntax_tree.left_operand().span()
            }));

            return Err(BindingError(syntax_tree.span()));
        };

        // lhs address must be mutable
        if !self.get_address_mutability(&address.address) {
            handler.recieve(Error::MutableLValueExpected(MutableLValueExpected {
                expression_span: syntax_tree.left_operand().span(),
            }));
        }

        // insert store instruction
        self.add_basic_instruction(
            Store {
                address: address.address.clone(),
                value: rhs,
                span: syntax_tree.span(),
            }
            .into(),
        );

        match binding_option.binding_target {
            BindingTarget::ForValue => {
                let load = Load {
                    span: syntax_tree.span(),
                    load_type: LoadType::Copy,
                    address: address.address,
                }
                .into();

                Ok(BindingResult::Value(Value::Register(
                    self.assign_new_register_binding(load),
                )))
            }
            BindingTarget::ForAddress => Ok(BindingResult::AddressWithSpan(address)),
        }
    }

    fn handle_compound_arithmetic(
        &mut self,
        syntax_tree: &BinarySyntaxTree,
        binding_option: BindingOption,
        arithmetic_operator: ArithmeticOperator,
        handler: &impl ErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let (lhs, rhs) = self.bind_left_and_right(syntax_tree, true, None, handler)?;

        // lhs must be address
        let BindingResult::AddressWithSpan(address) = lhs else {
            handler.recieve(Error::LValueExpected(LValueExpected {
                expression_span: syntax_tree.left_operand().span()
            }));

            return Err(BindingError(syntax_tree.span()));
        };

        // lhs address must be mutable
        if !self.get_address_mutability(&address.address) {
            handler.recieve(Error::MutableLValueExpected(MutableLValueExpected {
                expression_span: syntax_tree.left_operand().span(),
            }));
        }

        let load_temp_register = self.assign_new_register_binding(Binding::Load(Load {
            span: syntax_tree.left_operand().span(),
            load_type: LoadType::Copy,
            address: address.address.clone(),
        }));
        let arithmetic_result_register =
            self.assign_new_register_binding(Binding::Binary(Binary {
                span: syntax_tree.span(),
                left_operand: Value::Register(load_temp_register),
                right_operand: rhs,
                binary_operator: BinaryOperator::ArithmeticOperator(arithmetic_operator),
            }));
        // store the result
        self.add_basic_instruction(
            Store {
                address: address.address.clone(),
                value: Value::Register(arithmetic_result_register),
                span: syntax_tree.span(),
            }
            .into(),
        );

        match binding_option.binding_target {
            BindingTarget::ForValue => Ok(BindingResult::Value(Value::Register(
                arithmetic_result_register,
            ))),
            BindingTarget::ForAddress => Ok(BindingResult::AddressWithSpan(address)),
        }
    }

    fn handle_short_circuit(
        &mut self,
        syntax_tree: &BinarySyntaxTree,
        handler: &impl ErrorHandler,
    ) -> RegisterID {
        let left = self
            .expect_expression(
                syntax_tree.left_operand(),
                BindingOption::default(),
                ExpectedType::Type(Type::PrimitiveType(PrimitiveType::Bool)),
                handler,
            )
            .map_or_else(
                |err| {
                    Value::Placeholder(Placeholder {
                        span: err.0,
                        ty: IntermediateTypeID::Type(Type::PrimitiveType(PrimitiveType::Bool)),
                    })
                },
                |x| x.into_value().unwrap(),
            );

        let is_or = match syntax_tree.binary_operator() {
            BinaryOperatorSyntaxTree::LogicalAnd(..) => false,
            BinaryOperatorSyntaxTree::LogicalOr(..) => true,
            _ => unreachable!(),
        };

        let pre_block_id = self.current_basic_block_id;
        let rhs_condition_block_id = self.container.control_flow_graph.new_basic_block();
        let continue_block_id = self.container.control_flow_graph.new_basic_block();

        // add condition jump
        self.container
            .control_flow_graph
            .add_conditional_jump_instruction(pre_block_id, ConditionalJump {
                condition: left.clone(),
                true_jump_target: if is_or {
                    continue_block_id
                } else {
                    rhs_condition_block_id
                },
                false_jump_target: if is_or {
                    rhs_condition_block_id
                } else {
                    continue_block_id
                },
            })
            .unwrap();

        // to the rhs condition block, bind the rhs
        let rhs_value = {
            self.current_basic_block_id = rhs_condition_block_id;
            let rhs_value = self
                .expect_expression(
                    syntax_tree.right_operand(),
                    BindingOption::default(),
                    ExpectedType::Type(Type::PrimitiveType(PrimitiveType::Bool)),
                    handler,
                )
                .map_or_else(
                    |err| {
                        Value::Placeholder(Placeholder {
                            span: err.0,
                            ty: IntermediateTypeID::Type(Type::PrimitiveType(PrimitiveType::Bool)),
                        })
                    },
                    |x| x.into_value().unwrap(),
                );

            // branch to continue block
            self.container
                .control_flow_graph
                .add_jump_instruction(rhs_condition_block_id, Jump {
                    jump_target: continue_block_id,
                })
                .unwrap();

            rhs_value
        };

        // to the continue block, add phi node and return the result
        self.current_basic_block_id = continue_block_id;

        self.assign_new_register_binding(Binding::PhiNode(PhiNode {
            span: syntax_tree.span(),
            values_by_predecessor: {
                let mut values_by_predecessor = HashMap::new();
                values_by_predecessor.insert(pre_block_id, left);
                values_by_predecessor.insert(rhs_condition_block_id, rhs_value);
                values_by_predecessor
            },
            phi_node_source: PhiNodeSource::LogicalShortCircuit,
        }))
    }

    /// Binds the [`NamedSyntaxTree`] into a [`BindingResult`]
    ///
    /// # Errors
    /// - If encounters a fatal semantic error
    pub fn bind_binary(
        &mut self,
        syntax_tree: &BinarySyntaxTree,
        binding_option: BindingOption,
        handler: &impl ErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        match syntax_tree.binary_operator() {
            BinaryOperatorSyntaxTree::Add(..)
            | BinaryOperatorSyntaxTree::Subtract(..)
            | BinaryOperatorSyntaxTree::Multiply(..)
            | BinaryOperatorSyntaxTree::Divide(..)
            | BinaryOperatorSyntaxTree::Modulo(..) => self
                .handle_normal_binary(
                    syntax_tree,
                    Self::as_arithmetic_operator(syntax_tree.binary_operator()).into(),
                    Some(ExpectedType::Constraint(Constraint::Number)),
                    handler,
                )
                .map(|x| BindingResult::Value(Value::Register(x))),
            BinaryOperatorSyntaxTree::Assign(..) => {
                self.handle_assign(syntax_tree, binding_option, handler)
            }
            BinaryOperatorSyntaxTree::CompoundAdd(..)
            | BinaryOperatorSyntaxTree::CompoundSubtract(..)
            | BinaryOperatorSyntaxTree::CompoundMultiply(..)
            | BinaryOperatorSyntaxTree::CompoundDivide(..)
            | BinaryOperatorSyntaxTree::CompoundModulo(..) => self.handle_compound_arithmetic(
                syntax_tree,
                binding_option,
                Self::compound_bianry_operator_syntax_tree_to_arithmetic_operator(
                    syntax_tree.binary_operator(),
                ),
                handler,
            ),
            BinaryOperatorSyntaxTree::Equal(..) | BinaryOperatorSyntaxTree::NotEqual(..) => self
                .handle_normal_binary(
                    syntax_tree,
                    Self::as_equality_operator(syntax_tree.binary_operator()).into(),
                    Some(ExpectedType::Constraint(Constraint::PrimitiveType)),
                    handler,
                )
                .map(|x| BindingResult::Value(Value::Register(x))),
            BinaryOperatorSyntaxTree::LessThan(..)
            | BinaryOperatorSyntaxTree::LessThanOrEqual(..)
            | BinaryOperatorSyntaxTree::GreaterThan(..)
            | BinaryOperatorSyntaxTree::GreaterThanOrEqual(..) => self
                .handle_normal_binary(
                    syntax_tree,
                    Self::as_comparison_operator(syntax_tree.binary_operator()).into(),
                    Some(ExpectedType::Constraint(Constraint::Number)),
                    handler,
                )
                .map(|x| BindingResult::Value(Value::Register(x))),
            BinaryOperatorSyntaxTree::LogicalAnd(..) | BinaryOperatorSyntaxTree::LogicalOr(..) => {
                Ok(BindingResult::Value(Value::Register(
                    self.handle_short_circuit(syntax_tree, handler),
                )))
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////
}

impl Builder {
    fn add_basic_instruction(&mut self, inst: Basic<IntermediateTypeID>) {
        self.container
            .control_flow_graph
            .get_mut(self.current_basic_block_id)
            .unwrap()
            .add_basic_instruction(inst);
    }

    fn assign_new_register_binding(&mut self, binding: Binding<IntermediateTypeID>) -> RegisterID {
        let register_id = self.container.registers.insert(Register { binding });
        self.container
            .control_flow_graph
            .get_mut(self.current_basic_block_id)
            .unwrap()
            .add_basic_instruction(RegisterAssignment { register_id }.into());
        register_id
    }

    fn get_address_mutability(&self, address: &Address) -> bool {
        match address {
            Address::AllocaID(alloca_id) => {
                self.container.allocas.get(*alloca_id).unwrap().is_mutable
            }
            Address::ParameterID(param_id) => {
                self.container
                    .table
                    .get_parameter(*param_id)
                    .unwrap()
                    .type_binding()
                    .is_mutable
            }
            Address::FieldAddress(field_address) => {
                self.get_address_mutability(&field_address.operand_address)
            }
        }
    }

    fn get_address_inferable_type(&self, address: &Address) -> InferableType {
        let intermediate_type = self.get_address_intermediate_type_id(address);
        match intermediate_type {
            IntermediateTypeID::InferenceID(id) => {
                self.inference_context.get_inferable_type(id).unwrap()
            }
            IntermediateTypeID::Type(ty) => ty.into(),
        }
    }

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
            Address::FieldAddress(address) => self
                .container
                .table
                .get_field(address.field_id())
                .unwrap()
                .ty()
                .into(),
        }
    }

    fn get_binding_result_intermediate_type_id(
        &self,
        binding_result: &BindingResult,
    ) -> IntermediateTypeID {
        match binding_result {
            BindingResult::Value(value) => self.get_value_intermediate_type_id(value).unwrap(),
            BindingResult::AddressWithSpan(address) => {
                self.get_address_intermediate_type_id(&address.address)
            }
        }
    }

    /// Similar to `bind_expression` but with additional type checking.
    fn expect_expression(
        &mut self,
        syntax_tree: &ExpressionSyntaxTree,
        binding_option: BindingOption,
        expected: ExpectedType,
        handler: &impl ErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let result = self.bind_expression(syntax_tree, binding_option, handler)?;

        let intermediate_ty = self.get_binding_result_intermediate_type_id(&result);

        self.type_check(syntax_tree.span(), intermediate_ty, expected, handler)?;

        Ok(result)
    }

    fn handle_unification_error(expression_span: Span, err: UnificationError, swap: bool) -> Error {
        let (mut left, mut right) = match err {
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

        if swap {
            std::mem::swap(&mut left, &mut right);
        }

        TypeMismatch {
            expression_span,
            found: left,
            expect: right,
        }
        .into()
    }

    fn type_check(
        &mut self,
        span: Span,
        found: IntermediateTypeID,
        expected: ExpectedType,
        handler: &impl ErrorHandler,
    ) -> Result<(), BindingError> {
        let (err, swapped) = match (found, expected) {
            (IntermediateTypeID::InferenceID(found), ExpectedType::InferenceID(expect)) => {
                (self.inference_context.unify(found, expect), false)
            }
            (IntermediateTypeID::InferenceID(found), ExpectedType::Type(expect)) => (
                self.inference_context.unify_with_concrete(found, expect),
                false,
            ),
            (IntermediateTypeID::InferenceID(found), ExpectedType::Constraint(expect)) => (
                self.inference_context.unify_with_constraint(found, expect),
                false,
            ),
            (IntermediateTypeID::Type(found), ExpectedType::InferenceID(expect)) => (
                self.inference_context.unify_with_concrete(expect, found),
                true,
            ),
            (IntermediateTypeID::Type(found), ExpectedType::Type(expect)) => {
                if found == expect {
                    return Ok(());
                }

                handler.recieve(Error::TypeMismatch(TypeMismatch {
                    expression_span: span.clone(),
                    expect: InferableType::Type(expect),
                    found: InferableType::Type(found),
                }));

                return Err(BindingError(span));
            }
            (IntermediateTypeID::Type(found), ExpectedType::Constraint(expect)) => {
                if expect.check_satisfy(found) {
                    return Ok(());
                }

                handler.recieve(Error::TypeMismatch(TypeMismatch {
                    expression_span: span.clone(),
                    expect: InferableType::Constraint(expect),
                    found: InferableType::Type(found),
                }));

                return Err(BindingError(span));
            }
        };

        let Err(err) = err else {
            return Ok(())
        };

        handler.recieve(Self::handle_unification_error(span.clone(), err, swapped));

        Err(BindingError(span))
    }
}

#[cfg(test)]
mod tests;
