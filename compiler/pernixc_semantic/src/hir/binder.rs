//! Contains the definition of [`Binder`] -- the main interface for building the HIR.

use std::{
    collections::{HashMap, HashSet},
    sync::{Arc, RwLock},
};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{Getters, MutGetters};
use pernixc_source::{SourceElement, Span};

use pernixc_syntax::syntax_tree::{
    expression::{
        Binary as BinarySyntaxTree, BinaryOperator as BinaryOperatorSyntaxTree,
        Block as BlockSyntaxTree, BlockOrIfElse, BooleanLiteral as BooleanLiteralSyntaxTree,
        Break as BreakSyntaxTree, Cast as CastSyntaxTree, Continue as ContinueSyntaxTree,
        Express as ExpressSyntaxTree, Expression as ExpressionSyntaxTree,
        FunctionCall as FunctionCallSyntaxTree, Functional as FunctionalSyntaxTree,
        IfElse as IfElseSyntaxTree, Imperative as ImperativeSyntaxTree, Loop as LoopSyntaxTree,
        MemberAccess as MemberAccessSyntaxTree, Named as NamedSyntaxTree,
        NumericLiteral as NumericLiteralSyntaxTree, Prefix as PrefixSyntaxTree, PrefixOperator,
        Return as ReturnSyntaxTree, StructLiteral as StructLiteralSyntaxTree,
    },
    statement::{
        Declarative, Expressive, Statement as StatementSyntaxTree,
        VariableDeclaration as VariableDeclarationSyntaxTree,
    },
    ConnectedList, Label,
};
use pernixc_system::{
    arena::{Arena, InvalidIDError},
    error_handler::ErrorHandler,
};
use thiserror::Error;

use self::stack::Stack;

use super::{
    error::{
        AmbiguousFunctionCall, DuplicateFieldInitialization, Error, ExpressOutsideBlock,
        FieldInaccessible, FloatingPointLiteralHasIntegralSuffix, InvalidNumericLiteralSuffix,
        LValueExpected, MutableLValueExpected, NoAccessibleOverload, NoBlockWithGivenLabelFound,
        NoFieldOnType, NoOverloadWithMatchingArgumentTypes,
        NoOverloadWithMatchingNumberOfArguments, NotAllFlowPathExpressValue, ReturnValueExpected,
        StructExpected, SymbolNotCallable, TypeMismatch, UninitializedFields, UnknownField,
        ValueExpected,
    },
    instruction::{
        Backend, Basic, ConditionalJump, Jump, RegisterAssignment, Return, ScopePop, ScopePush,
        Store, VariableDeclaration,
    },
    value::{
        binding::{
            ArithmeticOperator, Binary, BinaryOperator, Binding, Cast, ComparisonOperator,
            EqualityOperator, FunctionCall, Load, LoadType, MemberAccess, PhiNode, PhiNodeSource,
            Prefix, StructLiteral,
        },
        Address, AddressWithSpan, BooleanLiteral, Constant, EnumLiteral, FieldAddress,
        NumericLiteral, Placeholder, Unreachable, Value, VariableID, VoidConstant,
    },
    Alloca, AllocaID, Branch, Container, ErrorHandler as HirErrorHandler, Hir, InvalidValueError,
    Register, RegisterID, Scope, ScopeChildID, ScopeID, ScopeSymbol, Suboptimal, TypeSystem,
    ValueInspect,
};
use crate::{
    cfg::{BasicBlock, BasicBlockID, ControlFlowGraph, Instruction},
    hir::instruction::JumpSource,
    infer::{
        Constraint, ConstraintNotSatisfiedError, InferableType, InferenceContext, InferenceID,
        TypeMismatchError, UnificationError,
    },
    symbol::{
        table::{ResolveError, Table},
        ty::{PrimitiveType, Type},
        FieldID, GlobalID, Overload, OverloadID, OverloadSetID, StructID, TypedID,
    },
};

mod stack;

#[derive(Debug)]
struct BlockScope {
    label: Option<String>,
    incoming_values: HashMap<BasicBlockID, Value<IntermediateTypeID>>,
    continue_basic_block_id: BasicBlockID,
    express_ty: Option<IntermediateTypeID>,
}

#[derive(Debug)]
struct LoopScope {
    label: Option<String>,
    incoming_values: HashMap<BasicBlockID, Value<IntermediateTypeID>>,
    loop_header_basic_block: BasicBlockID,
    loop_exit_basic_block: BasicBlockID,
    break_ty: Option<IntermediateTypeID>,
}

/// Is an enumeration flag that specifies how the builder should bind the syntax tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, EnumAsInner, From)]
pub enum BindingTarget {
    /// Binds the syntax tree for a value.
    ///
    /// This is the default binding target.
    #[default]
    ForValue,

    /// Binds the syntax tree for the underlying address of the value.
    ///
    /// This is used for obtaining the address of r-values.
    ForAddress {
        /// Specifies whether the storage from the address must be mutable or not
        is_mutable: bool,
    },

    /// Binds the syntax tree for the side effect of producing the value.
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
    pub binding_target: BindingTarget,
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
/// type equality. Instead, retrieve the [`InferableType`] from [`Binder`] then first
/// check for type equality.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum IntermediateTypeID {
    /// The type might be inferred later by the inference context.
    InferenceID(InferenceID),

    /// The type is determined right away at the time of the creation of the value.
    Type(Type),
}

impl TypeSystem for IntermediateTypeID {
    fn from_type(ty: Type) -> Self {
        ty.into()
    }
}

/// Is a builder that builds the [`super::Hir`] by inputting the various
/// [`StatementSyntaxTree`](pernixc_syntax::syntax_tree::statement::Statement) to it.
#[derive(Debug, Getters, MutGetters)]
pub struct Binder {
    #[get = "pub(super)"]
    container: Container<IntermediateTypeID>,
    current_basic_block_id: BasicBlockID,
    inference_context: InferenceContext,
    stack: Stack,
    block_scopes_by_scope_id: HashMap<ScopeID, BlockScope>,
    loop_scopes_by_scope_id: HashMap<ScopeID, LoopScope>,
    suboptimal: Arc<RwLock<bool>>,
}

struct BinderErrorHandlerAdapter<'a, T> {
    handler: &'a T,
    suboptimal: Arc<RwLock<bool>>,
}

impl<'a, T> pernixc_system::error_handler::ErrorHandler<crate::symbol::error::Error>
    for BinderErrorHandlerAdapter<'a, T>
where
    T: pernixc_system::error_handler::ErrorHandler<crate::symbol::error::Error>,
{
    fn recieve(&self, error: crate::symbol::error::Error) {
        self.handler.recieve(error);
        *self.suboptimal.write().unwrap() = true;
    }
}

impl<'a, T> pernixc_system::error_handler::ErrorHandler<super::error::Error>
    for BinderErrorHandlerAdapter<'a, T>
where
    T: pernixc_system::error_handler::ErrorHandler<super::error::Error>,
{
    fn recieve(&self, error: super::error::Error) {
        self.handler.recieve(error);
        *self.suboptimal.write().unwrap() = true;
    }
}

impl Binder {
    /// Creates a new [`Binder`].
    ///
    /// # Parameters
    /// - `table`: The [`Table`] that will be used for symbol resolution and various lookups.
    /// - `overload_id`: The context in which the [`Binder`] will be building the [`super::Hir`].
    ///
    /// # Errors
    /// - [`InvalidIDError`] if the `overload_id` is invalid for the `table`.
    pub fn new(table: Arc<Table>, overload_id: OverloadID) -> Result<Self, InvalidIDError> {
        let mut container = Container::new(table, overload_id)?;

        let overload = container.table.get_overload(overload_id).unwrap();
        let mut stack = Stack::new(container.scope_tree.root_scope());

        for parameter in overload
            .parameter_order()
            .iter()
            .map(|x| container.table.get_parameter(*x).unwrap())
        {
            let variable_id = VariableID::ParameterID(parameter.id());
            stack
                .current_local_mut()
                .new_variable(parameter.name().to_string(), variable_id);
        }

        let entry_basic_block_id = container.control_flow_graph.entry_block();
        container
            .control_flow_graph
            .get_mut(entry_basic_block_id)
            .unwrap()
            .add_basic_instruction(Basic::ScopePush(ScopePush {
                scope_id: container.scope_tree.root_scope(),
            }));

        Ok(Self {
            current_basic_block_id: container.control_flow_graph.entry_block(),
            container,
            inference_context: InferenceContext::new(),
            stack,
            block_scopes_by_scope_id: HashMap::new(),
            loop_scopes_by_scope_id: HashMap::new(),
            suboptimal: Arc::new(RwLock::new(false)),
        })
    }

    /// Obtains [`InferableType`] for the given value.
    ///
    /// # Errors
    /// - If the given value wasn't created from this [`Binder`].
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
    /// - If the given value wasn't created from this [`Binder`].
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
    /// - If the given value wasn't created from this [`Binder`].
    pub fn get_span<T>(&self, value: &T) -> Result<Span, InvalidValueError>
    where
        Container<IntermediateTypeID>: ValueInspect<IntermediateTypeID, T>,
    {
        self.container.get_span(value)
    }

    /// Mpas the given [`IntermediateTypeID`] to [`InferableType`].
    ///
    /// # Errors
    /// - If the given [`IntermediateTypeID`] wasn't created from this [`Binder`].
    pub fn map_intermediate_type_id(
        &self,
        intermediate_ty_id: IntermediateTypeID,
    ) -> Result<InferableType, InvalidIDError> {
        match intermediate_ty_id {
            IntermediateTypeID::InferenceID(inference_id) => self
                .inference_context
                .get_inferable_type(inference_id)
                .map_err(|_| InvalidIDError),
            IntermediateTypeID::Type(ty) => Ok(InferableType::Type(ty)),
        }
    }

    /// Assigns the given [`Binding`] into a new [`Register`] and returns the [`RegisterID`].
    fn assign_new_register_binding(&mut self, binding: Binding<IntermediateTypeID>) -> RegisterID {
        let register_id = self.container.registers.insert(Register { binding });
        self.container
            .control_flow_graph
            .get_mut(self.current_basic_block_id)
            .unwrap()
            .add_basic_instruction(RegisterAssignment { register_id }.into());
        register_id
    }

    fn get_binding_result_intermediate_type_id(
        &self,
        binding_result: &BindingResult,
    ) -> IntermediateTypeID {
        match binding_result {
            BindingResult::Value(value) => self.get_value_intermediate_type_id(value).unwrap(),
            BindingResult::AddressWithSpan(address) => self
                .get_address_intermediate_type_id(&address.address)
                .unwrap(),

            BindingResult::None(ty_id) => *ty_id,
        }
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

    /// Performs type checking on the given type against the expected type.
    ///
    /// # Parameters
    /// - `span`: The span of the value being type checked. This is used to generate diagnostics.
    /// - `found`: The type of the value being type checked.
    /// - `expected`: The expected type of the value being type checked.
    /// - `handler`: The error handler to use for reporting errors.
    ///
    /// # Returns
    /// `Ok(())` if the type check succeeded, `Err(BindingError)` otherwise.
    fn type_check(
        &mut self,
        span: Span,
        found: IntermediateTypeID,
        expected: ExpectedType,
        handler: &impl HirErrorHandler,
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

                self.create_error_handler_adapter(handler)
                    .recieve(Error::TypeMismatch(TypeMismatch {
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

                self.create_error_handler_adapter(handler)
                    .recieve(Error::TypeMismatch(TypeMismatch {
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

        self.create_error_handler_adapter(handler)
            .recieve(Self::handle_unification_error(span.clone(), err, swapped));

        Err(BindingError(span))
    }

    fn add_basic_instruction(&mut self, inst: Basic<IntermediateTypeID>) {
        self.container
            .control_flow_graph
            .get_mut(self.current_basic_block_id)
            .unwrap()
            .add_basic_instruction(inst);
    }

    /// Gets the [`IntermediateTypeID`] of the value stored at the given [`Address`].
    ///
    /// # Errors
    /// - If the given [`Address`] wasn't created from this [`Binder`]
    pub fn get_address_intermediate_type_id(
        &self,
        address: &Address,
    ) -> Result<IntermediateTypeID, InvalidValueError> {
        match address {
            Address::VariableID(VariableID::AllocaID(id)) => Ok(self
                .container
                .allocas
                .get(*id)
                .map_err(|_| InvalidValueError)?
                .ty),
            Address::VariableID(VariableID::ParameterID(id)) => Ok(IntermediateTypeID::Type(
                self.container
                    .table
                    .get_parameter(*id)
                    .map_err(|_| InvalidValueError)?
                    .type_binding()
                    .ty,
            )),
            Address::FieldAddress(address) => Ok(IntermediateTypeID::Type(
                self.container
                    .table
                    .get_field(address.field_id())
                    .map_err(|_| InvalidValueError)?
                    .ty(),
            )),
        }
    }

    /// Gets the [`InferableType`] of the value stored in the given [`Address`].
    ///
    /// # Errors
    /// - If the given [`Address`] wasn't created from this [`Binder`]
    pub fn get_address_inferable_type(
        &self,
        address: &Address,
    ) -> Result<InferableType, InvalidValueError> {
        let intermediate_type = self.get_address_intermediate_type_id(address)?;
        match intermediate_type {
            IntermediateTypeID::InferenceID(id) => Ok(self
                .inference_context
                .get_inferable_type(id)
                .map_err(|_| InvalidValueError)?),
            IntermediateTypeID::Type(ty) => Ok(InferableType::Type(ty)),
        }
    }

    /// Returns a boolean indicating whether the given [`Address`] is mutable or not.
    ///
    /// # Errors
    /// - If the given [`Address`] wasn't created from this [`Binder`]
    pub fn get_address_mutability(&self, address: &Address) -> Result<bool, InvalidValueError> {
        match address {
            Address::VariableID(VariableID::AllocaID(alloca_id)) => Ok(self
                .container
                .allocas
                .get(*alloca_id)
                .map_err(|_| InvalidValueError)?
                .is_mutable),
            Address::VariableID(VariableID::ParameterID(param_id)) => Ok(self
                .container
                .table
                .get_parameter(*param_id)
                .map_err(|_| InvalidValueError)?
                .type_binding()
                .is_mutable),
            Address::FieldAddress(field_address) => {
                self.get_address_mutability(&field_address.operand_address)
            }
        }
    }

    fn current_scope(&self) -> &ScopeSymbol {
        self.container
            .scope_tree
            .scopes
            .get(self.stack.current_local().scope_id())
            .unwrap()
    }

    fn current_scope_mut(&mut self) -> &mut ScopeSymbol {
        self.container
            .scope_tree
            .scopes
            .get_mut(self.stack.current_local().scope_id())
            .unwrap()
    }

    fn current_basic_block(&self) -> &BasicBlock<Backend<IntermediateTypeID>> {
        self.container
            .control_flow_graph
            .get(self.current_basic_block_id)
            .unwrap()
    }

    fn current_basic_block_mut(&mut self) -> &mut BasicBlock<Backend<IntermediateTypeID>> {
        self.container
            .control_flow_graph
            .get_mut(self.current_basic_block_id)
            .unwrap()
    }

    fn create_error_handler_adapter<'a, T: HirErrorHandler>(
        &self,
        handler: &'a T,
    ) -> BinderErrorHandlerAdapter<'a, T> {
        BinderErrorHandlerAdapter {
            handler,
            suboptimal: self.suboptimal.clone(),
        }
    }
}

/// Is an error occurred during the binding process.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Error)]
#[error("Encountered a fatal semantic error that cause the binding process to terminate.")]
pub struct BindingError(pub Span);

/// Is the result of various `bind_*` functions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum BindingResult {
    /// The binding process returns a value.
    Value(Value<IntermediateTypeID>),

    /// The binding process returns an address to a value.
    AddressWithSpan(AddressWithSpan),

    /// The binding process doesn't return any value but side effects have occurred.
    None(IntermediateTypeID),
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

impl Binder {
    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`ExpressionSyntaxTree`] and returns the [`BindingResult`].
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_expression(
        &mut self,
        syntax_tree: &ExpressionSyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
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

    /// Binds the given [`FunctionalSyntaxTree`] and returns the [`BindingResult`].
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_functional(
        &mut self,
        syntax_tree: &FunctionalSyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        match syntax_tree {
            FunctionalSyntaxTree::NumericLiteral(syn) => {
                self.bind_numeric_literal(syn, binding_option, handler)
            }
            FunctionalSyntaxTree::BooleanLiteral(syn) => {
                self.bind_boolean_literal(syn, binding_option, handler)
            }
            FunctionalSyntaxTree::Binary(syn) => self.bind_binary(syn, binding_option, handler),
            FunctionalSyntaxTree::Prefix(syn) => self.bind_prefix(syn, binding_option, handler),
            FunctionalSyntaxTree::Named(syn) => self.bind_named(syn, binding_option, handler),
            FunctionalSyntaxTree::FunctionCall(syn) => {
                self.bind_function_call(syn, binding_option, handler)
            }
            FunctionalSyntaxTree::Parenthesized(syn) => {
                self.bind_expression(syn.expression(), binding_option, handler)
            }
            FunctionalSyntaxTree::StructLiteral(syntax_tree) => {
                self.bind_struct_literal(syntax_tree, binding_option, handler)
            }
            FunctionalSyntaxTree::MemberAccess(syntax_tree) => {
                self.bind_member_access(syntax_tree, binding_option, handler)
            }
            FunctionalSyntaxTree::Continue(syn) => self.bind_continue(syn, binding_option, handler),
            FunctionalSyntaxTree::Break(syn) => self.bind_break(syn, binding_option, handler),
            FunctionalSyntaxTree::Return(syn) => self.bind_return(syn, binding_option, handler),
            FunctionalSyntaxTree::Express(syn) => self.bind_express(syn, binding_option, handler),
            FunctionalSyntaxTree::Cast(syn) => self.bind_cast(syn, binding_option, handler),
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`ImperativeSyntaxTree`] and returns the [`BindingResult`].
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_imperative(
        &mut self,
        syntax_tree: &ImperativeSyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        match syntax_tree {
            ImperativeSyntaxTree::Block(syn) => self.bind_block(syn, binding_option, handler),
            ImperativeSyntaxTree::IfElse(syn) => self.bind_if_else(syn, binding_option, handler),
            ImperativeSyntaxTree::Loop(syn) => self.bind_loop(syn, binding_option, handler),
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Similar to `bind_expression` but with additional type checking.
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error or a type mismatch.
    pub fn expect_expression(
        &mut self,
        syntax_tree: &ExpressionSyntaxTree,
        binding_option: BindingOption,
        expected: ExpectedType,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let result = self.bind_expression(syntax_tree, binding_option, handler)?;

        let intermediate_ty = self.get_binding_result_intermediate_type_id(&result);

        self.type_check(syntax_tree.span(), intermediate_ty, expected, handler)?;

        Ok(result)
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds [`StatementSyntaxTree`] into an instruction that will be inserted into the control
    /// flow graph in this builder.
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_statement(
        &mut self,
        syntax_tree: &StatementSyntaxTree,
        handler: &impl HirErrorHandler,
    ) -> Result<(), BindingError> {
        match syntax_tree {
            StatementSyntaxTree::Declarative(Declarative::VariableDeclaration(syntax_tree)) => self
                .bind_variable_declaration_statement(syntax_tree, handler)
                .map(|_| ()),
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
        handler: &impl HirErrorHandler,
    ) -> Result<AllocaID, BindingError> {
        // gets the type of the variable
        let ty = if let Some(type_annotation) = syntax_tree.type_annotation() {
            // gets the type of the variable
            self.container
                .table
                .resolve_type(
                    self.container.parent_scoped_id,
                    type_annotation.type_specifier(),
                    &self.create_error_handler_adapter(handler),
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
            scope_id: self.stack.current_local().scope_id(),
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
                    address: Address::VariableID(VariableID::AllocaID(alloca_id)),
                    value,
                    span: syntax_tree.span(),
                }
                .into(),
            );

        // insert into stack
        self.stack.current_local_mut().new_variable(
            syntax_tree.identifier().span.str().to_owned(),
            VariableID::AllocaID(alloca_id),
        );

        Ok(alloca_id)
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
        handler: &impl HirErrorHandler,
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
            self.create_error_handler_adapter(handler)
                .recieve(Error::NoAccessibleOverload(NoAccessibleOverload {
                    overload_set_id,
                    symbol_span: syntax_tree.qualified_identifier().span(),
                }));
            return Err(BindingError(syntax_tree.span()));
        }

        Ok(overload_candidates)
    }

    #[allow(clippy::too_many_arguments)]
    fn handle_overload_candidates(
        &mut self,
        arguments: Vec<Value<IntermediateTypeID>>,
        overload_candidates: Vec<OverloadID>,
        has_placeholders: bool,
        syntax_tree: &FunctionCallSyntaxTree,
        overload_set_id: OverloadSetID,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        // must be exactly one overload
        match overload_candidates.len() {
            // no overload matches
            0 => {
                self.create_error_handler_adapter(handler).recieve(
                    Error::NoOverloadWithMatchingArgumentTypes(
                        NoOverloadWithMatchingArgumentTypes {
                            overload_set_id,
                            symbol_span: syntax_tree.qualified_identifier().span(),
                            argument_types: arguments
                                .iter()
                                .map(|a| self.get_inferable_type(a).unwrap())
                                .collect(),
                        },
                    ),
                );

                Err(BindingError(syntax_tree.span()))
            }
            // found the overload
            1 => {
                // unify the argument types with the parameter types
                for (argument, parameter_id) in arguments.iter().zip(
                    self.container
                        .table
                        .get_overload(overload_candidates[0])
                        .unwrap()
                        .parameter_order()
                        .clone()
                        .into_iter(),
                ) {
                    let inference_id = if self
                        .get_inferable_type(argument)
                        .unwrap()
                        .as_constraint()
                        .is_some()
                    {
                        self.get_value_intermediate_type_id(argument)
                            .unwrap()
                            .into_inference_id()
                            .unwrap()
                    } else {
                        continue;
                    };

                    let parameter_ty = self
                        .container
                        .table
                        .get_parameter(parameter_id)
                        .unwrap()
                        .type_binding()
                        .ty;
                    self.inference_context
                        .unify_with_concrete(inference_id, parameter_ty)
                        .expect("unification failed");
                }

                let function_call = FunctionCall {
                    span: syntax_tree.span(),
                    overload_id: overload_candidates[0],
                    arguments,
                };
                let register_id = self.assign_new_register_binding(function_call.into());

                match binding_option.binding_target {
                    BindingTarget::ForValue => {
                        Ok(BindingResult::Value(Value::Register(register_id)))
                    }
                    BindingTarget::ForAddress { .. } => {
                        self.create_error_handler_adapter(handler)
                            .recieve(Error::LValueExpected(crate::hir::error::LValueExpected {
                                expression_span: syntax_tree.span(),
                            }));
                        Err(BindingError(syntax_tree.span()))
                    }
                    BindingTarget::ForStatement => {
                        Ok(BindingResult::None(IntermediateTypeID::Type(
                            self.container
                                .table
                                .get_overload(overload_candidates[0])
                                .unwrap()
                                .return_type(),
                        )))
                    }
                }
            }
            // ambiguous function call
            _ => {
                // since placeholders can match multiple types, it might produce multiple candidates
                if !has_placeholders {
                    self.create_error_handler_adapter(handler).recieve(
                        Error::AmbiguousFunctionCall(AmbiguousFunctionCall {
                            candidate_overloads: overload_candidates,
                            function_call_span: syntax_tree.span(),
                            overload_set_id,
                        }),
                    );
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
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        // resolve the symbol
        let symbol = self.container.table.resolve_symbol(
            self.container.parent_scoped_id,
            syntax_tree.qualified_identifier(),
            &self.create_error_handler_adapter(handler),
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
            self.create_error_handler_adapter(handler).recieve(
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
            self.create_error_handler_adapter(handler).recieve(
                Error::NoOverloadWithMatchingNumberOfArguments(
                    NoOverloadWithMatchingNumberOfArguments {
                        overload_set_id,
                        argument_count: arguments.len(),
                        symbol_span: syntax_tree.qualified_identifier().span(),
                    },
                ),
            );
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
            binding_option,
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
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
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

        let register_id = self.assign_new_register_binding(binding);

        match binding_option.binding_target {
            BindingTarget::ForValue => Ok(BindingResult::Value(Value::Register(register_id))),
            BindingTarget::ForAddress { .. } => {
                self.create_error_handler_adapter(handler)
                    .recieve(Error::LValueExpected(crate::hir::error::LValueExpected {
                        expression_span: syntax_tree.span(),
                    }));
                Err(BindingError(syntax_tree.span()))
            }
            BindingTarget::ForStatement => Ok(BindingResult::None(
                self.get_value_intermediate_type_id(&register_id).unwrap(),
            )),
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the [`NamedSyntaxTree`] into a [`BindingResult`]
    ///
    /// # Errors
    /// - If encounters a fatal semantic error
    #[allow(clippy::too_many_lines)]
    pub fn bind_named(
        &mut self,
        syntax_tree: &NamedSyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
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
            if let Some(variable_id) = self
                .stack
                .locals()
                .iter()
                .rev()
                .find_map(|x| x.lookup_variable(local_ident))
            {
                let address = Address::VariableID(variable_id);
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
                    BindingTarget::ForAddress { is_mutable } => {
                        if is_mutable {
                            let variable_is_mutable = match variable_id {
                                VariableID::AllocaID(alloca_id) => {
                                    self.container.allocas.get(alloca_id).unwrap().is_mutable
                                }
                                VariableID::ParameterID(parameter_id) => {
                                    self.container
                                        .table
                                        .get_parameter(parameter_id)
                                        .unwrap()
                                        .type_binding()
                                        .is_mutable
                                }
                            };

                            if !variable_is_mutable {
                                self.create_error_handler_adapter(handler).recieve(
                                    Error::MutableLValueExpected(MutableLValueExpected {
                                        expression_span: syntax_tree.span(),
                                    }),
                                );
                            }
                        }

                        return Ok(BindingResult::AddressWithSpan(AddressWithSpan {
                            address,
                            span: syntax_tree.span(),
                        }));
                    }
                    BindingTarget::ForStatement => {
                        return Ok(BindingResult::None(
                            self.get_address_intermediate_type_id(&address).unwrap(),
                        ))
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
                &self.create_error_handler_adapter(handler),
            )
            .map_err(|_| BindingError(syntax_tree.span()))?;

        // only enum literal can be used as a value
        let GlobalID::EnumVariant(enum_variant_id) = symbol else {
            self.create_error_handler_adapter(handler).recieve(Error::ValueExpected(ValueExpected {
                expression_span: syntax_tree.span(),
                found_symbol: symbol
            }));

            return Err(BindingError(syntax_tree.span()));
        };

        match binding_option.binding_target {
            BindingTarget::ForValue => Ok(BindingResult::Value(Value::Constant(
                Constant::EnumLiteral(EnumLiteral {
                    span: syntax_tree.span(),
                    enum_variant_id,
                }),
            ))),
            BindingTarget::ForAddress { .. } => {
                self.create_error_handler_adapter(handler)
                    .recieve(Error::LValueExpected(LValueExpected {
                        expression_span: syntax_tree.span(),
                    }));

                Err(BindingError(syntax_tree.span()))
            }
            BindingTarget::ForStatement => Ok(BindingResult::None(IntermediateTypeID::Type(
                Type::TypedID(TypedID::Enum(
                    self.container
                        .table
                        .get_enum_variant(enum_variant_id)
                        .unwrap()
                        .parent_enum_id(),
                )),
            ))),
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    fn get_struct_id(
        &self,
        syntax_tree: &StructLiteralSyntaxTree,
        handler: &impl HirErrorHandler,
    ) -> Result<StructID, BindingError> {
        let found_id = self
            .container
            .table
            .resolve_symbol(
                self.container.parent_scoped_id,
                syntax_tree.qualified_identifier(),
                &self.create_error_handler_adapter(handler),
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
            self.create_error_handler_adapter(handler).recieve(Error::StructExpected(StructExpected {
                found_id,
                symbol_span: syntax_tree.qualified_identifier().span()
            }));
            return Err(BindingError(syntax_tree.span()));
        };

        Ok(struct_id)
    }

    /// Binds the given [`StructLiteralSyntaxTree`] and returns the [`RegisterID`] where the result
    /// is stored.
    #[allow(clippy::too_many_lines, clippy::missing_errors_doc)]
    pub fn bind_struct_literal(
        &mut self,
        syntax_tree: &StructLiteralSyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        // get struct id
        let struct_id = self.get_struct_id(syntax_tree, handler)?;
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

            let struct_sym = self.container.table.get_struct(struct_id).unwrap();
            let Some(field_id) = struct_sym
                .field_ids_by_name()
                .get(initialization.identifier().span.str()).copied() else {
                self.create_error_handler_adapter(handler).recieve(Error::UnknownField(UnknownField {
                    struct_id,
                    field_name_span: initialization.identifier().span.clone(),
                }));
                continue;
            };

            let field_sym = self.container.table.get_field(field_id).unwrap();

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
                self.create_error_handler_adapter(handler)
                    .recieve(Error::FieldInaccessible(FieldInaccessible {
                        field_id,
                        struct_id,
                        field_span: initialization.identifier().span.clone(),
                        current_scope: self.container.parent_scoped_id,
                    }));
            }

            let redefined = if let Some((span, _)) = initializations.get(&field_id) {
                // field initialization duplication
                self.create_error_handler_adapter(handler).recieve(
                    Error::DuplicateFieldInitialization(DuplicateFieldInitialization {
                        duplicate_initialization_span: initialization.span(),
                        previous_initialization_span: span.clone(),
                        struct_id,
                        field_id,
                    }),
                );
                true
            } else {
                false
            };

            if !redefined {
                let field_ty = field_sym.ty();
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
                                ty: IntermediateTypeID::Type(field_ty),
                            }),
                        },
                    ),
                );
                field_initialize_order.push(field_id);
            }
        }

        // check for uninitialized fields
        let mut uninitialized_fields = Vec::new();

        let struct_sym = self.container.table.get_struct(struct_id).unwrap();
        for field in struct_sym.field_order() {
            if !initializations.contains_key(field) {
                uninitialized_fields.push(*field);
            }
        }

        if !uninitialized_fields.is_empty() {
            self.create_error_handler_adapter(handler)
                .recieve(Error::UninitializedFields(UninitializedFields {
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

        let register_id = self.assign_new_register_binding(binding);

        match binding_option.binding_target {
            BindingTarget::ForValue => Ok(BindingResult::Value(Value::Register(register_id))),
            BindingTarget::ForAddress { .. } => {
                self.create_error_handler_adapter(handler)
                    .recieve(Error::LValueExpected(LValueExpected {
                        expression_span: syntax_tree.span(),
                    }));

                Err(BindingError(syntax_tree.span()))
            }
            BindingTarget::ForStatement => Ok(BindingResult::None(IntermediateTypeID::Type(
                Type::TypedID(TypedID::Struct(struct_id)),
            ))),
        }
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
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let operand = self.bind_expression(syntax_tree.operand(), binding_option, handler)?;
        let ty = match &operand {
            BindingResult::Value(value) => self.get_inferable_type(value).unwrap(),
            BindingResult::AddressWithSpan(address) => {
                self.get_address_inferable_type(&address.address).unwrap()
            }
            BindingResult::None(ty) => self.map_intermediate_type_id(*ty).unwrap(),
        };

        // expect the type to be a struct
        let InferableType::Type(Type::TypedID(TypedID::Struct(struct_id))) = ty else {
            self.create_error_handler_adapter(handler).recieve(Error::NoFieldOnType(NoFieldOnType {
                operand_span: syntax_tree.operand().span(),
                operand_type: ty
            }));
            return Err(BindingError(syntax_tree.span()));
        };

        // search for the field
        let struct_sym = self.container.table.get_struct(struct_id).unwrap();

        // search for the field
        let Some(field_id) = struct_sym.field_ids_by_name().get(
            syntax_tree.identifier().span.str()
        ).copied() else {
            self.create_error_handler_adapter(handler).recieve(Error::UnknownField(UnknownField {
                struct_id,
                field_name_span: syntax_tree.identifier().span.clone()
            }));
            return Err(BindingError(syntax_tree.span()));
        };

        let field_sym = self.container.table.get_field(field_id).unwrap();

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
            self.create_error_handler_adapter(handler)
                .recieve(Error::FieldInaccessible(FieldInaccessible {
                    field_id,
                    struct_id,
                    field_span: syntax_tree.identifier().span.clone(),
                    current_scope: self.container.parent_scoped_id,
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
            BindingResult::None(..) => {
                BindingResult::None(IntermediateTypeID::Type(field_sym.ty()))
            }
        })
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`NumericLiteralSyntaxTree`] and returns the [`NumericLiteral`].
    #[allow(clippy::missing_errors_doc)]
    pub fn bind_numeric_literal(
        &mut self,
        syntax_tree: &NumericLiteralSyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
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
                    self.create_error_handler_adapter(handler).recieve(
                        Error::InvalidNumericLiteralSuffix(InvalidNumericLiteralSuffix {
                            suffix_span: suffix.clone(),
                        }),
                    );
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
                self.create_error_handler_adapter(handler).recieve(
                    Error::FloatingPointLiteralHasIntegralSuffix(
                        FloatingPointLiteralHasIntegralSuffix {
                            floating_point_span: syntax_tree.numeric_literal_token().span.clone(),
                        },
                    ),
                );
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

        match binding_option.binding_target {
            BindingTarget::ForValue => {
                // create the literal
                Ok(BindingResult::Value(Value::Constant(
                    Constant::NumericLiteral(NumericLiteral {
                        numeric_literal_syntax_tree: syntax_tree.clone(),
                        ty: type_id,
                    }),
                )))
            }
            BindingTarget::ForAddress { .. } => {
                self.create_error_handler_adapter(handler)
                    .recieve(Error::LValueExpected(LValueExpected {
                        expression_span: syntax_tree.span(),
                    }));
                Err(BindingError(syntax_tree.span()))
            }
            BindingTarget::ForStatement => {
                // no side effects, return the type of the literal
                Ok(BindingResult::None(type_id))
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the give [`BooleanLiteralSyntaxTree`] to a [`BooleanLiteral`].
    #[allow(clippy::missing_errors_doc)]
    pub fn bind_boolean_literal(
        &mut self,
        syntax_tree: &BooleanLiteralSyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let value = match syntax_tree {
            BooleanLiteralSyntaxTree::True(..) => true,
            BooleanLiteralSyntaxTree::False(..) => false,
        };

        match binding_option.binding_target {
            BindingTarget::ForValue => Ok(BindingResult::Value(Value::Constant(
                Constant::BooleanLiteral(BooleanLiteral {
                    span: syntax_tree.span(),
                    value,
                }),
            ))),
            BindingTarget::ForAddress { .. } => {
                self.create_error_handler_adapter(handler)
                    .recieve(Error::LValueExpected(LValueExpected {
                        expression_span: syntax_tree.span(),
                    }));
                Err(BindingError(syntax_tree.span()))
            }
            BindingTarget::ForStatement => {
                // no side effects, return the type of the literal
                Ok(BindingResult::None(IntermediateTypeID::Type(
                    Type::PrimitiveType(PrimitiveType::Bool),
                )))
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    fn as_arithmetic_operator(binary_operator: &BinaryOperatorSyntaxTree) -> ArithmeticOperator {
        match binary_operator {
            BinaryOperatorSyntaxTree::Add(..) => ArithmeticOperator::Add,
            BinaryOperatorSyntaxTree::Subtract(..) => ArithmeticOperator::Subtract,
            BinaryOperatorSyntaxTree::Multiply(..) => ArithmeticOperator::Multiply,
            BinaryOperatorSyntaxTree::Divide(..) => ArithmeticOperator::Divide,
            BinaryOperatorSyntaxTree::Modulo(..) => ArithmeticOperator::Modulo,
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
        handler: &impl HirErrorHandler,
    ) -> Result<(BindingResult, Value<IntermediateTypeID>), BindingError> {
        let left_binding_option = {
            let binding_target = if is_assign {
                BindingTarget::ForAddress { is_mutable: true }
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
            (Ok(left), Err(..)) => {
                let left_ty = self.get_binding_result_intermediate_type_id(&left);
                Ok((
                    left,
                    Value::Placeholder(Placeholder {
                        span: syntax_tree.right_operand().span(),
                        ty: left_ty,
                    }),
                ))
            }
            (Err(..), Ok(right)) => {
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
            (Err(..), Err(..)) => Err(BindingError(syntax_tree.span())),
        }
    }

    fn handle_normal_binary(
        &mut self,
        syntax_tree: &BinarySyntaxTree,
        binary_operator: BinaryOperator,
        inferable_type_check: Option<ExpectedType>,
        handler: &impl HirErrorHandler,
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
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let (lhs, rhs) = self.bind_left_and_right(syntax_tree, true, None, handler)?;

        let address = lhs.into_address_with_span().unwrap();

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
            BindingTarget::ForAddress { .. } => Ok(BindingResult::AddressWithSpan(address)),
            BindingTarget::ForStatement => Ok(BindingResult::None(
                self.get_address_intermediate_type_id(&address.address)
                    .unwrap(),
            )),
        }
    }

    fn handle_compound_arithmetic(
        &mut self,
        syntax_tree: &BinarySyntaxTree,
        binding_option: BindingOption,
        arithmetic_operator: ArithmeticOperator,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let (lhs, rhs) = self.bind_left_and_right(syntax_tree, true, None, handler)?;

        // lhs must be address
        let address = lhs.into_address_with_span().unwrap();

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
            BindingTarget::ForAddress { .. } => Ok(BindingResult::AddressWithSpan(address)),
            BindingTarget::ForStatement => Ok(BindingResult::None(
                self.get_address_intermediate_type_id(&address.address)
                    .unwrap(),
            )),
        }
    }

    #[allow(clippy::too_many_lines)]
    fn handle_short_circuit(
        &mut self,
        syntax_tree: &BinarySyntaxTree,
        handler: &impl HirErrorHandler,
    ) -> Value<IntermediateTypeID> {
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
            .add_conditional_jump_instruction(
                pre_block_id,
                ConditionalJump {
                    condition_value: left.clone(),
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
                },
            )
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
                .add_jump_instruction(
                    rhs_condition_block_id,
                    Jump {
                        jump_target: continue_block_id,
                        jump_source: None,
                    },
                )
                .unwrap();

            rhs_value
        };

        // to the continue block, add phi node and return the result
        self.current_basic_block_id = continue_block_id;

        let values_by_predecessor = {
            let mut values_by_predecessor = HashMap::new();

            if self
                .current_basic_block()
                .predecessors()
                .contains(&pre_block_id)
            {
                values_by_predecessor.insert(pre_block_id, left);
            }

            if self
                .current_basic_block()
                .predecessors()
                .contains(&rhs_condition_block_id)
            {
                values_by_predecessor.insert(rhs_condition_block_id, rhs_value);
            }

            values_by_predecessor
        };

        match values_by_predecessor.len() {
            0 => Value::Unreachable(Unreachable {
                span: syntax_tree.span(),
                ty: IntermediateTypeID::Type(Type::PrimitiveType(PrimitiveType::Bool)),
            }),
            1 => values_by_predecessor.into_values().next().unwrap(),
            2 => Value::Register(self.assign_new_register_binding(Binding::PhiNode(PhiNode {
                span: syntax_tree.span(),
                values_by_predecessor,
                phi_node_source: PhiNodeSource::LogicalShortCircuit,
            }))),
            _ => unreachable!(),
        }
    }

    /// Binds the [`NamedSyntaxTree`] into a [`BindingResult`]
    ///
    /// # Errors
    /// - If encounters a fatal semantic error
    pub fn bind_binary(
        &mut self,
        syntax_tree: &BinarySyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
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
                Ok(BindingResult::Value(
                    self.handle_short_circuit(syntax_tree, handler),
                ))
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`BlockSyntaxTree`] into its cooresponding instructions and returns its
    /// value.
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_block(
        &mut self,
        syntax_tree: &BlockSyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let scope_id = self.container.scope_tree.scopes.insert(Scope {
            parent_scope: Some(self.current_scope().id()),
            branch: None,
            children: Vec::new(),
            depth: self.current_scope().depth + 1,
        });
        self.current_scope_mut()
            .children
            .push(ScopeChildID::ScopeID(scope_id));
        let value = self.bind_block_internal(syntax_tree, scope_id, handler);

        match binding_option.binding_target {
            BindingTarget::ForValue => Ok(BindingResult::Value(value)),
            BindingTarget::ForAddress { .. } => {
                self.create_error_handler_adapter(handler)
                    .recieve(Error::LValueExpected(LValueExpected {
                        expression_span: syntax_tree.span(),
                    }));
                Err(BindingError(syntax_tree.span()))
            }
            BindingTarget::ForStatement => Ok(BindingResult::None(
                self.get_value_intermediate_type_id(&value).unwrap(),
            )),
        }
    }

    #[allow(clippy::too_many_lines)]
    fn bind_block_internal(
        &mut self,
        syntax_tree: &BlockSyntaxTree,
        scope_id: ScopeID,
        handler: &impl HirErrorHandler,
    ) -> Value<IntermediateTypeID> {
        // allocate a successor block
        let continue_basic_block_id = self.container.control_flow_graph.new_basic_block();

        // create new block id
        let label = syntax_tree
            .label_specifier()
            .as_ref()
            .map(|x| x.label().identifier().span.str().to_owned());

        self.block_scopes_by_scope_id.insert(
            scope_id,
            BlockScope {
                label,
                incoming_values: HashMap::new(),
                continue_basic_block_id,
                express_ty: None,
            },
        );

        // pushes a new scope
        self.stack.push(scope_id);
        self.current_basic_block_mut()
            .add_basic_instruction(Basic::ScopePush(ScopePush { scope_id }));

        // binds list of statements
        for statement in syntax_tree.block_without_label().statements() {
            let _: Result<(), BindingError> = self.bind_statement(statement, handler);
        }

        // ends a scope
        self.stack.pop();
        self.current_basic_block_mut()
            .add_basic_instruction(Basic::ScopePop(ScopePop { scope_id }));

        self.container
            .control_flow_graph
            .add_jump_instruction(
                self.current_basic_block_id,
                Jump {
                    jump_target: continue_basic_block_id,
                    jump_source: None,
                },
            )
            .unwrap();

        // set the current basic block id to the successor block
        self.current_basic_block_id = continue_basic_block_id;

        let mut block_scope = self.block_scopes_by_scope_id.remove(&scope_id).unwrap();

        {
            // if `block_scope.express_ty` has a value, check if there are any paths that do not
            // express the value

            if let Some(ty) = block_scope.express_ty {
                let mut missing_value_basic_blocks = Vec::new();

                for predecessor in self.current_basic_block().predecessors() {
                    if !block_scope.incoming_values.contains_key(predecessor) {
                        missing_value_basic_blocks.push(*predecessor);
                    }
                }

                // add place holder values for missing values
                for missing_value_basic_block in &missing_value_basic_blocks {
                    block_scope.incoming_values.insert(
                        *missing_value_basic_block,
                        Value::Placeholder(Placeholder {
                            span: syntax_tree.span(),
                            ty,
                        }),
                    );
                }

                if !missing_value_basic_blocks.is_empty() {
                    self.create_error_handler_adapter(handler).recieve(
                        Error::NotAllFlowPathExpressValue(NotAllFlowPathExpressValue {
                            block_span: syntax_tree.span(),
                            missing_value_basic_blocks,
                        }),
                    );
                }

                // based on the number of incoming values, create a phi node or return the value
                match block_scope.incoming_values.len() {
                    0 => Value::Unreachable(Unreachable {
                        span: syntax_tree.span(),
                        ty,
                    }),
                    1 => block_scope.incoming_values.into_values().next().unwrap(),
                    _ => Value::Register(self.assign_new_register_binding(Binding::PhiNode(
                        PhiNode {
                            span: syntax_tree.span(),
                            values_by_predecessor: block_scope.incoming_values,
                            phi_node_source: PhiNodeSource::Block,
                        },
                    ))),
                }
            } else if self.current_basic_block().predecessors().is_empty() {
                Value::Unreachable(Unreachable {
                    span: syntax_tree.span(),
                    ty: IntermediateTypeID::InferenceID(
                        self.inference_context.new_inference(Constraint::All),
                    ),
                })
            } else {
                Value::Constant(Constant::VoidConstant(VoidConstant {
                    span: syntax_tree.span(),
                }))
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    fn get_block_scope_id(
        &self,
        syntax_tree: &ExpressSyntaxTree,
        handler: &impl HirErrorHandler,
    ) -> Option<ScopeID> {
        // gets the target block
        syntax_tree.label().as_ref().map_or_else(
            || {
                // no label, gets the nearest block id
                let scope_id = 'scope_id: {
                    for local in self.stack.locals().iter().rev() {
                        if self
                            .block_scopes_by_scope_id
                            .contains_key(&local.scope_id())
                        {
                            break 'scope_id Some(local.scope_id());
                        }
                    }

                    None
                };

                if scope_id.is_none() {
                    self.create_error_handler_adapter(handler)
                        .recieve(Error::ExpressOutsideBlock(ExpressOutsideBlock {
                            express_span: syntax_tree.span(),
                        }));
                }

                scope_id
            },
            |label| {
                let scope_id = 'scope_id: {
                    for local in self.stack.locals().iter().rev() {
                        if let Some(block) = self.block_scopes_by_scope_id.get(&local.scope_id()) {
                            let Some(local_label) = block.label.as_ref() else {
                                continue;
                            };

                            if label.identifier().span.str() == local_label {
                                break 'scope_id Some(local.scope_id());
                            }
                        }
                    }

                    None
                };

                if scope_id.is_none() {
                    self.create_error_handler_adapter(handler).recieve(
                        Error::NoBlockWithGivenLabelFound(NoBlockWithGivenLabelFound {
                            label_span: label.identifier().span.clone(),
                        }),
                    );
                }

                scope_id
            },
        )
    }

    fn handle_express_value(
        &mut self,
        syntax_tree: &ExpressSyntaxTree,
        scope_id: ScopeID,
        value: Option<Result<BindingResult, BindingError>>,
        handler: &impl HirErrorHandler,
    ) -> Result<Value<IntermediateTypeID>, BindingError> {
        let current_express_ty = self
            .block_scopes_by_scope_id
            .get(&scope_id)
            .unwrap()
            .express_ty;

        Ok(match (current_express_ty, value) {
            // the block has type of void
            (None, None) => {
                self.block_scopes_by_scope_id
                    .get_mut(&scope_id)
                    .unwrap()
                    .express_ty = Some(IntermediateTypeID::Type(Type::PrimitiveType(
                    PrimitiveType::Void,
                )));

                Value::Constant(Constant::VoidConstant(VoidConstant {
                    span: syntax_tree.span(),
                }))
            }
            // assigns new type to the block
            (None, Some(result)) => {
                let value = result?.into_value().unwrap();
                self.block_scopes_by_scope_id
                    .get_mut(&scope_id)
                    .unwrap()
                    .express_ty = Some(self.get_value_intermediate_type_id(&value).unwrap());

                value
            }
            // the block's type should be void
            (Some(current), None) => self
                .type_check(
                    syntax_tree.express_keyword().span(),
                    IntermediateTypeID::Type(Type::PrimitiveType(PrimitiveType::Void)),
                    current.into(),
                    handler,
                )
                .map_or_else(
                    |err| {
                        Value::Placeholder(Placeholder {
                            span: err.0,
                            ty: current,
                        })
                    },
                    |_| {
                        Value::Constant(Constant::VoidConstant(VoidConstant {
                            span: syntax_tree.span(),
                        }))
                    },
                ),
            // unifies the block's type and value's type
            (Some(current), Some(result)) => {
                let value = result.map_or_else(
                    |x| {
                        Value::Placeholder(Placeholder {
                            span: x.0,
                            ty: current,
                        })
                    },
                    |x| x.into_value().unwrap(),
                );

                self.type_check(
                    self.get_span(&value).unwrap(),
                    self.get_value_intermediate_type_id(&value).unwrap(),
                    current.into(),
                    handler,
                )
                .map_or_else(
                    |err| {
                        Value::Placeholder(Placeholder {
                            span: err.0,
                            ty: current,
                        })
                    },
                    |_| value,
                )
            }
        })
    }

    /// Binds the given [`ExpressionSyntaxTree`] into its cooresponding instruction and returns the
    /// [`Unreachable`] value of it.
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_express(
        &mut self,
        syntax_tree: &ExpressSyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let target_block_id = self.get_block_scope_id(syntax_tree, handler);

        let value = syntax_tree
            .expression()
            .as_ref()
            .map(|x| self.bind_expression(x, BindingOption::default(), handler));

        // extract the block value
        let Some(target_scope_id) = target_block_id else {
            return Err(BindingError(syntax_tree.span()))
        };

        // the number of scope to pop
        let scope_pop_count = (self.current_scope().depth
            - self
                .container
                .scope_tree
                .scopes
                .get(target_scope_id)
                .unwrap()
                .depth)
            + 1;

        let value = self.handle_express_value(syntax_tree, target_scope_id, value, handler)?;

        // insert a jump instruction and scope pop instruction
        for local in self.stack.locals().iter().rev().take(scope_pop_count) {
            self.container
                .control_flow_graph
                .get_mut(self.current_basic_block_id)
                .unwrap()
                .add_basic_instruction(Basic::ScopePop(ScopePop {
                    scope_id: local.scope_id(),
                }));
        }

        let current_is_terminated = self.current_basic_block().is_terminated();

        let block = self
            .block_scopes_by_scope_id
            .get_mut(&target_scope_id)
            .unwrap();

        self.container
            .control_flow_graph
            .add_jump_instruction(
                self.current_basic_block_id,
                Jump {
                    jump_target: block.continue_basic_block_id,
                    jump_source: Some(JumpSource::Express(syntax_tree.span())),
                },
            )
            .unwrap();

        // in the end basic block, if the it has a predecessor to current basic block (which means
        // this `express` is reachable), then insert an incoming value to the phi node

        if !current_is_terminated {
            block
                .incoming_values
                .entry(self.current_basic_block_id)
                .or_insert_with(|| value);
        }

        let value = Unreachable {
            span: syntax_tree.span(),
            ty: IntermediateTypeID::InferenceID(
                self.inference_context.new_inference(Constraint::All),
            ),
        };

        match binding_option.binding_target {
            BindingTarget::ForValue => Ok(BindingResult::Value(Value::Unreachable(value))),
            BindingTarget::ForAddress { .. } => {
                self.create_error_handler_adapter(handler)
                    .recieve(Error::LValueExpected(LValueExpected {
                        expression_span: syntax_tree.span(),
                    }));
                Err(BindingError(syntax_tree.span()))
            }
            BindingTarget::ForStatement => Ok(BindingResult::None(value.ty)),
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`ReturnSyntaxTree`] into its cooresponding instruction and returns the
    /// [`Unreachable`] void value.
    ///
    /// # Errors
    /// - If encountered a fatal semantic error while binding the return value.
    pub fn bind_return(
        &mut self,
        syntax_tree: &ReturnSyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let return_type = self
            .container
            .table()
            .get_overload(self.container.overload_id)
            .unwrap()
            .return_type();

        let return_value = if let Some(expression) = syntax_tree.expression().as_ref() {
            Some(
                self.expect_expression(
                    expression,
                    BindingOption::default(),
                    ExpectedType::Type(return_type),
                    handler,
                )
                .map_or_else(
                    |err| {
                        Value::Placeholder(Placeholder {
                            span: err.0,
                            ty: IntermediateTypeID::Type(return_type),
                        })
                    },
                    |x| x.into_value().unwrap(),
                ),
            )
        } else {
            if self
                .container
                .table()
                .get_overload(self.container.overload_id)
                .unwrap()
                .return_type()
                != Type::PrimitiveType(PrimitiveType::Void)
            {
                // expect a return value
                self.create_error_handler_adapter(handler)
                    .recieve(Error::ReturnValueExpected(ReturnValueExpected {
                        return_span: syntax_tree.span(),
                    }));
            }

            None
        };

        // pop all the scopes
        for local in self.stack.locals().iter().rev() {
            self.container
                .control_flow_graph
                .get_mut(self.current_basic_block_id)
                .unwrap()
                .add_basic_instruction(Basic::ScopePop(ScopePop {
                    scope_id: local.scope_id(),
                }));
        }
        self.current_basic_block_mut()
            .add_return_instruction(Return { return_value });

        let value = Unreachable {
            span: syntax_tree.span(),
            ty: IntermediateTypeID::InferenceID(
                self.inference_context.new_inference(Constraint::All),
            ),
        };

        match binding_option.binding_target {
            BindingTarget::ForValue => Ok(BindingResult::Value(Value::Unreachable(value))),
            BindingTarget::ForAddress { .. } => {
                self.create_error_handler_adapter(handler)
                    .recieve(Error::LValueExpected(LValueExpected {
                        expression_span: syntax_tree.span(),
                    }));
                Err(BindingError(syntax_tree.span()))
            }
            BindingTarget::ForStatement => Ok(BindingResult::None(value.ty)),
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    fn handle_then_else_values(
        &mut self,
        syntax_tree: &IfElseSyntaxTree,
        (then_value, then_end_basic_block_id): (Value<IntermediateTypeID>, BasicBlockID),
        (else_value, else_end_basic_block_id): (Value<IntermediateTypeID>, BasicBlockID),
    ) -> Value<IntermediateTypeID> {
        // merge the two path
        match (then_value, else_value) {
            // unreachable
            (Value::Unreachable(then_value), Value::Unreachable(..)) => {
                Value::Unreachable(Unreachable {
                    span: syntax_tree.span(),
                    ty: (self.get_value_intermediate_type_id(&then_value).unwrap()),
                })
            }

            // then value only
            (then_value, Value::Unreachable(..)) => then_value,

            // else value only
            (Value::Unreachable(..), else_value) => else_value,

            // phi node
            (then_value, else_value) => {
                Value::Register(self.assign_new_register_binding(Binding::PhiNode(PhiNode {
                    span: syntax_tree.span(),
                    values_by_predecessor: {
                        let mut result = HashMap::new();
                        result.insert(then_end_basic_block_id, then_value);
                        result.insert(else_end_basic_block_id, else_value);
                        result
                    },
                    phi_node_source: PhiNodeSource::IfEsle,
                })))
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn bind_if_else_internal(
        &mut self,
        syntax_tree: &IfElseSyntaxTree,
        handler: &impl HirErrorHandler,
    ) -> Value<IntermediateTypeID> {
        let pre_basic_block_id = self.current_basic_block_id;
        let true_basic_block_id = self.container.control_flow_graph.new_basic_block();
        let false_basic_block_id = self.container.control_flow_graph.new_basic_block();
        let continue_basic_block_id = self.container.control_flow_graph.new_basic_block();

        let condition = self
            .expect_expression(
                syntax_tree.condition(),
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

        // add conditional jump expression
        self.container
            .control_flow_graph
            .add_conditional_jump_instruction(
                pre_basic_block_id,
                ConditionalJump {
                    condition_value: condition,
                    true_jump_target: true_basic_block_id,
                    false_jump_target: false_basic_block_id,
                },
            )
            .unwrap();

        let branch_id = self.container.scope_tree.branches.insert(Branch {
            parent_scope: self.current_scope().id(),
            scopes: Vec::new(),
        });

        let true_scope_id = self.container.scope_tree.scopes.insert(Scope {
            parent_scope: Some(self.current_scope().id()),
            branch: Some(branch_id),
            children: Vec::new(),
            depth: self.current_scope().depth + 1,
        });
        let false_scope_id = self.container.scope_tree.scopes.insert(Scope {
            parent_scope: Some(self.current_scope().id()),
            branch: Some(branch_id),
            children: Vec::new(),
            depth: self.current_scope().depth + 1,
        });

        self.container
            .scope_tree
            .branches
            .get_mut(branch_id)
            .unwrap()
            .scopes
            .extend([true_scope_id, false_scope_id]);

        // bind true block
        let (then_value, then_end_basic_block_id) = {
            self.current_basic_block_id = true_basic_block_id;
            let value =
                self.bind_block_internal(syntax_tree.then_expression(), true_scope_id, handler);

            // jump to continue block
            self.container
                .control_flow_graph
                .add_jump_instruction(
                    self.current_basic_block_id,
                    Jump {
                        jump_target: continue_basic_block_id,
                        jump_source: None,
                    },
                )
                .unwrap();

            (value, self.current_basic_block_id)
        };

        // bind false block
        let (mut else_value, else_end_basic_block_id) = {
            self.current_basic_block_id = false_basic_block_id;

            let value = match syntax_tree.else_expression() {
                Some(else_expression) => match else_expression.expression().as_ref() {
                    BlockOrIfElse::Block(block) => {
                        self.bind_block_internal(block, false_scope_id, handler)
                    }
                    BlockOrIfElse::IfElse(if_else) => {
                        self.current_basic_block_mut()
                            .add_basic_instruction(Basic::ScopePush(ScopePush {
                                scope_id: false_scope_id,
                            }));

                        let value = self.bind_if_else_internal(if_else, handler);

                        self.current_basic_block_mut()
                            .add_basic_instruction(Basic::ScopePop(ScopePop {
                                scope_id: false_scope_id,
                            }));

                        value
                    }
                },
                None => {
                    self.current_basic_block_mut()
                        .add_basic_instruction(Basic::ScopePush(ScopePush {
                            scope_id: false_scope_id,
                        }));

                    self.current_basic_block_mut()
                        .add_basic_instruction(Basic::ScopePop(ScopePop {
                            scope_id: false_scope_id,
                        }));

                    Value::Constant(Constant::VoidConstant(VoidConstant {
                        span: syntax_tree.span(),
                    }))
                }
            };

            // jump to continue block
            self.container
                .control_flow_graph
                .add_jump_instruction(
                    self.current_basic_block_id,
                    Jump {
                        jump_target: continue_basic_block_id,
                        jump_source: None,
                    },
                )
                .unwrap();

            (value, self.current_basic_block_id)
        };

        self.current_basic_block_id = continue_basic_block_id;

        {
            // unify the two values
            let then_value_ty = self.get_value_intermediate_type_id(&then_value).unwrap();
            if let Err(err) = self.type_check(
                self.get_span(&else_value).unwrap(),
                self.get_value_intermediate_type_id(&else_value).unwrap(),
                then_value_ty.into(),
                handler,
            ) {
                else_value = Value::Placeholder(Placeholder {
                    span: err.0,
                    ty: then_value_ty,
                });
            }
        }

        self.handle_then_else_values(
            syntax_tree,
            (then_value, then_end_basic_block_id),
            (else_value, else_end_basic_block_id),
        )
    }

    /// Binds the given [`IfElseSyntaxTree`] into its cooresponding instructions and returns its
    /// value.
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_if_else(
        &mut self,
        syntax_tree: &IfElseSyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let value = self.bind_if_else_internal(syntax_tree, handler);

        match binding_option.binding_target {
            BindingTarget::ForValue => Ok(BindingResult::Value(value)),
            BindingTarget::ForAddress { .. } => {
                self.create_error_handler_adapter(handler)
                    .recieve(Error::LValueExpected(LValueExpected {
                        expression_span: syntax_tree.span(),
                    }));

                Err(BindingError(syntax_tree.span()))
            }
            BindingTarget::ForStatement => Ok(BindingResult::None(
                self.get_value_intermediate_type_id(&value).unwrap(),
            )),
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`LoopSyntaxTree`] into its cooresponding instructions and returns its
    /// value.
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_loop(
        &mut self,
        syntax_tree: &LoopSyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let pre_basic_block_id = self.current_basic_block_id;
        let loop_header_basic_block = self.container.control_flow_graph.new_basic_block();
        let loop_exit_basic_block = self.container.control_flow_graph.new_basic_block();

        // loop scope
        let label = syntax_tree
            .label_specifier()
            .as_ref()
            .map(|x| x.label().identifier().span.str().to_owned());
        let scope_id = self.container.scope_tree.scopes.insert(Scope {
            parent_scope: Some(self.current_scope().id()),
            branch: None,
            children: Vec::new(),
            depth: self.current_scope().depth + 1,
        });
        self.loop_scopes_by_scope_id.insert(
            scope_id,
            LoopScope {
                label,
                incoming_values: HashMap::new(),
                loop_header_basic_block,
                loop_exit_basic_block,
                break_ty: None,
            },
        );

        // enter the loop body
        self.container
            .control_flow_graph
            .add_jump_instruction(
                pre_basic_block_id,
                Jump {
                    jump_target: loop_header_basic_block,
                    jump_source: None,
                },
            )
            .unwrap();
        self.current_basic_block_id = loop_header_basic_block;

        self.current_basic_block_mut()
            .add_basic_instruction(Basic::ScopePush(ScopePush { scope_id }));

        // push loop scope
        self.stack.push(scope_id);

        // binds all the statements
        for statement in syntax_tree.block_without_label().statements() {
            let _: Result<(), BindingError> = self.bind_statement(statement, handler);
        }

        // pop loop scope
        self.stack.pop();

        self.current_basic_block_mut()
            .add_basic_instruction(Basic::ScopePop(ScopePop { scope_id }));

        // go back to the loop header
        self.container
            .control_flow_graph
            .add_jump_instruction(
                self.current_basic_block_id,
                Jump {
                    jump_target: loop_header_basic_block,
                    jump_source: None,
                },
            )
            .unwrap();

        let loop_scope = self.loop_scopes_by_scope_id.remove(&scope_id).unwrap();
        self.current_basic_block_id = loop_exit_basic_block;

        // bind the target value
        let value = if let Some(ty) = loop_scope.break_ty {
            match loop_scope.incoming_values.len() {
                0 => Value::Unreachable(Unreachable {
                    span: syntax_tree.span(),
                    ty,
                }),
                1 => loop_scope.incoming_values.into_values().next().unwrap(),
                _ => Value::Register(self.assign_new_register_binding(Binding::PhiNode(PhiNode {
                    span: syntax_tree.span(),
                    values_by_predecessor: loop_scope.incoming_values,
                    phi_node_source: PhiNodeSource::Loop,
                }))),
            }
        } else {
            Value::Unreachable(Unreachable {
                span: syntax_tree.span(),
                ty: IntermediateTypeID::InferenceID(
                    self.inference_context.new_inference(Constraint::All),
                ),
            })
        };

        match binding_option.binding_target {
            BindingTarget::ForValue => Ok(BindingResult::Value(value)),
            BindingTarget::ForAddress { .. } => {
                self.create_error_handler_adapter(handler)
                    .recieve(Error::LValueExpected(LValueExpected {
                        expression_span: syntax_tree.span(),
                    }));
                Err(BindingError(syntax_tree.span()))
            }
            BindingTarget::ForStatement => Ok(BindingResult::None(
                self.get_value_intermediate_type_id(&value).unwrap(),
            )),
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    fn get_loop_scope_id(
        &self,
        syntax_tree_span: Span,
        label: &Option<Label>,
        handler: &impl HirErrorHandler,
    ) -> Option<ScopeID> {
        // gets the target block
        label.as_ref().map_or_else(
            || {
                // no label, gets the nearest block id
                let scope_id = 'scope_id: {
                    for local in self.stack.locals().iter().rev() {
                        if self.loop_scopes_by_scope_id.contains_key(&local.scope_id()) {
                            break 'scope_id Some(local.scope_id());
                        }
                    }

                    None
                };

                if scope_id.is_none() {
                    self.create_error_handler_adapter(handler).recieve(
                        Error::LoopControlExressionOutsideLoop(
                            super::error::LoopControlExressionOutsideLoop {
                                loop_control_span: syntax_tree_span,
                            },
                        ),
                    );
                }

                scope_id
            },
            |label| {
                let scope_id = 'scope_id: {
                    for local in self.stack.locals().iter().rev() {
                        if let Some(loop_scope) =
                            self.loop_scopes_by_scope_id.get(&local.scope_id())
                        {
                            let Some(local_label) = loop_scope.label.as_ref() else {
                                continue;
                            };

                            if label.identifier().span.str() == local_label {
                                break 'scope_id Some(local.scope_id());
                            }
                        }
                    }

                    None
                };

                if scope_id.is_none() {
                    self.create_error_handler_adapter(handler).recieve(
                        Error::NoBlockWithGivenLabelFound(NoBlockWithGivenLabelFound {
                            label_span: label.identifier().span.clone(),
                        }),
                    );
                }

                scope_id
            },
        )
    }

    /// Binds the given [`ContinueSyntaxTree`] into its cooresponding instructions and returns its
    /// value.
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_continue(
        &mut self,
        syntax_tree: &ContinueSyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let target_block_id =
            self.get_loop_scope_id(syntax_tree.span(), syntax_tree.label(), handler);

        // extract the block value
        let Some(target_scope_id) = target_block_id else {
            return Err(BindingError(syntax_tree.span()))
        };

        // the number of scope to pop
        let scope_pop_count = (self.current_scope().depth
            - self
                .container
                .scope_tree
                .scopes
                .get(target_scope_id)
                .unwrap()
                .depth)
            + 1;

        // insert a jump instruction and scope pop instruction
        for local in self.stack.locals().iter().rev().take(scope_pop_count) {
            self.container
                .control_flow_graph
                .get_mut(self.current_basic_block_id)
                .unwrap()
                .add_basic_instruction(Basic::ScopePop(ScopePop {
                    scope_id: local.scope_id(),
                }));
        }

        self.container
            .control_flow_graph
            .add_jump_instruction(
                self.current_basic_block_id,
                Jump {
                    jump_target: self
                        .loop_scopes_by_scope_id
                        .get(&target_scope_id)
                        .unwrap()
                        .loop_header_basic_block,
                    jump_source: Some(JumpSource::Continue(syntax_tree.span())),
                },
            )
            .unwrap();

        let value = Unreachable {
            span: syntax_tree.span(),
            ty: IntermediateTypeID::InferenceID(
                self.inference_context.new_inference(Constraint::All),
            ),
        };

        match binding_option.binding_target {
            BindingTarget::ForValue => Ok(BindingResult::Value(Value::Unreachable(value))),
            BindingTarget::ForAddress { .. } => {
                self.create_error_handler_adapter(handler)
                    .recieve(Error::LValueExpected(LValueExpected {
                        expression_span: syntax_tree.span(),
                    }));
                Err(BindingError(syntax_tree.span()))
            }
            BindingTarget::ForStatement => Ok(BindingResult::None(value.ty)),
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    fn handle_break_value(
        &mut self,
        syntax_tree: &BreakSyntaxTree,
        scope_id: ScopeID,
        value: Option<Result<BindingResult, BindingError>>,
        handler: &impl HirErrorHandler,
    ) -> Result<Value<IntermediateTypeID>, BindingError> {
        let current_express_ty = self
            .loop_scopes_by_scope_id
            .get(&scope_id)
            .unwrap()
            .break_ty;

        Ok(match (current_express_ty, value) {
            // the block has type of void
            (None, None) => {
                self.loop_scopes_by_scope_id
                    .get_mut(&scope_id)
                    .unwrap()
                    .break_ty = Some(IntermediateTypeID::Type(Type::PrimitiveType(
                    PrimitiveType::Void,
                )));

                Value::Constant(Constant::VoidConstant(VoidConstant {
                    span: syntax_tree.span(),
                }))
            }
            // assigns new type to the block
            (None, Some(result)) => {
                let value = result?.into_value().unwrap();
                self.loop_scopes_by_scope_id
                    .get_mut(&scope_id)
                    .unwrap()
                    .break_ty = Some(self.get_value_intermediate_type_id(&value).unwrap());

                value
            }
            // the block's type should be void
            (Some(current), None) => self
                .type_check(
                    syntax_tree.break_keyword().span(),
                    IntermediateTypeID::Type(Type::PrimitiveType(PrimitiveType::Void)),
                    current.into(),
                    handler,
                )
                .map_or_else(
                    |err| {
                        Value::Placeholder(Placeholder {
                            span: err.0,
                            ty: current,
                        })
                    },
                    |_| {
                        Value::Constant(Constant::VoidConstant(VoidConstant {
                            span: syntax_tree.span(),
                        }))
                    },
                ),
            // unifies the block's type and value's type
            (Some(current), Some(result)) => {
                let value = result.map_or_else(
                    |x| {
                        Value::Placeholder(Placeholder {
                            span: x.0,
                            ty: current,
                        })
                    },
                    |x| x.into_value().unwrap(),
                );

                self.type_check(
                    self.get_span(&value).unwrap(),
                    self.get_value_intermediate_type_id(&value).unwrap(),
                    current.into(),
                    handler,
                )
                .map_or_else(
                    |err| {
                        Value::Placeholder(Placeholder {
                            span: err.0,
                            ty: current,
                        })
                    },
                    |_| value,
                )
            }
        })
    }

    /// Binds the given [`BreakSyntaxTree`] into its cooresponding instruction and returns the
    /// [`Unreachable`] value of it.
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_break(
        &mut self,
        syntax_tree: &BreakSyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let target_block_id =
            self.get_loop_scope_id(syntax_tree.span(), syntax_tree.label(), handler);

        let value = syntax_tree
            .expression()
            .as_ref()
            .map(|x| self.bind_expression(x, BindingOption::default(), handler));

        // extract the block value
        let Some(target_scope_id) = target_block_id else {
            return Err(BindingError(syntax_tree.span()))
        };

        // the number of scope to pop
        let scope_pop_count = (self.current_scope().depth
            - self
                .container
                .scope_tree
                .scopes
                .get(target_scope_id)
                .unwrap()
                .depth)
            + 1;

        let value = self.handle_break_value(syntax_tree, target_scope_id, value, handler)?;

        // insert a jump instruction and scope pop instruction
        for local in self.stack.locals().iter().rev().take(scope_pop_count) {
            self.container
                .control_flow_graph
                .get_mut(self.current_basic_block_id)
                .unwrap()
                .add_basic_instruction(Basic::ScopePop(ScopePop {
                    scope_id: local.scope_id(),
                }));
        }

        let current_is_terminated = self.current_basic_block().is_terminated();

        let loop_scope = self
            .loop_scopes_by_scope_id
            .get_mut(&target_scope_id)
            .unwrap();

        self.container
            .control_flow_graph
            .add_jump_instruction(
                self.current_basic_block_id,
                Jump {
                    jump_target: loop_scope.loop_exit_basic_block,
                    jump_source: Some(JumpSource::Break(syntax_tree.span())),
                },
            )
            .unwrap();

        // in the end basic block, if the it has a predecessor to current basic block (which means
        // this `express` is reachable), then insert an incoming value to the phi node

        if !current_is_terminated {
            loop_scope
                .incoming_values
                .entry(self.current_basic_block_id)
                .or_insert_with(|| value);
        }

        let value = Unreachable {
            span: syntax_tree.span(),
            ty: IntermediateTypeID::InferenceID(
                self.inference_context.new_inference(Constraint::All),
            ),
        };

        match binding_option.binding_target {
            BindingTarget::ForValue => Ok(BindingResult::Value(Value::Unreachable(value))),
            BindingTarget::ForAddress { .. } => {
                self.create_error_handler_adapter(handler)
                    .recieve(Error::LValueExpected(LValueExpected {
                        expression_span: syntax_tree.span(),
                    }));
                Err(BindingError(syntax_tree.span()))
            }
            BindingTarget::ForStatement => Ok(BindingResult::None(value.ty)),
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Binds the [`CastSyntaxTree`] into a [`BindingResult`]
    ///
    /// # Errors
    /// - If encounters a fatal semantic error
    pub fn bind_cast(
        &mut self,
        syntax_tree: &CastSyntaxTree,
        binding_option: BindingOption,
        handler: &impl HirErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        let value = self
            .bind_expression(syntax_tree.operand(), BindingOption::default(), handler)?
            .into_value()
            .unwrap();

        // currently, the language only supports arithmetic type conversion
        let target_type = self
            .container
            .table
            .resolve_type(
                self.container.parent_scoped_id,
                syntax_tree.type_specifier(),
                handler,
            )
            .map_err(|x| match x {
                ResolveError::FatalSymbolError => BindingError(syntax_tree.span()),
                ResolveError::InvalidIDError(_) => unreachable!(),
            })?;

        // check if the target type is number type
        let target_type_is_number = matches!(
            target_type,
            Type::PrimitiveType(
                PrimitiveType::Int8
                    | PrimitiveType::Int16
                    | PrimitiveType::Int32
                    | PrimitiveType::Int64
                    | PrimitiveType::Uint8
                    | PrimitiveType::Uint16
                    | PrimitiveType::Uint32
                    | PrimitiveType::Uint64
                    | PrimitiveType::Float32
                    | PrimitiveType::Float64
            )
        );

        let value_type = self.get_inferable_type(&value).unwrap();

        let value_type_is_number = match value_type {
            InferableType::Type(ty) => matches!(
                ty,
                Type::PrimitiveType(
                    PrimitiveType::Int8
                        | PrimitiveType::Int16
                        | PrimitiveType::Int32
                        | PrimitiveType::Int64
                        | PrimitiveType::Uint8
                        | PrimitiveType::Uint16
                        | PrimitiveType::Uint32
                        | PrimitiveType::Uint64
                        | PrimitiveType::Float32
                        | PrimitiveType::Float64
                )
            ),
            InferableType::Constraint(constraint) => {
                matches!(
                    constraint,
                    Constraint::Float | Constraint::Number | Constraint::Signed
                )
            }
        };

        if value_type_is_number && target_type_is_number {
            let register_id = self.assign_new_register_binding(Binding::Cast(Cast {
                operand: value,
                target_type: IntermediateTypeID::Type(target_type),
                span: syntax_tree.span(),
            }));

            match binding_option.binding_target {
                BindingTarget::ForValue => Ok(BindingResult::Value(Value::Register(register_id))),
                BindingTarget::ForAddress { .. } => {
                    self.create_error_handler_adapter(handler)
                        .recieve(Error::LValueExpected(LValueExpected {
                            expression_span: syntax_tree.span(),
                        }));
                    Err(BindingError(syntax_tree.span()))
                }
                BindingTarget::ForStatement => {
                    Ok(BindingResult::None(IntermediateTypeID::Type(target_type)))
                }
            }
        } else {
            self.create_error_handler_adapter(handler)
                .recieve(Error::NoCastAvailable(super::error::NoCastAvailable {
                    cast_span: syntax_tree.span(),
                    expression_type: value_type,
                    cast_type: InferableType::Type(target_type),
                }));

            Err(BindingError(syntax_tree.span()))
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////
}

impl Binder {
    ///////////////////////////////////////////////////////////////////////////////////////////////

    /// Finalizes the binding process and returns the resulting [`Hir`] tree.
    ///
    /// # Errors
    /// Returns a [`Suboptimal`] HIR instead if there were any semantic errors during the binding
    pub fn build(mut self, _handler: &impl HirErrorHandler) -> Result<Hir, Box<Suboptimal>> {
        // should have no scopes left
        assert!(self.stack.locals().len() == 1);

        // early return if suboptimal
        if *self.suboptimal.read().unwrap() {
            return Err(Box::new(Suboptimal {
                container: self.container,
                inference_context: self.inference_context,
            }));
        }

        // pop the last stack frame
        self.container
            .control_flow_graph
            .get_mut(self.current_basic_block_id)
            .unwrap()
            .add_basic_instruction(Basic::ScopePop(ScopePop {
                scope_id: self.container.scope_tree.root_scope,
            }));

        // remove all unreachable instructions
        for basic_block in self.container.control_flow_graph.basic_blocks_mut() {
            // TODO: reports a warning if there are unreachable instructions
            basic_block.take_unreachable_instructions();
        }

        // transform intermediate types into concrete types
        let mut container = Self::transform(self.container, &self.inference_context);

        // perform auto moves
        let mut moved_variables = HashMap::new();
        Self::auto_move(
            &container.control_flow_graph,
            &mut container.registers,
            container.control_flow_graph.entry_block(),
            HashSet::new(),
            &mut Vec::new(),
            &mut moved_variables,
        );

        Ok(Hir { container })
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    fn auto_move(
        control_flow_graph: &ControlFlowGraph<Backend<Type>>,
        registers: &mut Arena<Register<Type>>,
        basic_block_id: BasicBlockID,
        mut visited: HashSet<BasicBlockID>,
        scope_stack: &mut Vec<ScopeID>,
        moved_variables: &mut HashMap<ScopeID, Vec<Address>>,
    ) {
        if visited.contains(&basic_block_id) {
            return;
        }

        visited.insert(basic_block_id);

        for (index, instruction) in control_flow_graph
            .get(basic_block_id)
            .unwrap()
            .instructions()
            .iter()
            .enumerate()
        {
            match instruction {
                Instruction::Jump(inst) => {
                    Self::auto_move(
                        control_flow_graph,
                        registers,
                        inst.jump_target,
                        visited,
                        scope_stack,
                        moved_variables,
                    );
                    return;
                }
                Instruction::Return(..) => return,
                Instruction::ConditionalJump(inst) => {
                    Self::auto_move(
                        control_flow_graph,
                        registers,
                        inst.true_jump_target,
                        visited.clone(),
                        scope_stack,
                        moved_variables,
                    );
                    Self::auto_move(
                        control_flow_graph,
                        registers,
                        inst.false_jump_target,
                        visited,
                        scope_stack,
                        moved_variables,
                    );
                    return;
                }
                Instruction::Basic(inst) => {
                    match inst {
                        Basic::RegisterAssignment(register_assignment) => {
                            // expect load binding
                            let Binding::Load(load) = &registers.get(register_assignment.register_id).unwrap().binding else {
                                continue;
                            };
                            let address = load.address.clone();

                            if Self::is_movable(
                                control_flow_graph,
                                registers,
                                basic_block_id,
                                index + 1,
                                &address,
                            ) {
                                registers
                                    .get_mut(register_assignment.register_id)
                                    .unwrap()
                                    .binding
                                    .as_load_mut()
                                    .unwrap()
                                    .load_type = LoadType::Move;

                                moved_variables
                                    .entry(*scope_stack.last().unwrap())
                                    .or_default()
                                    .push(address);
                            }
                        }
                        Basic::ScopePush(new_scope_id) => scope_stack.push(new_scope_id.scope_id),
                        Basic::ScopePop(pop_scope_id) => {
                            assert!(scope_stack.pop() == Some(pop_scope_id.scope_id));
                        }

                        Basic::VariableDeclaration(..) | Basic::Store(..) => (),
                    }
                }
            }
        }
    }

    fn is_movable(
        control_flow_graph: &ControlFlowGraph<Backend<Type>>,
        registers: &Arena<Register<Type>>,
        basic_block_id: BasicBlockID,
        instruction_offset: usize,
        target_address: &Address,
    ) -> bool {
        for instruction in control_flow_graph
            .get(basic_block_id)
            .unwrap()
            .instructions()
            .iter()
            .skip(instruction_offset)
        {
            match instruction {
                Instruction::Jump(inst) => {
                    return Self::is_movable(
                        control_flow_graph,
                        registers,
                        inst.jump_target,
                        0,
                        target_address,
                    );
                }
                Instruction::Return(..) => {
                    return true;
                }
                Instruction::ConditionalJump(inst) => {
                    return Self::is_movable(
                        control_flow_graph,
                        registers,
                        inst.true_jump_target,
                        0,
                        target_address,
                    ) && Self::is_movable(
                        control_flow_graph,
                        registers,
                        inst.false_jump_target,
                        0,
                        target_address,
                    );
                }
                Instruction::Basic(basic) => match basic {
                    Basic::ScopePop(..) | Basic::ScopePush(..) => {}
                    Basic::VariableDeclaration(inst) => {
                        if Self::is_subaddress_or_equal(
                            &Address::VariableID(VariableID::AllocaID(inst.alloca_id)),
                            target_address,
                        ) {
                            return true;
                        }
                    }
                    Basic::Store(inst) => {
                        if Self::is_subaddress_or_equal(&inst.address, target_address) {
                            return true;
                        }
                    }
                    Basic::RegisterAssignment(register_assignment) => {
                        let Binding::Load(load) = &registers.get(register_assignment.register_id).unwrap().binding else {
                            continue;
                        };

                        if Self::is_subaddress_or_equal(&load.address, target_address)
                            || Self::is_subaddress_or_equal(target_address, &load.address)
                        {
                            return false;
                        }
                    }
                },
            }
        }

        true
    }

    fn is_subaddress_or_equal(parent_address: &Address, sub_address: &Address) -> bool {
        match (parent_address, sub_address) {
            (Address::VariableID(lhs), Address::VariableID(rhs)) => lhs == rhs,
            (Address::VariableID(..), Address::FieldAddress(rhs)) => {
                Self::is_subaddress_or_equal(parent_address, &rhs.operand_address)
            }
            (Address::FieldAddress(..), Address::VariableID(..)) => false,
            (Address::FieldAddress(lhs), Address::FieldAddress(rhs)) => {
                if lhs == rhs {
                    true
                } else {
                    Self::is_subaddress_or_equal(parent_address, &rhs.operand_address)
                }
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////

    fn transform(
        container: Container<IntermediateTypeID>,
        inference_context: &InferenceContext,
    ) -> Container<Type> {
        let control_flow_graph = container.control_flow_graph.map::<Backend<Type>>(
            |x| x,
            |x| Return {
                return_value: x
                    .return_value
                    .map(|x| Self::transform_value(x, inference_context)),
            },
            |x| match x {
                Basic::RegisterAssignment(x) => Basic::RegisterAssignment(x),
                Basic::VariableDeclaration(x) => Basic::VariableDeclaration(x),
                Basic::Store(x) => Basic::Store(Store {
                    address: x.address,
                    value: Self::transform_value(x.value, inference_context),
                    span: x.span,
                }),
                Basic::ScopePush(x) => Basic::ScopePush(x),
                Basic::ScopePop(x) => Basic::ScopePop(x),
            },
            |x| ConditionalJump {
                condition_value: Self::transform_value(x.condition_value, inference_context),
                true_jump_target: x.true_jump_target,
                false_jump_target: x.false_jump_target,
            },
        );

        let allocas = container.allocas.map(|source_alloca| Alloca {
            identifier_token: source_alloca.identifier_token,
            is_mutable: source_alloca.is_mutable,
            ty: Self::transform_intermediate_type_id(source_alloca.ty, inference_context),
            scope_id: source_alloca.scope_id,
        });

        let registers = container.registers.map(|x| Register {
            binding: Self::transform_binding(x.binding, inference_context),
        });

        Container {
            control_flow_graph,
            registers,
            allocas,
            table: container.table,
            overload_id: container.overload_id,
            parent_overload_set_id: container.parent_overload_set_id,
            parent_scoped_id: container.parent_scoped_id,
            scope_tree: container.scope_tree,
        }
    }

    fn transform_binding(
        binding: Binding<IntermediateTypeID>,
        inference_context: &InferenceContext,
    ) -> Binding<Type> {
        match binding {
            Binding::FunctionCall(binding) => Binding::FunctionCall(FunctionCall {
                span: binding.span,
                overload_id: binding.overload_id,
                arguments: binding
                    .arguments
                    .into_iter()
                    .map(|x| Self::transform_value(x, inference_context))
                    .collect(),
            }),
            Binding::Prefix(binding) => Binding::Prefix(Prefix {
                span: binding.span,
                prefix_operator: binding.prefix_operator,
                operand: Self::transform_value(binding.operand, inference_context),
            }),
            Binding::Load(binding) => Binding::Load(Load {
                span: binding.span,
                load_type: binding.load_type,
                address: binding.address,
            }),
            Binding::StructLiteral(binding) => Binding::StructLiteral(StructLiteral {
                span: binding.span,
                struct_id: binding.struct_id,
                initializations: binding
                    .initializations
                    .into_iter()
                    .map(|(k, v)| (k, Self::transform_value(v, inference_context)))
                    .collect(),
            }),
            Binding::MemberAccess(binding) => Binding::MemberAccess(MemberAccess {
                span: binding.span,
                operand: Self::transform_value(binding.operand, inference_context),
                field_id: binding.field_id,
            }),
            Binding::Binary(binding) => Binding::Binary(Binary {
                span: binding.span,
                left_operand: Self::transform_value(binding.left_operand, inference_context),
                right_operand: Self::transform_value(binding.right_operand, inference_context),
                binary_operator: binding.binary_operator,
            }),
            Binding::PhiNode(binding) => Binding::PhiNode(PhiNode {
                span: binding.span,
                values_by_predecessor: binding
                    .values_by_predecessor
                    .into_iter()
                    .map(|(k, v)| (k, Self::transform_value(v, inference_context)))
                    .collect(),
                phi_node_source: binding.phi_node_source,
            }),
            Binding::Cast(binding) => Binding::Cast(Cast {
                operand: Self::transform_value(binding.operand, inference_context),
                target_type: Self::transform_intermediate_type_id(
                    binding.target_type,
                    inference_context,
                ),
                span: binding.span,
            }),
        }
    }

    fn transform_value(
        value: Value<IntermediateTypeID>,
        inference_context: &InferenceContext,
    ) -> Value<Type> {
        match value {
            Value::Register(register) => Value::Register(register),
            Value::Constant(constant) => match constant {
                Constant::NumericLiteral(literal) => {
                    Value::Constant(Constant::NumericLiteral(NumericLiteral {
                        numeric_literal_syntax_tree: literal.numeric_literal_syntax_tree,
                        ty: Self::transform_intermediate_type_id(literal.ty, inference_context),
                    }))
                }
                Constant::BooleanLiteral(literal) => {
                    Value::Constant(Constant::BooleanLiteral(literal))
                }
                Constant::EnumLiteral(literal) => Value::Constant(Constant::EnumLiteral(literal)),
                Constant::VoidConstant(constant) => {
                    Value::Constant(Constant::VoidConstant(constant))
                }
            },
            Value::Placeholder(..) | Value::Unreachable(..) => unreachable!(),
        }
    }

    fn transform_intermediate_type_id(
        intermediate_type_id: IntermediateTypeID,
        inference_context: &InferenceContext,
    ) -> Type {
        match intermediate_type_id {
            IntermediateTypeID::InferenceID(inference) => {
                match inference_context.get_inferable_type(inference).unwrap() {
                    InferableType::Type(ty) => ty,
                    InferableType::Constraint(constraint) => match constraint {
                        Constraint::All | Constraint::PrimitiveType => unreachable!(),
                        Constraint::Number | Constraint::Signed => {
                            Type::PrimitiveType(PrimitiveType::Int32)
                        }
                        Constraint::Float => Type::PrimitiveType(PrimitiveType::Float64),
                    },
                }
            }
            IntermediateTypeID::Type(ty) => ty,
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////
}

#[cfg(test)]
mod tests;
