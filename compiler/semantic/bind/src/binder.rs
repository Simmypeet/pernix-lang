//! Contains the definition of [`Binder`], the struct used for building the IR.

use std::{borrow::Cow, num::NonZeroUsize, sync::Arc};

use getset::{CopyGetters, Getters};
use pernixc_arena::ID;
use pernixc_extend::extend;
use pernixc_handler::Handler;
use pernixc_ir::{
    address::{Address, Memory},
    alloca::Alloca,
    control_flow_graph::Block,
    instruction::{self, Instruction, ScopePop, ScopePush, Terminator},
    pattern::{Irrefutable, NameBindingPoint, Wildcard},
    scope,
    value::{
        literal::{Literal, Unreachable},
        register::{Assignment, Load, Register},
        TypeOf, Value,
    },
    IR,
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{
    runtime::executor::{self, CyclicError},
    TrackedEngine,
};
use pernixc_resolution::{
    self, generic_parameter_namespace::get_generic_parameter_namespace,
    ExtraNamespace,
};
use pernixc_semantic_element::parameter::get_parameters;
use pernixc_serialize::Serialize;
use pernixc_source_file::SourceElement;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::syntax::get_function_signature_syntax;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    display::InferenceRenderingMap,
    inference,
    r#type::{Qualifier, Type},
};
pub use pernixc_type_system::UnrecoverableError;
use pernixc_type_system::{
    environment::{get_active_premise, Environment as TyEnvironment, Premise},
    Succeeded,
};

use crate::{
    bind::LValue,
    binder::{inference_context::InferenceContext, stack::Stack},
    diagnostic::Diagnostic,
    infer::constraint,
    pattern::insert_name_binding,
};

mod finalize;

pub mod block;
pub mod closure;
pub mod inference_context;
pub mod r#loop;
pub mod stack;
pub mod type_check;

/// The environment where the binder is operating on.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize)]
pub struct Environment {
    /// The current site where the binder is operating on.
    current_site: Global<pernixc_symbol::ID>,

    /// Gets the active premise at the `current_site`.
    premise: Arc<Premise>,

    extra_namespace: Arc<ExtraNamespace>,
}

impl Environment {
    /// Creates an environment object for the given `current_site`.
    pub async fn new(
        engine: &TrackedEngine,
        current_site: Global<pernixc_symbol::ID>,
    ) -> Result<Self, CyclicError> {
        let premise = engine.get_active_premise(current_site).await?;
        let generic_parameter_namespace =
            engine.get_generic_parameter_namespace(current_site).await?;

        Ok(Self {
            current_site,
            premise,
            extra_namespace: generic_parameter_namespace,
        })
    }
}

/// The binder used for building the IR.
#[derive(Debug, Getters, CopyGetters)]
pub struct Binder<'t> {
    /// Gets the engine used for querying information about the program.
    #[get = "pub"]
    engine: &'t TrackedEngine,

    /// The current environment information where the binder is operating on.
    environment: &'t Environment,

    /// The intermediate representation that is being built.
    #[get = "pub"]
    ir: IR,

    /// The current block ID where the binder is operating on.
    #[get_copy = "pub"]
    current_block_id: ID<Block>,

    /// The stack used for managing scopes and named bindings.
    #[get = "pub"]
    stack: stack::Stack,

    /// The inference context used for managing type and constant inferences.
    #[get = "pub"]
    inference_context: InferenceContext,

    unreachable_register_ids: Vec<ID<Register>>,

    block_context: block::Context,
    loop_context: r#loop::Context,
}

impl<'t> Binder<'t> {
    /// Returns the current site where the binder is operating on.
    #[must_use]
    pub const fn current_site(&self) -> Global<pernixc_symbol::ID> {
        self.environment.current_site
    }

    /// Returns the extra namespace used for resolving identifiers.
    #[must_use]
    pub fn extra_namespace(&self) -> &'t ExtraNamespace {
        &self.environment.extra_namespace
    }

    /// Returns the active premise where the binder is operating on.
    #[must_use]
    pub fn premise(&self) -> &'t Premise { &self.environment.premise }
}

impl<'t> Binder<'t> {
    /// Creates the binder for building the IR.
    pub async fn new_function(
        engine: &'t TrackedEngine,
        environment: &'t Environment,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Self, UnrecoverableError> {
        let ir = IR::default();
        let current_block_id = ir.control_flow_graph.entry_block_id();
        let stack = Stack::new(ir.scope_tree.root_scope_id(), false);

        let mut binder = Self {
            engine,
            ir,
            stack,
            current_block_id,
            environment,

            inference_context: InferenceContext::default(),

            unreachable_register_ids: Vec::new(),

            block_context: block::Context::default(),
            loop_context: r#loop::Context::default(),
        };

        let root_scope_id = binder.ir.scope_tree.root_scope_id();
        binder.push_instruction(instruction::Instruction::ScopePush(
            ScopePush(root_scope_id),
        ));

        let (parameters_syn, _) = binder
            .engine
            .get_function_signature_syntax(environment.current_site)
            .await;

        let parameters =
            binder.engine.get_parameters(environment.current_site).await?;

        let mut name_binding_point = NameBindingPoint::default();

        // bind the parameter patterns
        #[allow(clippy::significant_drop_in_scrutinee)]
        if let Some(parameters_syn) = parameters_syn {
            for ((parameter_id, parameter_sym), syntax_tree) in parameters
                .parameter_order
                .iter()
                .copied()
                .map(|x| (x, &parameters.parameters[x]))
                .zip(parameters_syn.parameters().filter_map(|x| {
                    x.into_regular().ok().and_then(|x| x.irrefutable_pattern())
                }))
            {
                let pattern = binder
                    .bind_pattern(&syntax_tree, &parameter_sym.r#type, handler)
                    .await?
                    .unwrap_or_else(|| {
                        Irrefutable::Wildcard(Wildcard {
                            span: syntax_tree.span(),
                        })
                    });

                binder
                    .insert_name_binding_point(
                        &mut name_binding_point,
                        &pattern,
                        &parameter_sym.r#type,
                        Address::Memory(Memory::Parameter(parameter_id)),
                        Qualifier::Mutable,
                        &insert_name_binding::Config {
                            must_copy: false,
                            scope_id: root_scope_id,
                            address_span: None,
                        },
                        handler,
                    )
                    .await?;
            }
        }

        binder
            .stack
            .current_scope_mut()
            .add_named_binding_point(name_binding_point);

        Ok(binder)
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error(
    "encountered a fatal semantic error that cannot be recovered while \
     binding an expression, the binder state is still intact and can still \
     bind the next statements or expressions"
)]
#[allow(missing_docs)]
pub struct BindingError(pub RelativeSpan);

impl From<executor::CyclicError> for Error {
    fn from(value: executor::CyclicError) -> Self {
        Self::Unrecoverable(UnrecoverableError::CyclicDependency(value))
    }
}

/// Is an error occurred while binding the syntax tree
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    Binding(#[from] BindingError),

    /// Encountered an error and the diagnostic is already reported. The binder
    /// should not be used anymore after encountered this error variant, the
    /// state of the binder is corrupted.
    #[error(transparent)]
    Unrecoverable(#[from] UnrecoverableError),
}

/// In case of the Overflow error from type system, reports it as a type    
/// calculating overflow
#[extend]
pub fn report_as_type_calculating_overflow(
    self: pernixc_type_system::Error,
    overflow_span: RelativeSpan,
    handler: &dyn Handler<Diagnostic>,
) -> UnrecoverableError {
    match self {
        pernixc_type_system::Error::Overflow(overflow) => {
            overflow
                .report_as_type_calculating_overflow(overflow_span, &handler);

            UnrecoverableError::Reported
        }
        pernixc_type_system::Error::CyclicDependency(cyclic) => {
            UnrecoverableError::CyclicDependency(cyclic)
        }
    }
}

/// In case of the Overflow error from type system, reports it as a type
/// checking overflow
#[extend]
pub fn report_as_type_check_overflow(
    self: pernixc_type_system::Error,
    overflow_span: RelativeSpan,
    handler: &dyn Handler<Diagnostic>,
) -> UnrecoverableError {
    match self {
        pernixc_type_system::Error::Overflow(overflow) => {
            overflow.report_as_type_check_overflow(overflow_span, &handler);

            UnrecoverableError::Reported
        }
        pernixc_type_system::Error::CyclicDependency(cyclic) => {
            UnrecoverableError::CyclicDependency(cyclic)
        }
    }
}

impl Binder<'_> {
    /// Creates a new error literal with an inferred type.
    pub fn create_error(&mut self, span: RelativeSpan) -> Literal {
        Literal::Error(pernixc_ir::value::literal::Error {
            r#type: {
                let inference =
                    self.inference_context.next_type_inference_variable();

                assert!(self
                    .inference_context
                    .register(inference, constraint::Type::All(true)));

                Type::Inference(inference)
            },
            span: Some(span),
        })
    }

    /// Returns the inference rendering map for types.
    #[must_use]
    pub fn type_inference_rendering_map(&self) -> InferenceRenderingMap<Type> {
        self.inference_context.type_table().get_inference_rendering_map()
    }

    /// Returns the inference rendering map for constants.
    #[must_use]
    pub fn constant_inference_rendering_map(
        &self,
    ) -> InferenceRenderingMap<Constant> {
        self.inference_context.const_table().get_inference_rendering_map()
    }

    /// Creates a new unreachable literal with an inferred type.
    pub fn create_unreachable(&mut self, span: RelativeSpan) -> Literal {
        Literal::Unreachable(Unreachable {
            r#type: {
                let inference =
                    self.inference_context.next_type_inference_variable();

                assert!(self
                    .inference_context
                    .register(inference, constraint::Type::All(true)));

                Type::Inference(inference)
            },
            span: Some(span),
        })
    }

    /// Creates a new alloca with explicitly specified scope that the alloca
    /// will be in .
    ///
    /// Unlike `create_alloca` which use the current scope of the binder, this
    /// function allows the caller to specify the scope explicitly.
    pub fn create_alloca_with_scope_id(
        &mut self,
        r#type: Type,
        scope_id: ID<scope::Scope>,
        span: RelativeSpan,
    ) -> ID<Alloca> {
        let alloca_id = self.ir.values.allocas.insert(Alloca {
            r#type,
            declared_in_scope_id: scope_id,
            declaration_order: self
                .stack
                .scopes()
                .iter()
                .rev()
                .find_map(|x| {
                    (x.scope_id() == scope_id)
                        .then(|| x.variable_declarations().len())
                })
                .expect("scope not found"),
            span: Some(span),
        });

        self.stack
            .scopes_mut()
            .iter_mut()
            .rev()
            .find(|x| x.scope_id() == scope_id)
            .unwrap()
            .add_variable_declaration(alloca_id);

        alloca_id
    }

    /// Creates a new alloca and adds it to the current scope.
    pub fn create_alloca(
        &mut self,
        r#type: Type,
        span: RelativeSpan,
    ) -> ID<Alloca> {
        let alloca_id = self.ir.values.allocas.insert(Alloca {
            r#type,
            declared_in_scope_id: self.stack.current_scope().scope_id(),
            declaration_order: self
                .stack
                .current_scope()
                .variable_declarations()
                .len(),
            span: Some(span),
        });

        self.stack.current_scope_mut().add_variable_declaration(alloca_id);

        alloca_id
    }

    /// Creates a new alloca and adds it to the current scope and generates a
    /// store instruction to initialize the alloca with the given value.
    pub async fn create_alloca_with_value(
        &mut self,
        value: Value,
        scope_id: ID<scope::Scope>,
        address_span: Option<RelativeSpan>,
        store_span: RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<ID<Alloca>, UnrecoverableError> {
        let ty = self.type_of_value(&value, handler).await?;
        let span = address_span.unwrap_or_else(|| match &value {
            Value::Register(id) => {
                self.ir.values.registers.get(*id).unwrap().span.unwrap()
            }
            Value::Literal(literal) => literal.span().copied().unwrap(),
        });

        let alloca_id = self.create_alloca_with_scope_id(ty, scope_id, span);
        let alloca_address = Address::Memory(Memory::Alloca(alloca_id));

        let _ = self.current_block_mut().add_instruction(
            instruction::Instruction::Store(instruction::Store {
                address: alloca_address,
                value,
                span: Some(store_span),
            }),
        );

        Ok(alloca_id)
    }

    /// Returns a reference to the current control flow graph.
    #[must_use]
    pub fn current_block(&self) -> &Block {
        &self.ir.control_flow_graph[self.current_block_id]
    }

    /// Returns a mutable reference to the current control flow graph.
    fn current_block_mut(&mut self) -> &mut Block {
        &mut self.ir.control_flow_graph[self.current_block_id]
    }

    /// Adds a new instruction to the current block.
    pub fn push_instruction(&mut self, instruction: Instruction) {
        assert!(
            !instruction.is_register_assignment(),
            "Use `create_register_assignment` to create register assignment \
             instructions"
        );

        let _ = self.current_block_mut().add_instruction(instruction);
    }

    /// Inserts a terminator to the current block.
    pub fn insert_terminator(&mut self, terminator: Terminator) {
        self.ir
            .control_flow_graph
            .insert_terminator(self.current_block_id, terminator);
    }

    /// Creates a new type inference variable and assigns it to the inference
    /// context with the given constraint.
    pub fn create_type_inference(
        &mut self,
        constraint: constraint::Type,
    ) -> inference::Variable<Type> {
        let infer_var = self.inference_context.next_type_inference_variable();
        assert!(self.inference_context.register(infer_var, constraint));

        infer_var
    }

    /// Creates a new constant inference variable and assigns it to the
    /// inference context.
    pub fn create_constant_inference(
        &mut self,
    ) -> inference::Variable<Constant> {
        let infer_var =
            self.inference_context.next_constant_inference_variable();
        assert!(self
            .inference_context
            .register(infer_var, constraint::Constant));

        infer_var
    }

    /// Creates a new child branch of scopes at the given `parent_scope_id`.
    pub fn new_child_branch_at(
        &mut self,
        parent_scope_id: ID<scope::Scope>,
        count: NonZeroUsize,
    ) -> Vec<ID<scope::Scope>> {
        self.ir.scope_tree.new_child_branch(parent_scope_id, count).unwrap()
    }

    /// Creates a new child branch of scopes at the current scope.
    pub fn new_child_branch(
        &mut self,
        count: NonZeroUsize,
    ) -> Vec<ID<scope::Scope>> {
        self.new_child_branch_at(self.stack.current_scope().scope_id(), count)
    }

    /// Creates a new block in the control flow graph.
    pub fn new_block(&mut self) -> ID<Block> {
        self.ir.control_flow_graph.new_block()
    }

    /// Creates an environment object that includes the `active_premise`,
    ///
    /// `table`, and `inference_context` normalizer.
    #[must_use]
    pub fn create_environment(&self) -> TyEnvironment<'_, InferenceContext> {
        let environment = TyEnvironment::new(
            Cow::Borrowed(&self.environment.premise),
            Cow::Borrowed(self.engine),
            &self.inference_context,
        );

        environment
    }

    /// Simplifies the type and possibly reports overflow error as a type
    /// calculating overflow.
    pub async fn simplify_type(
        &self,
        ty: Type,
        span: RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Arc<Succeeded<Type>>, UnrecoverableError> {
        self.create_environment()
            .simplify(ty)
            .await
            .map_err(|x| x.report_as_type_calculating_overflow(span, &handler))
    }

    /// Creates a new register and assigns the given `assignment` to it.
    pub fn create_register_assignment(
        &mut self,
        assignment: Assignment,
        span: RelativeSpan,
    ) -> ID<Register> {
        let register_id = self
            .ir
            .values
            .registers
            .insert(Register { assignment, span: Some(span) });

        let reachable = self.current_block_mut().add_instruction(
            instruction::Instruction::RegisterAssignment(
                instruction::RegisterAssignment { id: register_id },
            ),
        );

        if !reachable {
            self.unreachable_register_ids.push(register_id);
        }

        register_id
    }

    /// Creates a load instruction that loads the value from the given `lvalue`
    /// and returns the register ID that holds the loaded value.
    pub fn load_lvalue(&mut self, lvalue: LValue) -> ID<Register> {
        self.create_register_assignment(
            Assignment::Load(Load { address: lvalue.address }),
            lvalue.span,
        )
    }

    /// Gets the type of the given `register_id`.
    pub async fn type_of_register(
        &self,
        register_id: ID<Register>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Type, UnrecoverableError> {
        self.ir
            .values
            .type_of(
                register_id,
                self.current_site(),
                &self.create_environment(),
            )
            .await
            .map(|x| x.result)
            .map_err(|x| {
                x.report_as_type_calculating_overflow(
                    self.ir.values.registers[register_id].span.unwrap(),
                    &handler,
                )
            })
    }

    /// Gets the type of the given `value`.
    pub async fn type_of_value(
        &self,
        value: &Value,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Type, UnrecoverableError> {
        match value {
            Value::Register(register_id) => {
                self.type_of_register(*register_id, handler).await
            }
            Value::Literal(literal) => Ok(self
                .create_environment()
                .simplify(literal.r#type())
                .await
                .map_err(|x| {
                    x.report_as_type_calculating_overflow(
                        literal.span().copied().unwrap(),
                        &handler,
                    )
                })?
                .result
                .clone()),
        }
    }

    /// Creates a new scope at the current instruction pointer and pushes it to
    /// the stack.
    pub fn push_scope(&mut self, is_unsafe: bool) -> ID<scope::Scope> {
        let scope_id = self
            .ir
            .scope_tree
            .new_child_branch(
                self.stack.current_scope().scope_id(),
                NonZeroUsize::new(1).unwrap(),
            )
            .unwrap()[0];

        self.push_scope_with(scope_id, is_unsafe);

        scope_id
    }

    /// Pops the current scope from the stack without adding a scope pop
    /// instruction.
    pub fn pop_scope_context(&mut self, scope_id: ID<scope::Scope>) {
        assert_eq!(
            self.stack.pop_scope().map(|x| x.scope_id()),
            Some(scope_id)
        );
    }

    /// Pops the current scope from the stack and adds a scope pop instruction
    /// to the current block.
    pub fn pop_scope(&mut self, scope_id: ID<scope::Scope>) {
        assert_eq!(
            self.stack.pop_scope().map(|x| x.scope_id()),
            Some(scope_id)
        );
        let _ = self
            .current_block_mut()
            .add_instruction(Instruction::ScopePop(ScopePop(scope_id)));
    }

    /// Sets the current block ID to the given `block_id`.
    pub const fn set_current_block_id(&mut self, block_id: ID<Block>) {
        self.current_block_id = block_id;
    }

    /// Inserts a scope push instruction to the current block and pushes the
    /// scope to the stack.
    pub fn push_scope_with(
        &mut self,
        scope_id: ID<scope::Scope>,
        is_unsafe: bool,
    ) {
        self.stack.push_scope(scope_id, is_unsafe);
        let _ = self
            .current_block_mut()
            .add_instruction(Instruction::ScopePush(ScopePush(scope_id)));
    }

    /// Gets the span of the given `value`.
    #[must_use]
    pub fn span_of_value(&self, value: &Value) -> RelativeSpan {
        match value {
            Value::Register(id) => self.ir.values.registers[*id].span.unwrap(),
            Value::Literal(literal) => *literal.span().unwrap(),
        }
    }

    /// Returns the mutability qualifier accounting for all references the
    /// address is possibly behind.
    pub async fn get_behind_reference_qualifier(
        &self,
        mut address: &Address,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Option<Qualifier>, UnrecoverableError> {
        loop {
            match address {
                Address::Memory(
                    Memory::Alloca(_)
                    | Memory::Parameter(_)
                    | Memory::Capture(_),
                ) => return Ok(None),

                Address::Field(ad) => {
                    address = &ad.struct_address;
                }
                Address::Tuple(ad) => {
                    address = &ad.tuple_address;
                }
                Address::Index(ad) => {
                    address = &ad.array_address;
                }
                Address::Variant(ad) => {
                    address = &ad.enum_address;
                }
                Address::Reference(ad) => {
                    let ty = self
                        .type_of_address(&ad.reference_address, handler)
                        .await?;

                    let ref_ty = match ty {
                        Type::Reference(ref_ty) => ref_ty,
                        found => {
                            panic!(
                                "expected a reference type, found: {found:#?}",
                            );
                        }
                    };

                    let mut qualifier = ref_ty.qualifier;

                    if let Some(inner_qual) =
                        Box::pin(self.get_behind_reference_qualifier(
                            &ad.reference_address,
                            handler,
                        ))
                        .await?
                    {
                        qualifier = qualifier.min(inner_qual);
                    }

                    return Ok(Some(qualifier));
                }
            }
        }
    }

    /// Gets the type of the given `address`.
    pub async fn type_of_address(
        &self,
        address: &Address,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Type, UnrecoverableError> {
        match self
            .ir
            .values
            .type_of(address, self.current_site(), &self.create_environment())
            .await
        {
            Ok(x) => Ok(x.result),
            Err(err) => Err(err.report_as_type_calculating_overflow(
                match address.get_root_memory() {
                    Memory::Parameter(id) => {
                        let parameters = self
                            .engine
                            .get_parameters(self.current_site())
                            .await?;

                        parameters.parameters[*id].span.unwrap()
                    }
                    Memory::Alloca(id) => {
                        self.ir.values.allocas[*id].span.unwrap()
                    }
                    Memory::Capture(id) => {
                        self.ir.values.captures[*id].span.unwrap()
                    }
                },
                &handler,
            )),
        }
    }

    /// Inserts a panic terminator to the current block.
    pub fn insert_panic_terminator(&mut self) {
        self.ir
            .control_flow_graph
            .insert_terminator(self.current_block_id, Terminator::Panic);
    }

    /// Creates a checkpoint of the current inference context. The caller can
    /// restore the inference context's state to this checkpoint later.
    pub fn start_inference_context_checkpoint(
        &mut self,
    ) -> inference_context::Checkpoint {
        self.inference_context.start_checkpoint()
    }

    /// Commits the inference context's state at the given `checkpoint`, making
    /// it impossible to restore to the state at the checkpoint.
    pub fn commit_inference_context_checkpoint(
        &mut self,
        checkpoint: inference_context::Checkpoint,
    ) {
        self.inference_context.commit_checkpoint(checkpoint);
    }

    /// Restores the inference context's state to the given `checkpoint`.
    pub fn restore_inference_context_checkpoint(
        &mut self,
        checkpoint: inference_context::Checkpoint,
    ) {
        self.inference_context.restore(checkpoint);
    }

    /// Adds the given `name_binding_point` to the current scope.
    pub fn add_named_binding_point(
        &mut self,
        name_binding_point: NameBindingPoint,
    ) {
        self.stack
            .current_scope_mut()
            .add_named_binding_point(name_binding_point);
    }

    /// Traverses the scope stack from the top and pushes `ScopePop`
    /// instructions until the scope with `scope_id` is popped.
    pub fn pop_all_scope_to(&mut self, scope_id: ID<scope::Scope>) {
        for popping_scope in self
            .stack
            .scopes()
            .iter()
            .rev()
            .map(stack::Scope::scope_id)
            .take_while(|x| *x != scope_id)
            .chain(std::iter::once(scope_id))
        {
            let _ = self
                .ir
                .control_flow_graph
                .get_block_mut(self.current_block_id)
                .unwrap()
                .add_instruction(Instruction::ScopePop(ScopePop(
                    popping_scope,
                )));
        }
    }
}
