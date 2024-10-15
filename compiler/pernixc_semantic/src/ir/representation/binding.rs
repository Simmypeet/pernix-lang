//! Contains the definition of [`Binder`], the struct used for building the IR.

use std::{collections::HashMap, num::NonZeroUsize, sync::Arc};

use enum_as_inner::EnumAsInner;
use getset::Getters;
use infer::{
    Constraint, Context, Erased, InferenceVariable, NoConstraint, UnifyError,
};
use parking_lot::RwLock;
use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree;
use stack::Stack;

use super::Representation;
use crate::{
    arena::ID,
    error::{self, CyclicInference, MismatchedType},
    ir::{
        self,
        address::{Address, Memory},
        alloca::Alloca,
        control_flow_graph::Block,
        instruction::{self, Instruction, ScopePop, ScopePush},
        pattern::{Irrefutable, NameBindingPoint, Pattern, Wildcard},
        scope::{self, Scope},
        value::{
            register::{Assignment, Register},
            Value,
        },
        TypeOfError,
    },
    symbol::{
        table::{
            self,
            representation::Index,
            resolution::{self, ElidedTermProvider, Resolution},
            Table,
        },
        FunctionTemplate, GenericID, GenericTemplate, GlobalID,
    },
    type_system::{
        self,
        environment::Environment,
        model::Model,
        simplify::simplify,
        term::{
            self,
            constant::Constant,
            lifetime::Lifetime,
            r#type::{self, Expected, Qualifier, Type},
            Term,
        },
        Premise,
    },
};

pub mod expression;
pub mod infer;
pub mod stack;
pub mod statement;

mod pattern;

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
enum LoopKind {
    While,
    Loop {
        incoming_values: HashMap<ID<Block<infer::Model>>, Value<infer::Model>>,
        break_type: Option<Type<infer::Model>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LoopState {
    label: Option<String>,
    loop_block_id: ID<Block<infer::Model>>,
    kind: LoopKind,
    exit_block_id: ID<Block<infer::Model>>,
    span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct BlockState {
    label: Option<String>,
    incoming_values: HashMap<ID<Block<infer::Model>>, Value<infer::Model>>,
    successor_block_id: ID<Block<infer::Model>>,
    express_type: Option<Type<infer::Model>>,
    span: Span,
}

/// The binder used for building the IR.
///
/// # Invariants
///
/// The binder works as a state machine that builds the IR. When the one of the
/// methods returns an error of [`Error::Internal`], it's considered that the
/// binder state is corrupted and should not be used anymore.
#[derive(Debug, Getters)]
pub struct Binder<
    't,
    S: table::State,
    RO: resolution::Observer<S, infer::Model>,
    TO: type_system::observer::Observer<infer::Model, S>,
> {
    table: &'t Table<S>,

    resolution_observer: RO,
    type_system_observer: TO,

    current_site: GlobalID,
    premise: Premise<infer::Model>,
    stack: Stack,

    /// The intermediate representation that is being built.
    #[get = "pub"]
    intermediate_representation: Representation<infer::Model>,
    current_block_id: ID<Block<infer::Model>>,

    inference_context: infer::Context,

    block_states_by_scope_id: HashMap<ID<scope::Scope>, BlockState>,
    loop_states_by_scope_id: HashMap<ID<scope::Scope>, LoopState>,

    // a boolean flag indicating whether there's already been an error reported
    suboptimal: Arc<RwLock<bool>>,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum CreateFunctionBinderError {
    #[error("the given function ID does not exist in the table")]
    InvalidFunctionID,

    #[error(
        "the given parameter's pattern syntax iterator count does not match \
         the function's parameter count"
    )]
    MismatchedParameterCount,
}

#[derive(Clone)]
struct HandlerWrapper<'h> {
    handler: &'h dyn Handler<Box<dyn error::Error>>,
    suboptimal: Arc<RwLock<bool>>,
}

impl<'h> Handler<Box<dyn error::Error>> for HandlerWrapper<'h> {
    fn receive(&self, error: Box<dyn error::Error>) {
        // found an error, set the flag to true
        *self.suboptimal.write() = true;
        self.handler.receive(error);
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Binder<'t, S, RO, TO>
{
    /// Creates the binder for building the IR.
    ///
    /// # Errors
    ///
    /// See [`CreateFunctionBinderError`] for the possible errors.
    pub fn new_function<'a, P: Copy, D>(
        table: &'t Table<S>,
        resolution_observer: RO,
        type_system_observer: TO,
        function_id: ID<GenericTemplate<P, FunctionTemplate<D>>>,
        parameter_pattern_syns: impl ExactSizeIterator<
            Item = &'a syntax_tree::pattern::Irrefutable,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self, CreateFunctionBinderError>
    where
        GlobalID: From<P> + From<ID<GenericTemplate<P, FunctionTemplate<D>>>>,
        GenericTemplate<P, FunctionTemplate<D>>: table::representation::Element,
    {
        let premise = table
            .get_active_premise::<infer::Model>(function_id.into())
            .ok_or(CreateFunctionBinderError::InvalidFunctionID)?;

        let handler = HandlerWrapper {
            handler,
            suboptimal: Arc::new(RwLock::new(false)),
        };

        let intermediate_representation = Representation::default();
        let current_block_id =
            intermediate_representation.control_flow_graph.entry_block_id();

        let function_sym = table.get(function_id).unwrap();
        let stack =
            Stack::new(intermediate_representation.scope_tree.root_scope_id());

        // mismatched count
        if parameter_pattern_syns.len() != function_sym.parameters().len() {
            return Err(CreateFunctionBinderError::MismatchedParameterCount);
        }

        let mut binder = Self {
            table,
            current_site: function_id.into(),
            premise,
            intermediate_representation,
            stack,
            current_block_id,

            inference_context: Context::default(),

            resolution_observer,
            type_system_observer,

            block_states_by_scope_id: HashMap::new(),
            loop_states_by_scope_id: HashMap::new(),

            suboptimal: handler.suboptimal.clone(),
        };

        let mut parameter_name_binding_point = NameBindingPoint::default();

        let root_scope_id =
            binder.intermediate_representation.scope_tree.root_scope_id();

        let _ = binder.current_block_mut().insert_instruction(
            instruction::Instruction::ScopePush(ScopePush(root_scope_id)),
        );

        // bind the parameter patterns
        #[allow(clippy::significant_drop_in_scrutinee)]
        for ((parameter_id, parameter_sym), syntax_tree) in
            function_sym.parameter_as_order().zip(parameter_pattern_syns)
        {
            let parameter_type =
                infer::Model::from_default_type(parameter_sym.r#type.clone());

            let pattern = match Irrefutable::bind(
                syntax_tree,
                &parameter_type,
                binder.current_site,
                &binder.create_environment(),
                &handler,
            ) {
                Ok(v) => v.result,
                Err(ir::pattern::Error::Semantic) => {
                    Irrefutable::Wildcard(Wildcard {
                        span: Some(syntax_tree.span()),
                    })
                }
                Err(err) => {
                    panic!("unexpected error: {err:#?}");
                }
            };

            // add the binding point
            binder.insert_named_binding_point(
                &mut parameter_name_binding_point,
                &pattern,
                &parameter_type,
                Address::Memory(Memory::Parameter(parameter_id)),
                &handler,
            );
        }

        binder
            .stack
            .current_scope_mut()
            .add_named_binding_point(parameter_name_binding_point);

        drop(function_sym);

        Ok(binder)
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error(
    "encountered a fatal semantic error that halts the compilation; the error \
     diagnostics have been reported to the handler"
)]
#[allow(missing_docs)]
pub struct SemanticError(pub Span);

/// An internal compiler error that is not caused by the user but rather by the
/// incorrect invariant of the compiler.
///
/// When an internal error is encountered, the binder state is considered
/// corrupted and should not be used anymore.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum InternalError {
    #[error(
        "encountered an internal error while trying to retrieve the type of a \
         register or address while binding the IR"
    )]
    TypeOf(#[from] TypeOfError<infer::Model>),
}

/// Is an error occurred while binding the syntax tree
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    Semantic(#[from] SemanticError),

    #[error(transparent)]
    Internal(#[from] InternalError),
}

impl From<TypeOfError<infer::Model>> for Error {
    fn from(error: TypeOfError<infer::Model>) -> Self {
        Self::Internal(InternalError::TypeOf(error))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct LifetimeInferenceProvider;

impl ElidedTermProvider<Lifetime<infer::Model>> for LifetimeInferenceProvider {
    fn create(&mut self) -> Lifetime<infer::Model> {
        Lifetime::Inference(Erased)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct InferenceProvider<T> {
    cerated_inferences: Vec<T>,
}

impl<T: Default + Clone, U: Term<InferenceVariable = T>> ElidedTermProvider<U>
    for InferenceProvider<T>
{
    fn create(&mut self) -> U {
        let inference = T::default();
        let inference_term = U::from_inference(inference.clone());
        self.cerated_inferences.push(inference);

        inference_term
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Binder<'t, S, RO, TO>
{
    /// Creates a new alloca and adds it to the current scope.
    fn create_alloca(
        &mut self,
        r#type: Type<infer::Model>,
        span: Option<Span>,
    ) -> ID<Alloca<infer::Model>> {
        // the alloca allocation instructions will all be inserted after the
        // binding finishes

        let alloca_id =
            self.intermediate_representation.allocas.insert(Alloca {
                r#type,
                declared_in_scope_id: self.stack.current_scope().scope_id(),
                declaration_order: self
                    .stack
                    .current_scope()
                    .variable_declarations()
                    .len(),
                span,
            });

        self.stack.current_scope_mut().add_variable_declaration(alloca_id);

        alloca_id
    }

    /// Returns a reference to the current control flow graph.
    fn current_block(&self) -> &Block<infer::Model> {
        &self.intermediate_representation.control_flow_graph
            [self.current_block_id]
    }

    /// Returns a mutable reference to the current control flow graph.
    fn current_block_mut(&mut self) -> &mut Block<infer::Model> {
        &mut self.intermediate_representation.control_flow_graph
            [self.current_block_id]
    }

    /// Creates a new type inference variable and assigns it to the inference
    /// context with the given constraint.
    fn create_type_inference(
        &mut self,
        constraint: r#type::Constraint,
    ) -> InferenceVariable<Type<infer::Model>> {
        let inference_variable = InferenceVariable::new();
        assert!(self
            .inference_context
            .register::<Type<_>>(inference_variable, constraint));

        inference_variable
    }

    /// Creates an environment object that includes the `active_premise`,
    ///
    /// `table`, and `inference_context` normalizer.
    fn create_environment(
        &self,
    ) -> Environment<'_, infer::Model, S, infer::Context, TO> {
        let (environment, _) = Environment::new_with(
            self.premise.clone(),
            self.table,
            &self.inference_context,
            &self.type_system_observer,
        );

        environment
    }

    /// Creates a new register and assigns the given `assignment` to it.
    fn create_register_assignmnet(
        &mut self,
        assignment: Assignment<infer::Model>,
        span: Option<Span>,
    ) -> ID<Register<infer::Model>> {
        let register_id = self
            .intermediate_representation
            .registers
            .insert(Register { assignment, span });

        let _ = self.current_block_mut().insert_instruction(
            instruction::Instruction::RegisterAssignment(
                instruction::RegisterAssignment { id: register_id },
            ),
        );

        register_id
    }

    /// Gets the type of the given `register_id`.
    fn type_of_register(
        &self,
        register_id: ID<Register<infer::Model>>,
    ) -> Result<Type<infer::Model>, TypeOfError<infer::Model>> {
        self.intermediate_representation
            .type_of_register(
                register_id,
                self.current_site,
                &self.create_environment(),
            )
            .map(|x| x.result)
    }

    /// Gets the type of the given `value`.
    fn type_of_value(
        &self,
        value: &Value<infer::Model>,
    ) -> Result<Type<infer::Model>, TypeOfError<infer::Model>> {
        match value {
            Value::Register(register_id) => self.type_of_register(*register_id),
            Value::Literal(literal) => Ok(literal.r#type()),
        }
    }

    /// Gets the type of the given `address`.
    fn type_of_address(
        &self,
        address: &Address<infer::Model>,
    ) -> Result<Type<infer::Model>, TypeOfError<infer::Model>> {
        self.intermediate_representation
            .type_of_address(
                address,
                self.current_site,
                &self.create_environment(),
            )
            .map(|x| x.result)
    }

    /// Creates a handler that triggers the suboptimal flag inside this
    /// binder when an error is received.
    fn create_handler_wrapper<'a>(
        &self,
        handler: &'a dyn Handler<Box<dyn error::Error>>,
    ) -> HandlerWrapper<'a> {
        HandlerWrapper { handler, suboptimal: self.suboptimal.clone() }
    }

    fn verify_generic_arguments_for_with_inference(
        &mut self,
        generic_arguments: term::GenericArguments<infer::Model>,
        resolved_id: GenericID,
        generic_identifier_span: Span,
        include_suboptimal_flag: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> term::GenericArguments<infer::Model> {
        let handler_wrapper = self.create_handler_wrapper(handler);
        let mut type_inferences = InferenceProvider::default();
        let mut constant_inferences = InferenceProvider::default();

        let generic_arguments = self.table.verify_generic_arguments_for(
            generic_arguments,
            resolved_id,
            generic_identifier_span,
            resolution::Config {
                elided_lifetime_provider: Some(&mut LifetimeInferenceProvider),
                elided_type_provider: Some(&mut type_inferences),
                elided_constant_provider: Some(&mut constant_inferences),
                observer: Some(&mut self.resolution_observer),
                higher_ranked_lifetimes: None,
            },
            if include_suboptimal_flag { &handler_wrapper } else { handler },
        );

        for inference in type_inferences.cerated_inferences {
            assert!(self.inference_context.register::<Type<_>>(
                inference,
                r#type::Constraint::All(false)
            ));
        }

        for inference in constant_inferences.cerated_inferences {
            assert!(self
                .inference_context
                .register::<Constant<_>>(inference, NoConstraint));
        }

        generic_arguments
    }

    fn resolve_generic_arguments_with_inference(
        &mut self,
        generic_arguments: &syntax_tree::GenericArguments,
        include_suboptimal_flag: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<
        term::GenericArguments<infer::Model>,
        resolution::ResolveTermError,
    > {
        let handler_wrapper = self.create_handler_wrapper(handler);
        let mut type_inferences = InferenceProvider::default();
        let mut constant_inferences = InferenceProvider::default();

        let resolved_generic_arguments = self.table.resolve_generic_arguments(
            generic_arguments,
            self.current_site,
            resolution::Config {
                elided_lifetime_provider: Some(&mut LifetimeInferenceProvider),
                elided_type_provider: Some(&mut type_inferences),
                elided_constant_provider: Some(&mut constant_inferences),
                observer: Some(&mut self.resolution_observer),
                higher_ranked_lifetimes: None,
            },
            if include_suboptimal_flag { &handler_wrapper } else { handler },
        )?;

        for inference in type_inferences.cerated_inferences {
            assert!(self.inference_context.register::<Type<_>>(
                inference,
                r#type::Constraint::All(false)
            ));
        }

        for inference in constant_inferences.cerated_inferences {
            assert!(self
                .inference_context
                .register::<Constant<_>>(inference, NoConstraint));
        }

        Ok(resolved_generic_arguments)
    }

    fn resolve_with_inference(
        &mut self,
        syntax_tree: &syntax_tree::QualifiedIdentifier,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Option<Resolution<infer::Model>> {
        let handler = self.create_handler_wrapper(handler);

        let mut type_inferences = InferenceProvider::default();
        let mut constant_inferences = InferenceProvider::default();

        let resolution = self
            .table
            .resolve(
                syntax_tree,
                self.current_site,
                resolution::Config {
                    elided_lifetime_provider: Some(
                        &mut InferenceProvider::default(),
                    ),
                    elided_type_provider: Some(&mut type_inferences),
                    elided_constant_provider: Some(&mut constant_inferences),
                    observer: Some(&mut self.resolution_observer),
                    higher_ranked_lifetimes: None,
                },
                &handler,
            )
            .ok()?;

        for inference in type_inferences.cerated_inferences {
            assert!(self.inference_context.register::<Type<_>>(
                inference,
                r#type::Constraint::All(false)
            ));
        }

        for inference in constant_inferences.cerated_inferences {
            assert!(self
                .inference_context
                .register::<Constant<_>>(inference, NoConstraint));
        }

        Some(resolution)
    }

    /// Creates a new scope at the current instruction pointer and pushes it to
    /// the stack.
    fn push_scope(&mut self) -> ID<Scope> {
        let scope_id = self
            .intermediate_representation
            .scope_tree
            .new_child_branch(
                self.stack.current_scope().scope_id(),
                NonZeroUsize::new(1).unwrap(),
            )
            .unwrap()[0];

        self.stack.push_scope(scope_id);
        let _ = self
            .current_block_mut()
            .insert_instruction(Instruction::ScopePush(ScopePush(scope_id)));

        scope_id
    }

    /// Pops the current scope from the stack.
    fn pop_scope(&mut self, scope_id: ID<Scope>) {
        assert_eq!(
            self.stack.pop_scope().map(|x| x.scope_id()),
            Some(scope_id)
        );
        let _ = self
            .current_block_mut()
            .insert_instruction(Instruction::ScopePop(ScopePop(scope_id)));
    }

    /// Resolves the given `syntax_tree` to a type where inference is allowed.
    fn resolve_type_with_inference(
        &mut self,
        syntax_tree: &syntax_tree::r#type::Type,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Option<Type<infer::Model>> {
        let handler = self.create_handler_wrapper(handler);

        let mut type_inferences = InferenceProvider::default();
        let mut constant_inferences = InferenceProvider::default();

        let ty = self
            .table
            .resolve_type(
                syntax_tree,
                self.current_site,
                resolution::Config {
                    elided_lifetime_provider: Some(
                        &mut InferenceProvider::default(),
                    ),
                    elided_type_provider: Some(&mut type_inferences),
                    elided_constant_provider: Some(&mut constant_inferences),
                    observer: Some(&mut self.resolution_observer),
                    higher_ranked_lifetimes: None,
                },
                &handler,
            )
            .ok()?;

        for inference in type_inferences.cerated_inferences {
            assert!(self.inference_context.register::<Type<_>>(
                inference,
                r#type::Constraint::All(false)
            ));
        }

        for inference in constant_inferences.cerated_inferences {
            assert!(self
                .inference_context
                .register::<Constant<_>>(inference, NoConstraint));
        }

        Some(ty)
    }

    fn is_behind_reference(
        &self,
        address: &Address<infer::Model>,
    ) -> Option<Qualifier> {
        match address {
            Address::Memory(Memory::Alloca(_) | Memory::Parameter(_)) => None,

            Address::Memory(Memory::ReferenceValue(value)) => {
                let ty = self.type_of_value(value).unwrap();
                let ref_ty = match ty {
                    Type::Reference(ref_ty) => ref_ty,
                    found => {
                        panic!("expected a reference type, found: {found:#?}",);
                    }
                };

                Some(ref_ty.qualifier)
            }

            Address::Field(ad) => self.is_behind_reference(&ad.struct_address),
            Address::Tuple(ad) => self.is_behind_reference(&ad.tuple_address),
            Address::Index(ad) => self.is_behind_reference(&ad.array_address),
            Address::Variant(ad) => self.is_behind_reference(&ad.enum_address),
            Address::ReferenceAddress(ad) => {
                let ty = self.type_of_address(&ad.reference_address).unwrap();
                let ref_ty = match ty {
                    Type::Reference(ref_ty) => ref_ty,
                    found => {
                        panic!("expected a reference type, found: {found:#?}",);
                    }
                };

                let mut qualifier = ref_ty.qualifier;

                if let Some(inner_qual) =
                    self.is_behind_reference(&ad.reference_address)
                {
                    qualifier = qualifier.min(inner_qual);
                }

                Some(qualifier)
            }
        }
    }

    /// Performs type checking on the given `ty`.
    ///
    /// This function performs type inference as well as type checking. Any
    /// error, error found will make the binder suboptimal.
    ///
    /// # Parameters
    ///
    /// - `ty`: The type to check.
    /// - `expected_ty`: The type or constraint that `ty` should satisfy.
    /// - `type_check_span`: The span of the type check. This is used for error
    ///   reoprting.
    /// - `handler`: The handler to report errors to.
    ///
    /// # Panics
    ///
    /// This function panics if an unregistered inference variable is found.
    ///
    /// # Errors
    ///
    /// If the type check fails, an error is returned with the span of
    /// `type_check_span`
    fn type_check(
        &mut self,
        ty: &Type<infer::Model>,
        expected_ty: Expected<infer::Model>,
        type_check_span: Span,
        include_suboptimal_flag: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), SemanticError> {
        let environment = self.create_environment();

        // simplify the types
        let simplified_ty = simplify(ty, &environment).result;

        match expected_ty {
            Expected::Known(expected_ty) => {
                let simplified_expected =
                    simplify(&expected_ty, &environment).result;

                let error: Option<Box<dyn error::Error>> = match self
                    .inference_context
                    .unify_type(
                        &simplified_ty,
                        &simplified_expected,
                        &self.premise,
                        self.table,
                        &self.type_system_observer,
                    ) {
                    Ok(()) => None,

                    Err(
                        UnifyError::UnregisteredConstantInferenceVariable(_)
                        | UnifyError::UnregisteredTypeInferenceVariable(_),
                    ) => panic!("unregistered inference variable"),

                    Err(
                        UnifyError::CyclicTypeInference(_)
                        | UnifyError::CyclicConstantInference(_),
                    ) => Some(Box::new(CyclicInference {
                        first: self
                            .inference_context
                            .into_constraint_model(simplified_ty)
                            .unwrap(),
                        second: self
                            .inference_context
                            .into_constraint_model(simplified_expected)
                            .unwrap(),
                        span: type_check_span.clone(),
                    })),

                    Err(
                        UnifyError::IncompatibleTypes { .. }
                        | UnifyError::IncompatibleConstants { .. }
                        | UnifyError::ExceedLimitError(_)
                        | UnifyError::UnsatisfiedConstraint(_)
                        | UnifyError::CombineConstraint(_),
                    ) => Some(Box::new(MismatchedType {
                        expected_type: self
                            .inference_context
                            .into_constraint_model(simplified_expected)
                            .unwrap(),
                        found_type: self
                            .inference_context
                            .into_constraint_model(simplified_ty)
                            .unwrap(),
                        span: type_check_span.clone(),
                    })),
                };

                // report the error
                if let Some(error) = error {
                    if include_suboptimal_flag {
                        self.create_handler_wrapper(handler).receive(error);
                    } else {
                        handler.receive(error);
                    }

                    Err(SemanticError(type_check_span))
                } else {
                    Ok(())
                }
            }
            Expected::Constraint(constraint) => {
                let result =
                    if let Type::Inference(inference_var) = simplified_ty {
                        self.inference_context
                            .unify_with_constraint(inference_var, &constraint)
                            .is_ok()
                    } else {
                        constraint.satisfies(&simplified_ty)
                    };

                // report the error
                if result {
                    Ok(())
                } else {
                    let error = Box::new(MismatchedType {
                        expected_type: Type::Inference(constraint),
                        found_type: self
                            .inference_context
                            .into_constraint_model(simplified_ty)
                            .unwrap(),
                        span: type_check_span.clone(),
                    });

                    if include_suboptimal_flag {
                        self.create_handler_wrapper(handler).receive(error);
                    } else {
                        handler.receive(error);
                    }

                    Err(SemanticError(type_check_span))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests;
