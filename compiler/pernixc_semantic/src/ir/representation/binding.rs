//! Contains the definition of [`Binder`], the struct used for building the IR.

use std::{collections::HashMap, sync::Arc};

use getset::Getters;
use infer::{
    Constraint, Context, Erased, InferenceVariable, NoConstraint, UnifyError,
};
use parking_lot::RwLock;
use pernixc_base::{diagnostic::Handler, source_file::Span};
use pernixc_syntax::syntax_tree;
use stack::Stack;

use super::Representation;
use crate::{
    arena::ID,
    error::{self, MismatchedType},
    ir::{
        address::{Address, Memory},
        alloca::Alloca,
        control_flow_graph::Block,
        instruction::{self, ScopePush},
        pattern::NameBindingPoint,
        scope,
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
            resolution::{self, EliidedTermProvider, Observer, Resolution},
            Table,
        },
        FunctionTemplate, GenericTemplate, GlobalID,
    },
    type_system::{
        environment::Environment,
        model::Model,
        simplify::simplify,
        term::{
            self,
            constant::Constant,
            lifetime::Lifetime,
            r#type::{self, Expected, Type},
            GenericArguments,
        },
        visitor::RecursiveIterator,
        Premise,
    },
};

pub mod expression;
pub mod infer;
pub mod stack;
pub mod statement;

mod pattern;

#[derive(Debug, Clone, PartialEq, Eq)]
struct BlockState {
    label: Option<String>,
    incoming_values: HashMap<ID<Block<infer::Model>>, Value<infer::Model>>,
    successor_block_id: ID<Block<infer::Model>>,
    express_type: Option<Type<infer::Model>>,
}

/// The binder used for building the IR.
#[derive(Debug, Getters)]
pub struct Binder<'t, S: table::State, O: Observer<S, infer::Model>> {
    table: &'t Table<S>,
    resolution_observer: O,
    current_site: GlobalID,
    premise: Premise<infer::Model>,
    stack: Stack,

    /// The intermediate representation that is being built.
    #[get = "pub"]
    intermediate_representation: Representation<infer::Model>,
    current_block_id: ID<Block<infer::Model>>,

    inference_context: infer::Context,

    block_states_by_scope_id: HashMap<ID<scope::Scope>, BlockState>,

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

impl<'t, S: table::State, O: Observer<S, infer::Model>> Binder<'t, S, O> {
    /// Creates the binder for building the IR.
    ///
    /// # Errors
    ///
    /// See [`CreateFunctionBinderError`] for the possible errors.
    pub fn new_function<'a, P: Copy, D>(
        table: &'t Table<S>,
        resolution_observer: O,
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

            block_states_by_scope_id: HashMap::new(),

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

            let Ok(pattern) = binder.create_irrefutable(
                syntax_tree,
                &parameter_type,
                &Address::Base(Memory::Parameter(parameter_id)),
                &handler,
            ) else {
                continue;
            };

            // add the binding point
            parameter_name_binding_point
                .add_irrefutable_binding(&pattern, &handler);
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
        Error::Internal(InternalError::TypeOf(error))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct InferenceProvider;

impl EliidedTermProvider<Lifetime<infer::Model>> for InferenceProvider {
    fn create(&mut self) -> Lifetime<infer::Model> {
        Lifetime::Inference(Erased)
    }
}

impl EliidedTermProvider<Type<infer::Model>> for InferenceProvider {
    fn create(&mut self) -> Type<infer::Model> {
        Type::Inference(InferenceVariable::new())
    }
}

impl EliidedTermProvider<Constant<infer::Model>> for InferenceProvider {
    fn create(&mut self) -> Constant<infer::Model> {
        Constant::Inference(InferenceVariable::new())
    }
}

impl<'t, S: table::State, O: Observer<S, infer::Model>> Binder<'t, S, O> {
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
    ) -> Environment<'_, infer::Model, S, infer::Context> {
        let (environment, _) = Environment::new(
            self.premise.clone(),
            self.table,
            &self.inference_context,
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
        self.intermediate_representation.type_of_register(
            register_id,
            self.current_site,
            &self.create_environment(),
        )
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
        address: &Address<Memory<infer::Model>>,
    ) -> Result<Type<infer::Model>, TypeOfError<infer::Model>> {
        self.intermediate_representation.type_of_address(
            address,
            self.current_site,
            &self.create_environment(),
        )
    }

    /// Creates a handler that triggers the suboptimal flag inside this
    /// binder when an error is received.
    fn create_handler_wrapper<'a>(
        &self,
        handler: &'a dyn Handler<Box<dyn error::Error>>,
    ) -> HandlerWrapper<'a> {
        HandlerWrapper { handler, suboptimal: self.suboptimal.clone() }
    }

    fn resolve_with_inference(
        &mut self,
        syntax_tree: &syntax_tree::QualifiedIdentifier,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Option<Resolution<infer::Model>> {
        let handler = self.create_handler_wrapper(handler);

        let resolution = self
            .table
            .resolve(
                syntax_tree,
                self.current_site,
                resolution::Config {
                    ellided_lifetime_provider: Some(&mut InferenceProvider),
                    ellided_type_provider: Some(&mut InferenceProvider),
                    ellided_constant_provider: Some(&mut InferenceProvider),
                    observer: Some(&mut self.resolution_observer),
                    higher_ranked_lifetimes: None,
                },
                &handler,
            )
            .ok()?;

        let mut type_inferences = Vec::new();
        let mut constant_inferences = Vec::new();

        let mut gather_inferences =
            |generic_arguments: &GenericArguments<infer::Model>| {
                for types in generic_arguments.types.iter() {
                    for (kind, _) in RecursiveIterator::new(types) {
                        match kind {
                            term::Kind::Type(Type::Inference(
                                inference_variable,
                            )) => {
                                type_inferences.push(*inference_variable);
                            }
                            term::Kind::Constant(Constant::Inference(
                                inference_variable,
                            )) => {
                                constant_inferences.push(*inference_variable);
                            }
                            _ => {}
                        }
                    }
                }

                for constants in generic_arguments.constants.iter() {
                    for (kind, _) in RecursiveIterator::new(constants) {
                        match kind {
                            term::Kind::Type(Type::Inference(
                                inference_variable,
                            )) => {
                                type_inferences.push(*inference_variable);
                            }
                            term::Kind::Constant(Constant::Inference(
                                inference_variable,
                            )) => {
                                constant_inferences.push(*inference_variable);
                            }
                            _ => {}
                        }
                    }
                }
            };

        match &resolution {
            Resolution::Module(_) => {}
            Resolution::Variant(variant) => {
                gather_inferences(&variant.generic_arguments);
            }
            Resolution::Generic(generic) => {
                gather_inferences(&generic.generic_arguments);
            }
            Resolution::MemberGeneric(generic) => {
                gather_inferences(&generic.generic_arguments);
                gather_inferences(&generic.parent_generic_arguments);
            }
        }

        for inference in type_inferences {
            assert!(self
                .inference_context
                .register::<Type<_>>(inference, r#type::Constraint::All));
        }

        for inference in constant_inferences {
            assert!(self
                .inference_context
                .register::<Constant<_>>(inference, NoConstraint));
        }

        Some(resolution)
    }

    /// Resolves the given `syntax_tree` to a type where inference is allowed.
    fn resolve_type_with_inference(
        &mut self,
        syntax_tree: &syntax_tree::r#type::Type,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Option<Type<infer::Model>> {
        let handler = self.create_handler_wrapper(handler);

        let ty = self
            .table
            .resolve_type(
                syntax_tree,
                self.current_site,
                resolution::Config {
                    ellided_lifetime_provider: Some(&mut InferenceProvider),
                    ellided_type_provider: Some(&mut InferenceProvider),
                    ellided_constant_provider: Some(&mut InferenceProvider),
                    observer: Some(&mut self.resolution_observer),
                    higher_ranked_lifetimes: None,
                },
                &handler,
            )
            .ok()?;

        let mut type_inferences = Vec::new();
        let mut constant_inferences = Vec::new();

        for (kind, _) in RecursiveIterator::new(&ty) {
            match kind {
                term::Kind::Type(Type::Inference(inference_variable)) => {
                    type_inferences.push(*inference_variable);
                }
                term::Kind::Constant(Constant::Inference(
                    inference_variable,
                )) => {
                    constant_inferences.push(*inference_variable);
                }
                _ => {}
            }
        }

        for inference in type_inferences {
            assert!(self
                .inference_context
                .register::<Type<_>>(inference, r#type::Constraint::All));
        }

        for inference in constant_inferences {
            assert!(self
                .inference_context
                .register::<Constant<_>>(inference, NoConstraint));
        }

        Some(ty)
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
    #[must_use]
    fn type_check(
        &mut self,
        ty: Type<infer::Model>,
        expected_ty: Expected<infer::Model>,
        type_check_span: Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), SemanticError> {
        let environment = self.create_environment();

        // simplify the types
        let simplified_ty = simplify(&ty, &environment).result;

        match expected_ty {
            Expected::Known(expected_ty) => {
                let simplified_expected =
                    simplify(&expected_ty, &environment).result;

                let result = match self.inference_context.unify_type(
                    &simplified_ty,
                    &simplified_expected,
                    self.premise.clone(),
                    self.table,
                ) {
                    Ok(()) => true,

                    Err(
                        UnifyError::UnregisteredConstantInferenceVariable(_)
                        | UnifyError::UnregisteredTypeInferenceVariable(_),
                    ) => panic!("unregistered inference variable"),

                    Err(
                        UnifyError::IncompatibleTypes { .. }
                        | UnifyError::IncompatibleConstants { .. }
                        | UnifyError::ExceedLimitError(_)
                        | UnifyError::UnsatisfiedConstraint(_)
                        | UnifyError::CombineConstraint(_),
                    ) => false,
                };

                // report the error
                if result {
                    Ok(())
                } else {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        MismatchedType {
                            expected_type: self
                                .inference_context
                                .into_constraint_model(simplified_expected)
                                .unwrap(),
                            found_type: self
                                .inference_context
                                .into_constraint_model(simplified_ty)
                                .unwrap(),
                            span: type_check_span.clone(),
                        },
                    ));

                    Err(SemanticError(type_check_span))
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
                    self.create_handler_wrapper(handler).receive(Box::new(
                        MismatchedType {
                            expected_type: Type::Inference(constraint),
                            found_type: self
                                .inference_context
                                .into_constraint_model(simplified_ty)
                                .unwrap(),
                            span: type_check_span.clone(),
                        },
                    ));

                    Err(SemanticError(type_check_span))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests;
