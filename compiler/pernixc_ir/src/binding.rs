//! Contains the definition of [`Binder`], the struct used for building the IR.

use std::{borrow::Cow, collections::HashMap, num::NonZeroUsize, ops::Deref};

use diagnostic::{CyclicInference, MismatchedType};
use enum_as_inner::EnumAsInner;
use getset::Getters;
use infer::{
    Constraint as _, Context, Expected, InferenceVariable, UnifyError,
};
use pernixc_abort::Abort;
use pernixc_arena::ID;
use pernixc_component::function_signature::FunctionSignature;
use pernixc_handler::Handler;
use pernixc_resolution::{
    qualified_identifier::Resolution, ElidedTermProvider, Ext, ExtraNamespace,
    GetGenericParameterNamespaceExt,
};
use pernixc_source_file::{SourceElement, Span};
use pernixc_syntax::syntax_tree;
use pernixc_table::{diagnostic::Diagnostic, GlobalID, Table};
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    lifetime::Lifetime,
    r#type::{Qualifier, Type},
    Model,
};
use pernixc_type_system::{
    environment::{Environment, GetActivePremiseExt, Premise},
    term::Term,
};
use stack::Stack;

use crate::{
    address::{Address, Memory},
    alloca::Alloca,
    control_flow_graph::Block,
    instruction::{self, Instruction, ScopePop, ScopePush},
    model::{Constraint, Erased, NoConstraint},
    pattern::{NameBindingPoint, Wildcard},
    scope,
    value::{
        literal::{Literal, Unreachable},
        register::{Assignment, Register},
        Value,
    },
    Representation,
};

pub mod diagnostic;
pub mod expression;
pub mod stack;
pub mod statement;

mod finalize;
mod infer;
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
pub struct Binder<'t> {
    table: &'t Table,

    current_site: GlobalID,
    premise: Premise<infer::Model>,
    extra_namespace: ExtraNamespace<infer::Model>,
    stack: Stack,

    /// The intermediate representation that is being built.
    #[get = "pub"]
    intermediate_representation: Representation<infer::Model>,
    current_block_id: ID<Block<infer::Model>>,

    inference_context: infer::Context,

    block_states_by_scope_id: HashMap<ID<scope::Scope>, BlockState>,
    loop_states_by_scope_id: HashMap<ID<scope::Scope>, LoopState>,
}

impl<'t> Binder<'t> {
    /// Creates the binder for building the IR.
    ///
    /// # Errors
    ///
    /// See [`Abort`] for more information
    pub fn new_function<'a>(
        table: &'t Table,
        function_id: GlobalID,
        parameter_pattern_syns: impl IntoIterator<
            Item = &'a syntax_tree::pattern::Irrefutable,
        >,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Self, Abort> {
        let premise = table.get_active_premise::<infer::Model>(function_id);
        let generic_parameter_namespace =
            table.get_generic_parameter_namepsace::<infer::Model>(function_id);

        let intermediate_representation = Representation::default();
        let current_block_id =
            intermediate_representation.control_flow_graph.entry_block_id();

        let stack = Stack::new(
            intermediate_representation.scope_tree.root_scope_id(),
            false,
        );

        let mut binder = Self {
            table,
            current_site: function_id,
            premise,
            intermediate_representation,
            stack,
            extra_namespace: generic_parameter_namespace,
            current_block_id,

            inference_context: Context::default(),

            block_states_by_scope_id: HashMap::new(),
            loop_states_by_scope_id: HashMap::new(),
        };

        let mut parameter_name_binding_point = NameBindingPoint::default();

        let root_scope_id =
            binder.intermediate_representation.scope_tree.root_scope_id();

        let _ = binder.current_block_mut().add_instruction(
            instruction::Instruction::ScopePush(ScopePush(root_scope_id)),
        );

        let function_signature =
            binder.table.query::<FunctionSignature>(function_id)?;

        // bind the parameter patterns
        #[allow(clippy::significant_drop_in_scrutinee)]
        for ((parameter_id, parameter_sym), syntax_tree) in function_signature
            .parameter_order
            .iter()
            .copied()
            .map(|x| (x, &function_signature.parameters[x]))
            .zip(parameter_pattern_syns)
        {
            let parameter_type =
                infer::Model::from_default_type(parameter_sym.r#type.clone());

            // TODO: add the binding point
            let pattern = binder
                .bind_pattern(&parameter_type, syntax_tree, handler)?
                .unwrap_or_else(|| {
                    Wildcard { span: syntax_tree.span() }.into()
                });

            binder.insert_irrefutable_named_binding_point(
                &mut parameter_name_binding_point,
                &pattern,
                &parameter_type,
                Address::Memory(Memory::Parameter(parameter_id)),
                None,
                Qualifier::Mutable,
                false,
                root_scope_id,
                handler,
            )?;
        }

        binder
            .stack
            .current_scope_mut()
            .add_named_binding_point(parameter_name_binding_point);

        Ok(binder)
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error(
    "encountered a fatal semantic error that cannot be recovered while \
     binding an expression."
)]
#[allow(missing_docs)]
pub struct BindingError(pub Span);

/// Is an error occurred while binding the syntax tree
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    Binding(#[from] BindingError),

    /// Encountered an error and the diagnostic is already reported. The binder
    /// should not be used anymore after encountered this error variant, the
    /// state of the binder is corrupted.
    #[error(transparent)]
    Unrecoverable(#[from] Abort),
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
    created_inferences: Vec<T>,
}

impl<T: Default + Clone, U: Term + From<T>> ElidedTermProvider<U>
    for InferenceProvider<T>
{
    fn create(&mut self) -> U {
        let inference = T::default();
        let inference_term = inference.clone().into();
        self.created_inferences.push(inference);

        inference_term
    }
}

impl Binder<'_> {
    fn create_unreachable(&mut self, span: Span) -> Literal<infer::Model> {
        Literal::Unreachable(Unreachable {
            r#type: {
                let inference = InferenceVariable::<Type<_>>::new();

                assert!(self
                    .inference_context
                    .register(inference, Constraint::All(true)));

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
    fn create_alloca_with_scope_id(
        &mut self,
        r#type: Type<infer::Model>,
        scope_id: ID<scope::Scope>,
        span: Span,
    ) -> ID<Alloca<infer::Model>> {
        let alloca_id =
            self.intermediate_representation.values.allocas.insert(Alloca {
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
    fn create_alloca(
        &mut self,
        r#type: Type<infer::Model>,
        span: Span,
    ) -> ID<Alloca<infer::Model>> {
        let alloca_id =
            self.intermediate_representation.values.allocas.insert(Alloca {
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

    /// Creates a new alloca and adds it to the current scope. The type and span
    /// of the alloca is dedcued from the value.
    fn create_alloca_with_value(
        &mut self,
        value: Value<infer::Model>,
        scope_id: ID<scope::Scope>,
        address_span: Option<Span>,
        store_span: Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<ID<Alloca<infer::Model>>, Abort> {
        let ty = self.type_of_value(&value, handler)?;
        let span = address_span.unwrap_or_else(|| match &value {
            Value::Register(id) => self
                .intermediate_representation
                .values
                .registers
                .get(*id)
                .unwrap()
                .span
                .clone()
                .unwrap(),
            Value::Literal(literal) => literal.span().cloned().unwrap(),
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
        constraint: Constraint,
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
    ) -> Environment<'_, infer::Model, infer::Context> {
        let environment = Environment::new(
            Cow::Borrowed(&self.premise),
            self.table,
            &self.inference_context,
        );

        environment
    }

    /// Creates a new register and assigns the given `assignment` to it.
    fn create_register_assignmnet(
        &mut self,
        assignment: Assignment<infer::Model>,
        span: Span,
    ) -> ID<Register<infer::Model>> {
        let register_id = self
            .intermediate_representation
            .values
            .registers
            .insert(Register { assignment, span: Some(span) });

        let _ = self.current_block_mut().add_instruction(
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
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Type<infer::Model>, Abort> {
        self.intermediate_representation
            .values
            .type_of_register(
                register_id,
                self.current_site,
                &self.create_environment(),
            )
            .map(|x| x.result)
            .map_err(|x| {
                x.report_overflow(|x| {
                    x.report_as_type_calculating_overflow(
                        self.intermediate_representation.values.registers
                            [register_id]
                            .span
                            .clone()
                            .unwrap(),
                        handler,
                    )
                })
            })
    }

    /// Gets the type of the given `value`.
    fn type_of_value(
        &self,
        value: &Value<infer::Model>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Type<infer::Model>, Abort> {
        match value {
            Value::Register(register_id) => {
                self.type_of_register(*register_id, handler)
            }
            Value::Literal(literal) => Ok(self
                .create_environment()
                .simplify(literal.r#type())
                .map_err(|x| {
                    x.report_overflow(|x| {
                        x.report_as_type_calculating_overflow(
                            literal.span().cloned().unwrap(),
                            handler,
                        )
                    })
                })?
                .deref()
                .result
                .clone()),
        }
    }

    /// Gets the type of the given `address`.
    fn type_of_address(
        &self,
        address: &Address<infer::Model>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Type<infer::Model>, Abort> {
        self.intermediate_representation
            .values
            .type_of_address(
                address,
                self.current_site,
                &self.create_environment(),
            )
            .map(|x| x.result)
            .map_err(|x| {
                x.report_overflow(|x| {
                    x.report_as_type_calculating_overflow(
                        match address.get_root_memory() {
                            Memory::Parameter(id) => {
                                let signature = match self
                                    .table
                                    .query::<FunctionSignature>(
                                    self.current_site,
                                ) {
                                    Ok(signature) => signature,
                                    Err(error) => return error,
                                };

                                let parameter = &signature.parameters[*id];

                                parameter.span.clone().unwrap()
                            }
                            Memory::Alloca(id) => {
                                self.intermediate_representation.values.allocas
                                    [*id]
                                    .span
                                    .clone()
                                    .unwrap()
                            }
                        },
                        handler,
                    )
                })
            })
    }

    #[allow(clippy::type_complexity)]
    fn verify_generic_arguments_for_with_inference(
        &mut self,
        generic_arguments: GenericArguments<infer::Model>,
        resolved_id: GlobalID,
        generic_identifier_span: Span,
    ) -> Result<(GenericArguments<infer::Model>, Vec<Box<dyn Diagnostic>>), Abort>
    {
        let mut type_inferences = InferenceProvider::default();
        let mut constant_inferences = InferenceProvider::default();

        let (generic_arguments, diagnostics) =
            self.table.verify_generic_arguments_for(
                generic_arguments,
                resolved_id,
                generic_identifier_span,
                pernixc_resolution::Config {
                    elided_lifetime_provider: Some(
                        &mut LifetimeInferenceProvider,
                    ),
                    elided_type_provider: Some(&mut type_inferences),
                    elided_constant_provider: Some(&mut constant_inferences),
                    observer: None,
                    extra_namespace: Some(&self.extra_namespace),
                },
            )?;

        for inference in type_inferences.created_inferences {
            assert!(self
                .inference_context
                .register::<Type<_>>(inference, Constraint::All(false)));
        }

        for inference in constant_inferences.created_inferences {
            assert!(self
                .inference_context
                .register::<Constant<_>>(inference, NoConstraint));
        }

        Ok((generic_arguments, diagnostics))
    }

    fn resolve_generic_arguments_with_inference(
        &mut self,
        generic_arguments: &syntax_tree::GenericArguments,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> GenericArguments<infer::Model> {
        let mut type_inferences = InferenceProvider::default();
        let mut constant_inferences = InferenceProvider::default();

        let resolved_generic_arguments = self.table.resolve_generic_arguments(
            generic_arguments,
            self.current_site,
            pernixc_resolution::Config {
                elided_lifetime_provider: Some(&mut LifetimeInferenceProvider),
                elided_type_provider: Some(&mut type_inferences),
                elided_constant_provider: Some(&mut constant_inferences),
                observer: None,
                extra_namespace: Some(&self.extra_namespace),
            },
            handler,
        );

        for inference in type_inferences.created_inferences {
            assert!(self
                .inference_context
                .register::<Type<_>>(inference, Constraint::All(false)));
        }

        for inference in constant_inferences.created_inferences {
            assert!(self
                .inference_context
                .register::<Constant<_>>(inference, NoConstraint));
        }

        resolved_generic_arguments
    }

    fn resolve_qualified_identifier_with_inference(
        &mut self,
        syntax_tree: &syntax_tree::QualifiedIdentifier,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Resolution<infer::Model>, Abort> {
        let mut type_inferences = InferenceProvider::default();
        let mut constant_inferences = InferenceProvider::default();

        let resolution = self.table.resolve_qualified_identifier(
            syntax_tree,
            self.current_site,
            pernixc_resolution::Config {
                elided_lifetime_provider: Some(
                    &mut InferenceProvider::<Erased>::default(),
                ),
                elided_type_provider: Some(&mut type_inferences),
                elided_constant_provider: Some(&mut constant_inferences),
                observer: None,
                extra_namespace: Some(&self.extra_namespace),
            },
            handler,
        )?;

        for inference in type_inferences.created_inferences {
            assert!(self
                .inference_context
                .register::<Type<_>>(inference, Constraint::All(false)));
        }

        for inference in constant_inferences.created_inferences {
            assert!(self
                .inference_context
                .register::<Constant<_>>(inference, NoConstraint));
        }

        Ok(resolution)
    }

    fn push_scope_with(&mut self, scope_id: ID<scope::Scope>, is_unsafe: bool) {
        self.stack.push_scope(scope_id, is_unsafe);
        let _ = self
            .current_block_mut()
            .add_instruction(Instruction::ScopePush(ScopePush(scope_id)));
    }

    /// Creates a new scope at the current instruction pointer and pushes it to
    /// the stack.
    fn push_scope(&mut self, is_unsafe: bool) -> ID<scope::Scope> {
        let scope_id = self
            .intermediate_representation
            .scope_tree
            .new_child_branch(
                self.stack.current_scope().scope_id(),
                NonZeroUsize::new(1).unwrap(),
            )
            .unwrap()[0];

        self.push_scope_with(scope_id, is_unsafe);

        scope_id
    }

    /// Pops the current scope from the stack.
    fn pop_scope(&mut self, scope_id: ID<scope::Scope>) {
        assert_eq!(
            self.stack.pop_scope().map(|x| x.scope_id()),
            Some(scope_id)
        );
        let _ = self
            .current_block_mut()
            .add_instruction(Instruction::ScopePop(ScopePop(scope_id)));
    }

    /// Resolves the given `syntax_tree` to a type where inference is allowed.
    fn resolve_type_with_inference(
        &mut self,
        syntax_tree: &syntax_tree::r#type::Type,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Type<infer::Model> {
        let mut type_inferences = InferenceProvider::default();
        let mut constant_inferences = InferenceProvider::default();

        let ty = self.table.resolve_type(
            syntax_tree,
            self.current_site,
            pernixc_resolution::Config {
                elided_lifetime_provider: Some(
                    &mut InferenceProvider::<Erased>::default(),
                ),
                elided_type_provider: Some(&mut type_inferences),
                elided_constant_provider: Some(&mut constant_inferences),
                observer: None,
                extra_namespace: Some(&self.extra_namespace),
            },
            handler,
        );

        for inference in type_inferences.created_inferences {
            assert!(self
                .inference_context
                .register::<Type<_>>(inference, Constraint::All(false)));
        }

        for inference in constant_inferences.created_inferences {
            assert!(self
                .inference_context
                .register::<Constant<_>>(inference, NoConstraint));
        }

        ty
    }

    fn get_behind_reference_qualifier(
        &self,
        address: &Address<infer::Model>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Option<Qualifier>, Abort> {
        match address {
            Address::Memory(Memory::Alloca(_) | Memory::Parameter(_)) => {
                Ok(None)
            }

            Address::Field(ad) => {
                self.get_behind_reference_qualifier(&ad.struct_address, handler)
            }
            Address::Tuple(ad) => {
                self.get_behind_reference_qualifier(&ad.tuple_address, handler)
            }
            Address::Index(ad) => {
                self.get_behind_reference_qualifier(&ad.array_address, handler)
            }
            Address::Variant(ad) => {
                self.get_behind_reference_qualifier(&ad.enum_address, handler)
            }
            Address::Reference(ad) => {
                let ty =
                    self.type_of_address(&ad.reference_address, handler)?;

                let ref_ty = match ty {
                    Type::Reference(ref_ty) => ref_ty,
                    found => {
                        panic!("expected a reference type, found: {found:#?}",);
                    }
                };

                let mut qualifier = ref_ty.qualifier;

                if let Some(inner_qual) = self.get_behind_reference_qualifier(
                    &ad.reference_address,
                    handler,
                )? {
                    qualifier = qualifier.min(inner_qual);
                }

                Ok(Some(qualifier))
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn type_check_as_diagnostic(
        &mut self,
        ty: &Type<infer::Model>,
        expected_ty: Expected<infer::Model>,
        type_check_span: Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Option<Box<dyn Diagnostic>>, Abort> {
        let environment = self.create_environment();

        // simplify the types
        let simplified_ty = environment.simplify(ty.clone()).map_err(|x| {
            x.report_overflow(|x| {
                x.report_as_type_calculating_overflow(
                    type_check_span.clone(),
                    handler,
                )
            })
        })?;

        match expected_ty {
            Expected::Known(expected_ty) => {
                let simplified_expected =
                    environment.simplify(expected_ty).map_err(|x| {
                        x.report_overflow(|x| {
                            x.report_as_type_calculating_overflow(
                                type_check_span.clone(),
                                handler,
                            )
                        })
                    })?;

                let error: Option<Box<dyn Diagnostic>> = match self
                    .inference_context
                    .unify_type(
                        &simplified_ty.result,
                        &simplified_expected.result,
                        &self.premise,
                        self.table,
                    ) {
                    Ok(()) => None,

                    Err(
                        UnifyError::CyclicTypeInference(_)
                        | UnifyError::CyclicConstantInference(_),
                    ) => Some(Box::new(CyclicInference {
                        first: self
                            .inference_context
                            .transform_type_into_constraint_model(
                                simplified_ty.result.clone(),
                                type_check_span.clone(),
                                self.table,
                                handler,
                            )?,
                        second: self
                            .inference_context
                            .transform_type_into_constraint_model(
                                simplified_expected.result.clone(),
                                type_check_span.clone(),
                                self.table,
                                handler,
                            )?,
                        span: type_check_span,
                    })),

                    Err(UnifyError::TypeSystem(type_system_error)) => {
                        return Err(type_system_error.report_overflow(|x| {
                            x.report_as_type_check_overflow(
                                type_check_span.clone(),
                                handler,
                            )
                        }));
                    }

                    Err(
                        UnifyError::IncompatibleTypes { .. }
                        | UnifyError::IncompatibleConstants { .. }
                        | UnifyError::UnsatisfiedConstraint(_)
                        | UnifyError::CombineConstraint(_),
                    ) => Some(Box::new(MismatchedType {
                        expected_type: self
                            .inference_context
                            .transform_type_into_constraint_model(
                                simplified_expected.result.clone(),
                                type_check_span.clone(),
                                self.table,
                                handler,
                            )?,
                        found_type: self
                            .inference_context
                            .transform_type_into_constraint_model(
                                simplified_ty.result.clone(),
                                type_check_span.clone(),
                                self.table,
                                handler,
                            )?,
                        span: type_check_span,
                    })),
                };

                // report the error
                Ok(error)
            }
            Expected::Constraint(constraint) => {
                let result = if let Type::Inference(inference_var) =
                    &simplified_ty.result
                {
                    self.inference_context
                        .unify_with_constraint(*inference_var, &constraint)
                        .is_ok()
                } else {
                    constraint.satisfies(&simplified_ty.result)
                };

                // report the error
                if result {
                    Ok(None)
                } else {
                    let error = Box::new(MismatchedType {
                        expected_type: Type::Inference(constraint),
                        found_type: self
                            .inference_context
                            .transform_type_into_constraint_model(
                                simplified_ty.result.clone(),
                                type_check_span.clone(),
                                self.table,
                                handler,
                            )?,
                        span: type_check_span,
                    });

                    Ok(Some(error))
                }
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
    #[allow(clippy::too_many_lines)]
    fn type_check(
        &mut self,
        ty: &Type<infer::Model>,
        expected_ty: Expected<infer::Model>,
        type_check_span: Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<bool, Abort> {
        let error = self.type_check_as_diagnostic(
            ty,
            expected_ty,
            type_check_span,
            handler,
        )?;

        error.map_or(Ok(true), |error| {
            handler.receive(error);
            Ok(false)
        })
    }
}

#[cfg(test)]
mod test;
