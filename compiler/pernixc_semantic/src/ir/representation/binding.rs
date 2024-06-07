//! Contains the definition of [`Binder`], the struct used for building the IR.

use std::sync::Arc;

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
        control_flow_graph::Block,
        instruction::{self, ScopePush},
        pattern::NameBindingPoint,
        register::{Assignment, Register},
    },
    semantic::{
        instantiation::{self, Instantiation},
        model::Model as _,
        simplify::{self, simplify},
        term::{
            self,
            constant::Constant,
            lifetime::Lifetime,
            r#type::{self, Expected, Type},
            GenericArguments, Symbol, Tuple, TupleElement,
        },
        visitor::RecursiveIterator,
        Environment, Premise,
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
};

pub mod expression;
pub mod infer;
mod pattern;
pub mod stack;
pub mod statement;

/// The binder used for building the IR.
#[derive(Debug)]
pub struct Binder<'t, S: table::State, O: Observer<S, infer::Model>> {
    table: &'t Table<S>,
    resolution_observer: O,
    current_site: GlobalID,
    premise: Premise<infer::Model>,
    stack: Stack,
    constant: bool,

    intermediate_representation: Representation<infer::Model>,
    current_block_id: ID<Block<infer::Model>>,

    inference_context: infer::Context,

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
        is_const: bool,
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
            constant: is_const,
            intermediate_representation,
            stack,
            current_block_id,

            inference_context: Context::default(),

            resolution_observer,

            suboptimal: handler.suboptimal.clone(),
        };

        let mut parameter_name_binding_point = NameBindingPoint::default();

        let root_scope_id =
            binder.intermediate_representation.scope_tree.root_scope_id();

        binder.current_block_mut().insert_basic(
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

/// Is an error occurred while binding the syntax tree
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error("encountered a fatal semantic error")]
pub struct Error(pub Span);

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
        Environment {
            premise: &self.premise,
            table: self.table,
            normalizer: &self.inference_context,
        }
    }

    /// Creates a new register and assigns the given `assignment` to it.
    fn create_register_assignmnet(
        &mut self,
        assignment: Assignment<infer::Model>,
        ty: Type<infer::Model>,
        span: Option<Span>,
    ) -> ID<Register<infer::Model>> {
        let register_id = self
            .intermediate_representation
            .registers
            .insert(Register { assignment, r#type: ty, span });

        self.current_block_mut().insert_basic(
            instruction::Instruction::RegisterAssignment(
                instruction::RegisterAssignment { id: register_id },
            ),
        );

        register_id
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
                    higher_ranked_liftimes: None,
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
                    higher_ranked_liftimes: None,
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

    fn get_address_type(
        &self,
        address: &Address<Memory<infer::Model>>,
    ) -> Type<infer::Model> {
        match address {
            Address::Base(base) => match base {
                Memory::Parameter(parameter_id) => {
                    infer::Model::from_default_type(match self.current_site {
                        GlobalID::Function(id) => self
                            .table
                            .get(id)
                            .unwrap()
                            .parameters()
                            .get(*parameter_id)
                            .unwrap()
                            .r#type
                            .clone(),
                        GlobalID::TraitFunction(id) => self
                            .table
                            .get(id)
                            .unwrap()
                            .parameters()
                            .get(*parameter_id)
                            .unwrap()
                            .r#type
                            .clone(),
                        GlobalID::TraitImplementationFunction(id) => self
                            .table
                            .get(id)
                            .unwrap()
                            .parameters()
                            .get(*parameter_id)
                            .unwrap()
                            .r#type
                            .clone(),
                        GlobalID::AdtImplementationFunction(id) => self
                            .table
                            .get(id)
                            .unwrap()
                            .parameters()
                            .get(*parameter_id)
                            .unwrap()
                            .r#type
                            .clone(),
                        _ => {
                            panic!(
                                "the parameter id appears in an unexpected \
                                 site"
                            )
                        }
                    })
                }
                Memory::Alloca(alloca_id) => self
                    .intermediate_representation
                    .allocas
                    .get(*alloca_id)
                    .unwrap()
                    .r#type
                    .clone(),

                Memory::ReferenceValue(register) => {
                    let mut ty = self
                        .intermediate_representation
                        .registers
                        .get(*register)
                        .unwrap()
                        .r#type
                        .clone();

                    ty = simplify::simplify(&ty, &self.create_environment());

                    let Type::Reference(reference) = ty else {
                        panic!("expected a reference type")
                    };

                    *reference.pointee
                }
            },
            Address::Field(field_address) => {
                let mut struct_address = self.get_address_type(address);

                // simplify the struct type
                struct_address = simplify::simplify(
                    &struct_address,
                    &self.create_environment(),
                );

                let Type::Symbol(Symbol {
                    id: r#type::SymbolID::Struct(struct_id),
                    generic_arguments,
                }) = struct_address
                else {
                    panic!("expected a struct type")
                };

                let struct_sym = self.table.get(struct_id).unwrap();
                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments,
                    struct_id.into(),
                    &struct_sym.generic_declaration.parameters,
                )
                .unwrap();
                let mut field_ty = infer::Model::from_default_type(
                    struct_sym
                        .fields()
                        .get(field_address.id)
                        .unwrap()
                        .r#type
                        .clone(),
                );

                instantiation::instantiate(&mut field_ty, &instantiation);

                field_ty
            }
            Address::Tuple(tuple_address) => {
                let mut tuple_ty =
                    self.get_address_type(&tuple_address.tuple_address);

                // simplfiy the tuple type
                tuple_ty =
                    simplify::simplify(&tuple_ty, &self.create_environment());

                let Type::Tuple(mut tuple_ty) = tuple_ty else {
                    panic!("expected a tuple type")
                };

                let tuple_elem = match tuple_address.offset {
                    crate::ir::address::Offset::FromStart(id) => {
                        tuple_ty.elements.remove(id)
                    }
                    crate::ir::address::Offset::FromEnd(id) => tuple_ty
                        .elements
                        .remove(tuple_ty.elements.len() - id - 1),
                };

                if tuple_elem.is_unpacked {
                    Type::Tuple(Tuple {
                        elements: vec![TupleElement {
                            term: tuple_elem.term,
                            is_unpacked: true,
                        }],
                    })
                } else {
                    tuple_elem.term
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
    ) -> Result<(), Error> {
        // simplify the types
        let simplified_ty = simplify(&ty, &Environment {
            premise: &self.premise,
            table: self.table,
            normalizer: &self.inference_context,
        });

        match expected_ty {
            Expected::Known(expected_ty) => {
                let simplified_expected =
                    simplify(&expected_ty, &Environment {
                        premise: &self.premise,
                        table: self.table,
                        normalizer: &self.inference_context,
                    });

                let result = match self.inference_context.unify_type(
                    &simplified_ty,
                    &simplified_expected,
                    &self.premise,
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

                    Err(Error(type_check_span))
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

                    Err(Error(type_check_span))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests;
