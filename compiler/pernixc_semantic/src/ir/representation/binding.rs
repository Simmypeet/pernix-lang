//! Contains the definition of states and models used for building the IR.

use std::sync::Arc;

use infer::{Context, UnifyError};
use parking_lot::RwLock;
use pernixc_base::{diagnostic::Handler, source_file::Span};
use pernixc_syntax::syntax_tree;

use self::infer::InferenceVariable;
use super::Representation;
use crate::{
    arena::ID,
    error::{self, MismatchedType},
    ir::{
        address::Address, control_flow_graph::Block, pattern::NameBindingPoint,
        State,
    },
    semantic::{
        model::{self, Model as _},
        simplify::simplify,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Expected, Type},
            Never, Term,
        },
        Environment, Premise,
    },
    symbol::{
        table::{self, representation::Index, resolution::Observer, Table},
        FunctionTemplate, GenericTemplate, GlobalID,
    },
};

pub mod expression;
pub mod infer;
mod pattern;
pub mod statement;

/// The model used for building the IR
///
/// This model enables the use of inference variables
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Model;

impl<T> From<Never> for InferenceVariable<T> {
    fn from(value: Never) -> Self { match value {} }
}

impl model::Model for Model {
    type LifetimeInference = InferenceVariable<Lifetime<Self>>;
    type TypeInference = InferenceVariable<Type<Self>>;
    type ConstantInference = InferenceVariable<Constant<Self>>;

    fn from_default_type(ty: Type<model::Default>) -> Type<Self> {
        Type::from_other_model(ty)
    }

    fn from_default_lifetime(
        lifetime: Lifetime<model::Default>,
    ) -> Lifetime<Self> {
        Lifetime::from_other_model(lifetime)
    }

    fn from_default_constant(
        constant: Constant<model::Default>,
    ) -> Constant<Self> {
        Constant::from_other_model(constant)
    }
}

impl<T: table::State, U> table::Display<T> for InferenceVariable<U> {
    fn fmt(
        &self,
        _: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "?")
    }
}

/// The context used for binding the IR for a function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionBindingContext<P: Copy, D>
where
    GlobalID: From<P> + From<ID<GenericTemplate<P, FunctionTemplate<D>>>>,
    GenericTemplate<P, FunctionTemplate<D>>: table::representation::Element,
{
    function_id: ID<GenericTemplate<P, FunctionTemplate<D>>>,
    parameter_name_binding_point: NameBindingPoint<Model>,
}

/// The binder used for building the IR.
#[derive(Debug)]
pub struct Binder<'t, C, S: table::State, O: Observer<S, Model>> {
    table: &'t Table<S>,
    resolution_observer: O,
    current_site: GlobalID,
    premise: Premise<Model>,
    constant: bool,

    intermediate_representation: Representation<Model>,
    current_block_id: ID<Block<Model>>,

    inference_context: infer::Context,

    context: C,

    // a boolean flag indicating whether there's already been an error reported
    suboptimal: Arc<RwLock<bool>>,
}

impl<'t, C, S: table::State, O: Observer<S, Model>> State
    for Binder<'t, C, S, O>
{
    type Model = Model;
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

impl<'t, P: Copy, D, S: table::State, O: Observer<S, Model>>
    Binder<'t, FunctionBindingContext<P, D>, S, O>
where
    GlobalID: From<P> + From<ID<GenericTemplate<P, FunctionTemplate<D>>>>,
    GenericTemplate<P, FunctionTemplate<D>>: table::representation::Element,
{
    /// Creates the binder for building the IR.
    ///
    /// # Errors
    ///
    /// See [`CreateFunctionBinderError`] for the possible errors.
    pub fn new_function<'a>(
        table: &'t Table<S>,
        resolution_observer: O,
        function_id: ID<GenericTemplate<P, FunctionTemplate<D>>>,
        parameter_pattern_syns: impl ExactSizeIterator<
            Item = &'a syntax_tree::pattern::Irrefutable,
        >,
        is_const: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self, CreateFunctionBinderError> {
        let premise = table
            .get_active_premise::<Model>(function_id.into())
            .ok_or(CreateFunctionBinderError::InvalidFunctionID)?;

        let handler = HandlerWrapper {
            handler,
            suboptimal: Arc::new(RwLock::new(false)),
        };

        let mut intermediate_representation = Representation::default();
        let current_block_id =
            intermediate_representation.control_flow_graph.entry_block_id();

        let function_sym = table.get(function_id).unwrap();

        // mismatched count
        if parameter_pattern_syns.len() != function_sym.parameters().len() {
            return Err(CreateFunctionBinderError::MismatchedParameterCount);
        }

        let mut parameter_name_binding_point = NameBindingPoint::default();

        // bind the parameter patterns
        #[allow(clippy::significant_drop_in_scrutinee)]
        for ((parameter_id, parameter_sym), syntax_tree) in
            function_sym.parameter_as_order().zip(parameter_pattern_syns)
        {
            let parameter_type =
                Model::from_default_type(parameter_sym.r#type.clone());

            let pattern = intermediate_representation
                .create_irrefutable(
                    table,
                    syntax_tree,
                    &parameter_type,
                    &Address::Parameter(parameter_id),
                    current_block_id,
                    function_id.into(),
                    &handler,
                )
                .unwrap();

            parameter_name_binding_point
                .add_irrefutable_binding(&pattern, &handler);
        }

        drop(function_sym);

        Ok(Self {
            table,
            current_site: function_id.into(),
            premise,
            constant: is_const,
            intermediate_representation,
            current_block_id,

            inference_context: Context::default(),

            context: FunctionBindingContext {
                function_id,
                parameter_name_binding_point,
            },
            resolution_observer,

            suboptimal: handler.suboptimal,
        })
    }
}

impl<'t, C, S: table::State, O: Observer<S, Model>> Binder<'t, C, S, O> {
    /// Creates a handler that triggers the suboptimal flag inside this
    /// binder when an error is received.
    fn create_handler_wrapper<'a>(
        &self,
        handler: &'a dyn Handler<Box<dyn error::Error>>,
    ) -> HandlerWrapper<'a> {
        HandlerWrapper { handler, suboptimal: self.suboptimal.clone() }
    }
}

impl<'t, C, S: table::State, O: Observer<S, Model>> Binder<'t, C, S, O> {
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
    /// # Returns
    ///
    /// Returns `true` if no error was found during type checking, `false`
    /// otherwise.
    #[must_use]
    fn type_check(
        &mut self,
        ty: Type<Model>,
        expected_ty: Expected<Model>,
        type_check_span: Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> bool {
        match expected_ty {
            Expected::Known(expected_ty) => {
                // simplify the types
                let simplified_ty = simplify(&ty, &Environment {
                    premise: &self.premise,
                    table: self.table,
                    normalizer: &self.inference_context,
                })
                .unwrap_or(ty);

                let simplified_expected =
                    simplify(&expected_ty, &Environment {
                        premise: &self.premise,
                        table: self.table,
                        normalizer: &self.inference_context,
                    })
                    .unwrap_or(expected_ty);

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
                if !result {
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
                            span: type_check_span,
                        },
                    ))
                }

                result
            }
            Expected::Inferring(_) => todo!(),
        }
    }
}
