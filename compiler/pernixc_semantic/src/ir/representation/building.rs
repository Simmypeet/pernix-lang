//! Contains the definition of states and models used for building the IR.

use std::sync::atomic::{AtomicUsize, Ordering};

use parking_lot::RwLock;
use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::Representation;
use crate::{
    arena::ID,
    error::Error,
    ir::{
        address::Address, control_flow_graph::Block, pattern::NameBindingPoint,
        State,
    },
    semantic::{
        fresh::Fresh,
        model::{self, Model as _},
        term::{constant::Constant, lifetime::Lifetime, r#type::Type},
        Premise,
    },
    symbol::{
        table::{
            self,
            representation::{
                building::finalizing::Finalizer, Index, RwLockContainer,
            },
            Table,
        },
        FunctionTemplate, GenericTemplate, GlobalID,
    },
};

pub mod pattern;

/// A wrapper around a handler that detects if an error has been reported.
pub struct HandlerWrapper<'a> {
    handler: &'a dyn Handler<Box<dyn Error>>,
    reported: RwLock<bool>,
}

impl<'a> HandlerWrapper<'a> {
    /// Creates a new handler wrapper.
    #[must_use]
    pub fn new(handler: &'a dyn Handler<Box<dyn Error>>) -> Self {
        Self { handler, reported: RwLock::new(false) }
    }

    /// Returns `true` if an error has been reported.
    #[must_use]
    pub fn reported(&self) -> bool { *self.reported.read() }
}

impl<'a> Handler<Box<dyn Error>> for HandlerWrapper<'a> {
    fn receive(&self, error: Box<dyn Error>) {
        *self.reported.write() = true;
        self.handler.receive(error);
    }
}

/// The context used for binding the IR for a function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionBindingContext<P: Copy, D>
where
    GlobalID: From<P> + From<ID<GenericTemplate<P, FunctionTemplate<D>>>>,
    GenericTemplate<P, FunctionTemplate<D>>: table::representation::Element,
{
    pub function_id: ID<GenericTemplate<P, FunctionTemplate<D>>>,
    pub parameter_name_binding_point: NameBindingPoint<Model>,
    pub is_const: bool,
}

/// The binder used for building the IR.
pub struct Binder<'t, 'h, C> {
    table: &'t Table<table::Building<RwLockContainer, Finalizer>>,
    current_site: GlobalID,
    premise: Premise<Model>,

    intermediate_representation: Representation<Model>,
    current_block_id: ID<Block<Model>>,

    context: C,
    handler: HandlerWrapper<'h>,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum CreateFunctionBinderError {
    #[error("the given function ID does not exist in the table")]
    InvalidFunctionID,

    #[error(
        "the given parameter's pattern syntax iterator count does not match \
         the function's parameter count"
    )]
    MismatchedParameterCount,
}

impl<'t, 'h, P: Copy, D> Binder<'t, 'h, FunctionBindingContext<P, D>>
where
    GlobalID: From<P> + From<ID<GenericTemplate<P, FunctionTemplate<D>>>>,
    GenericTemplate<P, FunctionTemplate<D>>: table::representation::Element,
{
    /// Creates the binder for building the IR.
    pub fn new<'a>(
        table: &'t Table<table::Building<RwLockContainer, Finalizer>>,
        function_id: ID<GenericTemplate<P, FunctionTemplate<D>>>,
        parameter_pattern_syns: impl ExactSizeIterator<
            Item = &'a syntax_tree::pattern::Irrefutable,
        >,
        is_const: bool,
        handler: &'h dyn Handler<Box<dyn Error>>,
    ) -> Result<Self, CreateFunctionBinderError> {
        let handler = HandlerWrapper::new(handler);
        let premise = table
            .get_active_premise::<Model>(function_id.into())
            .ok_or(CreateFunctionBinderError::InvalidFunctionID)?;

        let mut intermediate_representation = Representation::default();
        let current_block_id =
            intermediate_representation.control_flow_graph.entry_block_id();

        let function_sym = table.get(function_id).unwrap();

        // mismatched count
        if parameter_pattern_syns.len() != function_sym.parameters().len() {
            return Err(CreateFunctionBinderError::MismatchedParameterCount);
        }

        let mut parameter_name_binding_point = NameBindingPoint::default();

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
            intermediate_representation,
            current_block_id,
            context: FunctionBindingContext {
                function_id,
                parameter_name_binding_point,
                is_const,
            },
            handler,
        })
    }
}

impl<'t, 'h, C> State for Binder<'t, 'h, C> {
    type Model = Model;
}

/// A unique identifier for an inference variable.
pub struct InferenceVariable<T> {
    id: usize,
    _phantom: std::marker::PhantomData<T>,
}

impl<T> std::fmt::Debug for InferenceVariable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InferenceVariable({})", self.id)
    }
}

impl<T> Clone for InferenceVariable<T> {
    fn clone(&self) -> Self { *self }
}

impl<T> Copy for InferenceVariable<T> {}

impl<T> PartialEq for InferenceVariable<T> {
    fn eq(&self, other: &Self) -> bool { self.id == other.id }
}

impl<T> Eq for InferenceVariable<T> {}

impl<T> std::hash::Hash for InferenceVariable<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.id.hash(state); }
}

impl<T> PartialOrd for InferenceVariable<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for InferenceVariable<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering { self.id.cmp(&other.id) }
}

impl<T> InferenceVariable<T> {
    /// Creates a new inference variable that is unique for all any created
    /// ones.
    pub fn new() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        Self {
            id: COUNTER.fetch_add(1, Ordering::SeqCst),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T> std::default::Default for InferenceVariable<T> {
    fn default() -> Self { Self::new() }
}

impl<T> Fresh for InferenceVariable<T> {
    fn fresh() -> Self { Self::new() }
}

impl From<InferenceVariable<Self>> for Lifetime<Model> {
    fn from(value: InferenceVariable<Self>) -> Self { Self::Inference(value) }
}

impl From<InferenceVariable<Self>> for Type<Model> {
    fn from(value: InferenceVariable<Self>) -> Self { Self::Inference(value) }
}

impl From<InferenceVariable<Self>> for Constant<Model> {
    fn from(value: InferenceVariable<Self>) -> Self { Self::Inference(value) }
}

/// The model used for building the IR
///
/// This model enables the use of inference variables
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Model;

impl model::Model for Model {
    type LifetimeInference = InferenceVariable<Lifetime<Self>>;
    type TypeInference = InferenceVariable<Type<Self>>;
    type ConstantInference = InferenceVariable<Constant<Self>>;
}
