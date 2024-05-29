use parking_lot::RwLock;
use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{
    infer::{self, Context},
    Model,
};
use crate::{
    arena::ID,
    error,
    ir::{
        address::Address, control_flow_graph::Block, pattern::NameBindingPoint,
        representation::Representation, State,
    },
    semantic::{model::Model as _, Premise},
    symbol::{
        table::{self, representation::Index, resolution::Observer, Table},
        FunctionTemplate, GenericTemplate, GlobalID,
    },
};

pub mod expression;
pub mod statement;

/// A wrapper around a handler that detects if an error has been reported.
pub struct HandlerWrapper<'a> {
    handler: &'a dyn Handler<Box<dyn error::Error>>,
    reported: RwLock<bool>,
}

impl<'a> HandlerWrapper<'a> {
    /// Creates a new handler wrapper.
    #[must_use]
    pub fn new(handler: &'a dyn Handler<Box<dyn error::Error>>) -> Self {
        Self { handler, reported: RwLock::new(false) }
    }

    /// Returns `true` if an error has been reported.
    #[must_use]
    pub fn reported(&self) -> bool { *self.reported.read() }
}

impl<'a> Handler<Box<dyn error::Error>> for HandlerWrapper<'a> {
    fn receive(&self, error: Box<dyn error::Error>) {
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
}

/// The binder used for building the IR.
pub struct Binder<'t, 'h, C, S: table::State, O: Observer<S, super::Model>> {
    table: &'t Table<S>,
    resolution_observer: O,
    current_site: GlobalID,
    premise: Premise<Model>,
    constant: bool,

    intermediate_representation: Representation<Model>,
    current_block_id: ID<Block<Model>>,

    inference_context: infer::Context,

    context: C,
    handler: HandlerWrapper<'h>,
}

impl<'t, 'h, C, S: table::State, O: Observer<S, super::Model>> State
    for Binder<'t, 'h, C, S, O>
{
    type Model = Model;
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

impl<'t, 'h, P: Copy, D, S: table::State, O: Observer<S, super::Model>>
    Binder<'t, 'h, FunctionBindingContext<P, D>, S, O>
where
    GlobalID: From<P> + From<ID<GenericTemplate<P, FunctionTemplate<D>>>>,
    GenericTemplate<P, FunctionTemplate<D>>: table::representation::Element,
{
    /// Creates the binder for building the IR.
    pub fn new_function<'a>(
        table: &'t Table<S>,
        resolution_observer: O,
        function_id: ID<GenericTemplate<P, FunctionTemplate<D>>>,
        parameter_pattern_syns: impl ExactSizeIterator<
            Item = &'a syntax_tree::pattern::Irrefutable,
        >,
        is_const: bool,
        handler: &'h dyn Handler<Box<dyn error::Error>>,
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
            handler,
            resolution_observer,
        })
    }
}

impl<'t, 'h, P: Copy, D, S: table::State, O: Observer<S, super::Model>>
    Binder<'t, 'h, FunctionBindingContext<P, D>, S, O>
where
    GlobalID: From<P> + From<ID<GenericTemplate<P, FunctionTemplate<D>>>>,
    GenericTemplate<P, FunctionTemplate<D>>: table::representation::Element,
{
}
