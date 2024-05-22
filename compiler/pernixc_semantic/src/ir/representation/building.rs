//! Contains the definition of states and models used for building the IR.

use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{
    ir::State,
    semantic::{
        fresh::Fresh,
        model,
        term::{constant::Constant, lifetime::Lifetime, r#type::Type},
        Premise,
    },
    symbol::{
        table::{self, Table},
        GlobalID,
    },
};

pub mod pattern;

#[derive(Debug, Clone)]
pub struct Building<'t, S: table::State> {
    table: &'t Table<S>,
    current_site: GlobalID,
    premise: Premise<Model>,
}

impl<'t, S: table::State> State for Building<'t, S> {
    type Model = Model;
}

/// A unique identifier for an inference variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct InferenceVariable(usize);

impl InferenceVariable {
    /// Creates a new inference variable that is unique for all any created
    /// ones.
    pub fn new() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        Self(COUNTER.fetch_add(1, Ordering::SeqCst))
    }
}

impl Fresh for InferenceVariable {
    fn fresh() -> Self { Self::new() }
}

impl From<InferenceVariable> for Lifetime<Model> {
    fn from(value: InferenceVariable) -> Self { Self::Inference(value) }
}

impl From<InferenceVariable> for Type<Model> {
    fn from(value: InferenceVariable) -> Self { Self::Inference(value) }
}

impl From<InferenceVariable> for Constant<Model> {
    fn from(value: InferenceVariable) -> Self { Self::Inference(value) }
}

/// The model used for building the IR
///
/// This model enables the use of inference variables
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Model;

impl model::Model for Model {
    type LifetimeInference = InferenceVariable;
    type TypeInference = InferenceVariable;
    type ConstantInference = InferenceVariable;
}
