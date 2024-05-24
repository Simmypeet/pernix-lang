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
        table::{
            self,
            representation::{
                building::finalizing::Finalizer, RwLockContainer,
            },
            Table,
        },
        GlobalID,
    },
};

pub mod pattern;

#[derive(Debug, Clone)]
pub struct Building<'t> {
    table: &'t Table<table::Building<RwLockContainer, Finalizer>>,
    current_site: GlobalID,
    premise: Premise<Model>,
}

impl<'t> State for Building<'t> {
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
