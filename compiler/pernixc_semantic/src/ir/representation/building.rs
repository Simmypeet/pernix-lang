//! Contains the definition of states and models used for building the IR.

use self::infer::InferenceVariable;
use crate::semantic::{
    model,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type},
};

pub mod binder;
pub mod infer;
pub mod pattern;

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

impl From<InferenceVariable<Self>> for Lifetime<Model> {
    fn from(value: InferenceVariable<Self>) -> Self { Self::Inference(value) }
}

impl From<InferenceVariable<Self>> for Type<Model> {
    fn from(value: InferenceVariable<Self>) -> Self { Self::Inference(value) }
}

impl From<InferenceVariable<Self>> for Constant<Model> {
    fn from(value: InferenceVariable<Self>) -> Self { Self::Inference(value) }
}
