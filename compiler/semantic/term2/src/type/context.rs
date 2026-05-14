use crate::r#type::{inference::InferenceVariable, kind::TyKind};

pub trait InferenceVariableContext: Send + Sync {
    fn get_inference_variable_kind(
        &self,
        inference_variable_id: InferenceVariable,
    ) -> impl Future<Output = TyKind> + Send;
}

pub trait TyContext: InferenceVariableContext {}

impl<T: InferenceVariableContext> TyContext for T {}
