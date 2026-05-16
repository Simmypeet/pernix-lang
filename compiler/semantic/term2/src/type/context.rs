use crate::r#type::{
    bound::BoundVar, inference::InferenceVariable, kind::TyKind,
};

pub trait InferenceVariableContext: Send + Sync {
    fn get_inference_variable_kind(
        &self,
        inference_variable_id: InferenceVariable,
    ) -> impl Future<Output = TyKind> + Send;

    fn get_bound_var_kind(
        &self,
        bound_var: &BoundVar,
    ) -> impl Future<Output = TyKind> + Send;
}

pub trait TyContext: InferenceVariableContext {}

impl<T: InferenceVariableContext> TyContext for T {}
