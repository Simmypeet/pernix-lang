use crate::r#type::{
    bound::BoundVariable, inference::InferenceVariable, kind::TyKind,
    skolem::SkolemizedVariable,
};

pub trait InferenceVariableContext: Send + Sync {
    fn get_inference_variable_kind(
        &self,
        inference_variable_id: &InferenceVariable,
    ) -> impl Future<Output = TyKind> + Send;

    fn get_bound_variable_kind(
        &self,
        bound_var: &BoundVariable,
    ) -> impl Future<Output = TyKind> + Send;

    fn get_skolemized_variable_kind(
        &self,
        skolemized_var: &SkolemizedVariable,
    ) -> impl Future<Output = TyKind> + Send;
}

pub trait TyContext: InferenceVariableContext {}

impl<T: InferenceVariableContext> TyContext for T {}
