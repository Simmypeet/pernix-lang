use crate::r#type::{
    bound::BoundVariable, inference::InferenceVariable, kind::TyKind,
    skolem::SkolemizedVariable,
};

pub trait TyContext: Send + Sync {
    fn get_inference_variable_kind(
        &self,
        inference_variable_id: &InferenceVariable,
    ) -> TyKind;

    fn get_bound_variable_kind(&self, bound_var: &BoundVariable) -> TyKind;

    fn get_skolemized_variable_kind(
        &self,
        skolemized_var: &SkolemizedVariable,
    ) -> TyKind;
}
