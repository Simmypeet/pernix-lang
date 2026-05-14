use pernixc_symbol::GlobalSymbolID;
use qbice::storage::intern::Interned;

use crate::{
    generic_parameters::{GenericParameter, GenericParameterID},
    r#type::{Type, inference::InferenceVariable, kind::TyKind},
};

#[trait_variant::make(Send)]
pub trait SymbolContext {
    async fn get_generic_parameter(
        &self,
        id: GenericParameterID,
    ) -> &GenericParameter;

    async fn get_instance_associated_type_kind(
        &self,
        instance_associated_symbol_id: GlobalSymbolID,
    ) -> TyKind;

    async fn get_instance_associated_type(
        &self,
        instance_associated_symbol_id: GlobalSymbolID,
    ) -> Option<Interned<Type>>;
}

#[trait_variant::make(Send)]
pub trait InferenceVariableContext {
    async fn get_inference_variable_kind(
        &self,
        inference_variable_id: InferenceVariable,
    ) -> TyKind;
}

pub trait TyContext: InferenceVariableContext + SymbolContext {}

impl<T: InferenceVariableContext + SymbolContext> TyContext for T {}
