use pernixc_symbol::GlobalSymbolID;
use qbice::storage::intern::Interned;

use crate::{
    generic_parameters::GenericParameters,
    r#type::{Type, inference::InferenceVariable, kind::TyKind},
};

pub trait SymbolContext: Send + Sync {
    fn get_instance_associated_type_kind(
        &self,
        instance_associated_symbol_id: GlobalSymbolID,
    ) -> impl Future<Output = TyKind> + Send;

    fn get_instance_associated_type(
        &self,
        instance_associated_symbol_id: GlobalSymbolID,
    ) -> impl Future<Output = Option<Interned<Type>>> + Send;

    fn get_symbol_generic_parameters(
        &self,
        symbol_id: GlobalSymbolID,
    ) -> impl Future<Output = Interned<GenericParameters>> + Send;
}

pub trait InferenceVariableContext: Send + Sync {
    fn get_inference_variable_kind(
        &self,
        inference_variable_id: InferenceVariable,
    ) -> impl Future<Output = TyKind> + Send;
}

pub trait TyContext: InferenceVariableContext + SymbolContext {}

impl<T: InferenceVariableContext + SymbolContext> TyContext for T {}
