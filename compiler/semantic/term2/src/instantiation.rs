use std::collections::HashMap;

use pernixc_qbice::TrackedEngine;
use pernixc_symbol::GlobalSymbolID;
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::{
    generic_parameters::{GenericParameterID, get_generic_parameters},
    r#type::Type,
};

#[derive(Debug, Clone, PartialEq, Eq, StableHash, Encode, Decode, Default)]
pub struct Instantiation(HashMap<GenericParameterID, Interned<Type>>);

impl Instantiation {
    pub(crate) fn insert(
        &mut self,
        gen_param_id: GenericParameterID,
        ty: Interned<Type>,
    ) {
        assert!(self.0.insert(gen_param_id, ty).is_none());
    }

    #[must_use]
    pub(crate) fn get_instantiated_type(
        &self,
        gen_param_id: GenericParameterID,
    ) -> Option<&Interned<Type>> {
        self.0.get(&gen_param_id)
    }

    pub(crate) async fn append_generic_arguments(
        &mut self,
        symbol_id: GlobalSymbolID,
        generic_arguments: &[Interned<Type>],
        engine: &TrackedEngine,
    ) {
        let generic_params = engine.get_generic_parameters(symbol_id).await;

        assert!(generic_params.len() == generic_arguments.len());

        for ((id, _), gen_arg) in
            generic_params.iter().zip(generic_arguments.iter())
        {
            self.insert(
                GenericParameterID::new(symbol_id, id),
                gen_arg.clone(),
            );
        }
    }
}
