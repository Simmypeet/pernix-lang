use std::collections::HashMap;

use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::{generic_parameters::GenericParameterID, r#type::Type};

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
}
