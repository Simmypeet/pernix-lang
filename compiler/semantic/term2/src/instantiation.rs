use std::collections::HashMap;

use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::{generic_parameters::GenericParameterID, r#type::Type};

#[derive(Debug, Clone, PartialEq, Eq, StableHash, Encode, Decode)]
pub struct Instantiation(HashMap<GenericParameterID, Interned<Type>>);

impl Instantiation {
    #[must_use]
    pub(crate) fn get_instantiated_type(
        &self,
        gen_param_id: GenericParameterID,
    ) -> Option<&Interned<Type>> {
        self.0.get(&gen_param_id)
    }
}
