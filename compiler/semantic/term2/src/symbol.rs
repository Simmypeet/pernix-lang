use pernixc_symbol::GlobalSymbolID;
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::{
    generic_parameters::GenericParameterID,
    instantiation::Instantiation,
    r#type::{Type, context::SymbolContext},
};

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Symbol {
    symbol_id: GlobalSymbolID,
    generic_arguments: Interned<[Interned<Type>]>,
}

impl Symbol {
    pub async fn create_instantiation(
        &self,
        symbol_context: &impl SymbolContext,
    ) -> Instantiation {
        let generic_params =
            symbol_context.get_symbol_generic_parameters(self.symbol_id).await;

        assert!(generic_params.len() == self.generic_arguments.len());

        let mut inst = Instantiation::default();

        for ((id, _), gen_arg) in
            generic_params.iter().zip(self.generic_arguments.iter())
        {
            inst.insert(
                GenericParameterID::new(self.symbol_id, id),
                gen_arg.clone(),
            );
        }

        inst
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct TraitRef(Symbol);
