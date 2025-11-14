//! Contains the definition of the [`Array`] register.

use pernixc_arena::ID;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::r#type::Type;

use crate::{
    transform::{self, Transformer, TypeTermSource},
    value::{register::Register, Value},
};

/// Represents an array of values.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Array {
    /// The elements of the array.
    pub elements: Vec<Value>,

    /// The type of the element in the array.
    ///
    /// The type must be declared separately as the element values can have
    /// different lifetime values; thus, the type of the array can't be solely
    /// determined by one of the element values.
    pub element_type: Type,
}

impl Array {
    /// Returns the list of registers that are used in the array.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.elements.iter().filter_map(|x| x.as_register().copied()).collect()
    }
}

impl transform::Element for Array {
    async fn transform<T: Transformer<Type>>(
        &mut self,
        transformer: &mut T,
        _engine: &pernixc_query::TrackedEngine,
    ) -> Result<(), CyclicError> {
        for value in &mut self.elements {
            if let Some(literal) = value.as_literal_mut() {
                literal.transform(transformer).await?;
            }
        }

        transformer
            .transform(&mut self.element_type, TypeTermSource::Array, None)
            .await
    }
}
