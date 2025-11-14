//! Contains the definition of the [`Cast`] register.

use pernixc_arena::ID;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::r#type::Type;

use crate::{
    transform::{self, Transformer, TypeTermSource},
    value::{register::Register, Value},
};

/// Represents a cast operation.
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
pub struct Cast {
    /// The value to be casted.
    pub value: Value,

    /// The type to cast the value to.
    pub r#type: Type,
}

impl Cast {
    /// Returns the register that is used in the cast.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.value.as_register().copied().into_iter().collect()
    }
}

impl transform::Element for Cast {
    async fn transform<T: Transformer<Type>>(
        &mut self,
        transformer: &mut T,
        _engine: &pernixc_query::TrackedEngine,
    ) -> Result<(), CyclicError> {
        if let Some(literal) = self.value.as_literal_mut() {
            literal.transform(transformer).await?;
        }

        transformer
            .transform(&mut self.r#type, TypeTermSource::Cast, None)
            .await
    }
}
