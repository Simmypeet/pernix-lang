//! Contains the definition of the [`Cast`] register.

use pernixc_arena::ID;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::r#type::Type;
use pernixc_type_system::{normalizer::Normalizer, Error, Succeeded};

use crate::{
    transform::{Transformer, TypeTermSource},
    value::{register::Register, Environment, TypeOf, Value},
    Values,
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

pub(super) async fn transform_cast<T: Transformer<Type>>(
    cast: &mut Cast,
    transformer: &mut T,
    span: Option<pernixc_lexical::tree::RelativeSpan>,
) -> Result<(), CyclicError> {
    if let Some(literal) = cast.value.as_literal_mut() {
        literal.transform(transformer).await?;
    }

    transformer.transform(&mut cast.r#type, TypeTermSource::Cast, span).await
}

impl TypeOf<&Cast> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        cast: &Cast,
        _environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        Ok(Succeeded::new(cast.r#type.clone()))
    }
}
