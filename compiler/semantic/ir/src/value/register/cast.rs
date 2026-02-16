//! Contains the definition of the [`Cast`] register.

use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_term::r#type::Type;
use pernixc_type_system::{Error, Succeeded, normalizer::Normalizer};
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    transform::{Transformer, TypeTermSource},
    value::{Environment, TypeOf, Value, register::Register},
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
    Encode,
    Decode,
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

impl crate::visitor::Element for Cast {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        visitor.visit_value(std::borrow::Cow::Borrowed(&self.value));
    }
}

pub(super) async fn transform_cast<T: Transformer<Type>>(
    cast: &mut Cast,
    transformer: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) {
    if let Some(literal) = cast.value.as_literal_mut() {
        literal.transform(transformer).await;
    }

    transformer.transform(&mut cast.r#type, TypeTermSource::Cast, span).await;
}

impl TypeOf<&Cast> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        cast: &Cast,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        Ok(environment
            .type_environment
            .simplify(cast.r#type.clone())
            .await?
            .deref()
            .clone())
    }
}
