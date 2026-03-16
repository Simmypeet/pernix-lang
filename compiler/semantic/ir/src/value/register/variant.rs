//! Contains the definition of the [`Variant`] register.

use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_term::r#type::Type;
use pernixc_type_system::OverflowError;
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    transform::{ResolutionMut, Transformer},
    value::{TypeOf, Value, register::Register},
};

/// Represents a variant value.
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
pub struct Variant {
    symbol: pernixc_resolution::qualified_identifier::Variant,
    associated_value: Option<Value>,
}

impl Variant {
    /// Returns the list of registers that are used in the variant.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.associated_value
            .as_ref()
            .map(|x| x.as_register().copied())
            .into_iter()
            .flatten()
            .collect()
    }
}

impl crate::visitor::Element for Variant {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        if let Some(value) = &self.associated_value {
            visitor.visit_value(std::borrow::Cow::Borrowed(value));
        }
    }
}

pub(super) async fn transform_variant<T: Transformer>(
    variant: &mut Variant,
    transformer: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) {
    if let Some(value) = variant.associated_value.as_mut()
        && let Some(literal) = value.as_literal_mut()
    {
        literal.transform(transformer).await;
    }

    transformer
        .transform(ResolutionMut::Variant(&mut variant.symbol), span)
        .await;
}

impl TypeOf<&Variant> for Values {
    async fn type_of<N: pernixc_type_system::normalizer::Normalizer>(
        &self,
        value: &Variant,
        environment: &crate::value::Environment<'_, N>,
    ) -> Result<pernixc_type_system::Succeeded<Type>, OverflowError> {
        Ok(environment
            .type_environment
            .simplify(
                value
                    .symbol
                    .create_enum_type(environment.tracked_engine())
                    .await,
            )
            .await?
            .deref()
            .clone())
    }
}
