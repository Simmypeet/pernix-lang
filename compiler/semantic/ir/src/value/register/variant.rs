//! Contains the definition of the [`Variant`] register.

use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_qbice::TrackedEngine;
use pernixc_target::Global;
use pernixc_term::{instantiation::Instantiation, r#type::Type};
use pernixc_type_system::OverflowError;
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    resolution_visitor::{
        Abort, MutableResolutionVisitor, Resolution, ResolutionMut,
        ResolutionVisitor,
    },
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
    #[must_use]
    pub const fn new(
        symbol: pernixc_resolution::qualified_identifier::Variant,
        associated_value: Option<Value>,
    ) -> Self {
        Self { symbol, associated_value }
    }

    pub async fn parent_enum_id(
        &self,
        engine: &TrackedEngine,
    ) -> Global<pernixc_symbol::ID> {
        self.symbol.parent_enum_id(engine).await
    }

    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> Instantiation {
        self.symbol.create_instantiation(engine).await
    }

    #[must_use]
    pub const fn variant_id(&self) -> Global<pernixc_symbol::ID> {
        self.symbol.variant_id()
    }

    #[must_use]
    pub const fn associated_value(&self) -> Option<&Value> {
        self.associated_value.as_ref()
    }
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

pub(super) async fn transform_variant<T: MutableResolutionVisitor>(
    variant: &mut Variant,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    if let Some(value) = variant.associated_value.as_mut()
        && let Some(literal) = value.as_literal_mut()
    {
        literal.accept_mut(visitor).await?;
    }

    visitor
        .visit_mut(ResolutionMut::Variant(&mut variant.symbol), span)
        .await?;
    Ok(())
}

pub(super) async fn inspect_variant<T: ResolutionVisitor>(
    variant: &Variant,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    if let Some(value) = variant.associated_value.as_ref()
        && let Some(literal) = value.as_literal()
    {
        literal.accept(visitor).await?;
    }

    visitor.visit(Resolution::Variant(&variant.symbol), span).await?;
    Ok(())
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
