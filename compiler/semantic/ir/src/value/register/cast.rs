//! Contains the definition of the [`Cast`] register.

use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_term::r#type::Type;
use pernixc_type_system::{OverflowError, Succeeded, normalizer::Normalizer};
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    resolution_visitor::{
        self, Abort, Resolution, ResolutionMut, ResolutionVisitor,
    },
    value::{Environment, TypeOf, Value, register::Register},
};

macro_rules! visit_cast {
    (
        $cast:expr,
        $visitor:expr,
        $span:expr,
        $literal_accessor:ident,
        $accept_method:ident,
        $visit_method:ident,
        $resolution_ctor:ident,
        $type_expr:expr
    ) => {{
        if let Some(literal) = $cast.value.$literal_accessor() {
            literal.$accept_method($visitor).await?;
        }

        $visitor
            .$visit_method($resolution_ctor::Type($type_expr), $span)
            .await?;
        Ok(())
    }};
}

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

pub(super) async fn transform_cast<
    T: resolution_visitor::MutableResolutionVisitor,
>(
    cast: &mut Cast,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_cast!(
        cast,
        visitor,
        span,
        as_literal_mut,
        accept_mut,
        visit_mut,
        ResolutionMut,
        &mut cast.r#type
    )
}

pub(super) async fn inspect_cast<T: ResolutionVisitor>(
    cast: &Cast,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_cast!(
        cast,
        visitor,
        span,
        as_literal,
        accept,
        visit,
        Resolution,
        &cast.r#type
    )
}

impl TypeOf<&Cast> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        cast: &Cast,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, OverflowError> {
        Ok(environment
            .type_environment
            .simplify(cast.r#type.clone())
            .await?
            .deref()
            .clone())
    }
}
