//! Contains various definition of predicates.

mod compatible;
mod constant_type;
mod marker;
mod outlives;
mod tuple;

pub use compatible::Compatible;
pub use constant_type::ConstantType;
use enum_as_inner::EnumAsInner;
pub use marker::{Negative as NegativeMarker, Positive as PositiveMarker};
pub use outlives::Outlives;
use qbice::{Decode, Encode, StableHash};
pub use tuple::Tuple;

use crate::{
    error::contains_error, instantiation::Instantiation, lifetime::Lifetime,
    r#type::Type,
};

/// A predicate that can appear in the where clause.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    StableHash,
    Encode,
    Decode,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Predicate {
    ConstantType(ConstantType),
    LifetimeOutlives(Outlives<Lifetime>),
    TypeOutlives(Outlives<Type>),
    TupleType(Tuple<Type>),
    PositiveMarker(PositiveMarker),
    NegativeMarker(NegativeMarker),
}

impl Predicate {
    /// Checks if the predicate has an errornous term.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        match self {
            Self::ConstantType(constant_type) => constant_type.contains_error(),
            Self::LifetimeOutlives(outlives) => outlives.contains_error(),
            Self::TypeOutlives(outlives) => outlives.contains_error(),
            Self::TupleType(tuple) => tuple.contains_error(),
            Self::PositiveMarker(marker) => marker.contains_error(),
            Self::NegativeMarker(marker) => marker.contains_error(),
        }
    }

    /// Creates a new [`Predicate::TypeOutlives`] predicate.
    #[must_use]
    pub const fn type_outlives(operand: Type, bound: Lifetime) -> Self {
        Self::TypeOutlives(outlives::Outlives { operand, bound })
    }

    /// Applies the instantiate to the predicate.
    pub fn instantiate(&mut self, substitution: &Instantiation) {
        match self {
            Self::ConstantType(constant_type) => {
                constant_type.instantiate(substitution);
            }
            Self::LifetimeOutlives(outlives) => {
                outlives.instantiate(substitution);
            }
            Self::TypeOutlives(outlives) => outlives.instantiate(substitution),
            Self::TupleType(tuple) => tuple.instantiate(substitution),
            Self::PositiveMarker(marker) => marker.instantiate(substitution),
            Self::NegativeMarker(marker) => marker.instantiate(substitution),
        }
    }
}

impl crate::display::Display for Predicate {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        match self {
            Self::ConstantType(constant_type) => {
                constant_type.fmt(engine, formatter).await
            }
            Self::LifetimeOutlives(outlives) => {
                outlives.fmt(engine, formatter).await
            }
            Self::TypeOutlives(outlives) => {
                outlives.fmt(engine, formatter).await
            }
            Self::TupleType(tuple) => tuple.fmt(engine, formatter).await,
            Self::PositiveMarker(marker) => marker.fmt(engine, formatter).await,
            Self::NegativeMarker(marker) => marker.fmt(engine, formatter).await,
        }
    }
}
