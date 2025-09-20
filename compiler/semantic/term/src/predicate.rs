//! Contains various definition of predicates.

mod compatible;
mod constant_type;
mod marker;
mod outlives;
mod r#trait;
mod tuple;

pub use compatible::Compatible;
pub use constant_type::ConstantType;
use enum_as_inner::EnumAsInner;
pub use marker::{Negative as NegativeMarker, Positive as PositiveMarker};
pub use outlives::Outlives;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
pub use r#trait::{Negative as NegativeTrait, Positive as PositiveTrait};
pub use tuple::Tuple;

use crate::{
    error::contains_error, generic_arguments::TraitMember,
    instantiation::Instantiation, lifetime::Lifetime, r#type::Type,
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
    Serialize,
    Deserialize,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Predicate {
    TraitTypeCompatible(Compatible<TraitMember, Type>),
    ConstantType(ConstantType),
    LifetimeOutlives(Outlives<Lifetime>),
    TypeOutlives(Outlives<Type>),
    TupleType(Tuple<Type>),
    PositiveTrait(PositiveTrait),
    NegativeTrait(NegativeTrait),
    PositiveMarker(PositiveMarker),
    NegativeMarker(NegativeMarker),
}

impl Predicate {
    /// Checks if the predicate has an errornous term.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        match self {
            Self::TraitTypeCompatible(equality) => equality.contains_error(),
            Self::ConstantType(constant_type) => constant_type.contains_error(),
            Self::LifetimeOutlives(outlives) => outlives.contains_error(),
            Self::TypeOutlives(outlives) => outlives.contains_error(),
            Self::TupleType(tuple) => tuple.contains_error(),
            Self::PositiveTrait(tr) => tr.contains_error(),
            Self::NegativeTrait(tr) => tr.contains_error(),
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
            Self::TraitTypeCompatible(equality) => {
                equality.instantiate(substitution);
            }
            Self::ConstantType(constant_type) => {
                constant_type.instantiate(substitution);
            }
            Self::LifetimeOutlives(outlives) => {
                outlives.instantiate(substitution);
            }
            Self::TypeOutlives(outlives) => outlives.instantiate(substitution),
            Self::TupleType(tuple) => tuple.instantiate(substitution),
            Self::PositiveTrait(tr) => tr.instantiate(substitution),
            Self::NegativeTrait(tr) => tr.instantiate(substitution),
            Self::PositiveMarker(marker) => marker.instantiate(substitution),
            Self::NegativeMarker(marker) => marker.instantiate(substitution),
        }
    }
}

impl Compatible<TraitMember, Type> {
    /// Checks if the predicate has an errornous term.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        let trait_member = Type::TraitMember(self.lhs.clone());

        contains_error(&trait_member) || contains_error(&self.rhs)
    }

    /// Applies the instantiation to both [`Self::lhs`] and
    /// [`Self::rhs`].
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        self.lhs.0.member_generic_arguments.instantiate(instantiation);
        self.lhs.0.parent_generic_arguments.instantiate(instantiation);
        instantiation.instantiate(&mut self.rhs);
    }
}

impl crate::display::Display for Predicate {
    async fn fmt(
        &self,
        engine: &pernixc_query::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        match self {
            Self::TraitTypeCompatible(equality) => {
                equality.fmt(engine, formatter).await
            }
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
            Self::PositiveTrait(tr) => tr.fmt(engine, formatter).await,
            Self::NegativeTrait(tr) => tr.fmt(engine, formatter).await,
            Self::PositiveMarker(marker) => marker.fmt(engine, formatter).await,
            Self::NegativeMarker(marker) => marker.fmt(engine, formatter).await,
        }
    }
}
