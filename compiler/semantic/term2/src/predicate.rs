//! Data definitions for where-clause predicates.

mod compatible;
mod constant_type;
mod instance_associated_type_equality;
mod marker;
mod outlives;
mod tuple;

pub use compatible::Compatible;
pub use constant_type::ConstantType;
use enum_as_inner::EnumAsInner;
pub use instance_associated_type_equality::InstanceAssociatedTypeEquality;
pub use marker::{Negative as NegativeMarker, Positive as PositiveMarker};
pub use outlives::Outlives;
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};
pub use tuple::Tuple;

use crate::{
    TermRef, generic_arguments::GenericArguments, instance, lifetime::Lifetime,
    sub_term::IterSubTerms, r#type::Type,
};

pub(crate) fn term_ref_contains_error(term_ref: TermRef<'_>) -> bool {
    if matches_direct_error(term_ref) {
        return true;
    }

    match term_ref {
        TermRef::Constant(constant) => constant
            .as_ref()
            .iter_sub_terms()
            .any(|(sub_term, _)| term_ref_contains_error(sub_term)),
        TermRef::Lifetime(lifetime) => lifetime
            .as_ref()
            .iter_sub_terms()
            .any(|(sub_term, _)| term_ref_contains_error(sub_term)),
        TermRef::Type(r#type) => r#type
            .as_ref()
            .iter_sub_terms()
            .any(|(sub_term, _)| term_ref_contains_error(sub_term)),
        TermRef::Instance(instance) => instance
            .as_ref()
            .iter_sub_terms()
            .any(|(sub_term, _)| term_ref_contains_error(sub_term)),
    }
}

fn matches_direct_error(term_ref: TermRef<'_>) -> bool {
    match term_ref {
        TermRef::Constant(constant) => constant.as_ref().is_error(),
        TermRef::Lifetime(lifetime) => lifetime.as_ref().is_error(),
        TermRef::Type(r#type) => r#type.as_ref().is_error(),
        TermRef::Instance(instance) => instance.as_ref().is_error(),
    }
}

pub(crate) fn generic_arguments_contains_error(
    generic_arguments: &GenericArguments,
) -> bool {
    generic_arguments.iter_sub_term().any(term_ref_contains_error)
}

fn instance_associated_contains_error(
    instance_associated: &instance::InstanceAssociated,
) -> bool {
    generic_arguments_contains_error(
        instance_associated.associated_instance_generic_arguments().as_ref(),
    ) || term_ref_contains_error(instance_associated.instance().into())
}

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
    LifetimeOutlives(Outlives<Interned<Lifetime>>),
    TypeOutlives(Outlives<Interned<Type>>),
    TupleType(Tuple<Interned<Type>>),
    PositiveMarker(PositiveMarker),
    NegativeMarker(NegativeMarker),
    InstanceAssociatedTypeEquality(InstanceAssociatedTypeEquality),
}

impl Predicate {
    /// Creates a new [`Predicate::LifetimeOutlives`] predicate.
    #[must_use]
    pub const fn lifetime_outlives(
        operand: Interned<Lifetime>,
        bound: Interned<Lifetime>,
    ) -> Self {
        Self::LifetimeOutlives(Outlives::new(operand, bound))
    }

    /// Creates a new [`Predicate::TypeOutlives`] predicate.
    #[must_use]
    pub const fn type_outlives(
        operand: Interned<Type>,
        bound: Interned<Lifetime>,
    ) -> Self {
        Self::TypeOutlives(Outlives::new(operand, bound))
    }

    /// Checks whether the predicate contains an erroneous term.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        match self {
            Self::ConstantType(constant_type) => constant_type.contains_error(),
            Self::LifetimeOutlives(outlives) => outlives.contains_error(),
            Self::TypeOutlives(outlives) => outlives.contains_error(),
            Self::TupleType(tuple) => tuple.contains_error(),
            Self::PositiveMarker(marker) => marker.contains_error(),
            Self::NegativeMarker(marker) => marker.contains_error(),
            Self::InstanceAssociatedTypeEquality(equality) => {
                instance_associated_contains_error(equality.lhs())
                    || term_ref_contains_error(equality.rhs().into())
            }
        }
    }
}
