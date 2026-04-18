//! Defines retrieval-oriented sub-term locations.

use std::{fmt::Debug, hash::Hash};

use enum_as_inner::EnumAsInner;
use pernixc_qbice::TrackedEngine;
use qbice::storage::intern::Interned;

use crate::{
    Never, TermRef,
    constant::{self, Constant},
    instance::{self, Instance},
    lifetime::Lifetime,
    r#type::{self, Type},
};

/// Contains the location types that can retrieve sub-terms from a term.
pub trait SubTerm: Sized {
    /// The type that represents the location of a sub-type in the term.
    type SubTypeLocation: Location<Self, Type>;

    /// The type that represents the location of a sub-constant in the term.
    type SubConstantLocation: Location<Self, Constant>;

    /// The type that represents the location of a sub-lifetime in the term.
    type SubLifetimeLocation: Location<Self, Lifetime>;

    /// The type that represents the location of a sub-instance in the term.
    type SubInstanceLocation: Location<Self, Instance>;

    /// The type that represents the location of a sub-term of the same kind.
    type ThisSubTermLocation: Location<Self, Self>;
}

/// Iterates over immediate sub-terms and their locations.
pub trait IterSubTerms {
    /// The location type yielded by [`Self::iter_sub_terms`].
    type TermLocation;

    /// Returns an iterator over one level of children for this term.
    fn iter_sub_terms(
        &self,
    ) -> impl Iterator<Item = (TermRef<'_>, Self::TermLocation)> + '_;
}

/// Iterates over all sub-terms recursively.
///
/// The iteration includes the root term and traverses descendants in
/// depth-first order.
pub trait RecursivelyIterSubTerms {
    /// Returns an iterator over this term and all of its descendants.
    fn iter_sub_terms_recursive(
        &self,
    ) -> impl Iterator<Item = TermRef<'_>> + '_;
}

impl<Term> RecursivelyIterSubTerms for Interned<Term>
where
    for<'this> &'this Self: Into<TermRef<'this>>,
{
    fn iter_sub_terms_recursive(
        &self,
    ) -> impl Iterator<Item = TermRef<'_>> + '_ {
        iter_sub_terms_recursive_from_term_ref(self.into())
    }
}

/// Represents a type used to retrieve a sub-term of a particular term.
pub trait Location<Term, SubTerm>:
    Debug
    + Clone
    + Copy
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + Hash
    + Send
    + Sync
    + Into<TermLocation>
    + 'static
{
    /// Returns the sub-term at this location.
    #[must_use]
    fn try_get_sub_term(
        self,
        term: &Term,
        tracked_engine: &TrackedEngine,
    ) -> Option<Interned<SubTerm>>;

    /// Returns the sub-term at this location or panics if the location is
    /// invalid for the given term.
    #[must_use]
    fn get_sub_term(
        self,
        term: &Term,
        tracked_engine: &TrackedEngine,
    ) -> Interned<SubTerm>
    where
        Term: Debug,
    {
        self.try_get_sub_term(term, tracked_engine).unwrap_or_else(|| {
            panic!(
                "invalid sub-term location: {:?} for term: {term:?}",
                Into::<TermLocation>::into(self),
            )
        })
    }
}

/// An enumeration of locations where a lifetime can be located.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
pub enum SubLifetimeLocation {
    /// The location points to a lifetime that is a part of a type.
    FromType(r#type::SubLifetimeLocation),

    /// The location points to a lifetime that is a part of an instance.
    FromInstance(instance::SubLifetimeLocation),
}

/// An enumeration of locations where a type can be located.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
pub enum SubTypeLocation {
    /// The location points to a type that is a part of a type.
    FromType(r#type::SubTypeLocation),

    /// The location points to a type that is a part of an instance.
    FromInstance(instance::SubTypeLocation),
}

/// An enumeration of locations where a constant can be located.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
pub enum SubConstantLocation {
    /// The location points to a constant that is a part of a type.
    FromType(r#type::SubConstantLocation),

    /// The location points to a constant that is a part of a constant.
    FromConstant(constant::SubConstantLocation),

    /// The location points to a constant that is a part of an instance.
    FromInstance(instance::SubConstantLocation),
}

/// An enumeration of locations where an instance can be located.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
pub enum SubInstanceLocation {
    /// The location points to an instance that is a part of a type.
    FromType(r#type::SubInstanceLocation),

    /// The location points to an instance that is a part of an instance.
    FromInstance(instance::SubInstanceLocation),
}

/// Enumeration of all sub-location of all kinds of terms.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
pub enum TermLocation {
    /// The location points to a lifetime.
    Lifetime(SubLifetimeLocation),

    /// The location points to a type.
    Type(SubTypeLocation),

    /// The location points to a constant.
    Constant(SubConstantLocation),

    /// The location points to an instance.
    Instance(SubInstanceLocation),
}

fn extend_stack_with_sub_terms<'this>(
    term_ref: TermRef<'this>,
    stack: &mut Vec<TermRef<'this>>,
) {
    let start = stack.len();

    match term_ref {
        TermRef::Constant(constant) => {
            for (sub_term, _) in constant.as_ref().iter_sub_terms() {
                stack.push(sub_term);
            }
        }

        TermRef::Lifetime(lifetime) => {
            for (sub_term, _) in lifetime.as_ref().iter_sub_terms() {
                stack.push(sub_term);
            }
        }

        TermRef::Type(ty) => {
            for (sub_term, _) in ty.as_ref().iter_sub_terms() {
                stack.push(sub_term);
            }
        }

        TermRef::Instance(instance) => {
            for (sub_term, _) in instance.as_ref().iter_sub_terms() {
                stack.push(sub_term);
            }
        }
    }

    // Reverse newly added children so DFS pops them in source iteration order.
    stack[start..].reverse();
}

fn iter_sub_terms_recursive_from_term_ref(
    term_ref: TermRef<'_>,
) -> impl Iterator<Item = TermRef<'_>> + '_ {
    let mut stack = vec![term_ref];

    std::iter::from_fn(move || {
        let current = stack.pop()?;

        extend_stack_with_sub_terms(current, &mut stack);

        Some(current)
    })
}

impl From<Never> for TermLocation {
    fn from(never: Never) -> Self { match never {} }
}

impl<Term, SubTerm> Location<Term, SubTerm> for Never {
    fn try_get_sub_term(
        self,
        _: &Term,
        _: &TrackedEngine,
    ) -> Option<Interned<SubTerm>> {
        match self {}
    }
}
