//! Contains various definition of predicates.

mod constant_type;
mod definite;
mod outlives;
mod r#trait;
mod tuple;

use enum_as_inner::EnumAsInner;

use super::{
    instantiation::{self, Instantiation},
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
    visitor::{accept_recursive, Recursive, SubTermLocation},
};
use crate::table::{self, DisplayObject, State};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ContainsForallLifetimeVisitor {
    contains_forall_lifetime: bool,
}

impl Recursive<Lifetime> for ContainsForallLifetimeVisitor {
    fn visit(
        &mut self,
        term: &Lifetime,
        _: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        if term.is_forall() {
            self.contains_forall_lifetime = true;
            false
        } else {
            true
        }
    }
}

impl Recursive<Type> for ContainsForallLifetimeVisitor {
    fn visit(
        &mut self,
        _: &Type,
        _: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        true
    }
}

impl Recursive<Constant> for ContainsForallLifetimeVisitor {
    fn visit(
        &mut self,
        _: &Constant,
        _: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        true
    }
}

fn contains_forall_lifetime<T: Term>(term: &T) -> bool {
    let mut visitor =
        ContainsForallLifetimeVisitor { contains_forall_lifetime: false };

    accept_recursive(term, &mut visitor);

    visitor.contains_forall_lifetime
}

/// Enumeration containing either a lifetime or a type outlives predicate.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeConstraint {
    LifetimeOutlives(Outlives<Lifetime>),
    TypeOutlives(Outlives<Type>),
}

/// Describes a satisfiability of a certain predicate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Satisfiability {
    /// The predicate is satisfiable.
    Satisfied,

    /// The predicate is unsatisfiable.
    Unsatisfied,

    /// If all the sub-term of the predicate are satisfiable, then the
    /// predicate is satisfiable.
    Congruent,
}

pub use constant_type::{
    ConstantType, Query as ConstantTypeQuery,
    QuerySource as ConstantTypeQuerySource,
};
pub use definite::{definite, Query as DefiniteQuery};
pub use outlives::{Outlives, Query as OutlivesQuery};
pub use r#trait::{
    resolve_implementation, Implementation, Query as TraitQuery,
    ResolveError as TraitResolveError, Satisfiability as TraitSatisfiability,
    Trait,
};
pub use tuple::{Query as TupleQuery, Tuple};

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
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Predicate {
    TraitTypeEquality(TraitMemberEquality<Type>),
    ConstantType(ConstantType),
    LifetimeOutlives(Outlives<Lifetime>),
    TypeOutlives(Outlives<Type>),
    TupleType(Tuple<Type>),
    TupleConstant(Tuple<Constant>),
    Trait(Trait),
}

impl<T: State> table::Display<T> for Predicate {
    fn fmt(
        &self,
        table: &table::Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::TraitTypeEquality(equality) => equality.fmt(table, f),
            Self::ConstantType(constant_type) => constant_type.fmt(table, f),
            Self::LifetimeOutlives(outlives) => outlives.fmt(table, f),
            Self::TypeOutlives(outlives) => outlives.fmt(table, f),
            Self::TupleType(tuple) => tuple.fmt(table, f),
            Self::TupleConstant(tuple) => tuple.fmt(table, f),
            Self::Trait(tr) => tr.fmt(table, f),
        }
    }
}

impl Predicate {
    /// Checks if the predicate contains a `forall` lifetime.
    #[must_use]
    pub fn contains_forall_lifetime(&self) -> bool {
        match self {
            Self::TraitTypeEquality(equality) => {
                equality.contains_forall_lifetime()
            }
            Self::ConstantType(constant_type) => {
                constant_type.contains_forall_lifetime()
            }
            Self::LifetimeOutlives(outlives) => {
                outlives.contains_forall_lifetime()
            }
            Self::TypeOutlives(outlives) => outlives.contains_forall_lifetime(),
            Self::TupleType(tuple) => tuple.contains_forall_lifetime(),
            Self::TupleConstant(tuple) => tuple.contains_forall_lifetime(),
            Self::Trait(tr) => tr.contains_forall_lifetime(),
        }
    }

    /// Applies the instantiate to the predicate.
    pub fn instantiate(&mut self, substitution: &Instantiation) {
        match self {
            Self::TraitTypeEquality(equality) => {
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
            Self::TupleConstant(tuple) => tuple.instantiate(substitution),
            Self::Trait(tr) => tr.instantiate(substitution),
        }
    }
}

/// A predicate that two terms are equal.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitMemberEquality<T: Term> {
    /// The trait member term under comparison.
    pub trait_member: T::TraitMember,

    /// The term that the trait member is equivalent to.
    pub equivalent: T,
}

impl<S: State, T: table::Display<S> + Term> table::Display<S>
    for TraitMemberEquality<T>
where
    T::TraitMember: table::Display<S>,
{
    fn fmt(
        &self,
        table: &table::Table<S>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{} = {}",
            DisplayObject { display: &self.trait_member, table },
            DisplayObject { display: &self.equivalent, table }
        )
    }
}

impl<T: Term> TraitMemberEquality<T> {
    /// Checks if the predicate has a `forall` lifetime.
    pub fn contains_forall_lifetime(&self) -> bool {
        let trait_member = T::from(self.trait_member.clone());

        contains_forall_lifetime(&trait_member)
            || contains_forall_lifetime(&self.equivalent)
    }
}

impl TraitMemberEquality<Type> {
    /// Applies the instantiation to both [`TraitMemberEquality::trait_member`]
    /// and [`TraitMemberEquality::equivalence`].
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        self.trait_member.member_generic_arguments.instantiate(instantiation);
        self.trait_member.parent_generic_arguments.instantiate(instantiation);
        instantiation::instantiate(&mut self.equivalent, instantiation);
    }
}
