//! Contains various definition of predicates.

mod constant_type;
mod marker;
mod outlives;
mod resolution;
mod r#trait;
mod tuple;

use enum_as_inner::EnumAsInner;

use super::{
    equality::Equality,
    instantiation::{self, Instantiation},
    model::{Default, Model},
    sub_term::TermLocation,
    term::{
        constant::Constant,
        lifetime::Lifetime,
        r#type::{self, Type},
        GenericArguments, MemberSymbol, Term,
    },
    visitor::{accept_recursive, Recursive},
};
use crate::symbol::table::{self, State};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ContainsErrorVisitor {
    contains_error: bool,
}

impl<'v, M: Model> Recursive<'v, Lifetime<M>> for ContainsErrorVisitor {
    fn visit(
        &mut self,
        term: &Lifetime<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_error() {
            self.contains_error = true;
            false
        } else {
            true
        }
    }
}

impl<'v, M: Model> Recursive<'v, Type<M>> for ContainsErrorVisitor {
    fn visit(
        &mut self,
        term: &Type<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_error() {
            self.contains_error = true;
            false
        } else {
            true
        }
    }
}

impl<'v, M: Model> Recursive<'v, Constant<M>> for ContainsErrorVisitor {
    fn visit(
        &mut self,
        term: &Constant<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_error() {
            self.contains_error = true;
            false
        } else {
            true
        }
    }
}

fn contains_error<T: Term>(term: &T) -> bool {
    let mut visitor = ContainsErrorVisitor { contains_error: false };

    accept_recursive(term, &mut visitor);

    visitor.contains_error
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

pub use constant_type::{ConstantType, QuerySource as ConstantTypeQuerySource};
pub use marker::{
    Negative as NegativeMarker, NegativeSatisfied as NegativeMarkerSatisfied,
    Positive as PositiveMarker, PositiveSatisfied as PositiveMarkerSatisfied,
};
pub use outlives::Outlives;
pub use r#trait::{
    Kind as TraitKind, Negative as NegativeTrait,
    NegativeSatisfied as NegativeTraitSatisfied, Positive as PositiveTrait,
    PositiveSatisfied as PositiveTraitSatisfied,
};
pub use resolution::{
    resolve_implementation, resolve_implementation_with_context,
    Error as ResolutionError, Implementation,
};
pub use tuple::Tuple;

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
pub enum Predicate<M: Model> {
    TraitTypeEquality(Equality<r#type::TraitMember<M>, Type<M>>),
    ConstantType(ConstantType<M>),
    LifetimeOutlives(Outlives<Lifetime<M>>),
    TypeOutlives(Outlives<Type<M>>),
    TupleType(Tuple<Type<M>>),
    TupleConstant(Tuple<Constant<M>>),
    PositiveTrait(PositiveTrait<M>),
    NegativeTrait(NegativeTrait<M>),
    PositiveMarker(PositiveMarker<M>),
    NegativeMarker(NegativeMarker<M>),
}

impl<M: Model> Predicate<M> {
    /// Converts the [`Predicate`] with [`Default`] model to the model `M`.
    #[must_use]
    pub fn from_default_model(predicate: Predicate<Default>) -> Self {
        match predicate {
            Predicate::TraitTypeEquality(x) => {
                Self::TraitTypeEquality(Equality {
                    lhs: MemberSymbol {
                        id: x.lhs.id,
                        member_generic_arguments:
                            GenericArguments::from_default_model(
                                x.lhs.member_generic_arguments,
                            ),
                        parent_generic_arguments:
                            GenericArguments::from_default_model(
                                x.lhs.parent_generic_arguments,
                            ),
                    },
                    rhs: M::from_default_type(x.rhs),
                })
            }
            Predicate::ConstantType(x) => {
                Self::ConstantType(ConstantType(M::from_default_type(x.0)))
            }
            Predicate::LifetimeOutlives(x) => {
                Self::LifetimeOutlives(Outlives {
                    operand: M::from_default_lifetime(x.operand),
                    bound: M::from_default_lifetime(x.bound),
                })
            }
            Predicate::TypeOutlives(x) => Self::TypeOutlives(Outlives {
                operand: M::from_default_type(x.operand),
                bound: M::from_default_lifetime(x.bound),
            }),
            Predicate::TupleType(x) => {
                Self::TupleType(Tuple(M::from_default_type(x.0)))
            }
            Predicate::TupleConstant(x) => {
                Self::TupleConstant(Tuple(M::from_default_constant(x.0)))
            }
            Predicate::PositiveTrait(x) => {
                Self::PositiveTrait(PositiveTrait::from_default_model(x))
            }
            Predicate::NegativeTrait(x) => {
                Self::NegativeTrait(NegativeTrait::from_default_model(x))
            }
            Predicate::PositiveMarker(positive) => Self::PositiveMarker(
                PositiveMarker::from_default_model(positive),
            ),
            Predicate::NegativeMarker(negative) => Self::NegativeMarker(
                NegativeMarker::from_default_model(negative),
            ),
        }
    }
}

impl<T: State, M: Model> table::Display<T> for Predicate<M>
where
    Equality<r#type::TraitMember<M>, Type<M>>: table::Display<T>,
    ConstantType<M>: table::Display<T>,
    Outlives<Lifetime<M>>: table::Display<T>,
    Outlives<Type<M>>: table::Display<T>,
    Tuple<Type<M>>: table::Display<T>,
    Tuple<Constant<M>>: table::Display<T>,
    PositiveTrait<M>: table::Display<T>,
    NegativeTrait<M>: table::Display<T>,
    PositiveMarker<M>: table::Display<T>,
    NegativeMarker<M>: table::Display<T>,
{
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
            Self::PositiveTrait(tr) => tr.fmt(table, f),
            Self::NegativeTrait(tr) => tr.fmt(table, f),
            Self::PositiveMarker(marker) => marker.fmt(table, f),
            Self::NegativeMarker(marker) => marker.fmt(table, f),
        }
    }
}

impl<M: Model> Predicate<M> {
    /// Checks if the predicate has an errornous term.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        match self {
            Self::TraitTypeEquality(equality) => equality.contains_error(),
            Self::ConstantType(constant_type) => constant_type.contains_error(),
            Self::LifetimeOutlives(outlives) => outlives.contains_error(),
            Self::TypeOutlives(outlives) => outlives.contains_error(),
            Self::TupleType(tuple) => tuple.contains_error(),
            Self::TupleConstant(tuple) => tuple.contains_error(),
            Self::PositiveTrait(tr) => tr.contains_error(),
            Self::NegativeTrait(tr) => tr.contains_error(),
            Self::PositiveMarker(marker) => marker.contains_error(),
            Self::NegativeMarker(marker) => marker.contains_error(),
        }
    }

    /// Applies the instantiate to the predicate.
    pub fn instantiate(&mut self, substitution: &Instantiation<M>) {
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
            Self::PositiveTrait(tr) => tr.instantiate(substitution),
            Self::NegativeTrait(tr) => tr.instantiate(substitution),
            Self::PositiveMarker(marker) => marker.instantiate(substitution),
            Self::NegativeMarker(marker) => marker.instantiate(substitution),
        }
    }
}

impl<T: Term> Equality<T::TraitMember, T> {
    /// Checks if the predicate has a `forall` lifetime.
    pub fn contains_error(&self) -> bool {
        let trait_member = T::from(self.lhs.clone());

        contains_error(&trait_member) || contains_error(&self.rhs)
    }
}

impl<M: Model> Equality<r#type::TraitMember<M>, Type<M>> {
    /// Applies the instantiation to both [`Equality::lhs`] and
    /// [`Equality::rhs`].
    pub fn instantiate(&mut self, instantiation: &Instantiation<M>) {
        self.lhs.member_generic_arguments.instantiate(instantiation);
        self.lhs.parent_generic_arguments.instantiate(instantiation);
        instantiation::instantiate(&mut self.rhs, instantiation);
    }
}
