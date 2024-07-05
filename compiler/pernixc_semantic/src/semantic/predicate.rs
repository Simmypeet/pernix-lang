//! Contains various definition of predicates.

mod constant_type;
mod outlives;
mod r#trait;
mod tuple;

use enum_as_inner::EnumAsInner;

use super::{
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
use crate::symbol::table::{self, DisplayObject, State};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ContainsForallLifetimeVisitor {
    contains_forall_lifetime: bool,
}

impl<'v, M: Model> Recursive<'v, Lifetime<M>>
    for ContainsForallLifetimeVisitor
{
    fn visit(
        &mut self,
        term: &'v Lifetime<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_forall() {
            self.contains_forall_lifetime = true;
            false
        } else {
            true
        }
    }
}

impl<'v, M: Model> Recursive<'v, Type<M>> for ContainsForallLifetimeVisitor {
    fn visit(
        &mut self,
        _: &'v Type<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl<'v, M: Model> Recursive<'v, Constant<M>>
    for ContainsForallLifetimeVisitor
{
    fn visit(
        &mut self,
        _: &'v Constant<M>,
        _: impl Iterator<Item = TermLocation>,
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
pub use outlives::Outlives;
pub(super) use r#trait::resolve_implementation_with_context;
pub use r#trait::{
    resolve_implementation, Implementation, ResolveError as TraitResolveError,
    Trait,
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
    ConstantType(ConstantType<Type<M>>),
    LifetimeOutlives(Outlives<Lifetime<M>>),
    TypeOutlives(Outlives<Type<M>>),
    TupleType(Tuple<Type<M>>),
    TupleConstant(Tuple<Constant<M>>),
    Trait(Trait<M>),
}

impl<M: Model> Predicate<M> {
    /// Converts the [`Predicate`] with [`Default`] model to the model `M`.
    pub fn from_default_model(predicate: Predicate<Default>) -> Self {
        match predicate {
            Predicate::TraitTypeEquality(x) => {
                Predicate::TraitTypeEquality(Equality {
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
                Predicate::ConstantType(ConstantType(M::from_default_type(x.0)))
            }
            Predicate::LifetimeOutlives(x) => {
                Predicate::LifetimeOutlives(Outlives {
                    operand: M::from_default_lifetime(x.operand),
                    bound: M::from_default_lifetime(x.bound),
                })
            }
            Predicate::TypeOutlives(x) => Predicate::TypeOutlives(Outlives {
                operand: M::from_default_type(x.operand),
                bound: M::from_default_lifetime(x.bound),
            }),
            Predicate::TupleType(x) => {
                Predicate::TupleType(Tuple(M::from_default_type(x.0)))
            }
            Predicate::TupleConstant(x) => {
                Predicate::TupleConstant(Tuple(M::from_default_constant(x.0)))
            }
            Predicate::Trait(x) => {
                Predicate::Trait(Trait::from_default_model(x))
            }
        }
    }
}

impl<T: State, M: Model> table::Display<T> for Predicate<M>
where
    Equality<r#type::TraitMember<M>, Type<M>>: table::Display<T>,
    ConstantType<Type<M>>: table::Display<T>,
    Outlives<Lifetime<M>>: table::Display<T>,
    Outlives<Type<M>>: table::Display<T>,
    Tuple<Type<M>>: table::Display<T>,
    Tuple<Constant<M>>: table::Display<T>,
    Trait<M>: table::Display<T>,
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
            Self::Trait(tr) => tr.fmt(table, f),
        }
    }
}

impl<M: Model> Predicate<M> {
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
            Self::Trait(tr) => tr.instantiate(substitution),
        }
    }
}

/// A predicate that two terms are equal.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Equality<T, U = T> {
    /// The trait member term under comparison.
    pub lhs: T,

    /// The term that the trait member is equivalent to.
    pub rhs: U,
}

impl<S: State, T: table::Display<S>, U: table::Display<S>> table::Display<S>
    for Equality<T, U>
{
    fn fmt(
        &self,
        table: &table::Table<S>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{} = {}",
            DisplayObject { display: &self.lhs, table },
            DisplayObject { display: &self.rhs, table }
        )
    }
}

impl<T: Term> Equality<T::TraitMember, T> {
    /// Checks if the predicate has a `forall` lifetime.
    pub fn contains_forall_lifetime(&self) -> bool {
        let trait_member = T::from(self.lhs.clone());

        contains_forall_lifetime(&trait_member)
            || contains_forall_lifetime(&self.rhs)
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
