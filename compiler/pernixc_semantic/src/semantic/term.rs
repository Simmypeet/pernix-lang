//! Contains the three fundamental terms of the language: [`Type`],
//! [`Constant`], and [`Lifetime`].

use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
};

use constant::Constant;
use enum_as_inner::EnumAsInner;
use lifetime::Lifetime;
use r#type::Type;

use super::{
    mapping::Mapping,
    predicate::{self, Outlives, Satisfiability},
    session::{ExceedLimitError, Limit, Session},
    substitution::{Substitute, Substitution},
    unification::{self, Unification},
    visitor, Premise, Semantic,
};
use crate::{
    arena::{Arena, ID},
    symbol::{GenericID, GenericParameter, GenericParameters, Variance},
    table::{State, Table},
};

pub mod constant;
pub mod lifetime;
pub mod r#type;

/// Represents a generic arguments supplied to a term (i.e., `type[ARGS]`).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArguments {
    /// The lifetimes supplied to the term.
    pub lifetimes: Vec<Lifetime>,

    /// The types supplied to the term.
    pub types: Vec<Type>,

    /// The constants supplied to the term.
    pub constants: Vec<Constant>,
}

/// Represents a term where its value is stored as a symbol (i.e., `type` or
/// `const` declaration).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol<ID> {
    /// The ID of the symbol that contains the value of the term.
    pub id: ID,

    /// The generic arguments supplied to the symbol.
    pub generic_arguments: GenericArguments,
}

impl<ID> Symbol<ID> {
    /// Returns a mutable reference to a particular sub-term of this generic
    /// arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term_mut<T: Term>(
        &mut self,
        location: SubSymbolTermLocation,
    ) -> Option<&mut T> {
        let generic_arguments =
            T::get_generic_arguments_mut(&mut self.generic_arguments);

        generic_arguments.get_mut(location.0)
    }

    /// Returns a reference to a particular sub-term of this generic arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term<T: Term>(
        &self,
        location: SubSymbolTermLocation,
    ) -> Option<&T> {
        let generic_arguments =
            T::get_generic_arguments(&self.generic_arguments);

        generic_arguments.get(location.0)
    }
}

/// Represents a sub-term location where the sub-term is stored as a generic
/// arguments.
///
/// The `usize` represents the index of the sub-term in the generic arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SubSymbolTermLocation(pub usize);

/// Represents a term where its value is stored as a member of a particular
/// symbol
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberSymbol<ID> {
    /// The ID of the symbol that contains the value of the term.
    pub id: ID,

    /// The generic arguments supplied to the member.
    pub member_generic_arguments: GenericArguments,

    /// The generic arguments supplied to the parent scope.
    pub parent_generic_arguments: GenericArguments,
}

impl<ID> MemberSymbol<ID> {
    /// Returns a mutable reference to a particular sub-term of this generic
    /// arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term_mut<T: Term>(
        &mut self,
        location: SubMemberSymbolTermLocation,
    ) -> Option<&mut T> {
        let generic_arguments = if location.from_parent {
            T::get_generic_arguments_mut(&mut self.parent_generic_arguments)
        } else {
            T::get_generic_arguments_mut(&mut self.member_generic_arguments)
        };

        generic_arguments.get_mut(location.index)
    }

    /// Returns a reference to a particular sub-term of this generic arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term<T: Term>(
        &self,
        location: SubMemberSymbolTermLocation,
    ) -> Option<&T> {
        let generic_arguments = if location.from_parent {
            T::get_generic_arguments(&self.parent_generic_arguments)
        } else {
            T::get_generic_arguments(&self.member_generic_arguments)
        };

        generic_arguments.get(location.index)
    }
}

/// Represents a sub-term location where the sub-term is stored as a generic
/// arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SubMemberSymbolTermLocation {
    /// The index of the sub-term in the generic arguments.
    pub index: usize,

    /// True if the sub-term is in the parent's generic arguments part,
    /// otherwise false.
    pub from_parent: bool,
}

/// Represents a single element of a tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum TupleElement<Term> {
    /// A regular term.
    Regular(Term),

    /// A term that can be unpacked into multiple terms.
    Unpacked(Term),
}

impl<Term> TupleElement<Term> {
    /// Returns a reference to the term in this element.
    #[must_use]
    pub const fn as_term(&self) -> &Term {
        match self {
            Self::Unpacked(term) | Self::Regular(term) => term,
        }
    }

    /// Returns a mutable reference to the term stored in this element.
    #[must_use]
    pub fn as_term_mut(&mut self) -> &mut Term {
        match self {
            Self::Unpacked(term) | Self::Regular(term) => term,
        }
    }

    /// Returns the term stored in this element.
    #[must_use]
    pub fn into_term(self) -> Term {
        match self {
            Self::Unpacked(term) | Self::Regular(term) => term,
        }
    }
}

/// Represents a tuple of terms.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Tuple<Term: Clone> {
    /// The elements of the tuple.
    pub elements: Vec<TupleElement<Term>>,
}

/// Represents a sub-term location where the sub-term is stored as an element of
/// a tuple.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SubTupleTermLocation {
    /// The sub-term is stored as a regular element of the tuple.
    Single(usize),

    /// The sub-term ranges into multiple elements of the tuple.
    Range {
        /// The index of the first element of the tuple.
        begin: usize,

        /// The index of the after the last element of the tuple.
        end: usize,
    },
}

/// A type that can't never be instantiated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Never {}

/// Represents a match between two terms.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Match<T, Location> {
    /// The left-hand side of the match.
    pub lhs: T,

    /// The right-hand side of the match.
    pub rhs: T,

    /// The location in lhs where the match occurred
    pub lhs_location: Location,

    /// The location in rhs where the match occurred
    pub rhs_location: Location,
}

/// Represents a substructural matching between two terms.
///
/// See [`Term::substructural_match`] for more information.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct SubstructuralMatching<
    SubLifetimeLocation,
    SubTypeLocation,
    SubConstantLocation,
> {
    pub lifetimes: Vec<Match<Lifetime, SubLifetimeLocation>>,
    pub types: Vec<Match<Type, SubTypeLocation>>,
    pub constants: Vec<Match<Constant, SubConstantLocation>>,
}

impl<L, T, C> Default for SubstructuralMatching<L, T, C> {
    fn default() -> Self {
        Self { lifetimes: Vec::new(), types: Vec::new(), constants: Vec::new() }
    }
}

/// The term under the `local` modifier.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Local<T>(pub Box<T>)
where
    Self: Into<T>;

/// An error that occurs when assigning a sub-term to a term.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum AssignSubTermError {
    #[error("the given location is invalid for this term")]
    InvalidLocation,
    #[error("the given term to assign was expected to be a tuple")]
    TupleExpected,
    #[error("the given tuple term to assign had a mismatched length")]
    InvalidTupleRange,
}

/// An error that occurs when getting the variance of a sub-term.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum GetVarianceError {
    #[error("the given location is invalid for this term")]
    InvalidLocation,

    #[error("the term contains an ID that is invalid to the given table")]
    InvalidID,
}

/// Represents a type used to retrieve a sub-term of a particular term.
pub trait SubTermLocation<Term, SubTerm>:
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
    + 'static
{
    /// Assigns the `sub_term` to the given `term` at this location.
    ///
    /// # Errors
    ///
    /// See [`AssignSubTermError`] for more information.
    fn assign_sub_term(
        self,
        term: &mut Term,
        sub_term: SubTerm,
    ) -> Result<(), AssignSubTermError>;

    /// Returns the sub-term at this location.
    #[must_use]
    fn get_sub_term(self, term: &Term) -> Option<SubTerm>;

    /// Returns the variance of the sub-term at this location.
    ///
    /// # Errors
    ///
    /// See [`GetVarianceError`] for more information.
    fn get_sub_variance(
        self,
        term: &Term,
        table: &Table<impl State>,
    ) -> Result<Variance, GetVarianceError>;
}

/// Contains the functionality for determining the properties of a term.
pub trait Term:
    Debug
    + Eq
    + Hash
    + Sized
    + Clone
    + Ord
    + Substitute
    + visitor::Element
    + unification::Element
{
    /// The type used to retrieve the sub-type term of a term.
    type SubTypeLocation: SubTermLocation<Self, Type>;

    /// The type used to retrieve the sub-lifetime term of a term.
    type SubLifetimeLocation: SubTermLocation<Self, Lifetime>;

    /// The type used to retrieve the sub-constant term of a term.
    type SubConstantLocation: SubTermLocation<Self, Constant>;

    /// The type used to retrieve the sub-tern of this type.
    type ThisSubTermLocation: SubTermLocation<Self, Self>;

    /// The type of generic parameters of this term kind.
    type GenericParameter: GenericParameter + 'static;

    /// Returns the matching substructural matches between `self` and `other`.
    ///
    /// # Example
    ///
    /// If `self` is `List[T, Map[U, V]]` and `other` is `List[A, B]`, then the
    /// substructural matches are:
    ///
    /// - `T` and `A`
    /// - `Map[U, V]` and `B`
    ///
    /// # Returns
    ///
    /// Returns `None` if the terms cannot be substructurally matched.
    fn substructural_match(
        &self,
        other: &Self,
    ) -> Option<
        SubstructuralMatching<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    >;

    #[doc(hidden)]
    fn get_adt_fields(&self, table: &Table<impl State>) -> Option<Vec<Self>>;

    #[doc(hidden)]
    fn is_tuple(&self) -> bool;

    #[doc(hidden)]
    fn outlives_predicates<'a>(
        premise: &'a Premise,
    ) -> impl Iterator<Item = &'a Outlives<Self>>
    where
        Self: 'a;

    #[doc(hidden)]
    fn constant_type_predicates<'a>(
        premise: &'a Premise,
    ) -> impl Iterator<Item = &'a Self>
    where
        Self: 'a;

    #[doc(hidden)]
    fn definite_satisfiability(&self) -> Satisfiability;

    #[doc(hidden)]
    fn constant_type_satisfiability(&self) -> Satisfiability;

    #[doc(hidden)]
    fn get_substructural_matching(
        substructural: &SubstructuralMatching<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<Match<Self, Self::ThisSubTermLocation>>;

    #[doc(hidden)]
    fn get_substructural_matching_mut(
        substructural: &mut SubstructuralMatching<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<Match<Self, Self::ThisSubTermLocation>>;

    #[doc(hidden)]
    fn get_substructural_unification<'a, T: Term>(
        substructural: &'a unification::Substructural<T>,
    ) -> impl Iterator<Item = &'a Unification<Self>>
    where
        Self: 'a;

    #[doc(hidden)]
    fn get_mapping(mapping: &Mapping) -> &HashMap<Self, HashSet<Self>>;

    #[doc(hidden)]
    fn get_mapping_mut(
        mapping: &mut Mapping,
    ) -> &mut HashMap<Self, HashSet<Self>>;

    #[doc(hidden)]
    fn get_generic_parameters(
        parameters: &GenericParameters,
    ) -> &Arena<Self::GenericParameter>;

    #[doc(hidden)]
    fn get_generic_parameter_order(
        parameters: &GenericParameters,
    ) -> &[ID<Self::GenericParameter>];

    #[doc(hidden)]
    fn get_generic_arguments(generic_arguments: &GenericArguments) -> &[Self];

    #[doc(hidden)]
    fn get_generic_arguments_mut(
        generic_arguments: &mut GenericArguments,
    ) -> &mut Vec<Self>;
}

impl<T: Term> Tuple<T>
where
    Self: TryFrom<T, Error = T> + Into<T>,
{
    fn assign_sub_term(
        &mut self,
        location: SubTupleTermLocation,
        sub_term: T,
    ) -> Result<(), AssignSubTermError> {
        match location {
            SubTupleTermLocation::Single(idx) => {
                let element = self
                    .elements
                    .get_mut(idx)
                    .map(TupleElement::as_term_mut)
                    .ok_or(AssignSubTermError::InvalidLocation)?;

                *element = sub_term;

                Ok(())
            }
            SubTupleTermLocation::Range { begin, end } => {
                let Ok(sub_constant_tuple) = Self::try_from(sub_term) else {
                    return Err(AssignSubTermError::TupleExpected);
                };

                let tuple_elements = self
                    .elements
                    .get_mut(begin..end)
                    .ok_or(AssignSubTermError::InvalidLocation)?;

                if sub_constant_tuple.elements.len() != tuple_elements.len() {
                    return Err(AssignSubTermError::InvalidLocation);
                }

                for (lhs, rhs) in
                    tuple_elements.iter_mut().zip(sub_constant_tuple.elements)
                {
                    *lhs = rhs;
                }

                Ok(())
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn substructural_match_internal<'a>(
        from: &'a Self,
        to: &'a Self,
        swap: bool,
    ) -> Option<
        SubstructuralMatching<
            T::SubLifetimeLocation,
            T::SubTypeLocation,
            T::SubConstantLocation,
        >,
    >
    where
        T::ThisSubTermLocation: From<SubTupleTermLocation>,
    {
        fn push<T: Term>(
            lhs: T,
            rhs: T,
            lhs_location: SubTupleTermLocation,
            rhs_location: SubTupleTermLocation,
            existing: &mut SubstructuralMatching<
                T::SubLifetimeLocation,
                T::SubTypeLocation,
                T::SubConstantLocation,
            >,
            swap: bool,
        ) where
            T::ThisSubTermLocation: From<SubTupleTermLocation>,
        {
            if swap {
                T::get_substructural_matching_mut(existing).push(Match {
                    lhs: rhs,
                    rhs: lhs,
                    lhs_location: rhs_location.into(),
                    rhs_location: lhs_location.into(),
                });
            } else {
                T::get_substructural_matching_mut(existing).push(Match {
                    lhs,
                    rhs,
                    lhs_location: lhs_location.into(),
                    rhs_location: rhs_location.into(),
                });
            }
        }

        match (
            from.elements.iter().filter(|x| x.is_unpacked()).count(),
            to.elements.iter().filter(|x| x.is_unpacked()).count(),
        ) {
            (0, 0) => {
                if from.elements.len() != to.elements.len() {
                    return None;
                }

                let mut existing = SubstructuralMatching::default();

                for (idx, (from_element, to_element)) in
                    from.elements.iter().zip(&to.elements).enumerate()
                {
                    let from_element = from_element.as_regular().unwrap();
                    let to_element = to_element.as_regular().unwrap();

                    push(
                        from_element.clone(),
                        to_element.clone(),
                        SubTupleTermLocation::Single(idx),
                        SubTupleTermLocation::Single(idx),
                        &mut existing,
                        swap,
                    );
                }

                return Some(existing);
            }

            (1, _) => {}

            (_, _) => return None,
        }

        let mut existing = SubstructuralMatching::default();

        if from.elements.len() > to.elements.len() + 1 {
            return None;
        }

        let unpacked_position =
            from.elements.iter().position(TupleElement::is_unpacked).unwrap();

        let head_range = 0..unpacked_position;
        let from_tail_range = (unpacked_position + 1)..from.elements.len();
        let to_tail_range = (to.elements.len()
            - from_tail_range.clone().count())
            ..to.elements.len();
        let to_unpack_range = unpacked_position..to_tail_range.start;

        // unify head
        for (idx, (from_element, to_element)) in from.elements
            [head_range.clone()]
        .iter()
        .zip(&to.elements[head_range])
        .enumerate()
        {
            let from_element = from_element.as_regular().unwrap();
            let TupleElement::Regular(to_element) = to_element else {
                return None;
            };

            push(
                from_element.clone(),
                to_element.clone(),
                SubTupleTermLocation::Single(idx),
                SubTupleTermLocation::Single(idx),
                &mut existing,
                swap,
            );
        }

        // unify tail
        for (idx, (from_element, to_element)) in from.elements
            [from_tail_range.clone()]
        .iter()
        .zip(&to.elements[to_tail_range.clone()])
        .enumerate()
        {
            let from_element = from_element.as_regular().unwrap();
            let TupleElement::Regular(to_element) = to_element else {
                return None;
            };

            push(
                from_element.clone(),
                to_element.clone(),
                SubTupleTermLocation::Single(idx + from_tail_range.start),
                SubTupleTermLocation::Single(idx + to_tail_range.start),
                &mut existing,
                swap,
            );
        }

        let to_unpack =
            Self { elements: to.elements[to_unpack_range.clone()].to_vec() }
                .into();

        let unpacked = from.elements[unpacked_position].as_unpacked().unwrap();
        push(
            unpacked.clone(),
            to_unpack,
            SubTupleTermLocation::Single(unpacked_position),
            SubTupleTermLocation::Range {
                begin: to_unpack_range.start,
                end: to_unpack_range.end,
            },
            &mut existing,
            swap,
        );

        Some(existing)
    }

    fn substructural_match<'a>(
        &'a self,
        to: &'a Self,
    ) -> Option<
        SubstructuralMatching<
            T::SubLifetimeLocation,
            T::SubTypeLocation,
            T::SubConstantLocation,
        >,
    >
    where
        T::ThisSubTermLocation: From<SubTupleTermLocation>,
    {
        Self::substructural_match_internal(self, to, false)
            .or_else(|| Self::substructural_match_internal(to, self, true))
    }
}

impl GenericArguments {
    /// Returns a unification of `this` generic arguments to `other` generic
    /// arguments.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    pub fn unify_as_mapping<
        S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        &self,
        other: &Self,
        premise: &Premise,
        table: &Table<impl State>,
        config: &mut impl unification::Config,
        semantic: &mut S,
        session: &mut Limit<R>,
    ) -> Result<Option<Mapping>, ExceedLimitError> {
        let mut mapping = Mapping::default();

        if self.lifetimes.len() != other.lifetimes.len()
            || self.types.len() != other.types.len()
            || self.constants.len() != other.constants.len()
        {
            return Ok(None);
        }

        for (lhs, rhs) in self.lifetimes.iter().zip(&other.lifetimes) {
            let Some(unification) = unification::unify(
                lhs, rhs, premise, table, config, semantic, session,
            )?
            else {
                return Ok(None);
            };

            mapping.append_from_unification(unification);
        }

        for (lhs, rhs) in self.types.iter().zip(&other.types) {
            let Some(unification) = unification::unify(
                lhs, rhs, premise, table, config, semantic, session,
            )?
            else {
                return Ok(None);
            };

            mapping.append_from_unification(unification);
        }

        for (lhs, rhs) in self.constants.iter().zip(&other.constants) {
            let Some(unification) = unification::unify(
                lhs, rhs, premise, table, config, semantic, session,
            )?
            else {
                return Ok(None);
            };

            mapping.append_from_unification(unification);
        }

        Ok(Some(mapping))
    }

    /// Applies the substitution to all the generic arguments.
    pub fn apply(&mut self, substitution: &Substitution) {
        for lifetime in &mut self.lifetimes {
            lifetime.apply(substitution);
        }

        for r#type in &mut self.types {
            r#type.apply(substitution);
        }

        for constant in &mut self.constants {
            constant.apply(substitution);
        }
    }

    /// Checks if all the generic arguments are definite.
    ///
    /// See [`predicate::definite`] for more information.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    pub fn definite<
        S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        &self,
        premise: &Premise,
        table: &Table<impl State>,
        semantic: &mut S,
        session: &mut Limit<R>,
    ) -> Result<bool, ExceedLimitError> {
        for lifetime in &self.lifetimes {
            if !predicate::definite(
                lifetime, premise, table, semantic, session,
            )? {
                return Ok(true);
            }
        }

        for r#type in &self.types {
            if !predicate::definite(r#type, premise, table, semantic, session)?
            {
                return Ok(true);
            }
        }

        for constant in &self.constants {
            if !predicate::definite(
                constant, premise, table, semantic, session,
            )? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    fn substructural_match<L, T, C, Y>(
        &self,
        other: &Self,
        mut existing: SubstructuralMatching<L, T, C>,
        to_location: impl Fn(usize) -> Y,
    ) -> Option<SubstructuralMatching<L, T, C>>
    where
        Y: Into<L> + Into<T> + Into<C> + Copy,
    {
        if self.lifetimes.len() != other.lifetimes.len()
            || self.types.len() != other.types.len()
            || self.constants.len() != other.constants.len()
        {
            return None;
        }

        for (idx, (lhs, rhs)) in self
            .lifetimes
            .iter()
            .copied()
            .zip(other.lifetimes.iter().copied())
            .enumerate()
        {
            let location = to_location(idx);
            existing.lifetimes.push(Match {
                lhs,
                rhs,
                lhs_location: location.into(),
                rhs_location: location.into(),
            });
        }

        for (idx, (lhs, rhs)) in self
            .types
            .iter()
            .cloned()
            .zip(other.types.iter().cloned())
            .enumerate()
        {
            let location = to_location(idx);
            existing.types.push(Match {
                lhs,
                rhs,
                lhs_location: location.into(),
                rhs_location: location.into(),
            });
        }

        for (idx, (lhs, rhs)) in self
            .constants
            .iter()
            .cloned()
            .zip(other.constants.iter().cloned())
            .enumerate()
        {
            let location = to_location(idx);
            existing.constants.push(Match {
                lhs,
                rhs,
                lhs_location: location.into(),
                rhs_location: location.into(),
            });
        }

        Some(existing)
    }
}

impl<S: State> Table<S> {
    fn get_generic_parameter_variance<T: Term>(
        &self,
        generic_id: GenericID,
        generic_parameter_order: usize,
    ) -> Result<Variance, GetVarianceError> {
        let generic_sym =
            self.get_generic(generic_id).ok_or(GetVarianceError::InvalidID)?;

        let id = T::get_generic_parameter_order(
            &generic_sym.generic_declaration().parameters,
        )
        .get(generic_parameter_order)
        .copied()
        .ok_or(GetVarianceError::InvalidLocation)?;

        Ok(T::get_generic_parameters(
            &generic_sym.generic_declaration().parameters,
        )
        .get(id)
        .unwrap()
        .variance())
    }
}

#[cfg(test)]
mod tests;
