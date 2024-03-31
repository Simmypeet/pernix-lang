//! Contains the three fundamental terms of the language: [`Type`],
//! [`Constant`], and [`Lifetime`].

use core::fmt;
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
    equality,
    instantiation::{self, Instantiation},
    mapping::Mapping,
    matching,
    predicate::{self, Outlives, Satisfiability},
    session::{ExceedLimitError, Limit, Session},
    sub_term::{self, AssignSubTermError, SubTupleLocation},
    unification::{self, Unification},
    visitor, Premise, Semantic,
};
use crate::{
    arena::ID,
    symbol::{GenericID, GenericParameter, GlobalID, MemberID},
    table::{self, DisplayObject, State, Table},
};

pub mod constant;
pub mod lifetime;
pub mod r#type;

/// Represents a generic arguments supplied to a term (i.e., `type[ARGS]`).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct GenericArguments {
    /// The lifetimes supplied to the term.
    pub lifetimes: Vec<Lifetime>,

    /// The types supplied to the term.
    pub types: Vec<Type>,

    /// The constants supplied to the term.
    pub constants: Vec<Constant>,
}

impl<T: State> table::Display<T> for GenericArguments {
    fn fmt(
        &self,
        table: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        if self.lifetimes.is_empty()
            && self.types.is_empty()
            && self.constants.is_empty()
        {
            return Ok(());
        }

        let mut lifetimes = self.lifetimes.iter().peekable();
        let mut types = self.types.iter().peekable();
        let mut constants = self.constants.iter().peekable();

        write!(f, "[")?;

        while let Some(lifetime) = lifetimes.next() {
            let is_last = lifetimes.peek().is_none()
                && self.types.is_empty()
                && self.constants.is_empty();

            write!(f, "{}", DisplayObject { table, display: lifetime })?;

            if !is_last {
                write!(f, ", ")?;
            }
        }

        while let Some(r#type) = types.next() {
            let is_last = types.peek().is_none() && self.constants.is_empty();

            write!(f, "{}", DisplayObject { table, display: r#type })?;

            if !is_last {
                write!(f, ", ")?;
            }
        }

        while let Some(constant) = constants.next() {
            let is_last = constants.peek().is_none();

            write!(f, "{}", DisplayObject { table, display: constant })?;

            if !is_last {
                write!(f, ", ")?;
            }
        }

        write!(f, "]")
    }
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

impl<T: State, ID: Into<GlobalID> + Copy> table::Display<T> for Symbol<ID> {
    fn fmt(
        &self,
        table: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let qualified_name =
            table.get_qualified_name(self.id.into()).ok_or(fmt::Error)?;

        write!(f, "{qualified_name}{}", DisplayObject {
            table,
            display: &self.generic_arguments,
        })
    }
}

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
impl<T: State, ID: Copy + Into<GlobalID>> table::Display<T>
    for MemberSymbol<ID>
{
    fn fmt(
        &self,
        table: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let this_sym = table.get_global(self.id.into()).ok_or(fmt::Error)?;
        let parent_qualified_identifier = table
            .get_qualified_name(this_sym.parent_global_id().ok_or(fmt::Error)?)
            .ok_or(fmt::Error)?;

        write!(f, "{parent_qualified_identifier}{}", DisplayObject {
            table,
            display: &self.parent_generic_arguments,
        })?;

        write!(f, "::{}{}", this_sym.name(), DisplayObject {
            table,
            display: &self.member_generic_arguments,
        })
    }
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

impl<S: State, T: table::Display<S> + Clone> table::Display<S> for Tuple<T> {
    fn fmt(
        &self,
        table: &Table<S>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "(")?;

        let mut peekable = self.elements.iter().peekable();

        while let Some(element) = peekable.next() {
            let is_last = peekable.peek().is_none();
            let is_unpacked = element.is_unpacked();
            let term = element.as_term();

            if is_unpacked {
                write!(f, "...")?;
            }

            write!(f, "{}", DisplayObject { table, display: term })?;

            if !is_last {
                write!(f, ", ")?;
            }
        }

        write!(f, ")")
    }
}

/// A type that can't never be instantiated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Never {}

/// The term under the `local` modifier.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Local<T>(pub Box<T>)
where
    Self: Into<T>;

/// Contains the functionality for determining the properties of a term.
pub trait Term:
    Debug
    + Eq
    + Hash
    + Sized
    + Clone
    + Ord
    + visitor::Element
    + unification::Element
    + matching::Match
    + sub_term::SubTerm
{
    /// The type of generic parameters of this term kind.
    type GenericParameter: GenericParameter + 'static;

    /// The type of trait member symbol that stores this term kind.
    type TraitMember: 'static;

    #[doc(hidden)]
    fn as_generic_parameter(
        &self,
    ) -> Option<&MemberID<ID<Self::GenericParameter>, GenericID>>;

    #[doc(hidden)]
    fn as_generic_parameter_mut(
        &mut self,
    ) -> Option<&mut MemberID<ID<Self::GenericParameter>, GenericID>>;

    #[doc(hidden)]
    fn into_generic_parameter(
        self,
    ) -> Result<MemberID<ID<Self::GenericParameter>, GenericID>, Self>;

    #[doc(hidden)]
    fn as_trait_member(&self) -> Option<&MemberSymbol<ID<Self::TraitMember>>>;

    #[doc(hidden)]
    fn as_trait_member_mut(
        &mut self,
    ) -> Option<&mut MemberSymbol<ID<Self::TraitMember>>>;

    #[doc(hidden)]
    fn into_trait_member(
        self,
    ) -> Result<MemberSymbol<ID<Self::TraitMember>>, Self>;

    #[doc(hidden)]
    fn as_tuple(&self) -> Option<&Tuple<Self>>;

    #[doc(hidden)]
    fn as_tuple_mut(&mut self) -> Option<&mut Tuple<Self>>;

    #[doc(hidden)]
    fn into_tuple(self) -> Result<Tuple<Self>, Self>;

    #[doc(hidden)]
    fn get_adt_fields(&self, table: &Table<impl State>) -> Option<Vec<Self>>;

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
    fn tuple_predicates<'a>(
        premise: &'a Premise,
    ) -> impl Iterator<Item = &'a Self>
    where
        Self: 'a;

    #[doc(hidden)]
    fn definite_satisfiability(&self) -> Satisfiability;

    #[doc(hidden)]
    fn constant_type_satisfiability(&self) -> Satisfiability;

    #[doc(hidden)]
    fn get_instantiation(instantiation: &Instantiation)
        -> &HashMap<Self, Self>;

    #[doc(hidden)]
    fn get_instantiation_mut(
        instantiation: &mut Instantiation,
    ) -> &mut HashMap<Self, Self>;

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
    #[allow(clippy::too_many_lines)]
    fn substructural_match_internal<'a>(
        from: &'a Self,
        to: &'a Self,
        swap: bool,
    ) -> Option<
        matching::Substructural<
            T::SubLifetimeLocation,
            T::SubTypeLocation,
            T::SubConstantLocation,
        >,
    >
    where
        T::ThisSubTermLocation: From<SubTupleLocation>,
    {
        fn push<T: Term>(
            lhs: T,
            rhs: T,
            lhs_location: SubTupleLocation,
            rhs_location: SubTupleLocation,
            existing: &mut matching::Substructural<
                T::SubLifetimeLocation,
                T::SubTypeLocation,
                T::SubConstantLocation,
            >,
            swap: bool,
        ) where
            T::ThisSubTermLocation: From<SubTupleLocation>,
        {
            if swap {
                T::get_substructural_mut(existing).push(matching::Matching {
                    lhs: rhs,
                    rhs: lhs,
                    lhs_location: rhs_location.into(),
                    rhs_location: lhs_location.into(),
                });
            } else {
                T::get_substructural_mut(existing).push(matching::Matching {
                    lhs,
                    rhs,
                    lhs_location: lhs_location.into(),
                    rhs_location: rhs_location.into(),
                });
            }
        }

        if from.elements.len() == to.elements.len() {
            let mut error = false;
            let mut existing = matching::Substructural::default();

            for (idx, (from_element, to_element)) in
                from.elements.iter().zip(&to.elements).enumerate()
            {
                if from_element.is_unpacked() != to_element.is_unpacked() {
                    error = true;
                    break;
                }

                let from_element = from_element.as_term();
                let to_element = to_element.as_term();

                push(
                    from_element.clone(),
                    to_element.clone(),
                    SubTupleLocation::Single(idx),
                    SubTupleLocation::Single(idx),
                    &mut existing,
                    swap,
                );
            }

            if !error {
                return Some(existing);
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

                let mut existing = matching::Substructural::default();

                for (idx, (from_element, to_element)) in
                    from.elements.iter().zip(&to.elements).enumerate()
                {
                    let from_element = from_element.as_regular().unwrap();
                    let to_element = to_element.as_regular().unwrap();

                    push(
                        from_element.clone(),
                        to_element.clone(),
                        SubTupleLocation::Single(idx),
                        SubTupleLocation::Single(idx),
                        &mut existing,
                        swap,
                    );
                }

                return Some(existing);
            }

            (1, _) => {}

            (_, _) => return None,
        }

        let mut existing = matching::Substructural::default();

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
                SubTupleLocation::Single(idx),
                SubTupleLocation::Single(idx),
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
                SubTupleLocation::Single(idx + from_tail_range.start),
                SubTupleLocation::Single(idx + to_tail_range.start),
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
            SubTupleLocation::Single(unpacked_position),
            SubTupleLocation::Range {
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
        matching::Substructural<
            T::SubLifetimeLocation,
            T::SubTypeLocation,
            T::SubConstantLocation,
        >,
    >
    where
        T::ThisSubTermLocation: From<SubTupleLocation>,
    {
        Self::substructural_match_internal(self, to, false)
            .or_else(|| Self::substructural_match_internal(to, self, true))
    }
}

impl GenericArguments {
    /// Determines if two generic arguments are equal.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    pub fn equals<
        S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        &self,
        other: &Self,
        premise: &Premise,
        table: &Table<impl State>,
        semantic: &mut S,
        session: &mut Limit<R>,
    ) -> Result<bool, ExceedLimitError> {
        if self.lifetimes.len() != other.lifetimes.len()
            || self.types.len() != other.types.len()
            || self.constants.len() != other.constants.len()
        {
            return Ok(false);
        }

        for (lhs, rhs) in self.lifetimes.iter().zip(&other.lifetimes) {
            if !equality::equals(lhs, rhs, premise, table, semantic, session)? {
                return Ok(false);
            }
        }

        for (lhs, rhs) in self.types.iter().zip(&other.types) {
            if !equality::equals(lhs, rhs, premise, table, semantic, session)? {
                return Ok(false);
            }
        }

        for (lhs, rhs) in self.constants.iter().zip(&other.constants) {
            if !equality::equals(lhs, rhs, premise, table, semantic, session)? {
                return Ok(false);
            }
        }

        Ok(true)
    }

    /// Gets the [`GlobalID`]s that occur in the generic arguments.
    #[must_use]
    pub fn get_global_id_dependencies(
        &self,
        table: &Table<impl State>,
    ) -> Option<Vec<GlobalID>> {
        let mut occurrences = Vec::new();

        for r#type in &self.types {
            occurrences.extend(r#type.get_global_id_dependencies(table)?);
        }

        for constant in &self.constants {
            occurrences.extend(constant.get_global_id_dependencies(table)?);
        }

        occurrences.sort_unstable();
        occurrences.dedup();

        Some(occurrences)
    }

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

    /// Applies the instantiation to all the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        for lifetime in &mut self.lifetimes {
            instantiation::instantiate(lifetime, instantiation);
        }

        for r#type in &mut self.types {
            instantiation::instantiate(r#type, instantiation);
        }

        for constant in &mut self.constants {
            instantiation::instantiate(constant, instantiation);
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
                return Ok(false);
            }
        }

        for r#type in &self.types {
            if !predicate::definite(r#type, premise, table, semantic, session)?
            {
                return Ok(false);
            }
        }

        for constant in &self.constants {
            if !predicate::definite(
                constant, premise, table, semantic, session,
            )? {
                return Ok(false);
            }
        }

        Ok(true)
    }

    fn substructural_match<L, T, C, Y>(
        &self,
        other: &Self,
        mut existing: matching::Substructural<L, T, C>,
        to_location: impl Fn(usize) -> Y,
    ) -> Option<matching::Substructural<L, T, C>>
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
            existing.lifetimes.push(matching::Matching {
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
            existing.types.push(matching::Matching {
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
            existing.constants.push(matching::Matching {
                lhs,
                rhs,
                lhs_location: location.into(),
                rhs_location: location.into(),
            });
        }

        Some(existing)
    }
}

#[cfg(test)]
mod tests;
