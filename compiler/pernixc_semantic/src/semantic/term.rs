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
    equality, equivalent,
    instantiation::{self, Instantiation},
    mapping::Mapping,
    matching,
    model::Model,
    normalizer::Normalizer,
    predicate::{self, Outlives, Predicate, Satisfiability},
    session::{ExceedLimitError, Limit, Session},
    sub_term::{self, AssignSubTermError, SubTupleLocation},
    unification::{self, Unification},
    visitor, Environment,
};
use crate::{
    arena::ID,
    semantic::model::Default,
    symbol::{
        table::{self, DisplayObject, State, Table},
        GenericID, GenericParameter, GlobalID, MemberID,
    },
};

pub mod constant;
pub mod lifetime;
pub mod r#type;

/// Represents a generic arguments supplied to a term (i.e., `type[ARGS]`).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct GenericArguments<M: Model> {
    /// The lifetimes supplied to the term.
    pub lifetimes: Vec<Lifetime<M>>,

    /// The types supplied to the term.
    pub types: Vec<Type<M>>,

    /// The constants supplied to the term.
    pub constants: Vec<Constant<M>>,
}

impl<T: State, M: Model> table::Display<T> for GenericArguments<M> {
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

impl<M: Model> GenericArguments<M> {
    /// Converts the generic arguments with the default model into the model `M`
    pub fn from_default_model(
        generic_arguments: GenericArguments<Default>,
    ) -> Self {
        Self {
            lifetimes: generic_arguments
                .lifetimes
                .into_iter()
                .map(|x| M::from_default_lifetime(x))
                .collect(),
            types: generic_arguments
                .types
                .into_iter()
                .map(|x| M::from_default_type(x))
                .collect(),
            constants: generic_arguments
                .constants
                .into_iter()
                .map(|x| M::from_default_constant(x))
                .collect(),
        }
    }
}

/// Represents a term where its value is stored as a symbol (i.e., `type` or
/// `const` declaration).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol<M: Model, ID> {
    /// The ID of the symbol that contains the value of the term.
    pub id: ID,

    /// The generic arguments supplied to the symbol.
    pub generic_arguments: GenericArguments<M>,
}

impl<T: State, ID: Into<GlobalID> + Copy, M: Model> table::Display<T>
    for Symbol<M, ID>
{
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
pub struct MemberSymbol<M: Model, ID> {
    /// The ID of the symbol that contains the value of the term.
    pub id: ID,

    /// The generic arguments supplied to the member.
    pub member_generic_arguments: GenericArguments<M>,

    /// The generic arguments supplied to the parent scope.
    pub parent_generic_arguments: GenericArguments<M>,
}
impl<T: State, ID: Copy + Into<GlobalID>, M: Model> table::Display<T>
    for MemberSymbol<M, ID>
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
pub struct Local<T: Term>(pub Box<T>)
where
    Self: Into<T>;

/// The trait used for specifying the model of a term.
pub trait ModelOf {
    /// In which model does the term belong to.
    type Model: Model;
}

/// Contains the functionality for determining the properties of a term.
#[allow(private_bounds)]
pub trait Term:
    Debug
    + Eq
    + Hash
    + Sized
    + Clone
    + Ord
    + ModelOf
    + visitor::Element
    + unification::Element
    + matching::Match
    + sub_term::SubTerm
    + equivalent::Get
    + From<MemberID<ID<Self::GenericParameter>, GenericID>>
    + From<Self::TraitMember>
    + 'static
{
    /// The type of generic parameters of this term kind.
    type GenericParameter: GenericParameter + 'static;

    /// The type of trait member symbol that stores this term kind.
    type TraitMember: Debug + Eq + Hash + Sized + Clone + Ord + 'static;

    /// The inference variable type of this term kind.
    type InferenceVariable: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + Into<Self>;

    /// Rebinds this kind of term to another model.
    type Rebind<M: Model>: Term<Model = M>;

    /// Normalizes the term.
    ///
    /// Normalization is the process of converting a term into more simpler
    /// forms.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    fn normalize(
        &self,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        limit: &mut Limit<
            impl Session<Self>
                + Session<Lifetime<Self::Model>>
                + Session<Type<Self::Model>>
                + Session<Constant<Self::Model>>,
        >,
    ) -> Result<Option<Self>, ExceedLimitError>;

    #[doc(hidden)]
    fn outlives_satisfiability(
        &self,
        lifetime: &Lifetime<Self::Model>,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        limit: &mut Limit<
            impl Session<Self>
                + Session<Lifetime<Self::Model>>
                + Session<Type<Self::Model>>
                + Session<Constant<Self::Model>>,
        >,
    ) -> Result<Satisfiability, ExceedLimitError>;

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
    fn as_trait_member(&self) -> Option<&Self::TraitMember>;

    #[doc(hidden)]
    fn as_trait_member_mut(&mut self) -> Option<&mut Self::TraitMember>;

    #[doc(hidden)]
    fn into_trait_member(self) -> Result<Self::TraitMember, Self>;

    #[doc(hidden)]
    fn as_tuple(&self) -> Option<&Tuple<Self>>;

    #[doc(hidden)]
    fn as_tuple_mut(&mut self) -> Option<&mut Tuple<Self>>;

    #[doc(hidden)]
    fn into_tuple(self) -> Result<Tuple<Self>, Self>;

    #[doc(hidden)]
    fn as_inference(&self) -> Option<&Self::InferenceVariable>;

    #[doc(hidden)]
    fn as_inference_mut(&mut self) -> Option<&mut Self::InferenceVariable>;

    #[doc(hidden)]
    fn into_inference(self) -> Result<Self::InferenceVariable, Self>;

    #[doc(hidden)]
    fn get_adt_fields(&self, table: &Table<impl State>) -> Option<Vec<Self>>;

    #[doc(hidden)]
    fn as_outlive_predicate(
        predicate: &Predicate<Self::Model>,
    ) -> Option<&Outlives<Self>>;

    #[doc(hidden)]
    fn as_outlive_predicate_mut(
        predicate: &mut Predicate<Self::Model>,
    ) -> Option<&mut Outlives<Self>>;

    #[doc(hidden)]
    fn into_outlive_predicate(
        predicate: Predicate<Self::Model>,
    ) -> Result<Outlives<Self>, Predicate<Self::Model>>;

    #[doc(hidden)]
    fn as_constant_type_predicate(
        predicate: &Predicate<Self::Model>,
    ) -> Option<&Self>;

    #[doc(hidden)]
    fn as_constant_type_predicate_mut(
        predicate: &mut Predicate<Self::Model>,
    ) -> Option<&mut Self>;

    #[doc(hidden)]
    fn into_constant_type_predicate(
        predicate: Predicate<Self::Model>,
    ) -> Result<Self, Predicate<Self::Model>>;

    #[doc(hidden)]
    fn as_trait_member_equality_predicate(
        predicate: &Predicate<Self::Model>,
    ) -> Option<&predicate::Equality<Self::TraitMember, Self>>;

    #[doc(hidden)]
    fn as_trait_member_equality_predicate_mut(
        predicate: &mut Predicate<Self::Model>,
    ) -> Option<&mut predicate::Equality<Self::TraitMember, Self>>;

    #[doc(hidden)]
    fn into_trait_member_equality_predicate(
        predicate: Predicate<Self::Model>,
    ) -> Result<
        predicate::Equality<Self::TraitMember, Self>,
        Predicate<Self::Model>,
    >;

    #[doc(hidden)]
    fn as_tuple_predicate(
        predicate: &Predicate<Self::Model>,
    ) -> Option<&predicate::Tuple<Self>>;

    #[doc(hidden)]
    fn as_tuple_predicate_mut(
        predicate: &mut Predicate<Self::Model>,
    ) -> Option<&mut predicate::Tuple<Self>>;

    #[doc(hidden)]
    fn into_tuple_predicate(
        predicate: Predicate<Self::Model>,
    ) -> Result<predicate::Tuple<Self>, Predicate<Self::Model>>;

    #[doc(hidden)]
    fn definite_satisfiability(&self) -> Satisfiability;

    #[doc(hidden)]
    fn constant_type_satisfiability(&self) -> Satisfiability;

    #[doc(hidden)]
    fn get_instantiation(
        instantiation: &Instantiation<Self::Model>,
    ) -> &HashMap<Self, Self>;

    #[doc(hidden)]
    fn get_instantiation_mut(
        instantiation: &mut Instantiation<Self::Model>,
    ) -> &mut HashMap<Self, Self>;

    #[doc(hidden)]
    fn get_substructural_unification<'a, T: Term>(
        substructural: &'a unification::Substructural<T>,
    ) -> impl Iterator<Item = &'a Unification<Self::Rebind<T::Model>>>
    where
        Self: 'a;

    #[doc(hidden)]
    fn get_mapping(
        mapping: &Mapping<Self::Model>,
    ) -> &HashMap<Self, HashSet<Self>>;

    #[doc(hidden)]
    fn get_mapping_mut(
        mapping: &mut Mapping<Self::Model>,
    ) -> &mut HashMap<Self, HashSet<Self>>;

    #[doc(hidden)]
    fn get_generic_arguments(
        generic_arguments: &GenericArguments<Self::Model>,
    ) -> &[Self];

    #[doc(hidden)]
    fn get_generic_arguments_mut(
        generic_arguments: &mut GenericArguments<Self::Model>,
    ) -> &mut Vec<Self>;

    #[doc(hidden)]
    fn from_default_model(term: Self::Rebind<Default>) -> Self;
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
            T::Model,
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
                T::Model,
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
            T::Model,
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

impl<M: Model> GenericArguments<M> {
    /// Determines if two generic arguments are equal.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    pub fn equals(
        &self,
        other: &Self,
        environment: &Environment<M, impl State, impl Normalizer<M>>,
        limit: &mut Limit<
            impl Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
        >,
    ) -> Result<bool, ExceedLimitError> {
        if self.lifetimes.len() != other.lifetimes.len()
            || self.types.len() != other.types.len()
            || self.constants.len() != other.constants.len()
        {
            return Ok(false);
        }

        for (lhs, rhs) in self.lifetimes.iter().zip(&other.lifetimes) {
            if !equality::equals(lhs, rhs, environment, limit)? {
                return Ok(false);
            }
        }

        for (lhs, rhs) in self.types.iter().zip(&other.types) {
            if !equality::equals(lhs, rhs, environment, limit)? {
                return Ok(false);
            }
        }

        for (lhs, rhs) in self.constants.iter().zip(&other.constants) {
            if !equality::equals(lhs, rhs, environment, limit)? {
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
    pub fn unify_as_mapping(
        &self,
        other: &Self,
        config: &mut impl unification::Config<M>,
        environment: &Environment<M, impl State, impl Normalizer<M>>,
        limit: &mut Limit<
            impl Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
        >,
    ) -> Result<Option<Mapping<M>>, ExceedLimitError> {
        let mut mapping = Mapping::default();

        if self.lifetimes.len() != other.lifetimes.len()
            || self.types.len() != other.types.len()
            || self.constants.len() != other.constants.len()
        {
            return Ok(None);
        }

        for (lhs, rhs) in self.lifetimes.iter().zip(&other.lifetimes) {
            let Some(unification) =
                unification::unify(lhs, rhs, config, environment, limit)?
            else {
                return Ok(None);
            };

            mapping.append_from_unification(unification);
        }

        for (lhs, rhs) in self.types.iter().zip(&other.types) {
            let Some(unification) =
                unification::unify(lhs, rhs, config, environment, limit)?
            else {
                return Ok(None);
            };

            mapping.append_from_unification(unification);
        }

        for (lhs, rhs) in self.constants.iter().zip(&other.constants) {
            let Some(unification) =
                unification::unify(lhs, rhs, config, environment, limit)?
            else {
                return Ok(None);
            };

            mapping.append_from_unification(unification);
        }

        Ok(Some(mapping))
    }

    /// Applies the instantiation to all the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation<M>) {
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
    pub fn definite(
        &self,
        environment: &Environment<M, impl State, impl Normalizer<M>>,
        limit: &mut Limit<
            impl Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
        >,
    ) -> Result<bool, ExceedLimitError> {
        for lifetime in &self.lifetimes {
            if !predicate::definite(lifetime, environment, limit)? {
                return Ok(false);
            }
        }

        for r#type in &self.types {
            if !predicate::definite(r#type, environment, limit)? {
                return Ok(false);
            }
        }

        for constant in &self.constants {
            if !predicate::definite(constant, environment, limit)? {
                return Ok(false);
            }
        }

        Ok(true)
    }

    fn substructural_match<L, T, C, Y>(
        &self,
        other: &Self,
        mut existing: matching::Substructural<M, L, T, C>,
        to_location: impl Fn(usize) -> Y,
    ) -> Option<matching::Substructural<M, L, T, C>>
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
            .cloned()
            .zip(other.lifetimes.iter().cloned())
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

/// An enumeration of references to all three kinds of terms: [`Lifetime`],
/// [`Type`], and [`Constant`].
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
#[allow(missing_docs)]
pub enum Kind<'a, M: Model> {
    Lifetime(&'a Lifetime<M>),
    Type(&'a Type<M>),
    Constant(&'a Constant<M>),
}

/// An enumeration of mutable references to all three kinds of terms:
/// [`Lifetime`], [`Type`], and [`Constant`].
#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From,
)]
#[allow(missing_docs)]
pub enum KindMut<'a, M: Model> {
    Lifetime(&'a mut Lifetime<M>),
    Type(&'a mut Type<M>),
    Constant(&'a mut Constant<M>),
}

#[cfg(test)]
mod tests;
