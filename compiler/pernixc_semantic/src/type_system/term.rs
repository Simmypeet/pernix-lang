//! Contains the three fundamental terms of the language: [`Type`],
//! [`Constant`], and [`Lifetime`].

use core::fmt;
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Debug,
    hash::Hash,
    sync::Arc,
};

use constant::Constant;
use enum_as_inner::EnumAsInner;
use lifetime::Lifetime;
use r#type::Type;

use super::{
    compatible,
    definite::Definite,
    equality::Equality,
    equivalence,
    instantiation::{self, Instantiation},
    mapping::Mapping,
    matching,
    model::Model,
    normalizer::Normalizer,
    observer::Observer,
    predicate::{self, Outlives, Predicate, Satisfiability},
    query::{self, Context},
    simplify,
    sub_term::{self, AssignSubTermError, SubTupleLocation},
    unification::{self, PredicateA, Unification, Unifier},
    visitor, Compute, Environment, Output, Satisfied, Succeeded,
};
use crate::{
    arena::ID,
    symbol::{
        table::{self, DisplayObject, State, Table},
        GenericID, GenericParameter, GlobalID, MemberID,
    },
    type_system::model::Default,
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

impl<T: State, M: Model> table::Display<T> for GenericArguments<M>
where
    Lifetime<M>: table::Display<T>,
    Type<M>: table::Display<T>,
    Constant<M>: table::Display<T>,
{
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
    /// Converts a generic arguments with the model `U` into the model `M`.
    pub fn from_other_model<U: Model>(term: GenericArguments<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        Self {
            lifetimes: term
                .lifetimes
                .into_iter()
                .map(Lifetime::from_other_model)
                .collect(),
            types: term.types.into_iter().map(Type::from_other_model).collect(),
            constants: term
                .constants
                .into_iter()
                .map(Constant::from_other_model)
                .collect(),
        }
    }

    /// Checks if there's any errornous term in the generic arguments.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.lifetimes.iter().any(Lifetime::is_error)
            || self.types.iter().any(Type::is_error)
            || self.constants.iter().any(Constant::is_error)
    }

    /// Tries to convert a generic arguments with the model `U` into the model
    /// `M`.
    ///
    /// # Errors
    ///
    /// Returns an error returned by the `TryFrom` implementation of the model.
    pub fn try_from_other_model<U: Model, E>(
        term: GenericArguments<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self {
            lifetimes: term
                .lifetimes
                .into_iter()
                .map(Lifetime::try_from_other_model)
                .collect::<Result<Vec<_>, _>>()?,
            types: term
                .types
                .into_iter()
                .map(Type::try_from_other_model)
                .collect::<Result<Vec<_>, _>>()?,
            constants: term
                .constants
                .into_iter()
                .map(Constant::try_from_other_model)
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    /// Converts the generic arguments with the default model into the model `M`
    #[must_use]
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

impl<M: Model, ID> Symbol<M, ID> {
    /// Converts a symbol from model `U` to model `M`.
    pub fn from_other_model<U: Model>(symbol: Symbol<U, ID>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        Self {
            id: symbol.id,
            generic_arguments: GenericArguments::from_other_model(
                symbol.generic_arguments,
            ),
        }
    }

    /// Tries to convert a symbol from model `U` to model `M`.
    ///
    /// # Errors
    ///
    /// Returns an error returned by the `TryFrom` implementation of the model.
    pub fn try_from_other_model<U: Model, E>(
        symbol: Symbol<U, ID>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self {
            id: symbol.id,
            generic_arguments: GenericArguments::try_from_other_model(
                symbol.generic_arguments,
            )?,
        })
    }
}

impl<T: State, ID: Into<GlobalID> + Copy, M: Model> table::Display<T>
    for Symbol<M, ID>
where
    GenericArguments<M>: table::Display<T>,
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
            display: &self.generic_arguments
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

impl<M: Model, ID> MemberSymbol<M, ID> {
    /// Converts a member symbol from model `U` to model `M`.
    pub fn from_other_model<U: Model>(
        member_symbol: MemberSymbol<U, ID>,
    ) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        Self {
            id: member_symbol.id,
            member_generic_arguments: GenericArguments::from_other_model(
                member_symbol.member_generic_arguments,
            ),
            parent_generic_arguments: GenericArguments::from_other_model(
                member_symbol.parent_generic_arguments,
            ),
        }
    }

    /// Tries to convert a member symbol from model `U` to model `M`.
    ///
    /// # Errors
    ///
    /// Returns an error returned by the `TryFrom` implementation of the model.
    pub fn try_from_other_model<U: Model, E>(
        member_symbol: MemberSymbol<U, ID>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self {
            id: member_symbol.id,
            member_generic_arguments: GenericArguments::try_from_other_model(
                member_symbol.member_generic_arguments,
            )?,
            parent_generic_arguments: GenericArguments::try_from_other_model(
                member_symbol.parent_generic_arguments,
            )?,
        })
    }
}

impl<T: State, ID: Copy + Into<GlobalID>, M: Model> table::Display<T>
    for MemberSymbol<M, ID>
where
    GenericArguments<M>: table::Display<T>,
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
            display: &self.parent_generic_arguments
        })?;

        write!(f, "::{}{}", this_sym.name(), DisplayObject {
            table,
            display: &self.member_generic_arguments
        })
    }
}

/// Represents a single element of a tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleElement<Term> {
    /// The term stored in this element.
    pub term: Term,

    /// Whether the term is unpacked.
    pub is_unpacked: bool,
}

/// Represents a tuple of terms.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Tuple<Term: Clone> {
    /// The elements of the tuple.
    pub elements: Vec<TupleElement<Term>>,
}

impl<T: Term> Tuple<T> {
    /// Converts a tuple from model `U` to model `M`.
    #[must_use]
    pub fn from_other_model<U: Model>(tuple: Tuple<T::Rebind<U>>) -> Self
    where
        <T::Model as Model>::LifetimeInference: From<U::LifetimeInference>,
        <T::Model as Model>::TypeInference: From<U::TypeInference>,
        <T::Model as Model>::ConstantInference: From<U::ConstantInference>,
    {
        Self {
            elements: tuple
                .elements
                .into_iter()
                .map(|x| TupleElement {
                    term: T::from_other_model(x.term),
                    is_unpacked: x.is_unpacked,
                })
                .collect(),
        }
    }

    /// Tries to convert a tuple from model `U` to model `M`.
    ///
    /// # Errors
    ///
    /// Returns an error returned by the `TryFrom` implementation of the model.
    pub fn try_from_other_model<U: Model, E>(
        tuple: Tuple<T::Rebind<U>>,
    ) -> Result<Self, E>
    where
        <T::Model as Model>::LifetimeInference:
            TryFrom<U::LifetimeInference, Error = E>,
        <T::Model as Model>::TypeInference:
            TryFrom<U::TypeInference, Error = E>,
        <T::Model as Model>::ConstantInference:
            TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self {
            elements: tuple
                .elements
                .into_iter()
                .map(|x| {
                    T::try_from_other_model(x.term).map(|term| TupleElement {
                        term,
                        is_unpacked: x.is_unpacked,
                    })
                })
                .collect::<Result<_, _>>()?,
        })
    }
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
            let is_unpacked = element.is_unpacked;

            if is_unpacked {
                write!(f, "...")?;
            }

            write!(f, "{}", DisplayObject { table, display: &element.term })?;

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

impl<T: State> table::Display<T> for Never {
    #[allow(clippy::uninhabited_references)]
    fn fmt(
        &self,
        _: &Table<T>,
        _: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match *self {}
    }
}

/// The trait used for specifying the model of a term.
pub trait ModelOf {
    /// In which model does the term belong to.
    type Model: Model;
}

/// The term under the `local` modifier.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Local<T: Term>(pub Box<T>)
where
    Self: Into<T>;

impl<T: Term> Local<T>
where
    Self: Into<T>,
{
    /// Converts a local term from model `U` to model `M`.
    #[must_use]
    pub fn from_other_model<U: Model>(local: Local<T::Rebind<U>>) -> Self
    where
        Local<T::Rebind<U>>: Into<T::Rebind<U>>,

        <T::Model as Model>::LifetimeInference: From<U::LifetimeInference>,
        <T::Model as Model>::TypeInference: From<U::TypeInference>,
        <T::Model as Model>::ConstantInference: From<U::ConstantInference>,
    {
        Self(Box::new(T::from_other_model(*local.0)))
    }

    /// Tries to convert a local term from model `U` to model `M`.
    ///
    /// # Errors
    ///
    /// Returns an error returned by the `TryFrom` implementation of the model.
    pub fn try_from_other_model<U: Model, E>(
        local: Local<T::Rebind<U>>,
    ) -> Result<Self, E>
    where
        Local<T::Rebind<U>>: Into<T::Rebind<U>>,

        <T::Model as Model>::LifetimeInference:
            TryFrom<U::LifetimeInference, Error = E>,
        <T::Model as Model>::TypeInference:
            TryFrom<U::TypeInference, Error = E>,
        <T::Model as Model>::ConstantInference:
            TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self(Box::new(T::try_from_other_model(*local.0)?)))
    }
}

/// A trait implemented by all three fundamental terms of the language:
/// [`Lifetime`], [`Type`], and [`Constant`].
///
/// This trait provides a common interface for all terms to be used in the
/// type system. Since most of the queries and operations in the type system are
/// generic over the kind of term, this trait allows for a common interface to
/// be used for all terms.
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
    + query::Element
    + matching::Match
    + sub_term::SubTerm
    + simplify::Simplify
    + equivalence::Equivalence
    + compatible::Compatible
    + From<MemberID<ID<Self::GenericParameter>, GenericID>>
    + From<Error>
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
        + Sync;

    /// Changes the model of the term to another model.
    type Rebind<M: Model>: Term<Model = M>;

    /// Converts a term from another model to this model.
    fn from_other_model<U: Model>(term: Self::Rebind<U>) -> Self
    where
        <Self::Model as Model>::LifetimeInference: From<U::LifetimeInference>,
        <Self::Model as Model>::TypeInference: From<U::TypeInference>,
        <Self::Model as Model>::ConstantInference: From<U::ConstantInference>;

    /// Tries to convert a term from another model to this model.
    ///
    /// # Errors
    ///
    /// Returns an error returned by the `TryFrom` implementation of the model.
    fn try_from_other_model<U: Model, E>(
        term: Self::Rebind<U>,
    ) -> Result<Self, E>
    where
        <Self::Model as Model>::LifetimeInference:
            TryFrom<U::LifetimeInference, Error = E>,
        <Self::Model as Model>::TypeInference:
            TryFrom<U::TypeInference, Error = E>,
        <Self::Model as Model>::ConstantInference:
            TryFrom<U::ConstantInference, Error = E>;

    /// An algorithm that normalizes the term.
    ///
    /// Normalization converts the term into a canonical form. For example,
    /// a type alias is expanded into its definition.
    ///
    /// ```pnx
    /// public type A = int32
    /// ```
    ///
    /// The type `A` is normalized into `int32`.
    #[doc(hidden)]
    fn normalize<S: State>(
        &self,
        environment: &Environment<
            Self::Model,
            S,
            impl Normalizer<Self::Model, S>,
            impl Observer<Self::Model, S>,
        >,
        context: &mut Context<Self::Model>,
    ) -> Result<Output<Self, Self::Model>, super::OverflowError>;

    #[doc(hidden)]
    fn as_kind(&self) -> Kind<Self::Model>;

    #[doc(hidden)]
    fn as_kind_mut(&mut self) -> KindMut<Self::Model>;

    #[doc(hidden)]
    fn try_from_kind(
        kind: Kind<Self::Model>,
    ) -> Result<&Self, Kind<Self::Model>>;

    #[doc(hidden)]
    fn try_from_kind_mut(
        kind: KindMut<Self::Model>,
    ) -> Result<&mut Self, KindMut<Self::Model>>;

    #[doc(hidden)]
    fn outlives_satisfiability(
        &self,
        lifetime: &Lifetime<Self::Model>,
    ) -> Satisfiability;

    /// Converts the inference variable of this kind of term into the term.
    #[doc(hidden)]
    fn from_inference(inference: Self::InferenceVariable) -> Self;

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
    fn as_trait_member_equality_predicate(
        predicate: &Predicate<Self::Model>,
    ) -> Option<&Equality<Self::TraitMember, Self>>;

    #[doc(hidden)]
    fn as_trait_member_equality_predicate_mut(
        predicate: &mut Predicate<Self::Model>,
    ) -> Option<&mut Equality<Self::TraitMember, Self>>;

    #[doc(hidden)]
    fn into_trait_member_equality_predicate(
        predicate: Predicate<Self::Model>,
    ) -> Result<Equality<Self::TraitMember, Self>, Predicate<Self::Model>>;

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
    fn get_instantiation(
        instantiation: &Instantiation<Self::Model>,
    ) -> &BTreeMap<Self, Self>;

    #[doc(hidden)]
    fn get_instantiation_mut(
        instantiation: &mut Instantiation<Self::Model>,
    ) -> &mut BTreeMap<Self, Self>;

    #[doc(hidden)]
    fn get_substructural_unifier<'a, T: Term>(
        substructural: &'a unification::Substructural<T>,
    ) -> impl Iterator<Item = &'a Unifier<Self::Rebind<T::Model>>>
    where
        Self: 'a;

    #[doc(hidden)]
    fn get_mapping(
        mapping: &Mapping<Self::Model>,
    ) -> &BTreeMap<Self, BTreeSet<Self>>;

    #[doc(hidden)]
    fn get_mapping_mut(
        mapping: &mut Mapping<Self::Model>,
    ) -> &mut BTreeMap<Self, BTreeSet<Self>>;

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
    #[allow(clippy::too_many_lines, clippy::type_complexity)]
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
                if from_element.is_unpacked != to_element.is_unpacked {
                    error = true;
                    break;
                }

                let from_element = &from_element.term;
                let to_element = &to_element.term;

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
            from.elements.iter().filter(|x| x.is_unpacked).count(),
            to.elements.iter().filter(|x| x.is_unpacked).count(),
        ) {
            (0, 0) => {
                if from.elements.len() != to.elements.len() {
                    return None;
                }

                let mut existing = matching::Substructural::default();

                for (idx, (from_element, to_element)) in
                    from.elements.iter().zip(&to.elements).enumerate()
                {
                    let from_element = &from_element.term;
                    let to_element = &to_element.term;

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
            from.elements.iter().position(|x| x.is_unpacked).unwrap();

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
            let from_element = &from_element.term;

            if to_element.is_unpacked {
                return None;
            }

            push(
                from_element.clone(),
                to_element.term.clone(),
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
            let from_element = &from_element.term;

            if to_element.is_unpacked {
                return None;
            }

            push(
                from_element.clone(),
                to_element.term.clone(),
                SubTupleLocation::Single(idx + from_tail_range.start),
                SubTupleLocation::Single(idx + to_tail_range.start),
                &mut existing,
                swap,
            );
        }

        let to_unpack =
            Self { elements: to.elements[to_unpack_range.clone()].to_vec() }
                .into();

        let unpacked = &from.elements[unpacked_position].term;

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

    #[allow(clippy::type_complexity)]
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
    /// See [`super::Error`] for more information.
    pub fn equals<S: State>(
        &self,
        other: &Self,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<Output<Satisfied, M>, super::OverflowError> {
        self.equals_with_context(other, environment, &mut Context::new())
    }

    pub(super) fn equals_with_context<S: State>(
        &self,
        other: &Self,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
        context: &mut Context<M>,
    ) -> Result<Output<Satisfied, M>, super::OverflowError> {
        let mut constraints = BTreeSet::new();

        if self.lifetimes.len() != other.lifetimes.len()
            || self.types.len() != other.types.len()
            || self.constants.len() != other.constants.len()
        {
            return Ok(None);
        }

        for (lhs, rhs) in self.lifetimes.iter().zip(&other.lifetimes) {
            let Some(result) = Equality::new(lhs.clone(), rhs.clone())
                .query_with_context(environment, context)?
            else {
                return Ok(None);
            };

            constraints.extend(result.constraints);
        }

        for (lhs, rhs) in self.types.iter().zip(&other.types) {
            let Some(result) = Equality::new(lhs.clone(), rhs.clone())
                .query_with_context(environment, context)?
            else {
                return Ok(None);
            };

            constraints.extend(result.constraints);
        }

        for (lhs, rhs) in self.constants.iter().zip(&other.constants) {
            let Some(result) = Equality::new(lhs.clone(), rhs.clone())
                .query_with_context(environment, context)?
            else {
                return Ok(None);
            };

            constraints.extend(result.constraints);
        }

        Ok(Some(Succeeded::satisfied_with(constraints)))
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
    /// See [`super::Error`] for more information.
    pub fn unify_as_mapping<S: State>(
        &self,
        other: &Self,
        config: &Arc<dyn PredicateA<M>>,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<Output<Mapping<M>, M>, super::OverflowError> {
        self.unify_as_mapping_with_context(
            other,
            config,
            environment,
            &mut Context::new(),
        )
    }

    pub(super) fn unify_as_mapping_with_context<S: State>(
        &self,
        other: &Self,
        config: &Arc<dyn PredicateA<M>>,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
        context: &mut Context<M>,
    ) -> Result<Output<Mapping<M>, M>, super::OverflowError> {
        let mut constraints = BTreeSet::new();

        let mut mapping = Mapping::default();

        if self.lifetimes.len() != other.lifetimes.len()
            || self.types.len() != other.types.len()
            || self.constants.len() != other.constants.len()
        {
            return Ok(None);
        }

        for (lhs, rhs) in self.lifetimes.iter().zip(&other.lifetimes) {
            let Some(unification) =
                Unification::new(lhs.clone(), rhs.clone(), config.clone())
                    .query_with_context(environment, context)?
            else {
                return Ok(None);
            };

            constraints.extend(unification.constraints);
            mapping.append_from_unifier(unification.result);
        }

        for (lhs, rhs) in self.types.iter().zip(&other.types) {
            let Some(unification) =
                Unification::new(lhs.clone(), rhs.clone(), config.clone())
                    .query_with_context(environment, context)?
            else {
                return Ok(None);
            };

            constraints.extend(unification.constraints);
            mapping.append_from_unifier(unification.result);
        }

        for (lhs, rhs) in self.constants.iter().zip(&other.constants) {
            let Some(unification) =
                Unification::new(lhs.clone(), rhs.clone(), config.clone())
                    .query_with_context(environment, context)?
            else {
                return Ok(None);
            };

            constraints.extend(unification.constraints);
            mapping.append_from_unifier(unification.result);
        }

        Ok(Some(Succeeded::with_constraints(mapping, constraints)))
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
    /// See [`Definite`] for more information.
    ///
    /// # Errors
    ///
    /// See [`super::Error`] for more information.
    pub fn definite<S: State>(
        &self,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<Output<Satisfied, M>, super::OverflowError> {
        self.definite_with_context(environment, &mut Context::new())
    }

    /// Checks if all the generic arguments are definite.
    ///
    /// See [`Definite`] for more information.
    ///
    /// # Errors
    ///
    /// See [`super::Error`] for more information.
    pub fn definite_with_context<S: State>(
        &self,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
        context: &mut Context<M>,
    ) -> Result<Output<Satisfied, M>, super::OverflowError> {
        let mut constraints = BTreeSet::new();

        for lifetime in &self.lifetimes {
            let Some(result) = Definite::new(lifetime.clone())
                .query_with_context(environment, context)?
            else {
                return Ok(None);
            };

            constraints.extend(result.constraints);
        }

        for r#type in &self.types {
            let Some(result) = Definite::new(r#type.clone())
                .query_with_context(environment, context)?
            else {
                return Ok(None);
            };

            constraints.extend(result.constraints);
        }

        for constant in &self.constants {
            let Some(result) = Definite::new(constant.clone())
                .query_with_context(environment, context)?
            else {
                return Ok(None);
            };

            constraints.extend(result.constraints);
        }

        Ok(Some(Succeeded::satisfied_with(constraints)))
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

/// Represents an errornuos term. Used for representing errors in the type
/// system.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Error;

#[cfg(test)]
mod tests;
