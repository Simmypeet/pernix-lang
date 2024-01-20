//! Contains the three fundamental terms of the language: [`Type`], [`Constant`], and [`Lifetime`].

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
    mapping::Map, predicate::Satisfiability, substitution::Substitute, unification::Unification,
    visitor::Element,
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

/// Represents a term where its value is stored as a symbol (i.e., `type` or `const` declaration).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol<ID> {
    /// The ID of the symbol that contains the value of the term.
    pub id: ID,

    /// The generic arguments supplied to the symbol.
    pub generic_arguments: GenericArguments,
}

/// Represents a term where its value is stored as a member of a particular symbol
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberSymbol<ID> {
    /// The ID of the symbol that contains the value of the term.
    pub id: ID,

    /// The generic arguments supplied to the member.
    pub member_generic_arguments: GenericArguments,

    /// The generic arguments supplied to the parent scope.
    pub parent_generic_arguments: GenericArguments,
}

/// Represents a single element of a tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum TupleElement<Term> {
    /// A regular term.
    Regular(Term),

    /// A term that can be unpacked into multiple terms.
    Unpacked(Term),
}

/// Represents a tuple of terms.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Tuple<Term: Clone>
where
    Self: TryFrom<Term, Error = Term> + Into<Term>,
{
    /// The elements of the tuple.
    pub elements: Vec<TupleElement<Term>>,
}

/// A type that can't never be instantiated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Never {}

/// Represents a substructural matching between two terms.
///
/// See [`Term::substructural_match`] for more information.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[allow(missing_docs)]
pub struct Substructural {
    pub lifetimes: Vec<(Lifetime, Lifetime)>,
    pub types: Vec<(Type, Type)>,
    pub constants: Vec<(Constant, Constant)>,
}

/// The term under the `local` modifier.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Local<T>(pub Box<T>)
where
    Self: Into<T>;

/// Contains the functionality for determining the properties of a term.
pub trait Term: Debug + Eq + Hash + Map + Sized + Clone + Ord + Element + Substitute {
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
    fn substructural_match(&self, other: &Self) -> Option<Substructural>;

    #[doc(hidden)]
    fn is_tuple(&self) -> bool;

    #[doc(hidden)]
    fn definite_satisfiability(&self) -> Satisfiability;

    #[doc(hidden)]
    fn get_substructural(substructural: &Substructural) -> &Vec<(Self, Self)>;

    #[doc(hidden)]
    fn get_substructural_mut(substructural: &mut Substructural) -> &mut Vec<(Self, Self)>;

    #[doc(hidden)]
    fn get_unification(unification: &Unification) -> &HashMap<Self, HashSet<Self>>;

    #[doc(hidden)]
    fn get_unification_mut(unification: &mut Unification) -> &mut HashMap<Self, HashSet<Self>>;
}

impl<T: Term> Tuple<T>
where
    Self: TryFrom<T, Error = T> + Into<T>,
{
    fn substructural_match_internal<'a>(
        from: &'a Self,
        to: &'a Self,
        swap: bool,
    ) -> Option<Substructural> {
        fn push<T: Term>(lhs: T, rhs: T, existing: &mut Substructural, swap: bool) {
            if swap {
                T::get_substructural_mut(existing).push((rhs, lhs));
            } else {
                T::get_substructural_mut(existing).push((lhs, rhs));
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

                let mut existing = Substructural::default();

                for (from_element, to_element) in from.elements.iter().zip(&to.elements) {
                    let from_element = from_element.as_regular().unwrap();
                    let to_element = to_element.as_regular().unwrap();

                    push(
                        from_element.clone(),
                        to_element.clone(),
                        &mut existing,
                        swap,
                    );
                }

                return Some(existing);
            }

            (1, _) => {}

            (_, _) => return None,
        }

        let mut existing = Substructural::default();

        if from.elements.len() > to.elements.len() + 1 {
            return None;
        }

        let unpacked_position = from
            .elements
            .iter()
            .position(TupleElement::is_unpacked)
            .unwrap();

        let head_range = 0..unpacked_position;
        let from_tail_range = (unpacked_position + 1)..from.elements.len();
        let to_tail_range =
            (to.elements.len() - from_tail_range.clone().count())..to.elements.len();
        let to_unpack_range = unpacked_position..to_tail_range.start;

        // unify head
        for (from_element, to_element) in from.elements[head_range.clone()]
            .iter()
            .zip(&to.elements[head_range])
        {
            let from_element = from_element.as_regular().unwrap();
            let TupleElement::Regular(to_element) = to_element else {
                return None;
            };

            push(
                from_element.clone(),
                to_element.clone(),
                &mut existing,
                swap,
            );
        }

        // unify tail
        for (from_element, to_element) in from.elements[from_tail_range]
            .iter()
            .zip(&to.elements[to_tail_range])
        {
            let from_element = from_element.as_regular().unwrap();
            let TupleElement::Regular(to_element) = to_element else {
                return None;
            };

            push(
                from_element.clone(),
                to_element.clone(),
                &mut existing,
                swap,
            );
        }

        let to_unpack = Self {
            elements: to.elements[to_unpack_range].to_vec(),
        }
        .into();

        let unpacked = from.elements[unpacked_position].as_unpacked().unwrap();
        push(unpacked.clone(), to_unpack, &mut existing, swap);

        Some(existing)
    }

    fn substructural_match<'a>(&'a self, to: &'a Self) -> Option<Substructural> {
        Self::substructural_match_internal(self, to, false)
            .or_else(|| Self::substructural_match_internal(to, self, true))
    }
}

impl GenericArguments {
    fn substructural_match(
        &self,
        other: &Self,
        mut existing: Substructural,
    ) -> Option<Substructural> {
        if self.lifetimes.len() != other.lifetimes.len()
            || self.types.len() != other.types.len()
            || self.constants.len() != other.constants.len()
        {
            return None;
        }

        for (lhs, rhs) in self.lifetimes.iter().zip(&other.lifetimes) {
            existing.lifetimes.push((*lhs, *rhs));
        }

        for (lhs, rhs) in self.types.iter().zip(&other.types) {
            existing.types.push((lhs.clone(), rhs.clone()));
        }

        for (lhs, rhs) in self.constants.iter().zip(&other.constants) {
            existing.constants.push((lhs.clone(), rhs.clone()));
        }

        Some(existing)
    }
}

#[cfg(test)]
mod tests;
