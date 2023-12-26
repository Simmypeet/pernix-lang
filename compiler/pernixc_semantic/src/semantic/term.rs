//! Contains the three fundamental terms of the language: [`Type`], [`Constant`], and [`Lifetime`].

use constant::Constant;
use enum_as_inner::EnumAsInner;
use lifetime::Lifetime;
use r#type::Type;

use super::mapping::Map;

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

/// Contains the functionality for determining the properties of a term.
pub trait Term: Map + Sized + Clone {
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
    fn get_substructural(substructural: &Substructural) -> &Vec<(Self, Self)>;

    #[doc(hidden)]
    fn get_substructural_mut(substructural: &mut Substructural) -> &mut Vec<(Self, Self)>;
}

impl<T: Term> Tuple<T>
where
    Self: TryFrom<T, Error = T> + Into<T>,
{
    fn substructural_match<'a>(mut self: &'a Self, mut other: &'a Self) -> Option<Substructural> {
        fn push<T: Term>(lhs: T, rhs: T, existing: &mut Substructural, swap: bool) {
            if swap {
                T::get_substructural_mut(existing).push((rhs, lhs));
            } else {
                T::get_substructural_mut(existing).push((lhs, rhs));
            }
        }

        let lhs_unpacked_count = self.elements.iter().filter(|x| x.is_unpacked()).count();
        let rhs_unpacked_count = other.elements.iter().filter(|x| x.is_unpacked()).count();

        let swap = match (lhs_unpacked_count, rhs_unpacked_count) {
            (1, _) => false,
            (_, 1) => true,

            (lhs_length, rhs_length) => {
                let mut result = Substructural::default();
                if lhs_length != rhs_length {
                    return None;
                }

                for (lhs, rhs) in self.elements.iter().zip(other.elements.iter()) {
                    match (lhs, rhs) {
                        (TupleElement::Regular(lhs), TupleElement::Regular(rhs))
                        | (TupleElement::Unpacked(lhs), TupleElement::Unpacked(rhs)) => {
                            T::get_substructural_mut(&mut result).push((lhs.clone(), rhs.clone()));
                        }

                        _ => return None,
                    }
                }

                return Some(result);
            }
        };

        let mut existing = Substructural::default();
        if swap {
            std::mem::swap(&mut self, &mut other);
        }

        if self.elements.len() > other.elements.len() + 1 {
            return None;
        }

        let unpacked_position = self
            .elements
            .iter()
            .position(TupleElement::is_unpacked)
            .unwrap();

        let head_range = 0..unpacked_position;
        let self_tail_range = (unpacked_position + 1)..self.elements.len();
        let other_tail_range =
            (other.elements.len() - self_tail_range.clone().count())..other.elements.len();
        let other_unpack_range = unpacked_position..other_tail_range.start;

        // unify head
        for (self_element, other_element) in self.elements[head_range.clone()]
            .iter()
            .zip(&other.elements[head_range])
        {
            let self_element = self_element.as_regular().unwrap();
            let other_element = other_element.as_regular().unwrap();

            push(
                self_element.clone(),
                other_element.clone(),
                &mut existing,
                swap,
            );
        }

        // unify tail
        for (self_element, other_element) in self.elements[self_tail_range]
            .iter()
            .zip(&other.elements[other_tail_range])
        {
            let self_element = self_element.as_regular().unwrap();
            let other_element = other_element.as_regular().unwrap();

            push(
                self_element.clone(),
                other_element.clone(),
                &mut existing,
                swap,
            );
        }

        let other_unpack = Self {
            elements: other.elements[other_unpack_range].to_vec(),
        }
        .into();

        let unpacked = self.elements[unpacked_position].as_unpacked().unwrap();
        push(unpacked.clone(), other_unpack, &mut existing, swap);

        Some(existing)
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
