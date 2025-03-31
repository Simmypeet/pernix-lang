//! Contains logic related to matching the term structurally.

use super::sub_term::SubTerm;
use crate::term::{
    constant::Constant, generic_arguments::GenericArguments,
    lifetime::Lifetime, r#type::Type, sub_term::SubTupleLocation, Model,
    ModelOf, Tuple,
};

/// Represents a match between two terms.
pub trait Match: Sized + SubTerm {
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
    #[allow(clippy::type_complexity)]
    fn substructural_match(
        &self,
        other: &Self,
    ) -> Option<
        Substructural<
            Self::Model,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    >;

    #[doc(hidden)]
    fn get_substructural(
        substructural: &Substructural<
            Self::Model,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<Matching<Self, Self::ThisSubTermLocation>>;

    #[doc(hidden)]
    fn get_substructural_mut(
        substructural: &mut Substructural<
            Self::Model,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<Matching<Self, Self::ThisSubTermLocation>>;
}

/// Represents a match between two terms.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Matching<T, Location> {
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Substructural<
    M: Model,
    SubLifetimeLocation,
    SubTypeLocation,
    SubConstantLocation,
> {
    pub lifetimes: Vec<Matching<Lifetime<M>, SubLifetimeLocation>>,
    pub types: Vec<Matching<Type<M>, SubTypeLocation>>,
    pub constants: Vec<Matching<Constant<M>, SubConstantLocation>>,
}

impl<M: Model, L, T, C> Default for Substructural<M, L, T, C> {
    fn default() -> Self {
        Self { lifetimes: Vec::new(), types: Vec::new(), constants: Vec::new() }
    }
}

impl<T: ModelOf + SubTerm + Match + Clone> Tuple<T>
where
    Self: TryFrom<T, Error = T> + Into<T>,
{
    #[allow(clippy::too_many_lines, clippy::type_complexity)]
    fn substructural_match_internal<'a>(
        from: &'a Self,
        to: &'a Self,
        swap: bool,
    ) -> Option<
        Substructural<
            T::Model,
            T::SubLifetimeLocation,
            T::SubTypeLocation,
            T::SubConstantLocation,
        >,
    >
    where
        T::ThisSubTermLocation: From<SubTupleLocation>,
    {
        fn push<T: ModelOf + SubTerm + Match>(
            lhs: T,
            rhs: T,
            lhs_location: SubTupleLocation,
            rhs_location: SubTupleLocation,
            existing: &mut Substructural<
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
                T::get_substructural_mut(existing).push(Matching {
                    lhs: rhs,
                    rhs: lhs,
                    lhs_location: rhs_location.into(),
                    rhs_location: lhs_location.into(),
                });
            } else {
                T::get_substructural_mut(existing).push(Matching {
                    lhs,
                    rhs,
                    lhs_location: lhs_location.into(),
                    rhs_location: rhs_location.into(),
                });
            }
        }

        if from.elements.len() == to.elements.len() {
            let mut error = false;
            let mut existing = Substructural::default();

            for (idx, (from_element, to_element)) in
                from.elements.iter().zip(&to.elements).enumerate()
            {
                if from_element.is_unpacked != to_element.is_unpacked {
                    error = true;
                    break;
                }

                let from_element = &from_element.term;
                let to_element = &to_element.term;

                push::<T>(
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

                let mut existing = Substructural::default();

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

        let mut existing = Substructural::default();

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

    /// Substructurally matches `self` to `to`.
    #[allow(clippy::type_complexity)]
    #[must_use]
    pub fn substructural_match<'a>(
        &'a self,
        to: &'a Self,
    ) -> Option<
        Substructural<
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
    /// Substructurally matches `self` to `to`.
    pub fn substructural_match<L, T, C, Y>(
        &self,
        other: &Self,
        mut existing: Substructural<M, L, T, C>,
        to_location: impl Fn(usize) -> Y,
    ) -> Option<Substructural<M, L, T, C>>
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
            existing.lifetimes.push(Matching {
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
            existing.types.push(Matching {
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
            existing.constants.push(Matching {
                lhs,
                rhs,
                lhs_location: location.into(),
                rhs_location: location.into(),
            });
        }

        Some(existing)
    }
}
