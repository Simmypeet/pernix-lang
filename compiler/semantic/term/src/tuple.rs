//! Contains the definition of a [`Tuple`] term.

use std::fmt::Write;

use enum_as_inner::EnumAsInner;
use qbice::{Decode, Encode, StableHash};

use crate::{
    matching::{Match, Matching, Substructural},
    sub_term::SubTerm,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

/// Represents a single element of a tuple.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    derive_new::new,
)]
pub struct Element<Term> {
    /// The term stored in this element.
    pub term: Term,

    /// Whether the term is unpacked.
    pub is_unpacked: bool,
}

impl<Term> Element<Term> {
    /// Creates a new regular (not unpacked) element.
    #[must_use]
    pub const fn new_regular(term: Term) -> Self {
        Self { term, is_unpacked: false }
    }
}

/// Represents a tuple of terms.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Tuple<Term> {
    /// The elements of the tuple.
    pub elements: Vec<Element<Term>>,
}

impl<Term> Default for Tuple<Term> {
    fn default() -> Self { Self { elements: Vec::new() } }
}

impl<T: crate::display::Display> crate::display::Display for Element<T> {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        if self.is_unpacked {
            formatter.write_str("...")?;
        }

        self.term.fmt(engine, formatter).await
    }
}

impl<T: crate::display::Display> crate::display::Display for Tuple<T> {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        formatter.write_str("(")?;

        for (i, element) in self.elements.iter().enumerate() {
            element.fmt(engine, formatter).await?;

            if i < self.elements.len() - 1 {
                formatter.write_str(", ")?;
            }
        }

        formatter.write_str(")")?;
        Ok(())
    }
}

/// Represents a sub-tuple location in a range form.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleRange {
    /// The begin index of the range.
    pub begin: usize,

    /// The end index of the range.
    pub end: usize,
}

impl TupleRange {
    /// Returns the length of the range.
    #[must_use]
    pub const fn len(&self) -> usize { self.end - self.begin }

    /// Returns whether the range is empty.
    #[must_use]
    pub const fn is_empty(&self) -> bool { self.len() == 0 }

    /// Converts the range to a standard library range.
    #[must_use]
    pub const fn to_std_range(&self) -> std::ops::Range<usize> {
        self.begin..self.end
    }
}

impl From<std::ops::Range<usize>> for TupleRange {
    fn from(range: std::ops::Range<usize>) -> Self {
        Self { begin: range.start, end: range.end }
    }
}

impl From<TupleRange> for std::ops::Range<usize> {
    fn from(val: TupleRange) -> Self { val.begin..val.end }
}

/// Represents a sub-term location where the sub-term is stored as an element of
/// a tuple.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum SubTupleLocation {
    /// The sub-term is stored as a regular element of the tuple.
    Single(usize),

    /// The sub-term ranges into multiple elements of the tuple.
    Range(TupleRange),
}

impl<T> Tuple<T>
where
    Self: TryFrom<T, Error = T> + Into<T>,
{
    /// Assigns the `sub_term` to the given `term` at this location.
    ///
    /// # Errors
    ///
    /// See [`AssignSubTermError`] for more information.
    pub fn assign_sub_term(&mut self, location: SubTupleLocation, sub_term: T) {
        match location {
            SubTupleLocation::Single(idx) => {
                let element =
                    self.elements.get_mut(idx).map(|x| &mut x.term).unwrap();

                *element = sub_term;
            }
            SubTupleLocation::Range(range) => {
                let Ok(sub_constant_tuple) = Self::try_from(sub_term) else {
                    panic!("tuple expected");
                };

                let tuple_elements =
                    self.elements.get_mut(range.begin..range.end).unwrap();

                assert!(
                    sub_constant_tuple.elements.len() == tuple_elements.len(),
                    "tuple length mismatch: expected {}, got {}",
                    tuple_elements.len(),
                    sub_constant_tuple.elements.len()
                );

                for (lhs, rhs) in
                    tuple_elements.iter_mut().zip(sub_constant_tuple.elements)
                {
                    *lhs = rhs;
                }
            }
        }
    }
}

impl<T: SubTerm + Match + Clone> Tuple<T>
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
            T::SubLifetimeLocation,
            T::SubTypeLocation,
            T::SubConstantLocation,
        >,
    >
    where
        T::ThisSubTermLocation: From<SubTupleLocation>,
    {
        fn push<T: SubTerm + Match>(
            lhs: T,
            rhs: T,
            lhs_location: SubTupleLocation,
            rhs_location: SubTupleLocation,
            existing: &mut Substructural<
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
            SubTupleLocation::Range(to_unpack_range.into()),
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
