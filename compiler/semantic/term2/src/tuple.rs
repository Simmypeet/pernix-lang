//! Data definitions for tuple terms.

use derive_new::new;
use enum_as_inner::EnumAsInner;
use pernixc_qbice::TrackedEngine;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::matching::{Match, Matching, Substructural};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

/// Represents a single element of a tuple.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, StableHash, new,
)]
pub struct Element<Term> {
    term: Interned<Term>,
    is_unpacked: bool,
}

impl<Term: Encode + Identifiable + StableHash + Send + Sync + 'static> Encode
    for Element<Term>
{
    fn encode<E: qbice::serialize::Encoder + ?Sized>(
        &self,
        encoder: &mut E,
        plugin: &qbice::serialize::Plugin,
        session: &mut qbice::serialize::session::Session,
    ) -> std::io::Result<()> {
        self.term.encode(encoder, plugin, session)?;
        self.is_unpacked.encode(encoder, plugin, session)
    }
}

impl<Term: Decode + Identifiable + StableHash + Send + Sync + 'static> Decode
    for Element<Term>
{
    fn decode<D: qbice::serialize::Decoder + ?Sized>(
        decoder: &mut D,
        plugin: &qbice::serialize::Plugin,
        session: &mut qbice::serialize::session::Session,
    ) -> std::io::Result<Self> {
        let term = Interned::<Term>::decode(decoder, plugin, session)?;
        let is_unpacked = bool::decode(decoder, plugin, session)?;

        Ok(Self { term, is_unpacked })
    }
}

impl<Term> Element<Term> {
    /// Creates a new regular element.
    #[must_use]
    pub const fn new_regular(term: Interned<Term>) -> Self {
        Self { term, is_unpacked: false }
    }

    /// Creates a new unpacked element.
    #[must_use]
    pub const fn new_unpacked(term: Interned<Term>) -> Self {
        Self { term, is_unpacked: true }
    }

    /// Returns whether the element is unpacked.
    #[must_use]
    pub const fn is_unpacked(&self) -> bool { self.is_unpacked }

    /// Returns the element term.
    #[must_use]
    pub const fn term(&self) -> &Interned<Term> { &self.term }

    /// Returns a mutable reference to the element term.
    #[must_use]
    pub const fn term_mut(&mut self) -> &mut Interned<Term> { &mut self.term }

    /// Consumes the element and returns its term.
    #[must_use]
    pub fn into_term(self) -> Interned<Term> { self.term }
}

/// Represents a tuple of terms.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, StableHash, new,
)]
pub struct Tuple<Term> {
    elements: Vec<Element<Term>>,
}

impl<Term: Encode + Identifiable + StableHash + Send + Sync + 'static> Encode
    for Tuple<Term>
{
    fn encode<E: qbice::serialize::Encoder + ?Sized>(
        &self,
        encoder: &mut E,
        plugin: &qbice::serialize::Plugin,
        session: &mut qbice::serialize::session::Session,
    ) -> std::io::Result<()> {
        self.elements.encode(encoder, plugin, session)
    }
}

impl<Term: Decode + Identifiable + StableHash + Send + Sync + 'static> Decode
    for Tuple<Term>
{
    fn decode<D: qbice::serialize::Decoder + ?Sized>(
        decoder: &mut D,
        plugin: &qbice::serialize::Plugin,
        session: &mut qbice::serialize::session::Session,
    ) -> std::io::Result<Self> {
        let elements = Vec::<Element<Term>>::decode(decoder, plugin, session)?;

        Ok(Self { elements })
    }
}

impl<Term> Tuple<Term> {
    /// Creates the unit tuple.
    #[must_use]
    pub const fn unit() -> Self { Self { elements: Vec::new() } }

    /// Returns the tuple elements.
    #[must_use]
    pub fn elements(&self) -> &[Element<Term>] { &self.elements }

    /// Returns a mutable slice of tuple elements.
    #[must_use]
    pub fn elements_mut(&mut self) -> &mut [Element<Term>] {
        &mut self.elements
    }

    /// Returns the number of elements.
    #[must_use]
    pub const fn len(&self) -> usize { self.elements.len() }

    /// Returns whether the tuple is empty.
    #[must_use]
    pub const fn is_empty(&self) -> bool { self.elements.is_empty() }

    /// Removes the element at `idx`.
    pub fn remove_at(&mut self, idx: usize) -> Element<Term> {
        self.elements.remove(idx)
    }

    /// Pushes a new element.
    pub fn push(&mut self, element: Element<Term>) {
        self.elements.push(element);
    }

    /// Consumes the tuple into its elements.
    #[must_use]
    pub fn into_elements(self) -> Vec<Element<Term>> { self.elements }
}

impl<Term: Identifiable + StableHash + Send + Sync + 'static> Default
    for Tuple<Term>
{
    fn default() -> Self { Self::unit() }
}

impl<Term> Tuple<Term>
where
    Term: Match + Clone + Identifiable + StableHash + Send + Sync + 'static,
    Self: TryFrom<Term, Error = Term> + Into<Term>,
    Term::ThisSubTermLocation: From<SubTupleLocation>,
{
    #[allow(clippy::too_many_lines, clippy::type_complexity)]
    fn substructural_match_internal<'a>(
        from: &'a Self,
        to: &'a Self,
        swap: bool,
    ) -> Option<
        impl Iterator<
            Item = Substructural<
                Term::SubLifetimeLocation,
                Term::SubTypeLocation,
                Term::SubConstantLocation,
                Term::SubInstanceLocation,
            >,
        > + 'a,
    > {
        enum MatchPlan {
            Exact,
            Packed {
                unpacked_position: usize,
                from_tail_range: std::ops::Range<usize>,
                to_tail_range: std::ops::Range<usize>,
                to_unpack_range: std::ops::Range<usize>,
            },
        }

        fn into_substructural<Term>(
            lhs: Interned<Term>,
            rhs: Interned<Term>,
            lhs_location: SubTupleLocation,
            rhs_location: SubTupleLocation,
            swap: bool,
        ) -> crate::matching::Substructural<
            Term::SubLifetimeLocation,
            Term::SubTypeLocation,
            Term::SubConstantLocation,
            Term::SubInstanceLocation,
        >
        where
            Term: Match + Identifiable + StableHash + Send + Sync + 'static,
            Term::ThisSubTermLocation: From<SubTupleLocation>,
        {
            let matching = if swap {
                Matching::new(
                    rhs,
                    lhs,
                    rhs_location.into(),
                    lhs_location.into(),
                )
            } else {
                Matching::new(
                    lhs,
                    rhs,
                    lhs_location.into(),
                    rhs_location.into(),
                )
            };

            Term::from_self_matching(matching)
        }

        let match_plan = if from.elements.len() == to.elements.len()
            && from.elements.iter().zip(&to.elements).all(
                |(from_element, to_element)| {
                    from_element.is_unpacked == to_element.is_unpacked
                },
            ) {
            MatchPlan::Exact
        } else {
            let from_unpacked_count = from
                .elements
                .iter()
                .filter(|element| element.is_unpacked)
                .count();
            let to_unpacked_count = to
                .elements
                .iter()
                .filter(|element| element.is_unpacked)
                .count();

            if from_unpacked_count != 1 || to_unpacked_count > 1 {
                return None;
            }

            if from.elements.len() > to.elements.len() + 1 {
                return None;
            }

            let unpacked_position =
                from.elements.iter().position(|element| element.is_unpacked)?;

            let head_range = 0..unpacked_position;
            let from_tail_range = (unpacked_position + 1)..from.elements.len();
            let to_tail_range = (to.elements.len()
                - from_tail_range.clone().count())
                ..to.elements.len();
            let to_unpack_range = unpacked_position..to_tail_range.start;

            if to.elements[head_range].iter().any(|element| element.is_unpacked)
                || to.elements[to_tail_range.clone()]
                    .iter()
                    .any(|element| element.is_unpacked)
            {
                return None;
            }

            MatchPlan::Packed {
                unpacked_position,
                from_tail_range,
                to_tail_range,
                to_unpack_range,
            }
        };

        Some(pernixc_coroutine_iter::coroutine_iter!({
            match match_plan {
                MatchPlan::Exact => {
                    for (idx, (from_element, to_element)) in
                        from.elements.iter().zip(&to.elements).enumerate()
                    {
                        yield into_substructural::<Term>(
                            from_element.term.clone(),
                            to_element.term.clone(),
                            SubTupleLocation::Single(idx),
                            SubTupleLocation::Single(idx),
                            swap,
                        );
                    }
                }

                MatchPlan::Packed {
                    unpacked_position,
                    from_tail_range,
                    to_tail_range,
                    to_unpack_range,
                } => {
                    for (idx, (from_element, to_element)) in from.elements
                        [0..unpacked_position]
                        .iter()
                        .zip(&to.elements[0..unpacked_position])
                        .enumerate()
                    {
                        yield into_substructural::<Term>(
                            from_element.term.clone(),
                            to_element.term.clone(),
                            SubTupleLocation::Single(idx),
                            SubTupleLocation::Single(idx),
                            swap,
                        );
                    }

                    for (idx, (from_element, to_element)) in from.elements
                        [from_tail_range.clone()]
                    .iter()
                    .zip(&to.elements[to_tail_range.clone()])
                    .enumerate()
                    {
                        yield into_substructural::<Term>(
                            from_element.term.clone(),
                            to_element.term.clone(),
                            SubTupleLocation::Single(
                                idx + from_tail_range.start,
                            ),
                            SubTupleLocation::Single(idx + to_tail_range.start),
                            swap,
                        );
                    }

                    let to_unpack = Interned::new_duplicating(
                        Self::new(
                            to.elements[to_unpack_range.clone()].to_vec(),
                        )
                        .into(),
                    );
                    let unpacked =
                        from.elements[unpacked_position].term.clone();

                    yield into_substructural::<Term>(
                        unpacked,
                        to_unpack,
                        SubTupleLocation::Single(unpacked_position),
                        SubTupleLocation::Range(to_unpack_range.into()),
                        swap,
                    );
                }
            }
        }))
    }

    /// Streams the substructural matches between two tuples.
    #[allow(clippy::type_complexity)]
    pub(crate) fn substructural_match<'a>(
        &'a self,
        other: &'a Self,
    ) -> Option<
        impl Iterator<
            Item = Substructural<
                Term::SubLifetimeLocation,
                Term::SubTypeLocation,
                Term::SubConstantLocation,
                Term::SubInstanceLocation,
            >,
        > + 'a,
    > {
        Self::substructural_match_internal(self, other, false)
            .or_else(|| Self::substructural_match_internal(other, self, true))
    }
}

/// Represents a range inside a tuple.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct TupleRange {
    begin: usize,
    end: usize,
}

impl TupleRange {
    /// Returns the begin index.
    #[must_use]
    pub const fn begin(&self) -> usize { self.begin }

    /// Returns the end index.
    #[must_use]
    pub const fn end(&self) -> usize { self.end }

    /// Converts the range into a standard range.
    #[must_use]
    pub const fn to_std_range(&self) -> std::ops::Range<usize> {
        self.begin..self.end
    }
}

impl From<std::ops::Range<usize>> for TupleRange {
    fn from(value: std::ops::Range<usize>) -> Self {
        Self::new(value.start, value.end)
    }
}

/// Represents the location of a term inside a tuple.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum SubTupleLocation {
    /// A single tuple element.
    Single(usize),

    /// A slice of tuple elements.
    Range(TupleRange),
}

impl<T> Tuple<T> {
    /// Retrieves the sub-term at `location`.
    #[must_use]
    pub fn get_term(
        &self,
        location: &SubTupleLocation,
        tracked_engine: &TrackedEngine,
    ) -> Option<Interned<T>>
    where
        T: Identifiable + StableHash + Send + Sync + 'static,
        Self: Into<T>,
    {
        match location {
            SubTupleLocation::Single(index) => {
                self.elements.get(*index).map(|element| element.term.clone())
            }

            SubTupleLocation::Range(range) => {
                self.elements.get(range.to_std_range()).map(|elements| {
                    let elements = elements
                        .iter()
                        .map(|element| {
                            Element::new(
                                element.term.clone(),
                                element.is_unpacked,
                            )
                        })
                        .collect();

                    tracked_engine.intern(Self::new(elements).into())
                })
            }
        }
    }

    /// Iterates over tuple elements with mapped single-element locations.
    pub fn iter_terms_with_location<TermLocation>(
        &self,
        mut map_location: impl FnMut(SubTupleLocation) -> TermLocation,
    ) -> impl Iterator<Item = (&Interned<T>, TermLocation)> {
        self.elements.iter().enumerate().map(move |(index, element)| {
            (element.term(), map_location(SubTupleLocation::Single(index)))
        })
    }
}
