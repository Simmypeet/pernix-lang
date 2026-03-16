//! Contains the definition of a [`Tuple`] term.

use std::fmt::Write;

use derive_new::new;
use enum_as_inner::EnumAsInner;
use qbice::{Decode, Encode, StableHash};

use crate::{
    constant::Constant,
    instance::Instance,
    lifetime::Lifetime,
    matching::{Match, Matching, Substructural},
    sub_term::SubTerm,
    r#type::Type,
    visitor::{self, AsyncMutable, AsyncVisitor, Mutable, Visitor},
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
    term: Term,

    /// Whether the term is unpacked.
    is_unpacked: bool,
}

impl<Term> Element<Term> {
    /// Creates a new regular (not unpacked) element.
    #[must_use]
    pub const fn new_regular(term: Term) -> Self {
        Self { term, is_unpacked: false }
    }

    /// Creates a new unpacked element.
    #[must_use]
    pub const fn new_unpacked(term: Term) -> Self {
        Self { term, is_unpacked: true }
    }

    /// Creates a new unpacked element.
    #[must_use]
    pub const fn is_unpacked(&self) -> bool { self.is_unpacked }

    /// Returns a reference to the term stored in this element.
    #[must_use]
    pub const fn term(&self) -> &Term { &self.term }

    /// Returns the inner term of this tuple element.
    #[must_use]
    pub fn into_term(self) -> Term { self.term }
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
    new,
)]
pub struct Tuple<Term> {
    /// The elements of the tuple.
    elements: Vec<Element<Term>>,
}

impl<Term> Tuple<Term> {
    /// Creates a unit tuple.
    #[must_use]
    pub const fn unit() -> Self { Self { elements: Vec::new() } }

    /// Returns a reference to the elements of the tuple.
    #[must_use]
    pub fn elements(&self) -> &[Element<Term>] { &self.elements }

    /// Unpacks the tuple if it contains unpacked elements and returns the
    /// result of flattening the tuple.
    #[must_use]
    pub fn unpack_one_level(&self) -> Option<Term>
    where
        Term: Clone + TryInto<Self, Error = Term> + From<Self>,
    {
        let contain_upacked = self.elements.iter().any(|x| x.is_unpacked);

        if !contain_upacked {
            return None;
        }

        if self.elements.len() == 1 {
            return Some(self.elements()[0].term().clone());
        }

        let mut result = Vec::new();

        for element in self.elements().iter().cloned() {
            if element.is_unpacked() {
                match element.term.try_into() {
                    Ok(inner) => {
                        result.extend(inner.elements);
                    }
                    Err(term) => {
                        result.push(Element { term, is_unpacked: true });
                    }
                }
            } else {
                result.push(element);
            }
        }

        Some(Self { elements: result }.into())
    }

    /// Converts the tuple into a vector of elements.
    #[must_use]
    pub fn into_elements(self) -> Vec<Element<Term>> { self.elements }

    /// Removes the element at the given index and returns it.
    pub fn remove_at(&mut self, idx: usize) -> Element<Term> {
        self.elements.remove(idx)
    }

    /// Pushes the given element to the end of the tuple.
    pub fn push(&mut self, element: Element<Term>) {
        self.elements.push(element);
    }
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
    begin: usize,

    /// The end index of the range.
    end: usize,
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

impl SubTupleLocation {}

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

impl<T> Tuple<T> {
    /// Retrieves the sub-term from the given tuple at this location.
    #[must_use]
    pub fn get_term(&self, location: &SubTupleLocation) -> Option<T>
    where
        T: Clone,
        Self: Into<T>,
    {
        match location {
            SubTupleLocation::Single(idx) => {
                self.elements.get(*idx).map(|x| x.term.clone())
            }
            SubTupleLocation::Range(range) => self
                .elements
                .get(range.to_std_range())
                .map(|x| Self::new(x.to_vec()).into()),
        }
    }

    /// Retrieves a reference to the sub-term from the given tuple at this
    /// location.
    #[must_use]
    pub fn get_term_ref(&self, location: &SubTupleLocation) -> Option<&T> {
        match location {
            SubTupleLocation::Single(idx) => {
                self.elements.get(*idx).map(|x| &x.term)
            }
            SubTupleLocation::Range(_) => None,
        }
    }

    /// Retrieves a mutable reference to the sub-term from the given tuple at
    /// this location.
    pub fn get_term_mut(
        &mut self,
        location: &SubTupleLocation,
    ) -> Option<&mut T> {
        match location {
            SubTupleLocation::Single(idx) => {
                self.elements.get_mut(*idx).map(|x| &mut x.term)
            }
            SubTupleLocation::Range(_) => None,
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
            T::SubInstanceLocation,
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
                T::SubInstanceLocation,
            >,
            swap: bool,
        ) where
            T::ThisSubTermLocation: From<SubTupleLocation>,
        {
            if swap {
                T::get_substructural_mut(existing).push(Matching::new(
                    rhs,
                    lhs,
                    rhs_location.into(),
                    lhs_location.into(),
                ));
            } else {
                T::get_substructural_mut(existing).push(Matching::new(
                    lhs,
                    rhs,
                    lhs_location.into(),
                    rhs_location.into(),
                ));
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
            T::SubInstanceLocation,
        >,
    >
    where
        T::ThisSubTermLocation: From<SubTupleLocation>,
    {
        Self::substructural_match_internal(self, to, false)
            .or_else(|| Self::substructural_match_internal(to, self, true))
    }
}

macro_rules! implements_tuple {
    ($self:ident, $visitor:ident, $accept_single:ident, $iter:ident $(,$await:ident)?) => {{
        for (idx, element) in $self.elements.$iter().enumerate() {
            if !element.term.$accept_single(
                $visitor,
                T::Location::from(SubTupleLocation::Single(idx).into()),
            )$(.$await)? {
                return false;
            }
        }

        true
    }};
}

impl<T: visitor::Element + Clone> Tuple<T>
where
    Self: TryFrom<T, Error = T> + Into<T>,
    SubTupleLocation: Into<T::ThisSubTermLocation>,
{
    pub(crate) fn accept_one_level<
        'a,
        V: Visitor<'a, Lifetime>
            + Visitor<'a, Type>
            + Visitor<'a, Constant>
            + Visitor<'a, Instance>,
    >(
        &'a self,
        visitor: &mut V,
    ) -> bool {
        implements_tuple!(self, visitor, accept_single, iter)
    }

    pub(crate) async fn accept_one_level_async<
        V: AsyncVisitor<Lifetime>
            + AsyncVisitor<Type>
            + AsyncVisitor<Constant>
            + AsyncVisitor<Instance>,
    >(
        &self,
        visitor: &mut V,
    ) -> bool {
        implements_tuple!(self, visitor, accept_single_async, iter, await)
    }

    pub(crate) fn accept_one_level_mut<
        V: Mutable<Lifetime>
            + Mutable<Type>
            + Mutable<Constant>
            + Mutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> bool {
        implements_tuple!(self, visitor, accept_single_mut, iter_mut)
    }

    pub(crate) async fn accept_one_level_async_mut<
        V: AsyncMutable<Lifetime>
            + AsyncMutable<Type>
            + AsyncMutable<Constant>
            + AsyncMutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> bool {
        implements_tuple!(
            self,
            visitor,
            accept_single_async_mut,
            iter_mut,
            await
        )
    }
}
