//! Data definitions for tuple terms.

use derive_new::new;
use qbice::{Decode, Encode, StableHash};

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
    new,
)]
pub struct Element<Term> {
    term: Term,
    is_unpacked: bool,
}

impl<Term> Element<Term> {
    /// Creates a new regular element.
    #[must_use]
    pub const fn new_regular(term: Term) -> Self {
        Self { term, is_unpacked: false }
    }

    /// Creates a new unpacked element.
    #[must_use]
    pub const fn new_unpacked(term: Term) -> Self {
        Self { term, is_unpacked: true }
    }

    /// Returns whether the element is unpacked.
    #[must_use]
    pub const fn is_unpacked(&self) -> bool { self.is_unpacked }

    /// Returns the element term.
    #[must_use]
    pub const fn term(&self) -> &Term { &self.term }

    /// Returns a mutable reference to the element term.
    #[must_use]
    pub const fn term_mut(&mut self) -> &mut Term { &mut self.term }

    /// Consumes the element and returns its term.
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
    elements: Vec<Element<Term>>,
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

impl<Term> Default for Tuple<Term> {
    fn default() -> Self { Self::unit() }
}
