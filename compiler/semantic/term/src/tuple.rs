//! Contains the definition of a [`Tuple`] term.

use enum_as_inner::EnumAsInner;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

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
    Serialize,
    Deserialize,
)]
pub struct Element<Term> {
    /// The term stored in this element.
    pub term: Term,

    /// Whether the term is unpacked.
    pub is_unpacked: bool,
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
    Default,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Tuple<Term> {
    /// The elements of the tuple.
    pub elements: Vec<Element<Term>>,
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
    Range {
        /// The index of the first element of the tuple.
        begin: usize,

        /// The index of the after the last element of the tuple.
        end: usize,
    },
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
            SubTupleLocation::Range { begin, end } => {
                let Ok(sub_constant_tuple) = Self::try_from(sub_term) else {
                    panic!("tuple expected");
                };

                let tuple_elements = self.elements.get_mut(begin..end).unwrap();

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
