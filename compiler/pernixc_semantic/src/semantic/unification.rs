//! Contains the the unification logic.

use std::collections::{HashMap, HashSet};

use super::term::{constant::Constant, lifetime::Lifetime, r#type::Type};

/// Represents a record of unifying two terms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Record<'a, T> {
    pub lhs: &'a T,
    pub rhs: &'a T,
}

/// A customization point for the unification logic.
pub trait Config<T> {
    /// Determines if the two given terms are unifiable.
    fn unifiable(&mut self, lhs: &T, rhs: &T) -> bool;
}

/// The result of unification.
///
/// The terms on the left-hand side are stored as the keys, and the terms on the right-hand side are
/// stored as the values.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Unification {
    pub lifetimes: HashMap<Lifetime, HashSet<Lifetime>>,
    pub types: HashMap<Type, HashSet<Type>>,
    pub constants: HashMap<Constant, HashSet<Constant>>,
}

impl Unification {
    /// Combines the other unification into this unification.
    pub fn combine(&mut self, other: Self) {
        for (lhs, rhs) in other.lifetimes {
            self.lifetimes.entry(lhs).or_default().extend(rhs);
        }

        for (lhs, rhs) in other.types {
            self.types.entry(lhs).or_default().extend(rhs);
        }

        for (lhs, rhs) in other.constants {
            self.constants.entry(lhs).or_default().extend(rhs);
        }
    }
}

#[cfg(test)]
mod tests;
