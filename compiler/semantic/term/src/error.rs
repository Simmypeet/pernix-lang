//! Contains the definition of the [`Error`] term.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    constant::Constant,
    lifetime::Lifetime,
    sub_term::TermLocation,
    r#type::Type,
    visitor::{self, Recursive},
};

/// Represents an errornuos term. Used for representing errors in the type
/// system.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ContainsErrorVisitor {
    contains_error: bool,
}

impl Recursive<'_, Lifetime> for ContainsErrorVisitor {
    fn visit(
        &mut self,
        term: &Lifetime,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_error() {
            self.contains_error = true;
            false
        } else {
            true
        }
    }
}

impl Recursive<'_, Type> for ContainsErrorVisitor {
    fn visit(
        &mut self,
        term: &Type,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_error() {
            self.contains_error = true;
            false
        } else {
            true
        }
    }
}

impl Recursive<'_, Constant> for ContainsErrorVisitor {
    fn visit(
        &mut self,
        term: &Constant,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_error() {
            self.contains_error = true;
            false
        } else {
            true
        }
    }
}

/// Checks if the term contains an error.
pub fn contains_error<T: visitor::Element>(term: &T) -> bool {
    let mut visitor = ContainsErrorVisitor { contains_error: false };

    visitor::accept_recursive(term, &mut visitor);

    visitor.contains_error
}
