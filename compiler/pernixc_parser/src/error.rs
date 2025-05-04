//! Contains the definition of [`Error`] struct

use crate::{expect::Expected, state::Cursor};

/// Represents an error of encountering an unexpected token at a certain
/// possition at its possible expected tokens.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Error {
    /// The tokens that are expected at the cursor position.
    pub expecteds: Vec<Expected>,

    /// The cursor position where the error occurred.
    pub at: Cursor,
}
