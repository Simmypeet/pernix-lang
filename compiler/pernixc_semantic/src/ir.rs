//! Contains the definition of the intermediate representation of the program.

use self::representation::Representation;

pub mod address;
pub mod alloca;
pub mod cfg;
pub mod instruction;
mod representation;
pub mod value;

/// A tag type representing a successfully generated IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Success(() /* Prevent arbitrary instantiation */);

/// A tag type representing an IR that is suboptimal (contains an error).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Suboptimal(() /* Prevent arbitrary instantitation */);

/// An intermediate representation of the program.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IR<T> {
    representation: Representation<T>,
    state: T,
}
