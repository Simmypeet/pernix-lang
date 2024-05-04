//! Contains the definition of the intermediate representation of the program.

use self::representation::Representation;

pub mod address;
pub mod alloca;
pub mod control_flow_graph;
pub mod instruction;
pub mod representation;
pub mod value;

/// A tag type representing a successfully generated IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Success(() /* Prevent arbitrary instantiation */);

/// A tag type representing an IR that is suboptimal (contains an error).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Suboptimal(pub ());

/// An intermediate representation of the program.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct IR<T> {
    representation: Representation,

    state: T,
}
