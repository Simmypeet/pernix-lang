//! Contains the definition of the intermediate representation of the program.

use std::{fmt::Debug, hash::Hash};

use self::representation::Representation;
use crate::semantic::model::{self, Model};

pub mod address;
pub mod alloca;
pub mod control_flow_graph;
pub mod instruction;
pub mod pattern;
pub mod representation;
pub mod value;

/// The model to used to generate the IR.
pub trait State {
    /// The model to use for the type system.
    type Model: Model;
}

/// A tag type representing a successfully generated IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Success(() /* Prevent arbitrary instantiation */);

impl State for Success {
    type Model = model::Default;
}

/// A tag type representing an IR that is suboptimal (contains an error).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Suboptimal;

impl State for Suboptimal {
    type Model = model::Default;
}

/// An intermediate representation of the program.
#[derive(Debug, Clone, PartialEq, Eq, Default, derive_more::Deref)]
pub struct IR<T: State> {
    #[deref]
    pub(crate) representation: Representation<T::Model>,

    state: T,
}
