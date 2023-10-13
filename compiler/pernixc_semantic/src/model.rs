//! Contains the semantic model of the langauage.
//!
//! Main semantic model in the Pernix language are types and constants.

use std::{fmt::Debug, hash::Hash};

use self::{constant::Constant, r#type::Type};

pub mod constant;
pub mod pattern;
pub mod r#type;

/// Represents a system of types and constants model.
/// /// Since the language mainly has two systems, symbolic and IR, these two systems use similar
/// types and constants model but have some subtle differences. To avoid code duplication, this
/// trait defines the common interface of the two systems.
pub trait System {
    /// The type used to represent type inference variable.
    type TypeInference: Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash;

    /// The type used to represent constant inference variable.
    type ConstantInference: Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash;

    /// The type used to represent lifetime variable.
    type Region: Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArguments<S: System> {
    pub lifetimes: Vec<Lifetime<S>>,
    pub types: Vec<Type<S>>,
    pub constants: Vec<Constant<S>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lifetime<S: System> {
    Static,
    Region(S::Region),
}
