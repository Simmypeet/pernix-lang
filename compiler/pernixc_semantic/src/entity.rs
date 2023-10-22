//! Contains all the declarations of the semantic terms.

use std::{fmt::Debug, hash::Hash};

use self::{constant::Constant, r#type::Type, region::Region};

pub mod constant;
pub mod pattern;
pub mod predicate;
pub mod region;
pub mod r#type;

/// Represents a model of entities
///
/// The trait defines several types that are used to represent dependent entities. Meaning that,
/// various models might have different types for representing the same entity.
pub trait Model:
    Debug + Clone + Copy + PartialEq + Eq + PartialOrd + Ord + Hash + Default + 'static + Send + Sync
{
    /// The type used to represent type inference variable.
    type TypeInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync;

    /// The type used to represent constant inference variable.
    type ConstantInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync;

    /// The type used to represent local region variable.
    type RegionContext: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync;
}

/// Represents a list of generic arguments supplied to a particular generic symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArguments<S: Model> {
    /// List of region arguments.
    pub regions: Vec<Region<S>>,

    /// List of type arguments.
    pub types: Vec<Type<S>>,

    /// List of constant arguments.
    pub constants: Vec<Constant<S>>,
}

/// A type that can't never be instantiated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Never {}
