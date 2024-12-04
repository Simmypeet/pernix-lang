//! Contains the definition of [`Model`].

use std::{fmt::Debug, hash::Hash};

use super::term::{
    constant::Constant, lifetime::Lifetime, r#type::Type, Never,
};

/// The model that the terms will be based on.
///
/// The model is used for defining the inferences that can be made in the terms.
pub trait Model:
    Debug
    + Clone
    + Copy
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + Hash
    + std::default::Default
    + 'static
    + Send
    + Sync
{
    /// The type to use for lifetime inference.
    type LifetimeInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + From<Never>;

    /// The type to use for type inference.
    type TypeInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + From<Never>;

    /// The type to use for constant inference.
    type ConstantInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + From<Never>;

    /// Converts a type from the default model to the current model.
    fn from_default_type(ty: Type<Default>) -> Type<Self>;

    /// Converts a lifetime from the default model to the current model.
    fn from_default_lifetime(lifetime: Lifetime<Default>) -> Lifetime<Self>;

    /// Converts a constant from the default model to the current model.
    fn from_default_constant(constant: Constant<Default>) -> Constant<Self>;
}

/// The default model where all inferences are [`Never`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Default;

impl Model for Default {
    type LifetimeInference = Never;
    type TypeInference = Never;
    type ConstantInference = Never;

    fn from_default_type(ty: Type<Default>) -> Type<Self> { ty }
    fn from_default_lifetime(lifetime: Lifetime<Default>) -> Lifetime<Self> {
        lifetime
    }
    fn from_default_constant(constant: Constant<Default>) -> Constant<Self> {
        constant
    }
}
