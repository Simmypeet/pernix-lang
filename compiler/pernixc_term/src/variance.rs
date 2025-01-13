//! Contains the definition of [`Variance`] and [`Map`] component
use std::collections::HashMap;

use pernixc_arena::ID;
use pernixc_table::component::Derived;
use serde::{Deserialize, Serialize};

use crate::generic_parameter::{LifetimeParameter, TypeParameter};

/// An enumeration of either an invariant or covariant variance.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub enum Variance {
    /// The term is covariant and can be changed to a subtype.
    ///
    /// This is the most common variance; that is, the lifetime can be changed
    /// to a smaller lifetime e.g. `'static` to `'a`.
    Covariant,

    /// The term is contravariant and can be changed to a supertype.
    ///
    /// This is the opposite of convariant; that is, the lifetime can be
    /// changed to a larger lifetime e.g. `'a` to `'static`. This is
    /// generally used in the parameter of a function i.e. the function
    /// parameter used to accept a particular lifetime `'a` can be changed
    /// to accept a larger lifetime `'static`.
    Contravariant,

    /// The term is invariant and cannot be changed.
    #[default]
    Invariant,
}

impl Variance {
    /// Combines the two variances in terms of the variance of the parent and
    /// the variance of the child.
    ///
    /// The [`self`] is the variance of the parent and the [`child`] is the
    /// variance of the inner type.
    ///
    /// For example, to find the variance of `T` in `&'a T`, the variance of
    /// `&'a` is [`self`] and the variance of `T` is [`child`].
    #[must_use]
    pub const fn xfrom(self, child: Self) -> Self {
        match (self, child) {
            (Self::Invariant, _) | (_, Self::Invariant) => Self::Invariant,
            (amb, Self::Covariant) => amb,
            (Self::Covariant, Self::Contravariant) => Self::Contravariant,
            (Self::Contravariant, Self::Contravariant) => Self::Covariant,
        }
    }

    /// Combines the two variances in terms of the variance from different
    /// locations to find the most general variance.
    #[must_use]
    pub const fn combine(self, other: Self) -> Self {
        match (self, other) {
            (Self::Invariant, _)
            | (_, Self::Invariant)
            | (Self::Covariant, Self::Contravariant)
            | (Self::Contravariant, Self::Covariant) => Self::Invariant,

            (Self::Covariant, Self::Covariant) => Self::Covariant,
            (Self::Contravariant, Self::Contravariant) => Self::Contravariant,
        }
    }
}

/// A **presistent-derived** component storing the variance of the generic
/// parameters. This is used for calculating the subtyping relationship between
/// two types.
///
/// The compnent should be attached to the structs and enums.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Map {
    /// Maps the lifetime parameter ID to its variance.
    pub variances_by_lifetime_ids: HashMap<ID<LifetimeParameter>, Variance>,

    /// Maps the type parameter ID to its variance.
    pub variances_by_type_ids: HashMap<ID<TypeParameter>, Variance>,
}

impl Derived for Map {
    fn component_name() -> &'static str { "variance map" }
}
