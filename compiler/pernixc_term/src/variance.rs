//! Contains the definition of [`Variance`].

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

    /// The term can be transformed to any lifetime.
    Bivariant,
}

impl Variance {
    /// Combines the two variances in terms of the variance of the parent and
    /// the variance of the child.
    ///
    /// For example, to find the variance of `T` in `&'a mutable T`, the
    /// variance of `&'a mutable`, which is [`Variance::Invariant`], is
    /// [`other`] and the variance of `T` is [`self`].
    #[must_use]
    pub const fn xfrom(self, other: Self) -> Self {
        match (self, other) {
            (Self::Contravariant, Self::Contravariant)
            | (Self::Covariant, Self::Covariant) => Self::Covariant,

            (Self::Contravariant, Self::Covariant)
            | (Self::Covariant, Self::Contravariant) => Self::Contravariant,

            (Self::Invariant, _)
            | (Self::Contravariant | Self::Covariant, Self::Invariant) => {
                Self::Invariant
            }

            (Self::Contravariant | Self::Covariant, Self::Bivariant) => {
                Self::Bivariant
            }

            (Self::Bivariant, _) => Self::Bivariant,
        }
    }

    /// Combines the two variances in terms of the variance from different
    /// locations to find the most general variance.
    #[must_use]
    pub const fn get_lower_bound(self, other: Self) -> Self {
        match (self, other) {
            (Self::Covariant, Self::Contravariant)
            | (Self::Contravariant, Self::Covariant)
            | (Self::Invariant, _)
            | (_, Self::Invariant) => Self::Invariant,

            (Self::Covariant, Self::Covariant) => Self::Covariant,

            (Self::Contravariant, Self::Contravariant) => Self::Contravariant,

            (x, Self::Bivariant) | (Self::Bivariant, x) => x,
        }
    }
}

/// A **presistent-derived** component storing the variance of the generic
/// parameters. This is used for calculating the subtyping relationship between
/// two types.
///
/// The compnent should be attached to the structs and enums.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Variances {
    /// Maps the lifetime parameter ID to its variance.
    pub variances_by_lifetime_ids: HashMap<ID<LifetimeParameter>, Variance>,

    /// Maps the type parameter ID to its variance.
    pub variances_by_type_ids: HashMap<ID<TypeParameter>, Variance>,
}

impl Derived for Variances {
    fn component_name() -> &'static str { "variances" }
}
