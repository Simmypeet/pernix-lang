//! Contains the definition of [`Variance`] and [`Variances`] query.
use pernixc_hash::HashMap;
use pernixc_target::Global;
use pernixc_term::generic_parameters::{LifetimeParameter, TypeParameter};
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, storage::intern::Interned,
};

/// Representing a variance used by the sub-typing system to determine the
/// relationship between two types.
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
    StableHash,
    Encode,
    Decode,
    Identifiable,
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

/// A mapping of generic parameters to their variances. This component only
/// presents in the `struct` or `enum` symbols.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    StableHash,
    Encode,
    Decode,
    Identifiable,
)]
pub struct Variances {
    /// Maps the lifetime parameter ID to its variance.
    pub variances_by_lifetime_ids:
        HashMap<pernixc_arena::ID<LifetimeParameter>, Variance>,

    /// Maps the type parameter ID to its variance.
    pub variances_by_type_ids:
        HashMap<pernixc_arena::ID<TypeParameter>, Variance>,
}

/// Query key for retrieving the variances of a symbol.
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
    Encode,
    Decode,
    Query,
)]
#[value(Interned<Variances>)]
#[extend(name = get_variances, by_val)]
pub struct Key {
    /// The global ID of the struct or enum symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
}
