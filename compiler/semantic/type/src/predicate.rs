use enum_as_inner::EnumAsInner;
use pernixc_qbice::Interner;
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::{
    substitution::{Substitutable, Substitution},
    symbol::Symbol2,
    r#type::{Type2, bound::Binder},
    variance::Variance2,
};

/// A basic type equality predicate, can also be used for rewriting.
///
/// Both left and right has to be a kind of type.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Equality {
    binder: Binder,
    left: Interned<Type2>,
    right: Interned<Type2>,
}

impl Equality {
    #[must_use]
    pub const fn new(
        binder: Binder,
        left: Interned<Type2>,
        right: Interned<Type2>,
    ) -> Self {
        Self { binder, left, right }
    }

    #[must_use]
    pub const fn binder(&self) -> &Binder { &self.binder }
    #[must_use]
    pub const fn left(&self) -> &Interned<Type2> { &self.left }
    #[must_use]
    pub const fn right(&self) -> &Interned<Type2> { &self.right }
}

impl Substitutable for Equality {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl Interner,
    ) -> Option<Self>
    where
        Self: Sized,
    {
        match (
            self.left.apply(subst, interner),
            self.right.apply(subst, interner),
        ) {
            (Some(left), Some(right)) => {
                Some(Self { binder: self.binder.clone(), left, right })
            }
            (Some(left), _) => Some(Self {
                binder: self.binder.clone(),
                left,
                right: self.right.clone(),
            }),
            (_, Some(right)) => Some(Self {
                binder: self.binder.clone(),
                left: self.left.clone(),
                right,
            }),
            _ => None,
        }
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Outlives {
    /// Must have lifetime, type, or instance kind.
    lesser: Interned<Type2>,

    /// Must have lifetime kind.
    greater: Interned<Type2>,
}

impl Outlives {
    #[must_use]
    pub const fn new(
        lesser: Interned<Type2>,
        greater: Interned<Type2>,
    ) -> Self {
        Self { lesser, greater }
    }

    #[must_use]
    pub const fn lesser(&self) -> &Interned<Type2> { &self.lesser }

    #[must_use]
    pub const fn greater(&self) -> &Interned<Type2> { &self.greater }
}

impl Substitutable for Outlives {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl Interner,
    ) -> Option<Self>
    where
        Self: Sized,
    {
        match (
            self.lesser.apply(subst, interner),
            self.greater.apply(subst, interner),
        ) {
            (Some(operand), Some(bound)) => {
                Some(Self { lesser: operand, greater: bound })
            }
            (Some(operand), _) => {
                Some(Self { lesser: operand, greater: self.greater.clone() })
            }
            (_, Some(bound)) => {
                Some(Self { lesser: self.lesser.clone(), greater: bound })
            }
            _ => None,
        }
    }
}

/// Requires the operand is a tuple type. The oeprand can only be a kind of
/// Type.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Tuple {
    binder: Binder,
    operand: Interned<Type2>,
}

impl Tuple {
    /// Creates a tuple predicate.
    #[must_use]
    pub const fn new(binder: Binder, operand: Interned<Type2>) -> Self {
        Self { binder, operand }
    }

    /// Returns the variables bound over the predicate's operand.
    #[must_use]
    pub const fn binder(&self) -> &Binder { &self.binder }

    /// Returns the type required to be a tuple.
    #[must_use]
    pub const fn operand(&self) -> &Interned<Type2> { &self.operand }
}

impl Substitutable for Tuple {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl Interner,
    ) -> Option<Self> {
        self.operand
            .apply(subst, interner)
            .map(|operand| Self { binder: self.binder.clone(), operand })
    }
}

/// Positive or Negative marker.
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
)]
pub enum MarkerPolar {
    Positive,
    Negative,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Marker {
    polar: MarkerPolar,
    binder: Binder,
    symbol: Symbol2,
}

impl Marker {
    /// Creates a marker predicate.
    #[must_use]
    pub const fn new(
        polar: MarkerPolar,
        binder: Binder,
        symbol: Symbol2,
    ) -> Self {
        Self { polar, binder, symbol }
    }

    /// Returns whether the marker predicate is positive or negative.
    #[must_use]
    pub const fn polar(&self) -> MarkerPolar { self.polar }

    /// Returns the variables bound over the marker's generic arguments.
    #[must_use]
    pub const fn binder(&self) -> &Binder { &self.binder }

    /// Returns the referenced marker and its generic arguments.
    #[must_use]
    pub const fn symbol(&self) -> &Symbol2 { &self.symbol }
}

impl Substitutable for Marker {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl Interner,
    ) -> Option<Self> {
        self.symbol.apply(subst, interner).map(|symbol| Self {
            polar: self.polar,
            binder: self.binder.clone(),
            symbol,
        })
    }
}

/// Like Rust, we don't have a full-blown subtyping relation, but only
/// subtyping relation between lifetimes.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Subtype {
    less: Interned<Type2>,
    greater: Interned<Type2>,
    variance: Variance2,
}

impl Subtype {
    #[must_use]
    pub const fn new(
        less: Interned<Type2>,
        greater: Interned<Type2>,
        variance: Variance2,
    ) -> Self {
        Self { less, greater, variance }
    }

    #[must_use]
    pub const fn lesser(&self) -> &Interned<Type2> { &self.less }

    #[must_use]
    pub const fn greater(&self) -> &Interned<Type2> { &self.greater }

    #[must_use]
    pub const fn variance(&self) -> Variance2 { self.variance }
}

impl Substitutable for Subtype {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl Interner,
    ) -> Option<Self> {
        match (
            self.less.apply(subst, interner),
            self.greater.apply(subst, interner),
        ) {
            (Some(less), Some(greater)) => {
                Some(Self { less, greater, variance: self.variance })
            }
            (Some(less), _) => Some(Self {
                less,
                greater: self.greater.clone(),
                variance: self.variance,
            }),
            (_, Some(greater)) => Some(Self {
                less: self.less.clone(),
                greater,
                variance: self.variance,
            }),
            _ => None,
        }
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    StableHash,
    Encode,
    Decode,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Predicate2 {
    Outlives(Outlives),
    Tuple(Tuple),
    Marker(Marker),
    Equality(Equality),
}

impl Substitutable for Predicate2 {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl Interner,
    ) -> Option<Self> {
        match self {
            Self::Outlives(outlives) => {
                outlives.apply(subst, interner).map(Self::Outlives)
            }
            Self::Tuple(tuple) => tuple.apply(subst, interner).map(Self::Tuple),
            Self::Marker(marker) => {
                marker.apply(subst, interner).map(Self::Marker)
            }
            Self::Equality(equality) => {
                equality.apply(subst, interner).map(Self::Equality)
            }
        }
    }
}
