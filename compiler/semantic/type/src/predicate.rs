use enum_as_inner::EnumAsInner;
use pernixc_qbice::Interner;
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::{
    substitution::{Substitutable, Substitution},
    symbol::Symbol,
    r#type::{Type, bound::Binder},
    variance::Variance,
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
    left: Interned<Type>,
    right: Interned<Type>,
}

impl Equality {
    #[must_use]
    pub const fn new(
        binder: Binder,
        left: Interned<Type>,
        right: Interned<Type>,
    ) -> Self {
        Self { binder, left, right }
    }

    #[must_use]
    pub const fn binder(&self) -> &Binder { &self.binder }
    #[must_use]
    pub const fn left(&self) -> &Interned<Type> { &self.left }
    #[must_use]
    pub const fn right(&self) -> &Interned<Type> { &self.right }
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
    /// Can either has a kind of lifetime or a kind of type.
    lesser: Interned<Type>,

    /// Must always has a kind of lifetime.
    greater: Interned<Type>,
}

impl Outlives {
    #[must_use]
    pub const fn new(lesser: Interned<Type>, greater: Interned<Type>) -> Self {
        Self { lesser, greater }
    }
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
    operand: Interned<Type>,
}

impl Substitutable for Tuple {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl Interner,
    ) -> Option<Self> {
        self.operand.apply(subst, interner).map(|operand| Self { operand })
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
    symbol: Symbol,
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
    less: Interned<Type>,
    greater: Interned<Type>,
    variance: Variance,
}

impl Subtype {
    #[must_use]
    pub const fn new(
        less: Interned<Type>,
        greater: Interned<Type>,
        variance: Variance,
    ) -> Self {
        Self { less, greater, variance }
    }

    #[must_use]
    pub const fn lesser(&self) -> &Interned<Type> { &self.less }

    #[must_use]
    pub const fn greater(&self) -> &Interned<Type> { &self.greater }

    #[must_use]
    pub const fn variance(&self) -> Variance { self.variance }
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
pub enum Predicate {
    Outlives(Outlives),
    Tuple(Tuple),
    Marker(Marker),
    Equality(Equality),
}

impl Substitutable for Predicate {
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
