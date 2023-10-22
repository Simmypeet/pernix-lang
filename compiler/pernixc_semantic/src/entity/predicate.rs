//! Contains the definition of [`Predicate`]

use std::{
    marker::PhantomData,
    sync::atomic::{AtomicUsize, Ordering},
};

use enum_as_inner::EnumAsInner;

use super::{constant::Constant, r#type::Type, GenericArguments, Model, Never, Region};
use crate::{arena::ID, symbol};

/// Represents a predicate in that will be used in the generic engine.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Predicate<S: Model> {
    RegionOutlive(RegionOutlives<S>),
    TypeOutlive(TypeOutlives<S>),
    TypeEquals(TypeEquals<S>),
    ConstantEquals(ConstantEquals<S>),
    Trait(Trait<S>),
}

/// Represents a region outlive predicate, defnoted by `'argument: 'target` syntax.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegionOutlives<S: Model> {
    /// The region that lives as long or longer than the [`Self::target`] lifetime.
    pub argument: Region<S>,

    /// The region that the [`Self::argument`] lives as long or longer than.
    pub target: Region<S>,
}

/// Represents a type outlive predicate, denoted by `TYPE: 'target` syntax.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeOutlives<S: Model> {
    /// The type that lives as long or longer than the [`Self::target`] lifetime.
    pub argument: Type<S>,

    /// The region that the [`Self::argument`] lives as long or longer than.
    pub target: Region<S>,
}

/// Represents an entity equality predicate, denoted by `ENTITY = ENTITY` syntax.
#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Equals<Entity> {
    /// The left hand side of the predicate.
    pub lhs: Entity,

    /// The right hand side of the predicate.
    pub rhs: Entity,
}

/// Represents a type equality predicate, denoted by `TYPE = TYPE` syntax.
pub type TypeEquals<S> = Equals<Type<S>>;

/// Represents a constant equality predicate, denoted by `CONSTANT = CONSTANT` syntax.
pub type ConstantEquals<S> = Equals<Constant<S>>;

/// Represents a trait implementation predicate, denoted by `TRAIT<GENERIC_ARGUMENTS>` syntax.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trait<S: Model> {
    /// The trait that is implemented.
    pub trait_id: ID<symbol::Trait>,

    /// The generic arguments supplied to the trait.
    pub generic_arguments: GenericArguments<S>,
}

/// Represents a for-all quantified region, denoted by `for<'a>` syntax, used in higher-ranked trait
/// bounds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Forall(usize);

impl Forall {
    #[allow(missing_docs)]
    pub fn generate() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        Self(COUNTER.fetch_add(1, Ordering::SeqCst))
    }
}

/// Represents a struct that implements [`System`] trait where the region context is assigned to
/// [`Forall`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct QuantifiedSystem<S: Model<RegionContext = Never>>(PhantomData<S>);

impl<S: Model<RegionContext = Never>> Model for QuantifiedSystem<S> {
    type ConstantInference = S::ConstantInference;
    type RegionContext = Forall;
    type TypeInference = S::TypeInference;
}
