//! Contains the definition of predicates that are used in the trait system.

use std::{
    marker::PhantomData,
    sync::atomic::{AtomicUsize, Ordering},
};

use enum_as_inner::EnumAsInner;

use super::{constant::Constant, r#type::Type, Entity, GenericArguments, Model, Never, Region};
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
    ConstantType(ConstantType<S>),
}

impl<S: Model<ForallRegion = Never>> Entity<S> for Predicate<S> {
    type This<A: Model> = Predicate<A>;

    fn into_other_model<T: Model>(self) -> Self::This<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LocalRegion: Into<T::LocalRegion>,
        S::ForallRegion: Into<T::ForallRegion>,
    {
        match self {
            Self::RegionOutlive(region) => Predicate::RegionOutlive(RegionOutlives {
                operand: region.operand.into_other_model(),
                argument: region.argument.into_other_model(),
            }),
            Self::TypeOutlive(outlive) => Predicate::TypeOutlive(TypeOutlives {
                operand: outlive.operand.into_other_model(),
                argument: outlive.argument.into_other_model(),
            }),
            Self::TypeEquals(equals) => Predicate::TypeEquals(TypeEquals {
                lhs: equals.lhs.into_other_model(),
                rhs: equals.rhs.into_other_model(),
            }),
            Self::ConstantEquals(equals) => Predicate::ConstantEquals(ConstantEquals {
                lhs: equals.lhs.into_other_model(),
                rhs: equals.rhs.into_other_model(),
            }),
            Self::Trait(predicate) => Predicate::Trait(Trait {
                trait_id: predicate.trait_id,
                generic_arguments: predicate.generic_arguments.into_other_model(),
                const_trait: predicate.const_trait,
            }),
            Self::ConstantType(predicate) => Predicate::ConstantType(ConstantType {
                r#type: predicate.r#type.into_other_model(),
            }),
        }
    }

    fn apply(&mut self, substitution: &super::Substitution<S>) {
        match self {
            Self::RegionOutlive(region) => {
                region.operand.apply(substitution);
                region.argument.apply(substitution);
            }
            Self::TypeOutlive(outlive) => {
                outlive.operand.apply(substitution);
                outlive.argument.apply(substitution);
            }
            Self::TypeEquals(equals) => {
                equals.lhs.apply(substitution);
                equals.rhs.apply(substitution);
            }
            Self::ConstantEquals(equals) => {
                equals.lhs.apply(substitution);
                equals.rhs.apply(substitution);
            }
            Self::Trait(predicate) => predicate
                .generic_arguments
                .apply(&substitution.clone().into_other_model()),
            Self::ConstantType(predicate) => predicate.r#type.apply(substitution),
        }
    }
}

/// Denotes a constant type predicate, denoted by `const TYPE` syntax.
///
/// This predicate allows the type to be used as a type of constant parameters.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantType<S: Model> {
    /// The type that is used as a constant type.
    pub r#type: Type<S>,
}

/// Represents a region outlive predicate, defnoted by `'argument: 'target` syntax.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegionOutlives<S: Model> {
    /// The region that lives as long or longer than the [`Self::argument`] lifetime.
    pub operand: Region<S>,

    /// The region that the [`Self::operand`] lives as long or longer than.
    pub argument: Region<S>,
}

/// Represents a type outlive predicate, denoted by `TYPE: 'target` syntax.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeOutlives<S: Model> {
    /// The type that lives as long or longer than the [`Self::argument`] lifetime.
    pub operand: Type<S>,

    /// The region that the [`Self::operand`] lives as long or longer than.
    pub argument: Region<S>,
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

    /// Determines whether the trait must be implemented in `const` context or not.
    pub const_trait: bool,

    /// The generic arguments supplied to the trait.
    pub generic_arguments: GenericArguments<Quantified<S>>,
}

/// Represents a for-all quantified region, denoted by `for<'a>` syntax, used in higher-ranked trait
/// bounds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Forall(usize);

impl From<Never> for Forall {
    fn from(never: Never) -> Self { match never {} }
}

impl Forall {
    #[allow(missing_docs)]
    pub fn generate() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        Self(COUNTER.fetch_add(1, Ordering::SeqCst))
    }
}

/// Represents a struct that implements [`Model`] trait where the region context is assigned to
/// [`Forall`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Quantified<S: Model>(PhantomData<S>);

impl<S: Model> Model for Quantified<S> {
    type ConstantInference = S::ConstantInference;
    type ForallRegion = Forall;
    type LocalRegion = S::LocalRegion;
    type TypeInference = S::TypeInference;
}
