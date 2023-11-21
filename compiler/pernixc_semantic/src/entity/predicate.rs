//! Contains the definition of predicates that are used in the trait system.

use std::{
    collections::HashSet,
    marker::PhantomData,
    sync::atomic::{AtomicUsize, Ordering},
};

mod constant_type;

use enum_as_inner::EnumAsInner;

use super::{
    constant::Constant,
    r#type::{self, Type},
    Entity, GenericArguments, Model, Never, Region,
};
use crate::{arena::ID, logic::Mapping, symbol};

/// Represents a subset of [`Predicate`] that does not contain equality predicates.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum NonEquality<S: Model> {
    RegionOutlive(RegionOutlives<S>),
    TypeOutlive(TypeOutlives<S>),
    Trait(Trait<S>),
    ConstantType(ConstantType<S>),
}

impl<S: Model> Entity<S> for NonEquality<S>
where
    Forall: From<S::ForallRegion>,
{
    type This<A: Model> = NonEquality<A>;

    fn into_other_model<T: Model>(self) -> Self::This<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LocalRegion: Into<T::LocalRegion>,
        S::ForallRegion: Into<T::ForallRegion>,
    {
        match self {
            Self::RegionOutlive(region_outlives) => {
                Self::This::RegionOutlive(region_outlives.into_other_model())
            }
            Self::TypeOutlive(type_outlives) => {
                Self::This::TypeOutlive(type_outlives.into_other_model())
            }
            Self::Trait(trait_) => Self::This::Trait(trait_.into_other_model()),
            Self::ConstantType(constant_type) => {
                Self::This::ConstantType(constant_type.into_other_model())
            }
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::This<T>>
    where
        <S as Model>::ConstantInference: TryInto<T::ConstantInference>,
        <S as Model>::TypeInference: TryInto<T::TypeInference>,
        <S as Model>::LocalRegion: TryInto<T::LocalRegion>,
        <S as Model>::ForallRegion: TryInto<T::ForallRegion>,
    {
        Some(match self {
            Self::RegionOutlive(region_outlives) => {
                Self::This::RegionOutlive(region_outlives.try_into_other_model()?)
            }
            Self::TypeOutlive(type_outlives) => {
                Self::This::TypeOutlive(type_outlives.try_into_other_model()?)
            }
            Self::Trait(trait_) => Self::This::Trait(trait_.try_into_other_model()?),
            Self::ConstantType(constant_type) => {
                Self::This::ConstantType(constant_type.try_into_other_model()?)
            }
        })
    }

    fn apply(&mut self, substitution: &super::Substitution<S>) {
        match self {
            Self::RegionOutlive(region_outlives) => region_outlives.apply(substitution),
            Self::TypeOutlive(type_outlives) => type_outlives.apply(substitution),
            Self::Trait(trait_) => trait_.apply(substitution),
            Self::ConstantType(constant_type) => constant_type.apply(substitution),
        }
    }
}

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

impl<S: Model> Entity<S> for Predicate<S>
where
    Forall: From<S::ForallRegion>,
{
    type This<A: Model> = Predicate<A>;

    fn into_other_model<T: Model>(self) -> Self::This<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LocalRegion: Into<T::LocalRegion>,
        S::ForallRegion: Into<T::ForallRegion>,
    {
        match self {
            Self::RegionOutlive(region_outlives) => {
                Self::This::RegionOutlive(region_outlives.into_other_model())
            }
            Self::TypeOutlive(type_outlives) => {
                Self::This::TypeOutlive(type_outlives.into_other_model())
            }
            Self::TypeEquals(type_equals) => Self::This::TypeEquals(type_equals.into_other_model()),
            Self::ConstantEquals(constant_equals) => {
                Self::This::ConstantEquals(constant_equals.into_other_model())
            }
            Self::Trait(trait_) => Self::This::Trait(trait_.into_other_model()),
            Self::ConstantType(constant_type) => {
                Self::This::ConstantType(constant_type.into_other_model())
            }
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::This<T>>
    where
        <S as Model>::ConstantInference: TryInto<T::ConstantInference>,
        <S as Model>::TypeInference: TryInto<T::TypeInference>,
        <S as Model>::LocalRegion: TryInto<T::LocalRegion>,
        <S as Model>::ForallRegion: TryInto<T::ForallRegion>,
    {
        Some(match self {
            Self::RegionOutlive(region_outlives) => {
                Self::This::RegionOutlive(region_outlives.try_into_other_model()?)
            }
            Self::TypeOutlive(type_outlives) => {
                Self::This::TypeOutlive(type_outlives.try_into_other_model()?)
            }
            Self::TypeEquals(type_equals) => {
                Self::This::TypeEquals(type_equals.try_into_other_model()?)
            }
            Self::ConstantEquals(constant_equals) => {
                Self::This::ConstantEquals(constant_equals.try_into_other_model()?)
            }
            Self::Trait(trait_) => Self::This::Trait(trait_.try_into_other_model()?),
            Self::ConstantType(constant_type) => {
                Self::This::ConstantType(constant_type.try_into_other_model()?)
            }
        })
    }

    fn apply(&mut self, substitution: &super::Substitution<S>) {
        match self {
            Self::RegionOutlive(region_outlives) => region_outlives.apply(substitution),
            Self::TypeOutlive(type_outlives) => type_outlives.apply(substitution),
            Self::TypeEquals(type_equals) => type_equals.apply(substitution),
            Self::ConstantEquals(constant_equals) => constant_equals.apply(substitution),
            Self::Trait(trait_) => trait_.apply(substitution),
            Self::ConstantType(constant_type) => constant_type.apply(substitution),
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

impl<S: Model> Entity<S> for ConstantType<S> {
    type This<A: Model> = ConstantType<A>;

    fn into_other_model<T: Model>(self) -> Self::This<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LocalRegion: Into<T::LocalRegion>,
        S::ForallRegion: Into<T::ForallRegion>,
    {
        Self::This {
            r#type: self.r#type.into_other_model(),
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::This<T>>
    where
        <S as Model>::ConstantInference: TryInto<T::ConstantInference>,
        <S as Model>::TypeInference: TryInto<T::TypeInference>,
        <S as Model>::LocalRegion: TryInto<T::LocalRegion>,
        <S as Model>::ForallRegion: TryInto<T::ForallRegion>,
    {
        Some(Self::This {
            r#type: self.r#type.try_into_other_model()?,
        })
    }

    fn apply(&mut self, substitution: &super::Substitution<S>) { self.r#type.apply(substitution); }
}

/// Represents a region outlive predicate, defnoted by `'argument: 'target` syntax.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegionOutlives<S: Model> {
    /// The region that lives as long or longer than the [`Self::argument`] lifetime.
    pub operand: Region<S>,

    /// The region that the [`Self::operand`] lives as long or longer than.
    pub argument: Region<S>,
}

impl<S: Model> Entity<S> for RegionOutlives<S> {
    type This<A: Model> = RegionOutlives<A>;

    fn into_other_model<T: Model>(self) -> Self::This<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LocalRegion: Into<T::LocalRegion>,
        S::ForallRegion: Into<T::ForallRegion>,
    {
        Self::This {
            operand: self.operand.into_other_model(),
            argument: self.argument.into_other_model(),
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::This<T>>
    where
        <S as Model>::ConstantInference: TryInto<T::ConstantInference>,
        <S as Model>::TypeInference: TryInto<T::TypeInference>,
        <S as Model>::LocalRegion: TryInto<T::LocalRegion>,
        <S as Model>::ForallRegion: TryInto<T::ForallRegion>,
    {
        Some(Self::This {
            operand: self.operand.try_into_other_model()?,
            argument: self.argument.try_into_other_model()?,
        })
    }

    fn apply(&mut self, substitution: &super::Substitution<S>) {
        self.operand.apply(substitution);
        self.argument.apply(substitution);
    }
}

/// Represents a type outlive predicate, denoted by `TYPE: 'target` syntax.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeOutlives<S: Model> {
    /// The type that lives as long or longer than the [`Self::argument`] lifetime.
    pub operand: Type<S>,

    /// The region that the [`Self::operand`] lives as long or longer than.
    pub argument: Region<S>,
}

impl<S: Model> Entity<S> for TypeOutlives<S> {
    type This<A: Model> = TypeOutlives<A>;

    fn into_other_model<T: Model>(self) -> Self::This<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LocalRegion: Into<T::LocalRegion>,
        S::ForallRegion: Into<T::ForallRegion>,
    {
        Self::This {
            operand: self.operand.into_other_model(),
            argument: self.argument.into_other_model(),
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::This<T>>
    where
        <S as Model>::ConstantInference: TryInto<T::ConstantInference>,
        <S as Model>::TypeInference: TryInto<T::TypeInference>,
        <S as Model>::LocalRegion: TryInto<T::LocalRegion>,
        <S as Model>::ForallRegion: TryInto<T::ForallRegion>,
    {
        Some(Self::This {
            operand: self.operand.try_into_other_model()?,
            argument: self.argument.try_into_other_model()?,
        })
    }

    fn apply(&mut self, substitution: &super::Substitution<S>) {
        self.operand.apply(substitution);
        self.argument.apply(substitution);
    }
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

impl<S: Model> Entity<S> for TypeEquals<S> {
    type This<A: Model> = TypeEquals<A>;

    fn into_other_model<T: Model>(self) -> Self::This<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LocalRegion: Into<T::LocalRegion>,
        S::ForallRegion: Into<T::ForallRegion>,
    {
        Self::This {
            lhs: self.lhs.into_other_model(),
            rhs: self.rhs.into_other_model(),
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::This<T>>
    where
        <S as Model>::ConstantInference: TryInto<T::ConstantInference>,
        <S as Model>::TypeInference: TryInto<T::TypeInference>,
        <S as Model>::LocalRegion: TryInto<T::LocalRegion>,
        <S as Model>::ForallRegion: TryInto<T::ForallRegion>,
    {
        Some(Self::This {
            lhs: self.lhs.try_into_other_model()?,
            rhs: self.rhs.try_into_other_model()?,
        })
    }

    fn apply(&mut self, substitution: &super::Substitution<S>) {
        self.lhs.apply(substitution);
        self.rhs.apply(substitution);
    }
}

/// Represents a constant equality predicate, denoted by `CONSTANT = CONSTANT` syntax.
pub type ConstantEquals<S> = Equals<Constant<S>>;

impl<S: Model> Entity<S> for ConstantEquals<S> {
    type This<A: Model> = ConstantEquals<A>;

    fn into_other_model<T: Model>(self) -> Self::This<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LocalRegion: Into<T::LocalRegion>,
        S::ForallRegion: Into<T::ForallRegion>,
    {
        Self::This {
            lhs: self.lhs.into_other_model(),
            rhs: self.rhs.into_other_model(),
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::This<T>>
    where
        <S as Model>::ConstantInference: TryInto<T::ConstantInference>,
        <S as Model>::TypeInference: TryInto<T::TypeInference>,
        <S as Model>::LocalRegion: TryInto<T::LocalRegion>,
        <S as Model>::ForallRegion: TryInto<T::ForallRegion>,
    {
        Some(Self::This {
            lhs: self.lhs.try_into_other_model()?,
            rhs: self.rhs.try_into_other_model()?,
        })
    }

    fn apply(&mut self, substitution: &super::Substitution<S>) {
        self.lhs.apply(substitution);
        self.rhs.apply(substitution);
    }
}

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

impl<S: Model> Entity<S> for Trait<S>
where
    Forall: From<S::ForallRegion>,
{
    type This<A: Model> = Trait<A>;

    fn into_other_model<T: Model>(self) -> Self::This<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LocalRegion: Into<T::LocalRegion>,
        S::ForallRegion: Into<T::ForallRegion>,
    {
        Self::This {
            trait_id: self.trait_id,
            const_trait: self.const_trait,
            generic_arguments: self.generic_arguments.into_other_model(),
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::This<T>>
    where
        <S as Model>::ConstantInference: TryInto<T::ConstantInference>,
        <S as Model>::TypeInference: TryInto<T::TypeInference>,
        <S as Model>::LocalRegion: TryInto<T::LocalRegion>,
        <S as Model>::ForallRegion: TryInto<T::ForallRegion>,
    {
        Some(Self::This {
            trait_id: self.trait_id,
            const_trait: self.const_trait,
            generic_arguments: self.generic_arguments.try_into_other_model()?,
        })
    }

    fn apply(&mut self, substitution: &super::Substitution<S>) {
        self.generic_arguments
            .apply(&substitution.clone().into_other_model());
    }
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

impl TryInto<Never> for Forall {
    type Error = Self;

    fn try_into(self) -> Result<Never, Self::Error> { Err(self) }
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

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct QueryReocrds<S: Model> {
    constant_types: HashSet<r#type::Type<S>>,
}

/// Containing a list of non-equality predicates and a [`Mapping`] representing equality of terms.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Premises<S: Model> {
    /// A list of non-equality predicates.
    pub non_equality_predicates: Vec<NonEquality<S>>,

    /// A mapping that represents equality of terms.
    pub mapping: Mapping<S>,
}

impl<S: Model> Premises<S> {
    /// Creates a new instance of [`Premises`] from a list of predicates.
    ///
    /// Equality predicates are separated from non-equality predicates and stored in a [`Mapping`].
    pub fn from_predicates(predicates: impl Iterator<Item = Predicate<S>>) -> Self {
        let mut mapping = Mapping::default();
        let mut non_equality_predicates = Vec::new();

        for predicate in predicates {
            match predicate {
                Predicate::RegionOutlive(outlives) => {
                    non_equality_predicates.push(NonEquality::RegionOutlive(outlives));
                }
                Predicate::TypeOutlive(outlives) => {
                    non_equality_predicates.push(NonEquality::TypeOutlive(outlives));
                }
                Predicate::TypeEquals(type_equals) => {
                    mapping.insert_type(type_equals.lhs, type_equals.rhs);
                }
                Predicate::ConstantEquals(constant_equals) => {
                    mapping.insert_constant(constant_equals.lhs, constant_equals.rhs);
                }
                Predicate::Trait(trait_bound) => {
                    non_equality_predicates.push(NonEquality::Trait(trait_bound));
                }
                Predicate::ConstantType(constant_type) => {
                    non_equality_predicates.push(NonEquality::ConstantType(constant_type));
                }
            }
        }

        Self {
            non_equality_predicates,
            mapping,
        }
    }
}
