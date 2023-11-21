//! Contains all the declarations of the semantic terms.

use std::{collections::HashMap, fmt::Debug, hash::Hash};

use self::{constant::Constant, r#type::Type, region::Region};
use crate::{
    arena::ID,
    symbol::{ConstantParameterID, GenericID, LifetimeParameterID, TypeParameterID},
};

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
        + Sync
        + From<Never>;

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
        + Sync
        + From<Never>;

    /// The type used to represent local region variable.
    type LocalRegion: Debug
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

    /// The type used to represents a higher-ranked (forall quantified) region.
    type ForallRegion: Debug
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
}

macro_rules! substitute_tuple_term {
    ($kind:ident, $self:ident, $tuple:ident, $substitution:ident) => {
        if $tuple
            .elements
            .iter()
            .filter(|x| x.as_unpacked().is_some())
            .count()
            == 0
        {
            for element in &mut $tuple.elements {
                element.as_regular_mut().unwrap().apply($substitution);
            }
        } else {
            let mut elements = Vec::with_capacity($tuple.elements.len());

            for element in $tuple.elements.clone() {
                match element {
                    $kind::TupleElement::Regular(mut regular) => {
                        regular.apply($substitution);
                        elements.push($kind::TupleElement::Regular(regular));
                    }
                    $kind::TupleElement::Unpacked(unpacked) => {
                        let mut unpacked = match unpacked {
                            $kind::Unpacked::Parameter(parameter) => Self::Parameter(parameter),
                            $kind::Unpacked::TraitMember(trait_member) => {
                                Self::TraitMember(trait_member)
                            }
                        };

                        unpacked.apply($substitution);

                        match unpacked {
                            Self::TraitMember(trait_member) => {
                                elements.push($kind::TupleElement::Unpacked(
                                    $kind::Unpacked::TraitMember(trait_member),
                                ));
                            }
                            Self::Parameter(parameter) => {
                                elements.push($kind::TupleElement::Unpacked(
                                    $kind::Unpacked::Parameter(parameter),
                                ));
                            }
                            Self::Tuple(tuples) => {
                                elements.reserve($tuple.elements.len());
                                for element in tuples.elements {
                                    elements.push(element);
                                }
                            }
                            regular => {
                                elements.push($kind::TupleElement::Regular(regular));
                            }
                        }
                    }
                }
            }

            *$self = Self::Tuple($kind::Tuple { elements });
        }
    };
}

use substitute_tuple_term;

/// Provides common functionalities for manipulating entities.
pub trait Entity<S: Model> {
    /// Placeholder type for the entity itself.
    ///
    /// Used for model conversion.
    type This<A: Model>;

    /// Converts this entity into another model.
    #[must_use]
    fn into_other_model<T: Model>(self) -> Self::This<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LocalRegion: Into<T::LocalRegion>,
        S::ForallRegion: Into<T::ForallRegion>;

    /// Tries to convert this entity into another model.
    #[must_use]
    fn try_into_other_model<T: Model>(self) -> Option<Self::This<T>>
    where
        S::ConstantInference: TryInto<T::ConstantInference>,
        S::TypeInference: TryInto<T::TypeInference>,
        S::LocalRegion: TryInto<T::LocalRegion>,
        S::ForallRegion: TryInto<T::ForallRegion>;

    /// Applies the given substitution to the entity.
    fn apply(&mut self, substitution: &Substitution<S>);
}

/// Represents a substitution of terms.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Substitution<S: Model> {
    pub types: HashMap<Type<S>, Type<S>>,
    pub constants: HashMap<Constant<S>, Constant<S>>,
    pub regions: HashMap<Region<S>, Region<S>>,
}

impl<S: Model> Substitution<S> {
    fn into_other_model<T: Model>(self) -> Substitution<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LocalRegion: Into<T::LocalRegion>,
        S::ForallRegion: Into<T::ForallRegion>,
    {
        Substitution {
            types: self
                .types
                .into_iter()
                .map(|(key, value)| (key.into_other_model(), value.into_other_model()))
                .collect(),
            constants: self
                .constants
                .into_iter()
                .map(|(key, value)| (key.into_other_model(), value.into_other_model()))
                .collect(),
            regions: self
                .regions
                .into_iter()
                .map(|(key, value)| (key.into_other_model(), value.into_other_model()))
                .collect(),
        }
    }
}

impl<S: Model> Substitution<S> {
    /// Appends the given generic arguments as a substitution.
    ///
    /// # Returns
    ///
    /// Returns `None` if the substitution already contains the given generic arguments.
    #[must_use]
    pub fn append_from_generic_arguments(
        mut self,
        generic_arguments: GenericArguments<S>,
        generic_id: GenericID,
    ) -> Option<Self> {
        for (index, term) in generic_arguments.types.into_iter().enumerate() {
            if self
                .types
                .insert(
                    Type::Parameter(TypeParameterID {
                        parent: generic_id,
                        id: ID::new(index),
                    }),
                    term,
                )
                .is_some()
            {
                return None;
            }
        }

        for (index, term) in generic_arguments.constants.into_iter().enumerate() {
            if self
                .constants
                .insert(
                    Constant::Parameter(ConstantParameterID {
                        parent: generic_id,
                        id: ID::new(index),
                    }),
                    term,
                )
                .is_some()
            {
                return None;
            }
        }

        for (index, term) in generic_arguments.regions.into_iter().enumerate() {
            if self
                .regions
                .insert(
                    Region::Named(LifetimeParameterID {
                        parent: generic_id,
                        id: ID::new(index),
                    }),
                    term,
                )
                .is_some()
            {
                return None;
            }
        }

        Some(self)
    }

    /// Converts the given generic arguments into a substitution.
    #[must_use]
    pub fn from_generic_arguments(
        generic_arguments: GenericArguments<S>,
        generic_id: GenericID,
    ) -> Self {
        Self::default()
            .append_from_generic_arguments(generic_arguments, generic_id)
            .unwrap()
    }
}

/// Represents a list of generic arguments supplied to a particular generic symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
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

impl<S: Model> Entity<S> for GenericArguments<S> {
    type This<A: Model> = GenericArguments<A>;

    fn into_other_model<T: Model>(self) -> Self::This<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LocalRegion: Into<T::LocalRegion>,
        S::ForallRegion: Into<T::ForallRegion>,
    {
        GenericArguments {
            regions: self
                .regions
                .into_iter()
                .map(Entity::into_other_model)
                .collect(),
            types: self
                .types
                .into_iter()
                .map(Entity::into_other_model)
                .collect(),
            constants: self
                .constants
                .into_iter()
                .map(Entity::into_other_model)
                .collect(),
        }
    }

    fn apply(&mut self, substitution: &Substitution<S>) {
        for region in &mut self.regions {
            region.apply(substitution);
        }

        for constant in &mut self.constants {
            constant.apply(substitution);
        }

        for ty in &mut self.types {
            ty.apply(substitution);
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::This<T>>
    where
        <S as Model>::ConstantInference: TryInto<T::ConstantInference>,
        <S as Model>::TypeInference: TryInto<T::TypeInference>,
        <S as Model>::LocalRegion: TryInto<T::LocalRegion>,
        <S as Model>::ForallRegion: TryInto<T::ForallRegion>,
    {
        let mut regions = Vec::with_capacity(self.regions.len());
        let mut types = Vec::with_capacity(self.types.len());
        let mut constants = Vec::with_capacity(self.constants.len());

        for region in self.regions {
            regions.push(region.try_into_other_model()?);
        }

        for ty in self.types {
            types.push(ty.try_into_other_model()?);
        }

        for constant in self.constants {
            constants.push(constant.try_into_other_model()?);
        }

        Some(GenericArguments {
            regions,
            types,
            constants,
        })
    }
}
