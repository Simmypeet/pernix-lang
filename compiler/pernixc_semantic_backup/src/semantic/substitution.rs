//! Contains the code related to applying substitutions to terms.

use std::collections::HashMap;

use super::model::{Entity, Model};
use crate::{
    arena::ID,
    semantic::term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments,
        Tuple, TupleElement, Unpacked,
    },
    symbol::{
        ConstantParameterID, GenericID, LifetimeParameterID, TypeParameterID,
    },
};

/// Represents a substitution of terms.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Substitution<S: Model> {
    pub types: HashMap<Type<S>, Type<S>>,
    pub constants: HashMap<Constant<S>, Constant<S>>,
    pub lifetimes: HashMap<Lifetime<S>, Lifetime<S>>,
}

impl<S: Model> Entity for Substitution<S> {
    type Model = S;
    type Rebind<A: Model> = Substitution<A>;

    fn into_other_model<T: Model>(self) -> Self::Rebind<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LifetimeInference: Into<T::LifetimeInference>,
        S::ScopedLifetime: Into<T::ScopedLifetime>,
    {
        Substitution {
            types: self
                .types
                .into_iter()
                .map(|(lhs, rhs)| {
                    (lhs.into_other_model(), rhs.into_other_model())
                })
                .collect(),
            constants: self
                .constants
                .into_iter()
                .map(|(lhs, rhs)| {
                    (lhs.into_other_model(), rhs.into_other_model())
                })
                .collect(),
            lifetimes: self
                .lifetimes
                .into_iter()
                .map(|(lhs, rhs)| {
                    (lhs.into_other_model(), rhs.into_other_model())
                })
                .collect(),
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::Rebind<T>>
    where
        S::ConstantInference: TryInto<T::ConstantInference>,
        S::TypeInference: TryInto<T::TypeInference>,
        S::LifetimeInference: TryInto<T::LifetimeInference>,
        S::ScopedLifetime: TryInto<T::ScopedLifetime>,
    {
        Some(Substitution {
            types: self
                .types
                .into_iter()
                .map(|(lhs, rhs)| {
                    Some((
                        lhs.try_into_other_model()?,
                        rhs.try_into_other_model()?,
                    ))
                })
                .collect::<Option<_>>()?,
            constants: self
                .constants
                .into_iter()
                .map(|(lhs, rhs)| {
                    Some((
                        lhs.try_into_other_model()?,
                        rhs.try_into_other_model()?,
                    ))
                })
                .collect::<Option<_>>()?,
            lifetimes: self
                .lifetimes
                .into_iter()
                .map(|(lhs, rhs)| {
                    Some((
                        lhs.try_into_other_model()?,
                        rhs.try_into_other_model()?,
                    ))
                })
                .collect::<Option<_>>()?,
        })
    }
}

/// Represents a substitution of terms.
pub trait Substitute: Sized {
    /// The model of the substitution.
    type Model: Model;

    /// Applies the given substitution to the term.
    fn apply(&mut self, substitution: &Substitution<Self::Model>);

    /// Gets the substitution map of this term kind.
    fn get(substitution: &Substitution<Self::Model>) -> &HashMap<Self, Self>;

    /// Gets the mutable reference to the substitution map of this term kind.
    fn get_mut(
        substitution: &mut Substitution<Self::Model>,
    ) -> &mut HashMap<Self, Self>;
}

fn tuple_apply<S: Model, T, TraitMember, Parameter>(
    tuple: &mut Tuple<T, Parameter, TraitMember>,
    substitution: &Substitution<S>,
) where
    Tuple<T, Parameter, TraitMember>: TryFrom<T, Error = T>,
    T: Substitute<Model = S>
        + From<Parameter>
        + From<TraitMember>
        + From<Tuple<T, Parameter, TraitMember>>,
    TraitMember: Clone + TryFrom<T, Error = T>,
    Parameter: Clone + TryFrom<T, Error = T>,
{
    if tuple.elements.iter().filter(|x| x.as_unpacked().is_some()).count() == 0
    {
        for element in &mut tuple.elements {
            element.as_regular_mut().unwrap().apply(substitution);
        }
    } else {
        let mut index = 0;
        while index < tuple.elements.len() {
            match &mut tuple.elements[index] {
                TupleElement::Regular(regular) => {
                    regular.apply(substitution);
                }
                TupleElement::Unpacked(unpacked) => {
                    let mut new_unpacked = match unpacked {
                        Unpacked::Parameter(parameter) => {
                            T::from(parameter.clone())
                        }
                        Unpacked::TraitMember(trait_member) => {
                            T::from(trait_member.clone())
                        }
                    };

                    new_unpacked.apply(substitution);

                    new_unpacked = match TraitMember::try_from(new_unpacked) {
                        Ok(ok) => {
                            *unpacked = Unpacked::TraitMember(ok);
                            continue;
                        }
                        Err(unpacked) => unpacked,
                    };

                    new_unpacked = match Parameter::try_from(new_unpacked) {
                        Ok(ok) => {
                            *unpacked = Unpacked::Parameter(ok);
                            continue;
                        }
                        Err(unpacked) => unpacked,
                    };

                    match Tuple::try_from(new_unpacked) {
                        Ok(ok) => {
                            tuple.elements.remove(index);
                            tuple.elements.reserve(ok.elements.len());
                            for element in ok.elements {
                                tuple.elements.insert(index, element);
                                index += 1;
                            }
                        }
                        Err(ok) => {
                            tuple.elements[index] = TupleElement::Regular(ok);
                        }
                    }
                }
            }
        }
    }
}

impl<S: Model> Substitute for Type<S> {
    type Model = S;

    fn apply(&mut self, substitution: &Substitution<S>) {
        if let Some(replacement) = substitution.types.get(self) {
            *self = replacement.clone();
            return;
        }

        match self {
            Self::Parameter(_) | Self::Primitive(_) | Self::Inference(_) => {}

            Self::Symbol(adt) => {
                apply_generic_arguments(
                    &mut adt.generic_arguments,
                    substitution,
                );
            }

            Self::Implementation(adt) => {
                apply_generic_arguments(
                    &mut adt.member_generic_arguments,
                    substitution,
                );
                apply_generic_arguments(
                    &mut adt.parent_generic_arguments,
                    substitution,
                );
            }

            Self::Pointer(pointer) => {
                pointer.pointee.apply(substitution);
            }

            Self::Reference(reference) => {
                reference.lifetime.apply(substitution);
                reference.pointee.apply(substitution);
            }

            Self::Array(array) => {
                array.element.apply(substitution);
                array.length.apply(substitution);
            }

            Self::TraitMember(trait_member) => {
                apply_generic_arguments(
                    &mut trait_member.parent_generic_arguments,
                    substitution,
                );
                apply_generic_arguments(
                    &mut trait_member.member_generic_arguments,
                    substitution,
                );
            }

            Self::Tuple(tuple_element) => {
                tuple_apply(tuple_element, substitution)
            }
            Self::Local(local) => local.0.apply(substitution),
        }
    }

    fn get(substitution: &Substitution<S>) -> &HashMap<Self, Self> {
        &substitution.types
    }

    fn get_mut(substitution: &mut Substitution<S>) -> &mut HashMap<Self, Self> {
        &mut substitution.types
    }
}

impl<S: Model> Substitute for Constant<S> {
    type Model = S;

    fn apply(&mut self, substitution: &Substitution<S>) {
        if let Some(replacement) = substitution.constants.get(self) {
            *self = replacement.clone();
            return;
        }

        match self {
            Self::Primitive(_) | Self::Inference(_) | Self::Parameter(_) => {}

            Self::Struct(value) => {
                apply_generic_arguments(
                    &mut value.generic_arguments,
                    substitution,
                );

                for field in &mut value.fields {
                    field.apply(substitution);
                }
            }
            Self::Enum(value) => {
                apply_generic_arguments(
                    &mut value.generic_arguments,
                    substitution,
                );

                if let Some(variant) = &mut value.associated_value {
                    variant.apply(substitution);
                }
            }
            Self::Array(arraay) => {
                arraay.element_ty.apply(substitution);

                for element in &mut arraay.elements {
                    element.apply(substitution);
                }
            }
            Self::TraitMember(value) => {
                apply_generic_arguments(
                    &mut value.parent_generic_arguments,
                    substitution,
                );
            }

            Self::Local(local) => local.0.apply(substitution),
            Self::Tuple(tuple) => tuple_apply(tuple, substitution),

            Self::Symbol(symbol) => {
                symbol.generic_arguments.apply(substitution)
            }

            Self::Implementation(symbol) => {
                symbol.member_generic_arguments.apply(substitution);
                symbol.parent_generic_arguments.apply(substitution);
            }
        }
    }

    fn get(substitution: &Substitution<S>) -> &HashMap<Self, Self> {
        &substitution.constants
    }

    fn get_mut(substitution: &mut Substitution<S>) -> &mut HashMap<Self, Self> {
        &mut substitution.constants
    }
}

impl<S: Model> Substitute for Lifetime<S> {
    type Model = S;

    fn apply(&mut self, substitution: &Substitution<S>) {
        if let Some(replacement) = substitution.lifetimes.get(self) {
            *self = replacement.clone();
        }
    }

    fn get(substitution: &Substitution<S>) -> &HashMap<Self, Self> {
        &substitution.lifetimes
    }

    fn get_mut(substitution: &mut Substitution<S>) -> &mut HashMap<Self, Self> {
        &mut substitution.lifetimes
    }
}

fn apply_generic_arguments<S: Model>(
    generic_arguments: &mut GenericArguments<S>,
    substitution: &Substitution<S>,
) {
    for lifetime in &mut generic_arguments.lifetimes {
        lifetime.apply(substitution);
    }

    for ty in &mut generic_arguments.types {
        ty.apply(substitution);
    }

    for constant in &mut generic_arguments.constants {
        constant.apply(substitution);
    }
}

impl<S: Model> Substitution<S> {
    /// Appends the given generic arguments as a substitution.
    ///
    /// # Returns
    ///
    /// Returns `None` if the substitution already contains the given generic
    /// arguments.
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

        for (index, term) in generic_arguments.constants.into_iter().enumerate()
        {
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

        for (index, term) in generic_arguments.lifetimes.into_iter().enumerate()
        {
            if self
                .lifetimes
                .insert(
                    Lifetime::Parameter(LifetimeParameterID {
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
