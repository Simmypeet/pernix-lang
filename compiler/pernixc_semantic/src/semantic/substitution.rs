//! Contains the code related to applying substitutions to terms.

use std::collections::HashMap;

use super::term::{
    constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments, Tuple, TupleElement,
};
use crate::{
    arena::ID,
    symbol::{ConstantParameterID, GenericID, LifetimeParameterID, TypeParameterID},
};

/// Represents a substitution of terms.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Substitution {
    pub types: HashMap<Type, Type>,
    pub constants: HashMap<Constant, Constant>,
    pub lifetimes: HashMap<Lifetime, Lifetime>,
}

/// Represents a substitution of terms.
pub trait Substitute: Sized {
    /// Applies the given substitution to the term.
    fn apply(&mut self, substitution: &Substitution);

    /// Gets the substitution map of this term kind.
    fn get(substitution: &Substitution) -> &HashMap<Self, Self>;

    /// Gets the mutable reference to the substitution map of this term kind.
    fn get_mut(substitution: &mut Substitution) -> &mut HashMap<Self, Self>;
}

fn tuple_apply<T: Substitute + Clone>(tuple: &mut Tuple<T>, substitution: &Substitution)
where
    Tuple<T>: TryFrom<T, Error = T> + Into<T>,
{
    for element in &mut tuple.elements {
        match element {
            TupleElement::Regular(term) | TupleElement::Unpacked(term) => term.apply(substitution),
        }
    }
}

impl Substitute for Type {
    fn apply(&mut self, substitution: &Substitution) {
        if let Some(replacement) = substitution.types.get(self) {
            *self = replacement.clone();
            return;
        }

        match self {
            Self::Parameter(_) | Self::Primitive(_) | Self::Inference(_) => {}

            Self::Symbol(adt) => {
                apply_generic_arguments(&mut adt.generic_arguments, substitution);
            }

            Self::MemberSymbol(adt) => {
                apply_generic_arguments(&mut adt.member_generic_arguments, substitution);
                apply_generic_arguments(&mut adt.parent_generic_arguments, substitution);
            }

            Self::Pointer(pointer) => {
                pointer.pointee.apply(substitution);
            }

            Self::Reference(reference) => {
                reference.lifetime.apply(substitution);
                reference.pointee.apply(substitution);
            }

            Self::Array(array) => {
                array.r#type.apply(substitution);
                array.length.apply(substitution);
            }

            Self::Tuple(tuple_element) => tuple_apply(tuple_element, substitution),
            Self::Local(local) => local.0.apply(substitution),
        }
    }

    fn get(substitution: &Substitution) -> &HashMap<Self, Self> { &substitution.types }

    fn get_mut(substitution: &mut Substitution) -> &mut HashMap<Self, Self> {
        &mut substitution.types
    }
}

impl Substitute for Constant {
    fn apply(&mut self, substitution: &Substitution) {
        if let Some(replacement) = substitution.constants.get(self) {
            *self = replacement.clone();
            return;
        }

        match self {
            Self::Primitive(_) | Self::Inference(_) | Self::Parameter(_) => {}

            Self::Struct(value) => {
                for field in &mut value.fields {
                    field.apply(substitution);
                }
            }
            Self::Enum(value) => {
                if let Some(variant) = &mut value.associated_value {
                    variant.apply(substitution);
                }
            }
            Self::Array(arraay) => {
                for element in &mut arraay.elements {
                    element.apply(substitution);
                }
            }

            Self::Local(local) => local.0.apply(substitution),
            Self::Tuple(tuple) => tuple_apply(tuple, substitution),

            Self::Symbol(symbol) => {
                apply_generic_arguments(&mut symbol.generic_arguments, substitution);
            }

            Self::MemberSymbol(symbol) => {
                apply_generic_arguments(&mut symbol.member_generic_arguments, substitution);
                apply_generic_arguments(&mut symbol.parent_generic_arguments, substitution);
            }
        }
    }

    fn get(substitution: &Substitution) -> &HashMap<Self, Self> { &substitution.constants }

    fn get_mut(substitution: &mut Substitution) -> &mut HashMap<Self, Self> {
        &mut substitution.constants
    }
}

impl Substitute for Lifetime {
    fn apply(&mut self, substitution: &Substitution) {
        if let Some(replacement) = substitution.lifetimes.get(self) {
            *self = *replacement;
        }
    }

    fn get(substitution: &Substitution) -> &HashMap<Self, Self> { &substitution.lifetimes }

    fn get_mut(substitution: &mut Substitution) -> &mut HashMap<Self, Self> {
        &mut substitution.lifetimes
    }
}

fn apply_generic_arguments(generic_arguments: &mut GenericArguments, substitution: &Substitution) {
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

impl Substitution {
    /// Appends the given generic arguments as a substitution.
    ///
    /// # Returns
    ///
    /// Returns `None` if the substitution already contains the given generic arguments.
    #[must_use]
    pub fn append_from_generic_arguments(
        mut self,
        generic_arguments: GenericArguments,
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

        for (index, term) in generic_arguments.lifetimes.into_iter().enumerate() {
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
        generic_arguments: GenericArguments,
        generic_id: GenericID,
    ) -> Self {
        Self::default()
            .append_from_generic_arguments(generic_arguments, generic_id)
            .unwrap()
    }
}
