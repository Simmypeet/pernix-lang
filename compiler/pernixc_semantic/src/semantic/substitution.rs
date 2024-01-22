//! Contains the code related to applying substitutions to terms.

use std::{
    collections::{hash_map::Entry, HashMap},
    hash::Hash,
};

use super::term::{
    constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments, Tuple, TupleElement,
};
use crate::{
    arena::ID,
    symbol::{
        ConstantParameter, GenericID, GenericParameters, LifetimeParameter, MemberID, TypeParameter,
    },
};

/// Represents a substitution of terms.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Substitution {
    pub lifetimes: HashMap<Lifetime, Lifetime>,
    pub types: HashMap<Type, Type>,
    pub constants: HashMap<Constant, Constant>,
}

/// Represents a substitution of terms.
pub trait Substitute: Sized + Eq + Hash {
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
            Self::Array(array) => {
                for element in &mut array.elements {
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

/// Error that occurs when converting a [`GenericArguments`] into a [`Substitution`] but the
/// number of generic arguments supplied does not match the number of generic parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[error("the number of generic arguments supplied does not match the number of generic parameters")]
pub struct MismatchedGenericParameterCount;

/// An error that occurs when converting generic arguments into a substitution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[allow(missing_docs)]
pub enum FromGenericArgumentsError {
    #[error("lifetime parameter collision with {0:?}")]
    LifetimeParameterCollision(ID<LifetimeParameter>),
    #[error("type parameter collision with {0:?}")]
    TypeParameterCollision(ID<TypeParameter>),
    #[error("constant parameter collision with {0:?}")]
    ConstantParameterCollision(ID<ConstantParameter>),
    #[error(transparent)]
    MismatchedGenericParameterCount(MismatchedGenericParameterCount),
}

impl Substitution {
    fn append_from_arguments<T: Substitute, U: Copy>(
        &mut self,
        terms: impl Iterator<Item = T>,
        term_parameter_order: impl Iterator<Item = U>,
        generic_id: GenericID,
        to_error: impl Fn(U) -> FromGenericArgumentsError,
    ) -> Result<(), FromGenericArgumentsError>
    where
        MemberID<U, GenericID>: Into<T>,
    {
        for (term, term_id) in terms.zip(term_parameter_order) {
            let parameter_id = MemberID {
                parent: generic_id,
                id: term_id,
            };

            match <T as Substitute>::get_mut(self).entry(parameter_id.into()) {
                Entry::Occupied(..) => {
                    return Err(to_error(term_id));
                }
                Entry::Vacant(entry) => {
                    entry.insert(term);
                }
            }
        }

        Ok(())
    }

    /// Appends the given generic arguments as a substitution.
    ///
    /// # Errors
    ///
    /// See [`FromGenericArgumentsError`]. If an error occurs, the substitution might be partially
    /// modified.
    pub fn append_from_generic_arguments(
        &mut self,
        generic_arguments: GenericArguments,
        generic_id: GenericID,
        generic_parameters: &GenericParameters,
    ) -> Result<(), FromGenericArgumentsError> {
        if generic_arguments.types.len() != generic_parameters.type_order.len()
            || generic_arguments.lifetimes.len() != generic_parameters.lifetime_order.len()
            || generic_arguments.constants.len() != generic_parameters.constant_order.len()
        {
            return Err(FromGenericArgumentsError::MismatchedGenericParameterCount(
                MismatchedGenericParameterCount,
            ));
        }

        self.append_from_arguments(
            generic_arguments.lifetimes.into_iter(),
            generic_parameters.lifetime_order.iter().copied(),
            generic_id,
            FromGenericArgumentsError::LifetimeParameterCollision,
        )?;

        self.append_from_arguments(
            generic_arguments.types.into_iter(),
            generic_parameters.type_order.iter().copied(),
            generic_id,
            FromGenericArgumentsError::TypeParameterCollision,
        )?;

        self.append_from_arguments(
            generic_arguments.constants.into_iter(),
            generic_parameters.constant_order.iter().copied(),
            generic_id,
            FromGenericArgumentsError::ConstantParameterCollision,
        )?;

        Ok(())
    }

    /// Converts the given generic arguments into a substitution.
    ///
    /// # Errors
    pub fn from_generic_arguments(
        generic_arguments: GenericArguments,
        generic_id: GenericID,
        generic_parameters: &GenericParameters,
    ) -> Result<Self, MismatchedGenericParameterCount> {
        let mut substitution = Self::default();

        substitution
            .append_from_generic_arguments(generic_arguments, generic_id, generic_parameters)
            .map_err(|x| match x {
                FromGenericArgumentsError::MismatchedGenericParameterCount(x) => x,
                _ => unreachable!(),
            })?;

        Ok(substitution)
    }
}
