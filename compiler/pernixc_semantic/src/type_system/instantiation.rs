//! Contains the code related to applying substitutions to terms.

use std::{
    collections::{btree_map::Entry, BTreeMap},
    hash::Hash,
};

use super::{
    model::Model,
    sub_term::TermLocation,
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments,
        Term,
    },
    visitor::{self, MutableRecursive},
};
use crate::{
    arena::ID,
    symbol::{
        ConstantParameter, GenericID, GenericKind, GenericParameters,
        LifetimeParameter, MemberID, TypeParameter,
    },
};

/// Represents an instantiation of generic parameters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[allow(missing_docs)]
pub struct Instantiation<M: Model> {
    pub lifetimes: BTreeMap<Lifetime<M>, Lifetime<M>>,
    pub types: BTreeMap<Type<M>, Type<M>>,
    pub constants: BTreeMap<Constant<M>, Constant<M>>,
}

struct Instantiater<'a, M: Model> {
    substitution: &'a Instantiation<M>,
}

impl<'a, T: Term> MutableRecursive<T> for Instantiater<'a, T::Model> {
    fn visit(
        &mut self,
        term: &mut T,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if let Some(substitution) =
            T::get_instantiation(self.substitution).get(term)
        {
            *term = substitution.clone();
        }

        true
    }
}

/// Applies the given substitution to the term.
pub fn instantiate<T: Term>(
    term: &mut T,
    instantiation: &Instantiation<T::Model>,
) {
    let mut instantiater = Instantiater { substitution: instantiation };
    visitor::accept_recursive_mut(term, &mut instantiater);
}

/// Error that occurs when converting a [`GenericArguments`] into a
/// [`Instantiation`], the number of generic arguments supplied does not
/// match the number of generic parameters.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error(
    "the number of generic arguments supplied does not match the number of \
     generic parameters"
)]
pub struct MismatchedGenericArgumentCountError {
    /// The generic ID that the generic arguments were supplied for.
    pub generic_id: GenericID,

    /// The number of generic parameters expected.
    pub expected: usize,

    /// The number of generic arguments supplied.
    pub found: usize,

    /// The kind of the generic parameters that the generic arguments were
    /// supplied for.
    pub kind: GenericKind,
}

/// Returns with [`Instantiation::append_from_arguments`] if the given
/// parameter ID is already occupied with a value.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum Collision<M: Model> {
    LifetimeParameterCollision {
        lifetime_parameter_id: ID<LifetimeParameter>,
        new_lifetime: Lifetime<M>,
    },
    TypeParameterCollision {
        type_parameter_id: ID<TypeParameter>,
        new_type: Type<M>,
    },
    ConstantParameterCollision {
        constant_parameter_id: ID<ConstantParameter>,
        new_constant: Constant<M>,
    },
}

impl<M: Model> Instantiation<M> {
    fn append_from_arguments<
        T: Term<Model = M> + From<MemberID<ID<T::GenericParameter>, GenericID>>,
    >(
        &mut self,
        terms: impl Iterator<Item = T>,
        term_parameter_order: impl Iterator<Item = ID<T::GenericParameter>>,
        generic_id: GenericID,
        to_error: impl Fn(ID<T::GenericParameter>, T) -> Collision<M>,
        collisions: &mut Vec<Collision<M>>,
    ) {
        for (term, term_id) in terms.zip(term_parameter_order) {
            let parameter_id = MemberID { parent: generic_id, id: term_id };

            match T::get_instantiation_mut(self).entry(parameter_id.into()) {
                Entry::Occupied(..) => {
                    collisions.push(to_error(term_id, term));
                }
                Entry::Vacant(entry) => {
                    entry.insert(term);
                }
            }
        }
    }

    /// Appends the given generic arguments as a substitution.
    ///
    /// If there's any collision, the prior value will be preserved and the new
    /// value will be collected in a list of collisions returned by this
    /// function.
    ///
    /// # Errors
    ///
    /// See [`MismatchedGenericArgumentCountError`].
    ///
    /// # Returns
    ///
    /// A list of collisions that occurred during the substitution.
    #[must_use]
    pub fn append_from_generic_arguments(
        &mut self,
        generic_arguments: GenericArguments<M>,
        generic_id: GenericID,
        generic_parameters: &GenericParameters,
    ) -> Result<Vec<Collision<M>>, MismatchedGenericArgumentCountError> {
        if generic_arguments.types.len()
            != generic_parameters.type_order().len()
        {
            return Err(MismatchedGenericArgumentCountError {
                generic_id,
                expected: generic_parameters.type_order().len(),
                found: generic_arguments.types.len(),
                kind: GenericKind::Type,
            });
        }

        if generic_arguments.lifetimes.len()
            != generic_parameters.lifetime_order().len()
        {
            return Err(MismatchedGenericArgumentCountError {
                generic_id,
                expected: generic_parameters.lifetime_order().len(),
                found: generic_arguments.lifetimes.len(),
                kind: GenericKind::Lifetime,
            });
        }

        if generic_arguments.constants.len()
            != generic_parameters.constant_order().len()
        {
            return Err(MismatchedGenericArgumentCountError {
                generic_id,
                expected: generic_parameters.constant_order().len(),
                found: generic_arguments.constants.len(),
                kind: GenericKind::Constant,
            });
        }

        let mut collisions = Vec::new();
        self.append_from_arguments(
            generic_arguments.lifetimes.into_iter(),
            generic_parameters.lifetime_order().iter().copied(),
            generic_id,
            |id, term| Collision::LifetimeParameterCollision {
                lifetime_parameter_id: id,
                new_lifetime: term,
            },
            &mut collisions,
        );

        self.append_from_arguments(
            generic_arguments.types.into_iter(),
            generic_parameters.type_order().iter().copied(),
            generic_id,
            |id, term| Collision::TypeParameterCollision {
                type_parameter_id: id,
                new_type: term,
            },
            &mut collisions,
        );

        self.append_from_arguments(
            generic_arguments.constants.into_iter(),
            generic_parameters.constant_order().iter().copied(),
            generic_id,
            |id, term| Collision::ConstantParameterCollision {
                constant_parameter_id: id,
                new_constant: term,
            },
            &mut collisions,
        );

        Ok(collisions)
    }

    /// Converts the given generic arguments into a substitution.
    ///
    /// # Errors
    ///
    /// See [`MismatchedGenericArgumentCountError`].
    pub fn from_generic_arguments(
        generic_arguments: GenericArguments<M>,
        generic_id: GenericID,
        generic_parameters: &GenericParameters,
    ) -> Result<Self, MismatchedGenericArgumentCountError> {
        let mut substitution = Self::default();

        let collisions = substitution.append_from_generic_arguments(
            generic_arguments,
            generic_id,
            generic_parameters,
        )?;
        assert!(collisions.is_empty());

        Ok(substitution)
    }
}
