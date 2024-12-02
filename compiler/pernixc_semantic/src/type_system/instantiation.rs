//! Contains the code related to applying substitutions to terms.

use std::{
    collections::{btree_map::Entry, BTreeMap},
    hash::Hash,
};

use super::{
    model::Model,
    sub_term::TermLocation,
    term::{
        constant::Constant,
        lifetime::{self, Lifetime},
        r#type::Type,
        GenericArguments, Term,
    },
    visitor::{self, MutableRecursive},
};
use crate::{
    arena::ID,
    symbol::{
        ConstantParameter, ConstantParameterID, GenericID, GenericKind,
        GenericParameters, LifetimeParameter, LifetimeParameterID, MemberID,
        TypeParameter, TypeParameterID,
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
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error(
    "the number of generic arguments supplied does not match the number of \
     generic parameters"
)]
pub struct MismatchedGenericArgumentCountError<M: Model> {
    /// The generic ID that the generic arguments were supplied for.
    pub generic_id: GenericID,

    /// The number of generic parameters expected.
    pub expected: usize,

    /// The number of generic arguments supplied.
    pub found: usize,

    /// The kind of the generic parameters that the generic arguments were
    /// supplied for.
    pub kind: GenericKind,

    /// The generic arguments passed into the function.
    pub generic_arguments: GenericArguments<M>,
}

/**
 * Error that occurs when creating a [`GenericArguments`] from an
 * [`Instantiation`] but a parameter is missing.
 */
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    thiserror::Error,
    displaydoc::Display,
)]
pub enum MissingInstantiationError {
    /// The lifetime parameter with the given ID is missing.
    Lifetime(ID<LifetimeParameter>),

    /// The type parameter with the given ID is missing.
    Type(ID<TypeParameter>),

    /// The constant parameter with the given ID is missing.
    Constant(ID<ConstantParameter>),
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
    pub fn append_from_generic_arguments(
        &mut self,
        generic_arguments: GenericArguments<M>,
        generic_id: GenericID,
        generic_parameters: &GenericParameters,
    ) -> Result<Vec<Collision<M>>, MismatchedGenericArgumentCountError<M>> {
        if generic_arguments.types.len()
            != generic_parameters.type_order().len()
        {
            return Err(MismatchedGenericArgumentCountError {
                generic_id,
                expected: generic_parameters.type_order().len(),
                found: generic_arguments.types.len(),
                kind: GenericKind::Type,
                generic_arguments,
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
                generic_arguments,
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
                generic_arguments,
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

    /// Converts the instantiation into a [`GenericArguments`].
    ///
    /// The function will search for the corresponding instantiation for each
    /// generic parameter.
    ///
    /// # Errors
    ///
    /// See [`MissingInstantiationError`] for the possible errors.
    pub fn create_generic_arguments(
        &self,
        generic_id: GenericID,
        parameters: &GenericParameters,
    ) -> Result<GenericArguments<M>, MissingInstantiationError> {
        Ok(GenericArguments {
            lifetimes: parameters
                .lifetime_order()
                .iter()
                .copied()
                .map(|x| {
                    let lifetime_parameter =
                        lifetime::Lifetime::Parameter(LifetimeParameterID {
                            parent: generic_id,
                            id: x,
                        });

                    let inst = self
                        .lifetimes
                        .get(&lifetime_parameter)
                        .cloned()
                        .ok_or(MissingInstantiationError::Lifetime(x))?;

                    Ok(inst)
                })
                .collect::<Result<_, _>>()?,
            types: parameters
                .type_order()
                .iter()
                .copied()
                .map(|x| {
                    let type_parameter = Type::Parameter(TypeParameterID {
                        parent: generic_id,
                        id: x,
                    });

                    let inst = self
                        .types
                        .get(&type_parameter)
                        .cloned()
                        .ok_or(MissingInstantiationError::Type(x))?;

                    Ok(inst)
                })
                .collect::<Result<_, _>>()?,
            constants: parameters
                .constant_order()
                .iter()
                .copied()
                .map(|x| {
                    let constant_parameter =
                        Constant::Parameter(ConstantParameterID {
                            parent: generic_id,
                            id: x,
                        });

                    let inst = self
                        .constants
                        .get(&constant_parameter)
                        .cloned()
                        .ok_or(MissingInstantiationError::Constant(x))?;

                    Ok(inst)
                })
                .collect::<Result<_, _>>()?,
        })
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
    ) -> Result<Self, MismatchedGenericArgumentCountError<M>> {
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
