//! Contains the code related to applying substitutions to terms.

use std::{
    collections::{btree_map::Entry, BTreeMap},
    hash::Hash,
};

use pernixc_arena::ID;
use pernixc_table::{GlobalID, MemberID};

use super::{
    sub_term::TermLocation,
    visitor::{self, MutableRecursive},
};
use crate::{
    constant::Constant,
    generic_arguments::GenericArguments,
    generic_parameter::{
        ConstantParameter, ConstantParameterID, GenericKind, GenericParameters,
        LifetimeParameter, LifetimeParameterID, TypeParameter, TypeParameterID,
    },
    lifetime::Lifetime,
    r#type::Type,
    Model, ModelOf,
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

impl<'a, T: Element + Clone + Ord> MutableRecursive<T>
    for Instantiater<'a, T::Model>
{
    fn visit(
        &mut self,
        term: &mut T,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if let Some(substitution) = T::get(self.substitution).get(term) {
            *term = substitution.clone();
        }

        true
    }
}

/// Applies the given substitution to the term.
pub fn instantiate<T: Element + Clone + visitor::Element>(
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
    pub generic_id: GlobalID,

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
    fn append_from_arguments<T: Element<Model = M> + Ord, U>(
        &mut self,
        terms: impl Iterator<Item = T>,
        term_parameter_order: impl Iterator<Item = ID<U>>,
        generic_id: GlobalID,
        to_error: impl Fn(ID<U>, T) -> Collision<M>,
        collisions: &mut Vec<Collision<M>>,
    ) where
        MemberID<ID<U>>: Into<T>,
    {
        for (term, term_id) in terms.zip(term_parameter_order) {
            let parameter_id = MemberID { parent: generic_id, id: term_id };

            match T::get_mut(self).entry(parameter_id.into()) {
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
        generic_id: GlobalID,
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
        global_id: GlobalID,
        parameters: &GenericParameters,
    ) -> Result<GenericArguments<M>, MissingInstantiationError> {
        Ok(GenericArguments {
            lifetimes: parameters
                .lifetime_order()
                .iter()
                .copied()
                .map(|x| {
                    let lifetime_parameter =
                        Lifetime::Parameter(LifetimeParameterID {
                            parent: global_id,
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
                        parent: global_id,
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
                            parent: global_id,
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
        global_id: GlobalID,
        generic_parameters: &GenericParameters,
    ) -> Result<Self, MismatchedGenericArgumentCountError<M>> {
        let mut substitution = Self::default();

        let collisions = substitution.append_from_generic_arguments(
            generic_arguments,
            global_id,
            generic_parameters,
        )?;
        assert!(collisions.is_empty());

        Ok(substitution)
    }
}

/// A trait for retrieving the instantiation map from the [`Instantiation`]
/// struct.
#[allow(missing_docs)]
pub trait Element: ModelOf {
    fn get(instantiation: &Instantiation<Self::Model>) -> &BTreeMap<Self, Self>
    where
        Self: Sized;

    fn get_mut(
        instantiation: &mut Instantiation<Self::Model>,
    ) -> &mut BTreeMap<Self, Self>
    where
        Self: Sized;
}

impl<M: Model> Element for Lifetime<M> {
    fn get(instantiation: &Instantiation<M>) -> &BTreeMap<Self, Self> {
        &instantiation.lifetimes
    }

    fn get_mut(
        instantiation: &mut Instantiation<M>,
    ) -> &mut BTreeMap<Self, Self> {
        &mut instantiation.lifetimes
    }
}

impl<M: Model> Element for Type<M> {
    fn get(instantiation: &Instantiation<M>) -> &BTreeMap<Self, Self> {
        &instantiation.types
    }

    fn get_mut(
        instantiation: &mut Instantiation<M>,
    ) -> &mut BTreeMap<Self, Self> {
        &mut instantiation.types
    }
}

impl<M: Model> Element for Constant<M> {
    fn get(instantiation: &Instantiation<M>) -> &BTreeMap<Self, Self> {
        &instantiation.constants
    }

    fn get_mut(
        instantiation: &mut Instantiation<M>,
    ) -> &mut BTreeMap<Self, Self> {
        &mut instantiation.constants
    }
}

impl<M: Model> GenericArguments<M> {
    /// Applies the instantiation to all the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation<M>) {
        for lifetime in &mut self.lifetimes {
            instantiate(lifetime, instantiation);
        }

        for r#type in &mut self.types {
            instantiate(r#type, instantiation);
        }

        for constant in &mut self.constants {
            instantiate(constant, instantiation);
        }
    }
}
