//! Contains the code related to applying substitutions to terms.

use std::{
    collections::{hash_map::Entry, HashMap},
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
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Instantiation<M: Model> {
    pub lifetimes: HashMap<Lifetime<M>, Lifetime<M>>,
    pub types: HashMap<Type<M>, Type<M>>,
    pub constants: HashMap<Constant<M>, Constant<M>>,
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

/// An error that occurs when converting generic arguments into a substitution.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum FromGenericArgumentsError {
    #[error("lifetime parameter collision with {0:?}")]
    LifetimeParameterCollision(ID<LifetimeParameter>),
    #[error("type parameter collision with {0:?}")]
    TypeParameterCollision(ID<TypeParameter>),
    #[error("constant parameter collision with {0:?}")]
    ConstantParameterCollision(ID<ConstantParameter>),
    #[error(transparent)]
    MismatchedGenericParameterCount(MismatchedGenericArgumentCountError),
}

impl<M: Model> Instantiation<M> {
    fn append_from_arguments<
        T: Term<Model = M> + From<MemberID<ID<T::GenericParameter>, GenericID>>,
    >(
        &mut self,
        terms: impl Iterator<Item = T>,
        term_parameter_order: impl Iterator<Item = ID<T::GenericParameter>>,
        generic_id: GenericID,
        to_error: impl Fn(ID<T::GenericParameter>) -> FromGenericArgumentsError,
    ) -> Result<(), FromGenericArgumentsError> {
        for (term, term_id) in terms.zip(term_parameter_order) {
            let parameter_id = MemberID { parent: generic_id, id: term_id };

            match T::get_instantiation_mut(self).entry(parameter_id.into()) {
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
    /// See [`FromGenericArgumentsError`]. If an error occurs, the substitution
    /// might be partially modified.
    pub fn append_from_generic_arguments(
        &mut self,
        generic_arguments: GenericArguments<M>,
        generic_id: GenericID,
        generic_parameters: &GenericParameters,
    ) -> Result<(), FromGenericArgumentsError> {
        if generic_arguments.types.len()
            != generic_parameters.type_order().len()
        {
            return Err(
                FromGenericArgumentsError::MismatchedGenericParameterCount(
                    MismatchedGenericArgumentCountError {
                        generic_id,
                        expected: generic_parameters.type_order().len(),
                        found: generic_arguments.types.len(),
                        kind: GenericKind::Type,
                    },
                ),
            );
        }

        if generic_arguments.lifetimes.len()
            != generic_parameters.lifetime_order().len()
        {
            return Err(
                FromGenericArgumentsError::MismatchedGenericParameterCount(
                    MismatchedGenericArgumentCountError {
                        generic_id,
                        expected: generic_parameters.lifetime_order().len(),
                        found: generic_arguments.lifetimes.len(),
                        kind: GenericKind::Lifetime,
                    },
                ),
            );
        }

        if generic_arguments.constants.len()
            != generic_parameters.constant_order().len()
        {
            return Err(
                FromGenericArgumentsError::MismatchedGenericParameterCount(
                    MismatchedGenericArgumentCountError {
                        generic_id,
                        expected: generic_parameters.constant_order().len(),
                        found: generic_arguments.constants.len(),
                        kind: GenericKind::Constant,
                    },
                ),
            );
        }

        self.append_from_arguments(
            generic_arguments.lifetimes.into_iter(),
            generic_parameters.lifetime_order().iter().copied(),
            generic_id,
            FromGenericArgumentsError::LifetimeParameterCollision,
        )?;

        self.append_from_arguments(
            generic_arguments.types.into_iter(),
            generic_parameters.type_order().iter().copied(),
            generic_id,
            FromGenericArgumentsError::TypeParameterCollision,
        )?;

        self.append_from_arguments(
            generic_arguments.constants.into_iter(),
            generic_parameters.constant_order().iter().copied(),
            generic_id,
            FromGenericArgumentsError::ConstantParameterCollision,
        )?;

        Ok(())
    }

    /// Converts the given generic arguments into a substitution.
    ///
    /// # Errors
    pub fn from_generic_arguments(
        generic_arguments: GenericArguments<M>,
        generic_id: GenericID,
        generic_parameters: &GenericParameters,
    ) -> Result<Self, MismatchedGenericArgumentCountError> {
        let mut substitution = Self::default();

        substitution
            .append_from_generic_arguments(
                generic_arguments,
                generic_id,
                generic_parameters,
            )
            .map_err(|x| match x {
                FromGenericArgumentsError::MismatchedGenericParameterCount(
                    x,
                ) => x,
                _ => unreachable!(),
            })?;

        Ok(substitution)
    }
}
