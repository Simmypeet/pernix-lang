//! Contains the code related to applying substitutions to terms.

use std::{
    collections::{hash_map::Entry, HashMap},
    hash::Hash,
};

use super::{
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments,
        Term,
    },
    visitor::{self, Visitor},
};
use crate::{
    arena::ID,
    symbol::{
        ConstantParameter, GenericID, GenericParameters, LifetimeParameter,
        MemberID, TypeParameter,
    },
};

/// Represents an instantiation of generic parameters.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Instantiation {
    pub lifetimes: HashMap<Lifetime, Lifetime>,
    pub types: HashMap<Type, Type>,
    pub constants: HashMap<Constant, Constant>,
}

struct Instantiater<'a> {
    substitution: &'a Instantiation,
}

impl<'a> Visitor for Instantiater<'a> {
    fn visit_type(&mut self, _: &Type) -> bool { unreachable!() }

    fn visit_lifetime(&mut self, _: &Lifetime) -> bool { unreachable!() }

    fn visit_constant(&mut self, _t: &Constant) -> bool { unreachable!() }

    fn visit_type_mut(&mut self, ty: &mut Type) -> bool {
        if let Some(substitution) = self.substitution.types.get(ty) {
            *ty = substitution.clone();
        }

        true
    }

    fn visit_lifetime_mut(&mut self, lifetime: &mut Lifetime) -> bool {
        if let Some(substitution) = self.substitution.lifetimes.get(lifetime) {
            *lifetime = *substitution;
        }

        true
    }

    fn visit_constant_mut(&mut self, constant: &mut Constant) -> bool {
        if let Some(substitution) = self.substitution.constants.get(constant) {
            *constant = substitution.clone();
        }

        true
    }
}

/// Applies the given substitution to the term.
pub fn instantiate(term: &mut impl Term, instantiation: &Instantiation) {
    let mut instantiater = Instantiater { substitution: instantiation };
    visitor::accept_recursive_mut(term, &mut instantiater);
}

/// Error that occurs when converting a [`GenericArguments`] into a
/// [`Substitution`] but the number of generic arguments supplied does not match
/// the number of generic parameters.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error(
    "the number of generic arguments supplied does not match the number of \
     generic parameters"
)]
pub struct MismatchedGenericParameterCount;

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
    MismatchedGenericParameterCount(MismatchedGenericParameterCount),
}

impl Instantiation {
    fn append_from_arguments<
        T: Term + From<MemberID<ID<T::GenericParameter>, GenericID>>,
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
        generic_arguments: GenericArguments,
        generic_id: GenericID,
        generic_parameters: &GenericParameters,
    ) -> Result<(), FromGenericArgumentsError> {
        if generic_arguments.types.len() != generic_parameters.type_order.len()
            || generic_arguments.lifetimes.len()
                != generic_parameters.lifetime_order.len()
            || generic_arguments.constants.len()
                != generic_parameters.constant_order.len()
        {
            return Err(
                FromGenericArgumentsError::MismatchedGenericParameterCount(
                    MismatchedGenericParameterCount,
                ),
            );
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
