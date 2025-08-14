//! Contains the code related to applying substitutions to terms.

use std::{collections::BTreeMap, hash::Hash};

use pernixc_arena::ID;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::MemberID;
use pernixc_target::Global;

use super::{
    sub_term::TermLocation,
    visitor::{self, MutableRecursive},
};
use crate::{
    constant::Constant,
    generic_arguments::GenericArguments,
    generic_parameters::{
        ConstantParameterID, GenericKind, GenericParameters,
        LifetimeParameterID, TypeParameterID,
    },
    lifetime::Lifetime,
    r#type::Type,
};

/// Represents an instantiation of generic parameters.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    StableHash,
    Serialize,
    Deserialize,
)]
#[allow(missing_docs)]
pub struct Instantiation {
    pub lifetimes: BTreeMap<Lifetime, Lifetime>,
    pub types: BTreeMap<Type, Type>,
    pub constants: BTreeMap<Constant, Constant>,
}

struct Instantiater<'a> {
    substitution: &'a Instantiation,
}

impl<T: Element + Clone + Ord> MutableRecursive<T> for Instantiater<'_> {
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
pub struct MismatchedGenericArgumentCountError {
    /// The generic ID that the generic arguments were supplied for.
    pub generic_id: Global<pernixc_symbol::ID>,

    /// The number of generic parameters expected.
    pub expected: usize,

    /// The number of generic arguments supplied.
    pub found: usize,

    /// The kind of the generic parameters that the generic arguments were
    /// supplied for.
    pub kind: GenericKind,

    /// The generic arguments passed into the function.
    pub generic_arguments: GenericArguments,
}

impl Instantiation {
    fn append_from_arguments<T: Element + Ord, U>(
        &mut self,
        terms: impl Iterator<Item = T>,
        term_parameter_order: impl Iterator<Item = ID<U>>,
        generic_id: Global<pernixc_symbol::ID>,
    ) where
        MemberID<ID<U>>: Into<T>,
    {
        for (term, term_id) in terms.zip(term_parameter_order) {
            let parameter_id = MemberID::new(generic_id, term_id);

            assert!(T::get_mut(self)
                .insert(parameter_id.into(), term)
                .is_none());
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
        generic_arguments: GenericArguments,
        generic_id: Global<pernixc_symbol::ID>,
        generic_parameters: &GenericParameters,
    ) -> Result<(), MismatchedGenericArgumentCountError> {
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

        self.append_from_arguments(
            generic_arguments.lifetimes.into_iter(),
            generic_parameters.lifetime_order().iter().copied(),
            generic_id,
        );

        self.append_from_arguments(
            generic_arguments.types.into_iter(),
            generic_parameters.type_order().iter().copied(),
            generic_id,
        );

        self.append_from_arguments(
            generic_arguments.constants.into_iter(),
            generic_parameters.constant_order().iter().copied(),
            generic_id,
        );

        Ok(())
    }

    /// Creates a mapping from one generic parameter to another.
    ///
    /// # Example
    ///
    /// Suppose `A[B, C, D]` and `T[U, V, W]`. To map the generic parameters
    /// of `A` to `T`, this function will create {B -> U, C -> V, D -> W}.
    pub fn append_from_generic_parameters_mapping(
        &mut self,
        from_id: Global<pernixc_symbol::ID>,
        from_parameters: &GenericParameters,
        to_id: Global<pernixc_symbol::ID>,
        to_parameters: &GenericParameters,
    ) {
        self.append_from_arguments(
            from_parameters.lifetime_order().iter().copied().map(|x| {
                Lifetime::Parameter(LifetimeParameterID::new(from_id, x))
            }),
            to_parameters.lifetime_order().iter().copied(),
            to_id,
        );

        self.append_from_arguments(
            from_parameters
                .type_order()
                .iter()
                .copied()
                .map(|x| Type::Parameter(TypeParameterID::new(from_id, x))),
            to_parameters.type_order().iter().copied(),
            to_id,
        );

        self.append_from_arguments(
            from_parameters.constant_order().iter().copied().map(|x| {
                Constant::Parameter(ConstantParameterID::new(from_id, x))
            }),
            to_parameters.constant_order().iter().copied(),
            to_id,
        );
    }

    /// Converts the instantiation into a [`GenericArguments`].
    ///
    /// The function will search for the corresponding instantiation for each
    /// generic parameter.
    #[must_use]
    pub fn create_generic_arguments(
        &self,
        global_id: Global<pernixc_symbol::ID>,
        parameters: &GenericParameters,
    ) -> GenericArguments {
        GenericArguments {
            lifetimes: parameters
                .lifetime_order()
                .iter()
                .copied()
                .map(|x| {
                    let lifetime_parameter = Lifetime::Parameter(
                        LifetimeParameterID::new(global_id, x),
                    );

                    let inst = self
                        .lifetimes
                        .get(&lifetime_parameter)
                        .copied()
                        .unwrap();

                    inst
                })
                .collect(),
            types: parameters
                .type_order()
                .iter()
                .copied()
                .map(|x| {
                    let type_parameter =
                        Type::Parameter(TypeParameterID::new(global_id, x));

                    let inst =
                        self.types.get(&type_parameter).cloned().unwrap();

                    inst
                })
                .collect(),
            constants: parameters
                .constant_order()
                .iter()
                .copied()
                .map(|x| {
                    let constant_parameter = Constant::Parameter(
                        ConstantParameterID::new(global_id, x),
                    );

                    let inst = self
                        .constants
                        .get(&constant_parameter)
                        .cloned()
                        .unwrap();

                    inst
                })
                .collect(),
        }
    }

    /// Converts the given generic arguments into a substitution.
    ///
    /// # Errors
    ///
    /// See [`MismatchedGenericArgumentCountError`].
    pub fn from_generic_arguments(
        generic_arguments: GenericArguments,
        global_id: Global<pernixc_symbol::ID>,
        generic_parameters: &GenericParameters,
    ) -> Result<Self, MismatchedGenericArgumentCountError> {
        let mut substitution = Self::default();

        substitution.append_from_generic_arguments(
            generic_arguments,
            global_id,
            generic_parameters,
        )?;

        Ok(substitution)
    }

    /// Applies the given substitution to the term.
    pub fn instantiate<T: Element + Clone + visitor::Element>(
        &self,
        element: &mut T,
    ) {
        let mut instantiater = Instantiater { substitution: self };
        visitor::accept_recursive_mut(element, &mut instantiater);
    }
}

/// A trait for retrieving the instantiation map from the [`Instantiation`]
/// struct.
#[allow(missing_docs)]
pub trait Element {
    fn get(instantiation: &Instantiation) -> &BTreeMap<Self, Self>
    where
        Self: Sized;

    fn get_mut(instantiation: &mut Instantiation) -> &mut BTreeMap<Self, Self>
    where
        Self: Sized;
}

impl Element for Lifetime {
    fn get(instantiation: &Instantiation) -> &BTreeMap<Self, Self> {
        &instantiation.lifetimes
    }

    fn get_mut(instantiation: &mut Instantiation) -> &mut BTreeMap<Self, Self> {
        &mut instantiation.lifetimes
    }
}

impl Element for Type {
    fn get(instantiation: &Instantiation) -> &BTreeMap<Self, Self> {
        &instantiation.types
    }

    fn get_mut(instantiation: &mut Instantiation) -> &mut BTreeMap<Self, Self> {
        &mut instantiation.types
    }
}

impl Element for Constant {
    fn get(instantiation: &Instantiation) -> &BTreeMap<Self, Self> {
        &instantiation.constants
    }

    fn get_mut(instantiation: &mut Instantiation) -> &mut BTreeMap<Self, Self> {
        &mut instantiation.constants
    }
}

impl GenericArguments {
    /// Applies the instantiation to all the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        for lifetime in &mut self.lifetimes {
            instantiation.instantiate(lifetime);
        }

        for r#type in &mut self.types {
            instantiation.instantiate(r#type);
        }

        for constant in &mut self.constants {
            instantiation.instantiate(constant);
        }
    }
}
