//! Contains the code related to applying substitutions to terms.

use std::{collections::BTreeMap, hash::Hash};

use derive_new::new;
use pernixc_arena::ID;
use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::{MemberID, parent::get_parent};
use pernixc_target::Global;
use qbice::{Decode, Encode, StableHash};

use super::{
    sub_term::TermLocation,
    visitor::{self, MutableRecursive},
};
use crate::{
    constant::Constant,
    generic_arguments::GenericArguments,
    generic_parameters::{
        ConstantParameterID, GenericKind, GenericParameters,
        InstanceParameterID, LifetimeParameterID, TypeParameterID,
        get_generic_parameters,
    },
    instance::Instance,
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
    Encode,
    Decode,
    new,
)]
#[allow(missing_docs)]
pub struct Instantiation {
    lifetimes: BTreeMap<Lifetime, Lifetime>,
    types: BTreeMap<Type, Type>,
    constants: BTreeMap<Constant, Constant>,
    instances: BTreeMap<Instance, Instance>,
}

impl Instantiation {
    /// Retrieves the mapping for the given lifetime if it exists.
    #[must_use]
    pub fn get_lifetime_mapping(
        &self,
        lifetime: &Lifetime,
    ) -> Option<&Lifetime> {
        self.lifetimes.get(lifetime)
    }

    /// Retrieves the mapping for the given type if it exists.
    #[must_use]
    pub fn get_type_mapping(&self, r#type: &Type) -> Option<&Type> {
        self.types.get(r#type)
    }

    /// Retrieves the mapping for the given constant if it exists.
    #[must_use]
    pub fn get_constant_mapping(
        &self,
        constant: &Constant,
    ) -> Option<&Constant> {
        self.constants.get(constant)
    }

    /// Retrieves the mapping for the given instance if it exists.
    #[must_use]
    pub fn get_instance_mapping(
        &self,
        instance: &Instance,
    ) -> Option<&Instance> {
        self.instances.get(instance)
    }
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

            assert!(
                T::get_mut(self).insert(parameter_id.into(), term).is_none()
            );
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
    #[allow(clippy::result_large_err)]
    pub fn append_from_generic_arguments(
        &mut self,
        generic_arguments: GenericArguments,
        generic_id: Global<pernixc_symbol::ID>,
        generic_parameters: &GenericParameters,
    ) -> Result<(), MismatchedGenericArgumentCountError> {
        if generic_arguments.types().len()
            != generic_parameters.type_parameter_order().len()
        {
            return Err(MismatchedGenericArgumentCountError {
                generic_id,
                expected: generic_parameters.type_parameter_order().len(),
                found: generic_arguments.types().len(),
                kind: GenericKind::Type,
                generic_arguments,
            });
        }

        if generic_arguments.lifetimes().len()
            != generic_parameters.lifetime_parameter_order().len()
        {
            return Err(MismatchedGenericArgumentCountError {
                generic_id,
                expected: generic_parameters.lifetime_parameter_order().len(),
                found: generic_arguments.lifetimes().len(),
                kind: GenericKind::Lifetime,
                generic_arguments,
            });
        }

        if generic_arguments.constants().len()
            != generic_parameters.constant_parameter_order().len()
        {
            return Err(MismatchedGenericArgumentCountError {
                generic_id,
                expected: generic_parameters.constant_parameter_order().len(),
                found: generic_arguments.constants().len(),
                kind: GenericKind::Constant,
                generic_arguments,
            });
        }

        if generic_arguments.instances().len()
            != generic_parameters.instance_parameter_order().len()
        {
            return Err(MismatchedGenericArgumentCountError {
                generic_id,
                expected: generic_parameters.instance_parameter_order().len(),
                found: generic_arguments.instances().len(),
                kind: GenericKind::Instance,
                generic_arguments,
            });
        }

        let (lifetimes, types, constants, instances) =
            generic_arguments.into_arguments();

        self.append_from_arguments(
            lifetimes.into_iter(),
            generic_parameters.lifetime_parameter_order(),
            generic_id,
        );

        self.append_from_arguments(
            types.into_iter(),
            generic_parameters.type_parameter_order(),
            generic_id,
        );

        self.append_from_arguments(
            constants.into_iter(),
            generic_parameters.constant_parameter_order(),
            generic_id,
        );

        self.append_from_arguments(
            instances.into_iter(),
            generic_parameters.instance_parameter_order(),
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
            from_parameters.lifetime_parameter_order().map(|x| {
                Lifetime::Parameter(LifetimeParameterID::new(from_id, x))
            }),
            to_parameters.lifetime_parameter_order(),
            to_id,
        );

        self.append_from_arguments(
            from_parameters
                .type_parameter_order()
                .map(|x| Type::Parameter(TypeParameterID::new(from_id, x))),
            to_parameters.type_parameter_order(),
            to_id,
        );

        self.append_from_arguments(
            from_parameters.constant_parameter_order().map(|x| {
                Constant::Parameter(ConstantParameterID::new(from_id, x))
            }),
            to_parameters.constant_parameter_order(),
            to_id,
        );

        self.append_from_arguments(
            from_parameters.instance_parameter_order().map(|x| {
                Instance::Parameter(InstanceParameterID::new(from_id, x))
            }),
            to_parameters.instance_parameter_order(),
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
        GenericArguments::new(
            parameters
                .lifetime_parameter_order()
                .map(|x| {
                    let lifetime_parameter = Lifetime::Parameter(
                        LifetimeParameterID::new(global_id, x),
                    );

                    self.lifetimes.get(&lifetime_parameter).cloned().unwrap()
                })
                .collect(),
            parameters
                .type_parameter_order()
                .map(|x| {
                    let type_parameter =
                        Type::Parameter(TypeParameterID::new(global_id, x));

                    self.types.get(&type_parameter).cloned().unwrap()
                })
                .collect(),
            parameters
                .constant_parameter_order()
                .map(|x| {
                    let constant_parameter = Constant::Parameter(
                        ConstantParameterID::new(global_id, x),
                    );

                    self.constants.get(&constant_parameter).cloned().unwrap()
                })
                .collect(),
            parameters
                .instance_parameter_order()
                .map(|x| {
                    let instance_parameter = Instance::Parameter(
                        InstanceParameterID::new(global_id, x),
                    );

                    self.instances.get(&instance_parameter).cloned().unwrap()
                })
                .collect(),
        )
    }

    /// Converts the given generic arguments into a substitution.
    ///
    /// # Errors
    ///
    /// See [`MismatchedGenericArgumentCountError`].
    #[allow(clippy::result_large_err)]
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

    /// Clones the given term and applies the substitution to the cloned
    /// term.
    pub fn clone_and_instantiate<T: Element + Clone + visitor::Element>(
        &self,
        element: &T,
    ) -> T {
        let mut cloned = element.clone();
        self.instantiate(&mut cloned);
        cloned
    }
}

/// Retrieves the [`Instantiation`] for the given generic ID with the given
/// generic arguments.
#[extend]
pub async fn get_instantiation(
    self: &TrackedEngine,
    id: Global<pernixc_symbol::ID>,
    generic_arguments: GenericArguments,
) -> Result<Instantiation, MismatchedGenericArgumentCountError> {
    let generic_parameters = self.get_generic_parameters(id).await;

    Instantiation::from_generic_arguments(
        generic_arguments,
        id,
        &generic_parameters,
    )
}

/// Retrieves the [`Instantiation`] for the given associated symbol ID with the
/// given generic arguments (both member level and parent level).
#[extend]
pub async fn get_instantiation_for_associated_symbol(
    self: &TrackedEngine,
    id: Global<pernixc_symbol::ID>,
    parent_generic_arguments: GenericArguments,
    member_generic_arguments: GenericArguments,
) -> Result<Instantiation, MismatchedGenericArgumentCountError> {
    let member_generic_parameters = self.get_generic_parameters(id).await;
    let mut instantiation = Instantiation::from_generic_arguments(
        member_generic_arguments,
        id,
        &member_generic_parameters,
    )?;

    let parent_id =
        id.target_id.make_global(self.get_parent(id).await.unwrap());

    let parent_generic_parameters =
        self.get_generic_parameters(parent_id).await;

    instantiation.append_from_generic_arguments(
        parent_generic_arguments,
        parent_id,
        &parent_generic_parameters,
    )?;

    Ok(instantiation)
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

impl Element for Instance {
    fn get(instantiation: &Instantiation) -> &BTreeMap<Self, Self> {
        &instantiation.instances
    }

    fn get_mut(instantiation: &mut Instantiation) -> &mut BTreeMap<Self, Self> {
        &mut instantiation.instances
    }
}
