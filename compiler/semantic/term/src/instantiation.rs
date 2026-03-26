//! Contains the code related to applying substitutions to terms.

use std::{collections::BTreeMap, hash::Hash};

use derive_new::new;
use pernixc_arena::ID;
use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::MemberID;
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
        ConstantParameterID, GenericParameters, InstanceParameterID,
        LifetimeParameterID, TypeParameterID, get_generic_parameters,
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

    /// Returns an iterator over the mutable lifetime mappings.
    pub fn lifetime_mappings_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = (&Lifetime, &mut Lifetime)> {
        self.lifetimes.iter_mut()
    }

    /// Returns an iterator over the mutable type mappings.
    pub fn type_mappings_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = (&Type, &mut Type)> {
        self.types.iter_mut()
    }

    /// Returns an iterator over the mutable constant mappings.
    pub fn constant_mappings_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = (&Constant, &mut Constant)> {
        self.constants.iter_mut()
    }

    /// Returns an iterator over the mutable instance mappings.
    pub fn instance_mappings_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = (&Instance, &mut Instance)> {
        self.instances.iter_mut()
    }

    /// Returns an iterator over the mutable lifetime values.
    #[must_use]
    pub fn lifetime_values_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = &mut Lifetime> {
        self.lifetimes.values_mut()
    }

    /// Returns an iterator over the mutable type values.
    #[must_use]
    pub fn type_values_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = &mut Type> {
        self.types.values_mut()
    }

    /// Returns an iterator over the mutable constant values.
    #[must_use]
    pub fn constant_values_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = &mut Constant> {
        self.constants.values_mut()
    }

    /// Returns an iterator over the mutable instance values.
    #[must_use]
    pub fn instance_values_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = &mut Instance> {
        self.instances.values_mut()
    }

    /// Returns an iterator over the lifetime mappings.
    #[must_use]
    pub fn lifetime_mappings(
        &self,
    ) -> impl ExactSizeIterator<Item = (&Lifetime, &Lifetime)> {
        self.lifetimes.iter()
    }

    /// Returns an iterator over the type mappings.
    #[must_use]
    pub fn type_mappings(
        &self,
    ) -> impl ExactSizeIterator<Item = (&Type, &Type)> {
        self.types.iter()
    }

    /// Returns an iterator over the constant mappings.
    #[must_use]
    pub fn constant_mappings(
        &self,
    ) -> impl ExactSizeIterator<Item = (&Constant, &Constant)> {
        self.constants.iter()
    }

    /// Returns an iterator over the instance mappings.
    #[must_use]
    pub fn instance_mappings(
        &self,
    ) -> impl ExactSizeIterator<Item = (&Instance, &Instance)> {
        self.instances.iter()
    }

    /// Extends the lifetime mappings with the given mappings.
    pub fn extend_lifetimes_mappings(
        &mut self,
        mappings: impl IntoIterator<Item = (Lifetime, Lifetime)>,
    ) {
        self.lifetimes.extend(mappings);
    }

    /// Extends the type mappings with the given mappings.
    pub fn extend_types_mappings(
        &mut self,
        mappings: impl IntoIterator<Item = (Type, Type)>,
    ) {
        self.types.extend(mappings);
    }

    /// Extends the constant mappings with the given mappings.
    pub fn extend_constants_mappings(
        &mut self,
        mappings: impl IntoIterator<Item = (Constant, Constant)>,
    ) {
        self.constants.extend(mappings);
    }

    /// Extends the instance mappings with the given mappings.
    pub fn extend_instances_mappings(
        &mut self,
        mappings: impl IntoIterator<Item = (Instance, Instance)>,
    ) {
        self.instances.extend(mappings);
    }

    /// Instantiates the term values in this instantiation with the given
    /// instantiation.
    pub fn instantiate_values(&mut self, inst: &Self) {
        self.lifetime_values_mut().for_each(|term| {
            inst.instantiate(term);
        });
        self.type_values_mut().for_each(|term| {
            inst.instantiate(term);
        });
        self.constant_values_mut().for_each(|term| {
            inst.instantiate(term);
        });
        self.instance_values_mut().for_each(|term| {
            inst.instantiate(term);
        });
    }

    /// Inserts a mapping from one lifetime to another.
    pub fn insert_lifetime_mapping(&mut self, from: Lifetime, to: Lifetime) {
        self.lifetimes.insert(from, to);
    }

    /// Inserts a mapping from one type to another.
    pub fn insert_type_mapping(&mut self, from: Type, to: Type) {
        self.types.insert(from, to);
    }

    /// Inserts a mapping from one constant to another.
    pub fn insert_constant_mapping(&mut self, from: Constant, to: Constant) {
        self.constants.insert(from, to);
    }

    /// Inserts a mapping from one instance to another.
    pub fn insert_instance_mapping(&mut self, from: Instance, to: Instance) {
        self.instances.insert(from, to);
    }

    /// Removes the mapping for the given lifetime if it exists.
    pub fn remove_lifetime_mapping(
        &mut self,
        from: &Lifetime,
    ) -> Option<Lifetime> {
        self.lifetimes.remove(from)
    }

    /// Removes the mapping for the given type if it exists.
    pub fn remove_type_mapping(&mut self, from: &Type) -> Option<Type> {
        self.types.remove(from)
    }

    /// Removes the mapping for the given constant if it exists.
    pub fn remove_constant_mapping(
        &mut self,
        from: &Constant,
    ) -> Option<Constant> {
        self.constants.remove(from)
    }

    /// Removes the mapping for the given instance if it exists.
    pub fn remove_instance_mapping(
        &mut self,
        from: &Instance,
    ) -> Option<Instance> {
        self.instances.remove(from)
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

impl Instantiation {
    fn append_from_arguments<T: Element + Ord, U>(
        &mut self,
        terms: impl Iterator<Item = T>,
        term_parameter_order: impl Iterator<Item = ID<U>>,
        generic_id: Global<pernixc_symbol::SymbolID>,
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
    #[allow(clippy::result_large_err)]
    pub fn append_from_generic_arguments(
        &mut self,
        generic_arguments: &GenericArguments,
        generic_id: Global<pernixc_symbol::SymbolID>,
        generic_parameters: &GenericParameters,
    ) {
        assert_eq!(
            generic_arguments.types().len(),
            generic_parameters.type_parameter_order().len(),
            "Mismatched number of type generic arguments",
        );

        assert_eq!(
            generic_arguments.lifetimes().len(),
            generic_parameters.lifetime_parameter_order().len(),
            "Mismatched number of lifetime generic arguments",
        );

        assert_eq!(
            generic_arguments.constants().len(),
            generic_parameters.constant_parameter_order().len(),
            "Mismatched number of constant generic arguments",
        );

        assert_eq!(
            generic_arguments.instances().len(),
            generic_parameters.instance_parameter_order().len(),
            "Mismatched number of instance generic arguments",
        );

        self.append_from_arguments(
            generic_arguments.lifetimes().iter().cloned(),
            generic_parameters.lifetime_parameter_order(),
            generic_id,
        );

        self.append_from_arguments(
            generic_arguments.types().iter().cloned(),
            generic_parameters.type_parameter_order(),
            generic_id,
        );

        self.append_from_arguments(
            generic_arguments.constants().iter().cloned(),
            generic_parameters.constant_parameter_order(),
            generic_id,
        );

        self.append_from_arguments(
            generic_arguments.instances().iter().cloned(),
            generic_parameters.instance_parameter_order(),
            generic_id,
        );
    }

    /// Creates a mapping from one generic parameter to another.
    ///
    /// # Example
    ///
    /// Suppose `A[B, C, D]` and `T[U, V, W]`. To map the generic parameters
    /// of `A` to `T`, this function will create {B -> U, C -> V, D -> W}.
    pub fn append_from_generic_parameters_mapping(
        &mut self,
        from_id: Global<pernixc_symbol::SymbolID>,
        from_parameters: &GenericParameters,
        to_id: Global<pernixc_symbol::SymbolID>,
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
        global_id: Global<pernixc_symbol::SymbolID>,
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
    #[must_use]
    pub fn from_generic_arguments(
        generic_arguments: &GenericArguments,
        global_id: Global<pernixc_symbol::SymbolID>,
        generic_parameters: &GenericParameters,
    ) -> Self {
        let mut substitution = Self::default();

        substitution.append_from_generic_arguments(
            generic_arguments,
            global_id,
            generic_parameters,
        );

        substitution
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

/// Retrieves the [`GenericArguments`] for the given generic ID with the given
/// instantiation.
#[extend]
pub async fn create_generic_arguments_from_instantiation(
    self: &TrackedEngine,
    id: Global<pernixc_symbol::SymbolID>,
    instantiation: Instantiation,
) -> GenericArguments {
    let generic_parameters = self.get_generic_parameters(id).await;

    instantiation.create_generic_arguments(id, &generic_parameters)
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
