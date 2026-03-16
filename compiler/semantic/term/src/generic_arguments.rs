//! Contains the definition of [`GenericArguments`] and related types.

use std::{fmt::Write, ops::Not};

use derive_new::new;
use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::{
    name::{get_name, get_qualified_name},
    parent::{get_parent, get_parent_global},
};
use pernixc_target::Global;
use qbice::{Decode, Encode, Identifiable, StableHash};

use crate::{
    TermMut,
    constant::Constant,
    generic_parameters::{
        ConstantParameterID, GenericKind, GenericParameters,
        InstanceParameterID, LifetimeParameterID, TypeParameterID,
        get_generic_parameters,
    },
    instance::Instance,
    instantiation::Instantiation,
    lifetime::Lifetime,
    matching::{Matching, Substructural},
    sub_term::{
        SubConstantLocation, SubInstanceLocation, SubLifetimeLocation,
        SubTypeLocation,
    },
    r#type::Type,
    visitor::{self, AsyncMutable, AsyncVisitor, Mutable, Visitor},
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

/// Represents a list of generic arguments supplied to a particular symbol that
/// has generic parameters (e.g., `symbol[ARGS]`).
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Encode,
    Decode,
    StableHash,
    Identifiable,
    new,
)]
pub struct GenericArguments {
    lifetimes: Vec<Lifetime>,
    types: Vec<Type>,
    constants: Vec<Constant>,
    instancces: Vec<Instance>,
}

/// The result from [`GenericArguments::zip_ref`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, StableHash)]
#[allow(missing_docs)]
pub enum ZipRef<'a> {
    Lifetime(&'a Lifetime, &'a Lifetime),
    Type(&'a Type, &'a Type),
    Constant(&'a Constant, &'a Constant),
    Instance(&'a Instance, &'a Instance),
}

impl GenericArguments {
    /// Returns the lifetimes supplied to the term.
    #[must_use]
    pub fn lifetimes(&self) -> &[Lifetime] { &self.lifetimes }

    /// Returns a mutable reference to the lifetimes supplied to the term.
    #[must_use]
    pub fn lifetimes_mut(&mut self) -> &mut [Lifetime] { &mut self.lifetimes }

    /// Returns the types supplied to the term.
    #[must_use]
    pub fn types(&self) -> &[Type] { &self.types }

    /// Returns a mutable reference to the types supplied to the term.
    #[must_use]
    pub fn types_mut(&mut self) -> &mut [Type] { &mut self.types }

    /// Returns the constants supplied to the term.
    #[must_use]
    pub fn constants(&self) -> &[Constant] { &self.constants }

    /// Returns a mutable reference to the constants supplied to the term.
    #[must_use]
    pub fn constants_mut(&mut self) -> &mut [Constant] { &mut self.constants }

    /// Returns the instances supplied to the term.
    #[must_use]
    pub fn instances(&self) -> &[Instance] { &self.instancces }

    /// Returns a mutable reference to the instances supplied to the term.
    #[must_use]
    pub fn instances_mut(&mut self) -> &mut [Instance] { &mut self.instancces }

    /// Checks if the generic arguments are the identity generic arguments for
    /// the symbol with the given ID.
    #[must_use]
    pub fn is_identity(
        &self,
        generic_parameters: &GenericParameters,
        symbol_id: Global<pernixc_symbol::ID>,
    ) -> bool {
        if !self.param_arity_matches(generic_parameters) {
            return false;
        }

        for (lifetime, lifetime_param) in self
            .lifetimes
            .iter()
            .zip(generic_parameters.lifetime_parameter_order())
        {
            if lifetime
                != &Lifetime::Parameter(LifetimeParameterID::new(
                    symbol_id,
                    lifetime_param,
                ))
            {
                return false;
            }
        }

        for (ty, type_param) in
            self.types.iter().zip(generic_parameters.type_parameter_order())
        {
            if ty
                != &Type::Parameter(TypeParameterID::new(symbol_id, type_param))
            {
                return false;
            }
        }

        for (constant, constant_param) in self
            .constants
            .iter()
            .zip(generic_parameters.constant_parameter_order())
        {
            if constant
                != &Constant::Parameter(ConstantParameterID::new(
                    symbol_id,
                    constant_param,
                ))
            {
                return false;
            }
        }

        for (instance, instance_param) in self
            .instancces
            .iter()
            .zip(generic_parameters.instance_parameter_order())
        {
            if instance
                != &Instance::Parameter(InstanceParameterID::new(
                    symbol_id,
                    instance_param,
                ))
            {
                return false;
            }
        }

        true
    }

    /// Constructs the generic arguments from the given generic parameters and
    /// instantiation.
    ///
    /// This works by extraing the mapping from the `instantiation` for each
    /// generic parameter in `generic_parameters` and constructing the generic
    /// arguments from the extracted mapping.
    #[must_use]
    pub fn from_generic_parameters_and_instantiation(
        instantiation: &Instantiation,
        generic_parameters: &GenericParameters,
        global_id: Global<pernixc_symbol::ID>,
    ) -> Self {
        let mut generic_arguments = Self::default();

        for lifetime_id in generic_parameters.lifetime_parameter_order() {
            let lifetime_parameter = Lifetime::Parameter(
                LifetimeParameterID::new(global_id, lifetime_id),
            );

            let lifetime = instantiation
                .get_lifetime_mapping(&lifetime_parameter)
                .unwrap();

            generic_arguments.lifetimes.push(lifetime.clone());
        }

        for type_id in generic_parameters.type_parameter_order() {
            let type_parameter =
                Type::Parameter(TypeParameterID::new(global_id, type_id));

            let r#type =
                instantiation.get_type_mapping(&type_parameter).unwrap();

            generic_arguments.types.push(r#type.clone());
        }

        for constant_id in generic_parameters.constant_parameter_order() {
            let constant_parameter = Constant::Parameter(
                ConstantParameterID::new(global_id, constant_id),
            );

            let constant = instantiation
                .get_constant_mapping(&constant_parameter)
                .unwrap();

            generic_arguments.constants.push(constant.clone());
        }

        for instance_id in generic_parameters.instance_parameter_order() {
            let instance_parameter = Instance::Parameter(
                InstanceParameterID::new(global_id, instance_id),
            );

            let instance = instantiation
                .get_instance_mapping(&instance_parameter)
                .unwrap();

            generic_arguments.instancces.push(instance.clone());
        }

        generic_arguments
    }

    /// Applies the given instantiation to `self`, replacing any generic
    /// arguments that are mapped by the `instantiation` with their
    /// corresponding mapped values.
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

        for instance in &mut self.instancces {
            instantiation.instantiate(instance);
        }
    }

    /// Destructures the generic arguments into its components.
    #[must_use]
    pub fn into_arguments(
        self,
    ) -> (Vec<Lifetime>, Vec<Type>, Vec<Constant>, Vec<Instance>) {
        (self.lifetimes, self.types, self.constants, self.instancces)
    }

    /// Checks if the generic arguments are empty.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.lifetimes.is_empty()
            && self.types.is_empty()
            && self.constants.is_empty()
    }

    /// Checks if both generic arguments have the same arity for each generic
    /// kind.
    #[must_use]
    pub const fn arity_matches(&self, generic_arguments: &Self) -> bool {
        self.lifetimes.len() == generic_arguments.lifetimes.len()
            && self.types.len() == generic_arguments.types.len()
            && self.constants.len() == generic_arguments.constants.len()
            && self.instancces.len() == generic_arguments.instancces.len()
    }

    /// Checks if the generic arguments have the same arguments as the given
    /// generic parameters.
    #[must_use]
    pub fn param_arity_matches(
        &self,
        generic_params: &GenericParameters,
    ) -> bool {
        self.lifetimes.len() == generic_params.lifetime_parameter_order().len()
            && self.types.len() == generic_params.type_parameter_order().len()
            && self.constants.len()
                == generic_params.constant_parameter_order().len()
            && self.instancces.len()
                == generic_params.instance_parameter_order().len()
    }

    /// Pushes a lifetime to the generic arguments.
    pub fn push_lifetime(&mut self, lifetime: Lifetime) {
        self.lifetimes.push(lifetime);
    }

    /// Pushes a type to the generic arguments.
    pub fn push_type(&mut self, ty: Type) { self.types.push(ty); }

    /// Pushes a constant to the generic arguments.
    pub fn push_constant(&mut self, constant: Constant) {
        self.constants.push(constant);
    }

    /// Pushes an instance to the generic arguments.
    pub fn push_instance(&mut self, instance: Instance) {
        self.instancces.push(instance);
    }

    /// Inserts a type argument at the given index in the generic arguments.
    pub fn insert_type_at(&mut self, index: usize, ty: Type) {
        self.types.insert(index, ty);
    }

    /// Pushes an element to the generic arguments.
    pub fn push<T: Element>(&mut self, element: T) {
        T::get_mut(self).push(element);
    }

    /// Returns the number of arguments of the given generic kind in the generic
    /// arguments.
    #[must_use]
    pub fn len_of<T: Element>(&self) -> usize { T::get(self).len() }

    /// Resizes the arguments of the given generic kind in the generic arguments
    /// to the given new length, filling new elements with the given default
    /// value if the new length is greater than the current length.
    pub fn resize<T: Element + Clone>(&mut self, new_len: usize, default: T) {
        T::get_mut(self).resize(new_len, default);
    }

    /// Returns an iterator over mutable references to all sub-terms in the
    /// generic arguments.
    pub fn iter_all_term_mut(
        &mut self,
    ) -> impl Iterator<Item = TermMut<'_>> + '_ {
        self.lifetimes
            .iter_mut()
            .map(TermMut::Lifetime)
            .chain(self.types.iter_mut().map(TermMut::Type))
            .chain(self.constants.iter_mut().map(TermMut::Constant))
            .chain(self.instancces.iter_mut().map(TermMut::Instance))
    }

    /// Returns an iterator that matches each argument in `self` to the
    /// corresponding argument in `other` and yields a reference to the matched
    /// pair.
    pub fn zip_ref<'a>(
        &'a self,
        other: &'a Self,
    ) -> impl Iterator<Item = ZipRef<'a>> + 'a {
        self.lifetimes
            .iter()
            .zip(other.lifetimes.iter())
            .map(|(l1, l2)| ZipRef::Lifetime(l1, l2))
            .chain(
                self.types
                    .iter()
                    .zip(other.types.iter())
                    .map(|(t1, t2)| ZipRef::Type(t1, t2)),
            )
            .chain(
                self.constants
                    .iter()
                    .zip(other.constants.iter())
                    .map(|(c1, c2)| ZipRef::Constant(c1, c2)),
            )
            .chain(
                self.instancces
                    .iter()
                    .zip(other.instancces.iter())
                    .map(|(i1, i2)| ZipRef::Instance(i1, i2)),
            )
    }
}

/// Creates an identity generic arguments for the symbol with the given ID.
///
/// The identity generic arguments for a symbol has all the arguments the same
/// as the generic parameters of the symbol.
#[extend]
pub async fn create_identity_generic_arguments(
    self: Global<pernixc_symbol::ID>,
    engine: &TrackedEngine,
) -> GenericArguments {
    let generic_params = engine.get_generic_parameters(self).await;

    let mut generic_arguments = GenericArguments::default();

    for lifetime_param in generic_params.lifetime_parameter_order() {
        let lt_param = Lifetime::new_parameter(self, lifetime_param);
        generic_arguments.push_lifetime(lt_param);
    }

    for type_param in generic_params.type_parameter_order() {
        let ty_param = Type::new_parameter(self, type_param);
        generic_arguments.push_type(ty_param);
    }

    for constant_param in generic_params.constant_parameter_order() {
        let const_param = Constant::new_parameter(self, constant_param);
        generic_arguments.push_constant(const_param);
    }

    for instance_param in generic_params.instance_parameter_order() {
        let instance_param = Instance::new_parameter(self, instance_param);
        generic_arguments.push_instance(instance_param);
    }

    generic_arguments
}

/// Checks if the given generic arguments are identity to the symbol with the
/// given ID.
#[extend]
pub async fn is_generic_arguments_identity_to(
    self: &GenericArguments,
    engine: &TrackedEngine,
    symbol_id: Global<pernixc_symbol::ID>,
) -> bool {
    let generic_params = engine.get_generic_parameters(symbol_id).await;

    self.is_identity(&generic_params, symbol_id)
}

/// Represents a sub-term location where the sub-term is stored as a generic
/// arguments.
///
/// The `usize` represents the index of the sub-term in the generic arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct SubGenericArgumentsLocation(usize);

impl GenericArguments {
    /// Returns a mutable reference to a particular sub-term of this generic
    /// arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term_mut<T: Element>(
        &mut self,
        location: SubGenericArgumentsLocation,
    ) -> Option<&mut T> {
        let generic_arguments = T::get_mut(self);

        generic_arguments.get_mut(location.0)
    }

    /// Returns a reference to a particular sub-term of this generic arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term<T: Element>(
        &self,
        location: SubGenericArgumentsLocation,
    ) -> Option<&T> {
        let generic_arguments = T::get(self);

        generic_arguments.get(location.0)
    }
}

impl crate::display::Display for GenericArguments {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        if !formatter.configuration().generic_arguments_will_be_displayed(self)
        {
            return Ok(());
        }

        let last_is =
            if self.instancces.is_empty().not() {
                GenericKind::Instance
            } else if self.constants.is_empty().not() {
                GenericKind::Constant
            } else if self.types.is_empty().not() {
                GenericKind::Type
            } else if self.lifetimes.iter().any(|x| {
                formatter.configuration().lifetime_will_be_displayed(x)
            }) {
                GenericKind::Lifetime
            } else {
                unreachable!()
            };

        write!(formatter, "[")?;

        let lts = self
            .lifetimes
            .iter()
            .filter(|lt| {
                formatter.configuration().lifetime_will_be_displayed(lt)
            })
            .collect::<Vec<_>>();
        let lts_len = lts.len();

        for (i, lt) in lts.into_iter().enumerate() {
            crate::display::Display::fmt(lt, engine, formatter).await?;

            if i + 1 != lts_len || last_is != GenericKind::Lifetime {
                write!(formatter, ", ")?;
            }
        }

        let tys_len = self.types.len();

        for (i, ty) in self.types.iter().enumerate() {
            crate::display::Display::fmt(ty, engine, formatter).await?;

            if i + 1 != tys_len || last_is != GenericKind::Type {
                write!(formatter, ", ")?;
            }
        }

        let consts_len = self.constants.len();

        for (i, constant) in self.constants.iter().enumerate() {
            crate::display::Display::fmt(constant, engine, formatter).await?;

            if i + 1 != consts_len || last_is != GenericKind::Constant {
                write!(formatter, ", ")?;
            }
        }

        let instances_len = self.instancces.len();

        for (i, instance) in self.instancces.iter().enumerate() {
            Box::pin(crate::display::Display::fmt(instance, engine, formatter))
                .await?;

            if i + 1 != instances_len || last_is != GenericKind::Instance {
                write!(formatter, ", ")?;
            }
        }

        write!(formatter, "]")
    }
}

mod sealed {
    use crate::generic_arguments::GenericArguments;

    pub trait Sealed {
        fn get(generic_arguments: &GenericArguments) -> &[Self]
        where
            Self: Sized;

        fn get_mut(generic_arguments: &mut GenericArguments) -> &mut Vec<Self>
        where
            Self: Sized;
    }
}

/// A trait for retrieving the arguments array from a generic arguments.
#[allow(missing_docs)]
pub trait Element: sealed::Sealed {}

impl sealed::Sealed for Lifetime {
    fn get(generic_arguments: &GenericArguments) -> &[Self] {
        &generic_arguments.lifetimes
    }

    fn get_mut(generic_arguments: &mut GenericArguments) -> &mut Vec<Self> {
        &mut generic_arguments.lifetimes
    }
}

impl Element for Lifetime {}

impl sealed::Sealed for Type {
    fn get(generic_arguments: &GenericArguments) -> &[Self] {
        &generic_arguments.types
    }

    fn get_mut(generic_arguments: &mut GenericArguments) -> &mut Vec<Self> {
        &mut generic_arguments.types
    }
}

impl Element for Type {}

impl sealed::Sealed for Constant {
    fn get(generic_arguments: &GenericArguments) -> &[Self] {
        &generic_arguments.constants
    }

    fn get_mut(generic_arguments: &mut GenericArguments) -> &mut Vec<Self> {
        &mut generic_arguments.constants
    }
}

impl Element for Constant {}

impl sealed::Sealed for Instance {
    fn get(generic_arguments: &GenericArguments) -> &[Self] {
        &generic_arguments.instancces
    }

    fn get_mut(generic_arguments: &mut GenericArguments) -> &mut Vec<Self> {
        &mut generic_arguments.instancces
    }
}

impl Element for Instance {}

/// Represents a term where the a symbol is supplied with generic arguments
/// (e.g., `symbol[ARGS]`).
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    new,
)]
pub struct Symbol {
    /// The ID of the symbol that is supplied with generic arguments.
    id: Global<pernixc_symbol::ID>,

    /// The generic arguments supplied to the symbol.
    generic_arguments: GenericArguments,
}

impl Symbol {
    /// Returns the ID of the symbol.
    #[must_use]
    pub const fn id(&self) -> Global<pernixc_symbol::ID> { self.id }

    /// Returns the generic arguments supplied to the symbol.
    #[must_use]
    pub const fn generic_arguments(&self) -> &GenericArguments {
        &self.generic_arguments
    }

    /// Destructures the symbol into its components.
    #[must_use]
    pub fn destructure(self) -> (Global<pernixc_symbol::ID>, GenericArguments) {
        (self.id, self.generic_arguments)
    }

    /// Destructures into the generic arguments supplied to the symbol.
    #[must_use]
    pub fn into_generic_arguments(self) -> GenericArguments {
        self.generic_arguments
    }

    /// Instantiates this [`Symbol`] with the given instantiation.
    pub fn instantiate(&mut self, inst: &Instantiation) {
        self.generic_arguments.instantiate(inst);
    }

    /// Creates an [`Instantiation`] for this symbol by using the generic
    /// arguments supplied to this symbol and the generic parameters of this
    /// symbol.
    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> Instantiation {
        let generic_parameters = engine.get_generic_parameters(self.id).await;

        Instantiation::from_generic_arguments(
            &self.generic_arguments,
            self.id,
            &generic_parameters,
        )
    }

    /// Creates an [`Instantiation`] for the parent of this symbol by using the
    /// generic arguments supplied to this symbol and the generic parameters of
    /// the parent of this symbol.
    pub async fn create_instantiation_parent(
        &self,
        engine: &TrackedEngine,
    ) -> Instantiation {
        let parent_id = engine.get_parent_global(self.id).await.unwrap();
        let generic_parameters = engine.get_generic_parameters(parent_id).await;

        Instantiation::from_generic_arguments(
            &self.generic_arguments,
            parent_id,
            &generic_parameters,
        )
    }

    /// Sets the first type argument of this symbol to the given type.
    #[allow(clippy::result_large_err)]
    pub fn set_first_type_argument(&mut self, ty: Type) -> Result<(), Type> {
        if let Some(first) = self.generic_arguments.types_mut().first_mut() {
            *first = ty;

            Ok(())
        } else {
            Err(ty)
        }
    }

    /// Returns an iterator yielding mutable references to all terms appeared in
    /// the generic arguments
    pub fn iter_all_term_mut(
        &mut self,
    ) -> impl Iterator<Item = TermMut<'_>> + '_ {
        self.generic_arguments.iter_all_term_mut()
    }
}

/// Represents a term where the associated symbol is supplied with generic
/// arguments as well as their parent (e.g., `symbol[ARGS]::associated[ARGS]`).
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    new,
)]
pub struct AssociatedSymbol {
    /// The ID of the associated symbol.
    ///
    /// By associated symbol, we mean a symbol that is defined in the context
    /// of another symbol, such as a method or an associated type.
    id: Global<pernixc_symbol::ID>,

    /// The generic arguments supplied to the parent of the associated symbol.
    parent_generic_arguments: GenericArguments,

    /// The generic arguments supplied to the associated symbol.
    member_generic_arguments: GenericArguments,
}

impl AssociatedSymbol {
    /// Returns the ID of the associated symbol.
    #[must_use]
    pub const fn id(&self) -> Global<pernixc_symbol::ID> { self.id }

    /// Returns the generic arguments supplied to the parent of the associated
    /// symbol.
    #[must_use]
    pub const fn parent_generic_arguments(&self) -> &GenericArguments {
        &self.parent_generic_arguments
    }

    /// Returns the generic arguments supplied to the associated symbol.
    #[must_use]
    pub const fn member_generic_arguments(&self) -> &GenericArguments {
        &self.member_generic_arguments
    }

    /// Creates an [`Instantiation`] for this associated symbol by using the
    /// generic arguments supplied to this associated symbol and the generic
    /// parameters of this associated symbol and its parent.
    #[must_use]
    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> Instantiation {
        let member_generic_parameters =
            engine.get_generic_parameters(self.id).await;

        let mut inst = Instantiation::from_generic_arguments(
            &self.member_generic_arguments,
            self.id,
            &member_generic_parameters,
        );

        let parent_id = engine.get_parent_global(self.id).await.unwrap();

        let parent_generic_parameters =
            engine.get_generic_parameters(parent_id).await;

        inst.append_from_generic_arguments(
            &self.parent_generic_arguments,
            parent_id,
            &parent_generic_parameters,
        );

        inst
    }

    /// Destructures the associated symbol into its components.
    #[must_use]
    pub fn into_id_and_member_generic_arguments(
        self,
    ) -> (Global<pernixc_symbol::ID>, GenericArguments) {
        (self.id, self.member_generic_arguments)
    }

    /// Returns an iterator yielding mutable references to all terms appeared in
    /// the generic arguments
    pub fn iter_all_term_mut(
        &mut self,
    ) -> impl Iterator<Item = TermMut<'_>> + '_ {
        self.member_generic_arguments
            .iter_all_term_mut()
            .chain(self.parent_generic_arguments.iter_all_term_mut())
    }
}

/// Represents a sub-term location where the sub-term is stored as a generic
/// arguments.
///
/// The `usize` represents the index of the sub-term in the generic arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct SubSymbolLocation(usize);

impl SubSymbolLocation {
    /// Retrieves the index of the sub-term in the generic arguments.
    #[must_use]
    pub const fn index(&self) -> usize { self.0 }
}

/// Represents a sub-term location where the sub-term is stored as a generic
/// arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct SubAssociatedSymbolLocation {
    /// The index of the sub-term in the generic arguments.
    index: usize,

    /// True if the sub-term is in the parent's generic arguments part,
    /// otherwise false.
    from_parent: bool,
}

impl AssociatedSymbol {
    /// Returns a mutable reference to a particular sub-term of this generic
    /// arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term_mut<T: Element>(
        &mut self,
        location: SubAssociatedSymbolLocation,
    ) -> Option<&mut T> {
        let generic_arguments = if location.from_parent {
            T::get_mut(&mut self.parent_generic_arguments)
        } else {
            T::get_mut(&mut self.member_generic_arguments)
        };

        generic_arguments.get_mut(location.index)
    }

    /// Returns a reference to a particular sub-term of this generic arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term<T: Element>(
        &self,
        location: SubAssociatedSymbolLocation,
    ) -> Option<&T> {
        let generic_arguments = if location.from_parent {
            T::get(&self.parent_generic_arguments)
        } else {
            T::get(&self.member_generic_arguments)
        };

        generic_arguments.get(location.index)
    }
}

impl Symbol {
    /// Returns a mutable reference to a particular sub-term of this generic
    /// arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term_mut<T: Element>(
        &mut self,
        location: SubSymbolLocation,
    ) -> Option<&mut T> {
        let generic_arguments = T::get_mut(&mut self.generic_arguments);

        generic_arguments.get_mut(location.0)
    }

    /// Returns a reference to a particular sub-term of this generic arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term<T: Element>(
        &self,
        location: SubSymbolLocation,
    ) -> Option<&T> {
        let generic_arguments = T::get(&self.generic_arguments);

        generic_arguments.get(location.0)
    }
}

impl GenericArguments {
    /// Substructurally matches `self` to `to`.
    pub fn substructural_match<L, T, C, I, Y>(
        &self,
        other: &Self,
        mut existing: Substructural<L, T, C, I>,
        to_location: impl Fn(usize) -> Y,
    ) -> Option<Substructural<L, T, C, I>>
    where
        Y: Into<L> + Into<T> + Into<C> + Into<I> + Copy,
    {
        if self.lifetimes.len() != other.lifetimes.len()
            || self.types.len() != other.types.len()
            || self.constants.len() != other.constants.len()
            || self.instancces.len() != other.instancces.len()
        {
            return None;
        }

        for (idx, (lhs, rhs)) in self
            .lifetimes
            .iter()
            .cloned()
            .zip(other.lifetimes.iter().cloned())
            .enumerate()
        {
            let location = to_location(idx);
            existing.lifetimes_mut().push(Matching::new(
                lhs,
                rhs,
                location.into(),
                location.into(),
            ));
        }

        for (idx, (lhs, rhs)) in self
            .types
            .iter()
            .cloned()
            .zip(other.types.iter().cloned())
            .enumerate()
        {
            let location = to_location(idx);
            existing.types_mut().push(Matching::new(
                lhs,
                rhs,
                location.into(),
                location.into(),
            ));
        }

        for (idx, (lhs, rhs)) in self
            .constants
            .iter()
            .cloned()
            .zip(other.constants.iter().cloned())
            .enumerate()
        {
            let location = to_location(idx);
            existing.constants_mut().push(Matching::new(
                lhs,
                rhs,
                location.into(),
                location.into(),
            ));
        }

        for (idx, (lhs, rhs)) in self
            .instancces
            .iter()
            .cloned()
            .zip(other.instancces.iter().cloned())
            .enumerate()
        {
            let location = to_location(idx);
            existing.instances_mut().push(Matching::new(
                lhs,
                rhs,
                location.into(),
                location.into(),
            ));
        }

        Some(existing)
    }
}

impl GenericArguments {
    /// Checks if there's any errornous term in the generic arguments.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.lifetimes.iter().any(Lifetime::is_error)
            || self.types.iter().any(Type::is_error)
            || self.constants.iter().any(Constant::is_error)
    }
}

impl crate::display::Display for Symbol {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        if formatter.configuration().short_qualified_identifiers() {
            let name = engine.get_name(self.id).await;
            write!(formatter, "{}", &*name)?;
        } else {
            let qualified_name = engine.get_qualified_name(self.id).await;
            write!(formatter, "{qualified_name}")?;
        }

        self.generic_arguments.fmt(engine, formatter).await
    }
}

impl crate::display::Display for AssociatedSymbol {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        let parent_id = self
            .id
            .target_id
            .make_global(engine.get_parent(self.id).await.unwrap());

        let parent_qualified_name = engine.get_qualified_name(parent_id).await;
        write!(formatter, "{parent_qualified_name}")?;
        self.parent_generic_arguments.fmt(engine, formatter).await?;

        let name = engine.get_name(self.id).await;
        write!(formatter, "::{}", &*name)?;

        self.member_generic_arguments.fmt(engine, formatter).await?;

        Ok(())
    }
}

macro_rules! implements_generic_arguments {
    (
        $self:ident,
        $visitor:ident,
        $visit_type:ident,
        $visit_lifetime:ident,
        $visit_constant:ident,
        $visit_instance:ident,
        $iter:ident,
        $map_idx:ident
        $(, $await:ident)?
    ) => {{
        for (id, lifetime) in $self.lifetimes.$iter().enumerate() {
            if !$visitor.$visit_lifetime(
                lifetime,
                Into::<T::SubLifetimeLocation>::into($map_idx(id)).into(),
            )$(.$await)? {
                return false;
            }
        }

        for (idx, ty) in $self.types.$iter().enumerate() {
            if !$visitor.$visit_type(
                ty,
                Into::<T::SubTypeLocation>::into($map_idx(idx)).into(),
            )$(.$await)? {
                return false;
            }
        }

        for (idx, constant) in $self.constants.$iter().enumerate() {
            if !$visitor.$visit_constant(
                constant,
                Into::<T::SubConstantLocation>::into($map_idx(idx)).into(),
            )$(.$await)? {
                return false;
            }
        }

        for (idx, instance) in $self.instancces.$iter().enumerate() {
            if !$visitor.$visit_instance(
                instance,
                Into::<T::SubInstanceLocation>::into($map_idx(idx)).into(),
            )$(.$await)? {
                return false;
            }
        }

        true
    }};
}

impl GenericArguments {
    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) fn accept_one_level<
        'a,
        T: visitor::Element,
        Idx,
        V: Visitor<'a, Lifetime>
            + Visitor<'a, Type>
            + Visitor<'a, Constant>
            + Visitor<'a, Instance>,
    >(
        &'a self,
        visitor: &mut V,
        map_idx: impl Fn(usize) -> Idx,
    ) -> bool
    where
        Idx: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
        T::SubInstanceLocation: Into<SubInstanceLocation>,
    {
        implements_generic_arguments!(
            self, visitor, visit, visit, visit, visit, iter, map_idx
        )
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) async fn accept_one_level_async<
        T: visitor::Element,
        Idx,
        V: AsyncVisitor<Lifetime>
            + AsyncVisitor<Type>
            + AsyncVisitor<Constant>
            + AsyncVisitor<Instance>,
    >(
        &self,
        visitor: &mut V,
        map_idx: impl Fn(usize) -> Idx,
    ) -> bool
    where
        Idx: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
        T::SubInstanceLocation: Into<SubInstanceLocation>,
    {
        implements_generic_arguments!(
            self, visitor, visit, visit, visit, visit, iter, map_idx, await
        )
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) fn accept_one_level_mut<
        T: visitor::Element,
        Idx,
        V: Mutable<Lifetime>
            + Mutable<Type>
            + Mutable<Constant>
            + Mutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
        map_idx: impl Fn(usize) -> Idx,
    ) -> bool
    where
        Idx: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
        T::SubInstanceLocation: Into<SubInstanceLocation>,
    {
        implements_generic_arguments!(
            self, visitor, visit, visit, visit, visit, iter_mut, map_idx
        )
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) async fn accept_one_level_async_mut<
        T: visitor::Element,
        Idx,
        V: AsyncMutable<Lifetime>
            + AsyncMutable<Type>
            + AsyncMutable<Constant>
            + AsyncMutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
        map_idx: impl Fn(usize) -> Idx,
    ) -> bool
    where
        Idx: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
        T::SubInstanceLocation: Into<SubInstanceLocation>,
    {
        implements_generic_arguments!(
            self, visitor, visit, visit, visit, visit, iter_mut, map_idx, await
        )
    }
}

macro_rules! implements_symbol {
    (
        $self:ident,
        $visitor:ident,
        $accept_one_level:ident,
        $visit_type:ident
        $(, $await:ident)?
    ) => {{
        if !$self
            .generic_arguments
            .$accept_one_level::<$visit_type, _, _>(
                $visitor,
                |id| SubSymbolLocation(id),
            )$(.$await)? {
            return false;
        }

        true
    }};
}

impl Symbol {
    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) fn accept_one_level<
        'a,
        T: visitor::Element,
        V: Visitor<'a, Lifetime>
            + Visitor<'a, Type>
            + Visitor<'a, Constant>
            + Visitor<'a, Instance>,
    >(
        &'a self,
        visitor: &mut V,
    ) -> bool
    where
        SubSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
        T::SubInstanceLocation: Into<SubInstanceLocation>,
    {
        implements_symbol!(self, visitor, accept_one_level, T)
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) async fn accept_one_level_async<
        T: visitor::Element,
        V: AsyncVisitor<Lifetime>
            + AsyncVisitor<Type>
            + AsyncVisitor<Constant>
            + AsyncVisitor<Instance>,
    >(
        &self,
        visitor: &mut V,
    ) -> bool
    where
        SubSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
        T::SubInstanceLocation: Into<SubInstanceLocation>,
    {
        implements_symbol!(self, visitor, accept_one_level_async, T, await)
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) fn accept_one_level_mut<
        T: visitor::Element,
        V: Mutable<Lifetime>
            + Mutable<Type>
            + Mutable<Constant>
            + Mutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> bool
    where
        SubSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
        T::SubInstanceLocation: Into<SubInstanceLocation>,
    {
        implements_symbol!(self, visitor, accept_one_level_mut, T)
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) async fn accept_one_level_async_mut<
        T: visitor::Element,
        V: AsyncMutable<Lifetime>
            + AsyncMutable<Type>
            + AsyncMutable<Constant>
            + AsyncMutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> bool
    where
        SubSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
        T::SubInstanceLocation: Into<SubInstanceLocation>,
    {
        implements_symbol!(self, visitor, accept_one_level_async_mut, T, await)
    }
}

macro_rules! implements_associated_symbol {
    (
        $self:ident,
        $visitor:ident,
        $accept_one_level:ident,
        $visit_type:ident
        $(, $await:ident)?
    ) => {
        $self
            .parent_generic_arguments
            .$accept_one_level::<$visit_type, _, _>(
                $visitor,
                |id| SubAssociatedSymbolLocation {
                    index: id,
                    from_parent: true,
                }
            )$(.$await)?
            && $self
                .member_generic_arguments
                .$accept_one_level::<$visit_type, _, _>(
                    $visitor,
                    |id| SubAssociatedSymbolLocation {
                        index: id,
                        from_parent: false,
                    }
                )$(.$await)?
    };
}

impl AssociatedSymbol {
    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) fn accept_one_level<
        'a,
        T: visitor::Element,
        V: Visitor<'a, Lifetime>
            + Visitor<'a, Type>
            + Visitor<'a, Constant>
            + Visitor<'a, Instance>,
    >(
        &'a self,
        visitor: &mut V,
    ) -> bool
    where
        SubAssociatedSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
        T::SubInstanceLocation: Into<SubInstanceLocation>,
    {
        implements_associated_symbol!(self, visitor, accept_one_level, T)
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) async fn accept_one_level_async<
        T: visitor::Element,
        V: AsyncVisitor<Lifetime>
            + AsyncVisitor<Type>
            + AsyncVisitor<Constant>
            + AsyncVisitor<Instance>,
    >(
        &self,
        visitor: &mut V,
    ) -> bool
    where
        SubAssociatedSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
        T::SubInstanceLocation: Into<SubInstanceLocation>,
    {
        implements_associated_symbol!(
            self,
            visitor,
            accept_one_level_async,
            T,
            await
        )
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) fn accept_one_level_mut<
        T: visitor::Element,
        V: Mutable<Lifetime>
            + Mutable<Type>
            + Mutable<Constant>
            + Mutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> bool
    where
        SubAssociatedSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
        T::SubInstanceLocation: Into<SubInstanceLocation>,
    {
        implements_associated_symbol!(self, visitor, accept_one_level_mut, T)
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) async fn accept_one_level_async_mut<
        T: visitor::Element,
        V: AsyncMutable<Lifetime>
            + AsyncMutable<Type>
            + AsyncMutable<Constant>
            + AsyncMutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> bool
    where
        SubAssociatedSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
        T::SubInstanceLocation: Into<SubInstanceLocation>,
    {
        implements_associated_symbol!(
            self,
            visitor,
            accept_one_level_async_mut,
            T,
            await
        )
    }
}
