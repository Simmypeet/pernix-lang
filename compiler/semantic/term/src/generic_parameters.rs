//! Contains the definition of [`GenericParameters`] component.
use std::collections::hash_map::Entry;

use getset::Getters;
use paste::paste;
use pernixc_arena::Arena;
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::Value;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::MemberID;
use pernixc_target::Global;

use crate::{
    constant::Constant, generic_arguments::GenericArguments,
    lifetime::Lifetime, r#type::Type,
};

/// Represents the generic parameters (e.g. `['a, T, const C: TYPE]`).
/// It contains the list of lifetime parameters, type parameters, and constant
/// parameters, as well as the order of their declaration and the maps that
/// maps the name of the generic parameters to their IDs.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Getters,
    StableHash,
    Serialize,
    Deserialize,
    Value,
)]
#[id(Global<pernixc_symbol::ID>)]
#[extend(method(get_generic_parameters))]
pub struct GenericParameters {
    /// List of defined lifetime parameters.
    #[get = "pub"]
    lifetimes: Arena<LifetimeParameter>,

    /// List of defined type parameters.
    #[get = "pub"]
    types: Arena<TypeParameter>,

    /// List of defined constant parameters.
    #[get = "pub"]
    constants: Arena<ConstantParameter>,

    /// The order of the declaration of lifetime parameters.
    #[get = "pub"]
    lifetime_order: Vec<pernixc_arena::ID<LifetimeParameter>>,

    /// The order of the declaration of type parameters.
    #[get = "pub"]
    type_order: Vec<pernixc_arena::ID<TypeParameter>>,

    /// The order of the declaration of constant parameters.
    #[get = "pub"]
    constant_order: Vec<pernixc_arena::ID<ConstantParameter>>,

    /// Maps the name of the lifetime parameter to its ID.
    #[get = "pub"]
    lifetime_parameter_ids_by_name:
        HashMap<String, pernixc_arena::ID<LifetimeParameter>>,

    /// Maps the name of the type parameter to its ID.
    #[get = "pub"]
    type_parameter_ids_by_name:
        HashMap<String, pernixc_arena::ID<TypeParameter>>,

    /// Maps the name of the constant parameter to its ID.
    #[get = "pub"]
    constant_parameter_ids_by_name:
        HashMap<String, pernixc_arena::ID<ConstantParameter>>,

    /// List of default type parameters to be used when the generic parameters
    /// are not specified.
    #[get = "pub"]
    default_type_parameters: Vec<Type>,

    /// List of default constant parameters to be used when the generic
    /// parameters are not
    #[get = "pub"]
    default_constant_parameters: Vec<Constant>,
}

/// Implemented by all generic parameters [`LifetimeParameter`],
/// [`TypeParameter`], and [`ConstantParameter`].
pub trait GenericParameter: Sized + 'static {
    /// Gets the name of the generic parameter.
    ///
    /// If the generic parameter is anonymous, (i.e. elided lifetime parameter),
    /// then this method returns `None`.
    fn name(&self) -> &str;

    /// Gets the span where the generic parameter is declared.
    fn span(&self) -> Option<&RelativeSpan>;

    /// Gets the kind of the generic parameter.
    fn kind() -> GenericKind;

    /// Gets the [`Arena`] of generic parameters of this type from
    /// [`GenericParameters`].
    fn get_generic_parameters_arena(
        generic_parameters: &GenericParameters,
    ) -> &Arena<Self>;

    /// Gets the list of generic parameter id stored in order.
    fn get_generic_parameters_order(
        generic_parameters: &GenericParameters,
    ) -> &[pernixc_arena::ID<Self>];

    /// Gets the map that maps between the name of the generic parameters to its
    /// id.
    fn get_generic_parameters_ids_by_name_map(
        generic_parameters: &GenericParameters,
    ) -> &HashMap<String, pernixc_arena::ID<Self>>;

    /// Adds a new generic parameter to the list of generic parameters.
    ///
    /// # Errors
    ///
    /// If the generic parameter has a name and it is a duplicate, then it
    /// returns `Err(ID)` where `ID` is the ID of the generic parameter.
    fn add_generic_parameter(
        generic_parameters: &mut GenericParameters,
        parameter: Self,
    ) -> Result<pernixc_arena::ID<Self>, pernixc_arena::ID<Self>>;
}

/// Represents a lifetime parameter, denoted by `'a` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct LifetimeParameter {
    /// The name of the lifetime parameter (if none, then it is anonymous
    /// lifetime parameter )
    pub name: String,

    /// Location of where the lifetime parameter is declared.
    pub span: Option<RelativeSpan>,
}

impl GenericParameter for LifetimeParameter {
    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<&RelativeSpan> { self.span.as_ref() }

    fn kind() -> GenericKind { GenericKind::Lifetime }

    fn get_generic_parameters_arena(
        generic_parameters: &GenericParameters,
    ) -> &Arena<Self> {
        &generic_parameters.lifetimes
    }

    fn get_generic_parameters_order(
        generic_parameters: &GenericParameters,
    ) -> &[pernixc_arena::ID<Self>] {
        &generic_parameters.lifetime_order
    }

    fn get_generic_parameters_ids_by_name_map(
        generic_parameters: &GenericParameters,
    ) -> &HashMap<String, pernixc_arena::ID<Self>> {
        &generic_parameters.lifetime_parameter_ids_by_name
    }

    fn add_generic_parameter(
        generic_parameters: &mut GenericParameters,
        parameter: Self,
    ) -> Result<pernixc_arena::ID<Self>, pernixc_arena::ID<Self>> {
        generic_parameters.add_lifetime_parameter(parameter)
    }
}

/// Represents a type parameter, denoted by `T` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct TypeParameter {
    /// The name of the type parameter.
    pub name: String,

    /// The kind of the type parameter.
    pub span: Option<RelativeSpan>,
}

impl GenericParameter for TypeParameter {
    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<&RelativeSpan> { self.span.as_ref() }

    fn kind() -> GenericKind { GenericKind::Type }

    fn get_generic_parameters_arena(
        generic_parameters: &GenericParameters,
    ) -> &Arena<Self> {
        &generic_parameters.types
    }

    fn get_generic_parameters_order(
        generic_parameters: &GenericParameters,
    ) -> &[pernixc_arena::ID<Self>] {
        &generic_parameters.type_order
    }

    fn get_generic_parameters_ids_by_name_map(
        generic_parameters: &GenericParameters,
    ) -> &HashMap<String, pernixc_arena::ID<Self>> {
        &generic_parameters.type_parameter_ids_by_name
    }

    fn add_generic_parameter(
        generic_parameters: &mut GenericParameters,
        parameter: Self,
    ) -> Result<pernixc_arena::ID<Self>, pernixc_arena::ID<Self>> {
        generic_parameters.add_type_parameter(parameter)
    }
}

/// Represents a constant parameter, denoted by `const C: TYPE` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct ConstantParameter {
    /// The name of the constant parameter.
    pub name: String,

    /// The type of the constant parameter.
    pub r#type: Type,

    /// The type of the constant parameter.
    #[serde(skip)]
    pub span: Option<RelativeSpan>,
}

impl GenericParameter for ConstantParameter {
    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<&RelativeSpan> { self.span.as_ref() }

    fn kind() -> GenericKind { GenericKind::Constant }

    fn get_generic_parameters_arena(
        generic_parameters: &GenericParameters,
    ) -> &Arena<Self> {
        &generic_parameters.constants
    }

    fn get_generic_parameters_order(
        generic_parameters: &GenericParameters,
    ) -> &[pernixc_arena::ID<Self>] {
        &generic_parameters.constant_order
    }

    fn get_generic_parameters_ids_by_name_map(
        generic_parameters: &GenericParameters,
    ) -> &HashMap<String, pernixc_arena::ID<Self>> {
        &generic_parameters.constant_parameter_ids_by_name
    }

    fn add_generic_parameter(
        generic_parameters: &mut GenericParameters,
        parameter: Self,
    ) -> Result<pernixc_arena::ID<Self>, pernixc_arena::ID<Self>> {
        generic_parameters.add_constant_parameter(parameter)
    }
}

/// Enumeration of all kinds of generic parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum GenericKind {
    Type,
    Lifetime,
    Constant,
}

macro_rules! implements_add_parameter {
    ($self:ident, $kind:ident) => {
        paste! {
            /// Adds a new generic parameter to the list of generic parameters.
            ///
            /// The generic parameter will be added to the list of generic
            /// parameters and the order of the declaration.
            ///
            /// If the generic parameter has a name, then it will be added to the
            /// map that maps the name of the generic parameter to its ID.
            ///
            /// # Errors
            ///
            /// If the generic parameter has a name and it is not a duplicate, then
            /// it returns `Ok(ID)` where `ID` is the ID of the generic parameter.
            pub fn [<add_ $kind:snake _parameter>](
                &mut $self,
                parameter: [< $kind Parameter >]
            ) -> Result<pernixc_arena::ID<[< $kind Parameter >]>, pernixc_arena::ID<[< $kind Parameter >]>> {
                let entry =
                    match $self
                        .[< $kind:snake _parameter_ids_by_name >]
                        .entry(parameter.name().to_owned()) {
                        Entry::Vacant(entry) => Some(entry),
                        Entry::Occupied(entry) => {
                            return Err(*entry.get());
                        }
                    };

                let id = $self.[< $kind:snake s>].insert(parameter);
                $self.[< $kind:snake _order >].push(id);

                if let Some(entry) = entry {
                    entry.insert(id);
                }

                Ok(id)
            }
        }
    };
}

impl GenericParameters {
    /// Returns an iterator of all type parameters that iterates in order as
    /// they are declared.
    #[must_use]
    pub fn type_parameters_as_order(
        &self,
    ) -> impl ExactSizeIterator<
        Item = (pernixc_arena::ID<TypeParameter>, &TypeParameter),
    > {
        self.type_order.iter().copied().map(|x| (x, self.types.get(x).unwrap()))
    }

    /// Returns an iterator of all lifetime parameters that iterates in order as
    /// they are declared.
    #[must_use]
    pub fn lifetime_parameters_as_order(
        &self,
    ) -> impl ExactSizeIterator<
        Item = (pernixc_arena::ID<LifetimeParameter>, &LifetimeParameter),
    > {
        self.lifetime_order
            .iter()
            .copied()
            .map(|x| (x, self.lifetimes.get(x).unwrap()))
    }

    /// Returns an iterator of all constant parameters that iterates in order as
    /// they are declared.
    #[must_use]
    pub fn constant_parameters_as_order(
        &self,
    ) -> impl ExactSizeIterator<
        Item = (pernixc_arena::ID<ConstantParameter>, &ConstantParameter),
    > {
        self.constant_order
            .iter()
            .copied()
            .map(|x| (x, self.constants.get(x).unwrap()))
    }

    implements_add_parameter!(self, Lifetime);

    implements_add_parameter!(self, Type);

    implements_add_parameter!(self, Constant);

    /// Creates a [`GenericArguments`] that all of its parameters are the
    /// generic parameters of this [`GenericParameters`].
    #[must_use]
    pub fn create_identity_generic_arguments(
        &self,
        global_id: Global<pernixc_symbol::ID>,
    ) -> GenericArguments {
        GenericArguments {
            lifetimes: self
                .lifetime_order
                .iter()
                .copied()
                .map(|id| Lifetime::Parameter(MemberID::new(global_id, id)))
                .collect(),
            types: self
                .type_order
                .iter()
                .copied()
                .map(|id| Type::Parameter(MemberID::new(global_id, id)))
                .collect(),
            constants: self
                .constant_order
                .iter()
                .copied()
                .map(|id| Constant::Parameter(MemberID::new(global_id, id)))
                .collect(),
        }
    }
}

/// An ID to a type parameter.
pub type TypeParameterID = MemberID<pernixc_arena::ID<TypeParameter>>;

/// An ID to a constant parameter.
pub type ConstantParameterID = MemberID<pernixc_arena::ID<ConstantParameter>>;

/// An ID to a lifetime parameter.
pub type LifetimeParameterID = MemberID<pernixc_arena::ID<LifetimeParameter>>;
