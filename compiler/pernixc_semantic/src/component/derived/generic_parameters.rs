//! Contains the definition of [`GenericParameters`]

use std::collections::{hash_map::Entry, HashMap};

use getset::Getters;
use paste::paste;
use pernixc_arena::{Arena, ID};
use pernixc_source_file::Span;
use serde::{Deserialize, Serialize};

use crate::{
    component::Derived,
    table::{GlobalID, MemberID},
    term::{
        constant::Constant, generic_arguments::GenericArguments,
        lifetime::Lifetime, r#type::Type, Default, Model,
    },
};

/// A **presistent-derived** component representing the generic parameters
/// declaration of a sybmol e.g. `struct Test['a, X, Y, const C: int32]`.
#[derive(
    Debug, Clone, PartialEq, Eq, Default, Getters, Serialize, Deserialize,
)]
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
    lifetime_order: Vec<ID<LifetimeParameter>>,

    /// The order of the declaration of type parameters.
    #[get = "pub"]
    type_order: Vec<ID<TypeParameter>>,

    /// The order of the declaration of constant parameters.
    #[get = "pub"]
    constant_order: Vec<ID<ConstantParameter>>,

    /// Maps the name of the lifetime parameter to its ID.
    #[get = "pub"]
    lifetime_parameter_ids_by_name: HashMap<String, ID<LifetimeParameter>>,

    /// Maps the name of the type parameter to its ID.
    #[get = "pub"]
    type_parameter_ids_by_name: HashMap<String, ID<TypeParameter>>,

    /// Maps the name of the constant parameter to its ID.
    #[get = "pub"]
    constant_parameter_ids_by_name: HashMap<String, ID<ConstantParameter>>,

    /// List of default type parameters to be used when the generic parameters
    /// are not specified.
    #[get = "pub"]
    default_type_parameters: Vec<Type<Default>>,

    /// List of default constant parameters to be used when the generic
    /// parameters are not
    #[get = "pub"]
    default_constant_parameters: Vec<Constant<Default>>,
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
    fn span(&self) -> Option<&Span>;

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
    ) -> &[ID<Self>];

    /// Gets the map that maps between the name of the generic parameters to its
    /// id.
    fn get_generic_parameters_ids_by_name_map(
        generic_parameters: &GenericParameters,
    ) -> &HashMap<String, ID<Self>>;

    /// Adds a new generic parameter to the list of generic parameters.
    ///
    /// # Errors
    ///
    /// If the generic parameter has a name and it is a duplicate, then it
    /// returns `Err(ID)` where `ID` is the ID of the generic parameter.
    fn add_generic_parameter(
        generic_parameters: &mut GenericParameters,
        parameter: Self,
    ) -> Result<ID<Self>, ID<Self>>;
}

/// Represents a lifetime parameter, denoted by `'a` syntax.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct LifetimeParameter {
    /// The name of the lifetime parameter (if none, then it is anonymous
    /// lifetime parameter )
    pub name: String,

    /// Location of where the lifetime parameter is declared.
    #[serde(skip)]
    pub span: Option<Span>,
}

impl GenericParameter for LifetimeParameter {
    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }

    fn kind() -> GenericKind { GenericKind::Lifetime }

    fn get_generic_parameters_arena(
        generic_parameters: &GenericParameters,
    ) -> &Arena<Self> {
        &generic_parameters.lifetimes
    }

    fn get_generic_parameters_order(
        generic_parameters: &GenericParameters,
    ) -> &[ID<Self>] {
        &generic_parameters.lifetime_order
    }

    fn get_generic_parameters_ids_by_name_map(
        generic_parameters: &GenericParameters,
    ) -> &HashMap<String, ID<Self>> {
        &generic_parameters.lifetime_parameter_ids_by_name
    }

    fn add_generic_parameter(
        generic_parameters: &mut GenericParameters,
        parameter: Self,
    ) -> Result<ID<Self>, ID<Self>> {
        generic_parameters.add_lifetime_parameter(parameter)
    }
}

/// Represents a type parameter, denoted by `T` syntax.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct TypeParameter {
    /// The name of the type parameter.
    pub name: String,

    /// The kind of the type parameter.
    #[serde(skip)]
    pub span: Option<Span>,
}

impl GenericParameter for TypeParameter {
    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }

    fn kind() -> GenericKind { GenericKind::Type }

    fn get_generic_parameters_arena(
        generic_parameters: &GenericParameters,
    ) -> &Arena<Self> {
        &generic_parameters.types
    }

    fn get_generic_parameters_order(
        generic_parameters: &GenericParameters,
    ) -> &[ID<Self>] {
        &generic_parameters.type_order
    }

    fn get_generic_parameters_ids_by_name_map(
        generic_parameters: &GenericParameters,
    ) -> &HashMap<String, ID<Self>> {
        &generic_parameters.type_parameter_ids_by_name
    }

    fn add_generic_parameter(
        generic_parameters: &mut GenericParameters,
        parameter: Self,
    ) -> Result<ID<Self>, ID<Self>> {
        generic_parameters.add_type_parameter(parameter)
    }
}

/// Represents a constant parameter, denoted by `const C: TYPE` syntax.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct ConstantParameter {
    /// The name of the constant parameter.
    pub name: String,

    /// The type of the constant parameter.
    pub r#type: Type<Default>,

    /// The type of the constant parameter.
    #[serde(skip)]
    pub span: Option<Span>,
}

impl GenericParameter for ConstantParameter {
    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }

    fn kind() -> GenericKind { GenericKind::Constant }

    fn get_generic_parameters_arena(
        generic_parameters: &GenericParameters,
    ) -> &Arena<Self> {
        &generic_parameters.constants
    }

    fn get_generic_parameters_order(
        generic_parameters: &GenericParameters,
    ) -> &[ID<Self>] {
        &generic_parameters.constant_order
    }

    fn get_generic_parameters_ids_by_name_map(
        generic_parameters: &GenericParameters,
    ) -> &HashMap<String, ID<Self>> {
        &generic_parameters.constant_parameter_ids_by_name
    }

    fn add_generic_parameter(
        generic_parameters: &mut GenericParameters,
        parameter: Self,
    ) -> Result<ID<Self>, ID<Self>> {
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
            ) -> Result<ID<[< $kind Parameter >]>, ID<[< $kind Parameter >]>> {
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
    ) -> impl ExactSizeIterator<Item = (ID<TypeParameter>, &TypeParameter)>
    {
        self.type_order.iter().copied().map(|x| (x, self.types.get(x).unwrap()))
    }

    /// Returns an iterator of all lifetime parameters that iterates in order as
    /// they are declared.
    #[must_use]
    pub fn lifetime_parameters_as_order(
        &self,
    ) -> impl ExactSizeIterator<Item = (ID<LifetimeParameter>, &LifetimeParameter)>
    {
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
    ) -> impl ExactSizeIterator<Item = (ID<ConstantParameter>, &ConstantParameter)>
    {
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
    pub fn create_identity_generic_arguments<M: Model>(
        &self,
        global_id: GlobalID,
    ) -> GenericArguments<M> {
        GenericArguments {
            lifetimes: self
                .lifetime_order
                .iter()
                .copied()
                .map(|id| {
                    Lifetime::Parameter(MemberID { parent: global_id, id })
                })
                .collect(),
            types: self
                .type_order
                .iter()
                .copied()
                .map(|id| Type::Parameter(MemberID { parent: global_id, id }))
                .collect(),
            constants: self
                .constant_order
                .iter()
                .copied()
                .map(|id| {
                    Constant::Parameter(MemberID { parent: global_id, id })
                })
                .collect(),
        }
    }
}

impl Derived for GenericParameters {
    fn component_name() -> &'static str { "generic parameters" }
}

/// An ID to a type parameter.
pub type TypeParameterID = MemberID<ID<TypeParameter>>;

/// An ID to a constant parameter.
pub type ConstantParameterID = MemberID<ID<ConstantParameter>>;

/// An ID to a lifetime parameter.
pub type LifetimeParameterID = MemberID<ID<LifetimeParameter>>;
