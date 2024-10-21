//! Contains the definition of all symbol kinds in the language.

use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    convert::Into,
    fmt::Debug,
};

use derive_more::{Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use paste::paste;
use pernixc_base::source_file::Span;

use crate::{
    arena::{Arena, Map, ID},
    ir::{self, Suboptimal, Success},
    type_system::{
        model::{Default, Model},
        predicate,
        term::{constant, lifetime, r#type, GenericArguments},
        variance::Variance,
    },
};

pub mod table;

/// Represents an accessibility of a symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum Accessibility {
    /// The symbol is accessible from anywhere.
    #[default]
    Public,

    /// The symbol is accessible from the given module and its children.
    Scoped(ID<Module>),
}

/// Represents a predicate introduced by either a where clause or implication.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Predicate {
    /// The predicate itself.
    pub predicate: predicate::Predicate<Default>,

    /// The span where the predicate was declared.
    pub span: Option<Span>,
}

/// An ID of all kinds of symbols that implements the [`Generic`] trait.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum GenericID {
    Struct(ID<Struct>),
    Trait(ID<Trait>),
    Enum(ID<Enum>),
    Type(ID<Type>),
    Function(ID<Function>),
    Constant(ID<Constant>),
    TraitType(ID<TraitType>),
    TraitFunction(ID<TraitFunction>),
    TraitConstant(ID<TraitConstant>),
    NegativeTraitImplementation(ID<NegativeTraitImplementation>),
    PositiveTraitImplementation(ID<PositiveTraitImplementation>),
    TraitImplementationFunction(ID<TraitImplementationFunction>),
    TraitImplementationType(ID<TraitImplementationType>),
    TraitImplementationConstant(ID<TraitImplementationConstant>),
    AdtImplementation(ID<AdtImplementation>),
    AdtImplementationFunction(ID<AdtImplementationFunction>),
    Marker(ID<Marker>),
    PositiveMarkerImplementation(ID<PositiveMarkerImplementation>),
    NegativeMarkerImplementation(ID<NegativeMarkerImplementation>),
}

macro_rules! from_ids {
    ($from:ident, $to:ident $(, ($from_kind:ident, $to_kind:ident))*) => {
        impl From<$from> for $to {
            fn from(value: $from) -> Self {
                match value {
                    $(
                        $from::$from_kind(id) => Self::$to_kind(id),
                    )*
                }
            }
        }
    }
}

from_ids!(
    GenericID,
    GlobalID,
    (Struct, Struct),
    (Trait, Trait),
    (Enum, Enum),
    (Type, Type),
    (Constant, Constant),
    (Function, Function),
    (TraitType, TraitType),
    (TraitFunction, TraitFunction),
    (TraitConstant, TraitConstant),
    (NegativeTraitImplementation, NegativeTraitImplementation),
    (PositiveTraitImplementation, PositiveTraitImplementation),
    (TraitImplementationFunction, TraitImplementationFunction),
    (TraitImplementationType, TraitImplementationType),
    (TraitImplementationConstant, TraitImplementationConstant),
    (AdtImplementation, AdtImplementation),
    (AdtImplementationFunction, AdtImplementationFunction),
    (Marker, Marker),
    (PositiveMarkerImplementation, PositiveMarkerImplementation),
    (NegativeMarkerImplementation, NegativeMarkerImplementation)
);

macro_rules! try_from_ids {
    ($from:ident, $to:ident $(, ($from_kind:ident, $to_kind:ident))*) => {
        impl TryFrom<$from> for $to {
            type Error = $from;

            #[allow(unreachable_patterns)]
            fn try_from(value: $from) -> Result<Self, Self::Error> {
                match value {
                    $(
                        $from::$from_kind(id) => Ok(Self::$to_kind(id)),
                    )*
                    _ => Err(value)
                }
            }
        }
    };
}

try_from_ids!(
    GlobalID,
    GenericID,
    (Struct, Struct),
    (Trait, Trait),
    (Enum, Enum),
    (Type, Type),
    (Function, Function),
    (Trait, Trait),
    (TraitType, TraitType),
    (TraitFunction, TraitFunction),
    (TraitConstant, TraitConstant),
    (PositiveTraitImplementation, PositiveTraitImplementation),
    (NegativeTraitImplementation, NegativeTraitImplementation),
    (TraitImplementationType, TraitImplementationType),
    (TraitImplementationFunction, TraitImplementationFunction),
    (TraitImplementationConstant, TraitImplementationConstant),
    (AdtImplementation, AdtImplementation),
    (AdtImplementationFunction, AdtImplementationFunction),
    (Marker, Marker),
    (PositiveMarkerImplementation, PositiveMarkerImplementation),
    (NegativeMarkerImplementation, NegativeMarkerImplementation)
);

/// Represents a kind of symbol that accepts generic arguments.
pub trait Generic: Global {
    /// Gets the [`GenericParameters`] of the symbol.
    fn generic_declaration(&self) -> &GenericDeclaration;

    #[doc(hidden)]
    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration;
}

/// An enumeration of all kinds of functions.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum CallableID {
    Function(ID<Function>),
    TraitFunction(ID<TraitFunction>),
    TraitImplementationFunction(ID<TraitImplementationFunction>),
    AdtImplementationFunction(ID<AdtImplementationFunction>),
}

from_ids! {
    CallableID,
    GlobalID,
    (Function, Function),
    (TraitFunction, TraitFunction),
    (TraitImplementationFunction, TraitImplementationFunction),
    (AdtImplementationFunction, AdtImplementationFunction)
}

from_ids! {
    CallableID,
    GenericID,
    (Function, Function),
    (TraitFunction, TraitFunction),
    (TraitImplementationFunction, TraitImplementationFunction),
    (AdtImplementationFunction, AdtImplementationFunction)
}

try_from_ids!(
    GlobalID,
    CallableID,
    (Function, Function),
    (TraitFunction, TraitFunction),
    (TraitImplementationFunction, TraitImplementationFunction),
    (AdtImplementationFunction, AdtImplementationFunction)
);

try_from_ids!(
    GenericID,
    CallableID,
    (Function, Function),
    (TraitFunction, TraitFunction),
    (TraitImplementationFunction, TraitImplementationFunction),
    (AdtImplementationFunction, AdtImplementationFunction)
);

/// A trait implemented by all kinds of functions: [`Function`],
/// [`TraitFunction`], [`TraitImplementationFunction`], and
/// [`AdtImplementationFunction`].
pub trait Callable: Generic {
    /// The parameters defined in the funciton.
    fn parameters(&self) -> &Arena<Parameter>;

    #[doc(hidden)]
    fn parameters_mut(&mut self) -> &mut Arena<Parameter>;

    /// The array of parameter IDs that are stored in order of the declaration.
    fn parameter_order(&self) -> &[ID<Parameter>];

    #[doc(hidden)]
    fn parameter_order_mut(&mut self) -> &mut Vec<ID<Parameter>>;

    /// The return type of the function.
    fn return_type(&self) -> &r#type::Type<Default>;

    #[doc(hidden)]
    fn return_type_mut(&mut self) -> &mut r#type::Type<Default>;
}

impl<ParentID: Copy + Into<GlobalID>, Definition> Callable
    for GenericTemplate<ParentID, FunctionTemplate<Definition>>
{
    fn parameters(&self) -> &Arena<Parameter> { &self.parameters }

    fn parameter_order(&self) -> &[ID<Parameter>] { &self.parameter_order }

    fn return_type(&self) -> &r#type::Type<Default> { &self.return_type }

    fn parameters_mut(&mut self) -> &mut Arena<Parameter> {
        &mut self.parameters
    }

    fn parameter_order_mut(&mut self) -> &mut Vec<ID<Parameter>> {
        &mut self.parameter_order
    }

    fn return_type_mut(&mut self) -> &mut r#type::Type<Default> {
        &mut self.return_type
    }
}

/// Represents a kind of symbol that defines a algebraic data type.
///
/// This primarily includes [`Struct`] and [`Enum`].
#[allow(private_bounds)]
pub trait Adt: ImplementedMut<ID<AdtImplementation>> {
    /// Gets the [`GenericParameterVariances`], containing the variances
    /// informations of all the generic parameters.
    fn generic_parameter_variances(&self) -> &GenericParameterVariances;
}

/// Represents a generic declaration containing generic parameters and
/// predicates.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct GenericDeclaration {
    /// Generic parameters defined in the generic declaration.
    pub parameters: GenericParameters,

    /// Contains all the predicates required by the generic declaration.
    pub predicates: Vec<Predicate>,
}

/// An ID of all kinds of symbols that implements the [`Global`] trait.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(missing_docs)]
pub enum GlobalID {
    Module(ID<Module>),
    Struct(ID<Struct>),
    Trait(ID<Trait>),
    Enum(ID<Enum>),
    Type(ID<Type>),
    Constant(ID<Constant>),
    Function(ID<Function>),
    Variant(ID<Variant>),
    TraitType(ID<TraitType>),
    TraitFunction(ID<TraitFunction>),
    TraitConstant(ID<TraitConstant>),
    PositiveTraitImplementation(ID<PositiveTraitImplementation>),
    NegativeTraitImplementation(ID<NegativeTraitImplementation>),
    TraitImplementationFunction(ID<TraitImplementationFunction>),
    TraitImplementationType(ID<TraitImplementationType>),
    TraitImplementationConstant(ID<TraitImplementationConstant>),
    AdtImplementation(ID<AdtImplementation>),
    AdtImplementationFunction(ID<AdtImplementationFunction>),
    Marker(ID<Marker>),
    PositiveMarkerImplementation(ID<PositiveMarkerImplementation>),
    NegativeMarkerImplementation(ID<NegativeMarkerImplementation>),
}

/// The private trait that is implemented by all symbols that can be implemented
/// by `implements SYMBOL` syntax.
pub trait Implemented<ImplementationID>: Generic {
    /// The list of implementations on the symbol.
    fn implementations(&self) -> &HashSet<ImplementationID>;
}

trait ImplementedMut<ImplementationID>: Implemented<ImplementationID> {
    /// The list of implementations on the symbol.
    fn implementations_mut(&mut self) -> &mut HashSet<ImplementationID>;
}

/// The private trait that is implemented by all symbols that can contain
/// members in its scope.
pub trait Parent {
    /// The type of the member ID.
    type MemberID: Debug
        + Clone
        + Copy
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
}

trait ParentSealed: Parent {
    /// Maps the name of the member to its ID.
    #[allow(unused)]
    fn member_ids_by_name(&self) -> &HashMap<String, Self::MemberID>;

    /// Maps the name of the member to its ID.
    fn member_ids_by_name_mut(
        &mut self,
    ) -> &mut HashMap<String, Self::MemberID>;
}

/// Represents a kind of symbol that has a clear hierarchy/name and can be
/// referenced globally by a qualified name.
pub trait Global {
    /// The name of the symbol.
    fn name(&self) -> &str;

    /// The ID of the parent symbol.
    fn parent_global_id(&self) -> Option<GlobalID>;

    /// Location of where the symbol is declared.
    fn span(&self) -> Option<&Span>;
}

/// An ID to all kinds of symbols that can be defined in a module.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum ModuleMemberID {
    Module(ID<Module>),
    Enum(ID<Enum>),
    Struct(ID<Struct>),
    Trait(ID<Trait>),
    Type(ID<Type>),
    Function(ID<Function>),
    Constant(ID<Constant>),
    Marker(ID<Marker>),
}

/// A template for defining a global symbol with generic declaration.
#[derive(
    Debug, Clone, PartialEq, Eq, Default, CopyGetters, Getters, Deref, DerefMut,
)]
pub struct GenericTemplate<ParentID: Copy, Definition> {
    /// The name of the symbol
    #[get = "pub"]
    name: String,

    /// The accessibility of the symbol.
    #[get_copy = "pub"]
    accessibility: Accessibility,

    /// The ID of the parent symbol.
    #[get_copy = "pub"]
    parent_id: ParentID,

    /// Span to the identifier that defines the symbol.
    #[get = "pub"]
    span: Option<Span>,

    /// The generic declaration of the symbol.
    pub generic_declaration: GenericDeclaration,

    /// The definition of the symbol.
    #[deref]
    #[deref_mut]
    pub definition: Definition,
}

impl<ParentID: Copy + Into<GlobalID>, Definition> Global
    for GenericTemplate<ParentID, Definition>
{
    fn name(&self) -> &str { &self.name }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(self.parent_id.into())
    }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

impl<ParentID: Copy + Into<GlobalID>, Definition> Generic
    for GenericTemplate<ParentID, Definition>
{
    fn generic_declaration(&self) -> &GenericDeclaration {
        &self.generic_declaration
    }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

/// A template for defining struct and enum.
#[derive(Debug, Clone, PartialEq, Eq, Default, Deref, DerefMut, Getters)]
pub struct AdtTemplate<Definition> {
    /// The variances of the generic parameters.
    pub generic_parameter_variances: GenericParameterVariances,

    /// The list of implementations on the adt.
    #[get = "pub"]
    implementations: HashSet<ID<AdtImplementation>>,

    /// The definition for the struct or enum.
    #[deref]
    #[deref_mut]
    pub definition: Definition,
}

impl<ParentID: Copy + Into<GlobalID>, Definition> Adt
    for GenericTemplate<ParentID, AdtTemplate<Definition>>
{
    fn generic_parameter_variances(&self) -> &GenericParameterVariances {
        &self.generic_parameter_variances
    }
}

from_ids!(
    ModuleMemberID,
    GlobalID,
    (Module, Module),
    (Enum, Enum),
    (Struct, Struct),
    (Trait, Trait),
    (Type, Type),
    (Function, Function),
    (Constant, Constant),
    (Marker, Marker)
);

/// Represents a module declaration.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct Module {
    /// The name of the module declaration.
    #[get = "pub"]
    name: String,

    /// The accessibility of the module.
    #[get_copy = "pub"]
    accessibility: Accessibility,

    /// The ID of the parent module.
    ///
    /// If this module is a root module, then this field is `None`.
    #[get_copy = "pub"]
    parent_module_id: Option<ID<Module>>,

    /// Maps the name of the module member to its ID.
    #[get = "pub"]
    member_ids_by_name: HashMap<String, ModuleMemberID>,

    /// Location of where the module is declared.
    #[get = "pub"]
    span: Option<Span>,

    /// The list of imports that are used by the module.
    #[get = "pub"]
    imports: HashMap<String, (ModuleMemberID, Option<Span>)>,
}

impl Parent for Module {
    type MemberID = ModuleMemberID;
}

impl ParentSealed for Module {
    fn member_ids_by_name(&self) -> &HashMap<String, Self::MemberID> {
        &self.member_ids_by_name
    }

    fn member_ids_by_name_mut(
        &mut self,
    ) -> &mut HashMap<String, Self::MemberID> {
        &mut self.member_ids_by_name
    }
}

impl Global for Module {
    fn name(&self) -> &str { &self.name }

    fn parent_global_id(&self) -> Option<GlobalID> {
        self.parent_module_id.map(GlobalID::Module)
    }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

/// Implemented by all generic parameters [`LifetimeParameter`],
/// [`TypeParameter`], and [`ConstantParameter`].
pub trait GenericParameter: Sized + 'static {
    /// Gets the name of the generic parameter.
    ///
    /// If the generic parameter is anonymous, (i.e. elided lifetime parameter),
    /// then this method returns `None`.
    fn name(&self) -> Option<&str>;

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

/// An ID used to refer to a particular symbol defined in a particular
/// **parent** symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberID<ChildID, ParentID> {
    /// Symbol ID of the parent symbol.
    pub parent: ParentID,

    /// Symbol ID of the child symbol.
    pub id: ChildID,
}

/// An ID to a type parameter.
pub type TypeParameterID = MemberID<ID<TypeParameter>, GenericID>;

/// An ID to a constant parameter.
pub type ConstantParameterID = MemberID<ID<ConstantParameter>, GenericID>;

/// An ID to a lifetime parameter.
pub type LifetimeParameterID = MemberID<ID<LifetimeParameter>, GenericID>;

/// Represents a lifetime parameter, denoted by `'a` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeParameter {
    /// The name of the lifetime parameter (if none, then it is anonymous
    /// lifetime parameter )
    pub name: Option<String>,

    /// Location of where the lifetime parameter is declared.
    pub span: Option<Span>,
}

impl GenericParameter for LifetimeParameter {
    fn name(&self) -> Option<&str> { self.name.as_ref().map(AsRef::as_ref) }

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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeParameter {
    /// The name of the type parameter.
    pub name: Option<String>,

    /// The kind of the type parameter.
    pub span: Option<Span>,
}

impl GenericParameter for TypeParameter {
    fn name(&self) -> Option<&str> { self.name.as_ref().map(AsRef::as_ref) }

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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantParameter {
    /// The name of the constant parameter.
    pub name: Option<String>,

    /// The type of the constant parameter.
    pub r#type: r#type::Type<Default>,

    /// The type of the constant parameter.
    pub span: Option<Span>,
}

impl GenericParameter for ConstantParameter {
    fn name(&self) -> Option<&str> { self.name.as_ref().map(AsRef::as_ref) }

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

/// Contains the [`Variance`] informations for all the generic parameters.
///
/// This is primarily used in algebraic data types declarations.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct GenericParameterVariances {
    /// Maps the lifetime parameter ID to its variance.
    pub variances_by_lifetime_ids: HashMap<ID<LifetimeParameter>, Variance>,

    /// Maps the type parameter ID to its variance.
    pub variances_by_type_ids: HashMap<ID<TypeParameter>, Variance>,
}

/// Represents a list of generic parameters.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
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
    default_type_parameters: Vec<r#type::Type<Default>>,

    /// List of default constant parameters to be used when the generic
    /// parameters are not
    #[get = "pub"]
    default_constant_parameters: Vec<constant::Constant<Default>>,
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
                let entry = if let Some(name) = parameter.name() {
                    match $self
                        .[< $kind:snake _parameter_ids_by_name >]
                        .entry(name.to_owned()) {
                        Entry::Vacant(entry) => Some(entry),
                        Entry::Occupied(entry) => {
                            return Err(*entry.get());
                        }
                    }
                } else {
                    None
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
        generic_id: GenericID,
    ) -> GenericArguments<M> {
        GenericArguments {
            lifetimes: self
                .lifetime_order
                .iter()
                .copied()
                .map(|id| {
                    lifetime::Lifetime::Parameter(MemberID {
                        parent: generic_id,
                        id,
                    })
                })
                .collect(),
            types: self
                .type_order
                .iter()
                .copied()
                .map(|id| {
                    r#type::Type::Parameter(MemberID { parent: generic_id, id })
                })
                .collect(),
            constants: self
                .constant_order
                .iter()
                .copied()
                .map(|id| {
                    constant::Constant::Parameter(MemberID {
                        parent: generic_id,
                        id,
                    })
                })
                .collect(),
        }
    }
}

/// Represents a field declaration in the struct, denoted by `NAME: TYPE`
/// syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field {
    /// The accessibility of the field.
    pub accessibility: Accessibility,

    /// The name of the field.
    pub name: String,

    /// The type of the field.
    pub r#type: r#type::Type<Default>,

    /// Location of where the field is declared.
    pub span: Option<Span>,
}

/// Represents a struct declaration, denoted by `struct NAME { ... }` syntax.
pub type Struct = GenericTemplate<ID<Module>, AdtTemplate<StructDefinition>>;

impl<ParentID: Copy + Into<GlobalID>, AdtDefinition>
    Implemented<ID<AdtImplementation>>
    for GenericTemplate<ParentID, AdtTemplate<AdtDefinition>>
{
    fn implementations(&self) -> &HashSet<ID<AdtImplementation>> {
        &self.implementations
    }
}

impl<ParentID: Copy + Into<GlobalID>, AdtDefinition>
    ImplementedMut<ID<AdtImplementation>>
    for GenericTemplate<ParentID, AdtTemplate<AdtDefinition>>
{
    fn implementations_mut(&mut self) -> &mut HashSet<ID<AdtImplementation>> {
        &mut self.implementations
    }
}

/// Contains the definition of the struct.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
pub struct StructDefinition {
    /// Contains all the fields defined in the struct.
    #[get = "pub"]
    fields: Map<Field>,

    /// The order of the fields in the struct in order of declaration.
    #[get = "pub"]
    field_declaration_order: Vec<ID<Field>>,
}

impl StructDefinition {
    /// Inserts a new field to the struct.
    ///
    /// # Errors
    ///
    /// Returns the ID of the existing field with the same name and the passed
    /// field.
    ///
    /// # Returns
    ///
    /// Returns the ID of the field if the field is successfully inserted.
    pub fn insert_field(
        &mut self,
        field: Field,
    ) -> Result<ID<Field>, (ID<Field>, Field)> {
        match self.fields.insert(field.name.clone(), field) {
            Ok(id) => {
                self.field_declaration_order.push(id);
                Ok(id)
            }
            Err((field_id, field)) => Err((field_id, field)),
        }
    }

    /// Returns an iterator of all fields that iterates in order as they are
    /// declared.
    #[must_use]
    pub fn fields_as_order(
        &self,
    ) -> impl ExactSizeIterator<Item = (ID<Field>, &Field)> {
        self.field_declaration_order
            .iter()
            .copied()
            .map(move |id| (id, self.fields.get(id).unwrap()))
    }
}

/// Represents an enum variant declaration, denoted by `NAME(ASSOC_TYPE)`
/// syntax.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters, CopyGetters,
)]
pub struct Variant {
    /// The name of the variant.
    #[get = "pub"]
    name: String,

    /// The type of the associated value of the variant (if any).
    pub associated_type: Option<r#type::Type<Default>>,

    /// The parent enum ID.
    #[get_copy = "pub"]
    parent_enum_id: ID<Enum>,

    /// The span where the variant is declared.
    #[get = "pub"]
    span: Option<Span>,
}

impl Global for Variant {
    fn name(&self) -> &str { &self.name }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Enum(self.parent_enum_id))
    }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

/// Represents an enum declaration, denoted by `enum NAME { ... }` syntax.
pub type Enum = GenericTemplate<ID<Module>, AdtTemplate<EnumDefinition>>;

impl Parent for Enum {
    type MemberID = ID<Variant>;
}

impl ParentSealed for Enum {
    fn member_ids_by_name(&self) -> &HashMap<String, Self::MemberID> {
        &self.variant_ids_by_name
    }

    fn member_ids_by_name_mut(
        &mut self,
    ) -> &mut HashMap<String, Self::MemberID> {
        &mut self.variant_ids_by_name
    }
}

/// Contains the definition of the enum.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters, CopyGetters)]
pub struct EnumDefinition {
    /// Maps the name of the variants defined in the enum to its ID.
    #[get = "pub"]
    variant_ids_by_name: HashMap<String, ID<Variant>>,

    /// The declaration order of the variants in the enum.
    #[get = "pub"]
    variant_declaration_order: Vec<ID<Variant>>,
}

/// Contains the definition for the regular type declaration i.e. those that are
/// declared in the module level.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters, CopyGetters)]
pub struct TypeDefinition {
    /// Type type aliased
    pub r#type: r#type::Type<Default>,
}

/// Represents a regular type declaration i.e. those that are declared in the
/// module level. `type NAME = TYPE` syntax.
pub type Type = GenericTemplate<ID<Module>, TypeDefinition>;

/// A template struct representing all kinds of constant declarations.
#[derive(
    Debug, Clone, PartialEq, Eq, Default, Deref, DerefMut, Getters, CopyGetters,
)]
pub struct ConstantTemplate<Definition> {
    /// The type of the constant.
    pub r#type: r#type::Type<Default>,

    /// The definition of the constant.
    #[deref]
    #[deref_mut]
    pub definition: Definition,
}

/// Contains the data for the regular constant declaration i.e. those that are
/// declared in the module level or in the adt implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, CopyGetters)]
pub struct ConstantDefinition {
    // TODO: Constant IR
}

/// Represents a constant declaration, denoted by `const NAME: TYPE = VALUE`
/// syntax.
pub type Constant =
    GenericTemplate<ID<Module>, ConstantTemplate<ConstantDefinition>>;

/// A template struct representing all kinds of function declarations.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Deref,
    DerefMut,
    Getters,
    CopyGetters,
    derive_more::Index,
    derive_more::IndexMut,
)]
pub struct FunctionTemplate<Definition> {
    /// The parameters of the function.
    #[index]
    #[index_mut]
    #[get = "pub"]
    parameters: Arena<Parameter>,

    /// The order of the parameters of the function in order of declaration.
    #[get = "pub"]
    parameter_order: Vec<ID<Parameter>>,

    /// The return type of the function.
    pub return_type: r#type::Type<Default>,

    /// The definition of the function.
    #[deref]
    #[deref_mut]
    pub definition: Definition,
}

impl<Definition> FunctionTemplate<Definition> {
    /// Iterates through the parameters of the function and their IDs in order.
    pub fn parameter_as_order(
        &self,
    ) -> impl ExactSizeIterator<Item = (ID<Parameter>, &'_ Parameter)> {
        self.parameter_order
            .iter()
            .copied()
            .map(|id| (id, self.parameters.get(id).unwrap()))
    }

    /// Iterates through the IDs of the parameters of the function in order.
    pub fn parameter_ids(&self) -> impl Iterator<Item = ID<Parameter>> + '_ {
        self.parameter_order.iter().copied()
    }

    /// Inserts a new parameter to the function.
    pub fn insert_parameter(&mut self, parameter: Parameter) -> ID<Parameter> {
        let id = self.parameters.insert(parameter);
        self.parameter_order.push(id);
        id
    }
}

/// An enumeration of all compiler intrinsics functions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Intrinsic {
    /// `memcpy` from the reference stored im the first parameter and returns
    /// it.
    ///
    /// This primarily used for `core::Clone` trait implementation for
    /// primitive types.
    Memcpy,

    /// Invokes each `core::Clone` trait implementation for each element in the
    /// array and returns the copied array.
    ///
    /// This is used for `core::Clone` trait implementation for array types.
    ArrayClone,

    /// Invokes each `core::Clone` trait implementation for each element in
    /// the tuple and returns the copied tuple.
    ///
    /// This is used for `core::Clone` trait implementation for tuple types.
    TupleClone,
}

/// Represents the external linkage of the function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Extern {
    /// The function is an external function that is implemented in the c call
    /// convention.
    C,

    /// Unknown external linkage.
    Unknown,
}

/// Represents an intermediate representation of the function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionIR {
    /// The intermediate representation contains the fully/correctly
    /// representation of the function.
    Success(ir::IR<Success>),

    /// The function contains a semantic error, which causes the function may
    /// not be able to be compiled to its correct meaning.
    Suboptimal(ir::IR<Suboptimal>),

    /// The funcrion is an intrinsic function.
    Intrinsic(Intrinsic),
}

impl std::default::Default for FunctionIR {
    fn default() -> Self { Self::Suboptimal(ir::IR::default()) }
}

/// Contains the data for the function declaration in the ADT implements block.
#[derive(Debug, Clone, PartialEq, Eq, Default, CopyGetters)]
pub struct AdtFunctionDefinition {
    /// Indicates whether the function is a constant function.
    pub const_function: bool,

    /// The intermediate representation of the function.
    pub ir: FunctionIR,
}

/// Contains the data for the function declaration in the module level.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
#[allow(clippy::large_enum_variant)]
pub enum FunctionDefinition {
    /// The function was defined regularly.
    Regular {
        /// Indicates whether the function is a constant function.
        const_function: bool,

        /// The intermediate representation of the function.
        ir: FunctionIR,
    },

    /// The function is an external linkage to the foreign function.
    Extern(Extern),
}

impl std::default::Default for FunctionDefinition {
    fn default() -> Self {
        Self::Regular { const_function: false, ir: FunctionIR::default() }
    }
}

/// Represents a regular function declaration i.e. those that are declared in
/// the module level. `function NAME(...) {...}` syntax.
pub type Function =
    GenericTemplate<ID<Module>, FunctionTemplate<FunctionDefinition>>;

/// Represents an enum implementation function, denoted by `function NAME(...)
/// {...}` syntax.
pub type AdtImplementationFunction = GenericTemplate<
    ID<AdtImplementation>,
    FunctionTemplate<AdtFunctionDefinition>,
>;

/// A template struct representing all kinds of implementation declarations.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Deref,
    DerefMut,
    Getters,
    CopyGetters,
)]
pub struct ImplementationTemplate<ImplementedID: Copy, Definition> {
    /// The generic arguments supplied to the implemented symbol.
    pub arguments: GenericArguments<Default>,

    /// The symbol that is being implemented.
    #[get_copy = "pub"]
    implemented_id: ImplementedID,

    /// The definition of the implementation.
    #[deref]
    #[deref_mut]
    pub definition: Definition,
}

impl<ParentID: Copy + Into<GlobalID>, ImplementedID: Copy, Definition>
    Implementation
    for GenericTemplate<
        ParentID,
        ImplementationTemplate<ImplementedID, Definition>,
    >
{
    fn arguments(&self) -> &GenericArguments<Default> { &self.arguments }
}

/// Contains the definition for the adt implementation
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
pub struct AdtImplementationDefinition {
    /// Maps the name of the adt implementation member to its ID.
    #[get = "pub"]
    member_ids_by_name: HashMap<String, ID<AdtImplementationFunction>>,
}

/// Enumeration of either struct or enum.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum AdtID {
    Struct(ID<Struct>),
    Enum(ID<Enum>),
}

from_ids!(AdtID, GlobalID, (Struct, Struct), (Enum, Enum));
from_ids!(AdtID, GenericID, (Struct, Struct), (Enum, Enum));

/// Represents a adt implementation, denoted by `implements<PARAM> adt<PARAM> {
/// ... }` syntax.
pub type AdtImplementation = GenericTemplate<
    ID<Module>,
    ImplementationTemplate<AdtID, AdtImplementationDefinition>,
>;

impl Parent for AdtImplementation {
    type MemberID = ID<AdtImplementationFunction>;
}

impl ParentSealed for AdtImplementation {
    fn member_ids_by_name(&self) -> &HashMap<String, Self::MemberID> {
        &self.member_ids_by_name
    }

    fn member_ids_by_name_mut(
        &mut self,
    ) -> &mut HashMap<String, Self::MemberID> {
        &mut self.member_ids_by_name
    }
}

/// Negative trait implementation data tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NegativeTraitImplementationDefinition {
    /// Indicates whether the trait implementation is a final implementation.
    pub is_final: bool,
}

/// Represents a negative trait implementation, denoted by
/// `implements<PARAM> TRAIT<PARAM> = delete;` syntax.
pub type NegativeTraitImplementation = GenericTemplate<
    ID<Module>,
    ImplementationTemplate<ID<Trait>, NegativeTraitImplementationDefinition>,
>;

impl ResolvableImplementation<ID<Trait>> for NegativeTraitImplementation {
    fn implemented_id(&self) -> ID<Trait> { self.implemented_id }

    fn is_final(&self) -> bool { self.is_final }
}

/// Contains the data for the trait implementation function declaration.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TraitImplementationFunctionDefinition {
    /// The intermediate representation of the function.
    pub ir: FunctionIR,
}

/// Contains the data for the trait implementation type declaration.
pub type TraitImplementationTypeDefinition = TypeDefinition;

/// Contains the data for the trait implementation constant declaration.
pub type TraitImplementationConstantDefinition = ConstantDefinition;

/// Represents a function declaration as an implements member, denoted by
/// `function NAME(...) {...}` syntax.
pub type TraitImplementationFunction = GenericTemplate<
    ID<PositiveTraitImplementation>,
    FunctionTemplate<TraitImplementationFunctionDefinition>,
>;

/// Represents a type declaration as an implements member, denoted by `type NAME
/// = TYPE;` syntax.
pub type TraitImplementationType = GenericTemplate<
    ID<PositiveTraitImplementation>,
    TraitImplementationTypeDefinition,
>;

/// Represents a constant declaration as an implements member, denoted by `const
/// NAME: TYPE` syntax.
pub type TraitImplementationConstant = GenericTemplate<
    ID<PositiveTraitImplementation>,
    ConstantTemplate<TraitImplementationConstantDefinition>,
>;

/// An ID to all kinds of symbols that can be defined in a trait implementation.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum TraitImplementationMemberID {
    Type(ID<TraitImplementationType>),
    Function(ID<TraitImplementationFunction>),
    Constant(ID<TraitImplementationConstant>),
}

from_ids!(
    TraitImplementationMemberID,
    GlobalID,
    (Type, TraitImplementationType),
    (Function, TraitImplementationFunction),
    (Constant, TraitImplementationConstant)
);

from_ids!(
    TraitImplementationMemberID,
    GenericID,
    (Type, TraitImplementationType),
    (Function, TraitImplementationFunction),
    (Constant, TraitImplementationConstant)
);

/// Represents a trait implementation, denoted by `implements<PARAM>
/// TRAIT<PARAM> { ... }` syntax.
pub type PositiveTraitImplementation = GenericTemplate<
    ID<Module>,
    ImplementationTemplate<ID<Trait>, PositiveTraitImplementationDefinition>,
>;

impl ResolvableImplementation<ID<Trait>> for PositiveTraitImplementation {
    fn implemented_id(&self) -> ID<Trait> { self.implemented_id }

    fn is_final(&self) -> bool { self.is_final }
}

impl Parent for PositiveTraitImplementation {
    type MemberID = TraitImplementationMemberID;
}

impl ParentSealed for PositiveTraitImplementation {
    fn member_ids_by_name(&self) -> &HashMap<String, Self::MemberID> {
        &self.member_ids_by_name
    }

    fn member_ids_by_name_mut(
        &mut self,
    ) -> &mut HashMap<String, Self::MemberID> {
        &mut self.member_ids_by_name
    }
}

/// Contains the implementation definition of a trait implementation symbol
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
pub struct PositiveTraitImplementationDefinition {
    /// Indicates whether the trait implementation is a constant
    /// implementation.
    pub is_const: bool,

    /// Indicates whether the trait implementation is a final implementation.
    pub is_final: bool,

    /// Maps the name of the trait member to its ID.
    #[get = "pub"]
    member_ids_by_name: HashMap<String, TraitImplementationMemberID>,
}

/// Represents a function parameter in the function signature, denoted by
/// `PATTERN: TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter {
    /// The type of the parameter.
    pub r#type: r#type::Type<Default>,

    /// Location of where the parameter is declared.
    pub span: Option<Span>,
}

/// Represents a type declaration as a trait member, denoted by `type NAME;`
/// syntax.
pub type TraitType = GenericTemplate<ID<Trait>, TraitTypeDefinition>;

/// Trait type data tag.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Default,
    Hash,
    CopyGetters,
)]
pub struct TraitTypeDefinition;

/// Represents a function declaration as a trait member, denoted by `function
/// NAME(...) {...}` syntax.
pub type TraitFunction =
    GenericTemplate<ID<Trait>, FunctionTemplate<TraitFunctionDefinition>>;

/// Trait function data tag.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    CopyGetters,
)]
pub struct TraitFunctionDefinition;

/// Represents a constant declaration as a trait member, denoted by `const NAME:
/// TYPE` syntax.
pub type TraitConstant =
    GenericTemplate<ID<Trait>, ConstantTemplate<TraitConstantDefinition>>;

/// Trait constant data tag.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    CopyGetters,
)]
pub struct TraitConstantDefinition;

/// An enumeration containing all an ID to all kinds of symbols that can be
/// defined in a trait.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum TraitMemberID {
    Type(ID<TraitType>),
    Function(ID<TraitFunction>),
    Constant(ID<TraitConstant>),
}

from_ids!(
    TraitMemberID,
    GlobalID,
    (Type, TraitType),
    (Function, TraitFunction),
    (Constant, TraitConstant)
);

from_ids!(
    TraitMemberID,
    GenericID,
    (Type, TraitType),
    (Function, TraitFunction),
    (Constant, TraitConstant)
);

/// Represents a trait declaration, denoted by `trait NAME { ... }` syntax.
pub type Trait = GenericTemplate<ID<Module>, TraitDefinition>;

impl Implemented<TraitImplementationID> for Trait {
    fn implementations(&self) -> &HashSet<TraitImplementationID> {
        &self.implementations
    }
}

impl ImplementedMut<TraitImplementationID> for Trait {
    fn implementations_mut(&mut self) -> &mut HashSet<TraitImplementationID> {
        &mut self.implementations
    }
}

impl Parent for Trait {
    type MemberID = TraitMemberID;
}

impl ParentSealed for Trait {
    fn member_ids_by_name(&self) -> &HashMap<String, Self::MemberID> {
        &self.member_ids_by_name
    }

    fn member_ids_by_name_mut(
        &mut self,
    ) -> &mut HashMap<String, Self::MemberID> {
        &mut self.member_ids_by_name
    }
}

/// Contains the definition of the trait.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
pub struct TraitDefinition {
    /// Maps the name of the trait member to its ID.
    #[get = "pub"]
    member_ids_by_name: HashMap<String, TraitMemberID>,

    /// Maps the ID of the trait type to the ID of the implementation type.
    #[get = "pub"]
    implementations: HashSet<TraitImplementationID>,
}

/// The kind of marker; either 'and' or 'or'.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum MarkerKind {
    /// All the fields must be satisfied to satisfy the marker.
    #[default]
    And,

    /// At least one of the fields must be satisfied to satisfy the marker.
    Or,
}

/// Contains the definition of the marker.
#[derive(Debug, Clone, PartialEq, Eq, Getters, Default)]
pub struct MarkerDefinition {
    /// The kind of the marker.
    pub kind: MarkerKind,

    /// All the implementations that implemented this marker.
    #[get = "pub"]
    implementations: HashSet<MarkerImplementationID>,
}

/// Represents a marker declaration, denoted by `marker NAME;` syntax.
pub type Marker = GenericTemplate<ID<Module>, MarkerDefinition>;

/// The definition of the positive marker implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PositiveMarkerImplementationDefinition;

/// The definition of the negative marker implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NegativeMarkerImplementationDefinition;

/// Represents a positive marker implementation, denoted by `implements[PARAM]
/// MARKER[PARAM];`.
pub type PositiveMarkerImplementation = GenericTemplate<
    ID<Module>,
    ImplementationTemplate<ID<Marker>, PositiveMarkerImplementationDefinition>,
>;

impl ResolvableImplementation<ID<Marker>> for PositiveMarkerImplementation {
    fn implemented_id(&self) -> ID<Marker> { self.implemented_id }

    fn is_final(&self) -> bool { true }
}

/// Represents a negative marker implementation, denoted by `implements[PARAM]
/// MARKER[PARAM] delete;`.
pub type NegativeMarkerImplementation = GenericTemplate<
    ID<Module>,
    ImplementationTemplate<ID<Marker>, NegativeMarkerImplementationDefinition>,
>;

impl ResolvableImplementation<ID<Marker>> for NegativeMarkerImplementation {
    fn implemented_id(&self) -> ID<Marker> { self.implemented_id }

    fn is_final(&self) -> bool { true }
}

/// Enumeration of both positive and negative marker implementation ids.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
#[allow(missing_docs)]
pub enum MarkerImplementationID {
    Positive(ID<PositiveMarkerImplementation>),
    Negative(ID<NegativeMarkerImplementation>),
}

try_from_ids!(
    GlobalID,
    MarkerImplementationID,
    (PositiveMarkerImplementation, Positive),
    (NegativeMarkerImplementation, Negative)
);

/// Enumerations of all kinds of generic parameter ids. This requires the parent
/// [`GenericID`] as a context to know which generic parameter it is.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
#[allow(missing_docs)]
pub enum LocalGenericParameterID {
    Lifetime(ID<LifetimeParameter>),
    Type(ID<TypeParameter>),
    Constant(ID<ConstantParameter>),
}

/// Enumeration of either positive or negative implementation.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum TraitImplementationID {
    Positive(ID<PositiveTraitImplementation>),
    Negative(ID<NegativeTraitImplementation>),
}

from_ids!(
    TraitImplementationID,
    GlobalID,
    (Positive, PositiveTraitImplementation),
    (Negative, NegativeTraitImplementation)
);

from_ids!(
    TraitImplementationID,
    GenericID,
    (Positive, PositiveTraitImplementation),
    (Negative, NegativeTraitImplementation)
);

try_from_ids!(
    GlobalID,
    TraitImplementationID,
    (PositiveTraitImplementation, Positive),
    (NegativeTraitImplementation, Negative)
);

/// The trait implemented by the implementation symbols.
pub trait Implementation: Generic {
    /// The generic arguments supplied to the implemented symbol.
    fn arguments(&self) -> &GenericArguments<Default>;
}

/// The trait implemented by the resolvable implementation symbols.
pub trait ResolvableImplementation<ImplementedID>: Implementation {
    /// The ID of the symbol that is being implemented by this implementation.
    fn implemented_id(&self) -> ImplementedID;

    /// Specifies whether the implementation is final or not.
    fn is_final(&self) -> bool;
}

/// An enumeration of either a trait or marker id.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
#[allow(missing_docs)]
pub enum ResolvableImplementedID {
    Trait(ID<Trait>),
    Marker(ID<Marker>),
}

/// Enumeration of all kinds of resolvable implementation symbol IDs.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
#[allow(missing_docs)]
pub enum ResolvableImplementationID {
    PositiveTrait(ID<PositiveTraitImplementation>),
    NegativeTrait(ID<NegativeTraitImplementation>),
    PositiveMarker(ID<PositiveMarkerImplementation>),
    NegativeMarker(ID<NegativeMarkerImplementation>),
}

impl From<ResolvableImplementationID> for GlobalID {
    fn from(value: ResolvableImplementationID) -> Self {
        match value {
            ResolvableImplementationID::PositiveTrait(id) => {
                Self::PositiveTraitImplementation(id)
            }
            ResolvableImplementationID::NegativeTrait(id) => {
                Self::NegativeTraitImplementation(id)
            }
            ResolvableImplementationID::PositiveMarker(id) => {
                Self::PositiveMarkerImplementation(id)
            }
            ResolvableImplementationID::NegativeMarker(id) => {
                Self::NegativeMarkerImplementation(id)
            }
        }
    }
}

impl From<ResolvableImplementationID> for GenericID {
    fn from(value: ResolvableImplementationID) -> Self {
        match value {
            ResolvableImplementationID::PositiveTrait(id) => {
                Self::PositiveTraitImplementation(id)
            }
            ResolvableImplementationID::NegativeTrait(id) => {
                Self::NegativeTraitImplementation(id)
            }
            ResolvableImplementationID::PositiveMarker(id) => {
                Self::PositiveMarkerImplementation(id)
            }
            ResolvableImplementationID::NegativeMarker(id) => {
                Self::NegativeMarkerImplementation(id)
            }
        }
    }
}

impl From<TraitImplementationID> for ResolvableImplementationID {
    fn from(value: TraitImplementationID) -> Self {
        match value {
            TraitImplementationID::Positive(id) => Self::PositiveTrait(id),
            TraitImplementationID::Negative(id) => Self::NegativeTrait(id),
        }
    }
}

/// Enumeration of all kinds of implementation symbol IDs.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
#[allow(missing_docs)]
pub enum ImplementationID {
    PositiveTrait(ID<PositiveTraitImplementation>),
    NegativeTrait(ID<NegativeTraitImplementation>),
    PositiveMarker(ID<PositiveMarkerImplementation>),
    NegativeMarker(ID<NegativeMarkerImplementation>),
    Adt(ID<AdtImplementation>),
}

impl From<ImplementationID> for GlobalID {
    fn from(value: ImplementationID) -> Self {
        match value {
            ImplementationID::PositiveTrait(id) => {
                Self::PositiveTraitImplementation(id)
            }
            ImplementationID::NegativeTrait(id) => {
                Self::NegativeTraitImplementation(id)
            }
            ImplementationID::PositiveMarker(id) => {
                Self::PositiveMarkerImplementation(id)
            }
            ImplementationID::NegativeMarker(id) => {
                Self::NegativeMarkerImplementation(id)
            }
            ImplementationID::Adt(id) => Self::AdtImplementation(id),
        }
    }
}

impl From<ImplementationID> for GenericID {
    fn from(value: ImplementationID) -> Self {
        match value {
            ImplementationID::PositiveTrait(id) => {
                Self::PositiveTraitImplementation(id)
            }
            ImplementationID::NegativeTrait(id) => {
                Self::NegativeTraitImplementation(id)
            }
            ImplementationID::PositiveMarker(id) => {
                Self::PositiveMarkerImplementation(id)
            }
            ImplementationID::NegativeMarker(id) => {
                Self::NegativeMarkerImplementation(id)
            }
            ImplementationID::Adt(id) => Self::AdtImplementation(id),
        }
    }
}

impl From<TraitImplementationID> for ImplementationID {
    fn from(value: TraitImplementationID) -> Self {
        match value {
            TraitImplementationID::Positive(id) => Self::PositiveTrait(id),
            TraitImplementationID::Negative(id) => Self::NegativeTrait(id),
        }
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

/// Either an ID of an enum or an ID of a struct.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum AlgebraicKind {
    Enum(ID<Enum>),
    Struct(ID<Struct>),
}

impl From<AlgebraicKind> for GlobalID {
    fn from(value: AlgebraicKind) -> Self {
        match value {
            AlgebraicKind::Enum(id) => Self::Enum(id),
            AlgebraicKind::Struct(id) => Self::Struct(id),
        }
    }
}

/// Describes the relationship between two symbols in the hierarchy.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HierarchyRelationship {
    /// The first symbol is the parent of the second symbol.
    Parent,

    /// The first symbol is the child of the second symbol.
    Child,

    /// Both symbols are two equivalent symbols.
    Equivalent,

    /// Both symbols are defined in different hierarchy scope.
    Unrelated,
}
