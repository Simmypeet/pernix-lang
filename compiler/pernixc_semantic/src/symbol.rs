//! Contains the definition of all symbol kinds in the language.

use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    convert::Into,
};

use derive_more::{Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;
use getset::Getters;
use paste::paste;
use pernixc_base::source_file::Span;
use pernixc_syntax::syntax_tree::AccessModifier;

use crate::{
    arena::{Arena, Map, ID},
    pattern::Irrefutable,
    semantic::{
        predicate,
        term::{constant, lifetime, r#type, GenericArguments, Never},
    },
};

/// Represents an accessibility of a symbol.
///
/// ```
/// use pernixc_semantic::symbol::Accessibility;
///
/// let private = Accessibility::Private;
/// let internal = Accessibility::Internal;
/// let public = Accessibility::Public;
///
/// assert!(private < internal);
/// assert!(internal < public);
/// assert!(private < public);
/// ```
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::Display,
)]
pub enum Accessibility {
    /// The symbol is accessible only from the same module and its submodules.
    #[display(fmt = "private")]
    Private,

    /// The symbol is accessible only from the same target.
    #[display(fmt = "internal")]
    Internal,

    /// The symbol is accessible from anywhere.
    #[display(fmt = "public")]
    Public,
}

impl Accessibility {
    /// Converts the [`AccessModifier`] to [`Accessibility`].
    #[must_use]
    pub const fn from_syntax_tree(syntax_tree: &AccessModifier) -> Self {
        match syntax_tree {
            AccessModifier::Public(..) => Self::Public,
            AccessModifier::Private(..) => Self::Private,
            AccessModifier::Internal(..) => Self::Internal,
        }
    }

    /// Gets the rank of the accessibility.
    ///
    /// The higher the number, the more accessible the accessibility is.
    #[must_use]
    pub const fn rank(&self) -> usize {
        match self {
            Self::Private => 0,
            Self::Internal => 1,
            Self::Public => 2,
        }
    }
}

/// Describes how a predicate is introduced.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PredicateKind {
    /// Introduced explicitly by a where clause syntax. The optional span
    /// represents the location of the where clause syntax.
    Explicit(Option<Span>),

    /// Introduced implicitly by a trait predicate. The optional span
    /// represents the location of the trait predicate that introduces the
    /// implicit predicate.
    ImpliedByTraitBound(Option<Span>),
}

impl PredicateKind {
    /// Gets the span of the predicate kind.
    #[must_use]
    pub const fn span(&self) -> Option<&Span> {
        match self {
            Self::Explicit(span) | Self::ImpliedByTraitBound(span) => {
                span.as_ref()
            }
        }
    }
}

/// Represents a predicate introduced by either a where clause or implication.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Predicate {
    /// The predicate itself.
    pub predicate: predicate::Predicate,

    /// The kind of the predicate.
    pub kind: PredicateKind,
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
    TraitImplementation(ID<TraitImplementation>),
    TraitImplementationFunction(ID<TraitImplementationFunction>),
    TraitImplementationType(ID<TraitImplementationType>),
    TraitImplementationConstant(ID<TraitImplementationConstant>),
    AdtImplementation(ID<AdtImplementation>),
    AdtImplementationFunction(ID<AdtImplementationFunction>),
    AdtImplementationType(ID<AdtImplementationType>),
    AdtImplementationConstant(ID<AdtImplementationConstant>),
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
    (TraitImplementation, TraitImplementation),
    (TraitImplementationFunction, TraitImplementationFunction),
    (TraitImplementationType, TraitImplementationType),
    (TraitImplementationConstant, TraitImplementationConstant),
    (AdtImplementation, AdtImplementation),
    (AdtImplementationFunction, AdtImplementationFunction),
    (AdtImplementationType, AdtImplementationType),
    (AdtImplementationConstant, AdtImplementationConstant)
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
    (TraitImplementation, TraitImplementation),
    (NegativeTraitImplementation, NegativeTraitImplementation),
    (TraitImplementationType, TraitImplementationType),
    (TraitImplementationFunction, TraitImplementationFunction)
);

/// Represents a kind of symbol that accepts generic arguments.
pub trait Generic: Global {
    /// Gets the [`GenericParameters`] of the symbol.
    fn generic_declaration(&self) -> &GenericDeclaration;

    #[doc(hidden)]
    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration;
}

/// Represents a kind of symbol that defines a algebraic data type.
///
/// This primarily includes [`Struct`] and [`Enum`].
pub trait Adt: Generic {
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
    TraitImplementation(ID<TraitImplementation>),
    NegativeTraitImplementation(ID<NegativeTraitImplementation>),
    TraitImplementationFunction(ID<TraitImplementationFunction>),
    TraitImplementationType(ID<TraitImplementationType>),
    TraitImplementationConstant(ID<TraitImplementationConstant>),
    AdtImplementation(ID<AdtImplementation>),
    AdtImplementationFunction(ID<AdtImplementationFunction>),
    AdtImplementationType(ID<AdtImplementationType>),
    AdtImplementationConstant(ID<AdtImplementationConstant>),
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
    (Constant, Constant)
);

/// Represents a module declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    /// The name of the module declaration.
    pub name: String,

    /// The accessibility of the module.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    ///
    /// If this module is a root module, then this field is `None`.
    pub parent_module_id: Option<ID<Module>>,

    /// Maps the name of the module child to its ID.
    pub child_ids_by_name: HashMap<String, ModuleMemberID>,

    /// Location of where the module is declared.
    pub span: Option<Span>,

    /// The modules that are used by `using` statements.
    pub usings: HashSet<ID<Module>>,
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

/// An enumeration of either an invariant or covariant variance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum Variance {
    /// The term is bivariant and can be changed to any subtype or supertype.
    Bivariant,

    /// The term is covariant and can be changed to a subtype.
    ///
    /// This is the most common variance; that is, the lifetime can be changed
    /// to a smaller lifetime e.g. `'static` to `'a`.
    Covariant,

    /// The term is contravariant and can be changed to a supertype.
    ///
    /// This is the opposite of convariant; that is, the lifetime can be
    /// changed to a larger lifetime e.g. `'a` to `'static`. This is
    /// generally used in the parameter of a function i.e. the function
    /// parameter used to accept a particular lifetime `'a` can be changed
    /// to accept a larger lifetime `'static`.
    Contravariant,

    /// The term is invariant and cannot be changed.
    #[default]
    Invariant,
}

impl Variance {
    /// Chains two variance together.
    #[must_use]
    pub const fn chain(self, other: Self) -> Self {
        match (self, other) {
            (Self::Bivariant, other) | (other, Self::Bivariant) => other,

            (Self::Invariant, _)
            | (_, Self::Invariant)
            | (Self::Covariant, Self::Contravariant)
            | (Self::Contravariant, Self::Covariant) => Self::Invariant,

            (Self::Covariant, Self::Covariant) => Self::Covariant,

            (Self::Contravariant, Self::Contravariant) => Self::Contravariant,
        }
    }
}

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
    pub r#type: r#type::Type,

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
    default_type_parameters: Vec<r#type::Type>,

    /// List of default constant parameters to be used when the generic
    /// parameters are not
    #[get = "pub"]
    default_constant_parameters: Vec<constant::Constant>,
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
    pub fn create_identity_generic_arguments(
        &self,
        generic_id: GenericID,
    ) -> GenericArguments {
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
    pub r#type: r#type::Type,

    /// Location of where the field is declared.
    pub span: Option<Span>,
}

/// Represents a struct declaration, denoted by `struct NAME { ... }` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
    /// The name of the struct.
    pub name: String,

    /// The accessibility of the struct.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    pub parent_module_id: ID<Module>,

    /// The generic declaration of the struct.
    pub generic_declaration: GenericDeclaration,

    /// Contains all the fields defined in the struct.
    pub fields: Map<Field>,

    /// All of the implementations of the struct.
    pub implementations: HashSet<ID<AdtImplementation>>,

    /// Contains the variances of the generic parameters.
    pub generic_parameter_variances: GenericParameterVariances,

    /// Location of where the struct is declared.
    pub span: Option<Span>,
}

impl Generic for Struct {
    fn generic_declaration(&self) -> &GenericDeclaration {
        &self.generic_declaration
    }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Struct {
    fn name(&self) -> &str { &self.name }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Module(self.parent_module_id))
    }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

impl Adt for Struct {
    fn generic_parameter_variances(&self) -> &GenericParameterVariances {
        &self.generic_parameter_variances
    }
}

/// Represents an enum variant declaration, denoted by `NAME(ASSOC_TYPE)`
/// syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    /// The name of the variant.
    pub name: String,

    /// The type of the associated value of the variant (if any).
    pub associated_type: Option<r#type::Type>,

    /// The parent enum ID.
    pub parent_enum_id: ID<Enum>,

    /// The span where the variant is declared.
    pub span: Option<Span>,
}

impl Global for Variant {
    fn name(&self) -> &str { &self.name }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Enum(self.parent_enum_id))
    }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

/// Represents an enum declaration, denoted by `enum NAME { ... }` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    /// The name of the enum.
    pub name: String,

    /// The accessibility of the enum.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    pub parent_module_id: ID<Module>,

    /// The generic declaration of the enum.
    pub generic_declaration: GenericDeclaration,

    /// Maps the name of variants defined in the enum to its ID.
    pub variant_ids_by_name: HashMap<String, ID<Variant>>,

    /// All of the implementations of the enums.
    pub implementations: HashSet<ID<AdtImplementation>>,

    /// Contains the variances of the generic parameters.
    pub generic_parameter_variances: GenericParameterVariances,

    /// Location of where the enum is declared.
    pub span: Option<Span>,
}

impl Generic for Enum {
    fn generic_declaration(&self) -> &GenericDeclaration {
        &self.generic_declaration
    }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Enum {
    fn name(&self) -> &str { &self.name }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Module(self.parent_module_id))
    }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

impl Adt for Enum {
    fn generic_parameter_variances(&self) -> &GenericParameterVariances {
        &self.generic_parameter_variances
    }
}

/// A template struct representing all kinds of type alias declarations.
#[derive(Debug, Clone, PartialEq, Eq, Deref, DerefMut)]
pub struct TypeTemplate<ParentID: 'static, Data: 'static> {
    /// The generic declaration of the type.
    pub generic_declaration: GenericDeclaration,

    /// The ID of the parent where the type is declared.
    pub parent_id: ParentID,

    /// The span where the type is declared.
    pub span: Option<Span>,

    /// The name of the type.
    pub name: String,

    /// The additional data of the type.
    #[deref]
    #[deref_mut]
    pub data: Data,
}

/// Contains the data for the regular type declaration i.e. those that are
/// declared in the module level.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeData {
    /// The accessibility of the type.
    pub accessibility: Accessibility,

    /// Type type aliased
    pub r#type: r#type::Type,
}

/// Represents a regular type declaration i.e. those that are declared in the
/// module level. `type NAME = TYPE` syntax.
pub type Type = TypeTemplate<ID<Module>, TypeData>;

/// Represents an adt implementation type, denoted by `type NAME = TYPE` syntax.
pub type AdtImplementationType = TypeTemplate<ID<AdtImplementation>, TypeData>;

impl<ParentID, Data> Global for TypeTemplate<ParentID, Data>
where
    ParentID: Into<GlobalID> + Copy,
    ID<Self>: Into<GlobalID>,
{
    fn name(&self) -> &str { &self.name }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(self.parent_id.into())
    }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

impl<ParentID, Data> Generic for TypeTemplate<ParentID, Data>
where
    Self: Global,
    ID<Self>: Into<GenericID>,
{
    fn generic_declaration(&self) -> &GenericDeclaration {
        &self.generic_declaration
    }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

/// A template struct representing all kinds of constant declarations.
#[derive(Debug, Clone, PartialEq, Eq, Deref, DerefMut)]
pub struct ConstantTemplate<ParentID: 'static, Data: 'static> {
    /// The name of the constant.
    pub name: String,

    /// The type of the constant.
    pub r#type: r#type::Type,

    /// The generic declaration of the constant.
    pub generic_declaration: GenericDeclaration,

    /// Location of where the constant is declared.
    pub span: Option<Span>,

    /// The ID of the parent module.
    pub parent_id: ParentID,

    /// The additional data of the constant.
    #[deref]
    #[deref_mut]
    pub data: Data,
}

impl<ParentID, Data> Global for ConstantTemplate<ParentID, Data>
where
    ParentID: Into<GlobalID> + Copy,
    ID<Self>: Into<GlobalID>,
{
    fn name(&self) -> &str { &self.name }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(self.parent_id.into())
    }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

impl<ParentID, Data> Generic for ConstantTemplate<ParentID, Data>
where
    Self: Global,
    ID<Self>: Into<GenericID>,
{
    fn generic_declaration(&self) -> &GenericDeclaration {
        &self.generic_declaration
    }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

/// Contains the data for the regular constant declaration i.e. those that are
/// declared in the module level.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ConstantData {
    /// The accessibility of the constant.
    pub accessibility: Accessibility,
    // TODO: Constant IR
}

/// Represents a constant declaration, denoted by `const NAME: TYPE = VALUE`
/// syntax.
pub type Constant = ConstantTemplate<ID<Module>, ConstantData>;

/// Represents an adt implementation constant, denoted by `const NAME: TYPE =
/// VALUE` syntax.
pub type AdtImplementationConstant =
    ConstantTemplate<ID<AdtImplementation>, ConstantData>;

/// A template struct representing all kinds of function declarations.
#[derive(Debug, Clone, PartialEq, Eq, Deref, DerefMut)]
pub struct FunctionTemplate<ParentID: 'static, Data: 'static> {
    /// The parameters of the function.
    pub parameters: Arena<Parameter>,

    /// The ID of the parent where the function is declared.
    pub parent_id: ParentID,

    /// Location of where the function is declared.
    pub span: Option<Span>,

    /// The name of the function.
    pub name: String,

    /// The return type of the function.
    pub return_type: r#type::Type,

    /// The accessibility of the function.
    pub generic_declaration: GenericDeclaration,

    /// The additional data of the function.
    #[deref]
    #[deref_mut]
    pub data: Data,
}

/// Contains the data for the regular function declaration i.e. those that are
/// declared in the module level.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionData {
    /// The accessibility of the function.
    pub accessibility: Accessibility,

    /// Indicates whether the function is a constant function.
    pub const_function: bool,

    /// Maps the parameter ID to its matching pattern.
    pub patterns_by_parameter_id: HashMap<ID<Parameter>, Irrefutable>,
    // TODO: Function IR
}

/// Represents a regular function declaration i.e. those that are declared in
/// the module level. `function NAME(...) {...}` syntax.
pub type Function = FunctionTemplate<ID<Module>, FunctionData>;

impl<ParentID, Data> Global for FunctionTemplate<ParentID, Data>
where
    ParentID: Into<GlobalID> + Copy,
    ID<Self>: Into<GlobalID>,
{
    fn name(&self) -> &str { &self.name }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(self.parent_id.into())
    }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

impl<ParentID, Data> Generic for FunctionTemplate<ParentID, Data>
where
    Self: Global,
    ID<Self>: Into<GenericID>,
{
    fn generic_declaration(&self) -> &GenericDeclaration {
        &self.generic_declaration
    }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

/// Represents an enum implementation function, denoted by `function NAME(...)
/// {...}` syntax.
pub type AdtImplementationFunction =
    FunctionTemplate<ID<AdtImplementation>, FunctionData>;

trait ImplementationData {
    type MemberID: Copy + Into<GlobalID>;

    fn get_member(&self, name: &str) -> Option<Self::MemberID>;
}

/// Represents an implementation signature, denoted by `implements<PARAM>
/// SYM<PARAM>` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplementationSignature<ImplementedID> {
    /// The generic declaration of this signature.
    pub generic_declaration: GenericDeclaration,

    /// The generic arguments supplied to the implemented symbol.
    pub arguments: GenericArguments,

    /// The ID of the symbol that is being implemented.
    pub implemented_id: ImplementedID,
}

/// A template struct representing all kinds of implementation declarations.
#[derive(Debug, Clone, PartialEq, Eq, Deref, DerefMut)]
pub struct ImplementationTemplate<ImplementedID, Data: 'static> {
    /// Location of where the implements is declared.
    pub span: Option<Span>,

    /// The implementation signature of the implementation.
    pub signature: ImplementationSignature<ImplementedID>,

    /// The name of the symbol that is being implemented.
    pub implementation_name: String,

    /// The ID module where the implementation is declared.
    pub declared_in: ID<Module>,

    /// The additional data of the implementation.
    #[deref]
    #[deref_mut]
    pub data: Data,
}

/// Contains the data for the adt implementation
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct AdtImplementationData {
    /// Maps the name of the adt implementation member to its ID.
    pub member_ids_by_name: HashMap<String, AdtImplementationMemberID>,
}

impl ImplementationData for AdtImplementationData {
    type MemberID = AdtImplementationMemberID;

    fn get_member(&self, name: &str) -> Option<Self::MemberID> {
        self.member_ids_by_name.get(name).copied()
    }
}

/// Enumeration of either struct or enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum AdtID {
    Struct(ID<Struct>),
    Enum(ID<Enum>),
}

from_ids!(AdtID, GlobalID, (Struct, Struct), (Enum, Enum));
from_ids!(AdtID, GenericID, (Struct, Struct), (Enum, Enum));

/// Represents a adt implementation, denoted by `implements<PARAM> adt<PARAM> {
/// ... }` syntax.
pub type AdtImplementation =
    ImplementationTemplate<AdtID, AdtImplementationData>;

/// Negative trait implementation data tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NegativeTraitImplementationData;

impl ImplementationData for NegativeTraitImplementationData {
    type MemberID = Never;

    fn get_member(&self, _: &str) -> Option<Self::MemberID> { None }
}

impl From<Never> for GlobalID {
    fn from(a: Never) -> Self { match a {} }
}

impl From<Never> for GenericID {
    fn from(a: Never) -> Self { match a {} }
}

/// Represents a negative trait implementation, denoted by
/// `implements<PARAM> TRAIT<PARAM> = delete;` syntax.
pub type NegativeTraitImplementation =
    ImplementationTemplate<ID<Trait>, NegativeTraitImplementationData>;

impl<ImplementedID, Data: ImplementationData> Global
    for ImplementationTemplate<ImplementedID, Data>
where
    ImplementedID: Into<GlobalID> + Copy,
    ID<Self>: Into<GlobalID>,
{
    fn name(&self) -> &str { &self.implementation_name }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(self.declared_in.into())
    }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

impl<ImplementedID, Data: ImplementationData> Generic
    for ImplementationTemplate<ImplementedID, Data>
where
    Self: Global,
    ID<Self>: Into<GenericID>,
{
    fn generic_declaration(&self) -> &GenericDeclaration {
        &self.signature.generic_declaration
    }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.signature.generic_declaration
    }
}

/// Represents a function declaration as an implements member, denoted by
/// `function NAME(...) {...}` syntax.
pub type TraitImplementationFunction =
    FunctionTemplate<ID<TraitImplementation>, TraitImplementationFunctionData>;

/// Contains the implementation data of a trait implementation function symbol
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitImplementationFunctionData {
    /// The trait function ID that is being implemented.
    pub implemented_trait_function_id: ID<TraitFunction>,

    /// Maps the parameter ID to its matching pattern.
    pub patterns_by_parameter_id: HashMap<ID<Parameter>, Irrefutable>,
}

/// Represents a type declaration as an implements member, denoted by `type NAME
/// = TYPE;` syntax.
pub type TraitImplementationType =
    TypeTemplate<ID<TraitImplementation>, TraitImplementationTypeData>;

/// Contains the implementation data of a trait implementation type symbol
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitImplementationTypeData {
    /// The aliased type.
    pub r#type: r#type::Type,

    /// The trait type ID that is being implemented.
    pub implemented_trait_type_id: ID<TraitType>,

    /// The variances of the generic parameters.
    pub generic_parameter_variances: GenericParameterVariances,
}

/// Represents a constant declaration as an implements member, denoted by `const
/// NAME: TYPE` syntax.
pub type TraitImplementationConstant =
    ConstantTemplate<ID<TraitImplementation>, TraitImplementationConstantData>;

/// Contains the implementation data of a trait implementation constant symbol
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TraitImplementationConstantData {
    // The IR of the constant.
    /// The trait constant ID that is being implemented.
    pub implemented_trait_constant_id: ID<TraitConstant>,
}

/// An ID to all kinds of symbols that can be defined in a adt implementation.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
#[allow(missing_docs)]
pub enum AdtImplementationMemberID {
    Function(ID<AdtImplementationFunction>),
    Type(ID<AdtImplementationType>),
    Constant(ID<AdtImplementationConstant>),
}

from_ids!(
    AdtImplementationMemberID,
    GlobalID,
    (Function, AdtImplementationFunction),
    (Type, AdtImplementationType),
    (Constant, AdtImplementationConstant)
);

from_ids!(
    AdtImplementationMemberID,
    GenericID,
    (Function, AdtImplementationFunction),
    (Type, AdtImplementationType),
    (Constant, AdtImplementationConstant)
);

/// An ID to all kinds of symbols that can be defined in a trait implementation.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
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
pub type TraitImplementation =
    ImplementationTemplate<ID<Trait>, TraitImplementationData>;

/// Contains the implementation data of a trait implementation symbol
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitImplementationData {
    /// Indicates whether the trait implementation is a constant
    /// implementation.
    pub is_const: bool,

    /// Maps the name of the trait member to its ID.
    pub member_ids_by_name: HashMap<String, TraitImplementationMemberID>,

    /// Maps the ID of the trait type to the ID of the implementation type.
    pub implementation_type_ids_by_trait_type_id:
        HashMap<ID<TraitType>, ID<TraitImplementationType>>,

    /// Maps the ID of the trait function to the ID of the implementation
    /// function.
    pub implementation_function_ids_by_trait_function_id:
        HashMap<ID<TraitFunction>, ID<TraitImplementationFunction>>,

    /// Maps the ID of the trait constant to the ID of the implementation
    /// constant.
    pub implementation_constant_ids_by_trait_constant_id:
        HashMap<ID<TraitConstant>, ID<TraitImplementationConstant>>,
}

impl ImplementationData for TraitImplementationData {
    type MemberID = TraitImplementationMemberID;

    fn get_member(&self, name: &str) -> Option<Self::MemberID> {
        self.member_ids_by_name.get(name).copied()
    }
}

/// Represents a function parameter in the function signature, denoted by `NAME:
/// TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter {
    /// The type of the parameter.
    pub r#type: r#type::Type,

    /// Location of where the parameter is declared.
    pub span: Option<Span>,
}

/// Represents a type declaration as a trait member, denoted by `type NAME;`
/// syntax.
pub type TraitType = TypeTemplate<ID<Trait>, TraitTypeData>;

/// Trait type data tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitTypeData {
    /// The accessibility of the type.
    pub accessibility: Accessibility,
}

/// Represents a function declaration as a trait member, denoted by `function
/// NAME(...) {...}` syntax.
pub type TraitFunction = FunctionTemplate<ID<Trait>, TraitFunctionData>;

/// Trait function data tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TraitFunctionData {
    /// The accessibility of the function.
    pub accessibility: Accessibility,
}

/// Represents a constant declaration as a trait member, denoted by `const NAME:
/// TYPE` syntax.
pub type TraitConstant = ConstantTemplate<ID<Trait>, TraitConstantData>;

/// Trait constant data tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitConstantData {
    /// The accessibility of the constant.
    pub accessibility: Accessibility,
}

/// An enumeration containing all an ID to all kinds of symbols that can be
/// defined in a trait.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trait {
    /// The name of the trait.
    pub name: String,

    /// The accessibility of the trait.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    pub parent_module_id: ID<Module>,

    /// The generic declaration of the trait.
    pub generic_declaration: GenericDeclaration,

    /// Contains all the negative trait implementation defined in the trait.
    pub negative_implementations: HashSet<ID<NegativeTraitImplementation>>,

    /// Contains all the trait implementation defined in the trait.
    pub implementations: HashSet<ID<TraitImplementation>>,

    /// Contains all the types defined in the trait.
    pub span: Option<Span>,

    /// Map the name of the trait member to its ID.
    pub member_ids_by_name: HashMap<String, TraitMemberID>,
}

impl Generic for Trait {
    fn generic_declaration(&self) -> &GenericDeclaration {
        &self.generic_declaration
    }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Trait {
    fn name(&self) -> &str { &self.name }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Module(self.parent_module_id))
    }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

/// Enumeration of all kinds of generic parameters.
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
pub enum TraitImplementationKindID {
    Positive(ID<TraitImplementation>),
    Negative(ID<NegativeTraitImplementation>),
}

from_ids!(
    TraitImplementationKindID,
    GlobalID,
    (Positive, TraitImplementation),
    (Negative, NegativeTraitImplementation)
);

from_ids!(
    TraitImplementationKindID,
    GenericID,
    (Positive, TraitImplementation),
    (Negative, NegativeTraitImplementation)
);

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

/// Represents an implementation to an algebraic data type i.e. enum or struct.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AlgebraicImplementation {
    /// Generic arguments applied to the adt.
    pub arguments: GenericArguments,

    /// The kind of the algebraic type.
    pub kind: AlgebraicKind,

    /// Location of where the implementation is declared.
    pub span: Option<Span>,

    /// The generic declaration of this signature.
    pub generic_declaration: GenericDeclaration,

    /// The name of the algebraic data type that is being implemented.
    pub algebraic_name: String,

    /// The ID module where the implementation is declared.
    pub declared_in: ID<Module>,
}
