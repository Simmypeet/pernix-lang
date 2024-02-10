//! Contains the definition of all symbol kinds in the language.

use std::{
    collections::{HashMap, HashSet},
    convert::Into,
    sync::Arc,
};

use derive_more::{Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;
use pernixc_syntax::syntax_tree::AccessModifier;

use crate::{
    arena::{Arena, Map, ID},
    semantic::{
        predicate,
        term::{constant, r#type, GenericArguments, Never},
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Accessibility {
    /// The symbol is accessible only from the same module and its submodules.
    Private,

    /// The symbol is accessible only from the same target.
    Internal,

    /// The symbol is accessible from anywhere.
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

/// Represents a predicate introduced by either a where clause or implication.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Predicate {
    /// The predicate itself.
    pub predicate: predicate::Predicate,

    /// Location of where the predicate is declared.
    pub span: Option<Span>,

    /// Whether the predicate is explicitly declared or was implied by the
    /// compiler.
    pub explicit: bool,
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
pub trait Generic {
    /// The ID representing the symbol itself.
    fn generic_id(&self) -> GenericID;

    /// Gets the [`GenericParameters`] of the symbol.
    fn generic_declaration(&self) -> &GenericDeclaration;

    #[doc(hidden)]
    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration;
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

    /// The ID representing the symbol itself.
    fn global_id(&self) -> GlobalID;

    /// The ID of the parent symbol.
    fn parent_global_id(&self) -> Option<GlobalID>;

    /// Gets the ID of the global symbol defined as a member of this symbol with
    /// its name.
    fn get_member(&self, name: &str) -> Option<GlobalID>;

    /// Location of where the symbol is declared.
    fn span(&self) -> Option<&Span>;
}

/// An ID to all kinds of symbols that can be defined in a module.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
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
    /// The ID of the module declaration.
    pub id: ID<Module>,

    /// The name of the module declaration.
    pub name: String,

    /// The accessibility of the module.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    ///
    /// If this module is a root module, then this field is `None`.
    pub parent_module_id: Option<ID<Module>>,

    /// Maps the name of the module child to its ID.
    pub module_child_ids_by_name: HashMap<String, ModuleMemberID>,

    /// Location of where the module is declared.
    pub span: Option<Span>,

    /// The modules that are used by `using` statements.
    pub usings: HashSet<ID<Module>>,
}

impl Global for Module {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Module(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> {
        self.parent_module_id.map(GlobalID::Module)
    }

    fn get_member(&self, name: &str) -> Option<GlobalID> {
        self.module_child_ids_by_name.get(name).copied().map(Into::into)
    }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

/// Implemented by all generic parameters [`LifetimeParameter`],
/// [`TypeParameter`], and [`ConstantParameter`].
pub trait GenericParameter {
    /// Gets the [`Variance`] of the generic parameter.
    fn variance(&self) -> Variance;

    /// Gets the name of the generic parameter.
    ///
    /// If the generic parameter is anonymous, (i.e. elided lifetime parameter),
    /// then this method returns `None`.
    fn name(&self) -> Option<&str>;

    /// Gets the span where the generic parameter is declared.
    fn span(&self) -> Option<&Span>;
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
    /// The term is invariant and cannot be changed.
    #[default]
    Invariant,

    /// The term is covariant and can be changed to a subtype.
    Covariant,
}

impl Variance {
    /// Chains two variance together.
    #[must_use]
    pub const fn chain(self, other: Self) -> Self {
        match (self, other) {
            (Self::Covariant, other) => other,
            (Self::Invariant, _) => Self::Invariant,
        }
    }
}

/// Represents a lifetime parameter, denoted by `'a` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeParameter {
    /// The name of the lifetime parameter (if none, then it is anonymous
    /// lifetime parameter )
    pub name: Option<String>,

    /// The ID where the lifetime parameter is declared.
    pub parent_generic_id: GenericID,

    /// Location of where the lifetime parameter is declared.
    pub span: Option<Span>,

    /// The variance of the lifetime parameter.
    pub variance: Variance,
}

impl GenericParameter for LifetimeParameter {
    fn variance(&self) -> Variance { self.variance }

    fn name(&self) -> Option<&str> { self.name.as_ref().map(AsRef::as_ref) }

    fn span(&self) -> Option<&Span> { todo!() }
}

/// Represents a type parameter, denoted by `T` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeParameter {
    /// The name of the type parameter.
    pub name: String,

    /// The ID where the type parameter is declared.
    pub parent_generic_id: GenericID,

    /// The kind of the type parameter.
    pub span: Option<Span>,

    /// The variance of the type parameter.
    pub variance: Variance,
}

impl GenericParameter for TypeParameter {
    fn variance(&self) -> Variance { self.variance }

    fn name(&self) -> Option<&str> { Some(&self.name) }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

/// Represents a constant parameter, denoted by `const C: TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantParameter {
    /// The name of the constant parameter.
    pub name: String,

    /// The ID where the constant parameter is declared.
    pub parent_generic_id: GenericID,

    /// The type of the constant parameter.
    pub r#type: Arc<r#type::Type>,

    /// The type of the constant parameter.
    pub span: Option<Span>,
}

impl GenericParameter for ConstantParameter {
    fn variance(&self) -> Variance { Variance::Invariant }

    fn name(&self) -> Option<&str> { Some(&self.name) }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

/// Represents a list of generic parameters.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct GenericParameters {
    /// List of defined lifetime parameters.
    pub lifetimes: Arena<LifetimeParameter>,

    /// List of defined type parameters.
    pub types: Arena<TypeParameter>,

    /// List of defined constant parameters.
    pub constants: Arena<ConstantParameter>,

    /// The order of the declaration of lifetime parameters.
    pub lifetime_order: Vec<ID<LifetimeParameter>>,

    /// The order of the declaration of type parameters.
    pub type_order: Vec<ID<TypeParameter>>,

    /// The order of the declaration of constant parameters.
    pub constant_order: Vec<ID<ConstantParameter>>,

    /// Maps the name of the lifetime parameter to its ID.
    pub lifetime_parameter_ids_by_name: HashMap<String, ID<LifetimeParameter>>,

    /// Maps the name of the type parameter to its ID.
    pub type_parameter_ids_by_name: HashMap<String, ID<TypeParameter>>,

    /// Maps the name of the constant parameter to its ID.
    pub constant_parameter_ids_by_name: HashMap<String, ID<ConstantParameter>>,

    /// List of default type parameters to be used when the generic parameters
    /// are not specified.
    pub default_type_parameters: Vec<r#type::Type>,

    /// List of default constant parameters to be used when the generic
    /// parameters are not
    pub default_constant_parameters: Vec<constant::Constant>,
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
    /// The ID of the struct.
    pub id: ID<Struct>,

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

    /// Location of where the struct is declared.
    pub span: Option<Span>,
}

impl Generic for Struct {
    fn generic_id(&self) -> GenericID { GenericID::Struct(self.id) }

    fn generic_declaration(&self) -> &GenericDeclaration {
        &self.generic_declaration
    }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Struct {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Struct(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Module(self.parent_module_id))
    }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

/// Represents an enum variant declaration, denoted by `NAME(ASSOC_TYPE)`
/// syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    /// The ID of the variant.
    pub id: ID<Variant>,

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

    fn global_id(&self) -> GlobalID { GlobalID::Variant(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Enum(self.parent_enum_id))
    }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

/// Represents an enum declaration, denoted by `enum NAME { ... }` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    /// The ID of the enum.
    pub id: ID<Enum>,

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

    /// Location of where the enum is declared.
    pub span: Option<Span>,
}

impl Generic for Enum {
    fn generic_id(&self) -> GenericID { GenericID::Enum(self.id) }

    fn generic_declaration(&self) -> &GenericDeclaration {
        &self.generic_declaration
    }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Enum {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Enum(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Module(self.parent_module_id))
    }

    fn get_member(&self, name: &str) -> Option<GlobalID> {
        self.variant_ids_by_name.get(name).copied().map(GlobalID::Variant)
    }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

/// A template struct representing all kinds of type alias declarations.
#[derive(Debug, Clone, PartialEq, Eq, Deref, DerefMut)]
pub struct TypeTemplate<ParentID: 'static, Data: 'static> {
    /// The ID of the type.
    pub id: ID<Self>,

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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

    fn global_id(&self) -> GlobalID { self.id.into() }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(self.parent_id.into())
    }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

impl<ParentID, Data> Generic for TypeTemplate<ParentID, Data>
where
    ID<Self>: Into<GenericID>,
{
    fn generic_id(&self) -> GenericID { self.id.into() }

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
    /// The ID of the constant.
    pub id: ID<Self>,

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

    fn global_id(&self) -> GlobalID { self.id.into() }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(self.parent_id.into())
    }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

impl<ParentID, Data> Generic for ConstantTemplate<ParentID, Data>
where
    ID<Self>: Into<GenericID>,
{
    fn generic_id(&self) -> GenericID { self.id.into() }

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
    /// The ID of the function.
    pub id: ID<Self>,

    /// The parameters of the function.
    pub parameters: Map<Parameter<ID<Self>>>,

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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionData {
    /// The accessibility of the function.
    pub accessibility: Accessibility,

    /// Indicates whether the function is a constant function.
    pub const_function: bool,
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

    fn global_id(&self) -> GlobalID { self.id.into() }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(self.parent_id.into())
    }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

impl<ParentID, Data> Generic for FunctionTemplate<ParentID, Data>
where
    ID<Self>: Into<GenericID>,
{
    fn generic_id(&self) -> GenericID { self.id.into() }

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
    /// The ID of the implementation.
    pub id: ID<Self>,

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
pub enum AdtKindID {
    Struct(ID<Struct>),
    Enum(ID<Enum>),
}

from_ids!(AdtKindID, GlobalID, (Struct, Struct), (Enum, Enum));
from_ids!(AdtKindID, GenericID, (Struct, Struct), (Enum, Enum));

/// Represents a adt implementation, denoted by `implements<PARAM> adt<PARAM> {
/// ... }` syntax.
pub type AdtImplementation =
    ImplementationTemplate<AdtKindID, AdtImplementationData>;

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

    fn global_id(&self) -> GlobalID { self.id.into() }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(self.declared_in.into())
    }

    fn get_member(&self, name: &str) -> Option<GlobalID> {
        self.data.get_member(name).map(Into::into)
    }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

impl<ImplementedID, Data: ImplementationData> Generic
    for ImplementationTemplate<ImplementedID, Data>
where
    ID<Self>: Into<GenericID>,
{
    fn generic_id(&self) -> GenericID { self.id.into() }

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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TraitImplementationFunctionData {
    // TODO: Function IR
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
}

/// Represents a constant declaration as an implements member, denoted by `const
/// NAME: TYPE` syntax.
pub type TraitImplementationConstant =
    ConstantTemplate<ID<TraitImplementation>, TraitImplementationConstantData>;

/// Contains the implementation data of a trait implementation constant symbol
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TraitImplementationConstantData {
    // The IR of the constant.
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
    pub implementation_member_ids_by_name:
        HashMap<String, TraitImplementationMemberID>,

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
        self.implementation_member_ids_by_name.get(name).copied()
    }
}

/// Represents a function parameter in the function signature, denoted by `NAME:
/// TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter<ParentID> {
    /// The pattern binding of the parameter.
    // pub pattern: Irrefutable,

    /// The type of the parameter.
    pub r#type: r#type::Type,

    /// The ID of the parent function.
    pub parent_id: ParentID,

    /// Location of where the parameter is declared.
    pub span: Option<Span>,
}

/// Represents a type declaration as a trait member, denoted by `type NAME;`
/// syntax.
pub type TraitType = TypeTemplate<ID<Trait>, TraitTypeData>;

/// Trait type data tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitTypeData;

/// Represents a function declaration as a trait member, denoted by `function
/// NAME(...) {...}` syntax.
pub type TraitFunction = FunctionTemplate<ID<Trait>, TraitFunctionData>;

/// Trait function data tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TraitFunctionData;

/// Represents a constant declaration as a trait member, denoted by `const NAME:
/// TYPE` syntax.
pub type TraitConstant = ConstantTemplate<ID<Trait>, TraitConstantData>;

/// Trait constant data tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitConstantData;

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

/// Represents a trait declaration, denoted by `trait NAME { ... }` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trait {
    /// The ID of the trait.
    pub id: ID<Trait>,

    /// The name of the trait.
    pub name: String,

    /// The accessibility of the trait.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    pub parent_module_id: ID<Module>,

    /// The generic declaration of the trait.
    pub generic_declaration: GenericDeclaration,

    /// Contains all the negative trait implementation defined in the trait.
    pub negative_implementations: Vec<ID<NegativeTraitImplementation>>,

    /// Contains all the trait implementation defined in the trait.
    pub implementations: Vec<ID<TraitImplementation>>,

    /// Contains all the types defined in the trait.
    pub span: Option<Span>,

    /// Map the name of the trait member to its ID.
    pub trait_member_ids_by_name: HashMap<String, TraitMemberID>,
}

impl Generic for Trait {
    fn generic_id(&self) -> GenericID { GenericID::Trait(self.id) }

    fn generic_declaration(&self) -> &GenericDeclaration {
        &self.generic_declaration
    }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Trait {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Trait(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Module(self.parent_module_id))
    }

    fn get_member(&self, name: &str) -> Option<GlobalID> {
        self.trait_member_ids_by_name.get(name).copied().map(|x| match x {
            TraitMemberID::Type(x) => GlobalID::TraitType(x),
            TraitMemberID::Function(x) => GlobalID::TraitFunction(x),
            TraitMemberID::Constant(x) => GlobalID::TraitConstant(x),
        })
    }

    fn span(&self) -> Option<&Span> { self.span.as_ref() }
}

/// Enumeration of all kinds of generic parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
