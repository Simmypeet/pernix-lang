//! Contains the code related to the symbol resolution pass of the compiler.

#![deny(
    missing_docs,
    missing_debug_implementations,
    missing_copy_implementations,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

use std::{collections::HashMap, fmt::Debug, hash::Hash, sync::Arc};

use derive_more::{Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::Identifier as IdentifierToken;
use pernixc_syntax::syntax_tree::{
    item::{
        EnumSignature as EnumSignatureSyntaxTree, Parameter as ParameterSyntaxTree,
        StructField as StructFieldSyntaxTree, StructSignature as StructSignatureSyntaxTree,
        Type as TypeSyntaxTree,
    },
    statement::Statement as StatementSyntaxTree,
    AccessModifier, TypeAnnotation,
};
use pernixc_system::create_symbol;
use ty::Type;

pub mod ty;

/// The accessibility of the symbol. Determines where the symbol can be accessed from.
#[derive(Debug, Clone, Copy, Hash, EnumAsInner)]
pub enum Accessibility {
    /// The symbol can only be accessed from the same module it is defined in.
    Private,

    /// The symbol can only be accessed from the same target it is defined in.
    Internal,

    /// The symbol can be accessed from anywhere.
    Public,
}

impl Accessibility {
    /// Creates a new [`Accessibility`] from the given [`AccessModifier`].
    #[must_use]
    pub fn from_syntax_tree(syntax_tree: &AccessModifier) -> Self {
        match syntax_tree {
            AccessModifier::Public(..) => Self::Public,
            AccessModifier::Private(..) => Self::Private,
            AccessModifier::Internal(..) => Self::Internal,
        }
    }

    /// Returns the rank of the accessibility. The higher the rank, the more accessible the symbol
    /// is.
    #[must_use]
    pub fn rank(&self) -> u8 {
        match self {
            Self::Private => 0,
            Self::Internal => 1,
            Self::Public => 2,
        }
    }
}

create_symbol! {
    #[derive(Debug, Clone)]
    pub struct AssociatedType {
        pub lifetime_bounds: Vec<LifetimeArgument>,
        pub generics: Generics,
    }
}

create_symbol! {
    #[derive(Debug, Clone)]
    pub struct Implements {
        pub generics: Generics,
        pub trait_id: TraitID,
        pub type_arguments_by_parameter: HashMap<TypeParameterID, Type>,
        pub lifetime_arguments_by_parameter: HashMap<LifetimeParameterID, LifetimeArgument>,
   }
}

create_symbol! {
    #[derive(Debug, Clone)]
    pub struct Trait {
        pub name: String,
        pub parent_module_id: ModuleID,
        pub generics: Generics,
        pub implements: Vec<Implements>,
        pub associated_type_ids_by_name: HashMap<String, AssociatedTypeID>,
        pub trait_function_ids_by_name: HashMap<String, TraitFunctionID>,
    }
}

create_symbol! {
    /// Represents a trait-member function symbol.
    #[derive(Debug, Clone, Deref, DerefMut)]
    pub struct TraitFunction {
        /// Contains the data of function signature.
        #[deref]
        #[deref_mut]
        pub function_signature: FunctionSignature,

        /// The ID of the module that contains the function.
        pub parent_trait_id: TraitID,
    }
}

/// Is an enumeration of symbols that accept generic parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum GenericableID {
    Struct(StructID),
    Function(FunctionID),
    Implements(ImplementsID),
    Trait(TraitID),
}

/// Is a trait for symbols that can be used as a generic parameter.
pub trait GenericParameter {
    /// Returns the name of the generic parameter.
    fn name(&self) -> &str;

    /// Returns the ID of the parent genericable.
    fn parent_genericable_id(&self) -> GenericableID;
}

impl GenericParameter for LifetimeParameter {
    fn name(&self) -> &str { &self.name }

    fn parent_genericable_id(&self) -> GenericableID { self.parent_id }
}

impl GenericParameter for TypeParameter {
    fn name(&self) -> &str { &self.name }

    fn parent_genericable_id(&self) -> GenericableID { self.parent_id }
}

/// Is an enumeration of symbols ID that can be used as a generic parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum GenericParameterID {
    Type(TypeParameterID),
    Lifetime(LifetimeParameterID),
}

create_symbol! {
    /// Represents a lifetime parameter symbol.
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct LifetimeParameter {
        /// The name of the lifetime parameter.
        pub name: String,

        /// The ID of the parent genericable.
        pub parent_id: GenericableID,
    }
}

create_symbol! {
    /// Represents a type parameter symbol.
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct TypeParameter {
        /// The name of the type parameter.
        pub name: String,

        /// The ID of the parent genericable.
        pub parent_id: GenericableID,
    }
}

/// Is an enumeration of either a lifetime parameter or a static lifetime.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeArgument {
    Static,
    LifetimeParamter(LifetimeParameterID),
}

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner, From)]
pub enum AssociatedTypeConstraint {
    Lifetime(LifetimeArgument),
    Type(Type),
}

#[derive(Debug, Clone)]
pub struct Generics {
    pub type_parameter_order: Vec<TypeParameterID>,
    pub type_parameter_id_by_name: HashMap<String, TypeParameterID>,
    pub lifetime_parameter_order: Vec<LifetimeParameterID>,
    pub lifetime_parameter_id_by_name: HashMap<String, LifetimeParameterID>,
    pub lifetime_bounds: HashMap<LifetimeParameterID, Vec<LifetimeArgument>>,
    pub associated_type_bounds: HashMap<AssociatedType, Vec<AssociatedTypeConstraint>>,
    pub type_parameter_bounds: HashMap<TypeParameterID, Vec<LifetimeArgument>>,
}

/// A trait representing the symbol that can be referred in the global scope and has a clear
/// hierarchy.
pub trait Global {
    /// Returns the ID of the symbol.
    fn id(&self) -> GlobalID;

    /// Returns the name of the symbol.
    fn name(&self) -> &str;

    /// Returns the accessibility of the symbol.
    fn accessibility(&self) -> Accessibility;

    /// Returns the ID of the symbol.
    ///
    /// If the symbol is at the root and has no preceding scope, then it returns `None`.
    fn parent_scoped_id(&self) -> Option<ScopedID>;
}

/// A trait representing the symbol that introduces a new scope and can contain child symbols.
pub trait Scoped: Global {
    /// Returns the ID of the child symbol contained in this scope from the given name.
    fn get_child_id_by_name(&self, name: &str) -> Option<GlobalID>;
}

/// A trait representing the symbol that can be used as a type.
pub trait Typed: Global {}

create_symbol! {
    /// Contains the data of the module symbol.
    #[derive(Debug, Clone)]
    pub struct Module {
        /// The name of the module
        pub name: String,

        /// The accessibility of the module
        pub accessibility: Accessibility,

        /// The parent ID of the module. If `None` then the module is the root module.
        pub parent_module_id: Option<ModuleID>,

        /// Maps the name of the symbol defined in this module to its corresponding ID.
        pub child_ids_by_name: HashMap<String, GlobalID>,
    }
}

impl Global for ModuleSymbol {
    fn id(&self) -> GlobalID { self.id().into() }

    fn name(&self) -> &str { &self.name }

    fn accessibility(&self) -> Accessibility { self.accessibility }

    fn parent_scoped_id(&self) -> Option<ScopedID> { self.parent_module_id.map(ScopedID::Module) }
}

impl Scoped for ModuleSymbol {
    fn get_child_id_by_name(&self, name: &str) -> Option<GlobalID> {
        self.child_ids_by_name.get(name).copied()
    }
}

create_symbol! {
    /// Contains the data of the field symbol.
    #[derive(Debug, Clone)]
    pub struct Field {
        /// The name of the field
        pub name: String,

        /// The accessibility of the field
        pub accessibility: Accessibility,

        /// The struct ID where the field is defined.
        pub parent_struct_id: StructID,

        /// The syntax tree that was used to create the field.
        pub syntax_tree: Arc<StructFieldSyntaxTree>,

        /// The order in which the field was declared.
        pub declaration_order: usize,

        /// The type of the field.
        pub ty: Type,
    }
}

create_symbol! {
    /// Contains the data of the struct symbol.
    #[derive(Debug, Clone)]
    pub struct Struct {
        /// The name of the struct
        pub name: String,

        /// The accessibility of the struct
        pub accessibility: Accessibility,

        /// The parent ID of the struct.
        pub parent_module_id: ModuleID,

        /// The syntax tree that was used to create the struct.
        pub syntax_tree: Arc<StructSignatureSyntaxTree>,

        /// Maps the name of the field to its corresponding ID.
        pub field_ids_by_name: HashMap<String, FieldID>,

        /// Maps the name of the field to its corresponding ID.
        pub generics: Generics,

        /// List of the fields in the order in which they were declared.
        pub field_order: Vec<FieldID>,
    }
}

impl Typed for StructSymbol {}

impl Global for StructSymbol {
    fn id(&self) -> GlobalID { self.id().into() }

    fn name(&self) -> &str { &self.name }

    fn accessibility(&self) -> Accessibility { self.accessibility }

    fn parent_scoped_id(&self) -> Option<ScopedID> { Some(ScopedID::Module(self.parent_module_id)) }
}

create_symbol! {
    /// Contains the data of the enum symbol.
    #[derive(Debug, Clone)]
    pub struct EnumVariant {
        /// The nae of the enum variant
        pub name: String,

        /// The ID of the enum that contains the enum variant.
        pub parent_enum_id: EnumID,

        /// The order in which the enum variant was declared.
        pub declaration_order: usize,

        /// The syntax tree that was used to create the enum variant.
        pub syntax_tree: Arc<IdentifierToken>,
    }
}

impl Global for EnumVariantSymbol {
    fn id(&self) -> GlobalID { self.id().into() }

    fn name(&self) -> &str { &self.name }

    fn accessibility(&self) -> Accessibility { Accessibility::Public }

    fn parent_scoped_id(&self) -> Option<ScopedID> { Some(ScopedID::Enum(self.parent_enum_id)) }
}

create_symbol! {
    /// Contains the data of the enum symbol.
    #[derive(Debug, Clone)]
    pub struct Enum {
        /// The name of the enum
        pub name: String,

        /// The accessibility of the enum
        pub accessibility: Accessibility,

        /// The ID of the module that contains the enum.
        pub parent_module_id: ModuleID,

        /// The syntax tree that was used to create the enum.
        pub syntax_tree: Arc<EnumSignatureSyntaxTree>,

        /// Maps the name of the enum variant to the ID of the enum variant.
        pub variant_ids_by_name: HashMap<String, EnumVariantID>,

        /// List of the enum variants in the order in which they were declared.
        pub variant_order: Vec<EnumVariantID>,
    }
}

impl Typed for EnumSymbol {}

impl Scoped for EnumSymbol {
    fn get_child_id_by_name(&self, name: &str) -> Option<GlobalID> {
        self.variant_ids_by_name
            .get(name)
            .copied()
            .map(GlobalID::EnumVariant)
    }
}

impl Global for EnumSymbol {
    fn id(&self) -> GlobalID { self.id().into() }

    fn name(&self) -> &str { &self.name }

    fn accessibility(&self) -> Accessibility { self.accessibility }

    fn parent_scoped_id(&self) -> Option<ScopedID> { Some(ScopedID::Module(self.parent_module_id)) }
}

/// Represents an overload signature syntax tree.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct FunctionSignatureSyntaxTree {
    pub access_modifier: AccessModifier,
    pub identifier: IdentifierToken,
    pub type_annotation: TypeAnnotation,
}

/// Is an enumeration of all symbols that are allowed to be a parent of a parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum ParentParameterID {
    Function(FunctionID),
}

create_symbol! {
    /// Contains the data of parameter symbol.
    #[derive(Debug, Clone)]
    pub struct Parameter {
        /// The name of the parameter
        pub name: String,

        /// The syntax tree that was used to create the parameter.
        pub syntax_tree: Arc<ParameterSyntaxTree>,

        /// The order in which the parameter was declared.
        pub parent_parameter_id: ParentParameterID,

        /// The order in which the parameter was declared.
        pub declaration_order: usize,

        /// The type of the parameter
        pub ty: Type,

        /// Whether the parameter is mutable.
        pub is_mutable: bool,
    }
}

/// Contains the data of the function signature symbol.
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    /// The name of the overload set.
    pub name: String,

    /// The syntax tree that was used to create the overload.
    pub syntax_tree: Arc<FunctionSignatureSyntaxTree>,

    /// The accessibility of the overload.
    pub accessibility: Accessibility,

    /// Maps the name of the parameter to its corresponding ID.
    pub parameter_ids_by_name: HashMap<String, ParameterID>,

    /// List of the parameters in the order in which they were declared.
    pub parameter_order: Vec<ParameterID>,

    /// The return type of the overload.
    pub return_type: Type,

    /// The generics of the overload.
    pub generics: Generics,
}

/// The syntax tree of the function  body.
#[derive(Debug, Clone)]
pub struct FunctionBodySyntaxTree {
    /// List of statements in the function body.
    pub statements: Vec<StatementSyntaxTree>,
}

create_symbol! {
    /// Contains the data of function symbol.
    #[derive(Debug, Clone, Deref, DerefMut)]
    pub struct Function {
        /// Contains the data of function signature.
        #[deref]
        #[deref_mut]
        pub function_signature: FunctionSignature,

        /// The ID of the module that contains the function.
        pub parent_module_id: ModuleID,

        /// The syntax tree of the function body.
        pub definition_syntax_tree: Arc<FunctionBodySyntaxTree>,
    }
}

/// Is an enumeration of symbols that can be used as a parent of an overload set.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ParentOverloadSetID {
    Module(ModuleID),
    Trait(TraitID),
}

create_symbol! {
    /// Contains the data of type alias symbol.
    #[derive(Debug, Clone)]
    pub struct TypeAlias {
        /// The name of the type alias.
        pub name: String,

        /// The accessibility of the type alias.
        pub accessibility: Accessibility,

        /// The ID of the parent symbol that contains the type alias.
        pub parent_module_id: ModuleID,

        /// The type that the type alias represents.
        pub alias: Type,

        /// The syntax tree that was used to create the type alias.
        pub syntax_tree: Arc<TypeSyntaxTree>
    }
}

impl Global for TypeAliasSymbol {
    fn id(&self) -> GlobalID { self.id().into() }

    fn name(&self) -> &str { &self.name }

    fn accessibility(&self) -> Accessibility { self.accessibility }

    fn parent_scoped_id(&self) -> Option<ScopedID> { Some(self.parent_module_id.into()) }
}

/// Is an enumeration of ID of the symbols that can be used as a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum TypedID {
    Enum(EnumID),
    Struct(StructID),
}

impl From<TypedID> for GlobalID {
    fn from(value: TypedID) -> Self {
        match value {
            TypedID::Enum(id) => Self::Enum(id),
            TypedID::Struct(id) => Self::Struct(id),
        }
    }
}

impl From<TypedID> for ID {
    fn from(value: TypedID) -> Self {
        match value {
            TypedID::Enum(id) => Self::Enum(id),
            TypedID::Struct(id) => Self::Struct(id),
        }
    }
}

/// Is an enumeration of ID of the symbols that introduce a new scope.
#[derive(Debug, Clone, Copy, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ScopedID {
    Module(ModuleID),
    Enum(EnumID),
    Trait(TraitID),
}

impl From<ScopedID> for GlobalID {
    fn from(value: ScopedID) -> Self {
        match value {
            ScopedID::Module(id) => Self::Module(id),
            ScopedID::Enum(id) => Self::Enum(id),
            ScopedID::Trait(id) => Self::Trait(id),
        }
    }
}

impl From<ScopedID> for ID {
    fn from(value: ScopedID) -> Self {
        match value {
            ScopedID::Module(id) => Self::Module(id),
            ScopedID::Enum(id) => Self::Enum(id),
            ScopedID::Trait(id) => Self::Trait(id),
        }
    }
}

/// Is an enumeration of ID of the symbols that can be referred in the global scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum GlobalID {
    Module(ModuleID),
    Struct(StructID),
    Enum(EnumID),
    EnumVariant(EnumVariantID),
    Function(FunctionID),
    TypeAlias(TypeAliasID),
    Trait(TraitID),
}

impl From<GlobalID> for ID {
    fn from(value: GlobalID) -> Self {
        match value {
            GlobalID::Module(id) => Self::Module(id),
            GlobalID::Struct(id) => Self::Struct(id),
            GlobalID::Enum(id) => Self::Enum(id),
            GlobalID::Function(id) => Self::Function(id),
            GlobalID::TypeAlias(id) => Self::TypeAlias(id),
            GlobalID::EnumVariant(id) => Self::EnumVariant(id),
            GlobalID::Trait(id) => Self::Trait(id),
        }
    }
}

/// Is an enumeration of Self of the symbols that are defined by global symbols and local to its
/// parent.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum LocalID {
    Field(FieldID),
    Parameter(ParameterID),
    LifetimeParameter(LifetimeParameterID),
    TypeParameter(TypeParameterID),
    AssociatedType(AssociatedTypeID),
    TraitFunction(TraitFunctionID),
}

impl From<LocalID> for ID {
    fn from(value: LocalID) -> Self {
        match value {
            LocalID::Field(id) => Self::Field(id),
            LocalID::Parameter(id) => Self::Parameter(id),
            LocalID::LifetimeParameter(id) => Self::LifetimeParameter(id),
            LocalID::TypeParameter(id) => Self::TypeParameter(id),
            LocalID::AssociatedType(id) => Self::AssociatedType(id),
            LocalID::TraitFunction(id) => Self::TraitFunction(id),
        }
    }
}

/// Is an enumeration of all kinds of symbol IDs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ID {
    Module(ModuleID),
    Struct(StructID),
    Enum(EnumID),
    EnumVariant(EnumVariantID),
    Function(FunctionID),
    TypeAlias(TypeAliasID),
    Field(FieldID),
    Parameter(ParameterID),
    Trait(TraitID),
    AssociatedType(AssociatedTypeID),
    TypeParameter(TypeParameterID),
    LifetimeParameter(LifetimeParameterID),
    TraitFunction(TraitFunctionID),
}
