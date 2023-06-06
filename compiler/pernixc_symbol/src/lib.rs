//! Contains definitions related to the symbol resolution pass of the compiler.

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

use std::{collections::HashMap, convert::Into, fmt::Debug, hash::Hash, sync::Arc};

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

pub mod error;
pub mod table;
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
    /// Represents an trait-member associated type symbol.
    #[derive(Debug, Clone)]
    pub struct AssociatedType {
        /// The name of the associated type.
        pub name: String,

        /// The list of lifetime bounds that the associated type must satisfy.
        pub lifetime_bounds: Vec<LifetimeArgument>,

        /// The generics of the associated type.
        pub generics: Generics,

        /// The ID of the trait that contais the associated type.
        pub parent_trait_id: TraitID,
    }
}

impl Symbol for AssociatedTypeSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_trait_id.into()) }
}

impl Global for AssociatedTypeSymbol {
    fn name(&self) -> &str { &self.name }
}

/// Represents generic parameters substitution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Substitution {
    /// Maps type parameters to their type arguments.
    pub type_arguments_by_parameter: HashMap<TypeParameterID, Type>,

    /// Maps lifetime parameters to their lifetime arguments.
    pub lifetime_arguments_by_parameter: HashMap<LifetimeParameterID, LifetimeArgument>,
}

impl Substitution {
    /// Checks if the type parameter substitution is empty.
    #[must_use]
    pub fn type_parameter_is_empty(&self) -> bool { self.type_arguments_by_parameter.is_empty() }

    /// Checks if the lifetime parameter substitution is empty.
    #[must_use]
    pub fn lifetime_parameter_is_empty(&self) -> bool {
        self.lifetime_arguments_by_parameter.is_empty()
    }

    /// Checks if both type and lifetime parameter substitutions are empty.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.type_parameter_is_empty() && self.lifetime_parameter_is_empty()
    }
}

/// Represents a trait bound constraint.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitBound {
    /// The ID of the trait that is bound.
    pub trait_id: TraitID,

    /// Contains the generic parameters substitution.
    pub substitution: Substitution,
}

create_symbol! {
    /// Represents an implements block.
    #[derive(Debug, Clone)]
    pub struct Implements {
        /// The generics of the implements block.
        pub generics: Generics,

        /// The trait that is implemented.
        pub trait_id: TraitID,

        /// Contains the generic parameters substitution of the trait.
        pub substitution: Substitution,

        /// Maps associated type to their type implementation.
        pub implements_types_by_associated_type: HashMap<AssociatedTypeID, ImplementsTypeID>,

        /// Maps function to their function implementation.
        pub implements_functions_by_trait_function: HashMap<TraitFunctionID, ImplementsFunctionID>,
   }
}

impl Symbol for ImplementsSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { None }
}

create_symbol! {
    /// Represents an implements-member type.
    #[derive(Debug, Clone)]
    pub struct ImplementsType {
        /// The generic parameters of the type.
        pub generics: Generics,

        /// The ID of the associated type that is implemented.
        pub associated_type_id: AssociatedTypeID,

        /// The type that implements the associated type.
        pub alias: Type,

        /// The ID of the implements that contains this symbol.
        pub parent_implements_id: ImplementsID,
    }
}

impl Symbol for ImplementsTypeSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_implements_id.into()) }
}

impl Genericable for ImplementsTypeSymbol {
    fn generics(&self) -> &Generics { &self.generics }
}

create_symbol! {
    /// Represents an implements-member function.
    #[derive(Debug, Clone, Deref, DerefMut)]
    pub struct ImplementsFunction {
        /// Contains the data of function signature.
        #[deref]
        #[deref_mut]
        pub function_signature: FunctionSignature,

        /// The ID of the implements that contains the function.
        pub parent_implements_id: TraitID,

        /// The ID of the trait function that is implemented.
        pub trait_function_id: TraitFunctionID,
    }
}

impl Symbol for ImplementsFunctionSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_implements_id.into()) }
}

impl Genericable for ImplementsFunctionSymbol {
    fn generics(&self) -> &Generics { &self.generics }
}

/// Is an enumeration of symbol IDs that can be used as a trait member.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum TraitMemberID {
    TraitFunction(TraitFunctionID),
    AssociatedType(AssociatedTypeID),
}

impl From<TraitMemberID> for GlobalID {
    fn from(id: TraitMemberID) -> Self {
        match id {
            TraitMemberID::TraitFunction(id) => id.into(),
            TraitMemberID::AssociatedType(id) => id.into(),
        }
    }
}

create_symbol! {
    /// Represents a trait symbol.
    #[derive(Debug, Clone)]
    pub struct Trait {
        /// The name of the trait.
        pub name: String,

        /// The ID of the module that contains this trait.
        pub parent_module_id: ModuleID,

        /// The generics of the trait.
        pub generics: Generics,

        /// The list of implements of the trait.
        pub implements: Vec<ImplementsID>,

        /// Maps the name of the trait member to its ID.
        pub trait_member_ids_by_name: HashMap<String, TraitMemberID>,
    }
}

impl Symbol for TraitSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_module_id.into()) }
}

impl Global for TraitSymbol {
    fn name(&self) -> &str { &self.name }
}

impl Scoped for TraitSymbol {
    fn get_child_id_by_name(&self, name: &str) -> Option<GlobalID> {
        self.trait_member_ids_by_name
            .get(name)
            .copied()
            .map(Into::into)
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

impl Symbol for TraitFunctionSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_trait_id.into()) }
}

impl Global for TraitFunctionSymbol {
    fn name(&self) -> &str { &self.name }
}

impl Genericable for TraitFunctionSymbol {
    fn generics(&self) -> &Generics { &self.generics }
}

/// Is an enumeration of symbol IDs that accept generic parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum GenericableID {
    Struct(StructID),
    Function(FunctionID),
    Implements(ImplementsID),
    Trait(TraitID),
    AssociatedType(AssociatedTypeID),
    TraitFunction(TraitFunctionID),
    TypeAlias(TypeAliasID),
    ImplementsFunction(ImplementsFunctionID),
    ImplementsType(ImplementsTypeID),
}

impl From<GenericableID> for ID {
    fn from(id: GenericableID) -> Self {
        match id {
            GenericableID::Struct(id) => id.into(),
            GenericableID::Function(id) => id.into(),
            GenericableID::Implements(id) => id.into(),
            GenericableID::Trait(id) => id.into(),
            GenericableID::AssociatedType(id) => id.into(),
            GenericableID::TraitFunction(id) => id.into(),
            GenericableID::TypeAlias(id) => id.into(),
            GenericableID::ImplementsFunction(id) => id.into(),
            GenericableID::ImplementsType(id) => id.into(),
        }
    }
}

/// Represents a symbol with generics
pub trait Genericable: Symbol {
    /// Returns the generics of the symbol.
    fn generics(&self) -> &Generics;
}

impl Genericable for StructSymbol {
    fn generics(&self) -> &Generics { &self.generics }
}

impl Genericable for FunctionSymbol {
    fn generics(&self) -> &Generics { &self.generics }
}

impl Genericable for ImplementsSymbol {
    fn generics(&self) -> &Generics { &self.generics }
}

impl Genericable for TraitSymbol {
    fn generics(&self) -> &Generics { &self.generics }
}

impl Genericable for AssociatedTypeSymbol {
    fn generics(&self) -> &Generics { &self.generics }
}

/// Is a trait for symbols that can be used as a generic parameter.
pub trait GenericParameter: Symbol {
    /// Returns the name of the generic parameter.
    fn name(&self) -> &str;

    /// Returns the ID of the parent genericable.
    fn parent_genericable_id(&self) -> GenericableID;
}

impl GenericParameter for LifetimeParameterSymbol {
    fn name(&self) -> &str { &self.name }

    fn parent_genericable_id(&self) -> GenericableID { self.parent_genericable_id }
}

impl GenericParameter for TypeParameterSymbol {
    fn name(&self) -> &str { &self.name }

    fn parent_genericable_id(&self) -> GenericableID { self.parent_genericable_id }
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
        pub parent_genericable_id: GenericableID,
    }
}

impl Symbol for LifetimeParameterSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_genericable_id.into()) }
}

create_symbol! {
    /// Represents a type parameter symbol.
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct TypeParameter {
        /// The name of the type parameter.
        pub name: String,

        /// The ID of the parent genericable.
        pub parent_genericable_id: GenericableID,
    }
}

impl Symbol for TypeParameterSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_genericable_id.into()) }
}

/// Is an enumeration of either a lifetime parameter or a static lifetime.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeArgument {
    Static,
    LifetimeParamter(LifetimeParameterID),
}

/// Contains all the information related to the generics.
#[derive(Debug, Clone)]
pub struct Generics {
    /// The declaration order of the type parameters.
    pub type_parameter_order: Vec<TypeParameterID>,

    /// Maps the name of the type parameter to its ID.
    pub type_parameter_id_by_name: HashMap<String, TypeParameterID>,

    /// The declaration order of the lifetime parameters.
    pub lifetime_parameter_order: Vec<LifetimeParameterID>,

    /// Maps the name of the lifetime parameter to its ID.
    pub lifetime_parameter_id_by_name: HashMap<String, LifetimeParameterID>,

    /// Maps the lifetime parameter to its lifetime bounds.
    pub lifetime_bounds: HashMap<LifetimeParameterID, Vec<LifetimeArgument>>,

    /// Maps the associated type to its type bound.
    pub associated_type_bounds: HashMap<AssociatedType, Type>,

    /// Maps the type parameter to its lifetime bounds.
    pub type_parameter_bounds: HashMap<TypeParameterID, Vec<LifetimeArgument>>,
}

/// Is a trait that all symbols must implement.
pub trait Symbol {
    /// The ID of the symbol.
    fn id(&self) -> ID;

    /// The ID of the symbol in the higher level.
    fn parent_symbol(&self) -> Option<ID>;
}

/// A trait representing the symbol that can be referred in the global scope and has a clear
/// hierarchy.
pub trait Global: Symbol {
    /// Returns the name of the symbol.
    fn name(&self) -> &str;
}

/// A trait representing the symbol that introduces a new scope and can contain child symbols.
pub trait Scoped: Global {
    /// Returns the ID of the child symbol contained in this scope from the given name.
    fn get_child_id_by_name(&self, name: &str) -> Option<GlobalID>;
}

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

        /// The IDs of modules that are used in the `using` statements.
        pub usings: Vec<ModuleID>,
    }
}

impl Symbol for ModuleSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { self.parent_module_id.map(ID::Module) }
}

impl Global for ModuleSymbol {
    fn name(&self) -> &str { &self.name }
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

impl Symbol for FieldSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_struct_id.into()) }
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

impl Symbol for StructSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_module_id.into()) }
}

impl Global for StructSymbol {
    fn name(&self) -> &str { &self.name }
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

impl Symbol for EnumVariantSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_enum_id.into()) }
}

impl Global for EnumVariantSymbol {
    fn name(&self) -> &str { &self.name }
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

impl Symbol for EnumSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_module_id.into()) }
}

impl Scoped for EnumSymbol {
    fn get_child_id_by_name(&self, name: &str) -> Option<GlobalID> {
        self.variant_ids_by_name
            .get(name)
            .copied()
            .map(GlobalID::EnumVariant)
    }
}

impl Global for EnumSymbol {
    fn name(&self) -> &str { &self.name }
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
    TraitFunction(TraitFunctionID),
}

impl From<ParentParameterID> for ID {
    fn from(id: ParentParameterID) -> Self {
        match id {
            ParentParameterID::Function(id) => id.into(),
            ParentParameterID::TraitFunction(id) => id.into(),
        }
    }
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

impl Symbol for ParameterSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_parameter_id.into()) }
}

/// Contains the data of the function signature symbol.
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    /// The name of the overload set.
    pub name: String,

    /// The syntax tree that was used to create the overload.
    pub syntax_tree: Arc<FunctionSignatureSyntaxTree>,

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

impl Symbol for FunctionSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_module_id.into()) }
}

impl Global for FunctionSymbol {
    fn name(&self) -> &str { &self.name }
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

        /// The accessibility of the function.
        pub accessibility: Accessibility,
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

        /// The generics of the type alias.
        pub generics: Generics,

        /// The syntax tree that was used to create the type alias.
        pub syntax_tree: Arc<TypeSyntaxTree>
    }
}

impl Genericable for TypeAliasSymbol {
    fn generics(&self) -> &Generics { &self.generics }
}

impl Symbol for TypeAliasSymbol {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_module_id.into()) }
}

impl Global for TypeAliasSymbol {
    fn name(&self) -> &str { &self.name }
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
    TraitFunction(TraitFunctionID),
    AssociatedType(AssociatedTypeID),
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
            GlobalID::TraitFunction(id) => Self::TraitFunction(id),
            GlobalID::AssociatedType(id) => Self::AssociatedType(id),
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
    Implements(ImplementsID),
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
            LocalID::Implements(id) => Self::Implements(id),
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
    Implements(ImplementsID),
    ImplementsFunction(ImplementsFunctionID),
    ImplementsType(ImplementsTypeID),
}
