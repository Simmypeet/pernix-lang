//! Contains the code related to the symbol resolution pass of the compiler.

use std::{collections::HashMap, fmt::Debug, hash::Hash, sync::Arc};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_lexical::token::{Identifier as IdentifierToken, Keyword};
use pernixc_syntax::syntax_tree::{
    expression::BlockWithoutLabel,
    item::{
        AccessModifier, EnumSignature as EnumSignatureSyntaxTree, Field as FieldSyntaxTree,
        Parameter as ParameterSyntaxTree, StructSignature as StructSignatureSyntaxTree,
        TypeAlias as TypeAliasSyntaxTree,
    },
    TypeAnnotation,
};
use pernixc_system::create_symbol;

use self::ty::{Type, TypeBinding};

pub mod error;
pub mod table;
pub mod ty;

/// The accessibility of the symbol. Determines where the symbol can be accessed from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
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
    #[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
    pub struct Module {
        /// The name of the module
        #[get = "pub"]
        name: String,

        /// The accessibility of the module
        #[get_copy = "pub"]
        accessibility: Accessibility,

        /// The parent ID of the module. If `None` then the module is the root module.
        #[get_copy = "pub"]
        parent_module_id: Option<ModuleID>,

        /// Maps the name of the symbol defined in this module to its corresponding ID.
        #[get = "pub"]
        child_ids_by_name: HashMap<String, GlobalID>,
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
    #[derive(Debug, Clone, PartialEq, Eq,  Hash, Getters, CopyGetters)]
    pub struct Field {
        /// The name of the field
        #[get = "pub"]
        name: String,

        /// The accessibility of the field
        #[get_copy = "pub"]
        accessibility: Accessibility,

        /// The struct ID where the field is defined.
        #[get_copy = "pub"]
        parent_struct_id: StructID,

        /// The syntax tree that was used to create the field.
        #[get = "pub"]
        syntax_tree: Arc<FieldSyntaxTree>,

        /// The order in which the field was declared.
        #[get_copy = "pub"]
        declaration_order: usize,

        /// The type of the field.
        #[get_copy = "pub"]
        ty: Type,
    }
}

create_symbol! {
    /// Contains the data of the struct symbol.
    #[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
    pub struct Struct {
        /// The name of the struct
        #[get = "pub"]
        name: String,

        /// The accessibility of the struct
        #[get_copy = "pub"]
        accessibility: Accessibility,

        /// The parent ID of the struct.
        #[get_copy = "pub"]
        parent_module_id: ModuleID,

        /// The syntax tree that was used to create the struct.
        #[get = "pub"]
        syntax_tree: Arc<StructSignatureSyntaxTree>,

        /// Maps the name of the field to its corresponding ID.
        #[get = "pub"]
        field_ids_by_name: HashMap<String, FieldID>,

        /// List of the fields in the order in which they were declared.
        #[get = "pub"]
        field_order: Vec<FieldID>,

        /// Maps the name of the type alias defined in this struct to its corresponding ID.
        #[get = "pub"]
        type_alias_ids_by_name: HashMap<String, TypeAliasID>,
    }
}

impl Typed for StructSymbol {}

impl Scoped for StructSymbol {
    fn get_child_id_by_name(&self, name: &str) -> Option<GlobalID> {
        self.type_alias_ids_by_name
            .get(name)
            .copied()
            .map(GlobalID::TypeAlias)
    }
}

impl Global for StructSymbol {
    fn id(&self) -> GlobalID { self.id().into() }

    fn name(&self) -> &str { &self.name }

    fn accessibility(&self) -> Accessibility { self.accessibility }

    fn parent_scoped_id(&self) -> Option<ScopedID> { Some(ScopedID::Module(self.parent_module_id)) }
}

create_symbol! {
    /// Contains the data of the enum symbol.
    #[derive(Debug, Clone, PartialEq, Eq,  Hash, Getters, CopyGetters)]
    pub struct EnumVariant {
        /// The name of the enum variant
        #[get = "pub"]
        name: String,

        /// The ID of the enum that contains the enum variant.
        #[get_copy = "pub"]
        parent_enum_id: EnumID,

        /// The order in which the enum variant was declared.
        #[get_copy = "pub"]
        declaration_order: usize,

        /// The syntax tree that was used to create the enum variant.
        #[get = "pub"]
        syntax_tree: Arc<IdentifierToken>,
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
    #[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
    pub struct Enum {
        /// The name of the enum
        #[get = "pub"]
        name: String,

        /// The accessibility of the enum
        #[get_copy = "pub"]
        accessibility: Accessibility,

        /// The ID of the module that contains the enum.
        #[get_copy = "pub"]
        parent_module_id: ModuleID,

        /// The syntax tree that was used to create the enum.
        #[get = "pub"]
        syntax_tree: Arc<EnumSignatureSyntaxTree>,

        /// Maps the name of the enum variant to the ID of the enum variant.
        #[get = "pub"]
        variant_ids_by_name: HashMap<String, EnumVariantID>,

        /// List of the enum variants in the order in which they were declared.
        #[get = "pub"]
        variant_order: Vec<EnumVariantID>,
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
#[allow(missing_docs)]
pub struct OverloadSyntaxTree {
    pub access_modifier: AccessModifier,
    pub function_keyword: Keyword,
    pub identifier: IdentifierToken,
    pub type_annotation: TypeAnnotation,
    pub block_without_label: BlockWithoutLabel,
}

create_symbol! {
    /// Contains the data of parameter symbol.
    #[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
    pub struct Parameter {
        /// The name of the parameter
        #[get = "pub"]
        name: String,

        /// The syntax tree that was used to create the parameter.
        #[get = "pub"]
        syntax_tree: Arc<ParameterSyntaxTree>,

        /// The order in which the parameter was declared.
        #[get_copy = "pub"]
        parent_overload_id: OverloadID,

        /// The order in which the parameter was declared.
        #[get_copy = "pub"]
        declaration_order: usize,

        /// The type binding of the parameter.
        #[get_copy = "pub"]
        type_binding: TypeBinding,
    }
}

create_symbol! {
    /// Contains the data of overload symbol.
    #[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
    pub struct Overload {
        /// The ID of the overload set that contains the overload.
        #[get_copy = "pub"]
        parent_overload_set_id: OverloadSetID,

        /// The syntax tree that was used to create the overload.
        #[get = "pub"]
        syntax_tree: Arc<OverloadSyntaxTree>,

        /// The accessibility of the overload.
        #[get_copy = "pub"]
        accessibility: Accessibility,

        /// Maps the name of the parameter to its corresponding ID.
        #[get = "pub"]
        parameter_ids_by_name: HashMap<String, ParameterID>,

        /// List of the parameters in the order in which they were declared.
        #[get = "pub"]
        parameter_order: Vec<ParameterID>,

        /// The return type of the overload.
        #[get_copy = "pub"]
        return_type: Type,
    }
}

create_symbol! {
    /// Contains the data of overload set symbol.
    #[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
    pub struct OverloadSet {
        /// The ID of the module that contains the overload set.
        #[get_copy = "pub"]
        parent_module_id: ModuleID,

        /// The name of the overload set.
        #[get = "pub"]
        name: String,

        /// Maps the name of the overload to its corresponding ID.
        #[get = "pub"]
        overloads: Vec<OverloadID>,
    }
}

impl Global for OverloadSetSymbol {
    fn id(&self) -> GlobalID { self.id().into() }

    fn name(&self) -> &str { &self.name }

    fn accessibility(&self) -> Accessibility { Accessibility::Public }

    fn parent_scoped_id(&self) -> Option<ScopedID> { Some(ScopedID::Module(self.parent_module_id)) }
}

create_symbol! {
    /// Contains the data of type alias symbol.
    #[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
    pub struct TypeAlias {
        /// The name of the type alias.
        #[get = "pub"]
        name: String,

        /// The accessibility of the type alias.
        #[get_copy = "pub"]
        accessibility: Accessibility,

        /// The ID of the parent symbol that contains the type alias.
        #[get_copy = "pub"]
        type_alias_parent_id: TypeAliasParentID,

        /// The type that the type alias represents.
        #[get_copy = "pub"]
        alias: Type,

        /// The syntax tree that was used to create the type alias.
        #[get = "pub"]
        syntax_tree: Arc<TypeAliasSyntaxTree>
    }
}

impl Global for TypeAliasSymbol {
    fn id(&self) -> GlobalID { self.id().into() }

    fn name(&self) -> &str { &self.name }

    fn accessibility(&self) -> Accessibility { self.accessibility }

    fn parent_scoped_id(&self) -> Option<ScopedID> { Some(self.type_alias_parent_id.into()) }
}

/// Is an enumeration of IDs that can contain a type alias.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum TypeAliasParentID {
    Module(ModuleID),
    Struct(StructID),
}

impl From<TypeAliasParentID> for ScopedID {
    fn from(id: TypeAliasParentID) -> Self {
        match id {
            TypeAliasParentID::Module(id) => Self::Module(id),
            TypeAliasParentID::Struct(id) => Self::Struct(id),
        }
    }
}

impl From<TypeAliasParentID> for GlobalID {
    fn from(id: TypeAliasParentID) -> Self {
        match id {
            TypeAliasParentID::Module(id) => Self::Module(id),
            TypeAliasParentID::Struct(id) => Self::Struct(id),
        }
    }
}

impl From<TypeAliasParentID> for ID {
    fn from(id: TypeAliasParentID) -> Self {
        match id {
            TypeAliasParentID::Module(id) => Self::Module(id),
            TypeAliasParentID::Struct(id) => Self::Struct(id),
        }
    }
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

impl From<TypedID> for ScopedID {
    fn from(value: TypedID) -> Self {
        match value {
            TypedID::Enum(id) => Self::Enum(id),
            TypedID::Struct(id) => Self::Struct(id),
        }
    }
}

/// Is an enumeration of ID of the symbols that introduce a new scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ScopedID {
    Module(ModuleID),
    Struct(StructID),
    Enum(EnumID),
}

impl From<ScopedID> for GlobalID {
    fn from(value: ScopedID) -> Self {
        match value {
            ScopedID::Module(id) => Self::Module(id),
            ScopedID::Struct(id) => Self::Struct(id),
            ScopedID::Enum(id) => Self::Enum(id),
        }
    }
}

impl From<ScopedID> for ID {
    fn from(value: ScopedID) -> Self {
        match value {
            ScopedID::Module(id) => Self::Module(id),
            ScopedID::Struct(id) => Self::Struct(id),
            ScopedID::Enum(id) => Self::Enum(id),
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
    OverloadSet(OverloadSetID),
    TypeAlias(TypeAliasID),
}

impl From<GlobalID> for ID {
    fn from(value: GlobalID) -> Self {
        match value {
            GlobalID::Module(id) => Self::Module(id),
            GlobalID::Struct(id) => Self::Struct(id),
            GlobalID::Enum(id) => Self::Enum(id),
            GlobalID::OverloadSet(id) => Self::OverloadSet(id),
            GlobalID::TypeAlias(id) => Self::TypeAlias(id),
            GlobalID::EnumVariant(id) => Self::EnumVariant(id),
        }
    }
}

/// Is an enumeration of Self of the symbols that are defined by global symbols and local to its
/// parent.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum LocalID {
    Field(FieldID),
    Overload(OverloadID),
    Parameter(ParameterID),
}

impl From<LocalID> for ID {
    fn from(value: LocalID) -> Self {
        match value {
            LocalID::Field(id) => Self::Field(id),
            LocalID::Overload(id) => Self::Overload(id),
            LocalID::Parameter(id) => Self::Parameter(id),
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
    OverloadSet(OverloadSetID),
    TypeAlias(TypeAliasID),
    Field(FieldID),
    Overload(OverloadID),
    Parameter(ParameterID),
}
