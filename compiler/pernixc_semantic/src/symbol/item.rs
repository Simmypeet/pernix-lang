//! Contains all the definitions of the item symbols and the table.

use std::{
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    ops::{Index, IndexMut},
};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_common::source_file::SourceElement;
use pernixc_lexical::token::Identifier as IdentifierToken;
use pernixc_syntax::{
    syntax_tree::{
        expression::BlockWithoutLabel as BlockWithoutLabelSyntaxTree,
        item::{
            Enum as EnumSyntaxTree, EnumSignature as EnumSignatureSyntaxTree,
            Field as FieldSyntaxTree, Function as FunctionSyntaxTree,
            FunctionSignature as FunctionSignatureSyntaxTree, Item as ItemSyntaxTree,
            Member as MemberSyntaxTree, Parameter as ParameterSyntaxTree, ParameterList,
            Struct as StructSyntaxTree, StructSignature as StructSignatureSyntaxTree,
            TypeAlias as TypeAliasSyntaxTree,
        },
        PrimitiveTypeSpecifier, QualifiedIdentifier, TypeSpecifier,
    },
    target_parsing::TargetParsing,
};

use super::{
    errors::{
        CircularDependency, EnumVariantRedefinition, FieldRedefinition, OverloadRedefinition,
        ParameterRedefinition, PrivateSymbolLeak, StructMemberMoreAccessibleThanStruct,
        SymbolError, SymbolRedifinition, TypeExpected,
    },
    ty::{PrimitiveType, Type, TypeBinding},
    SymbolData, SymbolWithData, Uid, UniqueIdentifier,
};
use crate::symbol::errors::{SymbolNotAccessible, SymbolNotFound};

/// Is an unique identifier used to identify a struct in the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructID(Uid);

impl UniqueIdentifier for StructID {
    fn fresh() -> Self { Self(Uid::fresh()) }
}

/// Is an unique identifier used to identify an enum in the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumID(Uid);

impl UniqueIdentifier for EnumID {
    fn fresh() -> Self { Self(Uid::fresh()) }
}

/// Is an unique identifier used to identify a module in the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleID(Uid);

impl UniqueIdentifier for ModuleID {
    fn fresh() -> Self { Self(Uid::fresh()) }
}

/// Is an unique identifier used to identify a function overload set in the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionOverloadSetID(Uid);

impl UniqueIdentifier for FunctionOverloadSetID {
    fn fresh() -> Self { Self(Uid::fresh()) }
}

/// Is an unique identifier used to identify an enum variant in the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumVariantID(Uid);

impl UniqueIdentifier for EnumVariantID {
    fn fresh() -> Self { Self(Uid::fresh()) }
}

/// Is an unique identifier used to identify a type alias in the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeAliasID(Uid);

impl UniqueIdentifier for TypeAliasID {
    fn fresh() -> Self { Self(Uid::fresh()) }
}

/// Is an enumeration of all the identifiers of the symbol that can be used as a type in
/// the program.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, From)]

pub enum TypedID {
    Struct(StructID),
    Enum(EnumID),
}

impl From<TypedID> for ID {
    fn from(typed_id: TypedID) -> Self {
        match typed_id {
            TypedID::Struct(id) => Self::Struct(id),
            TypedID::Enum(id) => Self::Enum(id),
        }
    }
}

/// Is an enumeration of identifier types of the symbols that introduce a new scope to the
/// namespace.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, From, EnumAsInner)]

pub enum ScopedID {
    Struct(StructID),
    Enum(EnumID),
    Module(ModuleID),
}

impl From<ScopedID> for ID {
    fn from(scoped_id: ScopedID) -> Self {
        match scoped_id {
            ScopedID::Struct(id) => Self::Struct(id),
            ScopedID::Enum(id) => Self::Enum(id),
            ScopedID::Module(id) => Self::Module(id),
        }
    }
}

/// Is an enumeration of all the identifiers that can be accessed in the global scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]

pub enum ID {
    Struct(StructID),
    Enum(EnumID),
    Module(ModuleID),
    FunctionOverloadSet(FunctionOverloadSetID),
    EnumVariant(EnumVariantID),
    TypeAlias(TypeAliasID),
}

impl ID {
    /// Returns the [`TypedID`] if the [`ID`] is a subset of [`TypedID`].
    #[must_use]
    pub fn as_typed_id(&self) -> Option<TypedID> {
        match self {
            Self::Struct(id) => Some(TypedID::Struct(*id)),
            Self::Enum(id) => Some(TypedID::Enum(*id)),
            _ => None,
        }
    }

    /// Returns the [`ScopedID`] if the [`ID`] is a subset of [`ScopedID`].
    #[must_use]
    pub fn as_scoped_id(&self) -> Option<ScopedID> {
        match self {
            Self::Struct(id) => Some(ScopedID::Struct(*id)),
            Self::Enum(id) => Some(ScopedID::Enum(*id)),
            Self::Module(id) => Some(ScopedID::Module(*id)),
            _ => None,
        }
    }
}

/// Is an unique identifier used to identify a field in the [`StructData`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldID(Uid);

impl UniqueIdentifier for FieldID {
    fn fresh() -> Self { Self(Uid::fresh()) }
}

/// Is an enumeration of all the access modifiers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Accessibility {
    /// The symbol can be accessed from the same target.
    Internal,

    /// The symbol can be accessed if the reference site shares the same module ancestry.
    Private,

    /// The symbol can be accessed from any where.
    Public,
}

impl Accessibility {
    /// Creates a new [`AccessModifier`] from the given
    /// [`pernixc_syntax::syntax_tree::item::AccessModifier`].
    #[must_use]
    pub fn from_syntax_tree(
        syntax_tree: &pernixc_syntax::syntax_tree::item::AccessModifier,
    ) -> Self {
        match syntax_tree {
            pernixc_syntax::syntax_tree::item::AccessModifier::Internal(..) => Self::Internal,
            pernixc_syntax::syntax_tree::item::AccessModifier::Private(..) => Self::Private,
            pernixc_syntax::syntax_tree::item::AccessModifier::Public(..) => Self::Public,
        }
    }

    /// Returns the rank of the [`AccessModifier`]. The lower the number the more restrictive
    /// the access modifier is.
    #[must_use]
    pub fn restrictiveness_rank(&self) -> usize {
        match self {
            Self::Private => 0,
            Self::Internal => 1,
            Self::Public => 2,
        }
    }
}

/// Represents a field data of a struct.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct Field {
    /// The name of the field.
    #[get = "pub"]
    name: String,

    /// The access modifier of the field.
    #[get_copy = "pub"]
    accessibility: Accessibility,

    /// The type of the field.
    #[get = "pub"]
    ty: Type,

    /// The syntax tree of the field member.
    #[get = "pub"]
    syntax_tree: FieldSyntaxTree,
}

/// Is a data structure that allows mapping between name, ID and data.
#[derive(Debug, Clone)]
pub struct SymbolMap<T: SymbolData> {
    /// Maps the ID to the data.
    values_by_id: HashMap<T::ID, SymbolWithData<T>>,

    /// Maps the name to the ID.
    ids_by_name: HashMap<String, T::ID>,
}

impl<T: SymbolData> SymbolMap<T> {
    /// Creates a new empty [`SymbolMap`].
    #[must_use]
    fn new() -> Self {
        Self {
            values_by_id: HashMap::new(),
            ids_by_name: HashMap::new(),
        }
    }

    /// Adds a new symbol to the [`SymbolMap`].
    ///
    /// # Returns
    /// - `Ok(id)` returns the ID of the symbol of the added symbol.
    ///
    /// # Errors
    /// - `Err(id)` returns the ID of the symbol that already exists with the same name.
    fn add(&mut self, name: String, data: T) -> Result<T::ID, T::ID> {
        if let Some(id) = self.ids_by_name.get(&name).copied() {
            Err(id)
        } else {
            let id = T::ID::fresh();
            let symbol = SymbolWithData { data, id };
            self.ids_by_name.insert(name, id);
            self.values_by_id.insert(id, symbol);
            Ok(id)
        }
    }

    /// Returns the number of symbols in the [`SymbolMap`].
    #[must_use]
    pub fn len(&self) -> usize { self.values_by_id.len() }

    /// Returns `true` if the [`SymbolMap`] is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.values_by_id.is_empty() }

    /// Returns the ID of the symbol with the given name.
    #[must_use]
    pub fn map_name_to_id(&self, name: &str) -> Option<T::ID> {
        self.ids_by_name.get(name).copied()
    }

    /// Returns an iterator over the symbols in the [`SymbolMap`].
    pub fn values(&self) -> impl Iterator<Item = &SymbolWithData<T>> { self.values_by_id.values() }
}

impl<T: SymbolData> Index<T::ID> for SymbolMap<T> {
    type Output = SymbolWithData<T>;

    fn index(&self, id: T::ID) -> &Self::Output { &self.values_by_id[&id] }
}

impl<T: SymbolData> Default for SymbolMap<T> {
    fn default() -> Self { Self::new() }
}

/// Represents a struct data.
#[derive(Debug, Clone, Getters, CopyGetters)]
pub struct StructData {
    /// The fully qualified name of the struct.
    #[get = "pub"]
    qualified_name: Vec<String>,

    /// The field members of the struct.
    #[get = "pub"]
    fields: Vec<Field>,

    /// The type alias members of the struct.
    #[get = "pub"]
    type_alias_member_ids_by_name: HashMap<String, TypeAliasID>,

    /// The access modifier of the struct.
    #[get_copy = "pub"]
    accessibility: Accessibility,

    /// Is the ID of the module that contains the struct.
    #[get_copy = "pub"]
    parent: ModuleID,

    /// The syntax tree that was used to create the struct.
    #[get = "pub"]
    signature_syntax_tree: StructSignatureSyntaxTree,
}

impl SymbolData for StructData {
    type ID = StructID;
}

/// Represents a struct symbol.
pub type Struct = SymbolWithData<StructData>;

/// Represents an enum data.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct EnumData {
    /// The fully qualified name of the enum.
    #[get = "pub"]
    qualified_name: Vec<String>,

    /// The variant members of the enum.
    #[get = "pub"]
    variant_ids_by_name: HashMap<String, EnumVariantID>,

    /// The access modifier of the enum.
    #[get_copy = "pub"]
    accessibility: Accessibility,

    /// Is the ID of the module that contains the enum.
    #[get_copy = "pub"]
    parent: ModuleID,

    /// The syntax tree that was used to create the enum.
    #[get = "pub"]
    signature_syntax_tree: EnumSignatureSyntaxTree,
}

impl SymbolData for EnumData {
    type ID = EnumID;
}

/// Represents an enum symbol.
pub type Enum = SymbolWithData<EnumData>;

/// Represents a module data.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct ModuleData {
    /// Is the ID of the module that contains this module. If this module is the root module,
    /// then this field is `None`.
    #[get_copy = "pub"]
    parent: Option<ModuleID>,

    /// The fully qualified name of the module.
    #[get = "pub"]
    qualified_name: Vec<String>,

    /// The items that are contained in the module.
    #[get = "pub"]
    children_ids_by_name: HashMap<String, ID>,

    /// The access modifier of the module.
    #[get_copy = "pub"]
    accessibility: Accessibility,
}

impl SymbolData for ModuleData {
    type ID = ModuleID;
}

/// Represents a module symbol.
pub type Module = SymbolWithData<ModuleData>;

/// Represents a parameter data.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct Parameter {
    /// The name of the parameter
    #[get = "pub"]
    name: String,

    /// The type binding of this parameter.
    #[get_copy = "pub"]
    type_binding: TypeBinding,

    /// The syntax tree that was used to create the parameter.
    #[get = "pub"]
    syntax_tree: ParameterSyntaxTree,
}

/// Is an unique identifier used to identify a function overload in the [`FunctionOverloadSetData`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OverloadID(Uid);

impl UniqueIdentifier for OverloadID {
    fn fresh() -> Self { Self(Uid::fresh()) }
}

/// Represents a data of a single function overload.
#[derive(Debug, Clone, Getters, CopyGetters)]
pub struct OverloadData {
    /// The ID of the function overload set that contains this overload.
    #[get_copy = "pub"]
    function_overload_set_id: FunctionOverloadSetID,

    /// The return type of the function overload.
    #[get = "pub"]
    return_type: Type,

    /// The parameters of the function overload.
    #[get = "pub"]
    parameters: Vec<Parameter>,

    /// The identifier token of the function overload.
    #[get = "pub"]
    identifier_token: IdentifierToken,

    /// The return type specifier of the function overload.
    #[get = "pub"]
    return_type_syntax_tree: TypeSpecifier,

    /// The body syntax tree of the function overload.
    #[get = "pub"]
    body_syntax_tree: BlockWithoutLabelSyntaxTree,

    /// The accessibility of the function overload.
    #[get_copy = "pub"]
    accessibility: Accessibility,
}

impl SymbolData for OverloadData {
    type ID = OverloadID;
}

/// Represents a function overload symbol.
pub type Overload = SymbolWithData<OverloadData>;

/// Represents a function overload set data.
#[derive(Debug, Clone, Getters, CopyGetters)]
pub struct FunctionOverloadSetData {
    /// The fully qualified name of the function overload set.
    #[get = "pub"]
    qualified_name: Vec<String>,

    /// Is the ID of the module that contains the function overload set.
    #[get_copy = "pub"]
    parent: ModuleID,

    /// Maps the ID of the overload to the overload.
    #[get = "pub"]
    overloads_by_id: HashMap<OverloadID, Overload>,
}

impl SymbolData for FunctionOverloadSetData {
    type ID = FunctionOverloadSetID;
}

/// Represents a function overload set symbol.
pub type FunctionOverloadSet = SymbolWithData<FunctionOverloadSetData>;

/// Represents an enum variant data.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct EnumVariantData {
    /// The parent enum of the variant.
    #[get_copy = "pub"]
    parent: EnumID,

    /// The fully qualified name of the variant.
    #[get = "pub"]
    qualified_name: Vec<String>,

    /// The order of declaration of the variant.
    #[get_copy = "pub"]
    variant_number: u64,

    /// The syntax tree that was used to create the variant.
    #[get = "pub"]
    syntax_tree: IdentifierToken,
}

impl SymbolData for EnumVariantData {
    type ID = EnumVariantID;
}

/// Represents an enum variant symbol.
pub type EnumVariant = SymbolWithData<EnumVariantData>;

/// Represents a type alias data.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct TypeAliasData {
    /// The fully qualified name of the type alias.
    #[get = "pub"]
    qualified_name: Vec<String>,

    /// The type of this alias.
    #[get_copy = "pub"]
    ty: Type,

    /// The access modifier of the type alias.
    #[get_copy = "pub"]
    accessibility: Accessibility,

    /// Is the ID of the scope that contains the type alias (it can either be a module or a
    /// struct).
    #[get_copy = "pub"]
    parent: ScopedID,

    /// The syntax tree that was used to create the type alias.
    #[get = "pub"]
    syntax_tree: TypeAliasSyntaxTree,
}

impl SymbolData for TypeAliasData {
    type ID = TypeAliasID;
}

/// Represents a type alias symbol.
pub type TypeAlias = SymbolWithData<TypeAliasData>;

/// Is an enumeration of all symbols that can be accessed in the global scope.
#[derive(Debug, Clone, EnumAsInner, From)]

pub enum Item {
    Module(Module),
    Struct(Struct),
    FunctionOverloadSet(FunctionOverloadSet),
    TypeAlias(TypeAlias),
    Enum(Enum),
    EnumVariant(EnumVariant),
}

impl Item {
    /// Gets the fully qualified name of the item.
    #[must_use]
    pub fn qualified_name(&self) -> &Vec<String> {
        match self {
            Self::Module(module) => module.qualified_name(),
            Self::Struct(struct_) => struct_.qualified_name(),
            Self::FunctionOverloadSet(function_overload_set) => {
                function_overload_set.qualified_name()
            }
            Self::TypeAlias(type_alias) => type_alias.qualified_name(),
            Self::Enum(enum_) => enum_.qualified_name(),
            Self::EnumVariant(enum_variant) => enum_variant.qualified_name(),
        }
    }

    /// Gets the parent of the item.
    #[must_use]
    pub fn parent(&self) -> Option<ScopedID> {
        match self {
            Self::Module(sym) => sym.parent.map(std::convert::Into::into),
            Self::Struct(sym) => Some(sym.parent.into()),
            Self::FunctionOverloadSet(sym) => Some(sym.parent.into()),
            Self::TypeAlias(sym) => Some(sym.parent),
            Self::Enum(sym) => Some(sym.parent.into()),
            Self::EnumVariant(sym) => Some(sym.parent.into()),
        }
    }

    /// Gets the access modifier of the item.
    #[must_use]
    pub fn accessibility(&self) -> Option<Accessibility> {
        match self {
            Self::Module(sym) => Some(sym.accessibility),
            Self::Struct(sym) => Some(sym.accessibility),
            Self::TypeAlias(sym) => Some(sym.accessibility),
            Self::Enum(sym) => Some(sym.accessibility),
            Self::FunctionOverloadSet(..) | Self::EnumVariant(..) => None,
        }
    }
}

/// Represents a symbol table.
#[derive(Debug, Clone, Getters, CopyGetters)]
pub struct Table {
    items_by_id: HashMap<ID, Item>,

    root_ids_by_name: HashMap<String, ModuleID>,
}

impl Table {
    /// Gets the [`ID`] of the symbol from the given fully qualified name.
    ///
    /// The search starts from the root module.
    #[must_use]
    pub fn get_id_by_full_name<'a>(
        &self,
        identifiers: impl Iterator<Item = &'a str>,
    ) -> Option<ID> {
        let mut current_id: Option<ID> = None;

        for name in identifiers {
            if let Some(id) = current_id {
                match id {
                    ID::Module(id) => {
                        current_id = Some(self[id].children_ids_by_name.get(name).copied()?);
                    }
                    ID::Enum(id) => {
                        current_id = Some(self[id].variant_ids_by_name.get(name).copied()?.into());
                    }
                    ID::Struct(id) => {
                        current_id = Some(
                            self[id]
                                .type_alias_member_ids_by_name
                                .get(name)
                                .copied()?
                                .into(),
                        );
                    }
                    _ => return None,
                }
            } else {
                current_id = Some(self.root_ids_by_name.get(name).copied()?.into());
            }
        }

        current_id
    }
}

impl Index<ModuleID> for Table {
    type Output = Module;

    fn index(&self, index: ModuleID) -> &Self::Output {
        self.items_by_id[&index.into()].as_module().unwrap()
    }
}
impl IndexMut<ModuleID> for Table {
    fn index_mut(&mut self, index: ModuleID) -> &mut Self::Output {
        self.items_by_id
            .get_mut(&index.into())
            .unwrap()
            .as_module_mut()
            .unwrap()
    }
}

impl Index<StructID> for Table {
    type Output = Struct;

    fn index(&self, index: StructID) -> &Self::Output {
        self.items_by_id[&index.into()].as_struct().unwrap()
    }
}
impl IndexMut<StructID> for Table {
    fn index_mut(&mut self, index: StructID) -> &mut Self::Output {
        self.items_by_id
            .get_mut(&index.into())
            .unwrap()
            .as_struct_mut()
            .unwrap()
    }
}

impl Index<FunctionOverloadSetID> for Table {
    type Output = FunctionOverloadSet;

    fn index(&self, index: FunctionOverloadSetID) -> &Self::Output {
        self.items_by_id[&index.into()]
            .as_function_overload_set()
            .unwrap()
    }
}
impl IndexMut<FunctionOverloadSetID> for Table {
    fn index_mut(&mut self, index: FunctionOverloadSetID) -> &mut Self::Output {
        self.items_by_id
            .get_mut(&index.into())
            .unwrap()
            .as_function_overload_set_mut()
            .unwrap()
    }
}

impl Index<EnumID> for Table {
    type Output = Enum;

    fn index(&self, index: EnumID) -> &Self::Output {
        self.items_by_id[&index.into()].as_enum().unwrap()
    }
}
impl IndexMut<EnumID> for Table {
    fn index_mut(&mut self, index: EnumID) -> &mut Self::Output {
        self.items_by_id
            .get_mut(&index.into())
            .unwrap()
            .as_enum_mut()
            .unwrap()
    }
}

impl Index<EnumVariantID> for Table {
    type Output = EnumVariant;

    fn index(&self, index: EnumVariantID) -> &Self::Output {
        self.items_by_id[&index.into()].as_enum_variant().unwrap()
    }
}
impl IndexMut<EnumVariantID> for Table {
    fn index_mut(&mut self, index: EnumVariantID) -> &mut Self::Output {
        self.items_by_id
            .get_mut(&index.into())
            .unwrap()
            .as_enum_variant_mut()
            .unwrap()
    }
}

impl Index<TypeAliasID> for Table {
    type Output = TypeAlias;

    fn index(&self, index: TypeAliasID) -> &Self::Output {
        self.items_by_id[&index.into()].as_type_alias().unwrap()
    }
}
impl IndexMut<TypeAliasID> for Table {
    fn index_mut(&mut self, index: TypeAliasID) -> &mut Self::Output {
        self.items_by_id
            .get_mut(&index.into())
            .unwrap()
            .as_type_alias_mut()
            .unwrap()
    }
}

impl Index<ID> for Table {
    type Output = Item;

    fn index(&self, index: ID) -> &Self::Output { &self.items_by_id[&index] }
}
impl IndexMut<ID> for Table {
    fn index_mut(&mut self, index: ID) -> &mut Self::Output {
        self.items_by_id.get_mut(&index).unwrap()
    }
}

impl Index<ScopedID> for Table {
    type Output = Item;

    fn index(&self, index: ScopedID) -> &Self::Output { &self.items_by_id[&index.into()] }
}
impl IndexMut<ScopedID> for Table {
    fn index_mut(&mut self, index: ScopedID) -> &mut Self::Output {
        self.items_by_id.get_mut(&index.into()).unwrap()
    }
}

impl Index<TypedID> for Table {
    type Output = Item;

    fn index(&self, index: TypedID) -> &Self::Output { &self.items_by_id[&index.into()] }
}
impl IndexMut<TypedID> for Table {
    fn index_mut(&mut self, index: TypedID) -> &mut Self::Output {
        self.items_by_id.get_mut(&index.into()).unwrap()
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// IMPLEMENTATIONS
////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum SymbolState {
    Unconstructed,
    Constructing,
}

impl Table {
    /// Creates a new empty default table
    #[must_use]
    fn new() -> Self {
        Self {
            items_by_id: HashMap::new(),
            root_ids_by_name: HashMap::new(),
        }
    }

    
    pub fn analyze(target: TargetParsing) -> (Self, Vec<SymbolError>) {
        let mut table = Self::new();
        let mut errors = Vec::new();

        // Used to keep track of circular dependencies between symbols
        let mut symbol_states_by_id = HashMap::new();

        table.generate_modules(&target);
        table.generate_symbols(target, &mut errors, &mut symbol_states_by_id);

        // build the symbol in the symbol_states_by_id one by one
        while !symbol_states_by_id.is_empty() {
            let id = *symbol_states_by_id
                .iter()
                .find(|(_, state)| **state == SymbolState::Unconstructed)
                .unwrap()
                .0;

            table.build_symbol(id, &mut errors, &mut symbol_states_by_id);
        }

        (table, errors)
    }

    fn add_symbol<T: SymbolData>(&mut self, data: T) -> <T as SymbolData>::ID
    where
        <T as SymbolData>::ID: Into<ID>,
        SymbolWithData<T>: Into<Item>,
    {
        let id = T::ID::fresh();
        let symbol = SymbolWithData { data, id };

        self.items_by_id.insert(id.into(), symbol.into());

        id
    }

    /// Generates the module hierarchy for the given target
    fn generate_modules(&mut self, target: &TargetParsing) {
        // iteration is sorted by the depth of the module hierarchy
        for file in target.file_parsings() {
            let parent = if file.source_file().module_hierarchy().len() == 1 {
                None
            } else {
                // the parent module name is the full name of the target without the last part
                Some(
                    self.get_id_by_full_name(
                        file.source_file().module_hierarchy()
                            [..file.source_file().module_hierarchy().len() - 1]
                            .iter()
                            .map(std::string::String::as_str),
                    )
                    .unwrap()
                    .into_module()
                    .unwrap(),
                )
            };

            let module_data = ModuleData {
                parent,
                qualified_name: file.source_file().module_hierarchy().clone(),
                children_ids_by_name: HashMap::new(),
                accessibility: file
                    .access_modifier()
                    .as_ref()
                    .map_or(Accessibility::Public, Accessibility::from_syntax_tree),
            };

            let id = self.add_symbol(module_data);

            // add the module to the parent module
            if let Some(parent) = parent {
                self[parent].children_ids_by_name.insert(
                    file.source_file()
                        .module_hierarchy()
                        .last()
                        .unwrap()
                        .clone(),
                    id.into(),
                );
            } else {
                // add to the root
                self.root_ids_by_name.insert(
                    file.source_file()
                        .module_hierarchy()
                        .last()
                        .unwrap()
                        .clone(),
                    id,
                );
            }
        }
    }

    fn generate_symbols(
        &mut self,
        file_parsing: TargetParsing,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) {
        let files = file_parsing
            .dissolve()
            .into_iter()
            .map(pernixc_syntax::target_parsing::FileParsing::dissolve)
            .collect::<Vec<_>>();

        for (source_file, file_syntax_tree, _) in files {
            for item in file_syntax_tree.dissolve() {
                // the parent module id
                let module_id = self
                    .get_id_by_full_name(
                        source_file
                            .module_hierarchy()
                            .iter()
                            .map(std::string::String::as_str),
                    )
                    .unwrap()
                    .into_module()
                    .unwrap();

                let identifier = {
                    let (is_function, identifier) = match &item {
                        ItemSyntaxTree::Struct(sym) => (false, sym.signature().identifier()),
                        ItemSyntaxTree::Enum(sym) => (false, sym.signature().identifier()),
                        ItemSyntaxTree::Function(sym) => (true, sym.signature().identifier()),
                        ItemSyntaxTree::Module(..) => continue,
                        ItemSyntaxTree::TypeAlias(sym) => (false, sym.identifier()),
                    };

                    // check for symbol redefinition
                    if let Some(available_symbol_id) = self.get_id_by_full_name(
                        source_file
                            .module_hierarchy()
                            .iter()
                            .map(std::string::String::as_str)
                            .chain(std::iter::once(identifier.span().str())),
                    ) {
                        // if the current item is function, the available item should be function
                        // overload set (function overloadings are allowed).
                        let is_redefinition = if is_function {
                            available_symbol_id.as_function_overload_set().is_none()
                        } else {
                            true
                        };

                        if is_redefinition {
                            errors.push(
                                SymbolRedifinition {
                                    available_symbol_id,
                                    span: identifier.span().clone(),
                                }
                                .into(),
                            );
                            continue;
                        }
                    }

                    identifier.span().str().to_owned()
                };

                // construct the symbol and add it to the symbol table
                let id: ID = match item {
                    ItemSyntaxTree::Struct(syntax_tree) => self
                        .generate_struct(syntax_tree, module_id, errors, symbol_states_by_id)
                        .into(),
                    ItemSyntaxTree::Enum(syntax_tree) => self
                        .add_symbol(self.create_enum_data(syntax_tree, module_id))
                        .into(),
                    ItemSyntaxTree::TypeAlias(syntax_tree) => self
                        .add_symbol(self.create_type_alias_data(syntax_tree, module_id))
                        .into(),
                    ItemSyntaxTree::Function(syntax_tree) => self
                        .generate_function_overload_set(syntax_tree, module_id, errors)
                        .into(),
                    ItemSyntaxTree::Module(_) => continue,
                };

                // add to the parent module
                self[module_id].children_ids_by_name.insert(identifier, id);

                symbol_states_by_id.insert(id, SymbolState::Unconstructed);
            }
        }
    }

    fn generate_enum(
        &mut self,
        syntax_tree: EnumSyntaxTree,
        parent: ModuleID,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) {
        let (signature, body) = syntax_tree.dissolve();
        let enum_id = EnumID::fresh();
        let data = EnumData {
            qualified_name: self[parent]
                .qualified_name()
                .iter()
                .map(std::borrow::ToOwned::to_owned)
                .chain(std::iter::once(
                    signature.identifier().span().str().to_owned(),
                ))
                .collect(),
            variant_ids_by_name: HashMap::new(), // To be filled later
            accessibility: Accessibility::from_syntax_tree(signature.access_modifier()),
            parent,
            signature_syntax_tree: signature,
        };

        for (index, variant) in body
            .dissolve()
            .1
            .into_iter()
            .flat_map(|x| x.into_elements())
            .enumerate()
        {
            let variant_id = EnumVariantID::fresh();
            let variant_data = EnumVariantData {
                qualified_name: self[parent]
                    .qualified_name()
                    .iter()
                    .map(std::borrow::ToOwned::to_owned)
                    .chain(std::iter::once(
                        signature.identifier().span().str().to_owned(),
                    ))
                    .chain(std::iter::once(
                        variant.identifier().span().str().to_owned(),
                    ))
                    .collect(),
                accessibility: Accessibility::from_syntax_tree(signature.access_modifier()),
                parent: enum_id,
                signature_syntax_tree: variant,
                index,
            };
        }

        // add to the symbol table
        self.items_by_id.insert(
            enum_id.into(),
            SymbolWithData {
                data,
                id: enum_id.into(),
            }
            .into(),
        );
        self[parent].children_ids_by_name.insert(
            signature.identifier().span().str().to_owned(),
            enum_id.into(),
        );
    }

    fn generate_struct(
        &mut self,
        syntax_tree: StructSyntaxTree,
        parent: ModuleID,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) -> StructID {
        let (signature, body) = syntax_tree.dissolve();

        let struct_id = StructID::fresh();
        let mut data = StructData {
            qualified_name: self[parent]
                .qualified_name()
                .iter()
                .map(std::borrow::ToOwned::to_owned)
                .chain(std::iter::once(
                    signature.identifier().span().str().to_owned(),
                ))
                .collect(),
            fields: Vec::new(),                            // To be filled later
            type_alias_member_ids_by_name: HashMap::new(), // To be filled later
            accessibility: Accessibility::from_syntax_tree(signature.access_modifier()),
            parent,
            signature_syntax_tree: signature,
        };

        for member in body.dissolve().1 {
            match member {
                // handle field member
                MemberSyntaxTree::Field(field) => {
                    let field = Field {
                        name: field.identifier().span().str().to_owned(),
                        accessibility: Accessibility::from_syntax_tree(field.access_modifier()),
                        ty: PrimitiveType::Void.into(),
                        syntax_tree: field,
                    };

                    if let Some(available_field_index) = data
                        .fields
                        .iter()
                        .enumerate()
                        .filter_map(|(index, available_field)| {
                            if available_field.name == field.name {
                                Some(index)
                            } else {
                                None
                            }
                        })
                        .next()
                    {
                        errors.push(
                            FieldRedefinition {
                                span: field.syntax_tree.identifier().span().clone(),
                                struct_id,
                                available_field_index,
                            }
                            .into(),
                        );
                    } else {
                        data.fields.push(field);
                    }
                }
                // handle type alias member
                MemberSyntaxTree::TypeAlias(type_alias) => {
                    let type_alias_id = TypeAliasID::fresh();

                    if let Some(available_symbol_id) = data
                        .type_alias_member_ids_by_name
                        .get(type_alias.identifier().span.str())
                        .copied()
                    {
                        errors.push(
                            SymbolRedifinition {
                                span: type_alias.identifier().span.clone(),
                                available_symbol_id: available_symbol_id.into(),
                            }
                            .into(),
                        );
                    } else {
                        self.items_by_id.insert(
                            type_alias_id.into(),
                            SymbolWithData {
                                data: TypeAliasData {
                                    qualified_name: data
                                        .qualified_name()
                                        .iter()
                                        .cloned()
                                        .chain(std::iter::once(
                                            type_alias.identifier().span.str().to_owned(),
                                        ))
                                        .collect(),
                                    accessibility: Accessibility::from_syntax_tree(
                                        type_alias.access_modifier(),
                                    ),
                                    parent: struct_id.into(),
                                    syntax_tree: type_alias,
                                    ty: PrimitiveType::Void.into(), // To be filled later
                                },
                                id: type_alias_id.into(),
                            }
                            .into(),
                        );

                        // add to the parent struct
                        data.type_alias_member_ids_by_name
                            .insert(type_alias.identifier().span.str().to_owned(), type_alias_id);

                        // add to the symbol states
                        symbol_states_by_id
                            .insert(type_alias_id.into(), SymbolState::Unconstructed);
                    }
                }
            }
        }

        self.items_by_id.insert(
            struct_id.into(),
            SymbolWithData {
                id: struct_id,
                data,
            }
            .into(),
        );
        struct_id
    }

    fn generate_function_overload_set(
        &mut self,
        syntax_tree: FunctionSyntaxTree,
        parent: ModuleID,
        errors: &mut Vec<SymbolError>,
    ) -> FunctionOverloadSetID {
        #[allow(clippy::option_if_let_else)]
        let function_overload_set_id = if let Some(id) = self[parent]
            .children_ids_by_name
            .get(syntax_tree.signature().identifier().span().str())
        {
            id.into_function_overload_set().unwrap()
        } else {
            self.add_symbol(FunctionOverloadSetData {
                qualified_name: self[parent]
                    .qualified_name()
                    .iter()
                    .map(std::borrow::ToOwned::to_owned)
                    .chain(std::iter::once(
                        syntax_tree.signature().identifier().span().str().to_owned(),
                    ))
                    .collect(),
                parent,
                overloads_by_id: HashMap::new(),
            })
        };

        let overload_id = OverloadID::fresh();

        let (function_signature, body) = syntax_tree.dissolve();
        let (
            access_modifier,
            keyword,
            identifier,
            left_paren,
            parameter_list,
            right_paren,
            type_annotation,
        ) = function_signature.dissolve();

        let parameters = {
            let mut parameters: Vec<Parameter> = Vec::new();

            // creates a parameter from a parameter syntax tree
            for parameter in parameter_list.into_iter().flat_map(|x| x.into_elements()) {
                // redefinition
                if let Some(available_parameter_index) = parameters
                    .iter()
                    .enumerate()
                    .filter_map(|(idx, x)| {
                        if x.name == parameter.identifier().span.str() {
                            Some(idx)
                        } else {
                            None
                        }
                    })
                    .next()
                {
                    errors.push(
                        ParameterRedefinition {
                            span: parameter.identifier().span.clone(),
                            available_parameter_index,
                            function_overload_set_id,
                            overload_id,
                        }
                        .into(),
                    );
                } else {
                    // add the parameter
                    parameters.push(Parameter {
                        name: parameter.identifier().span.str().to_owned(),
                        type_binding: TypeBinding {
                            ty: PrimitiveType::Void.into(),
                            is_mutable: false,
                        },
                        syntax_tree: parameter,
                    });
                }
            }

            parameters
        };

        self[function_overload_set_id]
            .overloads_by_id
            .insert(overload_id, SymbolWithData {
                data: OverloadData {
                    function_overload_set_id,
                    return_type: PrimitiveType::Void.into(),
                    parameters,
                    identifier_token: identifier,
                    return_type_syntax_tree: type_annotation.dissolve().1,
                    body_syntax_tree: body,
                    accessibility: Accessibility::from_syntax_tree(&access_modifier),
                },
                id: overload_id,
            });

        function_overload_set_id
    }

    fn create_type_alias_data(
        &self,
        syntax_tree: TypeAliasSyntaxTree,
        parent: ModuleID,
    ) -> TypeAliasData {
        TypeAliasData {
            qualified_name: self[parent]
                .qualified_name()
                .iter()
                .map(std::borrow::ToOwned::to_owned)
                .chain(std::iter::once(
                    syntax_tree
                        .type_without_access_modifier()
                        .identifier()
                        .span()
                        .str()
                        .to_owned(),
                ))
                .collect(),
            ty: PrimitiveType::Void.into(), // Use void as a placeholder, to be filled later
            accessibility: Accessibility::from_syntax_tree(syntax_tree.access_modifier()),
            parent: parent.into(),
            syntax_tree: syntax_tree.deconstruct().1,
        }
    }

    fn create_enum_data(&self, syntax_tree: EnumSyntaxTree, parent: ModuleID) -> EnumData {}

    fn create_struct_data(&self, syntax_tree: StructSyntaxTree, parent: ModuleID) -> StructData {}

    fn build_symbol(
        &mut self,
        id: ID,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) {
        match id {
            ID::Struct(id) => self.build_struct_symbol(id, errors, symbol_states_by_id),
            ID::Enum(id) => self.build_enum_symbol(id, errors, symbol_states_by_id),
            ID::FunctionOverloadSet(id) => {
                self.build_function_overload_set_symbol(id, errors, symbol_states_by_id);
            }
            ID::TypeAlias(id) => self.build_type_alias_symbol(id, errors, symbol_states_by_id),

            ID::EnumVariant(..) | ID::Module(..) => unreachable!(),
        }
    }

    fn build_function_overload_set_symbol(
        &mut self,
        function_overload_set_id: FunctionOverloadSetID,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) {
        // mark as constructed, remove from symbol states
        symbol_states_by_id.remove(&function_overload_set_id.into());

        let mut constructed_overload_ids = Vec::new();
        let parent_id = self[function_overload_set_id].parent;
        let overload_ids = self[function_overload_set_id]
            .overloads_by_id
            .keys()
            .copied()
            .collect::<Vec<_>>();

        for overload_id in overload_ids {
            let return_type = {
                let ty_syntax = self[function_overload_set_id].overloads_by_id[&overload_id]
                    .syntax_tree
                    .type_annotation()
                    .type_specifier()
                    .clone();
                let return_type = self
                    .resolve_type(parent_id.into(), &ty_syntax, errors, symbol_states_by_id)
                    .unwrap_or(Type::PrimitiveType(PrimitiveType::Void));

                let ty_accessibility = self.get_overall_accessibility(return_type);

                // the return type accessibility can't be more restrictive than the function's
                if ty_accessibility.restrictiveness_rank()
                    > self[parent_id].accessibility.restrictiveness_rank()
                {
                    errors.push(
                        PrivateSymbolLeak {
                            span: self[function_overload_set_id].overloads_by_id[&overload_id]
                                .syntax_tree
                                .type_annotation()
                                .type_specifier()
                                .span(),
                            accessibility: ty_accessibility,
                            parent_accessibility: self[parent_id].accessibility,
                        }
                        .into(),
                    );
                }

                return_type
            };

            let parameters = {
                let syntax = self[function_overload_set_id].overloads_by_id[&overload_id]
                    .syntax_tree
                    .parameters()
                    .clone();

                self.create_function_parameters(
                    function_overload_set_id,
                    overload_id,
                    &syntax,
                    errors,
                    symbol_states_by_id,
                )
            };

            // check for overload redefinition
            let mut redefinition = false;
            'check: for constructed_overload in constructed_overload_ids
                .iter()
                .map(|x| &self[function_overload_set_id].overloads_by_id[x])
            {
                if constructed_overload.parameters.len() != parameters.len() {
                    continue;
                }

                // if found any parameter pair that is not equal, then this is not a redefinition
                for (parameter, constructed_parameter) in parameters
                    .iter()
                    .zip(constructed_overload.parameters.iter())
                {
                    if parameter.type_binding().ty != constructed_parameter.type_binding().ty {
                        continue 'check;
                    }
                }

                errors.push(
                    OverloadRedefinition {
                        span: self[function_overload_set_id].overloads_by_id[&overload_id]
                            .syntax_tree
                            .identifier()
                            .span()
                            .clone(),
                        function_overload_set_id,
                        available_overload_id: constructed_overload.id,
                    }
                    .into(),
                );

                redefinition = true;
                break;
            }

            if redefinition {
                self[function_overload_set_id]
                    .overloads_by_id
                    .remove(&overload_id);
            } else {
                let overload = self[function_overload_set_id]
                    .overloads_by_id
                    .get_mut(&overload_id)
                    .unwrap();

                overload.parameters = parameters;
                overload.return_type = return_type;

                // add to constructed list
                constructed_overload_ids.push(overload_id);
            }
        }
    }

    fn create_function_parameters(
        &mut self,
        function_overload_set_id: FunctionOverloadSetID,
        overload_id: OverloadID,
        syntax: &Option<ParameterList>,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) -> Vec<Parameter> {
        let parent_id = self[function_overload_set_id].parent;
        let mut parameters: Vec<Parameter> = Vec::new();

        for parameter_syntax in syntax
            .iter()
            .flat_map(pernixc_syntax::syntax_tree::ConnectedList::elements)
        {
            let ty = self
                .resolve_type(
                    parent_id.into(),
                    parameter_syntax.type_annotation().type_specifier(),
                    errors,
                    symbol_states_by_id,
                )
                .unwrap_or(Type::PrimitiveType(PrimitiveType::Void));
            let name = parameter_syntax.identifier().span().str().to_owned();
            let parameter_data = Parameter {
                name: name.clone(),
                type_binding: TypeBinding {
                    ty,
                    is_mutable: parameter_syntax.mutable_keyword().is_some(),
                },
            };

            // check fo the redefinition
            if let Some(available_parameter_index) =
                parameters
                    .iter()
                    .enumerate()
                    .find_map(|(index, parameter)| {
                        if parameter.name == name {
                            Some(index)
                        } else {
                            None
                        }
                    })
            {
                errors.push(
                    ParameterRedefinition {
                        span: parameter_syntax.identifier().span().clone(),
                        available_parameter_index,
                    }
                    .into(),
                );
                continue;
            }

            parameters.push(parameter_data);

            let ty_accessibility = self.get_overall_accessibility(ty);

            // the parameter ty accessibility can not be more restrictive than the function
            // ty accessibility
            if ty_accessibility.restrictiveness_rank()
                < self[function_overload_set_id].overloads_by_id[&overload_id]
                    .accessibility()
                    .restrictiveness_rank()
            {
                errors.push(
                    PrivateSymbolLeak {
                        span: parameter_syntax.span().clone(),
                        accessibility: ty_accessibility,
                        parent_accessibility: self[function_overload_set_id].overloads_by_id
                            [&overload_id]
                            .accessibility,
                    }
                    .into(),
                );
            }
        }

        parameters
    }

    fn build_enum_symbol(
        &mut self,
        enum_id: EnumID,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) {
        // mark as constructed
        symbol_states_by_id.remove(&enum_id.into());

        let syntax_tree = self[enum_id].syntax_tree.variants().clone();

        for (variant_number, variant) in syntax_tree
            .iter()
            .flat_map(pernixc_syntax::syntax_tree::ConnectedList::elements)
            .enumerate()
        {
            let name = variant.span().str().to_owned();
            if let Some(available_symbol_id) = self[enum_id].variant_ids_by_name.get(&name).copied()
            {
                errors.push(
                    EnumVariantRedefinition {
                        span: variant.span().clone(),
                        available_symbol_id,
                    }
                    .into(),
                );
                continue;
            }

            let data = EnumVariantData {
                parent: enum_id,
                qualified_name: self[enum_id]
                    .qualified_name()
                    .iter()
                    .cloned()
                    .chain(std::iter::once(name.clone()))
                    .collect(),
                variant_number: variant_number as u64,
            };

            let variant_id = self.add_symbol(data);
            self[enum_id].variant_ids_by_name.insert(name, variant_id);
        }
    }

    fn build_type_alias_symbol(
        &mut self,
        type_alias_id: TypeAliasID,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) {
        // mark as constructing
        *symbol_states_by_id
            .get_mut(&type_alias_id.into())
            .expect("should exist") = SymbolState::Constructing;

        let ty = self.resolve_type(
            self[type_alias_id].parent(),
            &self[type_alias_id].syntax_tree.type_specifier().clone(),
            errors,
            symbol_states_by_id,
        );

        self[type_alias_id].ty = ty.unwrap_or(Type::PrimitiveType(PrimitiveType::Void));

        // the type accessibility must not be more restrictive than the type alias accessibility
        let ty_accessibility = self.get_overall_accessibility(self[type_alias_id].ty);
        if ty_accessibility.restrictiveness_rank()
            > self[type_alias_id].accessibility.restrictiveness_rank()
        {
            errors.push(
                PrivateSymbolLeak {
                    span: self[type_alias_id].syntax_tree.type_specifier().span(),
                    accessibility: ty_accessibility,
                    parent_accessibility: self[type_alias_id].accessibility,
                }
                .into(),
            );
        }

        // mark as constructed by removing from symbol states map
        symbol_states_by_id.remove(&type_alias_id.into());
    }

    fn build_struct_symbol(
        &mut self,
        struct_id: StructID,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) {
        // mark as constructed
        symbol_states_by_id.remove(&struct_id.into());

        let syntax_tree = self[struct_id].syntax_tree.member_groups().clone();
        self.build_type_alias_members(struct_id, &syntax_tree, errors, symbol_states_by_id);
        self.build_struct_field_members(struct_id, &syntax_tree, errors, symbol_states_by_id);
    }

    fn build_struct_field_members(
        &mut self,
        struct_id: StructID,
        syntax_tree: &[MemberGroup],
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) {
        for member_group in syntax_tree {
            let member_accessibility =
                Accessibility::from_syntax_tree(member_group.access_modifier());

            for member in member_group.members() {
                let MemberSyntaxTree::Field(field) = member else {
                    continue;
                };

                let ty = self
                    .resolve_type(
                        struct_id.into(),
                        field.type_annotation().type_specifier(),
                        errors,
                        symbol_states_by_id,
                    )
                    .unwrap_or(Type::PrimitiveType(PrimitiveType::Void));

                // the access modifier of the type can't be more restrictive than the field's access
                let ty_accessibility = self.get_overall_accessibility(ty);
                if ty_accessibility.restrictiveness_rank()
                    < member_accessibility.restrictiveness_rank()
                {
                    errors.push(
                        PrivateSymbolLeak {
                            accessibility: ty_accessibility,
                            parent_accessibility: member_accessibility,
                            span: field.type_annotation().type_specifier().span(),
                        }
                        .into(),
                    );
                }

                let field_data = FieldData {
                    name: field.identifier().span().str().to_owned(),
                    accessibility: member_accessibility,
                    ty,
                };

                if let Err(available_id) = self[struct_id]
                    .fields
                    .add(field.identifier().span().str().to_owned(), field_data)
                {
                    errors.push(
                        FieldRedefinition {
                            span: field.identifier().span().clone(),
                            available_id,
                        }
                        .into(),
                    );
                }
            }
        }
    }

    fn build_type_alias_members(
        &mut self,
        struct_id: StructID,
        syntax_tree: &[MemberGroup],
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) {
        for member_group in syntax_tree {
            let accessibility = Accessibility::from_syntax_tree(member_group.access_modifier());
            // the access modifier must not be less restrictive than the struct's access modifier
            if accessibility.restrictiveness_rank()
                < self[struct_id].accessibility.restrictiveness_rank()
            {
                errors.push(
                    StructMemberMoreAccessibleThanStruct {
                        span: member_group.access_modifier().span(),
                        struct_accessibility: self[struct_id].accessibility,
                        member_group_accessibility: accessibility,
                    }
                    .into(),
                );
            }

            for member in member_group.members() {
                let MemberSyntaxTree::TypeWithoutAccessModifier(type_alias) = member else {
                   continue;
                };
                self[struct_id]
                    .type_alias_member_ids_by_name
                    .get(type_alias.identifier().span().str())
                    .copied()
                    .map_or_else(
                        || {
                            let id = self.add_symbol(TypeAliasData {
                                parent: struct_id.into(),
                                qualified_name: self[struct_id]
                                    .qualified_name()
                                    .iter()
                                    .cloned()
                                    .chain(std::iter::once(
                                        type_alias.identifier().span().str().to_owned(),
                                    ))
                                    .collect(),
                                accessibility,
                                syntax_tree: type_alias.clone(),
                                ty: PrimitiveType::Void.into(), // to be filled later
                            });

                            self[struct_id]
                                .type_alias_member_ids_by_name
                                .insert(type_alias.identifier().span().str().to_owned(), id);

                            symbol_states_by_id.insert(id.into(), SymbolState::Unconstructed);
                        },
                        |available_symbol_id| {
                            errors.push(
                                SymbolRedifinition {
                                    span: type_alias.identifier().span().clone(),
                                    available_symbol_id: available_symbol_id.into(),
                                }
                                .into(),
                            );
                        },
                    );
            }
        }
    }

    fn check_symbol_requirement(
        &mut self,
        id: ID,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) -> bool {
        match symbol_states_by_id.get(&id) {
            // constructs the symbol if it is not constructed
            Some(SymbolState::Unconstructed) => {
                self.build_symbol(id, errors, symbol_states_by_id);
                true
            }

            // circular dependency
            Some(SymbolState::Constructing) => {
                let symbol_ids = symbol_states_by_id
                    .iter()
                    .filter(|(_, state)| **state == SymbolState::Constructing)
                    .map(|(id, _)| id)
                    .copied()
                    .collect::<Vec<_>>();

                // remove all the symbol state from the map
                for id in &symbol_ids {
                    symbol_states_by_id.remove(id);
                }

                errors.push(CircularDependency { symbol_ids }.into());
                false
            }
            None => true,
        }
    }

    fn search_child(
        &mut self,
        parent: ID,
        name: &str,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) -> Option<ID> {
        if !self.check_symbol_requirement(parent, errors, symbol_states_by_id) {
            return None;
        }

        match parent {
            ID::Struct(sym) => self[sym]
                .type_alias_member_ids_by_name()
                .get(name)
                .copied()
                .map(std::convert::Into::into),
            ID::Enum(sym) => self[sym]
                .variant_ids_by_name()
                .get(name)
                .copied()
                .map(std::convert::Into::into),
            ID::Module(sym) => self[sym]
                .children_ids_by_name
                .get(name)
                .copied()
                .map(std::convert::Into::into),
            ID::FunctionOverloadSet(..) | ID::EnumVariant(..) => None,
            ID::TypeAlias(sym) => match self[sym].ty {
                Type::PrimitiveType(..) => None,
                Type::TypedID(type_id) => {
                    self.search_child(type_id.into(), name, errors, symbol_states_by_id)
                }
            },
        }
    }

    fn find_nearest_parent_module(&self, mut symbol: ID) -> ModuleID {
        loop {
            if let ID::Module(symbol) = symbol {
                return symbol;
            }

            symbol = self[symbol].parent().unwrap().into();
        }
    }

    fn symbol_accessible(
        &self,
        referring_site: ScopedID,
        symbol: ID,
        accessibility: Accessibility,
    ) -> bool {
        match accessibility {
            Accessibility::Internal => {
                self[referring_site].qualified_name().first()
                    == self[symbol].qualified_name().first()
            }
            Accessibility::Private => {
                // find the nearest parent module of both the referring site and the symbol and
                // check if the parent module of the referring site is the same or a child of the
                // parent module of the symbol
                let sym_parent = self.find_nearest_parent_module(symbol);
                let mut current_module = referring_site;
                loop {
                    match current_module {
                        ScopedID::Module(module) if module == sym_parent => {
                            break true;
                        }
                        _ => {
                            if let Some(parent) = self[current_module].parent() {
                                current_module = parent;
                            } else {
                                break false;
                            }
                        }
                    }
                }
            }
            Accessibility::Public => true,
        }
    }

    fn resolve_type(
        &mut self,
        referring_site: ScopedID,
        type_specifier: &TypeSpecifier,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) -> Option<Type> {
        match type_specifier {
            TypeSpecifier::PrimitiveTypeSpecifier(primitive_type) => match primitive_type {
                PrimitiveTypeSpecifier::Bool(..) => Some(Type::PrimitiveType(PrimitiveType::Bool)),
                PrimitiveTypeSpecifier::Int8(..) => Some(Type::PrimitiveType(PrimitiveType::Int8)),
                PrimitiveTypeSpecifier::Int16(..) => {
                    Some(Type::PrimitiveType(PrimitiveType::Int16))
                }
                PrimitiveTypeSpecifier::Int32(..) => {
                    Some(Type::PrimitiveType(PrimitiveType::Int32))
                }
                PrimitiveTypeSpecifier::Int64(..) => {
                    Some(Type::PrimitiveType(PrimitiveType::Int64))
                }
                PrimitiveTypeSpecifier::Uint8(..) => {
                    Some(Type::PrimitiveType(PrimitiveType::Uint8))
                }
                PrimitiveTypeSpecifier::Uint16(..) => {
                    Some(Type::PrimitiveType(PrimitiveType::Uint16))
                }
                PrimitiveTypeSpecifier::Uint32(..) => {
                    Some(Type::PrimitiveType(PrimitiveType::Uint32))
                }
                PrimitiveTypeSpecifier::Uint64(..) => {
                    Some(Type::PrimitiveType(PrimitiveType::Uint64))
                }
                PrimitiveTypeSpecifier::Float32(..) => {
                    Some(Type::PrimitiveType(PrimitiveType::Float32))
                }
                PrimitiveTypeSpecifier::Float64(..) => {
                    Some(Type::PrimitiveType(PrimitiveType::Float64))
                }
                PrimitiveTypeSpecifier::Void(..) => Some(Type::PrimitiveType(PrimitiveType::Void)),
            },
            TypeSpecifier::QualifiedIdentifier(qualified_identifier) => {
                let symbol_id = self.resolve_symbol(
                    referring_site,
                    qualified_identifier,
                    errors,
                    symbol_states_by_id,
                )?;

                match symbol_id {
                    ID::Struct(sym) => Some(Type::TypedID(sym.into())),
                    ID::Enum(sym) => Some(Type::TypedID(sym.into())),
                    ID::TypeAlias(sym) => Some(self[sym].ty),
                    ID::Module(..) | ID::FunctionOverloadSet(..) | ID::EnumVariant(..) => {
                        errors.push(
                            TypeExpected {
                                span: qualified_identifier.span(),
                                found: symbol_id,
                            }
                            .into(),
                        );
                        None
                    }
                }
            }
        }
    }

    fn get_overall_accessibility(&mut self, ty: Type) -> Accessibility {
        match ty {
            Type::PrimitiveType(..) => Accessibility::Public,
            Type::TypedID(ty) => self[ty].accessibility().unwrap_or(Accessibility::Public),
        }
    }

    /// Retrieves the symbol ID of the given qualified identifier
    fn resolve_symbol(
        &mut self,
        referring_site: ScopedID,
        qualified_identifier: &QualifiedIdentifier,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) -> Option<ID> {
        // at the referring site itself, it must be fully constructed
        assert!(!symbol_states_by_id.contains_key(&referring_site.into()));

        // find the starting point of the name resolution
        let mut current_id = if qualified_identifier.leading_separator().is_some() {
            // start from root
            if let Some(id) = self
                .root_ids_by_name
                .get(qualified_identifier.identifiers().first().span().str())
                .copied()
            {
                id.into()
            } else {
                errors.push(
                    SymbolNotFound {
                        span: qualified_identifier.identifiers().first().span().clone(),
                        in_scope_id: None,
                    }
                    .into(),
                );

                return None;
            }
        } else {
            // search starting from the referring site and going up the parent chain. the first
            // scope that contains the symbol is the starting point
            let mut in_scope_id: ID = referring_site.into();
            loop {
                let result = self.search_child(
                    in_scope_id,
                    qualified_identifier.identifiers().first().span().str(),
                    errors,
                    symbol_states_by_id,
                );

                if let Some(result) = result {
                    break result;
                }

                // if the parent scope doesn't contain the symbol, go up the parent chain
                // if returns None, then the symbol is not found
                in_scope_id = if let Some(id) = self[in_scope_id].parent() {
                    id.into()
                } else {
                    errors.push(
                        SymbolNotFound {
                            span: qualified_identifier.identifiers().first().span().clone(),
                            in_scope_id: None,
                        }
                        .into(),
                    );
                    return None;
                };
            }
        };

        // the symbol must be built
        self.check_symbol_requirement(current_id, errors, symbol_states_by_id);

        // search the symbol for the rest of the identifiers
        for identifier in qualified_identifier.identifiers().rest() {
            if let Some(result) = self.search_child(
                current_id,
                identifier.1.span().str(),
                errors,
                symbol_states_by_id,
            ) {
                // check if the symbol is accessible
                if let Some(accessibility) = self[current_id].accessibility() {
                    if !self.symbol_accessible(referring_site, result, accessibility) {
                        errors.push(
                            SymbolNotAccessible {
                                span: identifier.1.span().clone(),
                                symbol_id: result,
                            }
                            .into(),
                        );
                    }
                }

                current_id = result;
            } else {
                errors.push(
                    SymbolNotFound {
                        span: identifier.1.span().clone(),
                        in_scope_id: Some(current_id),
                    }
                    .into(),
                );
                return None;
            }
        }

        self.check_symbol_requirement(current_id, errors, symbol_states_by_id);

        Some(current_id)
    }
}

#[cfg(test)]
mod tests;
