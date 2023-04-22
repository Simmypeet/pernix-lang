//! Contains all the definitions of the item symbols and the table.

use std::{
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    ops::{Index, IndexMut},
};

use derive_more::{Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_syntax::{
    syntax_tree::item::{
        Enum as EnumSyntaxTree, Function as FunctionSyntaxTree, Struct as StructSyntaxTree,
        TypeAlias as TypeAliasSyntaxTree,
    },
    target_parsing::TargetParsing,
};

use self::builder::Builder;
use super::{
    errors::SymbolError,
    ty::{Type, TypeBinding},
    Uid, UniqueIdentifier,
};

mod builder;

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
#[allow(missing_docs)]
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

/// Is an enumeration of all the identifiers that can be accessed in the global scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ID {
    Struct(StructID),
    Enum(EnumID),
    Module(ModuleID),
    FunctionOverloadSet(FunctionOverloadSetID),
    EnumVariant(EnumVariantID),
    TypeAlias(TypeAliasID),
    TypeAliasMemberID(TypeAliasMemberID),
}

/// Is an unique identifier used to identify a field in the [`StructData`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldID(Uid);

impl UniqueIdentifier for FieldID {
    fn fresh() -> Self { Self(Uid::fresh()) }
}

/// Is an unique identifier used to identify a type alias member in the [`StructData`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeAliasMemberID(Uid);

impl UniqueIdentifier for TypeAliasMemberID {
    fn fresh() -> Self { Self(Uid::fresh()) }
}

/// Is an enumeration of all the access modifiers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AccessModifier {
    /// The symbol can be accessed from the same target.
    Internal,

    /// The symbol can be accessed if the reference site shares the same module ancestry.
    Private,

    /// The symbol can be accessed from any where.
    Public,
}

impl AccessModifier {
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
}

/// Represents a field data of a struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters, CopyGetters)]
pub struct FieldData {
    /// The name of the field.
    #[get = "pub"]
    name: String,

    /// The access modifier of the field.
    #[get_copy = "pub"]
    access_modifier: AccessModifier,

    /// The type of the field.
    #[get = "pub"]
    ty: Type,
}

impl SymbolData for FieldData {
    type ID = FieldID;
}

/// Represents a type alias member.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, CopyGetters)]
pub struct TypeAliasMemberData {
    /// The type that is aliased by the type alias.
    #[get_copy = "pub"]
    ty: Type,
}

impl SymbolData for TypeAliasMemberData {
    type ID = TypeAliasMemberID;
}

/// Represents a type alias member symbol.
pub type TypeAliasMember = SymbolWithData<TypeAliasMemberData>;

/// Is a data structure that allows mapping between name, ID and data.
#[derive(Debug, Clone)]
pub struct IDMap<T: SymbolData> {
    /// Maps the ID to the data.
    values_by_id: HashMap<T::ID, SymbolWithData<T>>,

    /// Maps the name to the ID.
    ids_by_name: HashMap<String, T::ID>,
}

impl<T: SymbolData> IDMap<T> {
    /// Creates a new empty [`IDMap`].
    #[must_use]
    pub fn new() -> Self {
        Self {
            values_by_id: HashMap::new(),
            ids_by_name: HashMap::new(),
        }
    }
}

impl<T: SymbolData> Default for IDMap<T> {
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
    field_member_map: IDMap<FieldData>,

    /// The type alias members of the struct.
    #[get = "pub"]
    type_alias_member_ids_by_name: HashMap<String, TypeAliasMemberID>,

    /// The access modifier of the struct.
    #[get_copy = "pub"]
    access_modifier: AccessModifier,

    /// Is the ID of the module that contains the struct.
    #[get_copy = "pub"]
    parent: ModuleID,

    /// The syntax tree that was used to create the struct.
    #[get = "pub"]
    syntax_tree: StructSyntaxTree,
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
    access_modifier: AccessModifier,

    /// Is the ID of the module that contains the enum.
    #[get_copy = "pub"]
    parent: ModuleID,

    /// The syntax tree that was used to create the enum.
    #[get = "pub"]
    syntax_tree: EnumSyntaxTree,
}

impl SymbolData for EnumData {
    type ID = EnumID;
}

/// Represents an enum symbol.
pub type Enum = SymbolWithData<EnumData>;

/// Is a trait that all the data of a symbol must implement.
pub trait SymbolData {
    /// The type of the ID that can be used to identify the symbol.
    type ID: UniqueIdentifier;
}

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
    access_modifier: AccessModifier,
}

impl SymbolData for ModuleData {
    type ID = ModuleID;
}

/// Represents a module symbol.
pub type Module = SymbolWithData<ModuleData>;

/// Is an unique identifier used to identify a parameter in the [`OverloadData`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParameterID(Uid);

impl UniqueIdentifier for ParameterID {
    fn fresh() -> Self { Self(Uid::fresh()) }
}

/// Represents a parameter data.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters, CopyGetters)]
pub struct ParameterData {
    /// The name of the parameter
    #[get = "pub"]
    name: String,

    /// The type binding of this parameter.
    #[get_copy = "pub"]
    type_binding: TypeBinding,
}

impl SymbolData for ParameterData {
    type ID = ParameterID;
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
    overload_set_id: FunctionOverloadSetID,

    /// The return type of the function overload.
    #[get = "pub"]
    return_type: Type,

    /// The parameters of the function overload.
    #[get = "pub"]
    parameters: IDMap<ParameterData>,

    /// The syntax tree that was used to create the function overload.
    #[get = "pub"]
    syntax_tree: FunctionSyntaxTree,
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumVariantData {
    /// The parent enum of the variant.
    parent: EnumID,

    /// The fully qualified name of the variant.
    qualified_name: Vec<String>,

    /// The order of declaration of the variant.
    variant_number: u64,
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
    access_modifier: AccessModifier,

    /// Is the ID of the module that contains the type alias.
    #[get_copy = "pub"]
    parent: ModuleID,

    /// The syntax tree that was used to create the type alias.
    #[get = "pub"]
    syntax_tree: TypeAliasSyntaxTree,
}

impl SymbolData for TypeAliasData {
    type ID = TypeAliasID;
}

/// Represents a type alias symbol.
pub type TypeAlias = SymbolWithData<TypeAliasData>;

/// The struct contains the data of the symbol and its ID.
#[derive(
    Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, DerefMut, CopyGetters,
)]
pub struct SymbolWithData<T: SymbolData> {
    #[deref]
    #[deref_mut]
    data: T,

    /// The ID of the symbol.
    #[get_copy = "pub"]
    id: T::ID,
}

/// Is an enumeration of all symbols that can be accessed in the global scope.
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Item {
    Module(Module),
    Struct(Struct),
    FunctionOverloadSet(FunctionOverloadSet),
    TypeAlias(TypeAlias),
    Enum(Enum),
    EnumVariant(EnumVariant),
    TypeAliasMember(TypeAliasMember),
}

/// Represents a symbol table.
#[derive(Debug, Clone, Getters, CopyGetters)]
pub struct Table {
    items_by_id: HashMap<ID, Item>,

    root_ids_by_name: HashMap<String, ModuleID>,
}

impl Table {
    /// Analyzes the given target syntax tree and creates the symbol table.
    ///
    /// Returns the symbol table and a list of errors that occurred during the analysis.
    #[must_use]
    pub fn analyze(target: TargetParsing) -> (Self, Vec<SymbolError>) {
        let mut builder = Builder::new();

        builder.generate_modules(&target);
        builder.generate_symbols(target);

        todo!()
    }

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

impl Index<TypeAliasMemberID> for Table {
    type Output = TypeAliasMember;

    fn index(&self, index: TypeAliasMemberID) -> &Self::Output {
        self.items_by_id[&index.into()]
            .as_type_alias_member()
            .unwrap()
    }
}
impl IndexMut<TypeAliasMemberID> for Table {
    fn index_mut(&mut self, index: TypeAliasMemberID) -> &mut Self::Output {
        self.items_by_id
            .get_mut(&index.into())
            .unwrap()
            .as_type_alias_member_mut()
            .unwrap()
    }
}
