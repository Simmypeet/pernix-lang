//! Contains all the definitions of the item symbols and the table.

use std::{collections::HashMap, fmt::Debug, hash::Hash};

use derive_more::{Deref, DerefMut};
use getset::{CopyGetters, Getters};

use super::{ty::Type, Uid};

/// Is an unique identifier used to identify a struct in the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructID(Uid);

/// Is an unique identifier used to identify an enum in the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumID(Uid);

/// Is an unique identifier used to identify a module in the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleID(Uid);

/// Is an unique identifier used to identify a function overload set in the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionOverloadSetID(Uid);

/// Is an unique identifier used to identify an enum variant in the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumVariantID(Uid);

/// Is an enumeration of all the identifiers of the symbol that can be used as a type in
/// the program.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum TypedID {
    Struct(StructID),
    Enum(EnumID),
}

/// Is an enumeration of all the identifiers that can be accessed in the global scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum ID {
    Struct(StructID),
    Enum(EnumID),
    Module(ModuleID),
    FunctionOverloadSet(FunctionOverloadSetID),
    EnumVariant(EnumVariantID),
}

/// Is an unique identifier used to identify a field in the [`StructData`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldID(Uid);

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

/// Represents a field of a struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters, CopyGetters)]
pub struct Field {
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

/// Represents a struct.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct StructData {
    /// The fully qualified name of the struct.
    #[get = "pub"]
    qualified_name: Vec<String>,

    /// Maps the name of the field to the field id.
    #[get = "pub"]
    field_ids_by_name: HashMap<String, FieldID>,

    /// Maps the field id to the field.
    #[get = "pub"]
    fields: HashMap<FieldID, Field>,

    /// The access modifier of the struct.
    #[get_copy = "pub"]
    access_modifier: AccessModifier,

    /// Is the ID of the module that contains the struct.
    #[get_copy = "pub"]
    parent: ModuleID,
}

/// Is a trait that all the data of a symbol must implement.
pub trait SymbolData {
    /// The type of the ID that can be used to identify the symbol.
    type ID: Debug + Clone + Copy + PartialEq + Eq + PartialOrd + Ord + Hash;
}

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

