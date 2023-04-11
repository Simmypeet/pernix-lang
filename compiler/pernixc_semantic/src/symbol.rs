//! Contains the definition of the [`Table`] and all the symbols that can be defined in
//! it.
//!
//! Since the symbol table will likely involve a lot of graph and tree structure, it is implemented
//! in index-based fashion. This means that all the symbols are stored in a vector and each edge in
//! the graph is represented by an index. This is done to cope with the borrow checker and to avoid
//! having to use `Rc` and `RefCell` everywhere.

use std::{
    any::Any,
    borrow::Borrow,
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::Hash,
    ops::{Index, IndexMut},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use derive_more::{From, Into};
use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::{SourceElement, SourceFile};
use pernixc_lexical::token::Identifier as IdentifierToken;
use pernixc_syntax::{
    file_parsing::FileParsing,
    syntax_tree::{
        self, item::Item as ItemSyntaxTree, File, PrimitiveTypeSpecifier, TypeSpecifier,
    },
};
use thiserror::Error;

use self::{
    private::{SealedSymbol, SymbolBundle},
    ty::{PrimitiveType, Type, TypeBinding},
};
use crate::{
    errors::{
        AccessibilityLeaking, EnumVariantRedefinition, FieldRedefinition, ParameterRedifinition,
        SemanticError, SymbolIsNotAccessible, SymbolNotFound, SymbolRedifinition, TypeExpected,
    },
    OkResult, SemanticResult, SourceSpan,
};

pub mod ty;

/// Is an enumeration of all access modifiers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AccessModifier {
    /// The symbol is accessible from anywhere.
    Public,

    /// The symbol is only accessible within the same module.
    Private,

    /// The symbol is only accessible within the same target.
    Internal,
}

impl AccessModifier {
    /// Gets the rank of the access modifier in terms of restrictions.
    ///
    /// The higher the rank, the more restrictive the access modifier is.
    /// The higher the number, the higher the rank.
    ///
    /// The rank starts at 0.
    #[must_use]
    pub fn rank(self) -> u32 {
        match self {
            Self::Public => 0,
            Self::Private => 1,
            Self::Internal => 2,
        }
    }

    /// Converts [`pernixc_syntax::syntax_tree::item::AccessModifier`] to [`AccessModifier`].
    #[must_use]
    pub fn from_syntax_tree(
        access_modifier_syntax_tree: &syntax_tree::item::AccessModifier,
    ) -> Self {
        match access_modifier_syntax_tree {
            syntax_tree::item::AccessModifier::Public(..) => Self::Public,
            syntax_tree::item::AccessModifier::Private(..) => Self::Private,
            syntax_tree::item::AccessModifier::Internal(..) => Self::Internal,
        }
    }
}

/// Is a trait that all ID structs must implement in order to be able to retrieve symbols from the
/// [`Table`].
///
/// This trait retrieves a reference to the symbol.
pub trait Indexer<'a> {
    /// The symbol type that the ID is associated with.
    type Output: 'a;

    /// Gets the symbol from the symbol table.
    ///
    /// # Panics
    /// Panics if the indexer is an invalid index or indexing to the wrong table.
    fn get(self, symbol_table: &'a Table) -> Self::Output;
}

/// Is a trait that all ID structs must implement in order to be able to retrieve symbols from the
/// [`Table`].
///
/// This trait retrieves a mutable reference to the symbol.
pub trait IndexerMut<'a> {
    /// The symbol type that the ID is associated with.
    type Output: 'a;

    /// Gets the symbol from the symbol table.
    ///
    /// # Panics
    /// Panics if the indexer is an invalid index or indexing to the wrong table.
    fn get_mut(self, symbol_table: &'a mut Table) -> Self::Output;
}

/// Is a structure used to retrieve [`Module`] symbols from the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleID(usize);

impl<'a> Indexer<'a> for ModuleID {
    type Output = &'a Module;

    fn get(self, symbol_table: &'a Table) -> Self::Output {
        symbol_table.symbol_bundle.modules.get(&self).unwrap()
    }
}

impl<'a> IndexerMut<'a> for ModuleID {
    type Output = &'a mut Module;

    fn get_mut(self, symbol_table: &'a mut Table) -> Self::Output {
        symbol_table.symbol_bundle.modules.get_mut(&self).unwrap()
    }
}

/// Is a structure used to retrieve [`Struct`] symbols from the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StructID(usize);

impl<'a> Indexer<'a> for StructID {
    type Output = &'a Struct;

    fn get(self, symbol_table: &'a Table) -> Self::Output {
        symbol_table.symbol_bundle.structs.get(&self).unwrap()
    }
}

impl<'a> IndexerMut<'a> for StructID {
    type Output = &'a mut Struct;

    fn get_mut(self, symbol_table: &'a mut Table) -> Self::Output {
        symbol_table.symbol_bundle.structs.get_mut(&self).unwrap()
    }
}

/// Is a structure used to retrieve [`Enum`] symbols from the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EnumID(usize);

impl<'a> Indexer<'a> for EnumID {
    type Output = &'a Enum;

    fn get(self, symbol_table: &'a Table) -> Self::Output {
        symbol_table.symbol_bundle.enums.get(&self).unwrap()
    }
}

impl<'a> IndexerMut<'a> for EnumID {
    type Output = &'a mut Enum;

    fn get_mut(self, symbol_table: &'a mut Table) -> Self::Output {
        symbol_table.symbol_bundle.enums.get_mut(&self).unwrap()
    }
}

/// Is a structure used to retrieve [`Function`] symbols from the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FunctionID(usize);

impl<'a> Indexer<'a> for FunctionID {
    type Output = &'a Function;

    fn get(self, symbol_table: &'a Table) -> Self::Output {
        symbol_table.symbol_bundle.functions.get(&self).unwrap()
    }
}

impl<'a> IndexerMut<'a> for FunctionID {
    type Output = &'a mut Function;

    fn get_mut(self, symbol_table: &'a mut Table) -> Self::Output {
        symbol_table.symbol_bundle.functions.get_mut(&self).unwrap()
    }
}

/// Is a structure used to retrieve [`EnumVariant`] symbols from the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EnumVariantID(usize);

impl<'a> Indexer<'a> for EnumVariantID {
    type Output = &'a EnumVariant;

    fn get(self, symbol_table: &'a Table) -> Self::Output {
        symbol_table.symbol_bundle.enum_variants.get(&self).unwrap()
    }
}

impl<'a> IndexerMut<'a> for EnumVariantID {
    type Output = &'a mut EnumVariant;

    fn get_mut(self, symbol_table: &'a mut Table) -> Self::Output {
        symbol_table
            .symbol_bundle
            .enum_variants
            .get_mut(&self)
            .unwrap()
    }
}

/// Is an identifier that can be used to retrieve a [`Scoped`] symbol from the
/// [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ScopedID {
    Module(ModuleID),
    Enum(EnumID),
}

impl From<ScopedID> for ID {
    fn from(val: ScopedID) -> Self {
        match val {
            ScopedID::Module(id) => Self::Module(id),
            ScopedID::Enum(id) => Self::Enum(id),
        }
    }
}

/// Is a trait that is implemented by scoped symbols.
///
/// Scoped symbol refers to symbols that can be used as a parent of other symbols. In other words,
/// it introduces a new scope into the namespace.
pub trait Scoped: Symbol {}

impl Scoped for Module {}
impl Scoped for Enum {}

impl<'a> Indexer<'a> for ScopedID {
    type Output = &'a dyn Scoped;

    fn get(self, symbol_table: &'a Table) -> Self::Output {
        match self {
            Self::Module(id) => symbol_table.symbol_bundle.modules.get(&id).unwrap() as _,
            Self::Enum(id) => symbol_table.symbol_bundle.enums.get(&id).unwrap() as _,
        }
    }
}

impl<'a> IndexerMut<'a> for ScopedID {
    type Output = &'a mut dyn Scoped;

    fn get_mut(self, symbol_table: &'a mut Table) -> Self::Output {
        match self {
            Self::Module(id) => symbol_table.symbol_bundle.modules.get_mut(&id).unwrap() as _,
            Self::Enum(id) => symbol_table.symbol_bundle.enums.get_mut(&id).unwrap() as _,
        }
    }
}

/// Is an enumeration of all the symbols that can be defined in the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ID {
    Module(ModuleID),
    Struct(StructID),
    Enum(EnumID),
    Function(FunctionID),
    EnumVariant(EnumVariantID),
}

impl<'a> Indexer<'a> for ID {
    type Output = &'a dyn Symbol;

    fn get(self, symbol_table: &'a Table) -> Self::Output {
        match self {
            Self::Module(id) => symbol_table.symbol_bundle.modules.get(&id).unwrap() as _,
            Self::Struct(id) => symbol_table.symbol_bundle.structs.get(&id).unwrap() as _,
            Self::Enum(id) => symbol_table.symbol_bundle.enums.get(&id).unwrap() as _,
            Self::Function(id) => symbol_table.symbol_bundle.functions.get(&id).unwrap() as _,
            Self::EnumVariant(id) => {
                symbol_table.symbol_bundle.enum_variants.get(&id).unwrap() as _
            }
        }
    }
}

impl<'a> IndexerMut<'a> for ID {
    type Output = &'a mut dyn Symbol;

    fn get_mut(self, symbol_table: &'a mut Table) -> Self::Output {
        match self {
            Self::Module(id) => symbol_table.symbol_bundle.modules.get_mut(&id).unwrap() as _,
            Self::Struct(id) => symbol_table.symbol_bundle.structs.get_mut(&id).unwrap() as _,
            Self::Enum(id) => symbol_table.symbol_bundle.enums.get_mut(&id).unwrap() as _,
            Self::Function(id) => symbol_table.symbol_bundle.functions.get_mut(&id).unwrap() as _,
            Self::EnumVariant(id) => symbol_table
                .symbol_bundle
                .enum_variants
                .get_mut(&id)
                .unwrap() as _,
        }
    }
}

/// Is an identifier that can be used to retrieve a [`Typed`] symbol from the
/// [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum TypedID {
    Struct(StructID),
    Enum(EnumID),
}

/// Is a trait that is implemented by typed symbols.
///
/// Typed symbol refers to symbols that can be used as a type in the type system of the language.
pub trait Typed: Symbol {}
impl Typed for Struct {}
impl Typed for Enum {}

impl<'a> Indexer<'a> for TypedID {
    type Output = &'a dyn Typed;

    fn get(self, symbol_table: &'a Table) -> Self::Output {
        match self {
            Self::Struct(id) => symbol_table.symbol_bundle.structs.get(&id).unwrap() as _,
            Self::Enum(id) => symbol_table.symbol_bundle.enums.get(&id).unwrap() as _,
        }
    }
}

impl<'a> IndexerMut<'a> for TypedID {
    type Output = &'a mut dyn Typed;

    fn get_mut(self, symbol_table: &'a mut Table) -> Self::Output {
        match self {
            Self::Struct(id) => symbol_table.symbol_bundle.structs.get_mut(&id).unwrap() as _,
            Self::Enum(id) => symbol_table.symbol_bundle.enums.get_mut(&id).unwrap() as _,
        }
    }
}

/// Represents a variable symbol.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Parameter {
    /// The name of the variable.
    pub name: String,

    /// The type binding of the variable.
    pub type_binding: TypeBinding,
}

/// Is an identifier that can be used to retrieve a [`Variable`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, From, Into)]
pub struct ParameterID(usize);

/// Is a trait that all symbols must implement.
pub trait Symbol: Any + private::SealedSymbol {
    /// Gets the name of the symbol.
    fn name(&self) -> &str;

    /// Gets the access modifier of the symbol.
    fn access_modifier(&self) -> AccessModifier;

    /// Gets the parent id
    fn parent_id(&self) -> Option<ScopedID>;

    /// Gets the [`Any`] reference to the symbol.
    fn as_any(&self) -> &dyn Any;

    /// Gets the mutable [`Any`] reference to the symbol.
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

/// Is a container that allows access to items by both name and id.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VecNameMap<T, IDKey = usize, SecondaryKey = String>
where
    IDKey: Into<usize> + From<usize> + Copy,
    SecondaryKey: Eq + Hash,
{
    vec: Vec<T>,
    map: HashMap<SecondaryKey, IDKey>,
}

impl<T, IDKey: Into<usize> + From<usize> + Copy, NameKey: Eq + Hash> VecNameMap<T, IDKey, NameKey> {
    /// Creates a new empty [`VecNameMap`].
    #[must_use]
    pub fn new() -> Self {
        Self {
            vec: Vec::new(),
            map: HashMap::new(),
        }
    }

    /// Adds an item to the container and returns the index of the item.
    ///
    /// # Returns
    /// - `Ok(id)` if the item was added successfully.
    /// - `Err(id)` if the item already with the same name exists.
    #[allow(clippy::missing_errors_doc)]
    pub fn add_item(&mut self, name: NameKey, item: T) -> Result<IDKey, IDKey> {
        match self.map.entry(name) {
            // If the item already exists, return the index of the existing item.
            Entry::Occupied(entry) => Err(*entry.get()),

            // Add the item to the vector and insert the index into the map.
            Entry::Vacant(entry) => {
                // generate new index
                let id = self.vec.len().into();

                self.vec.push(item);
                entry.insert(id);

                Ok(id)
            }
        }
    }

    /// Returns an index of the item with the given name.
    #[must_use]
    pub fn map_name_to_id<Q>(&self, name: &Q) -> Option<IDKey>
    where
        NameKey: Borrow<Q>,
        Q: ?Sized + Eq + Hash,
    {
        self.map.get(name.borrow()).copied()
    }

    /// Returns a reference to the item with the given name.
    #[must_use]
    pub fn get_by_id(&self, id: IDKey) -> Option<&T> { self.vec.get(id.into()) }

    /// Returns a mutable reference to the item with the given name.
    pub fn get_mut_by_id(&mut self, id: IDKey) -> Option<&mut T> { self.vec.get_mut(id.into()) }

    /// Returns a reference to the item with the given name.
    #[must_use]
    pub fn get_by_name<Q>(&self, name: &Q) -> Option<&T>
    where
        NameKey: Borrow<Q>,
        Q: ?Sized + Eq + Hash,
    {
        self.map_name_to_id(name)
            .and_then(|index| self.get_by_id(index))
    }

    /// Returns a mutable reference to the item with the given name.
    pub fn get_mut_by_name<Q>(&mut self, name: &Q) -> Option<&mut T>
    where
        NameKey: Borrow<Q>,
        Q: ?Sized + Eq + Hash,
    {
        self.map_name_to_id(name)
            .and_then(move |index| self.get_mut_by_id(index))
    }

    /// Returns an iterator over all items in the container.
    pub fn iter(&self) -> impl Iterator<Item = &T> { self.vec.iter() }

    /// Returns a mutable iterator over all items in the container.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> { self.vec.iter_mut() }

    /// Returns an iterator over all items in the container with their ID key.
    pub fn iter_with_id(&self) -> impl Iterator<Item = (IDKey, &T)> {
        self.vec.iter().enumerate().map(|(i, v)| (i.into(), v))
    }

    /// Returns a mutable iterator over all items in the container with their ID key.
    pub fn iter_mut_with_id(&mut self) -> impl Iterator<Item = (IDKey, &mut T)> {
        self.vec.iter_mut().enumerate().map(|(i, v)| (i.into(), v))
    }

    /// Returns the number of items in the container.
    #[must_use]
    pub fn len(&self) -> usize { self.vec.len() }

    /// Returns `true` if the container is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.vec.is_empty() }
}

impl<T, IDKey: Into<usize> + From<usize> + Copy, NameKey: Eq + Hash> Index<IDKey>
    for VecNameMap<T, IDKey, NameKey>
{
    type Output = T;

    fn index(&self, id: IDKey) -> &Self::Output { &self.vec[id.into()] }
}

impl<T, IDKey: Into<usize> + From<usize> + Copy, NameKey: Eq + Hash> IndexMut<IDKey>
    for VecNameMap<T, IDKey, NameKey>
{
    fn index_mut(&mut self, index: IDKey) -> &mut Self::Output { &mut self.vec[index.into()] }
}

impl<T, IDKey: Into<usize> + From<usize> + Copy, NameKey: Eq + Hash> Default
    for VecNameMap<T, IDKey, NameKey>
{
    fn default() -> Self { Self::new() }
}

/// Is a structure that contains the syntax tree and the source file where it was parsed from.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxTreeWithSource<T> {
    /// The source file where the syntax tree was parsed from.
    pub source_file: Arc<SourceFile>,

    /// The syntax tree.
    pub syntax_tree: T,
}

/// Represents a function symbol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    /// The name of the function.
    pub name: String,

    /// The access modifier of the function.
    pub access_modifier: AccessModifier,

    /// The return type of the function.
    pub return_type: Type,

    /// The id of the symbol that this function is defined in.
    pub parent_id: ModuleID,

    /// The list of parameters of the function.
    pub parameters: VecNameMap<Parameter, ParameterID>,

    /// The syntax tree that was used to create this symbol.
    pub syntax_tree: SyntaxTreeWithSource<syntax_tree::item::Function>,
}

impl Symbol for Function {
    fn name(&self) -> &str { &self.name }

    fn access_modifier(&self) -> AccessModifier { self.access_modifier }

    fn parent_id(&self) -> Option<ScopedID> { Some(ScopedID::Module(self.parent_id)) }

    fn as_any(&self) -> &dyn Any { self }

    fn as_any_mut(&mut self) -> &mut dyn Any { self }
}

/// Represents a field symbol in a struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field {
    /// The access modifier of the field.
    pub access_modifier: AccessModifier,

    /// The name of the field.
    pub name: String,

    /// The type of the field.
    pub ty: Type,
}

/// Is an identifier that can be used to retrieve a [`Field`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, From, Into)]
pub struct FieldID(usize);

/// Represents a struct symbol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
    /// The name of the struct.
    pub name: String,

    /// The id of the symbol that this struct is defined in.
    pub parent_id: ModuleID,

    /// The access modifier of the struct.
    pub access_modifier: AccessModifier,

    /// The list of fields defined in the struct.
    pub fields: VecNameMap<Field, FieldID>,

    /// The syntax tree that was used to create this symbol.
    pub syntax_tree: SyntaxTreeWithSource<syntax_tree::item::Struct>,
}

impl Symbol for Struct {
    fn name(&self) -> &str { &self.name }

    fn access_modifier(&self) -> AccessModifier { self.access_modifier }

    fn parent_id(&self) -> Option<ScopedID> { Some(ScopedID::Module(self.parent_id)) }

    fn as_any(&self) -> &dyn Any { self }

    fn as_any_mut(&mut self) -> &mut dyn Any { self }
}

/// Represents an enum symbol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    /// The name of the enum.
    pub name: String,

    /// The id of the symbol that this enum is defined in.
    pub parent_id: ModuleID,

    /// The access modifier of the enum.
    pub access_modifier: AccessModifier,

    /// The list of variants defined in the enum.
    ///
    /// The key is the name of the variant and the value is the id symbol of it.
    pub variant_symbol_ids_by_name: HashMap<String, EnumVariantID>,

    /// The syntax tree that was used to create this symbol.
    pub syntax_tree: SyntaxTreeWithSource<syntax_tree::item::Enum>,
}

impl Symbol for Enum {
    fn name(&self) -> &str { &self.name }

    fn access_modifier(&self) -> AccessModifier { self.access_modifier }

    fn parent_id(&self) -> Option<ScopedID> { Some(ScopedID::Module(self.parent_id)) }

    fn as_any(&self) -> &dyn Any { self }

    fn as_any_mut(&mut self) -> &mut dyn Any { self }
}

/// Represents a module symbol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    /// The name of the module.
    pub name: String,

    /// The id of the symbol that this module is defined in.
    ///
    /// If this is `None`, then this module is the root module.
    pub parent_id: Option<ModuleID>,

    /// The list of children symbols of this module.
    ///
    /// The key is the name of the child symbol and the value is the id symbol of it.
    pub children_symbol_ids_by_name: HashMap<String, ID>,

    /// The syntax tree that was used to create this symbol.
    pub access_modifier: AccessModifier,
}

impl Symbol for Module {
    fn name(&self) -> &str { &self.name }

    fn access_modifier(&self) -> AccessModifier { self.access_modifier }

    fn parent_id(&self) -> Option<ScopedID> { self.parent_id.map(ScopedID::Module) }

    fn as_any(&self) -> &dyn Any { self }

    fn as_any_mut(&mut self) -> &mut dyn Any { self }
}

/// Represents a variant defined in an enum.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EnumVariant {
    /// The name of the variant.
    pub name: String,

    /// The declaration order of the variant (starting at 0).
    pub variant_number: usize,

    /// The id of the parent enum symbol.
    pub parent_id: EnumID,

    /// The access modifier of the variant.
    pub access_modifier: AccessModifier,
}

impl Symbol for EnumVariant {
    fn name(&self) -> &str { &self.name }

    fn access_modifier(&self) -> AccessModifier { self.access_modifier }

    fn parent_id(&self) -> Option<ScopedID> { Some(ScopedID::Enum(self.parent_id)) }

    fn as_any(&self) -> &dyn Any { self }

    fn as_any_mut(&mut self) -> &mut dyn Any { self }
}

mod private {
    use std::collections::HashMap;

    use super::{
        Enum, EnumID, EnumVariant, EnumVariantID, Function, FunctionID, Module, ModuleID, Struct,
        StructID, Table, ID,
    };

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct SymbolBundle {
        pub(super) modules: HashMap<ModuleID, Module>,
        pub(super) structs: HashMap<StructID, Struct>,
        pub(super) enums: HashMap<EnumID, Enum>,
        pub(super) functions: HashMap<FunctionID, Function>,
        pub(super) enum_variants: HashMap<EnumVariantID, EnumVariant>,
    }

    pub trait SealedSymbol {
        fn add_to_symbol_bundle(self, symbol_bundle: &mut SymbolBundle) -> ID;
    }

    impl SealedSymbol for Module {
        fn add_to_symbol_bundle(self, symbol_bundle: &mut SymbolBundle) -> ID {
            let id = ModuleID(Table::new_id());
            symbol_bundle.modules.insert(id, self);
            id.into()
        }
    }

    impl SealedSymbol for Struct {
        fn add_to_symbol_bundle(self, symbol_bundle: &mut SymbolBundle) -> ID {
            let id = StructID(Table::new_id());
            symbol_bundle.structs.insert(id, self);
            id.into()
        }
    }

    impl SealedSymbol for Enum {
        fn add_to_symbol_bundle(self, symbol_bundle: &mut SymbolBundle) -> ID {
            let id = EnumID(Table::new_id());
            symbol_bundle.enums.insert(id, self);
            id.into()
        }
    }

    impl SealedSymbol for Function {
        fn add_to_symbol_bundle(self, symbol_bundle: &mut SymbolBundle) -> ID {
            let id = FunctionID(Table::new_id());
            symbol_bundle.functions.insert(id, self);
            id.into()
        }
    }

    impl SealedSymbol for EnumVariant {
        fn add_to_symbol_bundle(self, symbol_bundle: &mut SymbolBundle) -> ID {
            let id = EnumVariantID(Table::new_id());
            symbol_bundle.enum_variants.insert(id, self);
            id.into()
        }
    }
}

/// Is an error that can occur when resolving a symbol with [`Table::resolve_symbol()`]
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From, Error)]
#[allow(missing_docs)]
pub enum ResolveError {
    #[error("{0}")]
    SemanticError(SemanticError),

    #[error("The given `qualified_identifier` argument is not a valid identifier token iterator.")]
    InvalidQualifiedIdentifierIterator,
}

/// Is an error that can occur when resolving a symbol with [`Table::resolve_type()`]
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From, Error)]
#[allow(missing_docs)]
pub enum TypeResolveError {
    #[error("{0}")]
    SemanticError(SemanticError),

    #[error("The given `referring_site_symbol_id` argument is not a valid symbol id.")]
    InvalidSymbolID,

    #[error("The given `type_specifier_syntax_tree` argument is not a valid syntax tree.")]
    InvalidSyntaxTree,
}

/// Is a structure that contains all the symbols that can be accessed in a global scope.
///
/// This structure represnts the output of the first pass in the semantic analysis -- the symbol
/// generation pass.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Table {
    symbol_bundle: SymbolBundle,
    root_ids_by_name: HashMap<String, ModuleID>,
}

impl Table {
    fn new_id() -> usize {
        // atomic increment
        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

        NEXT_ID.fetch_add(1, Ordering::SeqCst)
    }

    /// Gets a reference to the symbol by its identifier.
    ///
    /// # Panics
    /// Panics if the given `id` is not a valid symbol id or the `id` was not created by this table.
    pub fn get<'a, T: Indexer<'a>>(&'a self, id: T) -> <T as Indexer<'a>>::Output { id.get(self) }

    /// Gets the [`ID`] for the symbol with the given qualified name.
    ///
    /// Returns `None` if the symbol does not exist.
    pub fn get_id_by_full_name<'a>(
        &self,
        qualified_name: impl Iterator<Item = &'a str>,
    ) -> Option<ID> {
        let mut current_index: Option<ID> = None;

        for name in qualified_name {
            if let Some(index) = current_index {
                match index {
                    ID::Module(module_id) => {
                        if let Some(id) = self
                            .get(module_id)
                            .children_symbol_ids_by_name
                            .get(name)
                            .copied()
                        {
                            current_index = Some(id);
                            continue;
                        }
                    }
                    ID::Enum(enum_id) => {
                        if let Some(id) = self
                            .get(enum_id)
                            .variant_symbol_ids_by_name
                            .get(name)
                            .copied()
                        {
                            current_index = Some(id.into());
                            continue;
                        }
                    }
                    _ => (),
                }

                return None;
            } else if let Some(index) = self.root_ids_by_name.get(name).copied() {
                current_index = Some(index.into());
            } else {
                return None;
            }
        }

        current_index
    }

    /// Resolves the type of the given [`TypeSpecifier`] syntax tree.
    ///
    /// # Parameters
    /// - `referring_site_symbol_index`: the symbol index of the site where the type is being
    ///   resolved. This is used in scope resolution and accessibility checks.
    /// - `type_specifier_syntax_tree`: the [`TypeSpecifier`] syntax tree to resolve.
    /// - `source_file`: the source file where the syntax tree is from.
    ///
    /// # Panics
    /// `referring_site_symbol_index` must be a valid symbol index.
    ///
    /// # Errors
    /// Returns [`SemanticError::SymbolNotFound`] any semantic error that occurs during the type
    /// resolution.
    pub fn resolve_type(
        &self,
        referring_site_symbol_id: ID,
        type_specifier_syntax_tree: &TypeSpecifier,
        source_file: &Arc<SourceFile>,
    ) -> SemanticResult<Type, TypeResolveError, SemanticError> {
        match type_specifier_syntax_tree {
            TypeSpecifier::PrimitiveTypeSpecifier(primitive_type) => Ok(match primitive_type {
                PrimitiveTypeSpecifier::Bool(..) => Type::Primitive(PrimitiveType::Bool),
                PrimitiveTypeSpecifier::Float32(..) => Type::Primitive(PrimitiveType::Float32),
                PrimitiveTypeSpecifier::Float64(..) => Type::Primitive(PrimitiveType::Float64),
                PrimitiveTypeSpecifier::Int8(..) => Type::Primitive(PrimitiveType::Int8),
                PrimitiveTypeSpecifier::Int16(..) => Type::Primitive(PrimitiveType::Int16),
                PrimitiveTypeSpecifier::Int32(..) => Type::Primitive(PrimitiveType::Int32),
                PrimitiveTypeSpecifier::Int64(..) => Type::Primitive(PrimitiveType::Int64),
                PrimitiveTypeSpecifier::Uint8(..) => Type::Primitive(PrimitiveType::Uint8),
                PrimitiveTypeSpecifier::Uint16(..) => Type::Primitive(PrimitiveType::Uint16),
                PrimitiveTypeSpecifier::Uint32(..) => Type::Primitive(PrimitiveType::Uint32),
                PrimitiveTypeSpecifier::Uint64(..) => Type::Primitive(PrimitiveType::Uint64),
                PrimitiveTypeSpecifier::Void(..) => Type::Primitive(PrimitiveType::Void),
            }
            .into()),
            TypeSpecifier::QualifiedIdentifier(qualified_identifier) => {
                fn error_map(resolve: ResolveError) -> TypeResolveError {
                    match resolve {
                        ResolveError::SemanticError(semantic_error) => {
                            TypeResolveError::SemanticError(semantic_error)
                        }
                        ResolveError::InvalidQualifiedIdentifierIterator => {
                            TypeResolveError::InvalidSyntaxTree
                        }
                    }
                }

                let OkResult {
                    value: symbol_id,
                    errors,
                } = self
                    .resolve_symbol(
                        referring_site_symbol_id,
                        qualified_identifier.elements(),
                        source_file,
                    )
                    .map_err(|err| err.into_iter().map(error_map).collect::<Vec<_>>())?;

                match symbol_id {
                    ID::Struct(struct_id) => Ok(OkResult {
                        value: Type::TypedID(struct_id.into()),
                        errors,
                    }),
                    ID::Enum(enum_id) => Ok(OkResult {
                        value: Type::TypedID(enum_id.into()),
                        errors,
                    }),
                    _ => Err(errors
                        .into_iter()
                        .map(TypeResolveError::SemanticError)
                        .chain(std::iter::once(TypeResolveError::SemanticError(
                            TypeExpected {
                                source_span: SourceSpan {
                                    source_file: source_file.clone(),
                                    span: qualified_identifier.span(),
                                },
                                symbol_id,
                            }
                            .into(),
                        )))
                        .collect()),
                }
            }
        }
    }

    /// Check if `child` is a child of `parent` or not
    ///
    /// # Panics
    /// `child` must be a valid symbol index.
    /// `parent` must be a valid symbol index.
    #[must_use]
    pub fn is_parent(&self, parent: ScopedID, child: ID) -> bool {
        let mut current_symbol = self.get(child);

        loop {
            match current_symbol.parent_id() {
                Some(parent_id) => {
                    if parent_id == parent {
                        return true;
                    }

                    let parent_id: ID = parent_id.into();
                    current_symbol = self.get(parent_id);
                }
                None => return false,
            }
        }
    }

    /// Applies the symbol resolution from the given qualified identifier syntax.
    ///
    /// # Parameters
    /// - `referring_site_symbol_index`: the index of the symbol that is referring to the symbol
    ///   being resolved.
    /// - `qualified_name`: the iterator yielding the identifier syntaxes of the qualified name.
    /// - `qualified_identifier`: the source file that the syntax tree is from.
    ///
    /// # Errors
    /// - [`ResolveError::SemanticError`]: any semantic error that occurs during the resolution.
    /// - [`ResolveError::InvalidQualifiedIdentifierIterator`]: `qualified_identifier` is not a
    ///   valid qualified identifier iterator.
    /// - [`ResolveError::InvalidQualifiedIdentifierIterator`]: `qualified_identifier` is an empty
    ///   iterator.
    #[allow(clippy::too_many_lines)]
    pub fn resolve_symbol<'a>(
        &self,
        referring_site_symbol_id: ID,
        mut qualified_identifier: impl Iterator<Item = &'a IdentifierToken>,
        source_file: &Arc<SourceFile>,
    ) -> SemanticResult<ID, ResolveError, SemanticError> {
        // the closest parent module symbol from the given `parent_index`
        let (parent_module_symbol, parent_module_id) = {
            let mut current_symbol = self.get(referring_site_symbol_id);

            let mut current_id = referring_site_symbol_id;
            loop {
                // found module, break
                if let Some(module) = current_symbol.as_any().downcast_ref::<Module>() {
                    break (module, current_id.into_module().unwrap());
                }

                current_symbol.parent_id().map_or_else(
                    || panic!("must have a parent module!"),
                    |parent_id| {
                        let parent_id: ID = parent_id.into();
                        current_symbol = self.get(parent_id);
                        current_id = parent_id;
                    },
                );
            }
        };

        let first_name = qualified_identifier
            .next()
            .ok_or(vec![ResolveError::InvalidQualifiedIdentifierIterator])?;

        let (mut current_symbol, mut current_id) = match parent_module_symbol
            .children_symbol_ids_by_name
            .get(&source_file[first_name.span])
            .copied()
        {
            Some(symbol_id) => {
                // start from this symbol
                (self.get(symbol_id), symbol_id)
            }
            None => {
                // start from the root
                match self
                    .root_ids_by_name
                    .get(&source_file[first_name.span])
                    .copied()
                {
                    Some(symbol_id) => (self.get(symbol_id) as _, symbol_id.into()),
                    None => {
                        return Err(vec![ResolveError::SemanticError(
                            SymbolNotFound {
                                referencing_site: SourceSpan {
                                    source_file: source_file.clone(),
                                    span: first_name.span,
                                },
                                in_scope: None,
                            }
                            .into(),
                        )]);
                    }
                }
            }
        };

        // the list of recoverable errors
        let mut errors = Vec::new();

        // loop through the rest of the identifiers
        loop {
            match qualified_identifier.next() {
                Some(identifier) => {
                    let Some(name) = source_file.get(identifier.span) else {
                        return Err(errors
                            .into_iter()
                            .map(ResolveError::SemanticError)
                            .chain(std::iter::once(
                                ResolveError::InvalidQualifiedIdentifierIterator,
                            ))
                            .collect::<Vec<_>>());
                    };

                    // must be module symbol to continue
                    let child_id = if let Some(module_symbol) =
                        current_symbol.as_any().downcast_ref::<Module>()
                    {
                        module_symbol.children_symbol_ids_by_name.get(name).copied()
                    } else if let Some(enum_symbol) = current_symbol.as_any().downcast_ref::<Enum>()
                    {
                        enum_symbol
                            .variant_symbol_ids_by_name
                            .get(name)
                            .copied()
                            .map(ID::EnumVariant)
                    } else {
                        return Err(errors
                            .into_iter()
                            .map(ResolveError::SemanticError)
                            .chain(std::iter::once(ResolveError::SemanticError(
                                SymbolNotFound {
                                    referencing_site: SourceSpan {
                                        source_file: source_file.clone(),
                                        span: identifier.span,
                                    },
                                    in_scope: Some(current_id),
                                }
                                .into(),
                            )))
                            .collect());
                    };

                    // look for the child symbol in the current module
                    match child_id {
                        Some(symbol_index) => {
                            let symbol = self.get(symbol_index);

                            // if symbol is defined as private, it must be a child of the parent
                            // module symbol
                            if symbol.access_modifier() == AccessModifier::Private
                                && !self.is_parent(parent_module_id.into(), symbol_index)
                            {
                                errors.push(
                                    SymbolIsNotAccessible {
                                        referencing_site: SourceSpan {
                                            source_file: source_file.clone(),
                                            span: identifier.span,
                                        },
                                        symbol_id: symbol_index,
                                    }
                                    .into(),
                                );
                            }

                            // continue to the next symbol
                            current_symbol = symbol;
                            current_id = symbol_index;
                        }
                        None => {
                            // symbol not found
                            return Err(errors
                                .into_iter()
                                .map(ResolveError::SemanticError)
                                .chain(std::iter::once(ResolveError::SemanticError(
                                    SymbolNotFound {
                                        referencing_site: SourceSpan {
                                            source_file: source_file.clone(),
                                            span: identifier.span,
                                        },
                                        in_scope: Some(current_id),
                                    }
                                    .into(),
                                )))
                                .collect());
                        }
                    }
                }
                None => {
                    break Ok(OkResult {
                        value: current_id,
                        errors,
                    })
                }
            }
        }
    }

    /// Creates a new [`Table`] from the given file inputs.
    pub fn analyze(files: impl Iterator<Item = FileParsing>) -> (Self, Vec<SemanticError>) {
        let mut symbol_table = Self {
            symbol_bundle: SymbolBundle {
                modules: HashMap::new(),
                structs: HashMap::new(),
                enums: HashMap::new(),
                functions: HashMap::new(),
                enum_variants: HashMap::new(),
            },
            root_ids_by_name: HashMap::new(),
        };
        let mut errors = Vec::new();
        let files = files.collect::<Vec<_>>();

        // generates the module heirarchy
        symbol_table.generate_module(&files);

        let mut files = files
            .into_iter()
            .map(pernixc_syntax::file_parsing::FileParsing::destruct)
            .map(|(source_file, syntax_tree, _)| FileParsingInternal {
                source_file,
                syntax_tree,
            })
            .collect::<Vec<_>>();

        // populates the enum symbols
        symbol_table.populate_symbol(
            &mut files,
            &mut errors,
            // filter enum
            |syntax_tree| matches!(syntax_tree, ItemSyntaxTree::Enum(..)),
            // get enum name
            |syntax_tree| match syntax_tree {
                ItemSyntaxTree::Enum(enum_syntax_tree) => &enum_syntax_tree.identifier,
                _ => unreachable!(),
            },
            // create enum symbol
            |_, source_file, parent_id, _, syntax_tree| {
                let enum_syntax_tree = syntax_tree.into_enum().expect("should be enum");

                Enum {
                    name: source_file[enum_syntax_tree.identifier.span].to_string(),
                    parent_id,
                    access_modifier: AccessModifier::from_syntax_tree(
                        &enum_syntax_tree.access_modifier,
                    ),
                    variant_symbol_ids_by_name: HashMap::new(),
                    syntax_tree: SyntaxTreeWithSource {
                        source_file: source_file.clone(),
                        syntax_tree: enum_syntax_tree,
                    },
                }
            },
        );

        // populate the struct symbols
        symbol_table.populate_symbol(
            &mut files,
            &mut errors,
            // filter struct
            |syntax_tree| matches!(syntax_tree, ItemSyntaxTree::Struct(..)),
            // get struct name
            |syntax_tree| match syntax_tree {
                ItemSyntaxTree::Struct(struct_syntax_tree) => &struct_syntax_tree.identifier,
                _ => unreachable!(),
            },
            // create struct symbol
            |_, source_file, parent_id, _, syntax_tree| {
                let struct_syntax_tree = syntax_tree.into_struct().expect("should be struct");

                Struct {
                    name: source_file[struct_syntax_tree.identifier.span].to_string(),
                    parent_id,
                    access_modifier: AccessModifier::from_syntax_tree(
                        &struct_syntax_tree.access_modifier,
                    ),
                    fields: VecNameMap::default(), // will be added later
                    syntax_tree: SyntaxTreeWithSource {
                        source_file: source_file.clone(),
                        syntax_tree: struct_syntax_tree,
                    },
                }
            },
        );

        // populate the function symbols
        symbol_table.populate_symbol(
            &mut files,
            &mut errors,
            // filter function
            |syntax_tree| matches!(syntax_tree, ItemSyntaxTree::Function(..)),
            // get function name
            |syntax_tree| match syntax_tree {
                syntax_tree::item::Item::Function(function_syntax_tree) => {
                    &function_syntax_tree.identifier
                }
                _ => unreachable!(),
            },
            // create function symbol
            |_, source_file, parent_id, _, syntax_tree| {
                let function_syntax_tree = syntax_tree.into_function().expect("should be function");

                Function {
                    name: source_file[function_syntax_tree.identifier.span].to_string(),
                    access_modifier: AccessModifier::from_syntax_tree(
                        &function_syntax_tree.access_modifier,
                    ),
                    return_type: Type::Primitive(PrimitiveType::Void), // Will be determined later
                    parent_id,
                    parameters: VecNameMap::default(), // Will be determined later
                    syntax_tree: SyntaxTreeWithSource {
                        source_file: source_file.clone(),
                        syntax_tree: function_syntax_tree,
                    },
                }
            },
        );

        // populate the function data
        symbol_table.populate_function_data(&mut errors);

        // populate the struct fields
        symbol_table.populate_struct_data(&mut errors);

        // populate the enum variants
        symbol_table.populate_enum_data(&mut errors);

        (symbol_table, errors)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FileParsingInternal {
    source_file: Arc<SourceFile>,
    syntax_tree: File,
}

impl Table {
    fn index_mut<'a, T: IndexerMut<'a>>(&'a mut self, id: T) -> <T as IndexerMut<'a>>::Output {
        id.get_mut(self)
    }

    fn create_function_parameters(
        &self,
        function: &Function,
        errors: &mut Vec<SemanticError>,
    ) -> VecNameMap<Parameter, ParameterID> {
        // get arguments
        function
            .syntax_tree
            .syntax_tree
            .parameters
            .as_ref()
            .map_or_else(VecNameMap::default, |parameters_syntax| {
                let mut parameters = VecNameMap::new();

                for parameter in parameters_syntax.elements() {
                    let name =
                        function.syntax_tree.source_file[parameter.identifier.span].to_string();

                    let type_binding_specifier = TypeBinding {
                        ty: match self.resolve_type(
                            function.parent_id.into(),
                            &parameter.type_binding_specifier.type_specifier,
                            &function.syntax_tree.source_file,
                        ) {
                            Ok(ty) => {
                                let ty = ty.unwrap_extend(errors);
                                if let Type::TypedID(typed_id) = ty {
                                    // accessibility leak
                                    let type_symbol = self.get(typed_id);

                                    if type_symbol.access_modifier().rank()
                                        > function.access_modifier.rank()
                                    {
                                        errors.push(
                                            AccessibilityLeaking {
                                                symbol_span: SourceSpan {
                                                    source_file: function
                                                        .syntax_tree
                                                        .source_file
                                                        .clone(),
                                                    span: function
                                                        .syntax_tree
                                                        .syntax_tree
                                                        .type_specifier
                                                        .span(),
                                                },
                                                symbol_access_modifier: type_symbol
                                                    .access_modifier(),
                                                parent_access_modifier: function.access_modifier,
                                            }
                                            .into(),
                                        );
                                    }
                                }
                                ty
                            }
                            Err(err) => {
                                // should be only a semantic error
                                errors.extend(
                                    err.into_iter()
                                        .map(|err| err.into_semantic_error().unwrap()),
                                );
                                Type::default()
                            }
                        },
                        is_mutable: parameter.type_binding_specifier.mutable_keyword.is_some(),
                    };

                    // parameter redefinition
                    if parameters
                        .add_item(name.clone(), Parameter {
                            name,
                            type_binding: type_binding_specifier,
                        })
                        .is_err()
                    {
                        errors.push(
                            ParameterRedifinition {
                                new_parameter_span: SourceSpan {
                                    source_file: function.syntax_tree.source_file.clone(),
                                    span: parameter.identifier.span(),
                                },
                            }
                            .into(),
                        );
                    }
                }

                parameters
            })
    }

    fn create_function_return_type(
        &self,
        function: &Function,
        errors: &mut Vec<SemanticError>,
    ) -> Type {
        match self.resolve_type(
            function.parent_id.into(),
            &function.syntax_tree.syntax_tree.type_specifier,
            &function.syntax_tree.source_file,
        ) {
            Ok(ty) => {
                let ty = ty.unwrap_extend(errors);
                if let Type::TypedID(typed_id) = ty {
                    // accessibility leak
                    let type_symbol = self.get(typed_id);

                    if type_symbol.access_modifier().rank() > function.access_modifier.rank() {
                        errors.push(
                            AccessibilityLeaking {
                                symbol_span: SourceSpan {
                                    source_file: function.syntax_tree.source_file.clone(),
                                    span: function.syntax_tree.syntax_tree.type_specifier.span(),
                                },
                                symbol_access_modifier: type_symbol.access_modifier(),
                                parent_access_modifier: function.access_modifier,
                            }
                            .into(),
                        );
                    }
                }
                ty
            }
            Err(err) => {
                // should be only a semantic error
                errors.extend(
                    err.into_iter()
                        .map(|err| err.into_semantic_error().unwrap()),
                );
                Type::default()
            }
        }
    }

    fn populate_function_data(&mut self, errors: &mut Vec<SemanticError>) {
        // I have to make a whole new copy of keys because I can't borrow table twice, sucks
        for index in self
            .symbol_bundle
            .functions
            .keys()
            .copied()
            .collect::<Vec<_>>()
        {
            let (parameters, return_type_specifier) = {
                let function_symbol = &self.symbol_bundle.functions[&index];

                (
                    self.create_function_parameters(function_symbol, errors),
                    self.create_function_return_type(function_symbol, errors),
                )
            };

            // update the function symbol
            self.symbol_bundle
                .functions
                .get_mut(&index)
                .unwrap()
                .parameters = parameters;
            self.symbol_bundle
                .functions
                .get_mut(&index)
                .unwrap()
                .return_type = return_type_specifier;
        }
    }

    fn populate_struct_data(&mut self, errors: &mut Vec<SemanticError>) {
        // I have to make a whole new copy of keys because I can't borrow table twice, sucks
        for index in self
            .symbol_bundle
            .structs
            .keys()
            .copied()
            .collect::<Vec<_>>()
        {
            let fields = {
                let struct_symbol = self.symbol_bundle.structs.get(&index).unwrap();
                let mut fields = VecNameMap::new();

                for field_group in &struct_symbol.syntax_tree.syntax_tree.field_groups {
                    let group_access_modifier =
                        AccessModifier::from_syntax_tree(&field_group.access_modifier);

                    if group_access_modifier.rank() < struct_symbol.access_modifier.rank() {
                        errors.push(SemanticError::AccessibilityLeaking(AccessibilityLeaking {
                            symbol_span: SourceSpan {
                                source_file: struct_symbol.syntax_tree.source_file.clone(),
                                span: field_group.access_modifier.span(),
                            },
                            symbol_access_modifier: group_access_modifier,
                            parent_access_modifier: struct_symbol.access_modifier,
                        }));
                    }

                    for field in &field_group.fields {
                        let ty = match self.resolve_type(
                            struct_symbol.parent_id.into(),
                            &field.type_specifier,
                            &struct_symbol.syntax_tree.source_file,
                        ) {
                            Ok(ty) => ty.unwrap_extend(errors),
                            Err(err) => {
                                // should be only a semantic error
                                errors.extend(
                                    err.into_iter()
                                        .map(|err| err.into_semantic_error().unwrap()),
                                );
                                Type::default()
                            }
                        };

                        let name = struct_symbol.syntax_tree.source_file[field.identifier.span]
                            .to_string();

                        if fields
                            .add_item(name.clone(), Field {
                                access_modifier: group_access_modifier,
                                name,
                                ty,
                            })
                            .is_err()
                        {
                            errors.push(
                                FieldRedefinition {
                                    new_field_span: SourceSpan {
                                        source_file: struct_symbol.syntax_tree.source_file.clone(),
                                        span: field.identifier.span,
                                    },
                                }
                                .into(),
                            );
                        }
                    }
                }

                fields
            };
            self.symbol_bundle.structs.get_mut(&index).unwrap().fields = fields;
        }
    }

    fn populate_enum_data(&mut self, errors: &mut Vec<SemanticError>) {
        // I have to make a whole new copy of keys because I can't borrow table twice, sucks
        for index in self.symbol_bundle.enums.keys().copied().collect::<Vec<_>>() {
            let (access_modifier, variants) = {
                let enum_sym = self.symbol_bundle.enums.get(&index).unwrap();

                (enum_sym.access_modifier, 'ret: {
                    // check if the variants are empty.
                    let Some(variants) = &enum_sym.syntax_tree.syntax_tree.variants  else {
                        break 'ret Vec::new();
                    };

                    let mut return_variants = Vec::with_capacity(variants.len());
                    let mut inserted_variants = HashSet::with_capacity(variants.len());

                    for variant in variants.elements() {
                        let variant_string =
                            enum_sym.syntax_tree.source_file[variant.span].to_string();

                        if inserted_variants.contains(&variant_string) {
                            errors.push(
                                EnumVariantRedefinition {
                                    new_variant_span: SourceSpan {
                                        source_file: enum_sym.syntax_tree.source_file.clone(),
                                        span: variant.span,
                                    },
                                }
                                .into(),
                            );
                            continue;
                        }

                        inserted_variants.insert(variant_string.clone());
                        return_variants.push(variant_string);
                    }

                    return_variants
                })
            };

            let mut variant_indices_by_name = HashMap::new();
            for (variant_number, variant) in variants.iter().enumerate() {
                variant_indices_by_name.insert(
                    variant.clone(),
                    EnumVariant {
                        name: variant.clone(),
                        parent_id: index,
                        access_modifier,
                        variant_number,
                    }
                    .add_to_symbol_bundle(&mut self.symbol_bundle)
                    .into_enum_variant()
                    .unwrap(),
                );
            }

            self.symbol_bundle
                .enums
                .get_mut(&index)
                .unwrap()
                .variant_symbol_ids_by_name = variant_indices_by_name;
        }
    }

    fn generate_module(&mut self, files: &[FileParsing]) {
        for file in files {
            let mut parent_module: Option<ModuleID> = None;

            for module_name in file.source_file().module_heirarchy() {
                if let Some(parent_module_id) = parent_module {
                    // module heirarchy should not duplicate
                    if let Some(children_id) = self
                        .get(parent_module_id)
                        .children_symbol_ids_by_name
                        .get(module_name)
                    {
                        parent_module = Some(children_id.into_module().unwrap());
                    } else {
                        // create the module
                        let module_symbol = Module {
                            name: module_name.to_string(),
                            parent_id: Some(parent_module_id),
                            children_symbol_ids_by_name: HashMap::new(),
                            access_modifier: AccessModifier::Public,
                        };

                        // add the module to the symbol table
                        let module_id = module_symbol
                            .add_to_symbol_bundle(&mut self.symbol_bundle)
                            .into_module()
                            .unwrap();
                        parent_module = Some(module_id);

                        // add the module to the parent module
                        self.index_mut(parent_module_id)
                            .children_symbol_ids_by_name
                            .insert(module_name.to_string(), module_id.into());
                    }
                } else {
                    // module heirarchy should not duplicate
                    if let Some(idx) = self.root_ids_by_name.get(module_name) {
                        parent_module = Some(*idx);
                    } else {
                        // create the module
                        let module_symbol = Module {
                            name: module_name.to_string(),
                            parent_id: None,
                            children_symbol_ids_by_name: HashMap::new(),
                            access_modifier: AccessModifier::Public,
                        };

                        // add the module to the symbol table
                        let module_id = module_symbol
                            .add_to_symbol_bundle(&mut self.symbol_bundle)
                            .into_module()
                            .unwrap();

                        // add to the root
                        self.root_ids_by_name
                            .insert(module_name.to_string(), module_id);

                        parent_module = Some(module_id);
                    }
                }
            }
        }
    }

    fn populate_symbol<T: Symbol>(
        &mut self,
        files: &mut Vec<FileParsingInternal>,
        errors: &mut Vec<SemanticError>,
        syntax_tree_predicate: impl Fn(&ItemSyntaxTree) -> bool,
        syntax_tree_indexentifier_getter: impl Fn(&ItemSyntaxTree) -> &IdentifierToken,
        symbol_creator: impl Fn(
            &mut Self,               // the symbol table
            Arc<SourceFile>,         // the source file
            ModuleID,                // the parent symbol index
            &mut Vec<SemanticError>, // the error list for reporting the errors
            ItemSyntaxTree,          // the syntax tree
        ) -> T,
    ) {
        for FileParsingInternal {
            source_file,
            syntax_tree,
        } in files
        {
            let mut index = 0;

            // gets all the syntax tree that satisfies the predicate
            while index < syntax_tree.items.len() {
                let item_syntax_tree = if syntax_tree_predicate(&syntax_tree.items[index]) {
                    syntax_tree.items.remove(index)
                } else {
                    index += 1;
                    continue;
                };

                // symbol redefinition error
                {
                    let identifier_token = syntax_tree_indexentifier_getter(&item_syntax_tree);
                    if let Some(available_symbol_id) = self
                        .get_id_by_full_name(
                            source_file.module_heirarchy().iter().map(AsRef::as_ref),
                        )
                        .map(|idx| self.index_mut(idx.into_module().unwrap()))
                        .unwrap()
                        .children_symbol_ids_by_name
                        .get(&source_file[identifier_token.span])
                        .copied()
                    {
                        errors.push(
                            SymbolRedifinition {
                                available_symbol_id,
                                new_symbol_span: SourceSpan {
                                    source_file: source_file.clone(),
                                    span: identifier_token.span,
                                },
                            }
                            .into(),
                        );
                        continue;
                    }
                }

                // module symbol index
                let parent_module_id = self
                    .get_id_by_full_name(source_file.module_heirarchy().iter().map(AsRef::as_ref))
                    .unwrap()
                    .into_module()
                    .unwrap();

                // create symbol
                let symbol = symbol_creator(
                    self,
                    source_file.clone(),
                    parent_module_id,
                    errors,
                    item_syntax_tree,
                );
                let symbol_name = symbol.name().to_string();

                // add symbol to module
                let global_id = symbol.add_to_symbol_bundle(&mut self.symbol_bundle);

                // add to the module
                self.index_mut(parent_module_id)
                    .children_symbol_ids_by_name
                    .insert(symbol_name, global_id);
            }
        }
    }
}

#[cfg(test)]
mod tests;
