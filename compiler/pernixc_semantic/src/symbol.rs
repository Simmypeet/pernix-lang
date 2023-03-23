use std::collections::{hash_map::Entry, HashMap};

use enum_as_inner::EnumAsInner;
use pernixc_syntax::syntax_tree::item::{EnumSyntaxTree, FunctionSyntaxTree, StructSyntaxTree};

use self::ty::{Type, TypeBinding};

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

/// Is an enumeration of all item symbols that can be defined in a namespace.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum ItemSymbol {
    Module(ModuleItemSymbol),
    Struct(StructItemSymbol),
    Enum(EnumItemSymbol),
    Function(FunctionItemSymbol),
}

impl ItemSymbol {
    /// Gets the name of the item.
    pub fn name(&self) -> &str {
        match self {
            ItemSymbol::Module(symbol) => &symbol.name,
            ItemSymbol::Struct(symbol) => &symbol.name,
            ItemSymbol::Enum(symbol) => &symbol.name,
            ItemSymbol::Function(symbol) => &symbol.name,
        }
    }

    pub fn access_modifier(&self) -> AccessModifier {
        match self {
            ItemSymbol::Module(symbol) => symbol.access_modifier,
            ItemSymbol::Struct(symbol) => symbol.access_modifier,
            ItemSymbol::Enum(symbol) => symbol.access_modifier,
            ItemSymbol::Function(symbol) => symbol.access_modifier,
        }
    }

    pub fn parent(&self) -> Option<SymbolID> {
        match self {
            ItemSymbol::Module(symbol) => symbol.parent,
            ItemSymbol::Struct(symbol) => symbol.parent,
            ItemSymbol::Enum(symbol) => symbol.parent,
            ItemSymbol::Function(symbol) => symbol.parent,
        }
    }
}

/// Is a container that works similar to a hash map -- allows fast access to items by name -- but
/// also allows fast access to items by index.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VecNameMap<T> {
    vec: Vec<T>,
    map: HashMap<String, usize>,
}

impl<T> VecNameMap<T> {
    /// Creates a new empty [`VecNameMap`].
    pub fn new() -> Self {
        Self {
            vec: Vec::new(),
            map: HashMap::new(),
        }
    }

    /// Adds an item to the container and returns the index of the item.
    ///
    /// # Returns
    /// - `Ok(index)` if the item was added successfully.
    /// - `Err(index)` if the item already with the same name exists.
    pub fn add_item(&mut self, name: String, item: T) -> Result<usize, usize> {
        match self.map.entry(name) {
            // If the item already exists, return the index of the existing item.
            Entry::Occupied(entry) => Err(*entry.get()),

            // Add the item to the vector and insert the index into the map.
            Entry::Vacant(entry) => {
                // generate new index
                let index = self.vec.len();

                self.vec.push(item);
                entry.insert(index);

                Ok(index)
            }
        }
    }

    /// Returns an index of the item with the given name.
    pub fn get_index(&self, name: &str) -> Option<usize> { self.map.get(name).copied() }

    /// Returns a reference to the item with the given name.
    pub fn get(&self, index: usize) -> Option<&T> { self.vec.get(index) }

    /// Returns a mutable reference to the item with the given name.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> { self.vec.get_mut(index) }
}

impl<T> Default for VecNameMap<T> {
    fn default() -> Self { Self::new() }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParameterSymbol {
    pub name:         String,
    pub type_binding: TypeBinding,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionItemSymbol {
    pub name:            String,
    pub parent:          Option<SymbolID>,
    pub access_modifier: AccessModifier,
    pub return_type:     Type,
    pub parameters:      VecNameMap<ParameterSymbol>,
    pub syntax_tree:     FunctionSyntaxTree,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldSymbol {
    pub access_modifier: AccessModifier,
    pub name:            String,
    pub ty:              Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructItemSymbol {
    pub name:            String,
    pub parent:          Option<SymbolID>,
    pub access_modifier: AccessModifier,
    pub fields:          VecNameMap<FieldSymbol>,
    pub syntax_tree:     StructSyntaxTree,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumItemSymbol {
    pub name:            String,
    pub parent:          Option<SymbolID>,
    pub access_modifier: AccessModifier,
    pub variants:        VecNameMap<String>,
    pub syntax_tree:     EnumSyntaxTree,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleItemSymbol {
    pub name:            String,
    pub parent:          Option<SymbolID>,
    pub children:        VecNameMap<SymbolID>,
    pub access_modifier: AccessModifier,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SymbolID(usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemSymbolTable {
    symbols: VecNameMap<ItemSymbol>,
}

impl ItemSymbolTable {
    
}
