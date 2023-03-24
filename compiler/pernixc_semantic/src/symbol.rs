use std::{
    collections::{hash_map::Entry, HashMap},
    hash::Hash,
    sync::Arc,
};

use derive_new::new;
use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::SourceFile;
use pernixc_lexical::token::IdentifierToken;
use pernixc_syntax::{
    file_parsing::{FileParsing, FileParsingError},
    syntax_tree::{
        item::{
            AccessModifierSyntaxTree, EnumSyntaxTree, FunctionSyntaxTree, ItemSyntaxTree,
            StructSyntaxTree,
        },
        FileSyntaxTree,
    },
};
use thiserror::Error;

use self::{
    errors::{SemanticSymbolError, SymbolRedifinition},
    ty::{Type, TypeBinding},
};
use crate::SourceSpan;

pub mod errors;
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
    pub fn rank(self) -> u32 {
        match self {
            AccessModifier::Public => 0,
            AccessModifier::Private => 1,
            AccessModifier::Internal => 2,
        }
    }

    /// Converts [`AccessModifierSyntaxTree`] to [`AccessModifier`].
    pub fn from_syntax_tree(access_modifier_syntax_tree: &AccessModifierSyntaxTree) -> Self {
        match access_modifier_syntax_tree {
            AccessModifierSyntaxTree::Public(_) => AccessModifier::Public,
            AccessModifierSyntaxTree::Private(_) => AccessModifier::Private,
            AccessModifierSyntaxTree::Internal(_) => AccessModifier::Internal,
        }
    }
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
    pub fn map_name_to_index(&self, name: &str) -> Option<usize> { self.map.get(name).copied() }

    /// Returns a reference to the item with the given name.
    pub fn get_by_index(&self, index: usize) -> Option<&T> { self.vec.get(index) }

    /// Returns a mutable reference to the item with the given name.
    pub fn get_mut_by_index(&mut self, index: usize) -> Option<&mut T> { self.vec.get_mut(index) }

    /// Returns a reference to the item with the given name.
    pub fn get_by_name(&self, name: &str) -> Option<&T> {
        self.map_name_to_index(name)
            .and_then(|index| self.get_by_index(index))
    }

    /// Returns a mutable reference to the item with the given name.
    pub fn get_mut_by_name(&mut self, name: &str) -> Option<&mut T> {
        self.map_name_to_index(name)
            .and_then(move |index| self.get_mut_by_index(index))
    }
}

impl<T> Default for VecNameMap<T> {
    fn default() -> Self { Self::new() }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, new)]
pub struct SyntaxTreeWithSource<T> {
    pub source_file: Arc<SourceFile>,
    pub syntax_tree: T,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParameterSymbol {
    pub name: String,
    pub type_binding: TypeBinding,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionItemSymbol {
    pub name: String,
    pub parent: Option<SymbolID>,
    pub access_modifier: AccessModifier,
    pub return_type: Type,
    pub parameters: VecNameMap<ParameterSymbol>,
    pub syntax_tree: SyntaxTreeWithSource<FunctionSyntaxTree>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldSymbol {
    pub access_modifier: AccessModifier,
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructItemSymbol {
    pub name: String,
    pub parent: SymbolID,
    pub access_modifier: AccessModifier,
    pub fields: VecNameMap<FieldSymbol>,
    pub syntax_tree: SyntaxTreeWithSource<StructSyntaxTree>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumItemSymbol {
    pub name: String,
    pub parent: SymbolID,
    pub access_modifier: AccessModifier,
    pub variants: HashMap<String, usize>,
    pub syntax_tree: SyntaxTreeWithSource<EnumSyntaxTree>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleItemSymbol {
    pub name: String,
    pub parent: Option<SymbolID>,
    pub children: VecNameMap<SymbolID>,
    pub access_modifier: AccessModifier,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SymbolID(usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemSymbolTable {
    symbols: VecNameMap<ItemSymbol>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Error)]
pub enum ItemSymbolTableCreateError {
    #[error("{0}")]
    SemanticSymbolError(#[from] SemanticSymbolError),
}

impl ItemSymbolTable {
    /// Creates a new [`ItemSymbolTable`] from the given file inputs.
    ///
    /// The passed in iterator over [`FileInput`]s must not contain duplicate module heirarchies.
    pub fn analyze(
        files: impl Iterator<Item = FileParsing>,
    ) -> (ItemSymbolTable, Vec<ItemSymbolTableCreateError>) {
        let mut symbols = VecNameMap::new();
        let mut errors = Vec::new();
        let files = files.collect::<Vec<_>>();

        // generates the module heirarchy
        Self::create_module(&mut symbols, &files);

        let mut files = files
            .into_iter()
            .map(|file_parsing| file_parsing.destruct())
            .collect::<Vec<_>>();

        // populates the enum symbols
        Self::populate_symbol(
            &mut symbols,
            &mut files,
            &mut errors,
            // filter enum
            |syntax_tree| matches!(syntax_tree, ItemSyntaxTree::Enum(_)),
            // get enum name
            |syntax_tree| match syntax_tree {
                ItemSyntaxTree::Enum(enum_syntax_tree) => &enum_syntax_tree.identifier,
                _ => unreachable!(),
            },
            // create enum symbol
            |source_file, parent_symbol_id, _, syntax_tree| {
                let enum_syntax_tree = syntax_tree.into_enum().expect("should be enum");

                ItemSymbol::Enum(EnumItemSymbol {
                    name: source_file[enum_syntax_tree.identifier.span].to_string(),
                    parent: parent_symbol_id,
                    access_modifier: AccessModifier::from_syntax_tree(
                        &enum_syntax_tree.access_modifier,
                    ),
                    variants: match &enum_syntax_tree.variants {
                        None => HashMap::new(),
                        Some(syntax_variants) => {
                            let mut variants = HashMap::new();

                            // map name to value
                            for (idx, variant) in syntax_variants.elements().enumerate() {
                                variants.insert(source_file[variant.span].to_string(), idx);
                            }

                            variants
                        }
                    },
                    syntax_tree: SyntaxTreeWithSource::new(source_file.clone(), enum_syntax_tree),
                })
            },
        );

        // populate the struct symbols
        Self::populate_symbol(
            &mut symbols,
            &mut files,
            &mut errors,
            // filter struct
            |syntax_tree| matches!(syntax_tree, ItemSyntaxTree::Struct(_)),
            // get struct name
            |syntax_tree| match syntax_tree {
                ItemSyntaxTree::Struct(struct_syntax_tree) => &struct_syntax_tree.identifier,
                _ => unreachable!(),
            },
            // create struct symbol
            |source_file, parent_symbol_id, _, syntax_tree| {
                let struct_syntax_tree = syntax_tree.into_struct().expect("should be struct");

                ItemSymbol::Struct(StructItemSymbol {
                    name: source_file[struct_syntax_tree.identifier.span].to_string(),
                    parent: parent_symbol_id,
                    access_modifier: AccessModifier::from_syntax_tree(
                        &struct_syntax_tree.access_modifier,
                    ),
                    fields: VecNameMap::default(), // will be added later
                    syntax_tree: SyntaxTreeWithSource::new(source_file.clone(), struct_syntax_tree),
                })
            },
        );

        (ItemSymbolTable { symbols }, errors)
    }

    fn populate_symbol(
        symbols: &mut VecNameMap<ItemSymbol>,
        files: &mut Vec<(Arc<SourceFile>, FileSyntaxTree, Vec<FileParsingError>)>,
        errors: &mut Vec<ItemSymbolTableCreateError>,
        syntax_tree_predicate: impl Fn(&ItemSyntaxTree) -> bool,
        syntax_tree_identifier_getter: impl Fn(&ItemSyntaxTree) -> &IdentifierToken,
        symbol_creator: impl Fn(
            Arc<SourceFile>,             // the source file
            SymbolID,                    // the parent symbol id
            &mut VecNameMap<ItemSymbol>, // the symbol table
            ItemSyntaxTree,              // the syntax tree
        ) -> ItemSymbol,
    ) {
        for (source_file, file_syntax_tree, _) in files {
            let mut index = 0;

            // gets all the syntax tree that satisfies the predicate
            while index < file_syntax_tree.items.len() {
                let item_syntax_tree = if syntax_tree_predicate(&file_syntax_tree.items[index]) {
                    file_syntax_tree.items.remove(index)
                } else {
                    index += 1;
                    continue;
                };

                // symbol redefinition error
                {
                    let identifier_token = syntax_tree_identifier_getter(&item_syntax_tree);
                    if let Some(declared_symbol_id) = symbols
                        .get_mut_by_name(source_file.module_qualified_name())
                        .expect("should exist")
                        .as_module_mut()
                        .expect("should be module")
                        .children
                        .get_by_name(&source_file[identifier_token.span])
                    {
                        errors.push(
                            SemanticSymbolError::SymbolRedifinition(SymbolRedifinition {
                                available_symbol: *declared_symbol_id,
                                new_symbol_span: SourceSpan::new(
                                    source_file.clone(),
                                    identifier_token.span,
                                ),
                            })
                            .into(),
                        );
                        continue;
                    }
                }

                // module symbol id
                let parent_symbol_id = SymbolID(
                    symbols
                        .map_name_to_index(source_file.module_qualified_name())
                        .expect("should exist"),
                );

                // create symbol
                let symbol = symbol_creator(
                    source_file.clone(),
                    parent_symbol_id,
                    symbols,
                    item_syntax_tree,
                );
                let symbol_name = symbol.name().to_string();

                // add symbol to module
                let symbol_id = symbols
                    .add_item(
                        format!("{}::{}", source_file.module_qualified_name(), &symbol_name),
                        symbol,
                    )
                    .expect("should not duplicate");

                // add to the module
                symbols
                    .get_mut_by_name(source_file.module_qualified_name())
                    .expect("should exist")
                    .as_module_mut()
                    .expect("should be module")
                    .children
                    .add_item(symbol_name, SymbolID(symbol_id))
                    .expect("should not duplicate");
            }
        }
    }

    fn create_module(symbols: &mut VecNameMap<ItemSymbol>, files: &[FileParsing]) {
        let mut current_module_full_name = String::new();

        for file in files {
            let mut parent_module: Option<usize> = None;
            current_module_full_name.clear();

            for (index, module_name) in file.source_file().module_heirarchy().iter().enumerate() {
                if index == 0 {
                    current_module_full_name = module_name.to_string();
                } else {
                    current_module_full_name.push_str("::");
                    current_module_full_name.push_str(module_name);
                }

                if let Some(parent_module_idx) = parent_module {
                    // module heirarchy should not duplicate
                    if let Some(children_idx) = symbols
                        .get_mut_by_index(parent_module_idx)
                        .expect("should exist")
                        .as_module_mut()
                        .expect("should be module")
                        .children
                        .get_by_name(module_name)
                    {
                        parent_module = Some(children_idx.0);
                    } else {
                        // create the module
                        let module_symbol = ItemSymbol::Module(ModuleItemSymbol {
                            name: module_name.to_string(),
                            parent: Some(SymbolID(parent_module_idx)),
                            children: VecNameMap::new(),
                            access_modifier: AccessModifier::Public,
                        });

                        // add the module to the symbol table
                        let module_idx = symbols
                            .add_item(current_module_full_name.clone(), module_symbol)
                            .expect("should not exist");
                        println!("Added: {current_module_full_name}, {module_idx}");

                        parent_module = Some(module_idx);

                        // add the module to the parent module
                        symbols
                            .get_mut_by_index(parent_module_idx)
                            .expect("should exist")
                            .as_module_mut()
                            .expect("should be module")
                            .children
                            .add_item(module_name.to_string(), SymbolID(module_idx))
                            .expect("should not duplicate");
                    }
                } else {
                    // module heirarchy should not duplicate
                    if let Some(idx) = symbols.map_name_to_index(&current_module_full_name) {
                        parent_module = Some(idx);
                    } else {
                        // create the module
                        let module_symbol = ItemSymbol::Module(ModuleItemSymbol {
                            name: module_name.to_string(),
                            parent: None,
                            children: VecNameMap::new(),
                            access_modifier: AccessModifier::Public,
                        });

                        // add the module to the symbol table
                        let module_idx = symbols
                            .add_item(module_name.to_string(), module_symbol)
                            .expect("should not exist");

                        parent_module = Some(module_idx);
                    }
                }
            }
        }
    }
}
