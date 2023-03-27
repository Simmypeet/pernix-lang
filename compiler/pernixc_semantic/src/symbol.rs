use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::Hash,
    sync::Arc,
};

use derive_new::new;
use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::SourceFile;
use pernixc_lexical::token::Identifier;
use pernixc_syntax::{
    file_parsing::{FileParsing, FileParsingError},
    syntax_tree::{
        item::{AccessModifier, Enum, Function, Item, Struct},
        FileSyntaxTree, PrimitiveTypeSpecifierSyntaxTree, SourceElement, TypeSpecifierSyntaxTree,
    },
};

use self::ty::{PrimitiveType, Type, TypeBinding};
use crate::{
    errors::{
        AccessibilityLeaking, EnumVariantRedefinition, FieldRedefinition, ParameterRedifinition,
        SemanticError, SymbolIsNotAccessible, SymbolNotFound, SymbolRedifinition, TypeExpected,
    },
    SourceSpan,
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
    pub fn rank(self) -> u32 {
        match self {
            AccessModifier::Public => 0,
            AccessModifier::Private => 1,
            AccessModifier::Internal => 2,
        }
    }

    /// Converts [`AccessModifier`] to [`AccessModifier`].
    pub fn from_syntax_tree(access_modifier_syntax_tree: &AccessModifier) -> Self {
        match access_modifier_syntax_tree {
            AccessModifier::Public(..) => AccessModifier::Public,
            AccessModifier::Private(..) => AccessModifier::Private,
            AccessModifier::Internal(..) => AccessModifier::Internal,
        }
    }
}

/// Is an enumeration of all item symbols that can be defined in a namespace.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum GlobalSymbol {
    ModuleSymbol(ModuleSymbol),
    StructSymbol(StructSymbol),
    EnumSymbol(EnumSymbol),
    FunctionSymbol(FunctionSymbol),
    EnumVariantSymbol(EnumVariantSymbol),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EnumVariantSymbol {
    pub name: String,
    pub variant_number: usize,
    pub parent_index: usize,
    pub access_modifier: AccessModifier,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VariableSymbol {
    pub name: String,
    pub type_binding_specifier: TypeBinding,
}

impl GlobalSymbol {
    /// Gets the name of the item.
    pub fn name(&self) -> &str {
        match self {
            GlobalSymbol::ModuleSymbol(symbol) => &symbol.name,
            GlobalSymbol::StructSymbol(symbol) => &symbol.name,
            GlobalSymbol::EnumSymbol(symbol) => &symbol.name,
            GlobalSymbol::FunctionSymbol(symbol) => &symbol.name,
            GlobalSymbol::EnumVariantSymbol(symbol) => &symbol.name,
        }
    }

    /// Gets the access modifier of the tiem.
    pub fn access_modifier(&self) -> AccessModifier {
        match self {
            GlobalSymbol::ModuleSymbol(symbol) => symbol.access_modifier,
            GlobalSymbol::StructSymbol(symbol) => symbol.access_modifier,
            GlobalSymbol::EnumSymbol(symbol) => symbol.access_modifier,
            GlobalSymbol::FunctionSymbol(symbol) => symbol.access_modifier,
            GlobalSymbol::EnumVariantSymbol(symbol) => symbol.access_modifier,
        }
    }

    pub fn parent_index(&self) -> Option<usize> {
        match self {
            GlobalSymbol::ModuleSymbol(symbol) => symbol.parent_index,
            GlobalSymbol::StructSymbol(symbol) => Some(symbol.parent_index),
            GlobalSymbol::EnumSymbol(symbol) => Some(symbol.parent_index),
            GlobalSymbol::FunctionSymbol(symbol) => Some(symbol.parent_index),
            GlobalSymbol::EnumVariantSymbol(symbol) => Some(symbol.parent_index),
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

    /// Returns an iterator over all items in the container.
    pub fn iter(&self) -> impl Iterator<Item = &T> { self.vec.iter() }

    /// Returns a mutable iterator over all items in the container.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> { self.vec.iter_mut() }

    /// Returns the number of items in the container.
    pub fn len(&self) -> usize { self.vec.len() }

    /// Returns `true` if the container is empty.
    pub fn is_empty(&self) -> bool { self.vec.is_empty() }
}

impl<T> Default for VecNameMap<T> {
    fn default() -> Self { Self::new() }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, new)]
pub struct SyntaxTreeWithSource<T> {
    pub source_file: Arc<SourceFile>,
    pub syntax_tree: T,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSymbol {
    pub name: String,
    pub parent: usize,
    pub access_modifier: AccessModifier,
    pub return_type: Type,
    pub parent_index: usize,
    pub parameters: VecNameMap<VariableSymbol>,
    pub syntax_tree: SyntaxTreeWithSource<Function>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldSymbol {
    pub access_modifier: AccessModifier,
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructSymbol {
    pub name: String,
    pub parent_index: usize,
    pub access_modifier: AccessModifier,
    pub fields: VecNameMap<FieldSymbol>,
    pub syntax_tree: SyntaxTreeWithSource<Struct>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumSymbol {
    pub name: String,
    pub parent_index: usize,
    pub access_modifier: AccessModifier,
    pub variant_symbol_indices_by_name: HashMap<String, usize>,
    pub syntax_tree: SyntaxTreeWithSource<Enum>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleSymbol {
    pub name: String,
    pub parent_index: Option<usize>,
    pub children_sybol_indices_by_name: HashMap<String, usize>,
    pub access_modifier: AccessModifier,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalSymbolTable {
    symbols: Vec<GlobalSymbol>,
    root_indices_by_name: HashMap<String, usize>,
}

impl GlobalSymbolTable {
    /// Creates a new [`GlobalSymbolTable`] from the given file inputs.
    pub fn analyze(files: impl Iterator<Item = FileParsing>) -> (Self, Vec<SemanticError>) {
        let mut item_symbol_table = GlobalSymbolTable {
            symbols: Vec::new(),
            root_indices_by_name: HashMap::new(),
        };
        let mut errors = Vec::new();
        let files = files.collect::<Vec<_>>();

        // generates the module heirarchy
        item_symbol_table.generate_module(&files);

        let mut files = files
            .into_iter()
            .map(|file_parsing| file_parsing.destruct())
            .collect::<Vec<_>>();

        // populates the enum symbols
        item_symbol_table.populate_symbol(
            &mut files,
            &mut errors,
            // filter enum
            |syntax_tree| matches!(syntax_tree, Item::Enum(..)),
            // get enum name
            |syntax_tree| match syntax_tree {
                Item::Enum(enum_syntax_tree) => &enum_syntax_tree.identifier,
                _ => unreachable!(),
            },
            // create enum symbol
            |source_file, parent_symbol_index, _, _, syntax_tree| {
                let enum_syntax_tree = syntax_tree.into_enum_syntax_tree().expect("should be enum");

                GlobalSymbol::EnumSymbol(EnumSymbol {
                    name: source_file[enum_syntax_tree.identifier.span].to_string(),
                    parent_index: parent_symbol_index,
                    access_modifier: AccessModifier::from_syntax_tree(
                        &enum_syntax_tree.access_modifier,
                    ),
                    variant_symbol_indices_by_name: HashMap::new(),
                    syntax_tree: SyntaxTreeWithSource::new(source_file.clone(), enum_syntax_tree),
                })
            },
        );

        // populate the struct symbols
        item_symbol_table.populate_symbol(
            &mut files,
            &mut errors,
            // filter struct
            |syntax_tree| matches!(syntax_tree, Item::Struct(..)),
            // get struct name
            |syntax_tree| match syntax_tree {
                Item::Struct(struct_syntax_tree) => &struct_syntax_tree.identifier,
                _ => unreachable!(),
            },
            // create struct symbol
            |source_file, parent_symbol_index, _, _, syntax_tree| {
                let struct_syntax_tree = syntax_tree
                    .into_struct_syntax_tree()
                    .expect("should be struct");

                GlobalSymbol::StructSymbol(StructSymbol {
                    name: source_file[struct_syntax_tree.identifier.span].to_string(),
                    parent_index: parent_symbol_index,
                    access_modifier: AccessModifier::from_syntax_tree(
                        &struct_syntax_tree.access_modifier,
                    ),
                    fields: VecNameMap::default(), // will be added later
                    syntax_tree: SyntaxTreeWithSource::new(source_file.clone(), struct_syntax_tree),
                })
            },
        );

        // populate the function symbols
        item_symbol_table.populate_symbol(
            &mut files,
            &mut errors,
            // filter function
            |syntax_tree| matches!(syntax_tree, Item::Function(..)),
            // get function name
            |syntax_tree| match syntax_tree {
                Item::Function(function_syntax_tree) => &function_syntax_tree.identifier,
                _ => unreachable!(),
            },
            // create function symbol
            |source_file, parent_symbol_index, _, _, syntax_tree| {
                let function_syntax_tree = syntax_tree
                    .into_function_syntax_tree()
                    .expect("should be function");

                GlobalSymbol::FunctionSymbol(FunctionSymbol {
                    name: source_file[function_syntax_tree.identifier.span].to_string(),
                    parent: parent_symbol_index,
                    access_modifier: AccessModifier::from_syntax_tree(
                        &function_syntax_tree.access_modifier,
                    ),
                    return_type: Type::Primitive(PrimitiveType::Void), // Will be determined later
                    parent_index: parent_symbol_index,
                    parameters: VecNameMap::default(), // Will be determined later
                    syntax_tree: SyntaxTreeWithSource::new(
                        source_file.clone(),
                        function_syntax_tree,
                    ),
                })
            },
        );

        // Populate the function return types and parameters
        item_symbol_table.populate_function_data(&mut errors);

        // Populate the struct fields
        item_symbol_table.populate_struct_data(&mut errors);

        // Populate the enum variants
        item_symbol_table.populate_enum_data(&mut errors);

        (item_symbol_table, errors)
    }

    /// Gets the [usize] of the symbol with the given fully qualified name.
    pub fn get_index_by_qualified_name<'a>(
        &self,
        qualified_name: impl Iterator<Item = &'a str>,
    ) -> Option<usize> {
        let mut current_index: Option<usize> = None;

        for name in qualified_name {
            if let Some(index) = current_index {
                let symbol = self.symbols.get(index).expect("must exist");
                match symbol {
                    GlobalSymbol::ModuleSymbol(module) => {
                        if let Some(index) = module.children_sybol_indices_by_name.get(name) {
                            current_index = Some(*index);
                        } else {
                            return None;
                        }
                    }
                    GlobalSymbol::EnumSymbol(enum_symbol) => {
                        if let Some(index) = enum_symbol.variant_symbol_indices_by_name.get(name) {
                            current_index = Some(*index);
                        } else {
                            return None;
                        }
                    }
                    _ => return None,
                }
            } else if let Some(index) = self.root_indices_by_name.get(name) {
                current_index = Some(*index);
            } else {
                return None;
            }
        }

        current_index
    }

    /// Gets a reference to the [GlobalSymbol] with the given fully qualified name.
    pub fn get_by_qualified_name<'a>(
        &self,
        qualified_name: impl Iterator<Item = &'a str>,
    ) -> Option<(usize, &GlobalSymbol)> {
        let symbol_index = self.get_index_by_qualified_name(qualified_name)?;
        Some((
            symbol_index,
            self.get_by_index(symbol_index).expect("should exist"),
        ))
    }

    /// Gets a reference to the [GlobalSymbol] with the given [usize].
    pub fn get_by_index(&self, index: usize) -> Option<&GlobalSymbol> { self.symbols.get(index) }

    /// Applies symbol resolution from the given [referring_site] to the given [qualified_name].
    ///
    /// Returns the [usize] of the symbol if it exists.
    pub fn resolve_symbol<'a>(
        &self,
        parent_index: usize,
        source_file: &Arc<SourceFile>,
        mut qualified_name: impl Iterator<Item = &'a Identifier>,
    ) -> Result<usize, SemanticError> {
        // the closest parent module symbol from the given `parent_index`
        let (parent_module_symbol, parent_module_index) = {
            let mut current_symbol = self
                .symbols
                .get(parent_index)
                .expect("invalid parent index!");
            let mut current_index = parent_index;
            loop {
                // found module, break
                if let GlobalSymbol::ModuleSymbol(module) = current_symbol {
                    break (module, current_index);
                }

                match current_symbol.parent_index() {
                    Some(parent_index) => {
                        current_symbol = self
                            .symbols
                            .get(parent_index)
                            .expect("invalid parent index!");
                        current_index = parent_index;
                    }
                    None => panic!("must have a parent module!"),
                }
            }
        };

        let first_name = qualified_name.next().expect("should have at least one!");

        let (mut current_symbol, mut current_index) = match parent_module_symbol
            .children_sybol_indices_by_name
            .get(&source_file[first_name.span])
        {
            Some(symbol_index) => {
                // start from this symbol
                (
                    self.symbols
                        .get(*symbol_index)
                        .expect("invalid symbol index!"),
                    *symbol_index,
                )
            }
            None => {
                // start from the root
                match self.root_indices_by_name.get(&source_file[first_name.span]) {
                    Some(symbol_index) => (
                        self.symbols.get(*symbol_index).expect("should exist"),
                        *symbol_index,
                    ),
                    None => {
                        return Err(SemanticError::SymbolNotFound(SymbolNotFound {
                            referencing_site: SourceSpan::new(source_file.clone(), first_name.span),
                            in_scope: None,
                        }));
                    }
                }
            }
        };

        // loop through the rest of the identifiers
        loop {
            match qualified_name.next() {
                Some(identifier) => {
                    let name = &source_file[identifier.span];

                    // must be module symbol to continue
                    let child_indices_by_name = match current_symbol {
                        GlobalSymbol::ModuleSymbol(module) => {
                            &module.children_sybol_indices_by_name
                        }
                        GlobalSymbol::EnumSymbol(enum_sym) => {
                            &enum_sym.variant_symbol_indices_by_name
                        }
                        _ => {
                            return Err(SemanticError::SymbolNotFound(SymbolNotFound {
                                referencing_site: SourceSpan::new(
                                    source_file.clone(),
                                    identifier.span,
                                ),
                                in_scope: Some(current_index),
                            }))
                        }
                    };

                    // look for the child symbol in the current module
                    match child_indices_by_name.get(name) {
                        Some(symbol_index) => {
                            let symbol = self.symbols.get(*symbol_index).expect("should exist");

                            // if symbol is defined as private, it must be a child of the parent
                            // module symbol
                            if symbol.access_modifier() == AccessModifier::Private
                                && !self.is_parent(parent_module_index, *symbol_index)
                            {
                                return Err(SemanticError::SymbolIsNotAccessible(
                                    SymbolIsNotAccessible {
                                        referencing_site: SourceSpan::new(
                                            source_file.clone(),
                                            identifier.span,
                                        ),
                                        symbol_index: *symbol_index,
                                    },
                                ));
                            }

                            // continue to the next symbol
                            current_symbol = symbol;
                            current_index = *symbol_index;
                        }
                        None => {
                            // symbol not found
                            return Err(SemanticError::SymbolNotFound(SymbolNotFound {
                                referencing_site: SourceSpan::new(
                                    source_file.clone(),
                                    identifier.span,
                                ),
                                in_scope: Some(current_index),
                            }));
                        }
                    }
                }
                None => break Ok(current_index),
            }
        }
    }
}

impl GlobalSymbolTable {
    fn populate_struct_data(&mut self, errors: &mut Vec<SemanticError>) {
        for index in 0..self.symbols.len() {
            let fields = match self.symbols.get(index).expect("must exist") {
                GlobalSymbol::StructSymbol(struct_symbol) => {
                    let mut fields = VecNameMap::new();

                    for field_group in &struct_symbol.syntax_tree.syntax_tree.field_groups {
                        let group_access_modifier =
                            AccessModifier::from_syntax_tree(&field_group.access_modifier);

                        if group_access_modifier.rank() < struct_symbol.access_modifier.rank() {
                            errors.push(SemanticError::AccessibilityLeaking(AccessibilityLeaking {
                                symbol_span: SourceSpan::new(
                                    struct_symbol.syntax_tree.source_file.clone(),
                                    field_group.access_modifier.span(),
                                ),
                                symbol_access_modifier: group_access_modifier,
                                parent_access_modifier: struct_symbol.access_modifier,
                            }))
                        }

                        for field in &field_group.fields {
                            let ty = match self.get_type(
                                struct_symbol.parent_index,
                                &field.type_specifier,
                                &struct_symbol.syntax_tree.source_file,
                            ) {
                                Ok(ty) => {
                                    if let Type::UserDefinedSymbolIndex(ty_index) = ty {
                                        let ty_sym =
                                            self.symbols.get(ty_index).expect("should exist");

                                        if ty_sym.access_modifier().rank()
                                            > struct_symbol.access_modifier.rank()
                                        {
                                            errors.push(SemanticError::AccessibilityLeaking(
                                                AccessibilityLeaking {
                                                    symbol_span: SourceSpan::new(
                                                        struct_symbol
                                                            .syntax_tree
                                                            .source_file
                                                            .clone(),
                                                        field.type_specifier.span(),
                                                    ),
                                                    symbol_access_modifier: ty_sym
                                                        .access_modifier(),
                                                    parent_access_modifier: struct_symbol
                                                        .access_modifier,
                                                },
                                            ))
                                        }
                                    }
                                    ty
                                }
                                Err(err) => {
                                    errors.push(err);
                                    Type::default()
                                }
                            };

                            let name = struct_symbol.syntax_tree.source_file[field.identifier.span]
                                .to_string();

                            if fields
                                .add_item(name.clone(), FieldSymbol {
                                    access_modifier: group_access_modifier,
                                    name,
                                    ty,
                                })
                                .is_err()
                            {
                                errors.push(SemanticError::FieldRedefinition(FieldRedefinition {
                                    new_field_span: SourceSpan::new(
                                        struct_symbol.syntax_tree.source_file.clone(),
                                        field.identifier.span,
                                    ),
                                }))
                            }
                        }
                    }

                    fields
                }
                _ => continue,
            };

            match self.symbols.get_mut(index).expect("must exist") {
                GlobalSymbol::StructSymbol(struct_symbol) => struct_symbol.fields = fields,
                _ => unreachable!(),
            }
        }
    }

    fn populate_enum_data(&mut self, errors: &mut Vec<SemanticError>) {
        for index in 0..self.symbols.len() {
            let (access_modifier, variants) = match self.symbols.get(index).expect("should exist") {
                GlobalSymbol::EnumSymbol(enum_sym) => (enum_sym.access_modifier, 'ret: {
                    let variants =
                        if let Some(variants) = &enum_sym.syntax_tree.syntax_tree.variants {
                            variants
                        } else {
                            break 'ret Vec::new();
                        };

                    let mut return_variants = Vec::with_capacity(variants.len());
                    let mut inserted_variants = HashSet::with_capacity(variants.len());

                    for variant in variants.elements() {
                        let variant_string =
                            enum_sym.syntax_tree.source_file[variant.span].to_string();

                        if inserted_variants.contains(&variant_string) {
                            errors.push(SemanticError::EnumVariantRedefinition(
                                EnumVariantRedefinition {
                                    new_variant_span: SourceSpan::new(
                                        enum_sym.syntax_tree.source_file.clone(),
                                        variant.span,
                                    ),
                                },
                            ));
                            continue;
                        }

                        inserted_variants.insert(variant_string.clone());
                        return_variants.push(variant_string);
                    }

                    return_variants
                }),
                _ => continue,
            };

            let mut variant_indices_by_name = HashMap::new();
            for (index, variant) in variants.iter().enumerate() {
                variant_indices_by_name.insert(
                    variant.clone(),
                    self.add_symbol(GlobalSymbol::EnumVariantSymbol(EnumVariantSymbol {
                        name: variant.clone(),
                        parent_index: index,
                        access_modifier,
                        variant_number: index,
                    })),
                );
            }

            match self.symbols.get_mut(index).expect("should exist") {
                GlobalSymbol::EnumSymbol(enum_sym) => {
                    enum_sym.variant_symbol_indices_by_name = variant_indices_by_name
                }
                _ => unreachable!(),
            }
        }
    }

    fn get_full_qualified_name(&self, symbol_index: usize) -> String {
        let mut return_string = String::new();
        let mut current_index = symbol_index;

        loop {
            let symbol = self.symbols.get(current_index).expect("must exist");

            return_string.insert_str(0, symbol.name());

            if let Some(parent_index) = symbol.parent_index() {
                return_string.insert_str(0, "::");
                current_index = parent_index;
            } else {
                break;
            }
        }

        return_string
    }

    fn populate_function_data(&mut self, errors: &mut Vec<SemanticError>) {
        for index in 0..self.symbols.len() {
            let (arguments, return_type_specifier) = {
                let function = match self.symbols.get(index).expect("must exist") {
                    GlobalSymbol::FunctionSymbol(func) => func,
                    _ => continue,
                };

                (
                    // get arguments
                    match &function.syntax_tree.syntax_tree.parameters {
                        Some(parameters_syntax) => {
                            let mut parameters = VecNameMap::new();

                            for parameter in parameters_syntax.elements() {
                                let name = function.syntax_tree.source_file
                                    [parameter.identifier.span]
                                    .to_string();

                                let type_binding_specifier = TypeBinding {
                                    ty: match self.get_type(
                                        function.parent_index,
                                        &parameter.type_binding_specifier.type_specifier,
                                        &function.syntax_tree.source_file,
                                    ) {
                                        Ok(type_index) => {
                                            if let Type::UserDefinedSymbolIndex(type_index) =
                                                type_index
                                            {
                                                // accessibility leak
                                                let type_symbol = self
                                                    .symbols
                                                    .get(type_index)
                                                    .expect("should exist");

                                                if type_symbol.access_modifier().rank()
                                                    > function.access_modifier.rank()
                                                {
                                                    errors.push(
                                                        SemanticError::AccessibilityLeaking(
                                                            AccessibilityLeaking {
                                                                symbol_span: SourceSpan::new(
                                                                    function
                                                                        .syntax_tree
                                                                        .source_file
                                                                        .clone(),
                                                                    function
                                                                        .syntax_tree
                                                                        .syntax_tree
                                                                        .type_specifier
                                                                        .span(),
                                                                ),
                                                                symbol_access_modifier: type_symbol
                                                                    .access_modifier(),
                                                                parent_access_modifier: function
                                                                    .access_modifier,
                                                            },
                                                        ),
                                                    )
                                                }
                                            }
                                            type_index
                                        }
                                        Err(err) => {
                                            errors.push(err);
                                            Type::default()
                                        }
                                    },
                                    is_mutable: parameter
                                        .type_binding_specifier
                                        .mutable_keyword
                                        .is_some(),
                                };

                                // parameter redefinition
                                if parameters
                                    .add_item(name.clone(), VariableSymbol {
                                        name,
                                        type_binding_specifier,
                                    })
                                    .is_err()
                                {
                                    errors.push(SemanticError::ParameterRedifinition(
                                        ParameterRedifinition {
                                            new_parameter_span: SourceSpan::new(
                                                function.syntax_tree.source_file.clone(),
                                                parameter.identifier.span(),
                                            ),
                                        },
                                    ))
                                }
                            }

                            parameters
                        }
                        None => VecNameMap::default(),
                    },
                    // get function return type
                    match self.get_type(
                        function.parent_index,
                        &function.syntax_tree.syntax_tree.type_specifier,
                        &function.syntax_tree.source_file,
                    ) {
                        Ok(type_index) => {
                            if let Type::UserDefinedSymbolIndex(type_index) = type_index {
                                // accessibility leak
                                let type_symbol =
                                    self.symbols.get(type_index).expect("should exist");

                                if type_symbol.access_modifier().rank()
                                    > function.access_modifier.rank()
                                {
                                    errors.push(SemanticError::AccessibilityLeaking(
                                        AccessibilityLeaking {
                                            symbol_span: SourceSpan::new(
                                                function.syntax_tree.source_file.clone(),
                                                function
                                                    .syntax_tree
                                                    .syntax_tree
                                                    .type_specifier
                                                    .span(),
                                            ),
                                            symbol_access_modifier: type_symbol.access_modifier(),
                                            parent_access_modifier: function.access_modifier,
                                        },
                                    ))
                                }
                            }
                            type_index
                        }
                        Err(err) => {
                            errors.push(err);
                            Type::default()
                        }
                    },
                )
            };

            // update the function symbol
            match self.symbols.get_mut(index).expect("must exist") {
                GlobalSymbol::FunctionSymbol(func) => {
                    func.parameters = arguments;
                    func.return_type = return_type_specifier;
                }
                _ => unreachable!(),
            }
        }
    }

    fn get_type(
        &self,
        parent_index: usize,
        type_specifier_syntax_tree: &TypeSpecifierSyntaxTree,
        source_file: &Arc<SourceFile>,
    ) -> Result<Type, SemanticError> {
        match type_specifier_syntax_tree {
            TypeSpecifierSyntaxTree::PrimitiveTypeSpecifierSyntaxTree(primitive_type) => {
                Ok(match primitive_type {
                    PrimitiveTypeSpecifierSyntaxTree::Bool(..) => {
                        Type::Primitive(PrimitiveType::Bool)
                    }
                    PrimitiveTypeSpecifierSyntaxTree::Float32(..) => {
                        Type::Primitive(PrimitiveType::Float32)
                    }
                    PrimitiveTypeSpecifierSyntaxTree::Float64(..) => {
                        Type::Primitive(PrimitiveType::Float64)
                    }
                    PrimitiveTypeSpecifierSyntaxTree::Int8(..) => {
                        Type::Primitive(PrimitiveType::Int8)
                    }
                    PrimitiveTypeSpecifierSyntaxTree::Int16(..) => {
                        Type::Primitive(PrimitiveType::Int16)
                    }
                    PrimitiveTypeSpecifierSyntaxTree::Int32(..) => {
                        Type::Primitive(PrimitiveType::Int32)
                    }
                    PrimitiveTypeSpecifierSyntaxTree::Int64(..) => {
                        Type::Primitive(PrimitiveType::Int64)
                    }
                    PrimitiveTypeSpecifierSyntaxTree::Uint8(..) => {
                        Type::Primitive(PrimitiveType::Uint8)
                    }
                    PrimitiveTypeSpecifierSyntaxTree::Uint16(..) => {
                        Type::Primitive(PrimitiveType::Uint16)
                    }
                    PrimitiveTypeSpecifierSyntaxTree::Uint32(..) => {
                        Type::Primitive(PrimitiveType::Uint32)
                    }
                    PrimitiveTypeSpecifierSyntaxTree::Uint64(..) => {
                        Type::Primitive(PrimitiveType::Uint64)
                    }
                    PrimitiveTypeSpecifierSyntaxTree::Void(..) => {
                        Type::Primitive(PrimitiveType::Void)
                    }
                })
            }
            TypeSpecifierSyntaxTree::QualifiedIdentifierSyntaxTree(qualified_indexentifier) => {
                let symbol_index = self.resolve_symbol(
                    parent_index,
                    source_file,
                    qualified_indexentifier.elements(),
                )?;

                match self.symbols.get(symbol_index).expect("must exist") {
                    GlobalSymbol::StructSymbol(..) | GlobalSymbol::EnumSymbol(..) => {
                        Ok(Type::UserDefinedSymbolIndex(symbol_index))
                    }
                    _ => Err(SemanticError::TypeExpected(TypeExpected {
                        span: SourceSpan::new(
                            source_file.clone(),
                            type_specifier_syntax_tree.span(),
                        ),
                        symbol_index,
                    })),
                }
            }
        }
    }

    /// Check if `child` is a child of `parent` or not
    fn is_parent(&self, parent: usize, child: usize) -> bool {
        let mut current_symbol = self.symbols.get(child).expect("invalid child index!");

        loop {
            match current_symbol.parent_index() {
                Some(parent_index) => {
                    if parent_index == parent {
                        return true;
                    }

                    current_symbol = self
                        .symbols
                        .get(parent_index)
                        .expect("invalid parent index!");
                }
                None => return false,
            }
        }
    }

    fn add_symbol(&mut self, symbol: GlobalSymbol) -> usize {
        let index = self.symbols.len();
        self.symbols.push(symbol);
        index
    }

    fn populate_symbol(
        &mut self,
        files: &mut Vec<(Arc<SourceFile>, FileSyntaxTree, Vec<FileParsingError>)>,
        errors: &mut Vec<SemanticError>,
        syntax_tree_predicate: impl Fn(&Item) -> bool,
        syntax_tree_indexentifier_getter: impl Fn(&Item) -> &Identifier,
        symbol_creator: impl Fn(
            Arc<SourceFile>,         // the source file
            usize,                   // the parent symbol index
            &mut Self,               // the symbol table
            &mut Vec<SemanticError>, // the error list for reporting the errors
            Item,                    // the syntax tree
        ) -> GlobalSymbol,
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
                    let identifier_token = syntax_tree_indexentifier_getter(&item_syntax_tree);
                    if let Some(declared_symbol_index) = self
                        .get_index_by_qualified_name(
                            source_file.module_heirarchy().iter().map(AsRef::as_ref),
                        )
                        .map(|idx| self.symbols.get_mut(idx).expect("should exist"))
                        .expect("should exist")
                        .as_module_symbol_mut()
                        .expect("should be module")
                        .children_sybol_indices_by_name
                        .get(&source_file[identifier_token.span])
                    {
                        errors.push(SemanticError::SymbolRedifinition(SymbolRedifinition {
                            available_symbol_index: *declared_symbol_index,
                            new_symbol_span: SourceSpan::new(
                                source_file.clone(),
                                identifier_token.span,
                            ),
                        }));
                        continue;
                    }
                }

                // module symbol index
                let parent_symbol_index = self
                    .get_index_by_qualified_name(
                        source_file.module_heirarchy().iter().map(AsRef::as_ref),
                    )
                    .expect("should exist");

                // create symbol
                let symbol = symbol_creator(
                    source_file.clone(),
                    parent_symbol_index,
                    self,
                    errors,
                    item_syntax_tree,
                );
                let symbol_name = symbol.name().to_string();

                // add symbol to module
                let symbol_index = self.add_symbol(symbol);

                // add to the module
                self.symbols
                    .get_mut(parent_symbol_index)
                    .expect("should exist")
                    .as_module_symbol_mut()
                    .expect("should be module")
                    .children_sybol_indices_by_name
                    .insert(symbol_name, symbol_index);
            }
        }
    }

    fn generate_module(&mut self, files: &[FileParsing]) {
        for file in files {
            let mut parent_module: Option<usize> = None;

            for module_name in file.source_file().module_heirarchy() {
                if let Some(parent_module_index) = parent_module {
                    // module heirarchy should not duplicate
                    if let Some(children_indexx) = self
                        .symbols
                        .get_mut(parent_module_index)
                        .expect("should exist")
                        .as_module_symbol_mut()
                        .expect("should be module")
                        .children_sybol_indices_by_name
                        .get(module_name)
                    {
                        parent_module = Some(*children_indexx);
                    } else {
                        // create the module
                        let module_symbol = GlobalSymbol::ModuleSymbol(ModuleSymbol {
                            name: module_name.to_string(),
                            parent_index: Some(parent_module_index),
                            children_sybol_indices_by_name: HashMap::new(),
                            access_modifier: AccessModifier::Public,
                        });

                        // add the module to the symbol table
                        let module_index = self.symbols.len();
                        self.symbols.push(module_symbol);

                        parent_module = Some(module_index);

                        // add the module to the parent module
                        self.symbols
                            .get_mut(parent_module_index)
                            .expect("should exist")
                            .as_module_symbol_mut()
                            .expect("should be module")
                            .children_sybol_indices_by_name
                            .insert(module_name.to_string(), module_index);
                    }
                } else {
                    // module heirarchy should not duplicate
                    if let Some(idx) = self.root_indices_by_name.get(module_name) {
                        parent_module = Some(*idx);
                    } else {
                        // create the module
                        let module_symbol = GlobalSymbol::ModuleSymbol(ModuleSymbol {
                            name: module_name.to_string(),
                            parent_index: None,
                            children_sybol_indices_by_name: HashMap::new(),
                            access_modifier: AccessModifier::Public,
                        });

                        // add the module to the symbol table
                        let module_index = self.symbols.len();
                        self.symbols.push(module_symbol);

                        // add to the root
                        self.root_indices_by_name
                            .insert(module_name.to_string(), module_index);

                        parent_module = Some(module_index);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests;
