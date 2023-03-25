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
        FileSyntaxTree, PrimitiveTypeSpecifierSyntaxTree, QualifiedIdentifierSyntaxTree,
        SyntaxTree, TypeSpecifierSyntaxTree,
    },
};

use self::ty::{PrimitiveType, Type, TypeBinding};
use crate::{
    errors::{
        AccessibilityLeaking, FieldRedefinition, ParameterRedifinition, SemanticError,
        SymbolIsNotAccessible, SymbolNotFound, SymbolRedifinition, TypeExpected,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParameterSymbol {
    pub name: String,
    pub type_binding: TypeBinding,
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

    /// Gets the access modifier of the tiem.
    pub fn access_modifier(&self) -> AccessModifier {
        match self {
            ItemSymbol::Module(symbol) => symbol.access_modifier,
            ItemSymbol::Struct(symbol) => symbol.access_modifier,
            ItemSymbol::Enum(symbol) => symbol.access_modifier,
            ItemSymbol::Function(symbol) => symbol.access_modifier,
        }
    }

    pub fn parent_index(&self) -> Option<usize> {
        match self {
            ItemSymbol::Module(symbol) => symbol.parent_index,
            ItemSymbol::Struct(symbol) => Some(symbol.parent_index),
            ItemSymbol::Enum(symbol) => Some(symbol.parent_index),
            ItemSymbol::Function(symbol) => Some(symbol.parent_index),
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
pub struct FunctionItemSymbol {
    pub name: String,
    pub parent: usize,
    pub access_modifier: AccessModifier,
    pub return_type: Type,
    pub parent_index: usize,
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
    pub parent_index: usize,
    pub access_modifier: AccessModifier,
    pub fields: VecNameMap<FieldSymbol>,
    pub syntax_tree: SyntaxTreeWithSource<StructSyntaxTree>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumItemSymbol {
    pub name: String,
    pub parent_index: usize,
    pub access_modifier: AccessModifier,
    pub variants: HashMap<String, usize>,
    pub syntax_tree: SyntaxTreeWithSource<EnumSyntaxTree>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleItemSymbol {
    pub name: String,
    pub parent_index: Option<usize>,
    pub name_children_index_map: HashMap<String, usize>,
    pub access_modifier: AccessModifier,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemSymbolTable {
    symbols: VecNameMap<ItemSymbol>,
}

impl ItemSymbolTable {
    /// Creates a new [`ItemSymbolTable`] from the given file inputs.
    ///
    /// The passed in iterator over [`FileInput`]s must not contain duplicate module heirarchies.
    pub fn analyze(files: impl Iterator<Item = FileParsing>) -> (Self, Vec<SemanticError>) {
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
            |source_file, parent_symbol_index, _, _, syntax_tree| {
                let enum_syntax_tree = syntax_tree.into_enum().expect("should be enum");

                ItemSymbol::Enum(EnumItemSymbol {
                    name: source_file[enum_syntax_tree.identifier.span].to_string(),
                    parent_index: parent_symbol_index,
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
            |source_file, parent_symbol_index, _, _, syntax_tree| {
                let struct_syntax_tree = syntax_tree.into_struct().expect("should be struct");

                ItemSymbol::Struct(StructItemSymbol {
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
        Self::populate_symbol(
            &mut symbols,
            &mut files,
            &mut errors,
            // filter function
            |syntax_tree| matches!(syntax_tree, ItemSyntaxTree::Function(_)),
            // get function name
            |syntax_tree| match syntax_tree {
                ItemSyntaxTree::Function(function_syntax_tree) => &function_syntax_tree.identifier,
                _ => unreachable!(),
            },
            // create function symbol
            |source_file, parent_symbol_index, _, _, syntax_tree| {
                let function_syntax_tree = syntax_tree.into_function().expect("should be function");

                ItemSymbol::Function(FunctionItemSymbol {
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
        Self::populate_function_data(&mut symbols, &mut errors);

        // Populate the struct fields
        Self::populate_struct_data(&mut symbols, &mut errors);

        (Self { symbols }, errors)
    }

    /// Gets the [usize] of the symbol with the given fully qualified name.
    pub fn get_symbol_index_by_qualified_name<'a>(
        &self,
        qualified_name: impl Iterator<Item = &'a str>,
    ) -> Option<usize> {
        let mut current_index: Option<usize> = None;

        for name in qualified_name {
            if let Some(index) = current_index {
                let symbol = self.symbols.get_by_index(index).expect("must exist");
                match symbol {
                    ItemSymbol::Module(module) => {
                        if let Some(index) = module.name_children_index_map.get(name) {
                            current_index = Some(*index);
                        } else {
                            return None;
                        }
                    }
                    _ => return None,
                }
            } else if let Some(index) = self.symbols.map_name_to_index(name) {
                current_index = Some(index);
            } else {
                return None;
            }
        }

        current_index
    }

    /// Gets a reference to the [ItemSymbol] with the given fully qualified name.
    pub fn get_symbol_by_full_qualified_name<'a>(
        &self,
        qualified_name: impl Iterator<Item = &'a str>,
    ) -> Option<(usize, &ItemSymbol)> {
        let symbol_index = self.get_symbol_index_by_qualified_name(qualified_name)?;
        Some((
            symbol_index,
            self.get_symbol_by_index(symbol_index)
                .expect("should exist"),
        ))
    }

    /// Gets a reference to the [ItemSymbol] with the given [usize].
    pub fn get_symbol_by_index(&self, index: usize) -> Option<&ItemSymbol> {
        self.symbols.get_by_index(index)
    }
}

impl ItemSymbolTable {
    fn populate_struct_data(symbols: &mut VecNameMap<ItemSymbol>, errors: &mut Vec<SemanticError>) {
        for index in 0..symbols.len() {
            let fields = match symbols.get_by_index(index).expect("must exist") {
                ItemSymbol::Struct(struct_symbol) => {
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
                            let ty = match Self::get_type(
                                symbols,
                                struct_symbol.parent_index,
                                &field.type_specifier,
                                &struct_symbol.syntax_tree.source_file,
                            ) {
                                Ok(ty) => {
                                    if let Type::UserDefinedSymbolIndex(ty_index) = ty {
                                        let ty_sym =
                                            symbols.get_by_index(ty_index).expect("should exist");

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

            match symbols.get_mut_by_index(index).expect("must exist") {
                ItemSymbol::Struct(struct_symbol) => struct_symbol.fields = fields,
                _ => unreachable!(),
            }
        }
    }

    fn populate_function_data(
        symbols: &mut VecNameMap<ItemSymbol>,
        errors: &mut Vec<SemanticError>,
    ) {
        for index in 0..symbols.len() {
            let (arguments, return_type_specifier) = {
                let function = match symbols.get_by_index(index).expect("must exist") {
                    ItemSymbol::Function(func) => func,
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

                                let type_binding = TypeBinding {
                                    ty: match Self::get_type(
                                        symbols,
                                        function.parent_index,
                                        &parameter.type_binding.type_specifier,
                                        &function.syntax_tree.source_file,
                                    ) {
                                        Ok(type_index) => {
                                            if let Type::UserDefinedSymbolIndex(type_index) =
                                                type_index
                                            {
                                                // accessibility leak
                                                let type_symbol = symbols
                                                    .get_by_index(type_index)
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
                                    is_mutable: parameter.type_binding.mutable_keyword.is_some(),
                                };

                                // parameter redefinition
                                if parameters
                                    .add_item(name.clone(), ParameterSymbol { name, type_binding })
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
                    match Self::get_type(
                        symbols,
                        function.parent_index,
                        &function.syntax_tree.syntax_tree.type_specifier,
                        &function.syntax_tree.source_file,
                    ) {
                        Ok(type_index) => {
                            if let Type::UserDefinedSymbolIndex(type_index) = type_index {
                                // accessibility leak
                                let type_symbol =
                                    symbols.get_by_index(type_index).expect("should exist");

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
            match symbols.get_mut_by_index(index).expect("must exist") {
                ItemSymbol::Function(func) => {
                    func.parameters = arguments;
                    func.return_type = return_type_specifier;
                }
                _ => unreachable!(),
            }
        }
    }

    fn get_type(
        symbols: &VecNameMap<ItemSymbol>,
        parent_index: usize,
        type_specifier_syntax_tree: &TypeSpecifierSyntaxTree,
        source_file: &Arc<SourceFile>,
    ) -> Result<Type, SemanticError> {
        match type_specifier_syntax_tree {
            TypeSpecifierSyntaxTree::Primitive(primitive_type) => Ok(match primitive_type {
                PrimitiveTypeSpecifierSyntaxTree::Bool(_) => Type::Primitive(PrimitiveType::Bool),
                PrimitiveTypeSpecifierSyntaxTree::Float32(_) => {
                    Type::Primitive(PrimitiveType::Float32)
                }
                PrimitiveTypeSpecifierSyntaxTree::Float64(_) => {
                    Type::Primitive(PrimitiveType::Float64)
                }
                PrimitiveTypeSpecifierSyntaxTree::Int8(_) => Type::Primitive(PrimitiveType::Int8),
                PrimitiveTypeSpecifierSyntaxTree::Int16(_) => Type::Primitive(PrimitiveType::Int16),
                PrimitiveTypeSpecifierSyntaxTree::Int32(_) => Type::Primitive(PrimitiveType::Int32),
                PrimitiveTypeSpecifierSyntaxTree::Int64(_) => Type::Primitive(PrimitiveType::Int64),
                PrimitiveTypeSpecifierSyntaxTree::Uint8(_) => Type::Primitive(PrimitiveType::Uint8),
                PrimitiveTypeSpecifierSyntaxTree::Uint16(_) => {
                    Type::Primitive(PrimitiveType::Uint16)
                }
                PrimitiveTypeSpecifierSyntaxTree::Uint32(_) => {
                    Type::Primitive(PrimitiveType::Uint32)
                }
                PrimitiveTypeSpecifierSyntaxTree::Uint64(_) => {
                    Type::Primitive(PrimitiveType::Uint64)
                }
                PrimitiveTypeSpecifierSyntaxTree::Void(_) => Type::Primitive(PrimitiveType::Void),
            }),
            TypeSpecifierSyntaxTree::Qualified(qualified_indexentifier) => {
                let symbol_index = Self::refer_symbol(
                    symbols,
                    parent_index,
                    qualified_indexentifier,
                    source_file,
                )?;

                match symbols.get_by_index(symbol_index).expect("must exist") {
                    ItemSymbol::Struct(_) | ItemSymbol::Enum(_) => {
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

    fn refer_symbol(
        symbols: &VecNameMap<ItemSymbol>,
        parent_index: usize,
        qualified_indexentifier_syntax_tree: &QualifiedIdentifierSyntaxTree,
        source_file: &Arc<SourceFile>,
    ) -> Result<usize, SemanticError> {
        // the closest parent module symbol from the given `parent_index`
        let (parent_module_symbol, parent_module_index) = {
            let mut current_symbol = symbols
                .get_by_index(parent_index)
                .expect("invalid parent index!");
            let mut current_index = parent_index;
            loop {
                // found module, break
                if let ItemSymbol::Module(module) = current_symbol {
                    break (module, current_index);
                }

                match current_symbol.parent_index() {
                    Some(parent_index) => {
                        current_symbol = symbols
                            .get_by_index(parent_index)
                            .expect("invalid parent index!");
                        current_index = parent_index;
                    }
                    None => panic!("must have a parent module!"),
                }
            }
        };

        let mut iter = qualified_indexentifier_syntax_tree.elements();
        let first_name = iter.next().expect("should have at least one!");

        let (mut current_symbol, mut current_index) = match parent_module_symbol
            .name_children_index_map
            .get(&source_file[first_name.span])
        {
            Some(symbol_index) => {
                // start from this symbol
                (
                    symbols
                        .get_by_index(*symbol_index)
                        .expect("invalid symbol index!"),
                    *symbol_index,
                )
            }
            None => {
                // start from the root
                match symbols.map_name_to_index(&source_file[first_name.span]) {
                    Some(symbol_index) => (
                        symbols.get_by_index(symbol_index).expect("should exist"),
                        symbol_index,
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
            match iter.next() {
                Some(identifier) => {
                    let name = &source_file[identifier.span];

                    // must be module symbol to continue
                    let current_module = match current_symbol {
                        ItemSymbol::Module(module) => module,
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
                    match current_module.name_children_index_map.get(name) {
                        Some(symbol_index) => {
                            let symbol = symbols.get_by_index(*symbol_index).expect("should exist");

                            // if symbol is defined as private, it must be a child of the parent
                            // module symbol
                            if symbol.access_modifier() == AccessModifier::Private
                                && !Self::is_parent(symbols, parent_module_index, *symbol_index)
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

    /// Check if `child` is a child of `parent` or not
    fn is_parent(symbols: &VecNameMap<ItemSymbol>, parent: usize, child: usize) -> bool {
        let mut current_symbol = symbols.get_by_index(child).expect("invalid child index!");

        loop {
            match current_symbol.parent_index() {
                Some(parent_index) => {
                    if parent_index == parent {
                        return true;
                    }

                    current_symbol = symbols
                        .get_by_index(parent_index)
                        .expect("invalid parent index!");
                }
                None => return false,
            }
        }
    }

    fn populate_symbol(
        symbols: &mut VecNameMap<ItemSymbol>,
        files: &mut Vec<(Arc<SourceFile>, FileSyntaxTree, Vec<FileParsingError>)>,
        errors: &mut Vec<SemanticError>,
        syntax_tree_predicate: impl Fn(&ItemSyntaxTree) -> bool,
        syntax_tree_indexentifier_getter: impl Fn(&ItemSyntaxTree) -> &IdentifierToken,
        symbol_creator: impl Fn(
            Arc<SourceFile>,             // the source file
            usize,                       // the parent symbol index
            &mut VecNameMap<ItemSymbol>, // the symbol table
            &mut Vec<SemanticError>,     // the error list for reporting the errors
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
                    let identifier_token = syntax_tree_indexentifier_getter(&item_syntax_tree);
                    if let Some(declared_symbol_index) = symbols
                        .get_mut_by_name(source_file.module_qualified_name())
                        .expect("should exist")
                        .as_module_mut()
                        .expect("should be module")
                        .name_children_index_map
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
                let parent_symbol_index = symbols
                    .map_name_to_index(source_file.module_qualified_name())
                    .expect("should exist");

                // create symbol
                let symbol = symbol_creator(
                    source_file.clone(),
                    parent_symbol_index,
                    symbols,
                    errors,
                    item_syntax_tree,
                );
                let symbol_name = symbol.name().to_string();

                // add symbol to module
                let symbol_index = symbols
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
                    .name_children_index_map
                    .insert(symbol_name, symbol_index);
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

                if let Some(parent_module_indexx) = parent_module {
                    // module heirarchy should not duplicate
                    if let Some(children_indexx) = symbols
                        .get_mut_by_index(parent_module_indexx)
                        .expect("should exist")
                        .as_module_mut()
                        .expect("should be module")
                        .name_children_index_map
                        .get(module_name)
                    {
                        parent_module = Some(*children_indexx);
                    } else {
                        // create the module
                        let module_symbol = ItemSymbol::Module(ModuleItemSymbol {
                            name: module_name.to_string(),
                            parent_index: Some(parent_module_indexx),
                            name_children_index_map: HashMap::new(),
                            access_modifier: AccessModifier::Public,
                        });

                        // add the module to the symbol table
                        let module_indexx = symbols
                            .add_item(current_module_full_name.clone(), module_symbol)
                            .expect("should not exist");

                        parent_module = Some(module_indexx);

                        // add the module to the parent module
                        symbols
                            .get_mut_by_index(parent_module_indexx)
                            .expect("should exist")
                            .as_module_mut()
                            .expect("should be module")
                            .name_children_index_map
                            .insert(module_name.to_string(), module_indexx);
                    }
                } else {
                    // module heirarchy should not duplicate
                    if let Some(idx) = symbols.map_name_to_index(&current_module_full_name) {
                        parent_module = Some(idx);
                    } else {
                        // create the module
                        let module_symbol = ItemSymbol::Module(ModuleItemSymbol {
                            name: module_name.to_string(),
                            parent_index: None,
                            name_children_index_map: HashMap::new(),
                            access_modifier: AccessModifier::Public,
                        });

                        // add the module to the symbol table
                        let module_indexx = symbols
                            .add_item(module_name.to_string(), module_symbol)
                            .expect("should not exist");

                        parent_module = Some(module_indexx);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests;
