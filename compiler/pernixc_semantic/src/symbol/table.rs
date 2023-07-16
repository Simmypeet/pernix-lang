//! Contains the definition of [`Table`].

use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::Identifier as IdentifierToken;
use pernixc_source::SourceElement;
use pernixc_syntax::{
    syntax_tree::{
        item::{
            Enum as EnumSyntaxTree, Field as FieldSyntaxTree, Function as FunctionSyntaxTree,
            Item as ItemSyntaxTree, Member as MemberSyntaxTree, Parameter as ParameterSyntaxTree,
            Struct as StructSyntaxTree, TypeAlias as TypeAliasSyntaxTree,
        },
        ConnectedList, PrimitiveTypeSpecifier, QualifiedIdentifier, TypeSpecifier,
    },
    target_parsing::{FileParsing, TargetParsing},
};
use pernixc_system::{
    arena::{Arena, InvalidIDError},
    error_handler::ErrorHandler,
};
use thiserror::Error;

use super::{
    error::{
        CircularDependency, EnumVariantRedefinition, Error, FieldRedefinition,
        OverloadRedefinition, ParameterRedefinition, PrivateSymbolLeak, RecursiveType,
        StructMemberMoreAccessibleThanStruct, SymbolNotAccessible, SymbolNotFound,
        SymbolRedifinition, TypeExpected,
    },
    ty::{PrimitiveType, Type, TypeBinding},
    Accessibility, Enum, EnumID, EnumSymbol, EnumVariant, EnumVariantID, EnumVariantSymbol, Field,
    FieldID, FieldSymbol, GlobalID, Module, ModuleID, ModuleSymbol, Overload, OverloadID,
    OverloadSet, OverloadSetID, OverloadSetSymbol, OverloadSymbol, OverloadSyntaxTree, Parameter,
    ParameterID, ParameterSymbol, ScopedID, Struct, StructID, StructSymbol, TypeAlias, TypeAliasID,
    TypeAliasParentID, TypeAliasSymbol, ID,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum SymbolState {
    Drafted,
    Constructing,
}

/// Is a symbol table for the compiler.
#[derive(Debug)]
pub struct Table {
    modules: Arena<Module>,
    structs: Arena<Struct>,
    enums: Arena<Enum>,
    enum_variants: Arena<EnumVariant>,
    type_aliases: Arena<TypeAlias>,
    overload_sets: Arena<OverloadSet>,
    overloads: Arena<Overload>,
    fields: Arena<Field>,
    parameters: Arena<Parameter>,

    root_module_ids_by_name: HashMap<String, ModuleID>,
}

impl Table {
    /// Returns an iterator over all [`ModuleSymbol`] in the table.
    pub fn modules(&self) -> impl Iterator<Item = &ModuleSymbol> { self.modules.values() }

    /// Returns an iterator over all [`StructSymbol`] in the table.
    pub fn structs(&self) -> impl Iterator<Item = &StructSymbol> { self.structs.values() }

    /// Returns an iterator over all [`EnumSymbol`] in the table.
    pub fn enums(&self) -> impl Iterator<Item = &EnumSymbol> { self.enums.values() }

    /// Returns an iterator over all [`EnumVariantSymbol`] in the table.
    pub fn enum_variants(&self) -> impl Iterator<Item = &EnumVariantSymbol> {
        self.enum_variants.values()
    }

    /// Returns an iterator over all [`TypeAliasSymbol`] in the table.
    pub fn type_aliases(&self) -> impl Iterator<Item = &TypeAliasSymbol> {
        self.type_aliases.values()
    }

    /// Returns an iterator over all [`OverloadSetSymbol`] in the table.
    pub fn overload_sets(&self) -> impl Iterator<Item = &OverloadSetSymbol> {
        self.overload_sets.values()
    }

    /// Returns an iterator over all [`OverloadSymbol`] in the table.
    pub fn overloads(&self) -> impl Iterator<Item = &OverloadSymbol> { self.overloads.values() }

    /// Returns an iterator over all [`FeildSymbol`] in the table.
    pub fn fields(&self) -> impl Iterator<Item = &FieldSymbol> { self.fields.values() }

    /// Returns an iterator over all [`ParameterSymbol`] in the table.
    pub fn parameters(&self) -> impl Iterator<Item = &ParameterSymbol> { self.parameters.values() }
}

/// Is an error that can occur when resolving a symbol in the [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From, Error)]
#[allow(missing_docs)]
pub enum ResolveError {
    #[error("Encountered a fatal symbol error, the resolution must be aborted.")]
    FatalSymbolError,

    #[error("{0}")]
    InvalidIDError(InvalidIDError),
}

impl Table {
    /// Analyzes the syntax trees of the given target and returns the symbol table containing
    /// all the symbols and the errors that occurred during the analysis.
    ///
    /// # Error Handling
    /// When encountering an error, the analysis will not halt completely, but instead continue to
    /// analyze the rest of the code while producing the suboptimal symbol table. These are the
    /// expected behavior of the errors:
    ///
    /// - Symbol Redefinition: One of the definitions will be used in the symbol table, the other
    ///   will be discarded and an error will be produced.
    /// - Symbol Not Found: If the symbol is treated as a type, the type will be defaulted to
    ///   `void`.
    /// - Symbol Not Accessible: The symbol will be treated as if it was accessible and an error
    ///   will be produced (non-fatal error).
    pub fn analyze(target_parsing: TargetParsing, handler: &impl ErrorHandler<Error>) -> Self {
        let mut table = Self {
            modules: Arena::new(),
            structs: Arena::new(),
            enums: Arena::new(),
            enum_variants: Arena::new(),
            type_aliases: Arena::new(),
            overload_sets: Arena::new(),
            overloads: Arena::new(),
            fields: Arena::new(),
            parameters: Arena::new(),

            root_module_ids_by_name: HashMap::new(),
        };

        let mut symbol_states_by_id = HashMap::new();

        table.generate_modules(&target_parsing);
        table.draft_symbols(target_parsing, &mut symbol_states_by_id, handler);

        // build the symbol in the symbol_states_by_id one by one
        while !symbol_states_by_id.is_empty() {
            let id = *symbol_states_by_id
                .iter()
                .find(|(_, state)| **state == SymbolState::Drafted)
                .unwrap()
                .0;

            table.construct_symbol(id, &mut symbol_states_by_id, handler);
        }

        for recursive_type in table.check_recusive_type() {
            handler.receive(Error::RecursiveType(RecursiveType {
                struct_ids: recursive_type.into_iter().collect(),
            }));
        }

        table
    }

    /// Gets the [`GlobalID`] of a symbol with the given fully qualified name.
    pub fn get_global_id_by_full_name<'a>(
        &self,
        mut name: impl Iterator<Item = &'a str>,
    ) -> Option<GlobalID> {
        let mut current_id = self
            .root_module_ids_by_name
            .get(name.next()?)
            .copied()?
            .into();

        for name_part in name {
            let scoped_id: ScopedID = match current_id {
                GlobalID::Module(id) => id.into(),
                GlobalID::Struct(id) => id.into(),
                GlobalID::Enum(id) => id.into(),
                GlobalID::EnumVariant(..) | GlobalID::OverloadSet(..) | GlobalID::TypeAlias(..) => {
                    return None;
                }
            };

            current_id = self
                .get_scoped(scoped_id)
                .unwrap()
                .get_child_id_by_name(name_part)?;
        }

        Some(current_id)
    }

    /// Gets the full name of the symbol of the given [`GlobalID`].
    ///
    /// # Errors
    /// - If the given [`GlobalID`] is invalid.
    pub fn get_full_name_of(
        &self,
        global_id: impl Into<GlobalID>,
    ) -> Result<String, InvalidIDError> {
        let global_id = global_id.into();
        let global = self.get_global(global_id)?;
        let mut name = global.name().to_owned();
        let mut parent = global
            .parent_scoped_id()
            .map(|id| self.get_scoped(id).unwrap());

        while let Some(current_parent) = parent {
            name.insert_str(0, "::");
            name.insert_str(0, current_parent.name());
            parent = current_parent
                .parent_scoped_id()
                .map(|id| self.get_scoped(id).unwrap());
        }

        Ok(name)
    }

    /// Checks whether if the `symol` is accessible from the `referring_site` if the
    /// `symbol` with the given `accessibility`.
    ///
    /// # Errors
    /// - If the given `referring_site` or `symbol` is invalid.
    pub fn symbol_accessible(
        &self,
        referring_site: ScopedID,
        symbol: GlobalID,
        accessibility: Accessibility,
    ) -> Result<bool, InvalidIDError> {
        match accessibility {
            Accessibility::Internal => {
                let referring_site_root_module = self.find_root_module(referring_site.into())?;
                let symbol_root_module = self.find_root_module(symbol)?;

                Ok(referring_site_root_module == symbol_root_module)
            }
            Accessibility::Private => {
                // find the nearest parent module of both the referring site and the symbol and
                // check if the parent module of the referring site is the same or a child of the
                // parent module of the symbol
                let sym_parent = self.find_nearest_parent_module(symbol)?;
                let mut current_module = referring_site;
                loop {
                    match current_module {
                        ScopedID::Module(module) if module == sym_parent => {
                            break Ok(true);
                        }
                        _ => {
                            if let Some(parent) =
                                self.get_scoped(current_module)?.parent_scoped_id()
                            {
                                current_module = parent;
                            } else {
                                break Ok(false);
                            }
                        }
                    }
                }
            }
            Accessibility::Public => Ok(true),
        }
    }

    /// Performs a name resolution from the given [`QualifiedIdentifier`] syntax tree and referring
    /// site (where the name resolution is performed from).
    ///
    /// # Errors
    /// - [`ResolveError::FatalSymbolError`]: Found a [`enum@Error`] that is fatal to the name
    /// resolution process.
    /// - [`ResolveError::InvalidIDError`]: The given `referring_site` is not a valid [`ScopedID`].
    pub fn resolve_symbol(
        &self,
        referring_site: ScopedID,
        qualified_identifier: &QualifiedIdentifier,
        handler: &impl ErrorHandler<Error>,
    ) -> Result<GlobalID, ResolveError> {
        // find the starting point of the name resolution
        let mut current_id = if qualified_identifier.leading_separator().is_some() {
            // start from root
            if let Some(id) = self
                .root_module_ids_by_name
                .get(qualified_identifier.identifiers().first().span.str())
                .copied()
            {
                id.into()
            } else {
                handler.receive(
                    SymbolNotFound {
                        span: qualified_identifier.identifiers().first().span.clone(),
                        in_scope_id: None,
                    }
                    .into(),
                );

                return Err(ResolveError::FatalSymbolError);
            }
        } else {
            // search starting from the referring site and going up the parent chain. the first
            // scope that contains the symbol is the starting point
            let mut in_scope_id: GlobalID = referring_site.into();
            loop {
                let result = self.search_child(
                    in_scope_id,
                    qualified_identifier.identifiers().first().span.str(),
                );

                if let Some(result) = result {
                    break result;
                }

                // if the parent scope doesn't contain the symbol, go up the parent chain
                // if returns None, then the symbol is not found
                in_scope_id =
                    if let Some(id) = self.get_global(in_scope_id).unwrap().parent_scoped_id() {
                        id.into()
                    } else {
                        handler.receive(
                            SymbolNotFound {
                                span: qualified_identifier.identifiers().first().span.clone(),
                                in_scope_id: None,
                            }
                            .into(),
                        );
                        return Err(ResolveError::FatalSymbolError);
                    };
            }
        };

        // search the symbol for the rest of the identifiers
        for identifier in qualified_identifier.identifiers().rest() {
            if let Some(result) = self.search_child(current_id, identifier.1.span.str()) {
                // check if the symbol is accessible
                if !self.symbol_accessible(
                    referring_site,
                    result,
                    self.get_global(current_id).unwrap().accessibility(),
                )? {
                    handler.receive(
                        SymbolNotAccessible {
                            span: identifier.1.span.clone(),
                            global_id: result,
                        }
                        .into(),
                    );
                }

                current_id = result;
            } else {
                handler.receive(
                    SymbolNotFound {
                        span: identifier.1.span.clone(),
                        in_scope_id: Some(current_id),
                    }
                    .into(),
                );
                return Err(ResolveError::FatalSymbolError);
            }
        }

        Ok(current_id)
    }

    /// Resolves a [`TypeSpecifier`] syntax tree into a [`Type`]
    ///
    /// # Errors
    /// - [`ResolveError::FatalSymbolError`]: Found a [`enum@Error`] that is fatal to the name
    /// resolution process.
    /// - [`ResolveError::InvalidIDError`]: The given `referring_site` is not a valid [`ScopedID`].
    pub fn resolve_type(
        &self,
        referring_site: ScopedID,
        type_specifier: &TypeSpecifier,
        handler: &impl ErrorHandler<Error>,
    ) -> Result<Type, ResolveError> {
        match type_specifier {
            TypeSpecifier::PrimitiveTypeSpecifier(primitive_type) => match primitive_type {
                PrimitiveTypeSpecifier::Bool(..) => Ok(Type::PrimitiveType(PrimitiveType::Bool)),
                PrimitiveTypeSpecifier::Int8(..) => Ok(Type::PrimitiveType(PrimitiveType::Int8)),
                PrimitiveTypeSpecifier::Int16(..) => Ok(Type::PrimitiveType(PrimitiveType::Int16)),
                PrimitiveTypeSpecifier::Int32(..) => Ok(Type::PrimitiveType(PrimitiveType::Int32)),
                PrimitiveTypeSpecifier::Int64(..) => Ok(Type::PrimitiveType(PrimitiveType::Int64)),
                PrimitiveTypeSpecifier::Uint8(..) => Ok(Type::PrimitiveType(PrimitiveType::Uint8)),
                PrimitiveTypeSpecifier::Uint16(..) => {
                    Ok(Type::PrimitiveType(PrimitiveType::Uint16))
                }
                PrimitiveTypeSpecifier::Uint32(..) => {
                    Ok(Type::PrimitiveType(PrimitiveType::Uint32))
                }
                PrimitiveTypeSpecifier::Uint64(..) => {
                    Ok(Type::PrimitiveType(PrimitiveType::Uint64))
                }
                PrimitiveTypeSpecifier::Float32(..) => {
                    Ok(Type::PrimitiveType(PrimitiveType::Float32))
                }
                PrimitiveTypeSpecifier::Float64(..) => {
                    Ok(Type::PrimitiveType(PrimitiveType::Float64))
                }
                PrimitiveTypeSpecifier::Void(..) => Ok(Type::PrimitiveType(PrimitiveType::Void)),
            },
            TypeSpecifier::QualifiedIdentifier(qualified_identifier) => {
                let symbol_id =
                    self.resolve_symbol(referring_site, qualified_identifier, handler)?;

                match symbol_id {
                    GlobalID::Struct(sym) => Ok(Type::TypedID(sym.into())),
                    GlobalID::Enum(sym) => Ok(Type::TypedID(sym.into())),
                    GlobalID::TypeAlias(sym) => Ok(self.get_type_alias(sym).unwrap().alias),
                    GlobalID::Module(..)
                    | GlobalID::OverloadSet(..)
                    | GlobalID::EnumVariant(..) => {
                        handler.receive(
                            TypeExpected {
                                span: qualified_identifier.span(),
                                found: symbol_id,
                            }
                            .into(),
                        );
                        Err(ResolveError::FatalSymbolError)
                    }
                }
            }
        }
    }
}

impl Table {
    #[allow(dead_code)]
    fn new() -> Self {
        Self {
            modules: Arena::new(),
            structs: Arena::new(),
            enums: Arena::new(),
            enum_variants: Arena::new(),
            type_aliases: Arena::new(),
            overload_sets: Arena::new(),
            overloads: Arena::new(),
            fields: Arena::new(),
            parameters: Arena::new(),
            root_module_ids_by_name: HashMap::new(),
        }
    }

    /// Generates the module hierarchy for the given target
    fn generate_modules(&mut self, target: &TargetParsing) {
        // iteration is sorted by the depth of the module hierarchy
        for file in target.file_parsings() {
            let parent_module_id = if file.source_file().module_hierarchy().len() == 1 {
                None
            } else {
                // the parent module name is the full name of the target without the last part
                Some(
                    self.get_global_id_by_full_name(
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

            let module = Module {
                parent_module_id,
                name: file
                    .source_file()
                    .module_hierarchy()
                    .last()
                    .unwrap()
                    .clone(),
                child_ids_by_name: HashMap::new(),
                accessibility: file
                    .access_modifier()
                    .as_ref()
                    .map_or(Accessibility::Public, Accessibility::from_syntax_tree),
            };

            let id = self.modules.insert(module);

            // add the module to the parent module
            if let Some(parent) = parent_module_id {
                self.get_module_mut(parent)
                    .unwrap()
                    .child_ids_by_name
                    .insert(
                        file.source_file()
                            .module_hierarchy()
                            .last()
                            .unwrap()
                            .clone(),
                        id.into(),
                    );
            } else {
                // add to the root
                self.root_module_ids_by_name.insert(
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

    fn draft_symbols(
        &mut self,
        target_parsing: TargetParsing,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl ErrorHandler<Error>,
    ) {
        let files = target_parsing
            .dissolve()
            .into_iter()
            .map(FileParsing::dissolve)
            .collect::<Vec<_>>();

        for (source_file, file_syntax_tree, _) in files {
            for item in file_syntax_tree.dissolve() {
                // parent module id
                let parent_module_id = self
                    .get_global_id_by_full_name(
                        source_file.module_hierarchy().iter().map(String::as_str),
                    )
                    .unwrap()
                    .into_module()
                    .unwrap();

                let name = {
                    let (is_function, identifier) = match &item {
                        ItemSyntaxTree::Struct(sym) => (false, sym.signature().identifier()),
                        ItemSyntaxTree::Enum(sym) => (false, sym.signature().identifier()),
                        ItemSyntaxTree::Function(sym) => (true, sym.signature().identifier()),
                        ItemSyntaxTree::Module(..) => continue,
                        ItemSyntaxTree::TypeAlias(sym) => (false, sym.identifier()),
                    };

                    // check for symbol redefinition
                    if let Some(available_global_id) = self.get_global_id_by_full_name(
                        source_file
                            .module_hierarchy()
                            .iter()
                            .map(String::as_str)
                            .chain(std::iter::once(identifier.span.str())),
                    ) {
                        // if the current item is function, the available item should be function
                        // overload set (function overloadings are allowed).
                        let is_redefinition = if is_function {
                            available_global_id.as_overload_set().is_none()
                        } else {
                            true
                        };

                        if is_redefinition {
                            handler.receive(
                                SymbolRedifinition {
                                    available_global_id,
                                    span: identifier.span.clone(),
                                }
                                .into(),
                            );
                            continue;
                        }
                    }

                    identifier.span.str().to_owned()
                };

                let global_id = match item {
                    ItemSyntaxTree::Struct(syntax_tree) => Some(
                        self.draft_struct(
                            syntax_tree,
                            parent_module_id,
                            symbol_states_by_id,
                            handler,
                        )
                        .into(),
                    ),
                    ItemSyntaxTree::Enum(syntax_tree) => Some(
                        self.draft_enum(syntax_tree, parent_module_id, handler)
                            .into(),
                    ),
                    ItemSyntaxTree::Function(syntax_tree) => Some(
                        self.draft_overload_set(
                            syntax_tree,
                            parent_module_id,
                            symbol_states_by_id,
                            handler,
                        )
                        .into(),
                    ),
                    ItemSyntaxTree::Module(..) => continue,
                    ItemSyntaxTree::TypeAlias(syntax_tree) => self
                        .draft_type_alias(syntax_tree, parent_module_id.into(), handler)
                        .map(GlobalID::TypeAlias),
                };

                let Some(global_id) = global_id else {
                    continue
                };

                // add the symbol to the parent module
                let module_sym = self.get_module_mut(parent_module_id).unwrap();
                module_sym.child_ids_by_name.insert(name, global_id);

                match global_id {
                    GlobalID::TypeAlias(id) => {
                        symbol_states_by_id.insert(id.into(), SymbolState::Drafted);
                    }
                    GlobalID::Module(..) | GlobalID::Struct(..) | GlobalID::Enum(..) => (),
                    GlobalID::OverloadSet(id) => {
                        symbol_states_by_id.insert(id.into(), SymbolState::Drafted);
                    }
                    GlobalID::EnumVariant(_) => unreachable!(),
                }
            }
        }
    }

    fn draft_overload_set(
        &mut self,
        syntax_tree: FunctionSyntaxTree,
        parent_module_id: ModuleID,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl ErrorHandler<Error>,
    ) -> OverloadSetID {
        // create new overload set
        #[allow(clippy::option_if_let_else)]
        let overload_set_id = if let Some(id) = self
            .get_module(parent_module_id)
            .unwrap()
            .child_ids_by_name
            .get(syntax_tree.signature().identifier().span.str())
        {
            id.into_overload_set().unwrap()
        } else {
            self.overload_sets.insert(OverloadSet {
                parent_module_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                overloads: Vec::new(),
            })
        };

        let (signature, block_without_label) = syntax_tree.dissolve();
        let (access_modifier, function_keyword, identifier, _, parameter_ist, _, type_annotation) =
            signature.dissolve();

        // signature syntax tree
        let overload_syntax_tree = OverloadSyntaxTree {
            access_modifier,
            function_keyword,
            identifier,
            type_annotation,
            block_without_label,
        };

        // create new overload
        let overload_id = self.overloads.insert(Overload {
            parent_overload_set_id: overload_set_id,
            accessibility: Accessibility::from_syntax_tree(&overload_syntax_tree.access_modifier),
            syntax_tree: Arc::new(overload_syntax_tree),
            parameter_ids_by_name: HashMap::new(), // To be filled later
            parameter_order: Vec::new(),           // To be filled later
            return_type: PrimitiveType::Void.into(),
        });

        // add symbol state
        symbol_states_by_id.insert(overload_id.into(), SymbolState::Drafted);
        self.get_overload_set_mut(overload_set_id)
            .unwrap()
            .overloads
            .push(overload_id);

        // draft parameter
        for parameter in parameter_ist
            .into_iter()
            .flat_map(ConnectedList::into_elements)
        {
            let Some(parameter_id) = self.draft_parameter(
                parameter,
                overload_id,
                self.get_overload(overload_id).unwrap().parameter_order().len(),
                handler
            ) else {
                continue;
            };

            // add symbol state
            let parameter_name = self.get_parameter(parameter_id).unwrap().name.clone();
            symbol_states_by_id.insert(parameter_id.into(), SymbolState::Drafted);

            let overload = self.get_overload_mut(overload_id).unwrap();
            overload
                .parameter_ids_by_name
                .insert(parameter_name, parameter_id);
            overload.parameter_order.push(parameter_id);
        }

        overload_set_id
    }

    fn draft_parameter(
        &mut self,
        syntax_tree: ParameterSyntaxTree,
        parent_overload_id: OverloadID,
        declaration_order: usize,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<ParameterID> {
        let name = syntax_tree.identifier().span.str().to_owned();
        if let Some(available_parameter_id) = self
            .get_overload(parent_overload_id)
            .unwrap()
            .parameter_ids_by_name()
            .get(&name)
            .copied()
        {
            handler.receive(
                ParameterRedefinition {
                    span: syntax_tree.identifier().span.clone(),
                    available_parameter_id,
                }
                .into(),
            );
            return None;
        }

        Some(self.parameters.insert(Parameter {
            name,
            syntax_tree: Arc::new(syntax_tree),
            parent_overload_id,
            declaration_order,
            type_binding: TypeBinding {
                ty: PrimitiveType::Void.into(),
                is_mutable: false,
            }, //  to be filled later
        }))
    }

    fn draft_struct(
        &mut self,
        syntax_tree: StructSyntaxTree,
        parent_module_id: ModuleID,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl ErrorHandler<Error>,
    ) -> StructID {
        let (signature, body) = syntax_tree.dissolve();

        let data = Struct {
            name: signature.identifier().span.str().to_owned(),
            accessibility: Accessibility::from_syntax_tree(signature.access_modifier()),
            parent_module_id,
            syntax_tree: Arc::new(signature),
            field_ids_by_name: HashMap::new(), // To be filled later
            field_order: Vec::new(),           // To be filled later
            type_alias_ids_by_name: HashMap::new(), // To be filled later
        };

        let id = self.structs.insert(data);

        // Draft each member
        for member in body.dissolve().1 {
            let id = match member {
                MemberSyntaxTree::Field(field) => {
                    // add to the field_ids_by_name
                    let Some(field_id) = self.draft_field(
                        field,
                        id,
                        self.get_struct(id).unwrap().field_order.len(),
                        handler,
                    ) else {
                        continue;
                    };

                    let field_name = self.get_field(field_id).unwrap().name.clone();
                    let struct_sym = self.get_struct_mut(id).unwrap();

                    struct_sym.field_ids_by_name.insert(field_name, field_id);
                    struct_sym.field_order.push(field_id);

                    field_id.into()
                }
                MemberSyntaxTree::TypeAlias(type_alias) => {
                    // add to the type_alias_ids_by_name
                    let Some(type_alias_id) = self.draft_type_alias(
                        type_alias,
                        TypeAliasParentID::Struct(id),
                        handler,
                    ) else {
                        continue;
                    };

                    let type_alias_name = self.get_type_alias(type_alias_id).unwrap().name.clone();
                    let struct_sym = self.get_struct_mut(id).unwrap();

                    struct_sym
                        .type_alias_ids_by_name
                        .insert(type_alias_name, type_alias_id);

                    type_alias_id.into()
                }
            };

            symbol_states_by_id.insert(id, SymbolState::Drafted);
        }

        id
    }

    fn draft_type_alias(
        &mut self,
        syntax_tree: TypeAliasSyntaxTree,
        parent_type_alias_id: TypeAliasParentID,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<TypeAliasID> {
        let name = syntax_tree.identifier().span.str().to_owned();
        let accessibility = Accessibility::from_syntax_tree(syntax_tree.access_modifier());

        // redifinition and accessibility check
        match parent_type_alias_id {
            TypeAliasParentID::Module(sym) => {
                let module_sym = self.get_module(sym).unwrap();
                if let Some(available_global_id) = module_sym.child_ids_by_name.get(&name).copied()
                {
                    // symbol redefinition
                    handler.receive(
                        SymbolRedifinition {
                            available_global_id,
                            span: syntax_tree.identifier().span.clone(),
                        }
                        .into(),
                    );

                    return None;
                }
            }
            TypeAliasParentID::Struct(sym) => {
                let struct_sym = self.get_struct(sym).unwrap();
                if let Some(available_global_id) =
                    struct_sym.type_alias_ids_by_name.get(&name).copied()
                {
                    // symbol redefinition
                    handler.receive(
                        SymbolRedifinition {
                            available_global_id: available_global_id.into(),
                            span: syntax_tree.identifier().span.clone(),
                        }
                        .into(),
                    );

                    return None;
                }

                // accessibility check
                if accessibility < struct_sym.accessibility {
                    handler.receive(
                        StructMemberMoreAccessibleThanStruct {
                            span: syntax_tree.identifier().span.clone(),
                            member_accessibility: accessibility,
                            struct_accessibility: struct_sym.accessibility,
                        }
                        .into(),
                    );
                }
            }
        }

        Some(self.type_aliases.insert(TypeAlias {
            name,
            accessibility,
            type_alias_parent_id: parent_type_alias_id,
            syntax_tree: Arc::new(syntax_tree),
            alias: PrimitiveType::Void.into(), // To be filled later
        }))
    }

    fn draft_field(
        &mut self,
        syntax_tree: FieldSyntaxTree,
        parent_struct_id: StructID,
        declaration_order: usize,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<FieldID> {
        if let Some(available_field_id) = self
            .get_struct(parent_struct_id)
            .unwrap()
            .field_ids_by_name
            .get(syntax_tree.identifier().span.str())
            .copied()
        {
            // field redefinition
            handler.receive(
                FieldRedefinition {
                    span: syntax_tree.identifier().span.clone(),
                    available_field_id,
                }
                .into(),
            );

            return None;
        }

        let accessibility = Accessibility::from_syntax_tree(syntax_tree.access_modifier());

        // accessibility check
        {
            let struct_symbol = &self.get_struct(parent_struct_id).unwrap();
            if struct_symbol.accessibility.rank() < accessibility.rank() {
                handler.receive(
                    StructMemberMoreAccessibleThanStruct {
                        span: syntax_tree.identifier().span.clone(),
                        member_accessibility: accessibility,
                        struct_accessibility: struct_symbol.accessibility,
                    }
                    .into(),
                );
            }
        }

        Some(self.fields.insert(Field {
            name: syntax_tree.identifier().span.str().to_owned(),
            accessibility,
            parent_struct_id,
            syntax_tree: Arc::new(syntax_tree),
            declaration_order,
            ty: PrimitiveType::Void.into(), // To be filled later
        }))
    }

    fn draft_enum(
        &mut self,
        syntax_tree: EnumSyntaxTree,
        parent_module_id: ModuleID,
        handler: &impl ErrorHandler<Error>,
    ) -> EnumID {
        let (signature, body) = syntax_tree.dissolve();
        let name = signature.identifier().span.str().to_owned();
        let accessibility = Accessibility::from_syntax_tree(signature.access_modifier());

        let id = self.enums.insert(Enum {
            name,
            accessibility,
            parent_module_id,
            syntax_tree: Arc::new(signature),
            variant_ids_by_name: HashMap::new(), // To be filled later
            variant_order: Vec::new(),           // To be filled later
        });

        for variant in body
            .dissolve()
            .1
            .into_iter()
            .flat_map(ConnectedList::into_elements)
        {
            let variant_id = self.draft_enum_variant(
                variant,
                id,
                self.get_enum(id).unwrap().variant_order.len(),
                handler,
            );

            if let Some(variant_id) = variant_id {
                let variant_name = self.get_enum_variant(variant_id).unwrap().name.clone();
                let enum_sym = self.get_enum_mut(id).unwrap();

                // add to the name map
                enum_sym
                    .variant_ids_by_name
                    .insert(variant_name, variant_id);

                // add to the order vec
                enum_sym.variant_order.push(variant_id);
            }
        }

        id
    }

    fn draft_enum_variant(
        &mut self,
        syntax_tree: IdentifierToken,
        parent_enum_id: EnumID,
        declaration_order: usize,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<EnumVariantID> {
        let name = syntax_tree.span.str().to_owned();

        // redefinition check
        if let Some(available_enum_variant_id) = self
            .get_enum(parent_enum_id)
            .unwrap()
            .variant_ids_by_name
            .get(&name)
            .copied()
        {
            handler.receive(
                EnumVariantRedefinition {
                    span: syntax_tree.span,
                    available_enum_variant_id,
                }
                .into(),
            );

            return None;
        }

        Some(self.enum_variants.insert(EnumVariant {
            name,
            parent_enum_id,
            syntax_tree: Arc::new(syntax_tree),
            declaration_order,
        }))
    }

    fn construct_symbol(
        &mut self,
        id: ID,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl ErrorHandler<Error>,
    ) {
        match id {
            ID::Module(..) | ID::Struct(..) | ID::Enum(..) | ID::EnumVariant(..) => unreachable!(),
            ID::OverloadSet(overload_set_id) => {
                self.construct_overload_set(overload_set_id, symbol_states_by_id, handler);
            }
            ID::TypeAlias(type_alias_id) => {
                self.construct_type_alias(type_alias_id, symbol_states_by_id, handler);
            }
            ID::Field(field_id) => {
                self.construct_field(field_id, symbol_states_by_id, handler);
            }
            ID::Overload(overload_id) => {
                self.construct_overload(overload_id, symbol_states_by_id, handler);
            }
            ID::Parameter(parameter_id) => {
                self.construct_parameter(parameter_id, symbol_states_by_id, handler);
            }
        }
    }

    fn construct_overload_set(
        &mut self,
        overload_set_id: OverloadSetID,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl ErrorHandler<Error>,
    ) {
        for i in 0..self
            .get_overload_set(overload_set_id)
            .unwrap()
            .overloads
            .len()
        {
            let overload_set = self.get_overload_set(overload_set_id).unwrap();
            let overload_id = overload_set.overloads[i];
            self.check_symbol_requirement(overload_id.into(), symbol_states_by_id, handler);
        }

        // check for overload redefinition
        let mut i = 0;
        while i < self
            .get_overload_set(overload_set_id)
            .unwrap()
            .overloads
            .len()
        {
            let mut j = i + 1;
            while j < self
                .get_overload_set(overload_set_id)
                .unwrap()
                .overloads
                .len()
            {
                let overload_set = self.get_overload_set(overload_set_id).unwrap();
                let overload_j = self.get_overload(overload_set.overloads[j]).unwrap();

                let redefined = 'result: {
                    let overload_i = self.get_overload(overload_set.overloads[i]).unwrap();

                    if overload_i.parameter_order.len() != overload_j.parameter_order.len() {
                        break 'result false;
                    }

                    for (parameter_i, parameter_j) in overload_i
                        .parameter_order
                        .iter()
                        .zip(overload_j.parameter_order.iter())
                        .map(|(i, j)| {
                            (
                                self.get_parameter(*i).unwrap(),
                                self.get_parameter(*j).unwrap(),
                            )
                        })
                    {
                        if parameter_i.type_binding.ty != parameter_j.type_binding.ty {
                            break 'result false;
                        }
                    }

                    true
                };

                if redefined {
                    handler.receive(
                        OverloadRedefinition {
                            span: overload_j.syntax_tree.identifier.span(),
                            available_overload_id: overload_set.overloads[i],
                        }
                        .into(),
                    );
                    self.get_overload_set_mut(overload_set_id)
                        .unwrap()
                        .overloads
                        .remove(j);
                } else {
                    j += 1;
                }
            }
            i += 1;
        }

        // remove the overload set from the symbol states by id map
        symbol_states_by_id.remove(&overload_set_id.into());
    }

    fn construct_overload(
        &mut self,
        overload_id: OverloadID,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl ErrorHandler<Error>,
    ) {
        *symbol_states_by_id.get_mut(&overload_id.into()).unwrap() = SymbolState::Constructing;

        let overload = self.get_overload(overload_id).unwrap();

        let syntax_tree = overload.syntax_tree.clone();
        let ty = self
            .resolve_type_with_accessibility_constraint(
                syntax_tree.type_annotation.type_specifier(),
                overload.accessibility,
                self.get_overload_set(overload.parent_overload_set_id)
                    .unwrap()
                    .parent_module_id
                    .into(),
                symbol_states_by_id,
                handler,
            )
            .unwrap_or(Type::PrimitiveType(PrimitiveType::Void));

        self.get_overload_mut(overload_id).unwrap().return_type = ty;

        for i in 0..self
            .get_overload(overload_id)
            .unwrap()
            .parameter_order
            .len()
        {
            self.check_symbol_requirement(
                self.get_overload(overload_id).unwrap().parameter_order[i].into(),
                symbol_states_by_id,
                handler,
            );
        }

        symbol_states_by_id.remove(&overload_id.into());
    }

    fn construct_parameter(
        &mut self,
        parameter_id: ParameterID,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl ErrorHandler<Error>,
    ) {
        *symbol_states_by_id.get_mut(&parameter_id.into()).unwrap() = SymbolState::Constructing;
        let parameter = self.get_parameter(parameter_id).unwrap();

        let syntax_tree = parameter.syntax_tree.clone();
        let parent_module_id = self
            .get_overload_set(
                self.get_overload(parameter.parent_overload_id)
                    .unwrap()
                    .parent_overload_set_id,
            )
            .unwrap()
            .parent_module_id;

        let ty = self
            .resolve_type_with_accessibility_constraint(
                syntax_tree.type_annotation().type_specifier(),
                self.get_overload(self.get_parameter(parameter_id).unwrap().parent_overload_id)
                    .unwrap()
                    .accessibility,
                parent_module_id.into(),
                symbol_states_by_id,
                handler,
            )
            .unwrap_or(Type::PrimitiveType(PrimitiveType::Void));

        self.get_parameter_mut(parameter_id).unwrap().type_binding = TypeBinding {
            ty,
            is_mutable: syntax_tree.mutable_keyword().is_some(),
        };

        // remove the parameter from the constructing list
        symbol_states_by_id.remove(&parameter_id.into());
    }

    fn construct_field(
        &mut self,
        field_id: FieldID,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl ErrorHandler<Error>,
    ) {
        *symbol_states_by_id.get_mut(&field_id.into()).unwrap() = SymbolState::Constructing;

        let field = self.get_field(field_id).unwrap();
        let syntax_tree = field.syntax_tree.clone();

        // resolve the type
        let ty = self
            .resolve_type_with_accessibility_constraint(
                syntax_tree.type_annotation().type_specifier(),
                field.accessibility,
                field.parent_struct_id.into(),
                symbol_states_by_id,
                handler,
            )
            .unwrap_or(Type::PrimitiveType(PrimitiveType::Void));

        // update the field
        self.get_field_mut(field_id).unwrap().ty = ty;

        // remove from the constructing set
        symbol_states_by_id.remove(&field_id.into());
    }

    fn construct_type_alias(
        &mut self,
        type_alias_id: TypeAliasID,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl ErrorHandler<Error>,
    ) {
        *symbol_states_by_id.get_mut(&type_alias_id.into()).unwrap() = SymbolState::Constructing;

        let type_alias = self.get_type_alias(type_alias_id).unwrap();
        let type_specifier = type_alias.syntax_tree.clone();

        // resolve the type
        let ty = self
            .resolve_type_with_accessibility_constraint(
                type_specifier.type_specifier(),
                type_alias.accessibility,
                type_alias.type_alias_parent_id.into(),
                symbol_states_by_id,
                handler,
            )
            .unwrap_or(Type::PrimitiveType(PrimitiveType::Void));

        self.get_type_alias_mut(type_alias_id).unwrap().alias = ty;

        // remove the state
        symbol_states_by_id.remove(&type_alias_id.into());
    }

    fn check_symbol_requirement(
        &mut self,
        id: ID,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl ErrorHandler<Error>,
    ) -> bool {
        match symbol_states_by_id.get(&id) {
            // constructs the symbol if it is not constructed
            Some(SymbolState::Drafted) => {
                self.construct_symbol(id, symbol_states_by_id, handler);
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

                handler.receive(CircularDependency { symbol_ids }.into());
                false
            }
            None => true,
        }
    }

    fn resolve_type_constructing(
        &mut self,
        referring_site: ScopedID,
        type_specifier: &TypeSpecifier,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl ErrorHandler<Error>,
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
                let symbol_id = self.resolve_symbol_constructing(
                    referring_site,
                    qualified_identifier,
                    symbol_states_by_id,
                    handler,
                )?;

                match symbol_id {
                    GlobalID::Struct(sym) => Some(Type::TypedID(sym.into())),
                    GlobalID::Enum(sym) => Some(Type::TypedID(sym.into())),
                    GlobalID::TypeAlias(sym) => Some(self.get_type_alias(sym).unwrap().alias),
                    GlobalID::Module(..)
                    | GlobalID::OverloadSet(..)
                    | GlobalID::EnumVariant(..) => {
                        handler.receive(
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
            Type::TypedID(ty) => self.get_global(ty.into()).unwrap().accessibility(),
        }
    }

    fn resolve_type_with_accessibility_constraint(
        &mut self,
        type_specifier: &TypeSpecifier,
        accessibility_constraint: Accessibility,
        referring_site: ScopedID,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<Type> {
        let ty = self.resolve_type_constructing(
            referring_site,
            type_specifier,
            symbol_states_by_id,
            handler,
        )?;

        let ty_accessibility = self.get_overall_accessibility(ty);

        if accessibility_constraint.rank() > ty_accessibility.rank() {
            handler.receive(
                PrivateSymbolLeak {
                    span: type_specifier.span(),
                    accessibility: ty_accessibility,
                    parent_accessibility: accessibility_constraint,
                }
                .into(),
            );
        }

        Some(ty)
    }

    fn search_child(&self, parent: GlobalID, name: &str) -> Option<GlobalID> {
        match parent {
            GlobalID::Struct(sym) => self
                .get_struct(sym)
                .unwrap()
                .type_alias_ids_by_name
                .get(name)
                .copied()
                .map(std::convert::Into::into),
            GlobalID::Enum(sym) => self
                .get_enum(sym)
                .unwrap()
                .variant_ids_by_name
                .get(name)
                .copied()
                .map(std::convert::Into::into),
            GlobalID::Module(sym) => self
                .get_module(sym)
                .unwrap()
                .child_ids_by_name
                .get(name)
                .copied()
                .map(std::convert::Into::into),
            GlobalID::TypeAlias(sym) => match self.get_type_alias(sym).unwrap().alias {
                Type::PrimitiveType(..) => None,
                Type::TypedID(type_id) => self.search_child(type_id.into(), name),
            },
            GlobalID::OverloadSet(..) | GlobalID::EnumVariant(..) => None,
        }
    }

    fn search_child_constructing(
        &mut self,
        parent: GlobalID,
        name: &str,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<GlobalID> {
        if !self.check_symbol_requirement(parent.into(), symbol_states_by_id, handler) {
            return None;
        }

        match parent {
            GlobalID::Struct(sym) => self
                .get_struct(sym)
                .unwrap()
                .type_alias_ids_by_name
                .get(name)
                .copied()
                .map(std::convert::Into::into),
            GlobalID::Enum(sym) => self
                .get_enum(sym)
                .unwrap()
                .variant_ids_by_name
                .get(name)
                .copied()
                .map(std::convert::Into::into),
            GlobalID::Module(sym) => self
                .get_module(sym)
                .unwrap()
                .child_ids_by_name
                .get(name)
                .copied()
                .map(std::convert::Into::into),
            GlobalID::TypeAlias(sym) => match self.get_type_alias(sym).unwrap().alias {
                Type::PrimitiveType(..) => None,
                Type::TypedID(type_id) => self.search_child_constructing(
                    type_id.into(),
                    name,
                    symbol_states_by_id,
                    handler,
                ),
            },
            GlobalID::OverloadSet(..) | GlobalID::EnumVariant(..) => None,
        }
    }

    /// Retrieves the symbol ID of the given qualified identifier
    fn resolve_symbol_constructing(
        &mut self,
        referring_site: ScopedID,
        qualified_identifier: &QualifiedIdentifier,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<GlobalID> {
        // at the referring site itself, it must be fully constructed
        assert!(!symbol_states_by_id.contains_key(&referring_site.into()));

        // find the starting point of the name resolution
        let mut current_id = if qualified_identifier.leading_separator().is_some() {
            // start from root
            if let Some(id) = self
                .root_module_ids_by_name
                .get(qualified_identifier.identifiers().first().span.str())
                .copied()
            {
                id.into()
            } else {
                handler.receive(
                    SymbolNotFound {
                        span: qualified_identifier.identifiers().first().span.clone(),
                        in_scope_id: None,
                    }
                    .into(),
                );

                return None;
            }
        } else {
            // search starting from the referring site and going up the parent chain. the first
            // scope that contains the symbol is the starting point
            let mut in_scope_id: GlobalID = referring_site.into();
            loop {
                let result = self.search_child_constructing(
                    in_scope_id,
                    qualified_identifier.identifiers().first().span.str(),
                    symbol_states_by_id,
                    handler,
                );

                if let Some(result) = result {
                    break result;
                }

                // if the parent scope doesn't contain the symbol, go up the parent chain
                // if returns None, then the symbol is not found
                in_scope_id =
                    if let Some(id) = self.get_global(in_scope_id).unwrap().parent_scoped_id() {
                        id.into()
                    } else {
                        handler.receive(
                            SymbolNotFound {
                                span: qualified_identifier.identifiers().first().span.clone(),
                                in_scope_id: None,
                            }
                            .into(),
                        );
                        return None;
                    };
            }
        };

        // the symbol must be built
        self.check_symbol_requirement(current_id.into(), symbol_states_by_id, handler);

        // search the symbol for the rest of the identifiers
        for identifier in qualified_identifier.identifiers().rest() {
            if let Some(result) = self.search_child_constructing(
                current_id,
                identifier.1.span.str(),
                symbol_states_by_id,
                handler,
            ) {
                // check if the symbol is accessible
                if !self
                    .symbol_accessible(
                        referring_site,
                        result,
                        self.get_global(current_id).unwrap().accessibility(),
                    )
                    .unwrap()
                {
                    handler.receive(
                        SymbolNotAccessible {
                            span: identifier.1.span.clone(),
                            global_id: result,
                        }
                        .into(),
                    );
                }

                current_id = result;
            } else {
                handler.receive(
                    SymbolNotFound {
                        span: identifier.1.span.clone(),
                        in_scope_id: Some(current_id),
                    }
                    .into(),
                );
                return None;
            }
        }

        self.check_symbol_requirement(current_id.into(), symbol_states_by_id, handler);

        Some(current_id)
    }

    fn find_nearest_parent_module(&self, mut symbol: GlobalID) -> Result<ModuleID, InvalidIDError> {
        if self.get_global(symbol).is_err() {
            return Err(InvalidIDError);
        }

        loop {
            if let GlobalID::Module(symbol) = symbol {
                return Ok(symbol);
            }

            symbol = self.get_global(symbol)?.parent_scoped_id().unwrap().into();
        }
    }

    fn find_root_module(&self, mut symbol: GlobalID) -> Result<ModuleID, InvalidIDError> {
        if self.get_global(symbol).is_err() {
            return Err(InvalidIDError);
        }

        loop {
            if let Some(parent) = self.get_global(symbol)?.parent_scoped_id() {
                symbol = parent.into();
            } else {
                // must be module
                return Ok(symbol.into_module().unwrap());
            }
        }
    }

    fn check_recusive_type(&self) -> Vec<HashSet<StructID>> {
        let mut recursive_types = Vec::new();

        let mut visited_structs = HashSet::new();
        for symbol in self.structs.keys() {
            visited_structs.clear();

            if self.is_recursive(&mut visited_structs, *symbol)
                && !recursive_types.contains(&visited_structs)
            {
                recursive_types.push(visited_structs.clone());
            }
        }

        recursive_types
    }

    fn is_recursive(&self, visited_structs: &mut HashSet<StructID>, struct_id: StructID) -> bool {
        if visited_structs.contains(&struct_id) {
            return true;
        }

        visited_structs.insert(struct_id);

        for field in self
            .get_struct(struct_id)
            .unwrap()
            .field_order()
            .iter()
            .map(|x| self.get_field(*x).unwrap())
        {
            match field.ty() {
                Type::PrimitiveType(_) => (),
                Type::TypedID(typed_id) => match typed_id {
                    super::TypedID::Enum(_) => (),
                    super::TypedID::Struct(struct_id) => {
                        if self.is_recursive(visited_structs, struct_id) {
                            return true;
                        }
                    }
                },
            }
        }

        visited_structs.remove(&struct_id);
        false
    }
}

macro_rules! impl_index_getter {
    ($name:ident, $arena_name:ident $(, $opt_postfix:ident)?) => {
        paste::paste! {
            impl Table {
                #[doc = concat!("Returns a reference to [`super::", stringify!([< $name Symbol >]), "`] with the given ID.")]
                pub fn [< get_ $arena_name >](&self, id: super::[< $name ID >])
                    -> Result<&super::[< $name Symbol >], pernixc_system::arena::InvalidIDError> {
                    self.[< $arena_name $($opt_postfix)? s >].get(id)
                }

                #[allow(dead_code)]
                fn [< get_ $arena_name _mut>](&mut self, id: super::[< $name ID >])
                    -> Result<&mut super::[< $name Symbol >], pernixc_system::arena::InvalidIDError> {
                    self.[< $arena_name $($opt_postfix)? s >].get_mut(id)
                }
            }
        }
    };

    ($name:ident, $method_name:ident, $( ($variant:ident, $arena_name:ident) ),+) => {
        paste::paste! {
            impl Table {
                #[doc = concat!("Returns a mutable reference to [`super::", stringify!($name), "`] with the given ID.")]
                pub fn [< get_ $method_name >](&self, id: super::[< $name ID >])
                    -> Result<& dyn super::$name, pernixc_system::arena::InvalidIDError> {
                    match id {
                        $(
                            super::[< $name ID >]::$variant(id) => self.[< get_ $arena_name >](id).map(|s| s as &dyn super::$name),
                        )+
                    }
                }

                #[allow(dead_code)]
                fn [< get_ $method_name _mut>](&mut self, id: super::[< $name ID >])
                    -> Result<&mut dyn super::$name, pernixc_system::arena::InvalidIDError> {
                    match id {
                        $(
                            super::[< $name ID >]::$variant(id) => self.[< get_ $arena_name _mut>](id).map(|s| s as &mut dyn super::$name),
                        )+
                    }
                }
            }
        }
    };
}

impl_index_getter!(Module, module);
impl_index_getter!(Struct, struct);
impl_index_getter!(Enum, enum);
impl_index_getter!(EnumVariant, enum_variant);
impl_index_getter!(OverloadSet, overload_set);
impl_index_getter!(Overload, overload);
impl_index_getter!(Field, field);
impl_index_getter!(Parameter, parameter);
impl_index_getter!(TypeAlias, type_alias, e);
impl_index_getter!(Global, global, (Module, module), (Struct, struct), (Enum, enum), (EnumVariant, enum_variant), (OverloadSet, overload_set), (TypeAlias, type_alias));
impl_index_getter!(Scoped, scoped, (Module, module), (Struct, struct), (Enum, enum));

#[cfg(test)]
mod tests;
