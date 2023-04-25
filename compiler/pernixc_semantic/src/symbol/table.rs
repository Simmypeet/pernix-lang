use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
    sync::Arc,
};

use enum_as_inner::EnumAsInner;
use paste::paste;
use pernixc_common::source_file::SourceElement;
use pernixc_lexical::token::Identifier as IdentifierToken;
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

use super::{
    errors::{
        CircularDependency, EnumVariantRedefinition, FieldRedefinition, OverloadRedefinition,
        ParameterRedefinition, PrivateSymbolLeak, SymbolError, SymbolRedifinition, TypeExpected,
    },
    ty::{PrimitiveType, Type, TypeBinding},
    Accessibility, Data, Enum, EnumData, EnumID, EnumVariant, EnumVariantData, EnumVariantID,
    Field, FieldID, Global, GlobalID, Module, ModuleData, ModuleID, Overload, OverloadData,
    OverloadID, OverloadSet, OverloadSetData, OverloadSetID, OverloadSignatureSyntaxTree,
    Parameter, ParameterData, ParameterID, Scoped, ScopedID, Struct, StructData, StructID, Symbol,
    TypeAlias, TypeAliasData, TypeAliasID, TypeAliasParentID, Typed, TypedID, WithData, ID,
};
use crate::symbol::{
    errors::{StructMemberMoreAccessibleThanStruct, SymbolNotAccessible, SymbolNotFound},
    FieldData,
};

/// Is an enumeration flag used to indicate the state of the symbol.
///
/// It's as well used to detect cyclic dependencies when constructing the symbol table.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
enum SymbolState {
    /// The symbol is filled in the default state when it's created, doesn't contain the valid
    /// data.
    Drafted,

    /// The symbol is being constructed. This state is used to detect cyclic dependencies.
    Constructing,
}

/// Is the symbol table of the semantic analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Table {
    symbols_by_id: HashMap<ID, Symbol>,
    root_module_ids_by_name: HashMap<String, ModuleID>,
}

impl Table {
    /// Analyzes the given syntax trees of the given target and returns the symbol table containing
    /// all the symbols and the errors that occurred during the analysis.
    ///
    /// # Error Handling
    /// When encountering an error, the analysis will not halt completely, but instead continue to
    /// analyze the rest of the code while producing the suboptimal symbol table. These are the
    /// behavior of the errors:
    ///
    /// - Symbol Redefinition: One of the definitions will be used in the symbol table, the other
    ///   will be discarded and an error will be produced.
    /// - Symbol Not Found: If the symbol is treated as a type, the type will be defaulted to
    ///   `void`.
    /// - Symbol Not Accessible: The symbol will be treated as if it was accessible and an error
    ///   will be produced (non-fatal error).
    #[must_use]
    pub fn analyze(target_parsing: TargetParsing) -> (Self, Vec<SymbolError>) {
        let mut symbol_states_by_id = HashMap::new();
        let mut errors = Vec::new();
        let mut table = Self::new();

        table.generate_modules(&target_parsing);
        table.draft_symbols(target_parsing, &mut errors, &mut symbol_states_by_id);

        // build the symbol in the symbol_states_by_id one by one
        while !symbol_states_by_id.is_empty() {
            let id = *symbol_states_by_id
                .iter()
                .find(|(_, state)| **state == SymbolState::Drafted)
                .unwrap()
                .0;

            table.construct_symbol(id, &mut errors, &mut symbol_states_by_id);
        }

        (table, errors)
    }

    /// Gets the [`GlobalID`] from the given fully qualified name.
    pub fn get_global_id_by_full_name<'a>(
        &self,
        identifiers: impl Iterator<Item = &'a str>,
    ) -> Option<GlobalID> {
        let mut current_id: Option<GlobalID> = None;

        for name in identifiers {
            if let Some(id) = current_id {
                current_id = match id {
                    GlobalID::Module(sym) => Some(self[sym].child_ids_by_name.get(name).copied()?),
                    GlobalID::Struct(sym) => {
                        Some(self[sym].type_alias_ids_by_name.get(name).copied()?.into())
                    }
                    GlobalID::Enum(sym) => {
                        Some(self[sym].variant_ids_by_name.get(name).copied()?.into())
                    }
                    GlobalID::EnumVariant(..)
                    | GlobalID::OverloadSet(..)
                    | GlobalID::TypeAlias(..) => {
                        return None;
                    }
                }
            } else {
                current_id = Some(self.root_module_ids_by_name.get(name).copied()?.into());
            }
        }

        current_id
    }

    /// Gets the fully qualified name of the given symbol.
    #[must_use]
    pub fn get_full_name_of(&self, global_id: impl Into<GlobalID>) -> String {
        let global_id = global_id.into();
        let mut name = self[global_id].name().to_owned();
        let mut parent = self[global_id].parent_scoped_id();

        while let Some(parent_id) = parent {
            name.insert_str(0, "::");
            name.insert_str(0, self[parent_id].name());
            parent = self[parent_id].parent_scoped_id();
        }

        name
    }
}

impl Table {
    /// Creates a new empty [`Table`].
    fn new() -> Self {
        Self {
            symbols_by_id: HashMap::new(),
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

            let module_data = ModuleData {
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

            let id = self.add_symbol(module_data);

            // add the module to the parent module
            if let Some(parent) = parent_module_id {
                self[parent].child_ids_by_name.insert(
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

    /// Drafts all the symbols from the given target parsing.
    fn draft_symbols(
        &mut self,
        target_parsing: TargetParsing,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
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
                            errors.push(
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
                            errors,
                            symbol_states_by_id,
                        )
                        .into(),
                    ),
                    ItemSyntaxTree::Enum(syntax_tree) => Some(
                        self.draft_enum(syntax_tree, parent_module_id, errors)
                            .into(),
                    ),
                    ItemSyntaxTree::Function(syntax_tree) => Some(
                        self.draft_overload_set(
                            syntax_tree,
                            parent_module_id,
                            errors,
                            symbol_states_by_id,
                        )
                        .into(),
                    ),
                    ItemSyntaxTree::Module(..) => continue,
                    ItemSyntaxTree::TypeAlias(syntax_tree) => self
                        .draft_type_alias(syntax_tree, parent_module_id.into(), errors)
                        .map(GlobalID::TypeAlias),
                };

                let Some(global_id) = global_id else {
                    continue
                };

                // add the symbol to the parent module
                let module_sym = &mut self[parent_module_id];
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
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) -> OverloadSetID {
        // create new overload set
        #[allow(clippy::option_if_let_else)]
        let overload_set_id = if let Some(id) = self[parent_module_id]
            .child_ids_by_name
            .get(syntax_tree.signature().identifier().span.str())
        {
            id.into_overload_set().unwrap()
        } else {
            self.add_symbol(OverloadSetData {
                parent_module_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                overloads: Vec::new(),
            })
        };

        let (signature, block_without_label) = syntax_tree.dissolve();
        let (access_modifier, function_keyword, identifier, _, parameter_ist, _, type_annotation) =
            signature.dissolve();

        // signature syntax tree
        let overload_syntax_tree = OverloadSignatureSyntaxTree {
            access_modifier,
            function_keyword,
            identifier,
            type_annotation,
            block_without_label,
        };

        // create new overload
        let overload_id = self.add_symbol(OverloadData {
            parent_overload_set_id: overload_set_id,
            accessibility: Accessibility::from_syntax_tree(&overload_syntax_tree.access_modifier),
            syntax_tree: Arc::new(overload_syntax_tree),
            parameter_ids_by_name: HashMap::new(), // To be filled later
            parameter_order: Vec::new(),           // To be filled later
            return_type: PrimitiveType::Void.into(),
        });

        // add symbol state
        symbol_states_by_id.insert(overload_id.into(), SymbolState::Drafted);
        self[overload_set_id].overloads.push(overload_id);

        // draft parameter
        for parameter in parameter_ist
            .into_iter()
            .flat_map(ConnectedList::into_elements)
        {
            let Some(parameter_id) = self.draft_parameter(
                parameter,
                overload_id,
                self[overload_id].parameter_order().len(),
                errors
            ) else {
                continue;
            };

            // add symbol state
            let parameter_name = self[parameter_id].name.clone();
            symbol_states_by_id.insert(parameter_id.into(), SymbolState::Drafted);

            let overload = &mut self[overload_id];
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
        errors: &mut Vec<SymbolError>,
    ) -> Option<ParameterID> {
        let name = syntax_tree.identifier().span.str().to_owned();
        if let Some(available_parameter_id) = self[parent_overload_id]
            .parameter_ids_by_name()
            .get(&name)
            .copied()
        {
            errors.push(
                ParameterRedefinition {
                    span: syntax_tree.identifier().span.clone(),
                    available_parameter_id,
                }
                .into(),
            );
            return None;
        }

        Some(self.add_symbol(ParameterData {
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
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) -> StructID {
        let (signature, body) = syntax_tree.dissolve();

        let data = StructData {
            name: signature.identifier().span.str().to_owned(),
            accessibility: Accessibility::from_syntax_tree(signature.access_modifier()),
            parent_module_id,
            syntax_tree: Arc::new(signature),
            field_ids_by_name: HashMap::new(), // To be filled later
            field_order: Vec::new(),           // To be filled later
            type_alias_ids_by_name: HashMap::new(), // To be filled later
        };

        let id = self.add_symbol(data);

        // Draft each member
        for member in body.dissolve().1 {
            let id = match member {
                MemberSyntaxTree::Field(field) => {
                    // add to the field_ids_by_name
                    let Some(field_id) = self.draft_field(
                        field,
                        id,
                        self[id].field_order.len(),
                        errors,
                    ) else {
                        continue;
                    };

                    let field_name = self[field_id].name.clone();
                    let struct_sym = &mut self[id];

                    struct_sym.field_ids_by_name.insert(field_name, field_id);
                    struct_sym.field_order.push(field_id);

                    field_id.into()
                }
                MemberSyntaxTree::TypeAlias(type_alias) => {
                    // add to the type_alias_ids_by_name
                    let Some(type_alias_id) = self.draft_type_alias(
                        type_alias,
                        TypeAliasParentID::Struct(id),
                        errors,
                    ) else {
                        continue;
                    };

                    let type_alias_name = self[type_alias_id].name.clone();
                    let struct_sym = &mut self[id];

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
        errors: &mut Vec<SymbolError>,
    ) -> Option<TypeAliasID> {
        let name = syntax_tree.identifier().span.str().to_owned();
        let accessibility = Accessibility::from_syntax_tree(syntax_tree.access_modifier());

        // redifinition and accessibility check
        match parent_type_alias_id {
            TypeAliasParentID::Module(sym) => {
                let module_sym = &self[sym];
                if let Some(available_global_id) = module_sym.child_ids_by_name.get(&name).copied()
                {
                    // symbol redefinition
                    errors.push(
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
                let struct_sym = &self[sym];
                if let Some(available_global_id) =
                    struct_sym.type_alias_ids_by_name.get(&name).copied()
                {
                    // symbol redefinition
                    errors.push(
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
                    errors.push(
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

        Some(self.add_symbol(TypeAliasData {
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
        errors: &mut Vec<SymbolError>,
    ) -> Option<FieldID> {
        if let Some(available_field_id) = self[parent_struct_id]
            .field_ids_by_name
            .get(syntax_tree.identifier().span.str())
            .copied()
        {
            // field redefinition
            errors.push(
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
            let struct_symbol = &self[parent_struct_id];
            if struct_symbol.accessibility.rank() < accessibility.rank() {
                errors.push(
                    StructMemberMoreAccessibleThanStruct {
                        span: syntax_tree.identifier().span.clone(),
                        member_accessibility: accessibility,
                        struct_accessibility: struct_symbol.accessibility,
                    }
                    .into(),
                );
            }
        }

        Some(self.add_symbol(FieldData {
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
        errors: &mut Vec<SymbolError>,
    ) -> EnumID {
        let (signature, body) = syntax_tree.dissolve();
        let name = signature.identifier().span.str().to_owned();
        let accessibility = Accessibility::from_syntax_tree(signature.access_modifier());

        let id = self.add_symbol(EnumData {
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
            let variant_id =
                self.draft_enum_variant(variant, id, self[id].variant_order.len(), errors);

            if let Some(variant_id) = variant_id {
                let variant_name = self[variant_id].name.clone();
                let enum_sym = &mut self[id];

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
        errors: &mut Vec<SymbolError>,
    ) -> Option<EnumVariantID> {
        let name = syntax_tree.span.str().to_owned();

        // redefinition check
        if let Some(available_enum_variant_id) =
            self[parent_enum_id].variant_ids_by_name.get(&name).copied()
        {
            errors.push(
                EnumVariantRedefinition {
                    span: syntax_tree.span,
                    available_enum_variant_id,
                }
                .into(),
            );

            return None;
        }

        Some(self.add_symbol(EnumVariantData {
            name,
            parent_enum_id,
            syntax_tree: Arc::new(syntax_tree),
            declaration_order,
        }))
    }

    /// Add the symbol to the table and returns the ID of the symbol.
    fn add_symbol<T: Data>(&mut self, data: T) -> <T as Data>::ID
    where
        <T as Data>::ID: Into<ID>,
        WithData<T>: Into<Symbol>,
    {
        let with_data = WithData::new(data);
        let id = with_data.id;

        self.symbols_by_id.insert(id.into(), with_data.into());

        id
    }

    fn find_nearest_parent_module(&self, mut symbol: GlobalID) -> ModuleID {
        loop {
            if let GlobalID::Module(symbol) = symbol {
                return symbol;
            }

            symbol = self[symbol].parent_scoped_id().unwrap().into();
        }
    }

    fn find_root_module(&self, mut symbol: GlobalID) -> ModuleID {
        loop {
            if let Some(parent) = self[symbol].parent_scoped_id() {
                symbol = parent.into();
            } else {
                // must be module
                return symbol.into_module().unwrap();
            }
        }
    }

    fn symbol_accessible(
        &self,
        referring_site: ScopedID,
        symbol: GlobalID,
        accessibility: Accessibility,
    ) -> bool {
        match accessibility {
            Accessibility::Internal => {
                let referring_site_root_module = self.find_root_module(referring_site.into());
                let symbol_root_module = self.find_root_module(symbol);

                referring_site_root_module == symbol_root_module
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
                            if let Some(parent) = self[current_module].parent_scoped_id() {
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

    fn construct_symbol(
        &mut self,
        id: ID,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) {
        match id {
            ID::Module(..) | ID::Struct(..) | ID::Enum(..) | ID::EnumVariant(..) => unreachable!(),
            ID::OverloadSet(overload_set_id) => {
                self.construct_overload_set(overload_set_id, errors, symbol_states_by_id);
            }
            ID::TypeAlias(type_alias_id) => {
                self.construct_type_alias(type_alias_id, errors, symbol_states_by_id);
            }
            ID::Field(field_id) => {
                self.construct_field(field_id, errors, symbol_states_by_id);
            }
            ID::Overload(overload_id) => {
                self.construct_overload(overload_id, errors, symbol_states_by_id);
            }
            ID::Parameter(parameter_id) => {
                self.construct_parameter(parameter_id, errors, symbol_states_by_id);
            }
        }
    }

    fn construct_overload_set(
        &mut self,
        overload_set_id: OverloadSetID,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) {
        for i in 0..self[overload_set_id].overloads.len() {
            let overload_id = self[overload_set_id].overloads[i];
            self.check_symbol_requirement(overload_id.into(), errors, symbol_states_by_id);
        }

        // check for overload redefinition
        let mut i = 0;
        while i < self[overload_set_id].overloads.len() {
            let mut j = i + 1;
            while j < self[overload_set_id].overloads.len() {
                let redefined = 'result: {
                    let overload_i = &self[self[overload_set_id].overloads[i]];
                    let overload_j = &self[self[overload_set_id].overloads[j]];

                    if overload_i.parameter_order.len() != overload_j.parameter_order.len() {
                        break 'result false;
                    }

                    for (parameter_i, parameter_j) in overload_i
                        .parameter_order
                        .iter()
                        .zip(overload_j.parameter_order.iter())
                        .map(|(i, j)| (&self[*i], &self[*j]))
                    {
                        if parameter_i.type_binding.ty != parameter_j.type_binding.ty {
                            break 'result false;
                        }
                    }

                    true
                };

                if redefined {
                    errors.push(
                        OverloadRedefinition {
                            span: self[self[overload_set_id].overloads[j]]
                                .syntax_tree
                                .identifier
                                .span(),
                            available_overload_id: self[overload_set_id].overloads[i],
                        }
                        .into(),
                    );
                    self[overload_set_id].overloads.remove(j);
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
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) {
        *symbol_states_by_id.get_mut(&overload_id.into()).unwrap() = SymbolState::Constructing;

        let syntax_tree = self[overload_id].syntax_tree.clone();
        let ty = self
            .resolve_type_with_accessibility_constraint(
                syntax_tree.type_annotation.type_specifier(),
                self[overload_id].accessibility,
                self[self[overload_id].parent_overload_set_id]
                    .parent_module_id
                    .into(),
                errors,
                symbol_states_by_id,
            )
            .unwrap_or(Type::PrimitiveType(PrimitiveType::Void));

        self[overload_id].return_type = ty;

        for i in 0..self[overload_id].parameter_order.len() {
            self.check_symbol_requirement(
                self[overload_id].parameter_order[i].into(),
                errors,
                symbol_states_by_id,
            );
        }

        symbol_states_by_id.remove(&overload_id.into());
    }

    fn resolve_type_with_accessibility_constraint(
        &mut self,
        type_specifier: &TypeSpecifier,
        accessibility_constraint: Accessibility,
        referring_site: ScopedID,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) -> Option<Type> {
        let ty = self.resolve_type(referring_site, type_specifier, errors, symbol_states_by_id)?;

        let ty_accessibility = self.get_overall_accessibility(ty);

        if accessibility_constraint.rank() > ty_accessibility.rank() {
            errors.push(
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

    fn construct_parameter(
        &mut self,
        parameter_id: ParameterID,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) {
        *symbol_states_by_id.get_mut(&parameter_id.into()).unwrap() = SymbolState::Constructing;

        let parameter = self[parameter_id].syntax_tree.clone();
        let parent_module_id = self
            [self[self[parameter_id].parent_overload_id].parent_overload_set_id]
            .parent_module_id;

        let ty = self
            .resolve_type_with_accessibility_constraint(
                parameter.type_annotation().type_specifier(),
                self.index(self.index(parameter_id).parent_overload_id)
                    .accessibility,
                parent_module_id.into(),
                errors,
                symbol_states_by_id,
            )
            .unwrap_or(Type::PrimitiveType(PrimitiveType::Void));

        self[parameter_id].type_binding = TypeBinding {
            ty,
            is_mutable: parameter.mutable_keyword().is_some(),
        };

        // remove the parameter from the constructing list
        symbol_states_by_id.remove(&parameter_id.into());
    }

    fn construct_field(
        &mut self,
        field_id: FieldID,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) {
        *symbol_states_by_id.get_mut(&field_id.into()).unwrap() = SymbolState::Constructing;

        let field = self[field_id].syntax_tree.clone();

        // resolve the type
        let ty = self
            .resolve_type_with_accessibility_constraint(
                field.type_annotation().type_specifier(),
                self[field_id].accessibility,
                self[field_id].parent_struct_id.into(),
                errors,
                symbol_states_by_id,
            )
            .unwrap_or(Type::PrimitiveType(PrimitiveType::Void));

        // update the field
        self[field_id].ty = ty;

        // remove from the constructing set
        symbol_states_by_id.remove(&field_id.into());
    }

    fn construct_type_alias(
        &mut self,
        type_alias_id: TypeAliasID,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) {
        *symbol_states_by_id.get_mut(&type_alias_id.into()).unwrap() = SymbolState::Constructing;

        let type_specifier = self[type_alias_id].syntax_tree.clone();

        // resolve the type
        let ty = self
            .resolve_type_with_accessibility_constraint(
                type_specifier.type_specifier(),
                self[type_alias_id].accessibility,
                self[type_alias_id].type_alias_parent_id.into(),
                errors,
                symbol_states_by_id,
            )
            .unwrap_or(Type::PrimitiveType(PrimitiveType::Void));

        self[type_alias_id].alias = ty;

        // remove the state
        symbol_states_by_id.remove(&type_alias_id.into());
    }

    fn get_overall_accessibility(&mut self, ty: Type) -> Accessibility {
        match ty {
            Type::PrimitiveType(..) => Accessibility::Public,
            Type::TypedID(ty) => self[ty].accessibility(),
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
            Some(SymbolState::Drafted) => {
                self.construct_symbol(id, errors, symbol_states_by_id);
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
        parent: GlobalID,
        name: &str,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
    ) -> Option<GlobalID> {
        if !self.check_symbol_requirement(parent.into(), errors, symbol_states_by_id) {
            return None;
        }

        match parent {
            GlobalID::Struct(sym) => self[sym]
                .type_alias_ids_by_name
                .get(name)
                .copied()
                .map(std::convert::Into::into),
            GlobalID::Enum(sym) => self[sym]
                .variant_ids_by_name
                .get(name)
                .copied()
                .map(std::convert::Into::into),
            GlobalID::Module(sym) => self[sym]
                .child_ids_by_name
                .get(name)
                .copied()
                .map(std::convert::Into::into),
            GlobalID::TypeAlias(sym) => match self[sym].alias {
                Type::PrimitiveType(..) => None,
                Type::TypedID(type_id) => {
                    self.search_child(type_id.into(), name, errors, symbol_states_by_id)
                }
            },
            GlobalID::OverloadSet(..) | GlobalID::EnumVariant(..) => None,
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
                    GlobalID::Struct(sym) => Some(Type::TypedID(sym.into())),
                    GlobalID::Enum(sym) => Some(Type::TypedID(sym.into())),
                    GlobalID::TypeAlias(sym) => Some(self[sym].alias),
                    GlobalID::Module(..)
                    | GlobalID::OverloadSet(..)
                    | GlobalID::EnumVariant(..) => {
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

    /// Retrieves the symbol ID of the given qualified identifier
    fn resolve_symbol(
        &mut self,
        referring_site: ScopedID,
        qualified_identifier: &QualifiedIdentifier,
        errors: &mut Vec<SymbolError>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
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
                errors.push(
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
                let result = self.search_child(
                    in_scope_id,
                    qualified_identifier.identifiers().first().span.str(),
                    errors,
                    symbol_states_by_id,
                );

                if let Some(result) = result {
                    break result;
                }

                // if the parent scope doesn't contain the symbol, go up the parent chain
                // if returns None, then the symbol is not found
                in_scope_id = if let Some(id) = self[in_scope_id].parent_scoped_id() {
                    id.into()
                } else {
                    errors.push(
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
        self.check_symbol_requirement(current_id.into(), errors, symbol_states_by_id);

        // search the symbol for the rest of the identifiers
        for identifier in qualified_identifier.identifiers().rest() {
            if let Some(result) = self.search_child(
                current_id,
                identifier.1.span.str(),
                errors,
                symbol_states_by_id,
            ) {
                // check if the symbol is accessible
                if !self.symbol_accessible(referring_site, result, self[current_id].accessibility())
                {
                    errors.push(
                        SymbolNotAccessible {
                            span: identifier.1.span.clone(),
                            global_id: result,
                        }
                        .into(),
                    );
                }

                current_id = result;
            } else {
                errors.push(
                    SymbolNotFound {
                        span: identifier.1.span.clone(),
                        in_scope_id: Some(current_id),
                    }
                    .into(),
                );
                return None;
            }
        }

        self.check_symbol_requirement(current_id.into(), errors, symbol_states_by_id);

        Some(current_id)
    }
}

macro_rules! impl_index {
    // for unit index (pub struct ID(Uid);)
    ($name:ident) => {
        paste! {
            impl Index<[< $name ID >]> for Table {
                type Output = $name;

                fn index(&self, id: [< $name ID >]) -> &Self::Output {
                    match self.symbols_by_id.get(&id.into()) {
                        Some(Symbol::$name(sym)) => sym,
                        _ => panic!("invalid id"),
                    }
                }
            }

            impl IndexMut<[< $name ID >]> for Table {
                fn index_mut(&mut self, id: [< $name ID >]) -> &mut Self::Output {
                    match self.symbols_by_id.get_mut(&id.into()) {
                        Some(Symbol::$name(sym)) => sym,
                        _ => panic!("invalid id"),
                    }
                }
            }
        }
    };

    // for enum index (pub enum ID { ID1(ID1), ID2(ID2), .. })
    ($name:ident, $($variants:ident),+) => {
        paste! {
            impl Index<[< $name ID >]> for Table {
                type Output = dyn $name;

                fn index(&self, id: [< $name ID >]) -> &Self::Output {
                    match self.symbols_by_id.get(&id.into()) {
                        $(
                            Some(Symbol::$variants(sym)) => sym,
                        )+
                        _ => panic!("invalid id"),
                    }
                }
            }

            impl IndexMut<[< $name ID >]> for Table {
                fn index_mut(&mut self, id: [< $name ID >]) -> &mut Self::Output {
                    match self.symbols_by_id.get_mut(&id.into()) {
                        $(
                            Some(Symbol::$variants(sym)) => sym,
                        )+
                        _ => panic!("invalid id"),
                    }
                }
            }
        }
    };
}

impl_index!(Struct);
impl_index!(OverloadSet);
impl_index!(Module);
impl_index!(Enum);
impl_index!(EnumVariant);
impl_index!(TypeAlias);
impl_index!(Overload);
impl_index!(Field);
impl_index!(Parameter);

impl_index!(Scoped, Module, Enum, Struct);
impl_index!(Global, Module, Enum, Struct, TypeAlias, OverloadSet);
impl_index!(Typed, Enum, Struct);

impl Index<ID> for Table {
    type Output = Symbol;

    fn index(&self, id: ID) -> &Self::Output { &self.symbols_by_id[&id] }
}
impl IndexMut<ID> for Table {
    fn index_mut(&mut self, id: ID) -> &mut Self::Output {
        self.symbols_by_id.get_mut(&id).unwrap()
    }
}

#[cfg(test)]
mod tests;
