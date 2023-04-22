use std::{collections::HashMap, hash::Hash};

use pernixc_syntax::{
    syntax_tree::item::{
        Enum as EnumSyntaxTree, Function as FunctionSyntaxTree, Item as ItemSyntaxTree,
        Struct as StructSyntaxTree, TypeAlias as TypeAliasSyntaxTree,
    },
    target_parsing::TargetParsing,
};

use super::{
    AccessModifier, EnumData, FunctionOverloadSetID, IDMap, Item, ModuleData, ModuleID, Overload,
    OverloadData, OverloadID, StructData, SymbolData, SymbolWithData, Table, TypeAliasData, ID,
};
use crate::symbol::{
    errors::{SymbolError, SymbolRedifinition},
    item::FunctionOverloadSetData,
    ty::PrimitiveType,
    UniqueIdentifier,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum SymbolState {
    Unconstructed,
    Constructing,
    Constructed,
}

pub(super) struct Builder {
    symbol_states: HashMap<ID, SymbolState>,
    pub(super) table: Table,
    errors: Vec<SymbolError>,
}

impl Builder {
    pub(super) fn new() -> Self {
        Self {
            symbol_states: HashMap::new(),
            table: Table {
                items_by_id: HashMap::new(),
                root_ids_by_name: HashMap::new(),
            },
            errors: Vec::new(),
        }
    }

    pub(super) fn generate_symbols(&mut self, file_parsing: TargetParsing) {
        let files = file_parsing
            .destruct()
            .into_iter()
            .map(pernixc_syntax::target_parsing::FileParsing::destruct)
            .collect::<Vec<_>>();

        for (source_file, file_syntax_tree, _) in files {
            for item in file_syntax_tree.destruct() {
                // the parent module id
                let module_id = self
                    .table
                    .get_id_by_full_name(
                        source_file
                            .module_heirarchy()
                            .iter()
                            .map(std::string::String::as_str),
                    )
                    .unwrap()
                    .into_module()
                    .unwrap();

                let identifier = {
                    let (is_function, identifier) = match &item {
                        ItemSyntaxTree::Struct(sym) => (false, sym.identifier()),
                        ItemSyntaxTree::Enum(sym) => (false, sym.identifier()),
                        ItemSyntaxTree::Function(sym) => (true, sym.identifier()),
                        ItemSyntaxTree::Module(..) => continue,
                        ItemSyntaxTree::TypeAlias(sym) => {
                            (false, sym.type_without_access_modifier().identifier())
                        }
                    };

                    // check for symbol redefinition
                    if let Some(available_symbol_id) = self.table.get_id_by_full_name(
                        source_file
                            .module_heirarchy()
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
                            self.errors.push(
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
                        .add_symbol(self.create_struct_data(syntax_tree, module_id))
                        .into(),
                    ItemSyntaxTree::Enum(syntax_tree) => self
                        .add_symbol(self.create_enum_data(syntax_tree, module_id))
                        .into(),
                    ItemSyntaxTree::TypeAlias(syntax_tree) => self
                        .add_symbol(self.create_type_alias_data(syntax_tree, module_id))
                        .into(),
                    ItemSyntaxTree::Function(syntax_tree) => self
                        .handle_function_overload_set(syntax_tree, module_id)
                        .into(),
                    ItemSyntaxTree::Module(_) => continue,
                };

                // add to the parent module
                self.table[module_id]
                    .children_ids_by_name
                    .insert(identifier, id);

                // add symbol state
                self.symbol_states.insert(id, SymbolState::Unconstructed);
            }
        }
    }

    fn handle_function_overload_set(
        &mut self,
        syntax_tree: FunctionSyntaxTree,
        parent: ModuleID,
    ) -> FunctionOverloadSetID {
        let overload_set_id = if let Some(id) = self.table[parent]
            .children_ids_by_name
            .get(syntax_tree.identifier().span().str())
        {
            id.into_function_overload_set().unwrap()
        } else {
            self.add_symbol(FunctionOverloadSetData {
                qualified_name: self.table[parent]
                    .qualified_name()
                    .iter()
                    .map(std::borrow::ToOwned::to_owned)
                    .chain(std::iter::once(
                        syntax_tree.identifier().span().str().to_owned(),
                    ))
                    .collect(),
                parent,
                overloads_by_id: HashMap::new(),
            })
        };

        let overload_id = OverloadID::fresh();

        self.table[overload_set_id]
            .overloads_by_id
            .insert(overload_id, Overload {
                data: OverloadData {
                    overload_set_id,
                    return_type: PrimitiveType::Void.into(), // to be filled later
                    parameters: IDMap::new(),                // to be filled later
                    syntax_tree,
                },
                id: overload_id,
            });

        overload_set_id
    }

    fn create_type_alias_data(
        &self,
        syntax_tree: TypeAliasSyntaxTree,
        parent: ModuleID,
    ) -> TypeAliasData {
        TypeAliasData {
            qualified_name: self.table[parent]
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
            access_modifier: AccessModifier::from_syntax_tree(syntax_tree.access_modifier()),
            parent,
            syntax_tree,
        }
    }

    fn create_enum_data(&self, syntax_tree: EnumSyntaxTree, parent: ModuleID) -> EnumData {
        EnumData {
            qualified_name: self.table[parent]
                .qualified_name()
                .iter()
                .map(std::borrow::ToOwned::to_owned)
                .chain(std::iter::once(
                    syntax_tree.identifier().span().str().to_owned(),
                ))
                .collect(),
            variant_ids_by_name: HashMap::new(), // To be filled later
            access_modifier: AccessModifier::from_syntax_tree(syntax_tree.access_modifier()),
            parent,
            syntax_tree,
        }
    }

    fn create_struct_data(&self, syntax_tree: StructSyntaxTree, parent: ModuleID) -> StructData {
        StructData {
            qualified_name: self.table[parent]
                .qualified_name()
                .iter()
                .map(std::borrow::ToOwned::to_owned)
                .chain(std::iter::once(
                    syntax_tree.identifier().span().str().to_owned(),
                ))
                .collect(),
            field_member_map: IDMap::new(), // To be filled later
            type_alias_member_ids_by_name: HashMap::new(), // To be filled later
            access_modifier: AccessModifier::from_syntax_tree(syntax_tree.access_modifier()),
            parent,
            syntax_tree,
        }
    }

    pub(super) fn generate_modules(&mut self, target: &TargetParsing) {
        // iteration is sorted by the depth of the module heirarchy
        for file in target.file_parsings() {
            let parent = if file.source_file().module_heirarchy().len() == 1 {
                None
            } else {
                // the parent module name is the full name of the target without the last part
                Some(
                    self.table
                        .get_id_by_full_name(
                            file.source_file().module_heirarchy()
                                [..file.source_file().module_heirarchy().len() - 1]
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
                qualified_name: file.source_file().module_heirarchy().clone(),
                children_ids_by_name: HashMap::new(),
                access_modifier: file
                    .access_modifier()
                    .as_ref()
                    .map_or(AccessModifier::Public, AccessModifier::from_syntax_tree),
            };

            let id = self.add_symbol(module_data);

            // add the module to the parent module
            if let Some(parent) = parent {
                self.table[parent].children_ids_by_name.insert(
                    file.source_file()
                        .module_heirarchy()
                        .last()
                        .unwrap()
                        .clone(),
                    id.into(),
                );
            } else {
                // add to the root
                self.table.root_ids_by_name.insert(
                    file.source_file()
                        .module_heirarchy()
                        .last()
                        .unwrap()
                        .clone(),
                    id,
                );
            }
        }
    }

    fn add_symbol<T: SymbolData>(&mut self, data: T) -> <T as SymbolData>::ID
    where
        <T as SymbolData>::ID: Into<ID>,
        SymbolWithData<T>: Into<Item>,
    {
        let id = T::ID::fresh();
        let symbol = SymbolWithData { data, id };

        self.table.items_by_id.insert(id.into(), symbol.into());

        id
    }
}

#[cfg(test)]
mod tests;
