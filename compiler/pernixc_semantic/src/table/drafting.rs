use std::collections::{hash_map::Entry, HashMap, HashSet};

use parking_lot::RwLock;
use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, target::ModuleTree};
use rayon::prelude::{IntoParallelIterator, ParallelIterator};

use super::{state::State, IndexMut, Table};
use crate::{
    arena::{Map, ID},
    entity::{constant, r#type},
    error::{self, ItemDuplication},
    symbol::{
        Accessibility, Constant, Enum, Function, GenericDeclaration, Module, ModuleMemberID,
        Struct, Trait, Type,
    },
    table::state,
};

pub(super) struct Context<'a, 'b> {
    pub(super) table: &'a RwLock<Table>,
    pub(super) handler: &'b dyn Handler<error::Error>,
    pub(super) usings_by_module_id: RwLock<HashMap<ID<Module>, Vec<syntax_tree::item::Using>>>,
    pub(super) implementations_by_module_id:
        RwLock<HashMap<ID<Module>, Vec<syntax_tree::item::Implements>>>,
}

macro_rules! draft_symbol {
    ($self:ident, $symbol:ident, $symbol_id:ident, $expr:expr, $parent_module_id:ident, $state_expr:expr, $state:ident) => {{
        let symbol_id = {
            let mut table = $self.table.write();

            let $symbol_id = ID::new(table.$symbol.len());

            table.$symbol.insert(RwLock::new($expr));

            table
                .state_manager
                .write()
                .$state
                .insert($symbol_id, State::Drafted($state_expr));

            $symbol_id
        };

        symbol_id
    }};
}

impl<'a, 'b> Context<'a, 'b> {
    pub(super) fn draft_struct(
        &self,
        syntax_tree: syntax_tree::item::Struct,
        parent_module_id: ID<Module>,
    ) -> ID<Struct> {
        draft_symbol!(
            self,
            structs,
            struct_id,
            Struct {
                id: struct_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                accessibility: Accessibility::from_syntax_tree(syntax_tree.access_modifier()),
                parent_module_id,
                generic_declaration: GenericDeclaration::default(),
                fields: Map::new(),
                span: Some(syntax_tree.signature().identifier().span.clone())
            },
            parent_module_id,
            syntax_tree,
            states_by_struct_id
        )
    }

    pub(super) fn draft_enum(
        &self,
        syntax_tree: syntax_tree::item::Enum,
        parent_module_id: ID<Module>,
    ) -> ID<Enum> {
        draft_symbol!(
            self,
            enums,
            enum_id,
            Enum {
                id: enum_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                accessibility: Accessibility::from_syntax_tree(syntax_tree.access_modifier()),
                parent_module_id,
                generic_declaration: GenericDeclaration::default(),
                variant_ids_by_name: HashMap::new(),
                span: Some(syntax_tree.signature().identifier().span.clone()),
            },
            parent_module_id,
            syntax_tree,
            states_by_enum_id
        )
    }

    pub(super) fn draft_constant(
        &self,
        syntax_tree: syntax_tree::item::Constant,
        parent_module_id: ID<Module>,
    ) -> ID<Constant> {
        draft_symbol!(
            self,
            constants,
            constant_id,
            Constant {
                id: constant_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                r#type: r#type::Type::Primitive(r#type::Primitive::Bool),
                constant: constant::Constant::Primitive(constant::Primitive::Bool(false)),
                span: Some(syntax_tree.signature().identifier().span.clone()),
                accessibility: Accessibility::from_syntax_tree(syntax_tree.access_modifier()),
                parent_module_id
            },
            parent_module_id,
            syntax_tree,
            states_by_constant_id
        )
    }

    pub(super) fn draft_type(
        &self,
        syntax_tree: syntax_tree::item::Type,
        parent_module_id: ID<Module>,
    ) -> ID<Type> {
        draft_symbol!(
            self,
            types,
            type_id,
            Type {
                id: type_id,
                generic_declaration: GenericDeclaration::default(),
                r#type: r#type::Type::Primitive(r#type::Primitive::Bool),
                span: Some(syntax_tree.signature().identifier().span.clone()),
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                accessibility: Accessibility::from_syntax_tree(syntax_tree.access_modifier()),
                parent_module_id
            },
            parent_module_id,
            syntax_tree,
            states_by_type_id
        )
    }

    pub(super) fn draft_trait(
        &self,
        syntax_tree: syntax_tree::item::Trait,
        parent_module_id: ID<Module>,
    ) -> ID<Trait> {
        draft_symbol!(
            self,
            traits,
            trait_id,
            Trait {
                id: trait_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                accessibility: Accessibility::from_syntax_tree(syntax_tree.access_modifier()),
                parent_module_id,
                generic_declaration: GenericDeclaration::default(),
                negative_implementations: Vec::new(),
                implementations: Vec::new(),
                span: Some(syntax_tree.signature().identifier().span.clone()),
                local_trait_member_ids_by_name: HashMap::new(),
            },
            parent_module_id,
            state::Trait {
                syntax_tree,
                implementations: Vec::new()
            },
            states_by_trait_id
        )
    }

    pub(super) fn draft_function(
        &self,
        syntax_tree: syntax_tree::item::Function,
        parent_module_id: ID<Module>,
    ) -> ID<Function> {
        draft_symbol!(
            self,
            functions,
            function_id,
            Function {
                id: function_id,
                accessibility: Accessibility::from_syntax_tree(syntax_tree.access_modifier()),
                parameters: Map::default(),
                parent_module_id,
                span: Some(syntax_tree.signature().identifier().span.clone()),
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                return_type: r#type::Type::Primitive(r#type::Primitive::Bool),
                generic_declaration: GenericDeclaration::default(),
            },
            parent_module_id,
            syntax_tree,
            states_by_function_id
        )
    }

    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    pub(super) fn draft_module(
        &self,
        syntax_tree: ModuleTree,
        name: String,
        parent_module_id: Option<ID<Module>>,
    ) -> ID<Module> {
        let module_id = {
            let mut table = self.table.write();

            let len = table.modules.len();
            table.modules.insert(RwLock::new(Module {
                id: ID::new(len),
                name,
                accessibility: syntax_tree
                    .signature()
                    .as_ref()
                    .map_or(Accessibility::Public, |x| {
                        Accessibility::from_syntax_tree(&x.access_modifier)
                    }),
                parent_module_id,
                module_child_ids_by_name: HashMap::new(),
                span: syntax_tree
                    .signature()
                    .as_ref()
                    .map(|x| x.signature.identifier().span()),
                usings: HashSet::new(),
            }))
        };

        let (_, content, submodule_by_name) = syntax_tree.dissolve();
        let (usings, items) = content.dissolve();

        self.usings_by_module_id.write().insert(module_id, usings);

        // draft submodules
        submodule_by_name
            .into_par_iter()
            .for_each(|(name, submodule)| {
                let submodule_id = self.draft_module(submodule, name.clone(), Some(module_id));

                self.table
                    .read()
                    .get_mut(module_id)
                    .unwrap()
                    .module_child_ids_by_name
                    .insert(name, ModuleMemberID::Module(submodule_id));
            });

        // add each item to the module
        items.into_par_iter().for_each(|item| {
            // create the symbol and get the id
            let module_member_id = match item {
                syntax_tree::item::Item::Trait(syn) => {
                    ModuleMemberID::Trait(self.draft_trait(syn, module_id))
                }
                syntax_tree::item::Item::Function(syn) => {
                    ModuleMemberID::Function(self.draft_function(syn, module_id))
                }
                syntax_tree::item::Item::Type(syn) => {
                    ModuleMemberID::Type(self.draft_type(syn, module_id))
                }
                syntax_tree::item::Item::Struct(syn) => {
                    ModuleMemberID::Struct(self.draft_struct(syn, module_id))
                }
                syntax_tree::item::Item::Implements(syn) => {
                    self.implementations_by_module_id
                        .write()
                        .entry(module_id)
                        .or_default()
                        .push(syn);
                    return;
                }
                syntax_tree::item::Item::Enum(syn) => {
                    ModuleMemberID::Enum(self.draft_enum(syn, module_id))
                }
                syntax_tree::item::Item::Module(..) => {
                    unreachable!("submodules should've been extracted out")
                }
                syntax_tree::item::Item::Constant(syn) => {
                    ModuleMemberID::Constant(self.draft_constant(syn, module_id))
                }
            };

            let mut table = self.table.write();

            let name = table
                .get_global(module_member_id.into())
                .unwrap()
                .name()
                .to_owned();

            #[allow(clippy::significant_drop_in_scrutinee)]
            let dup = match table
                .modules
                .get_mut(module_id)
                .unwrap()
                .write()
                .module_child_ids_by_name
                .entry(name)
            {
                Entry::Occupied(entry) => {
                    self.handler
                        .receive(error::Error::GlobalSymbolDuplication(ItemDuplication {
                            existing_symbol: *entry.get(),
                            new_symbol: module_member_id,
                            scope: module_id,
                        }));
                    true
                }
                Entry::Vacant(entry) => {
                    entry.insert(module_member_id);
                    false
                }
            };

            if dup {
                assert!(table
                    .state_manager
                    .write()
                    .remove_state(module_member_id.into()));
            }
        });

        module_id
    }
}
