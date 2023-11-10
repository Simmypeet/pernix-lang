use std::collections::{hash_map::Entry, HashMap};

use parking_lot::RwLock;
use pernixc_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};

use super::Table;
use crate::{
    arena::ID,
    error::{self, ItemDuplication},
    symbol::{
        Accessibility, Constant, Enum, Function, Module, ModuleMemberID, Struct, Trait, Type,
    },
};

pub(super) struct Context<'a, 'b> {
    pub(super) table: &'a RwLock<Table>,
    pub(super) handler: &'b dyn Handler<error::Error>,
    pub(super) usings_by_module_id: RwLock<HashMap<ID<Module>, Vec<syntax_tree::item::Using>>>,
    pub(super) implementations_by_module_id:
        RwLock<HashMap<ID<Module>, Vec<syntax_tree::item::Implements>>>,
}

impl<'a, 'b> Context<'a, 'b> {
    pub(super) fn draft_struct(
        &self,
        syntax_tree: syntax_tree::item::Struct,
        parent_module_id: ID<Module>,
    ) -> ID<Struct> {
        todo!()
    }

    pub(super) fn draft_enum(
        &self,
        syntax_tree: syntax_tree::item::Enum,
        parent_module_id: ID<Module>,
    ) -> ID<Enum> {
        todo!()
    }

    pub(super) fn draft_constant(
        &self,
        syntax_tree: syntax_tree::item::Constant,
        parent_module_id: ID<Module>,
    ) -> ID<Constant> {
        todo!()
    }

    pub(super) fn draft_type(
        &self,
        syntax_tree: syntax_tree::item::Type,
        parent_module_id: ID<Module>,
    ) -> ID<Type> {
        todo!()
    }

    pub(super) fn draft_trait(
        &self,
        syntax_tree: syntax_tree::item::Trait,
        parent_module_id: ID<Module>,
    ) -> ID<Trait> {
        todo!()
    }

    pub(super) fn draft_function(
        &self,
        syntax_tree: syntax_tree::item::Function,
        parent_module_id: ID<Module>,
    ) -> ID<Function> {
        todo!()
    }

    pub(super) fn draft_module(
        &self,
        syntax_tree: syntax_tree::item::ModuleContent,
        accessibility: Accessibility,
        name: String,
        parent_module_id: Option<ID<Module>>,
        span: Option<Span>,
    ) -> ID<Module> {
        let module_id = {
            let mut table = self.table.write();

            let len = table.modules.len();
            table.modules.insert(RwLock::new(Module {
                id: ID::new(len),
                name,
                accessibility,
                parent_module_id,
                module_child_ids_by_name: HashMap::new(),
                span,
            }))
        };

        let (usings, items) = syntax_tree.dissolve();
        self.usings_by_module_id.write().insert(module_id, usings);

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
                syntax_tree::item::Item::Module(syn) => {
                    let (access_modifier, identifier, content) = syn.dissolve();

                    ModuleMemberID::Module(
                        self.draft_module(
                            content
                                .into_inline()
                                .expect("should've been inline")
                                .dissolve()
                                .1,
                            match access_modifier {
                                syntax_tree::AccessModifier::Public(_) => Accessibility::Public,
                                syntax_tree::AccessModifier::Private(_) => Accessibility::Private,
                                syntax_tree::AccessModifier::Internal(_) => Accessibility::Internal,
                            },
                            identifier.identifier().span.str().to_owned(),
                            Some(module_id),
                            Some(identifier.identifier().span()),
                        ),
                    )
                }
                syntax_tree::item::Item::Constant(syn) => {
                    ModuleMemberID::Constant(self.draft_constant(syn, module_id))
                }
            };

            let name = self
                .table
                .read()
                .get_global(module_member_id.into())
                .unwrap()
                .name()
                .to_owned();

            #[allow(clippy::significant_drop_in_scrutinee)]
            match self
                .table
                .write()
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
                }
                Entry::Vacant(entry) => {
                    entry.insert(module_member_id);
                }
            }
        });

        module_id
    }
}
