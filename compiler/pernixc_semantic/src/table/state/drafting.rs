use std::collections::{hash_map::Entry, HashMap, HashSet};

use parking_lot::RwLock;
use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, target::ModuleTree, ConnectedList};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use super::building::Building;
use crate::{
    arena::{Map, ID},
    error::{self, GlobalRedefinition},
    semantic::term::r#type,
    symbol::{
        Accessibility, Constant, ConstantData, Enum, Function, FunctionData,
        GenericDeclaration, GlobalID, Module, ModuleMemberID, Struct, Trait,
        TraitConstant, TraitConstantData, TraitFunction, TraitFunctionData,
        TraitMemberID, TraitType, TraitTypeData, Type, TypeData, Variant,
    },
    table::{self, Element, RwLockContainer, Table},
};

#[derive(Debug, Default)]
pub struct Drafting {
    pub usings_by_module_id: HashMap<ID<Module>, Vec<syntax_tree::item::Using>>,
    pub implementations_by_module_id:
        HashMap<ID<Module>, Vec<syntax_tree::item::Implementation>>,
    pub building: Building,
}

impl table::State for Drafting {
    type Container = RwLockContainer;
}

impl Table<Drafting> {
    #[allow(clippy::significant_drop_tightening)]
    fn insert_member_id_to_map<
        MemberID: Into<GlobalID> + Copy,
        Parent: Element,
    >(
        &self,
        map_fn: impl FnOnce(&mut Parent) -> &mut HashMap<String, MemberID>,
        member_id: MemberID,
        parent_id: ID<Parent>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        ID<Parent>: Into<GlobalID>,
    {
        let parent_arena = Parent::get_arena(self);
        let member_name =
            self.get_global(member_id.into()).unwrap().name().to_owned();

        let mut parent_write = parent_arena.get(parent_id).unwrap().write();
        let map = map_fn(&mut parent_write);

        #[allow(clippy::significant_drop_in_scrutinee)]
        match map.entry(member_name) {
            Entry::Occupied(entry) => {
                handler.receive(Box::new(GlobalRedefinition {
                    existing_global_id: (*entry.get()).into(),
                    new_global_id: member_id.into(),
                    in_global_id: parent_id.into(),
                }));
            }
            Entry::Vacant(entry) => {
                entry.insert(member_id);
            }
        }
    }

    fn draft_trait_function(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::TraitFunction,
        parent_trait_id: ID<Trait>,
    ) -> ID<TraitFunction> {
        let mut table_write = table.write();
        let id = table_write.representation.trait_functions.insert_with(|id| {
            RwLock::new(TraitFunction {
                id,
                parameters: Map::new(),
                parent_id: parent_trait_id,
                span: Some(syntax_tree.signature().identifier().span.clone()),
                name: syntax_tree
                    .signature()
                    .identifier()
                    .span
                    .str()
                    .to_owned(),
                return_type: r#type::Type::default(),
                generic_declaration: GenericDeclaration::default(),
                data: TraitFunctionData,
            })
        });
        assert!(table_write.state.building.draft_symbol(id, syntax_tree));
        drop(table_write);

        id
    }

    fn draft_trait_type(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::TraitType,
        parent_trait_id: ID<Trait>,
    ) -> ID<TraitType> {
        let mut table_write = table.write();
        let id = table_write.representation.trait_types.insert_with(|id| {
            RwLock::new(TraitType {
                id,
                parent_id: parent_trait_id,
                span: Some(syntax_tree.signature().identifier().span.clone()),
                name: syntax_tree
                    .signature()
                    .identifier()
                    .span
                    .str()
                    .to_owned(),
                generic_declaration: GenericDeclaration::default(),
                data: TraitTypeData,
            })
        });
        assert!(table_write.state.building.draft_symbol(id, syntax_tree));
        drop(table_write);

        id
    }

    fn draft_trait_constant(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::TraitConstant,
        parent_trait_id: ID<Trait>,
    ) -> ID<TraitConstant> {
        let mut table_write = table.write();
        let id = table_write.representation.trait_constants.insert_with(|id| {
            RwLock::new(TraitConstant {
                id,
                parent_id: parent_trait_id,
                span: Some(syntax_tree.signature().identifier().span.clone()),
                name: syntax_tree
                    .signature()
                    .identifier()
                    .span
                    .str()
                    .to_owned(),
                r#type: r#type::Type::default(),
                generic_declaration: GenericDeclaration::default(),
                data: TraitConstantData,
            })
        });
        assert!(table_write.state.building.draft_symbol(id, syntax_tree));
        drop(table_write);

        id
    }

    fn draft_trait(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::Trait,
        parent_module_id: ID<Module>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> ID<Trait> {
        let (access_modifier, signature, body) = syntax_tree.dissolve();
        let (_, member_list, _) = body.dissolve();

        let mut table_write = table.write();
        let id = table_write.representation.traits.insert_with(|id| {
            RwLock::new(Trait {
                id,
                name: signature.identifier().span.str().to_owned(),
                accessibility: Accessibility::from_syntax_tree(
                    &access_modifier,
                ),
                parent_module_id,
                generic_declaration: GenericDeclaration::default(),
                negative_implementations: Vec::new(),
                implementations: Vec::new(),
                span: Some(signature.identifier().span.clone()),
                trait_member_ids_by_name: HashMap::new(),
            })
        });
        assert!(table_write.state.building.draft_symbol(id, signature));
        drop(table_write);

        for trait_member in member_list {
            let trait_member_id = match trait_member {
                syntax_tree::item::TraitMember::Function(syn) => {
                    TraitMemberID::Function(Self::draft_trait_function(
                        table, syn, id,
                    ))
                }
                syntax_tree::item::TraitMember::Type(syn) => {
                    TraitMemberID::Type(Self::draft_trait_type(table, syn, id))
                }
                syntax_tree::item::TraitMember::Constant(syn) => {
                    TraitMemberID::Constant(Self::draft_trait_constant(
                        table, syn, id,
                    ))
                }
            };

            table.read().insert_member_id_to_map(
                |trait_sym| &mut trait_sym.trait_member_ids_by_name,
                trait_member_id,
                id,
                handler,
            );
        }

        id
    }

    fn draft_function(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::Function,
        parent_module_id: ID<Module>,
    ) -> ID<Function> {
        let mut table_write = table.write();
        let id = table_write.representation.functions.insert_with(|id| {
            RwLock::new(Function {
                id,
                parameters: Map::new(),
                parent_id: parent_module_id,
                span: Some(syntax_tree.signature().identifier().span.clone()),
                name: syntax_tree
                    .signature()
                    .identifier()
                    .span
                    .str()
                    .to_owned(),
                return_type: r#type::Type::default(),
                generic_declaration: GenericDeclaration::default(),
                data: FunctionData {
                    accessibility: Accessibility::from_syntax_tree(
                        syntax_tree.access_modifier(),
                    ),
                    const_function: syntax_tree.const_keyword().is_some(),
                },
            })
        });
        assert!(table_write.state.building.draft_symbol(id, syntax_tree));
        drop(table_write);

        id
    }

    fn draft_type(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::Type,
        parent_module_id: ID<Module>,
    ) -> ID<Type> {
        let mut table_write = table.write();
        let id = table_write.representation.types.insert_with(|id| {
            RwLock::new(Type {
                id,
                generic_declaration: GenericDeclaration::default(),
                parent_id: parent_module_id,
                span: Some(syntax_tree.signature().identifier().span.clone()),
                name: syntax_tree
                    .signature()
                    .identifier()
                    .span
                    .str()
                    .to_owned(),
                data: TypeData {
                    accessibility: Accessibility::from_syntax_tree(
                        syntax_tree.access_modifier(),
                    ),
                    r#type: r#type::Type::default(),
                },
            })
        });
        assert!(table_write.state.building.draft_symbol(id, syntax_tree));
        drop(table_write);

        id
    }

    fn draft_struct(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::Struct,
        parent_module_id: ID<Module>,
    ) -> ID<Struct> {
        let mut table_write = table.write();
        let id = table_write.representation.structs.insert_with(|id| {
            RwLock::new(Struct {
                id,
                name: syntax_tree
                    .signature()
                    .identifier()
                    .span
                    .str()
                    .to_owned(),
                accessibility: Accessibility::from_syntax_tree(
                    syntax_tree.access_modifier(),
                ),
                parent_module_id,
                generic_declaration: GenericDeclaration::default(),
                fields: Map::new(),
                implementations: HashSet::default(),
                span: Some(syntax_tree.signature().identifier().span.clone()),
            })
        });
        assert!(table_write.state.building.draft_symbol(id, syntax_tree));
        drop(table_write);

        id
    }

    fn draft_enum(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::Enum,
        parent_module_id: ID<Module>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> ID<Enum> {
        let (access_modifier, signature, body) = syntax_tree.dissolve();
        let mut table_write = table.write();

        let id = table_write.representation.enums.insert_with(|id| {
            RwLock::new(Enum {
                id,
                name: signature.identifier().span.str().to_owned(),
                accessibility: Accessibility::from_syntax_tree(
                    &access_modifier,
                ),
                parent_module_id,
                generic_declaration: GenericDeclaration::default(),
                variant_ids_by_name: HashMap::new(),
                implementations: HashSet::new(),
                span: Some(signature.identifier().span.clone()),
            })
        });
        assert!(table_write.state.building.draft_symbol(id, signature));
        drop(table_write);

        for variant in
            body.dissolve().1.into_iter().flat_map(ConnectedList::into_elements)
        {
            let variant_id = Self::draft_variant(table, variant, id);

            table.read().insert_member_id_to_map(
                |enum_sym| &mut enum_sym.variant_ids_by_name,
                variant_id,
                id,
                handler,
            );
        }

        id
    }

    fn draft_variant(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::Variant,
        parent_enum_id: ID<Enum>,
    ) -> ID<Variant> {
        let mut table_write = table.write();
        let id = table_write.representation.variants.insert_with(|id| {
            RwLock::new(Variant {
                id,
                name: syntax_tree.identifier().span.str().to_owned(),
                span: Some(syntax_tree.identifier().span.clone()),
                associated_type: None,
                parent_enum_id,
            })
        });
        assert!(table_write.state.building.draft_symbol(id, syntax_tree));
        drop(table_write);

        id
    }

    fn draft_constant(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::Constant,
        parent_module_id: ID<Module>,
    ) -> ID<Constant> {
        let mut table_write = table.write();
        let id = table_write.representation.constants.insert_with(|id| {
            RwLock::new(Constant {
                id,
                name: syntax_tree
                    .signature()
                    .identifier()
                    .span
                    .str()
                    .to_owned(),
                span: Some(syntax_tree.signature().identifier().span.clone()),
                r#type: r#type::Type::default(),
                generic_declaration: GenericDeclaration::default(),
                parent_id: parent_module_id,
                data: ConstantData {
                    accessibility: Accessibility::from_syntax_tree(
                        syntax_tree.access_modifier(),
                    ),
                },
            })
        });
        assert!(table_write.state.building.draft_symbol(id, syntax_tree));
        drop(table_write);

        id
    }

    pub(in crate::table) fn draft_module(
        table: &RwLock<Self>,
        syntax_tree: ModuleTree,
        name: String,
        parent_module_id: Option<ID<Module>>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> ID<Module> {
        let module_id = {
            table.write().representation.modules.insert_with(|id| {
                RwLock::new(Module {
                    id,
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
                })
            })
        };

        let (_, content, submodule_by_name) = syntax_tree.dissolve();
        let (usings, items) = content.dissolve();

        table.write().state.usings_by_module_id.insert(module_id, usings);

        // recursive draft submodules
        submodule_by_name.into_par_iter().for_each(|(name, submodule)| {
            let submodule_id = Self::draft_module(
                table,
                submodule,
                name.clone(),
                Some(module_id),
                handler,
            );

            table
                .write()
                .representation
                .modules
                .get(module_id)
                .unwrap()
                .write()
                .module_child_ids_by_name
                .insert(name, ModuleMemberID::Module(submodule_id));
        });

        // add each item to the module
        items.into_par_iter().for_each(|item| {
            // create the symbol and get the id
            let module_member_id = match item {
                syntax_tree::item::Item::Trait(syn) => ModuleMemberID::Trait(
                    Self::draft_trait(table, syn, module_id, handler),
                ),
                syntax_tree::item::Item::Function(syn) => {
                    ModuleMemberID::Function(Self::draft_function(
                        table, syn, module_id,
                    ))
                }
                syntax_tree::item::Item::Type(syn) => ModuleMemberID::Type(
                    Self::draft_type(table, syn, module_id),
                ),
                syntax_tree::item::Item::Struct(syn) => ModuleMemberID::Struct(
                    Self::draft_struct(table, syn, module_id),
                ),
                syntax_tree::item::Item::Implementation(syn) => {
                    table
                        .write()
                        .state
                        .implementations_by_module_id
                        .entry(module_id)
                        .or_default()
                        .push(syn);
                    return;
                }
                syntax_tree::item::Item::Enum(syn) => ModuleMemberID::Enum(
                    Self::draft_enum(table, syn, module_id, handler),
                ),
                syntax_tree::item::Item::Module(_) => {
                    unreachable!("submodules should've been extracted out")
                }
                syntax_tree::item::Item::Constant(syn) => {
                    ModuleMemberID::Constant(Self::draft_constant(
                        table, syn, module_id,
                    ))
                }
            };

            table.read().insert_member_id_to_map(
                |module| &mut module.module_child_ids_by_name,
                module_member_id,
                module_id,
                handler,
            );
        });

        module_id
    }
}

#[cfg(test)]
mod tests;
