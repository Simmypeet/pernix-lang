use std::collections::{hash_map::Entry, HashMap, HashSet};

use parking_lot::{RwLock, RwLockWriteGuard};
use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, target::ModuleTree, ConnectedList};
use rayon::prelude::{IntoParallelIterator, ParallelIterator};

use super::{
    state::{self},
    IndexMut, Table,
};
use crate::{
    arena::{Map, ID},
    error::{
        self, GlobalRedefinition, TraitMemberAndImplementationMemberMismatched,
        TraitMemberNotImplemented,
    },
    semantic::term::{constant, r#type, GenericArguments},
    symbol::{
        self, Accessibility, Constant, Enum, Function, GenericDeclaration, GlobalID,
        ImplementationSignature, Module, ModuleMemberID, Struct, Trait, TraitConstant,
        TraitFunction, TraitMemberID, TraitType, Type, Variant,
    },
    table::Index,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Implementation {
    pub(super) in_module: ID<Module>,
    pub(super) syntax_tree: syntax_tree::item::Implementation,
}

pub(super) struct Context<'a, 'b> {
    pub(super) table: &'a RwLock<Table>,
    pub(super) handler: &'b dyn Handler<error::Error>,
    pub(super) usings_by_module_id: RwLock<HashMap<ID<Module>, Vec<syntax_tree::item::Using>>>,
    pub(super) implementations_by_module_id:
        RwLock<HashMap<ID<Module>, Vec<syntax_tree::item::Implementation>>>,
    pub(super) builder: RwLock<state::Builder>,
}

impl Table {
    pub(super) fn draft_implementation_function(
        &mut self,
        syntax_tree: syntax_tree::item::ImplementationFunction,
        parent_implementation_id: ID<symbol::Implementation>,
        builder: &RwLock<state::Builder>,
    ) -> ID<ImplementationFunction> {
        builder.write().draft_symbol(
            self,
            |id, syntax_tree| ImplementationFunction {
                id,
                parent_implementation_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                generic_declaration: GenericDeclaration::default(),
                span: Some(syntax_tree.signature().identifier().span()),
                parameters: Map::new(),
                return_type: r#type::Type::default(),
            },
            syntax_tree,
        )
    }

    pub(super) fn draft_implementation_type(
        &mut self,
        syntax_tree: syntax_tree::item::ImplementationType,
        parent_implementation_id: ID<symbol::Implementation>,
        builder: &RwLock<state::Builder>,
    ) -> ID<ImplementationType> {
        builder.write().draft_symbol(
            self,
            |id, syntax_tree| ImplementationType {
                id,
                parent_implementation_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                generic_declaration: GenericDeclaration::default(),
                span: Some(syntax_tree.signature().identifier().span()),
                r#type: r#type::Type::default(),
            },
            syntax_tree,
        )
    }

    #[allow(clippy::significant_drop_tightening)]
    fn insert_member_id_to_map<MemberID: Into<GlobalID> + Copy, ParentID: Into<GlobalID> + Copy>(
        &self,
        map_fn: impl for<'a> FnOnce(
            &'a mut RwLockWriteGuard<<Self as Index<ParentID>>::Output>,
        ) -> &'a mut HashMap<String, MemberID>,
        id: MemberID,
        parent_id: ParentID,
        handler: &dyn Handler<error::Error>,
    ) where
        Self: IndexMut<ParentID>,
    {
        let name = self.get_global(id.into()).unwrap().name().to_owned();
        let mut parent = self.get_mut(parent_id).unwrap();
        let map = map_fn(&mut parent);

        #[allow(clippy::significant_drop_in_scrutinee)]
        match map.entry(name) {
            Entry::Occupied(entry) => {
                handler.receive(error::Error::GlobalRedefinition(GlobalRedefinition {
                    existing_global_id: (*entry.get()).into(),
                    new_global_id: id.into(),
                    in_global_id: parent_id.into(),
                }));
            }
            Entry::Vacant(entry) => {
                entry.insert(id);
            }
        }
    }

    pub(super) fn draft_implementation_constant(
        &mut self,
        syntax_tree: syntax_tree::item::ImplementationConstant,
        parent_implementation_id: ID<symbol::Implementation>,
        builder: &RwLock<state::Builder>,
    ) -> ID<ImplementationConstant> {
        builder.write().draft_symbol(
            self,
            |id, syntax_tree| ImplementationConstant {
                id,
                parent_implementation_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                r#type: r#type::Type::default(),
                constant: constant::Constant::default(),
                span: Some(syntax_tree.signature().identifier().span()),
            },
            syntax_tree,
        )
    }

    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    pub(super) fn draft_implementation(
        &mut self,
        implementation_syntax_tree: syntax_tree::item::Implementation,
        declared_in_module_id: ID<Module>,
        parent_trait_id: ID<Trait>,
        builder: &RwLock<state::Builder>,
        handler: &dyn Handler<error::Error>,
    ) {
        let (signature, kind) = implementation_syntax_tree.dissolve();
        let name = self.get(parent_trait_id).unwrap().name.clone();

        match kind {
            syntax_tree::item::ImplementationKind::Negative(..) => {
                let negative_implementation_id = builder.write().draft_symbol(
                    self,
                    |id, signature| NegativeImplementation {
                        id,
                        signature: ImplementationSignature {
                            arguments: GenericArguments::default(),
                            trait_id: parent_trait_id,
                            span: Some(signature.qualified_identifier().span()),
                            generic_declaration: GenericDeclaration::default(),
                            declared_in: declared_in_module_id,
                            trait_name: name,
                        },
                    },
                    signature,
                );

                self.get_mut(parent_trait_id)
                    .unwrap()
                    .negative_implementations
                    .push(negative_implementation_id);
            }
            syntax_tree::item::ImplementationKind::Positive(body) => {
                let (_, members, _) = body.dissolve();

                let implementation_id = builder.write().draft_symbol(
                    self,
                    |id, signature| symbol::Implementation {
                        id,
                        signature: ImplementationSignature {
                            arguments: GenericArguments::default(),
                            trait_id: parent_trait_id,
                            span: Some(signature.qualified_identifier().span()),
                            generic_declaration: GenericDeclaration::default(),
                            declared_in: declared_in_module_id,
                            trait_name: name,
                        },
                        is_const: signature.const_keyword().is_some(),
                        implementation_member_ids_by_name: HashMap::new(),
                        implementation_type_ids_by_trait_type_id: HashMap::new(),
                        implementation_function_ids_by_trait_function_id: HashMap::new(),
                        implementation_constant_ids_by_trait_constant_id: HashMap::new(),
                    },
                    signature,
                );

                self.get_mut(parent_trait_id)
                    .unwrap()
                    .implementations
                    .push(implementation_id);

                for member in members {
                    let member_id = match member {
                        syntax_tree::item::ImplementationMember::Type(syn) => {
                            ImplementationMemberID::Type(self.draft_implementation_type(
                                syn,
                                implementation_id,
                                builder,
                            ))
                        }
                        syntax_tree::item::ImplementationMember::Function(syn) => {
                            ImplementationMemberID::Function(self.draft_implementation_function(
                                syn,
                                implementation_id,
                                builder,
                            ))
                        }
                        syntax_tree::item::ImplementationMember::Constant(syn) => {
                            ImplementationMemberID::Constant(self.draft_implementation_constant(
                                syn,
                                implementation_id,
                                builder,
                            ))
                        }
                    };

                    self.insert_member_id_to_map(
                        |symbol| &mut symbol.implementation_member_ids_by_name,
                        member_id,
                        implementation_id,
                        handler,
                    );
                }

                let mut implementation_symbol = self.get_mut(implementation_id).unwrap();

                #[allow(clippy::significant_drop_in_scrutinee)]
                for (name, trait_member_id) in self
                    .get(parent_trait_id)
                    .unwrap()
                    .trait_member_ids_by_name
                    .iter()
                    .map(|(name, id)| (name, *id))
                {
                    let implementation_member_id = implementation_symbol
                        .implementation_member_ids_by_name
                        .get(name)
                        .copied();

                    // check if the trait member is implemented
                    match (trait_member_id, implementation_member_id) {
                        (
                            TraitMemberID::Type(trait_id),
                            Some(ImplementationMemberID::Type(implementation_id)),
                        ) => {
                            assert!(implementation_symbol
                                .implementation_type_ids_by_trait_type_id
                                .insert(trait_id, implementation_id)
                                .is_none());
                        }
                        (
                            TraitMemberID::Function(trait_id),
                            Some(ImplementationMemberID::Function(implementation_id)),
                        ) => {
                            assert!(implementation_symbol
                                .implementation_function_ids_by_trait_function_id
                                .insert(trait_id, implementation_id)
                                .is_none());
                        }
                        (
                            TraitMemberID::Constant(trait_id),
                            Some(ImplementationMemberID::Constant(implementation_id)),
                        ) => {
                            assert!(implementation_symbol
                                .implementation_constant_ids_by_trait_constant_id
                                .insert(trait_id, implementation_id)
                                .is_none());
                        }

                        (trait_member_id, None) => handler.receive(
                            error::Error::TraitMemberNotImplemented(TraitMemberNotImplemented {
                                trait_member_id,
                                implementation_id,
                            }),
                        ),

                        (trait_member_id, Some(implementation_member_id)) => handler.receive(
                            error::Error::TraitMemberAndImplementationMemberMismatched(
                                TraitMemberAndImplementationMemberMismatched {
                                    trait_member_id,
                                    implementation_member_id,
                                },
                            ),
                        ),
                    }
                }
            }
        }
    }
}

impl<'a, 'b> Context<'a, 'b> {
    #[allow(clippy::significant_drop_tightening)]
    pub(super) fn draft_trait_type(
        &self,
        syntax_tree: syntax_tree::item::TraitType,
        parent_trait_id: ID<Trait>,
    ) -> ID<TraitType> {
        self.builder.write().draft_symbol(
            &mut self.table.write(),
            |id, syntax_tree| TraitType {
                id,
                parent_trait_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                generic_declaration: GenericDeclaration::default(),
                span: Some(syntax_tree.signature().identifier().span()),
            },
            syntax_tree,
        )
    }

    #[allow(clippy::significant_drop_tightening)]
    pub(super) fn draft_trait_function(
        &self,
        syntax_tree: syntax_tree::item::TraitFunction,
        parent_trait_id: ID<Trait>,
    ) -> ID<TraitFunction> {
        self.builder.write().draft_symbol(
            &mut self.table.write(),
            |id, syntax_tree| TraitFunction {
                id,
                parent_trait_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                generic_declaration: GenericDeclaration::default(),
                span: Some(syntax_tree.signature().identifier().span()),
                parameters: Map::new(),
                return_type: r#type::Type::default(),
            },
            syntax_tree,
        )
    }

    #[allow(clippy::significant_drop_tightening)]
    pub(super) fn draft_trait_constant(
        &self,
        syntax_tree: syntax_tree::item::TraitConstant,
        parent_trait_id: ID<Trait>,
    ) -> ID<TraitConstant> {
        self.builder.write().draft_symbol(
            &mut self.table.write(),
            |id, syntax_tree| TraitConstant {
                id,
                parent_trait_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                r#type: r#type::Type::default(),
                span: Some(syntax_tree.signature().identifier().span()),
            },
            syntax_tree,
        )
    }

    #[allow(clippy::significant_drop_tightening)]
    pub(super) fn draft_struct(
        &self,
        syntax_tree: syntax_tree::item::Struct,
        parent_module_id: ID<Module>,
    ) -> ID<Struct> {
        self.builder.write().draft_symbol(
            &mut self.table.write(),
            |id, syntax_tree| Struct {
                id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                accessibility: Accessibility::from_syntax_tree(syntax_tree.access_modifier()),
                parent_module_id,
                generic_declaration: GenericDeclaration::default(),
                fields: Map::new(),
                span: Some(syntax_tree.signature().identifier().span.clone()),
            },
            syntax_tree,
        )
    }

    #[allow(clippy::significant_drop_tightening)]
    pub(super) fn draft_variant(
        &self,
        syntax_tree: syntax_tree::item::Variant,
        parent_enum_id: ID<Enum>,
    ) -> ID<Variant> {
        self.builder.write().draft_symbol(
            &mut self.table.write(),
            |id, syntax_tree| Variant {
                associated_type: None,
                id,
                name: syntax_tree.identifier().span.str().to_owned(),
                parent_enum_id,
                span: Some(syntax_tree.identifier().span.clone()),
            },
            syntax_tree,
        )
    }

    #[allow(clippy::significant_drop_tightening)]
    pub(super) fn draft_enum(
        &self,
        syntax_tree: syntax_tree::item::Enum,
        parent_module_id: ID<Module>,
    ) -> ID<Enum> {
        let (access_modifier, signature, body) = syntax_tree.dissolve();

        let enum_id = {
            self.builder.write().draft_symbol(
                &mut self.table.write(),
                |id, signature| Enum {
                    id,
                    name: signature.identifier().span.str().to_owned(),
                    accessibility: Accessibility::from_syntax_tree(&access_modifier),
                    parent_module_id,
                    generic_declaration: GenericDeclaration::default(),
                    variant_ids_by_name: HashMap::new(),
                    span: Some(signature.identifier().span.clone()),
                },
                signature,
            )
        };

        for variant in body
            .dissolve()
            .1
            .into_iter()
            .flat_map(ConnectedList::into_elements)
        {
            let variant_id = self.draft_variant(variant, enum_id);
            let table = self.table.read();

            table.insert_member_id_to_map(
                |enum_sym| &mut enum_sym.variant_ids_by_name,
                variant_id,
                enum_id,
                self.handler,
            );
        }

        enum_id
    }

    #[allow(clippy::significant_drop_tightening)]
    pub(super) fn draft_constant(
        &self,
        syntax_tree: syntax_tree::item::Constant,
        parent_module_id: ID<Module>,
    ) -> ID<Constant> {
        self.builder.write().draft_symbol(
            &mut self.table.write(),
            |id, syntax_tree| Constant {
                id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                r#type: r#type::Type::default(),
                constant: constant::Constant::default(),
                span: Some(syntax_tree.signature().identifier().span.clone()),
                accessibility: Accessibility::from_syntax_tree(syntax_tree.access_modifier()),
                parent_module_id,
            },
            syntax_tree,
        )
    }

    #[allow(clippy::significant_drop_tightening)]
    pub(super) fn draft_type(
        &self,
        syntax_tree: syntax_tree::item::Type,
        parent_module_id: ID<Module>,
    ) -> ID<Type> {
        self.builder.write().draft_symbol(
            &mut self.table.write(),
            |id, syntax_tree| Type {
                id,
                generic_declaration: GenericDeclaration::default(),
                r#type: r#type::Type::default(),
                span: Some(syntax_tree.signature().identifier().span.clone()),
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                accessibility: Accessibility::from_syntax_tree(syntax_tree.access_modifier()),
                parent_module_id,
            },
            syntax_tree,
        )
    }

    #[allow(clippy::significant_drop_tightening)]
    pub(super) fn draft_trait(
        &self,
        syntax_tree: syntax_tree::item::Trait,
        parent_module_id: ID<Module>,
    ) -> ID<Trait> {
        let (access_modifier, signature, body) = syntax_tree.dissolve();
        let (_, member_list, _) = body.dissolve();

        let trait_id = {
            self.builder.write().draft_symbol(
                &mut self.table.write(),
                |id, signature| Trait {
                    id,
                    name: signature.identifier().span.str().to_owned(),
                    accessibility: Accessibility::from_syntax_tree(&access_modifier),
                    parent_module_id,
                    generic_declaration: GenericDeclaration::default(),
                    negative_implementations: Vec::new(),
                    implementations: Vec::new(),
                    span: Some(signature.identifier().span.clone()),
                    trait_member_ids_by_name: HashMap::new(),
                },
                signature,
            )
        };

        for trait_member in member_list {
            let trait_member_id = match trait_member {
                syntax_tree::item::TraitMember::Function(syn) => {
                    TraitMemberID::Function(self.draft_trait_function(syn, trait_id))
                }
                syntax_tree::item::TraitMember::Type(syn) => {
                    TraitMemberID::Type(self.draft_trait_type(syn, trait_id))
                }
                syntax_tree::item::TraitMember::Constant(syn) => {
                    TraitMemberID::Constant(self.draft_trait_constant(syn, trait_id))
                }
            };

            let table = self.table.read();
            table.insert_member_id_to_map(
                |trait_sym| &mut trait_sym.trait_member_ids_by_name,
                trait_member_id,
                trait_id,
                self.handler,
            );
        }

        trait_id
    }

    #[allow(clippy::significant_drop_tightening)]
    pub(super) fn draft_function(
        &self,
        syntax_tree: syntax_tree::item::Function,
        parent_module_id: ID<Module>,
    ) -> ID<Function> {
        self.builder.write().draft_symbol(
            &mut self.table.write(),
            |id, syntax_tree| Function {
                id,
                accessibility: Accessibility::from_syntax_tree(syntax_tree.access_modifier()),
                parameters: Map::default(),
                parent_module_id,
                span: Some(syntax_tree.signature().identifier().span.clone()),
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                return_type: r#type::Type::default(),
                generic_declaration: GenericDeclaration::default(),
            },
            syntax_tree,
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
                syntax_tree::item::Item::Implementation(syn) => {
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

            let table = self.table.read();
            table.insert_member_id_to_map(
                |module| &mut module.module_child_ids_by_name,
                module_member_id,
                module_id,
                self.handler,
            );
        });

        module_id
    }
}
