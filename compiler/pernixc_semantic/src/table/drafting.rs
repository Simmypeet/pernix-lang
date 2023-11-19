use std::collections::{hash_map::Entry, HashMap, HashSet};

use parking_lot::{RwLock, RwLockWriteGuard};
use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, target::ModuleTree, ConnectedList};
use rayon::prelude::{IntoParallelIterator, ParallelIterator};

use super::{state::State, IndexMut, Table};
use crate::{
    arena::{Map, ID},
    entity::{constant, r#type, GenericArguments},
    error::{
        self, GlobalRedefinition, TraitMemberAndImplementationMemberMismatched,
        TraitMemberNotImplemented,
    },
    symbol::{
        self, Accessibility, Constant, Enum, Function, GenericDeclaration, GlobalID,
        ImplementationConstant, ImplementationFunction, ImplementationMemberID,
        ImplementationSignature, ImplementationType, Module, ModuleMemberID,
        NegativeImplementation, Struct, Trait, TraitConstant, TraitFunction, TraitMemberID,
        TraitType, Type, Variant,
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
}

macro_rules! draft_symbol {
    ($table:expr, $symbol:ident, $symbol_id:ident, $expr:expr, $state_expr:expr, $state:ident) => {{
        let symbol_id = {
            let $symbol_id = ID::new($table.$symbol.len());

            $table.$symbol.insert(RwLock::new($expr));

            $table
                .state_manager
                .write()
                .$state
                .insert($symbol_id, State::Drafted($state_expr));

            $symbol_id
        };

        symbol_id
    }};
}

impl Table {
    pub(super) fn draft_implementation_function(
        &mut self,
        syntax_tree: syntax_tree::item::ImplementationFunction,
        parent_implementation_id: ID<symbol::Implementation>,
    ) -> ID<ImplementationFunction> {
        draft_symbol!(
            self,
            implementation_functions,
            implementation_function_id,
            ImplementationFunction {
                id: implementation_function_id,
                parent_implementation_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                generic_declaration: GenericDeclaration::default(),
                span: Some(syntax_tree.signature().identifier().span()),
                parameters: Map::new(),
                return_type: r#type::Type::default(),
            },
            syntax_tree,
            states_by_implementation_function_id
        )
    }

    pub(super) fn draft_implementation_type(
        &mut self,
        syntax_tree: syntax_tree::item::ImplementationType,
        parent_implementation_id: ID<symbol::Implementation>,
    ) -> ID<ImplementationType> {
        draft_symbol!(
            self,
            implementation_types,
            implementation_type_id,
            ImplementationType {
                id: implementation_type_id,
                parent_implementation_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                generic_declaration: GenericDeclaration::default(),
                span: Some(syntax_tree.signature().identifier().span()),
                r#type: r#type::Type::default(),
            },
            syntax_tree,
            states_by_implementation_type_id
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
    ) -> ID<ImplementationConstant> {
        draft_symbol!(
            self,
            implementation_constants,
            implementation_constant_id,
            ImplementationConstant {
                id: implementation_constant_id,
                parent_implementation_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                r#type: r#type::Type::default(),
                constant: constant::Constant::default(),
                span: Some(syntax_tree.signature().identifier().span()),
            },
            syntax_tree,
            states_by_implementation_constant_id
        )
    }

    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    pub(super) fn draft_implementation(
        &mut self,
        implementation_syntax_tree: syntax_tree::item::Implementation,
        declared_in_module_id: ID<Module>,
        parent_trait_id: ID<Trait>,
        handler: &dyn Handler<error::Error>,
    ) {
        let (signature, kind) = implementation_syntax_tree.dissolve();
        let name = self.get(parent_trait_id).unwrap().name.clone();

        match kind {
            syntax_tree::item::ImplementationKind::Negative(..) => {
                let negative_implementation_id = draft_symbol!(
                    self,
                    negative_implementations,
                    negative_implementation_id,
                    NegativeImplementation {
                        id: negative_implementation_id,
                        signature: ImplementationSignature {
                            arguments: GenericArguments::default(),
                            trait_id: parent_trait_id,
                            span: Some(signature.qualified_identifier().span()),
                            generic_declaration: GenericDeclaration::default(),
                            declared_in: declared_in_module_id,
                            trait_name: name
                        }
                    },
                    signature,
                    states_by_negative_implementation_id
                );

                self.get_mut(parent_trait_id)
                    .unwrap()
                    .negative_implementations
                    .push(negative_implementation_id);
            }
            syntax_tree::item::ImplementationKind::Positive(body) => {
                let (where_clause, _, members, _) = body.dissolve();

                let implementation_id = draft_symbol!(
                    self,
                    implementations,
                    implementation_id,
                    symbol::Implementation {
                        id: implementation_id,
                        signature: ImplementationSignature {
                            arguments: GenericArguments::default(),
                            trait_id: parent_trait_id,
                            span: Some(signature.qualified_identifier().span()),
                            generic_declaration: GenericDeclaration::default(),
                            declared_in: declared_in_module_id,
                            trait_name: name
                        },
                        is_const: signature.const_keyword().is_some(),
                        implementation_member_ids_by_name: HashMap::new(),
                        implementation_type_ids_by_trait_type_id: HashMap::new(),
                        implementation_function_ids_by_trait_function_id: HashMap::new(),
                        implementation_constant_ids_by_trait_constant_id: HashMap::new()
                    },
                    (signature, where_clause),
                    states_by_implementation_id
                );

                self.get_mut(parent_trait_id)
                    .unwrap()
                    .implementations
                    .push(implementation_id);

                for member in members {
                    let member_id = match member {
                        syntax_tree::item::ImplementationMember::Type(syn) => {
                            ImplementationMemberID::Type(
                                self.draft_implementation_type(syn, implementation_id),
                            )
                        }
                        syntax_tree::item::ImplementationMember::Function(syn) => {
                            ImplementationMemberID::Function(
                                self.draft_implementation_function(syn, implementation_id),
                            )
                        }
                        syntax_tree::item::ImplementationMember::Constant(syn) => {
                            ImplementationMemberID::Constant(
                                self.draft_implementation_constant(syn, implementation_id),
                            )
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
        let mut table = self.table.write();
        draft_symbol!(
            table,
            trait_types,
            trait_id,
            TraitType {
                id: trait_id,
                parent_trait_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                generic_declaration: GenericDeclaration::default(),
                span: Some(syntax_tree.signature().identifier().span()),
            },
            syntax_tree,
            states_by_trait_type_id
        )
    }

    #[allow(clippy::significant_drop_tightening)]
    pub(super) fn draft_trait_function(
        &self,
        syntax_tree: syntax_tree::item::TraitFunction,
        parent_trait_id: ID<Trait>,
    ) -> ID<TraitFunction> {
        let mut table = self.table.write();
        draft_symbol!(
            table,
            trait_functions,
            trait_id,
            TraitFunction {
                id: trait_id,
                parent_trait_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                generic_declaration: GenericDeclaration::default(),
                span: Some(syntax_tree.signature().identifier().span()),
                parameters: Map::new(),
                return_type: r#type::Type::default(),
            },
            syntax_tree,
            states_by_trait_function_id
        )
    }

    #[allow(clippy::significant_drop_tightening)]
    pub(super) fn draft_trait_constant(
        &self,
        syntax_tree: syntax_tree::item::TraitConstant,
        parent_trait_id: ID<Trait>,
    ) -> ID<TraitConstant> {
        let mut table = self.table.write();
        draft_symbol!(
            table,
            trait_constants,
            trait_constant_id,
            TraitConstant {
                id: trait_constant_id,
                parent_trait_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                r#type: r#type::Type::default(),
                span: Some(syntax_tree.signature().identifier().span()),
            },
            syntax_tree,
            states_by_trait_constant_id
        )
    }

    #[allow(clippy::significant_drop_tightening)]
    pub(super) fn draft_struct(
        &self,
        syntax_tree: syntax_tree::item::Struct,
        parent_module_id: ID<Module>,
    ) -> ID<Struct> {
        let mut table = self.table.write();
        draft_symbol!(
            table,
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
            syntax_tree,
            states_by_struct_id
        )
    }

    #[allow(clippy::significant_drop_tightening)]
    pub(super) fn draft_variant(
        &self,
        syntax_tree: syntax_tree::item::Variant,
        parent_enum_id: ID<Enum>,
    ) -> ID<Variant> {
        let mut table = self.table.write();
        draft_symbol!(
            table,
            variants,
            variant_id,
            Variant {
                associated_type: None,
                id: variant_id,
                name: syntax_tree.identifier().span.str().to_owned(),
                parent_enum_id,
                span: Some(syntax_tree.identifier().span.clone())
            },
            syntax_tree,
            states_by_variant_id
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
            let mut table = self.table.write();
            draft_symbol!(
                table,
                enums,
                enum_id,
                Enum {
                    id: enum_id,
                    name: signature.identifier().span.str().to_owned(),
                    accessibility: Accessibility::from_syntax_tree(&access_modifier),
                    parent_module_id,
                    generic_declaration: GenericDeclaration::default(),
                    variant_ids_by_name: HashMap::new(),
                    span: Some(signature.identifier().span.clone()),
                },
                signature,
                states_by_enum_id
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
        let mut table = self.table.write();
        draft_symbol!(
            table,
            constants,
            constant_id,
            Constant {
                id: constant_id,
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                r#type: r#type::Type::default(),
                constant: constant::Constant::default(),
                span: Some(syntax_tree.signature().identifier().span.clone()),
                accessibility: Accessibility::from_syntax_tree(syntax_tree.access_modifier()),
                parent_module_id
            },
            syntax_tree,
            states_by_constant_id
        )
    }

    #[allow(clippy::significant_drop_tightening)]
    pub(super) fn draft_type(
        &self,
        syntax_tree: syntax_tree::item::Type,
        parent_module_id: ID<Module>,
    ) -> ID<Type> {
        let mut table = self.table.write();
        draft_symbol!(
            table,
            types,
            type_id,
            Type {
                id: type_id,
                generic_declaration: GenericDeclaration::default(),
                r#type: r#type::Type::default(),
                span: Some(syntax_tree.signature().identifier().span.clone()),
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                accessibility: Accessibility::from_syntax_tree(syntax_tree.access_modifier()),
                parent_module_id
            },
            syntax_tree,
            states_by_type_id
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
            let mut table = self.table.write();

            draft_symbol!(
                table,
                traits,
                trait_id,
                Trait {
                    id: trait_id,
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
                states_by_trait_id
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
        let mut table = self.table.write();
        draft_symbol!(
            table,
            functions,
            function_id,
            Function {
                id: function_id,
                accessibility: Accessibility::from_syntax_tree(syntax_tree.access_modifier()),
                parameters: Map::default(),
                parent_module_id,
                span: Some(syntax_tree.signature().identifier().span.clone()),
                name: syntax_tree.signature().identifier().span.str().to_owned(),
                return_type: r#type::Type::default(),
                generic_declaration: GenericDeclaration::default(),
            },
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
