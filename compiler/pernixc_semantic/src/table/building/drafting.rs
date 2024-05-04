//! Contains the code related to drafting phase of the table.
//!
//! The drafting phase is the first phase of the table. It builds all the
//! so that their names appear in the table. The symbols are not yet built
//! with full/correct information.

use std::collections::{hash_map::Entry, HashMap, HashSet};

use parking_lot::RwLock;
use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{
    self,
    item::{ImplementationKind, ImplementationMember},
    target::ModuleTree,
    ConnectedList, GenericIdentifier,
};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use super::finalizing::Finalizer;
use crate::{
    arena::{Arena, Map, ID},
    error::{
        self, AlreadyImplementedTraitMember, InvalidSymbolInImplementation,
        MismatchedTraitMemberAndImplementationMember,
        MismatchedTraitMemberAndImplementationMemberAccessibility,
        NegativeImplementationOnAdt, RedefinedGlobal,
        SymbolIsMoreAccessibleThanParent, TraitMemberKind,
        UnimplementedTraitMembers, UnknownTraitImplementationMember,
    },
    pattern::NameBindingPoint,
    semantic::term::{r#type, GenericArguments},
    symbol::{
        Accessibility, AdtID, AdtImplementation, AdtImplementationConstant,
        AdtImplementationData, AdtImplementationFunction,
        AdtImplementationMemberID, AdtImplementationType, Constant,
        ConstantData, Enum, Function, FunctionData, FunctionIR,
        GenericDeclaration, GenericParameterVariances, GlobalID,
        ImplementationSignature, Module, ModuleMemberID,
        NegativeTraitImplementation, Struct, Trait, TraitConstant,
        TraitConstantData, TraitFunction, TraitFunctionData,
        TraitImplementation, TraitImplementationConstant,
        TraitImplementationConstantData, TraitImplementationData,
        TraitImplementationFunction, TraitImplementationFunctionData,
        TraitImplementationMemberID, TraitImplementationType,
        TraitImplementationTypeData, TraitMemberID, TraitType, TraitTypeData,
        Type, TypeData, Variant,
    },
    table::{self, Element, GetMemberError, Index, RwLockContainer, Table},
};

#[derive(Debug, Default)]
pub struct Drafter {
    pub usings_by_module_id: HashMap<ID<Module>, Vec<syntax_tree::item::Using>>,
    pub implementations_by_module_id:
        HashMap<ID<Module>, Vec<syntax_tree::item::Implementation>>,
    pub building: Finalizer,
}

impl table::State for Drafter {
    type Container = RwLockContainer;

    fn on_global_id_resolved(
        _: &Table<Self>,
        _: GlobalID,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
    ) {
    }

    fn on_resolved(
        _: &Table<Self>,
        _: table::resolution::Resolution,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
    ) {
    }
}

impl Table<Drafter> {
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
                handler.receive(Box::new(RedefinedGlobal {
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
        let id = table_write.representation.trait_functions.insert_with(|_| {
            RwLock::new(TraitFunction {
                parameters: Arena::new(),
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
                data: TraitFunctionData {
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

    fn draft_trait_type(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::TraitType,
        parent_trait_id: ID<Trait>,
    ) -> ID<TraitType> {
        let mut table_write = table.write();
        let id = table_write.representation.trait_types.insert_with(|_| {
            RwLock::new(TraitType {
                parent_id: parent_trait_id,
                span: Some(syntax_tree.signature().identifier().span.clone()),
                name: syntax_tree
                    .signature()
                    .identifier()
                    .span
                    .str()
                    .to_owned(),
                generic_declaration: GenericDeclaration::default(),
                data: TraitTypeData {
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

    fn draft_trait_constant(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::TraitConstant,
        parent_trait_id: ID<Trait>,
    ) -> ID<TraitConstant> {
        let mut table_write = table.write();
        let id = table_write.representation.trait_constants.insert_with(|_| {
            RwLock::new(TraitConstant {
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
                data: TraitConstantData {
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

    fn draft_trait(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::Trait,
        parent_module_id: ID<Module>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> ID<Trait> {
        let (access_modifier, signature, body) = syntax_tree.dissolve();
        let (_, member_list, _) = body.dissolve();

        let mut table_write = table.write();
        let trait_accessibility =
            Accessibility::from_syntax_tree(&access_modifier);
        let id = table_write.representation.traits.insert_with(|_| {
            RwLock::new(Trait {
                name: signature.identifier().span.str().to_owned(),
                accessibility: trait_accessibility,
                parent_module_id,
                generic_declaration: GenericDeclaration::default(),
                negative_implementations: HashSet::new(),
                implementations: HashSet::new(),
                span: Some(signature.identifier().span.clone()),
                member_ids_by_name: HashMap::new(),
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

            if table.read().get_accessibility(trait_member_id.into()).unwrap()
                > trait_accessibility
            {
                handler.receive(Box::new(SymbolIsMoreAccessibleThanParent {
                    symbol_id: trait_member_id.into(),
                    parent_id: id.into(),
                }));
            }

            table.read().insert_member_id_to_map(
                |trait_sym| &mut trait_sym.member_ids_by_name,
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
        let id = table_write.representation.functions.insert_with(|_| {
            RwLock::new(Function {
                parameters: Arena::new(),
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
                    patterns_by_parameter_id: HashMap::new(),
                    parameters_name_binding_point: NameBindingPoint::default(),
                    ir: FunctionIR::default(),
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
        let id = table_write.representation.types.insert_with(|_| {
            RwLock::new(Type {
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
        let id = table_write.representation.structs.insert_with(|_| {
            RwLock::new(Struct {
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
                generic_parameter_variances: GenericParameterVariances::default(
                ),
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

        let id = table_write.representation.enums.insert_with(|_| {
            RwLock::new(Enum {
                name: signature.identifier().span.str().to_owned(),
                accessibility: Accessibility::from_syntax_tree(
                    &access_modifier,
                ),
                parent_module_id,
                generic_declaration: GenericDeclaration::default(),
                variant_ids_by_name: HashMap::new(),
                implementations: HashSet::new(),
                span: Some(signature.identifier().span.clone()),
                generic_parameter_variances: GenericParameterVariances::default(
                ),
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
        let id = table_write.representation.variants.insert_with(|_| {
            RwLock::new(Variant {
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
        let id = table_write.representation.constants.insert_with(|_| {
            RwLock::new(Constant {
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
            table.write().representation.modules.insert_with(|_| {
                RwLock::new(Module {
                    name,
                    accessibility: syntax_tree
                        .signature()
                        .as_ref()
                        .map_or(Accessibility::Public, |x| {
                            Accessibility::from_syntax_tree(&x.access_modifier)
                        }),
                    parent_module_id,
                    child_ids_by_name: HashMap::new(),
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
                .child_ids_by_name
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
                |module| &mut module.child_ids_by_name,
                module_member_id,
                module_id,
                handler,
            );
        });

        module_id
    }
}

impl Table<Finalizer> {
    fn draft_trait_implementation_function(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::Function,
        implemented_trait_function_id: ID<TraitFunction>,
        parent_id: ID<TraitImplementation>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> ID<TraitImplementationFunction> {
        let implementation_function_id = table
            .write()
            .representation
            .trait_implementation_functions
            .insert_with(|_| {
                RwLock::new(TraitImplementationFunction {
                    generic_declaration: GenericDeclaration::default(),
                    parent_id,
                    span: Some(
                        syntax_tree.signature().identifier().span.clone(),
                    ),
                    name: syntax_tree
                        .signature()
                        .identifier()
                        .span
                        .str()
                        .to_owned(),
                    data: TraitImplementationFunctionData {
                        implemented_trait_function_id,
                        patterns_by_parameter_id: HashMap::new(),
                    },
                    parameters: Arena::default(),
                    return_type: r#type::Type::default(),
                })
            });

        let this_accessibility =
            Accessibility::from_syntax_tree(syntax_tree.access_modifier());
        if this_accessibility
            != table
                .read()
                .get_accessibility(implemented_trait_function_id.into())
                .unwrap()
        {
            handler.receive(Box::new(
                MismatchedTraitMemberAndImplementationMemberAccessibility {
                    trait_member_id: implemented_trait_function_id.into(),
                    implementation_member_accessibility: this_accessibility,
                    implementation_member_id: implementation_function_id.into(),
                },
            ));
        }

        assert!(table
            .read()
            .state
            .draft_symbol(implementation_function_id, syntax_tree));

        implementation_function_id
    }

    fn draft_trait_implementation_type(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::Type,
        implemented_trait_type_id: ID<TraitType>,
        parent_id: ID<TraitImplementation>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> ID<TraitImplementationType> {
        let implementation_type_id = table
            .write()
            .representation
            .trait_implementation_types
            .insert_with(|_| {
                RwLock::new(TraitImplementationType {
                    generic_declaration: GenericDeclaration::default(),
                    parent_id,
                    span: Some(
                        syntax_tree.signature().identifier().span.clone(),
                    ),
                    name: syntax_tree
                        .signature()
                        .identifier()
                        .span
                        .str()
                        .to_owned(),
                    data: TraitImplementationTypeData {
                        r#type: r#type::Type::default(),
                        implemented_trait_type_id,
                        generic_parameter_variances:
                            GenericParameterVariances::default(),
                    },
                })
            });

        let this_accessibility =
            Accessibility::from_syntax_tree(syntax_tree.access_modifier());
        if this_accessibility
            != table
                .read()
                .get_accessibility(implemented_trait_type_id.into())
                .unwrap()
        {
            handler.receive(Box::new(
                MismatchedTraitMemberAndImplementationMemberAccessibility {
                    trait_member_id: implemented_trait_type_id.into(),
                    implementation_member_accessibility: this_accessibility,
                    implementation_member_id: implementation_type_id.into(),
                },
            ));
        }

        assert!(table
            .read()
            .state
            .draft_symbol(implementation_type_id, syntax_tree));

        implementation_type_id
    }

    fn draft_trait_implementation_constant(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::Constant,
        implemented_trait_constant_id: ID<TraitConstant>,
        parent_id: ID<TraitImplementation>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> ID<TraitImplementationConstant> {
        let implementation_constant_id = table
            .write()
            .representation
            .trait_implementation_constants
            .insert_with(|_| {
                RwLock::new(TraitImplementationConstant {
                    generic_declaration: GenericDeclaration::default(),
                    parent_id,
                    span: Some(
                        syntax_tree.signature().identifier().span.clone(),
                    ),
                    name: syntax_tree
                        .signature()
                        .identifier()
                        .span
                        .str()
                        .to_owned(),
                    r#type: r#type::Type::default(),
                    data: TraitImplementationConstantData {
                        implemented_trait_constant_id,
                    },
                })
            });

        let this_accessibility =
            Accessibility::from_syntax_tree(syntax_tree.access_modifier());
        if this_accessibility
            != table
                .read()
                .get_accessibility(implemented_trait_constant_id.into())
                .unwrap()
        {
            handler.receive(Box::new(
                MismatchedTraitMemberAndImplementationMemberAccessibility {
                    trait_member_id: implemented_trait_constant_id.into(),
                    implementation_member_accessibility: this_accessibility,
                    implementation_member_id: implementation_constant_id.into(),
                },
            ));
        }

        assert!(table
            .read()
            .state
            .draft_symbol(implementation_constant_id, syntax_tree));

        implementation_constant_id
    }

    #[allow(clippy::too_many_lines)]
    fn draft_positive_trait_implementation(
        table: &RwLock<Self>,
        implementation_signature: syntax_tree::item::ImplementationSignature,
        implementation_body: syntax_tree::item::ImplementationBody,
        declared_in: ID<Module>,
        trait_id: ID<Trait>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> ID<TraitImplementation> {
        let (_, members, _) = implementation_body.dissolve();

        // the implementation id
        let implementation_name =
            table.read().get(trait_id).unwrap().name.clone();
        let implementation_id = table
            .write()
            .representation
            .trait_implementations
            .insert_with(|_| {
                TraitImplementation {
                    span: Some(
                        implementation_signature.qualified_identifier().span(),
                    ),
                    signature: ImplementationSignature {
                        generic_declaration: GenericDeclaration::default(),
                        arguments: GenericArguments::default(),
                        implemented_id: trait_id,
                    },
                    implementation_name,
                    declared_in,
                    data: TraitImplementationData {
                        is_const: implementation_signature
                            .const_keyword()
                            .is_some(),
                        member_ids_by_name: HashMap::new(),
                        implementation_type_ids_by_trait_type_id: HashMap::new(
                        ),
                        implementation_function_ids_by_trait_function_id:
                            HashMap::new(),
                        implementation_constant_ids_by_trait_constant_id:
                            HashMap::new(),
                    },
                }
                .into()
            });

        assert!(table
            .read()
            .state
            .draft_symbol(implementation_id, implementation_signature));

        // used for checking if a particular member is implemented
        let mut implemented_member = HashMap::new();

        for member in members {
            let identifier = match &member {
                ImplementationMember::Type(syntax_tree) => {
                    syntax_tree.signature().identifier().clone()
                }
                ImplementationMember::Function(syntax_tree) => {
                    syntax_tree.signature().identifier().clone()
                }
                ImplementationMember::Constant(syntax_tree) => {
                    syntax_tree.signature().identifier().clone()
                }
            };

            // find the corresponding trait member
            let Some(trait_member_id) = table
                .read()
                .get(trait_id)
                .unwrap()
                .member_ids_by_name
                .get(identifier.span.str())
                .copied()
            else {
                handler.receive(Box::new(UnknownTraitImplementationMember {
                    identifier_span: identifier.span.clone(),
                    trait_id,
                }));
                continue;
            };

            let entry = match implemented_member.entry(trait_member_id) {
                Entry::Occupied(entry) => {
                    handler.receive(Box::new(AlreadyImplementedTraitMember {
                        trait_member_id,
                        implemented_id: *entry.get(),
                        new_implementation_span: identifier.span.clone(),
                    }));
                    continue;
                }
                Entry::Vacant(entry) => entry,
            };

            let trait_implemetation_member_id = match (trait_member_id, member)
            {
                (
                    TraitMemberID::Function(trait_function_id),
                    ImplementationMember::Function(syntax_tree),
                ) => {
                    let trait_implementation_function_id =
                        Self::draft_trait_implementation_function(
                            table,
                            syntax_tree,
                            trait_function_id,
                            implementation_id,
                            handler,
                        );
                    assert!(table
                        .read()
                        .trait_implementations
                        .get(implementation_id)
                        .unwrap()
                        .write()
                        .implementation_function_ids_by_trait_function_id
                        .insert(
                            trait_function_id,
                            trait_implementation_function_id
                        )
                        .is_none());
                    TraitImplementationMemberID::Function(
                        trait_implementation_function_id,
                    )
                }
                (
                    TraitMemberID::Type(trait_type_id),
                    ImplementationMember::Type(syntax_tree),
                ) => {
                    let trait_implementation_type_id =
                        Self::draft_trait_implementation_type(
                            table,
                            syntax_tree,
                            trait_type_id,
                            implementation_id,
                            handler,
                        );
                    assert!(table
                        .read()
                        .trait_implementations
                        .get(implementation_id)
                        .unwrap()
                        .write()
                        .implementation_type_ids_by_trait_type_id
                        .insert(trait_type_id, trait_implementation_type_id)
                        .is_none());
                    TraitImplementationMemberID::Type(
                        trait_implementation_type_id,
                    )
                }
                (
                    TraitMemberID::Constant(trait_constant_id),
                    ImplementationMember::Constant(syntax_tree),
                ) => {
                    let trait_implementation_constant_id =
                        Self::draft_trait_implementation_constant(
                            table,
                            syntax_tree,
                            trait_constant_id,
                            implementation_id,
                            handler,
                        );
                    assert!(table
                        .read()
                        .trait_implementations
                        .get(implementation_id)
                        .unwrap()
                        .write()
                        .implementation_constant_ids_by_trait_constant_id
                        .insert(
                            trait_constant_id,
                            trait_implementation_constant_id
                        )
                        .is_none());
                    TraitImplementationMemberID::Constant(
                        trait_implementation_constant_id,
                    )
                }

                (trait_member_id, member) => {
                    // TODO: report the mismatch symbol kind error
                    let found_kind = match member {
                        ImplementationMember::Type(_) => TraitMemberKind::Type,
                        ImplementationMember::Function(_) => {
                            TraitMemberKind::Function
                        }
                        ImplementationMember::Constant(_) => {
                            TraitMemberKind::Constant
                        }
                    };

                    handler.receive(Box::new(
                        MismatchedTraitMemberAndImplementationMember {
                            trait_member_id,
                            found_kind,
                            implementation_member_identifer_span: identifier
                                .span
                                .clone(),
                        },
                    ));
                    continue;
                }
            };

            // add to the implementation
            entry.insert(trait_implemetation_member_id);
            assert!(table
                .read()
                .trait_implementations
                .get(implementation_id)
                .unwrap()
                .write()
                .member_ids_by_name
                .insert(
                    identifier.span.str().to_owned(),
                    trait_implemetation_member_id,
                )
                .is_none());
        }

        // TODO: check if the trait members are implemented
        let mut unimplemented_members = Vec::new();

        #[allow(clippy::significant_drop_in_scrutinee)]
        for trait_member_id in
            table.read().get(trait_id).unwrap().member_ids_by_name.values()
        {
            if !implemented_member.contains_key(trait_member_id) {
                unimplemented_members.push(*trait_member_id);
            }
        }

        if !unimplemented_members.is_empty() {
            handler.receive(Box::new(UnimplementedTraitMembers {
                unimplemented_trait_member_ids: unimplemented_members,
                implementation_id,
            }));
        }

        implementation_id
    }

    fn draft_adt_implementation_type(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::Type,
        adt_implementations_id: ID<AdtImplementation>,
    ) -> ID<AdtImplementationType> {
        let implementation_type_id =
            table.write().representation.adt_implementation_types.insert_with(
                |_| {
                    RwLock::new(AdtImplementationType {
                        generic_declaration: GenericDeclaration::default(),
                        parent_id: adt_implementations_id,
                        span: Some(
                            syntax_tree.signature().identifier().span.clone(),
                        ),
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
                },
            );

        assert!(table
            .read()
            .state
            .draft_symbol(implementation_type_id, syntax_tree));

        implementation_type_id
    }

    fn draft_adt_implementation_function(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::Function,
        adt_implementations_id: ID<AdtImplementation>,
    ) -> ID<AdtImplementationFunction> {
        let implementation_function_id = table
            .write()
            .representation
            .adt_implementation_functions
            .insert_with(|_| {
                RwLock::new(AdtImplementationFunction {
                    generic_declaration: GenericDeclaration::default(),
                    parent_id: adt_implementations_id,
                    span: Some(
                        syntax_tree.signature().identifier().span.clone(),
                    ),
                    name: syntax_tree
                        .signature()
                        .identifier()
                        .span
                        .str()
                        .to_owned(),
                    data: FunctionData {
                        accessibility: Accessibility::from_syntax_tree(
                            syntax_tree.access_modifier(),
                        ),
                        const_function: syntax_tree.const_keyword().is_some(),
                        patterns_by_parameter_id: HashMap::new(),
                        parameters_name_binding_point:
                            NameBindingPoint::default(),
                        ir: FunctionIR::default(),
                    },
                    parameters: Arena::default(),
                    return_type: r#type::Type::default(),
                })
            });

        assert!(table
            .read()
            .state
            .draft_symbol(implementation_function_id, syntax_tree));

        implementation_function_id
    }

    fn draft_adt_implementation_constant(
        table: &RwLock<Self>,
        syntax_tree: syntax_tree::item::Constant,
        adt_implementations_id: ID<AdtImplementation>,
    ) -> ID<AdtImplementationConstant> {
        let implementation_constant_id = table
            .write()
            .representation
            .adt_implementation_constants
            .insert_with(|_| {
                RwLock::new(AdtImplementationConstant {
                    generic_declaration: GenericDeclaration::default(),
                    parent_id: adt_implementations_id,
                    span: Some(
                        syntax_tree.signature().identifier().span.clone(),
                    ),
                    name: syntax_tree
                        .signature()
                        .identifier()
                        .span
                        .str()
                        .to_owned(),
                    r#type: r#type::Type::default(),
                    data: ConstantData {
                        accessibility: Accessibility::from_syntax_tree(
                            syntax_tree.access_modifier(),
                        ),
                    },
                })
            });

        assert!(table
            .read()
            .state
            .draft_symbol(implementation_constant_id, syntax_tree));

        implementation_constant_id
    }

    #[allow(clippy::too_many_lines)]
    fn draft_adt_implementation(
        table: &RwLock<Self>,
        implementation: syntax_tree::item::Implementation,
        declared_in: ID<Module>,
        adt_id: AdtID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let (signature, kind) = implementation.dissolve();

        let body = match kind {
            ImplementationKind::Negative(_) => {
                handler.receive(Box::new(NegativeImplementationOnAdt {
                    negative_implementation_span: signature
                        .qualified_identifier()
                        .span(),
                    adt_id,
                }));
                return;
            }
            ImplementationKind::Positive(body) => body,
        };

        let (_, members, _) = body.dissolve();

        // create the implementation
        let implementation_name =
            table.read().get_global(adt_id.into()).unwrap().name().to_owned();

        let adt_implementation_id =
            table.write().representation.adt_implementations.insert_with(
                |_| {
                    RwLock::new(AdtImplementation {
                        span: Some(signature.qualified_identifier().span()),
                        signature: ImplementationSignature {
                            generic_declaration: GenericDeclaration::default(),
                            arguments: GenericArguments::default(),
                            implemented_id: adt_id,
                        },
                        implementation_name,
                        declared_in,
                        data: AdtImplementationData {
                            member_ids_by_name: HashMap::new(),
                        },
                    })
                },
            );

        assert!(table
            .read()
            .state
            .draft_symbol(adt_implementation_id, signature));

        for member in members {
            // get the identifier sytax
            let identifier = match &member {
                ImplementationMember::Type(member) => {
                    member.signature().identifier().clone()
                }
                ImplementationMember::Function(member) => {
                    member.signature().identifier().clone()
                }
                ImplementationMember::Constant(member) => {
                    member.signature().identifier().clone()
                }
            };

            let member_id = match member {
                ImplementationMember::Type(syntax_tree) => {
                    AdtImplementationMemberID::Type(
                        Self::draft_adt_implementation_type(
                            table,
                            syntax_tree,
                            adt_implementation_id,
                        ),
                    )
                }
                ImplementationMember::Function(syntax_tree) => {
                    AdtImplementationMemberID::Function(
                        Self::draft_adt_implementation_function(
                            table,
                            syntax_tree,
                            adt_implementation_id,
                        ),
                    )
                }
                ImplementationMember::Constant(syntax_tree) => {
                    AdtImplementationMemberID::Constant(
                        Self::draft_adt_implementation_constant(
                            table,
                            syntax_tree,
                            adt_implementation_id,
                        ),
                    )
                }
            };

            // check if the member already exists
            #[allow(clippy::significant_drop_in_scrutinee)]
            match table
                .read()
                .get_member_of(adt_id.into(), identifier.span().str())
            {
                Ok(id) => {
                    // redefinition error
                    handler.receive(Box::new(RedefinedGlobal {
                        existing_global_id: id,
                        new_global_id: member_id.into(),
                        in_global_id: adt_id.into(),
                    }));
                    continue;
                }
                Err(GetMemberError::MemberNotFound) => {}
                Err(GetMemberError::InvalidID) => {
                    unreachable!();
                }
            }

            // add the member to the implementation
            assert!(table
                .read()
                .adt_implementations
                .get(adt_implementation_id)
                .unwrap()
                .write()
                .member_ids_by_name
                .insert(identifier.span.str().to_owned(), member_id)
                .is_none());
        }
    }

    fn draft_trait_implementation(
        table: &RwLock<Self>,
        implementation: syntax_tree::item::Implementation,
        declared_in: ID<Module>,
        trait_id: ID<Trait>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let (signature, kind) = implementation.dissolve();

        match kind {
            ImplementationKind::Negative(..) => {
                let implementation_name =
                    table.read().get(trait_id).unwrap().name.clone();

                let negative_implementation_id = table
                    .write()
                    .representation
                    .negative_trait_implementations
                    .insert_with(|_| {
                        RwLock::new(NegativeTraitImplementation {
                            span: Some(signature.qualified_identifier().span()),
                            signature: ImplementationSignature {
                                generic_declaration:
                                    GenericDeclaration::default(),
                                arguments: GenericArguments::default(),
                                implemented_id: trait_id,
                            },
                            implementation_name,
                            declared_in,
                            data:
                                crate::symbol::NegativeTraitImplementationData,
                        })
                    });

                assert!(table
                    .read()
                    .state
                    .draft_symbol(negative_implementation_id, signature));

                assert!(table
                    .read()
                    .representation
                    .traits
                    .get(trait_id)
                    .unwrap()
                    .write()
                    .negative_implementations
                    .insert(negative_implementation_id));
            }
            ImplementationKind::Positive(body) => {
                let implementation_id =
                    Self::draft_positive_trait_implementation(
                        table,
                        signature,
                        body,
                        declared_in,
                        trait_id,
                        handler,
                    );

                assert!(table
                    .read()
                    .representation
                    .traits
                    .get(trait_id)
                    .unwrap()
                    .write()
                    .implementations
                    .insert(implementation_id));
            }
        }
    }

    pub(in crate::table) fn draft_implementation(
        table: &RwLock<Self>,
        implementation: syntax_tree::item::Implementation,
        defined_in_module_id: ID<Module>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let Ok(id) = table.read().resolve_simple_path(
            implementation
                .signature()
                .qualified_identifier()
                .generic_identifiers()
                .map(GenericIdentifier::identifier),
            defined_in_module_id.into(),
            implementation
                .signature()
                .qualified_identifier()
                .leading_scope_separator()
                .is_some(),
            handler,
        ) else {
            return;
        };

        match id {
            GlobalID::Trait(id) => Self::draft_trait_implementation(
                table,
                implementation,
                defined_in_module_id,
                id,
                handler,
            ),

            adt_id @ (GlobalID::Enum(_) | GlobalID::Struct(_)) => {
                Self::draft_adt_implementation(
                    table,
                    implementation,
                    defined_in_module_id,
                    match adt_id {
                        GlobalID::Struct(struct_id) => AdtID::Struct(struct_id),
                        GlobalID::Enum(enum_id) => AdtID::Enum(enum_id),
                        _ => unreachable!(),
                    },
                    handler,
                );
            }

            // invalid id
            invalid_global_id => {
                handler.receive(Box::new(InvalidSymbolInImplementation {
                    invalid_global_id,
                    qualified_identifier_span: implementation
                        .signature()
                        .qualified_identifier()
                        .span(),
                }));
            }
        }
    }
}

#[cfg(test)]
mod tests;
