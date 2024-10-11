//! Contains the code related to drafting phase of the table.
//!
//! The drafting phase is the first phase of the table. It builds all the
//! so that their names appear in the table. The symbols are not yet built
//! with full/correct information.

use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    ops::Not,
};

use parking_lot::RwLock;
use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_lexical::token::Identifier;
use pernixc_syntax::syntax_tree::{
    self,
    item::{ImplementationKind, ImplementationMember},
    target::ModuleTree,
    ConnectedList,
};

use super::finalizing::{self, Finalize, Finalizer};
use crate::{
    arena::ID,
    error::{
        self, ExpectModule, GlobalRedifinition, InvalidSymbolInImplementation,
        MismatchedTraitMemberAndImplementationMember,
        NoGenericArgumentsRequired, SymbolIsMoreAccessibleThanParent,
        SymbolNotFound, ThisNotFound, UnexpectedAdtImplementationMember,
        UnimplementedTraitMembers, UnknownExternCallingConvention,
        UnknownTraitImplementationMember,
    },
    symbol::{
        self,
        table::{
            representation::{
                self, building::finalizing::FunctionKind, Index,
                RwLockContainer,
            },
            Building, Table,
        },
        Accessibility, AdtID, AdtImplementationDefinition,
        AdtImplementationFunction, Constant, Enum, Extern, Function,
        FunctionDefinition, FunctionTemplate, GenericDeclaration,
        GenericTemplate, GlobalID, HierarchyRelationship, Module,
        ModuleMemberID, NegativeTraitImplementationDefinition,
        PositiveTraitImplementationDefinition, Struct, Trait,
        TraitImplementationMemberID, TraitMemberID, Type, Variant,
    },
    type_system::term::GenericArguments,
};

#[derive(Debug, Default)]
pub struct Drafter {
    pub usings_by_module_id: HashMap<ID<Module>, Vec<syntax_tree::item::Using>>,
    pub implementations_by_module_id:
        HashMap<ID<Module>, Vec<syntax_tree::item::Implementation>>,
    pub finalizer: Finalizer,
}

impl Table<Building<RwLockContainer, Drafter>> {
    fn draft_enum(
        &mut self,
        syntax_tree: syntax_tree::item::Enum,
        parent_module_id: ID<Module>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let identifiers = syntax_tree
            .body()
            .variant_list()
            .as_ref()
            .into_iter()
            .flat_map(ConnectedList::elements)
            .map(syntax_tree::item::Variant::identifier)
            .cloned()
            .collect::<Vec<_>>();

        let enum_accessibility = self
            .create_accessibility(
                parent_module_id.into(),
                syntax_tree.access_modifier(),
            )
            .unwrap();

        let enum_id: ID<Enum> = self.draft_member(
            syntax_tree,
            parent_module_id,
            |syn| syn.signature().identifier(),
            enum_accessibility,
            handler,
        );

        // draft each variant
        for variant_syn in identifiers {
            // gets the variant id
            let variant_id =
                self.representation.variants.insert(RwLock::new(Variant {
                    name: variant_syn.span.str().to_owned(),
                    associated_type: None,
                    parent_enum_id: enum_id,
                    span: Some(variant_syn.span.clone()),
                }));

            let enum_sym =
                self.representation.enums.get_mut(enum_id).unwrap().get_mut();

            // check for duplication
            match enum_sym
                .variant_ids_by_name
                .entry(variant_syn.span.str().to_owned())
            {
                Entry::Occupied(entry) => {
                    handler.receive(Box::new(GlobalRedifinition {
                        existing_global_id: (*entry.get()).into(),
                        new_global_id: variant_id.into(),
                        in_global_id: enum_id.into(),
                    }));
                }
                Entry::Vacant(entry) => {
                    entry.insert(variant_id);
                }
            }
        }
    }

    fn draft_trait(
        &mut self,
        syntax_tree: syntax_tree::item::Trait,
        parent_module_id: ID<Module>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let (access_modifier, signature, body) = syntax_tree.dissolve();
        let (_, member_list, _) = body.dissolve();
        let trait_accessibility = self
            .create_accessibility(parent_module_id.into(), &access_modifier)
            .unwrap();

        let trait_id = self.draft_member(
            signature,
            parent_module_id,
            |syn| syn.identifier(),
            trait_accessibility,
            handler,
        );

        for trait_member in member_list {
            let trait_member_id = match trait_member {
                syntax_tree::item::TraitMember::Function(syn) => {
                    let accessibility = self
                        .create_accessibility(
                            trait_id.into(),
                            syn.access_modifier(),
                        )
                        .unwrap();

                    TraitMemberID::Function(self.draft_member(
                        syn,
                        trait_id,
                        |syn| syn.signature().identifier(),
                        accessibility,
                        handler,
                    ))
                }
                syntax_tree::item::TraitMember::Type(syn) => {
                    let accessibility = self
                        .create_accessibility(
                            trait_id.into(),
                            syn.access_modifier(),
                        )
                        .unwrap();

                    TraitMemberID::Type(self.draft_member(
                        syn,
                        trait_id,
                        |syn| syn.signature().identifier(),
                        accessibility,
                        handler,
                    ))
                }
                syntax_tree::item::TraitMember::Constant(syn) => {
                    let accessibility = self
                        .create_accessibility(
                            trait_id.into(),
                            syn.access_modifier(),
                        )
                        .unwrap();

                    TraitMemberID::Constant(self.draft_member(
                        syn,
                        trait_id,
                        |syn| syn.signature().identifier(),
                        accessibility,
                        handler,
                    ))
                }
            };

            // check if the member is more accessible than the trait
            let member_accessibility =
                self.get_accessibility(trait_member_id.into()).unwrap();

            if self
                .accessibility_hierarchy_relationship(
                    member_accessibility,
                    trait_accessibility,
                )
                .unwrap()
                == HierarchyRelationship::Parent
            {
                handler.receive(Box::new(SymbolIsMoreAccessibleThanParent {
                    symbol_id: trait_member_id.into(),
                    parent_id: trait_id.into(),
                }));
            }
        }
    }

    /// Drafts aodule member.
    fn draft_member<
        S,
        Definition: std::default::Default,
        Parent: symbol::ParentSealed + representation::Element,
        ParentID: Copy + From<ID<Parent>>,
    >(
        &mut self,
        syntax_tree: S,
        parent_id: ID<Parent>,
        identifier_fn: impl FnOnce(&S) -> &Identifier,
        accessibility: Accessibility,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> ID<GenericTemplate<ParentID, Definition>>
    where
        GenericTemplate<ParentID, Definition>: representation::Element
            + Finalize<SyntaxTree = S>
            + finalizing::Element,

        ID<GenericTemplate<ParentID, Definition>>:
            Into<GlobalID> + Into<Parent::MemberID>,

        Parent::MemberID: Into<GlobalID>,
        ID<Parent>: Into<GlobalID>,
    {
        // extract the identifier and access modifier
        let identifier = identifier_fn(&syntax_tree);

        let insertion = self
            .insert_member(
                identifier.span.str().to_owned(),
                accessibility,
                parent_id,
                Some(identifier.span.clone()),
                GenericDeclaration::default(),
                Definition::default(),
            )
            .unwrap();

        // check for duplication
        if let Some(existing) = insertion.duplication {
            handler.receive(Box::new(GlobalRedifinition {
                existing_global_id: existing.into(),
                new_global_id: insertion.id.into(),
                in_global_id: parent_id.into(),
            }));
        }

        assert!(self.state.finalizer.draft_symbol(insertion.id, syntax_tree));

        insertion.id
    }

    #[allow(clippy::too_many_lines)]
    pub(in crate::symbol::table::representation) fn draft_module(
        &mut self,
        syntax_tree: ModuleTree,
        name: String,
        parent_module_id: Option<ID<Module>>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> ID<Module> {
        let module_id = {
            self.representation.modules.insert(RwLock::new(Module {
                name,
                accessibility: syntax_tree.signature().as_ref().map_or(
                    Accessibility::Public,
                    |x| {
                        self.create_accessibility(
                            parent_module_id.unwrap().into(),
                            &x.access_modifier,
                        )
                        .unwrap()
                    },
                ),
                parent_module_id,
                member_ids_by_name: HashMap::new(),
                span: syntax_tree
                    .signature()
                    .as_ref()
                    .map(|x| x.signature.identifier().span()),
                imports: HashMap::new(),
            }))
        };

        let (_, content, submodule_by_name) = syntax_tree.dissolve();
        let (usings, items) = content.dissolve();

        assert!(self
            .state
            .usings_by_module_id
            .insert(module_id, usings)
            .is_none());

        // recursive draft submodules
        for (name, submodule) in submodule_by_name {
            let submodule_id = self.draft_module(
                submodule,
                name.clone(),
                Some(module_id),
                handler,
            );

            self.representation
                .modules
                .get(module_id)
                .unwrap()
                .write()
                .member_ids_by_name
                .insert(name, ModuleMemberID::Module(submodule_id));
        }

        for item in items {
            match item {
                syntax_tree::item::Item::Trait(syn) => {
                    self.draft_trait(syn, module_id, handler);
                }
                syntax_tree::item::Item::Function(syn) => {
                    let accessibility = self
                        .create_accessibility(
                            module_id.into(),
                            syn.access_modifier(),
                        )
                        .unwrap();

                    let _: ID<Function> = self.draft_member(
                        FunctionKind::Normal(syn),
                        module_id,
                        |syn| syn.signature().identifier(),
                        accessibility,
                        handler,
                    );
                }
                syntax_tree::item::Item::Type(syn) => {
                    let accessibility = self
                        .create_accessibility(
                            module_id.into(),
                            syn.access_modifier(),
                        )
                        .unwrap();

                    let _: ID<Type> = self.draft_member(
                        syn,
                        module_id,
                        |syn| syn.signature().identifier(),
                        accessibility,
                        handler,
                    );
                }
                syntax_tree::item::Item::Struct(syn) => {
                    let accessibility = self
                        .create_accessibility(
                            module_id.into(),
                            syn.access_modifier(),
                        )
                        .unwrap();

                    let _: ID<Struct> = self.draft_member(
                        syn,
                        module_id,
                        |syn| syn.signature().identifier(),
                        accessibility,
                        handler,
                    );
                }
                syntax_tree::item::Item::Implementation(syn) => {
                    // remember the implementation, will be processed later
                    self.state
                        .implementations_by_module_id
                        .entry(module_id)
                        .or_default()
                        .push(syn);
                }
                syntax_tree::item::Item::Enum(syn) => {
                    self.draft_enum(syn, module_id, handler);
                }
                syntax_tree::item::Item::Module(_) => {
                    unreachable!("submodules should've been extracted out")
                }
                syntax_tree::item::Item::Constant(syn) => {
                    let accessibility = self
                        .create_accessibility(
                            module_id.into(),
                            syn.access_modifier(),
                        )
                        .unwrap();

                    let _: ID<Constant> = self.draft_member(
                        syn,
                        module_id,
                        |syn| syn.signature().identifier(),
                        accessibility,
                        handler,
                    );
                }
                syntax_tree::item::Item::Extern(syn) => {
                    let (_, calling_convention, _, functions, _) =
                        syn.dissolve();

                    // get the calling convention of this extern
                    let calling_convention =
                        if calling_convention.value.as_deref() == Some("C") {
                            Extern::C
                        } else {
                            handler.receive(Box::new(
                                UnknownExternCallingConvention {
                                    span: calling_convention.span.clone(),
                                },
                            ));

                            Extern::Unknown
                        };

                    for function in functions {
                        let accessibility = self
                            .create_accessibility(
                                module_id.into(),
                                function.access_modifier(),
                            )
                            .unwrap();

                        let function_id: ID<Function> = self.draft_member(
                            FunctionKind::Extern(function),
                            module_id,
                            |syn| syn.signature().identifier(),
                            accessibility,
                            handler,
                        );

                        self.functions
                            .get_mut(function_id)
                            .unwrap()
                            .get_mut()
                            .definition
                            .definition =
                            FunctionDefinition::Extern(calling_convention);
                    }
                }
            };
        }

        module_id
    }
}

impl Table<Building<RwLockContainer, Drafter>> {
    #[allow(clippy::too_many_lines)]
    fn draft_positive_trait_implementation(
        &mut self,
        implementation_signature: syntax_tree::item::ImplementationSignature,
        implementation_body: syntax_tree::item::ImplementationBody,
        declared_in: ID<Module>,
        trait_id: ID<Trait>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let (_, members, _) = implementation_body.dissolve();

        // the implementation id
        let implementation_id = self
            .insert_implementation(
                trait_id,
                declared_in,
                GenericDeclaration::default(),
                Some(implementation_signature.qualified_identifier().span()),
                GenericArguments::default(),
                PositiveTraitImplementationDefinition {
                    is_const: implementation_signature
                        .const_keyword()
                        .is_none(),
                    is_final: implementation_signature
                        .final_keyword()
                        .is_some(),
                    member_ids_by_name: HashMap::default(),
                },
            )
            .unwrap();

        assert!(self
            .state
            .finalizer
            .draft_symbol(implementation_id, implementation_signature));

        for member in members {
            let trait_implementation_member_id = match member {
                ImplementationMember::Type(syn) => {
                    let accessibility = self
                        .get(trait_id)
                        .unwrap()
                        .member_ids_by_name
                        .get(syn.signature().identifier().span.str())
                        .copied()
                        .map_or(Accessibility::Public, |x| {
                            self.get_accessibility(x.into()).unwrap()
                        });

                    TraitImplementationMemberID::Type(self.draft_member(
                        syn,
                        implementation_id,
                        |syn| syn.signature().identifier(),
                        accessibility,
                        handler,
                    ))
                }
                ImplementationMember::Function(syn) => {
                    let accessibility = self
                        .get(trait_id)
                        .unwrap()
                        .member_ids_by_name
                        .get(syn.signature().identifier().span.str())
                        .copied()
                        .map_or(Accessibility::Public, |x| {
                            self.get_accessibility(x.into()).unwrap()
                        });

                    TraitImplementationMemberID::Function(self.draft_member(
                        syn,
                        implementation_id,
                        |syn| syn.signature().identifier(),
                        accessibility,
                        handler,
                    ))
                }
                ImplementationMember::Constant(syn) => {
                    let accessibility = self
                        .get(trait_id)
                        .unwrap()
                        .member_ids_by_name
                        .get(syn.signature().identifier().span.str())
                        .copied()
                        .map_or(Accessibility::Public, |x| {
                            self.get_accessibility(x.into()).unwrap()
                        });

                    TraitImplementationMemberID::Constant(self.draft_member(
                        syn,
                        implementation_id,
                        |syn| syn.signature().identifier(),
                        accessibility,
                        handler,
                    ))
                }
            };

            // find the cooresponding trait member id
            let Some(trait_member_id) = self
                .get(trait_id)
                .unwrap()
                .member_ids_by_name
                .get(
                    self.get_global(trait_implementation_member_id.into())
                        .unwrap()
                        .name(),
                )
                .copied()
            else {
                handler.receive(Box::new(UnknownTraitImplementationMember {
                    identifier_span: self
                        .get_global(trait_implementation_member_id.into())
                        .unwrap()
                        .span()
                        .cloned()
                        .unwrap(),
                    trait_id,
                }));
                continue;
            };

            match (trait_member_id, trait_implementation_member_id) {
                (
                    TraitMemberID::Type(_),
                    TraitImplementationMemberID::Type(_),
                )
                | (
                    TraitMemberID::Function(_),
                    TraitImplementationMemberID::Function(_),
                )
                | (
                    TraitMemberID::Constant(_),
                    TraitImplementationMemberID::Constant(_),
                ) => { /* do nothing, correct implementation */ }

                _ => handler.receive(Box::new(
                    MismatchedTraitMemberAndImplementationMember {
                        trait_member_id,
                        found_kind: match trait_implementation_member_id {
                            TraitImplementationMemberID::Type(_) => {
                                error::TraitMemberKind::Type
                            }
                            TraitImplementationMemberID::Function(_) => {
                                error::TraitMemberKind::Function
                            }
                            TraitImplementationMemberID::Constant(_) => {
                                error::TraitMemberKind::Constant
                            }
                        },
                        implementation_member_identifer_span: self
                            .get_global(trait_implementation_member_id.into())
                            .unwrap()
                            .span()
                            .cloned()
                            .unwrap(),
                    },
                )),
            }
        }

        let trait_sym = self.get(trait_id).unwrap();
        let implementation_sym = self.get(implementation_id).unwrap();

        // check if there are any missing members
        let unimplemented_trait_member_ids = trait_sym
            .member_ids_by_name
            .iter()
            .filter_map(|(name, id)| {
                implementation_sym
                    .member_ids_by_name
                    .contains_key(name)
                    .not()
                    .then_some(*id)
            })
            .collect::<HashSet<_>>();

        drop(trait_sym);

        if !unimplemented_trait_member_ids.is_empty() {
            handler.receive(Box::new(UnimplementedTraitMembers {
                unimplemented_trait_member_ids,
                implementation_id,
            }));
        }
    }

    #[allow(clippy::too_many_lines)]
    fn draft_adt_implementation(
        &mut self,
        implementation: syntax_tree::item::Implementation,
        declared_in: ID<Module>,
        adt_id: AdtID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let (signature, kind) = implementation.dissolve();
        let implementation_body = match kind {
            ImplementationKind::Negative(_) => {
                handler.receive(Box::new(error::NegativeImplementationOnAdt {
                    negative_implementation_span: signature
                        .qualified_identifier()
                        .span(),
                    adt_id,
                }));
                return;
            }
            ImplementationKind::Positive(body) => body,
        };
        let (_, members, _) = implementation_body.dissolve();

        // the implementation id
        let adt_implementation_id = match adt_id {
            AdtID::Struct(id) => self.insert_implementation(
                id,
                declared_in,
                GenericDeclaration::default(),
                Some(signature.qualified_identifier().span()),
                GenericArguments::default(),
                AdtImplementationDefinition::default(),
            ),
            AdtID::Enum(id) => self.insert_implementation(
                id,
                declared_in,
                GenericDeclaration::default(),
                Some(signature.qualified_identifier().span()),
                GenericArguments::default(),
                AdtImplementationDefinition::default(),
            ),
        }
        .unwrap();

        assert!(self
            .state
            .finalizer
            .draft_symbol(adt_implementation_id, signature));

        for member in members {
            match member {
                ImplementationMember::Constant(syn) => {
                    handler.receive(Box::new(
                        UnexpectedAdtImplementationMember {
                            unexpected_member_span: syn.signature().span(),
                        },
                    ));
                }
                ImplementationMember::Type(syn) => {
                    handler.receive(Box::new(
                        UnexpectedAdtImplementationMember {
                            unexpected_member_span: syn.signature().span(),
                        },
                    ));
                }
                ImplementationMember::Function(syn) => {
                    let accessibility = self
                        .create_accessibility(
                            adt_implementation_id.into(),
                            syn.access_modifier(),
                        )
                        .unwrap();

                    if let Ok(id) = self.get_member_of(
                        adt_id.into(),
                        syn.signature().identifier().span.str(),
                    ) {
                        let new_id = self.adt_implementation_functions.insert(
                            RwLock::new(AdtImplementationFunction {
                                name: syn
                                    .signature()
                                    .identifier()
                                    .span
                                    .str()
                                    .to_owned(),
                                accessibility,
                                parent_id: adt_implementation_id,
                                span: Some(
                                    syn.signature().identifier().span.clone(),
                                ),
                                generic_declaration:
                                    GenericDeclaration::default(),
                                definition: FunctionTemplate::default(),
                            }),
                        );

                        let _ = self.state.finalizer.draft_symbol(new_id, syn);

                        handler.receive(Box::new(GlobalRedifinition {
                            existing_global_id: id,
                            new_global_id: new_id.into(),
                            in_global_id: adt_id.into(),
                        }));
                    } else {
                        let _: ID<AdtImplementationFunction> = self
                            .draft_member(
                                syn,
                                adt_implementation_id,
                                |syn| syn.signature().identifier(),
                                accessibility,
                                handler,
                            );
                    }
                }
            };
        }
    }

    fn draft_trait_implementation(
        &mut self,
        implementation: syntax_tree::item::Implementation,
        declared_in: ID<Module>,
        trait_id: ID<Trait>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let (signature, kind) = implementation.dissolve();

        match kind {
            ImplementationKind::Negative(..) => {
                let negative_trait_id = self
                    .insert_implementation(
                        trait_id,
                        declared_in,
                        GenericDeclaration::default(),
                        Some(signature.qualified_identifier().span()),
                        GenericArguments::default(),
                        NegativeTraitImplementationDefinition {
                            is_final: signature.final_keyword().is_some(),
                        },
                    )
                    .unwrap();

                // draft the symbol
                assert!(self
                    .state
                    .finalizer
                    .draft_symbol(negative_trait_id, signature));
            }
            ImplementationKind::Positive(body) => {
                self.draft_positive_trait_implementation(
                    signature,
                    body,
                    declared_in,
                    trait_id,
                    handler,
                );
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    pub(in crate::symbol::table::representation) fn draft_implementation(
        &mut self,
        implementation: syntax_tree::item::Implementation,
        defined_in_module_id: ID<Module>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let mut current_id: GlobalID = match implementation
            .signature()
            .qualified_identifier()
            .root()
        {
            syntax_tree::QualifiedIdentifierRoot::Target(_) => self
                .get_root_module_id(defined_in_module_id.into())
                .unwrap()
                .into(),
            syntax_tree::QualifiedIdentifierRoot::This(keyword) => {
                handler
                    .receive(Box::new(ThisNotFound { span: keyword.span() }));
                return;
            }
            syntax_tree::QualifiedIdentifierRoot::GenericIdentifier(
                generic_identifier,
            ) => {
                let Ok(id) = self.get_member_of(
                    defined_in_module_id.into(),
                    generic_identifier.identifier().span.str(),
                ) else {
                    handler.receive(Box::new(SymbolNotFound {
                        searched_global_id: Some(defined_in_module_id.into()),
                        resolution_span: generic_identifier
                            .identifier()
                            .span
                            .clone(),
                    }));
                    return;
                };

                id
            }
        };

        if !implementation.signature().qualified_identifier().rest().is_empty()
        {
            if !current_id.is_module() {
                handler.receive(Box::new(ExpectModule {
                    module_path: implementation
                        .signature()
                        .qualified_identifier()
                        .root()
                        .span(),
                    found_id: current_id,
                }));
                return;
            }

            if let Some(gen_args) = implementation
                .signature()
                .qualified_identifier()
                .root()
                .as_generic_identifier()
                .and_then(|x| x.generic_arguments().as_ref())
            {
                handler.receive(Box::new(NoGenericArgumentsRequired {
                    global_id: current_id,
                    generic_argument_span: gen_args.span(),
                }));
            }
        }

        for (index, (_, generic_identifier)) in implementation
            .signature()
            .qualified_identifier()
            .rest()
            .iter()
            .enumerate()
        {
            let Ok(next_id) = self.get_member_of(
                current_id,
                generic_identifier.identifier().span.str(),
            ) else {
                handler.receive(Box::new(SymbolNotFound {
                    searched_global_id: Some(current_id),
                    resolution_span: generic_identifier
                        .identifier()
                        .span
                        .clone(),
                }));
                return;
            };

            if index
                == implementation
                    .signature()
                    .qualified_identifier()
                    .rest()
                    .len()
                    - 1
            {
                current_id = next_id;
            } else {
                if !next_id.is_module() {
                    handler.receive(Box::new(ExpectModule {
                        module_path: generic_identifier
                            .identifier()
                            .span
                            .clone(),
                        found_id: next_id,
                    }));
                    return;
                }

                if let Some(gen_args) =
                    generic_identifier.generic_arguments().as_ref()
                {
                    handler.receive(Box::new(NoGenericArgumentsRequired {
                        global_id: next_id,
                        generic_argument_span: gen_args.span(),
                    }));
                }
            }
        }

        match current_id {
            GlobalID::Trait(id) => self.draft_trait_implementation(
                implementation,
                defined_in_module_id,
                id,
                handler,
            ),

            adt_id @ (GlobalID::Enum(_) | GlobalID::Struct(_)) => {
                self.draft_adt_implementation(
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
mod test;
