use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::{Hash, Hasher},
    time::SystemTime,
};

use pernixc_base::handler::Handler;
use pernixc_lexical::token::Identifier;
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{
    diagnostic::SymbolIsMoreAccessibleThanParent, GlobalID, Representation,
    TargetID, ID,
};
use crate::{
    component::{
        self, Accessibility, Extern, LocationSpan, Member, Name, Parent,
        SymbolKind,
    },
    diagnostic,
    table::{
        diagnostic::{ItemRedifinition, UnknownExternCallingConvention},
        Target,
    },
};

/// Errors that can occur when adding a target to the representation.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    thiserror::Error,
    displaydoc::Display,
)]
pub enum AddTargetError {
    /// The target name is already in use.
    DuplicateTargetName(String),

    /// The `linked_targets` ids contain an unknown target.
    UnknownTargetLink(TargetID),
}

impl Representation {
    /// Adds a target to the representation.
    ///
    /// # Errors
    ///
    /// See [`AddTargetError`] for possible errors.
    pub fn add_target(
        &mut self,
        name: String,
        linked_targets: impl IntoIterator<Item = TargetID>,
        target_syntax: syntax_tree::target::Target,
        handler: &dyn Handler<Box<dyn diagnostic::Diagnostic>>,
    ) -> Result<TargetID, AddTargetError> {
        // make sure every target links to the core target
        let mut linked_targets_vec =
            std::iter::once(TargetID::CORE).collect::<HashSet<_>>();

        for linked_target in linked_targets {
            if !self.targets_by_id.contains_key(&linked_target)
                && linked_target != TargetID::CORE
            {
                return Err(AddTargetError::UnknownTargetLink(linked_target));
            }

            linked_targets_vec.insert(linked_target);
        }

        // check if the target name is unique
        if name == "core" || self.targets_by_name.contains_key(&name) {
            return Err(AddTargetError::DuplicateTargetName(name));
        }

        let mut instant = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_nanos();

        let mut target_id = {
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            name.hash(&mut hasher);
            instant.hash(&mut hasher);
            TargetID(hasher.finish() as usize)
        };

        // keep generating target IDs until we find a unique one
        while self.targets_by_id.contains_key(&target_id)
            || target_id == TargetID::CORE
        {
            instant += 1;
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            name.hash(&mut hasher);
            instant.hash(&mut hasher);
            target_id = TargetID(hasher.finish() as usize);
        }

        let target =
            Target { generated_ids: 1, linked_targets: linked_targets_vec };

        // add target information
        assert!(self.targets_by_id.insert(target_id, target).is_none());
        assert!(self.targets_by_name.insert(name.clone(), target_id).is_none());

        let mut usings_by_module_id = HashMap::new();
        let mut implementations_by_module_id = HashMap::new();

        // create the root module
        self.create_module(
            target_id,
            name,
            target_syntax.dissolve().0,
            None,
            &mut usings_by_module_id,
            &mut implementations_by_module_id,
            handler,
        );

        Ok(target_id)
    }

    fn insert_member(
        &mut self,
        parent_id: GlobalID,
        name: &Identifier,
        symbol_kind: SymbolKind,
        accessibility: Option<Accessibility>,
        has_member: bool,
        generic_parameters_syn: Option<syntax_tree::item::GenericParameters>,
        where_clause_syn: Option<syntax_tree::item::WhereClause>,
        handler: &dyn Handler<Box<dyn diagnostic::Diagnostic>>,
    ) -> GlobalID {
        let new_symbol_id = GlobalID::new(
            parent_id.target_id,
            self.targets_by_id
                .get_mut(&parent_id.target_id)
                .unwrap()
                .generate_id(),
        );

        assert!(self
            .storage
            .add_component(new_symbol_id, LocationSpan(name.span.clone())));
        assert!(self
            .storage
            .add_component(new_symbol_id, Name(name.span.str().to_owned())));
        assert!(self
            .storage
            .add_component(new_symbol_id, Parent(parent_id.id)));
        assert!(self.storage.add_component(new_symbol_id, symbol_kind));

        if let Some(accessibility) = accessibility {
            assert!(self.storage.add_component(new_symbol_id, accessibility));
        }

        if has_member {
            assert!(self
                .storage
                .add_component(new_symbol_id, Member::default()));
        }

        if let Some(generic_parameters_syn) = generic_parameters_syn {
            assert!(self
                .storage
                .add_component(new_symbol_id, generic_parameters_syn,));
        }
        if let Some(where_clause_syn) = where_clause_syn {
            assert!(self
                .storage
                .add_component(new_symbol_id, where_clause_syn,));
        }

        let mut parent = self.storage.get_mut::<Member>(parent_id).unwrap();

        match parent.entry(name.span.str().to_owned()) {
            Entry::Occupied(entry) => {
                handler.receive(Box::new(ItemRedifinition {
                    existing_id: *entry.get(),
                    new_id: new_symbol_id.id,
                    in_id: parent_id,
                }));
            }
            Entry::Vacant(entry) => {
                entry.insert(new_symbol_id.id);
            }
        }

        new_symbol_id
    }

    fn create_enum(
        &mut self,
        syntax_tree: syntax_tree::item::Enum,
        parent_module_id: GlobalID,
        handler: &dyn Handler<Box<dyn diagnostic::Diagnostic>>,
    ) -> GlobalID {
        let (access_modifier, signature, body) = syntax_tree.dissolve();
        let (_, ident, generic_parameters, where_clause) = signature.dissolve();

        let enum_id = self.insert_member(
            parent_module_id,
            &ident,
            SymbolKind::Enum,
            Some(
                self.create_accessibility(parent_module_id, &access_modifier)
                    .unwrap(),
            ),
            true,
            generic_parameters,
            where_clause,
            handler,
        );

        for variant in
            body.dissolve().1.into_iter().flat_map(ConnectedList::into_elements)
        {
            let (ident, association) = variant.dissolve();

            let variant_id = self.insert_member(
                enum_id,
                &ident,
                SymbolKind::Variant,
                None,
                false,
                None,
                None,
                handler,
            );

            if let Some(association) = association {
                assert!(self
                    .storage
                    .add_component(variant_id, association.dissolve().1));
            }
        }

        enum_id
    }

    fn create_trait(
        &mut self,
        syntax_tree: syntax_tree::item::Trait,
        parent_module_id: GlobalID,
        handler: &dyn Handler<Box<dyn diagnostic::Diagnostic>>,
    ) -> GlobalID {
        let (access_modifier, signature, content) = syntax_tree.dissolve();
        let (_, ident, generic_parameters, where_clause) = signature.dissolve();

        let trait_id = self.insert_member(
            parent_module_id,
            &ident,
            SymbolKind::Trait,
            Some(
                self.create_accessibility(parent_module_id, &access_modifier)
                    .unwrap(),
            ),
            true,
            generic_parameters,
            where_clause,
            handler,
        );

        for item in content.dissolve().1 {
            let member_id = match item {
                syntax_tree::item::TraitMember::Function(trait_function) => {
                    let (access_modifier, signature, _) =
                        trait_function.dissolve();

                    let (
                        _,
                        ident,
                        generic_parameters_syn,
                        parameters,
                        return_type,
                        where_clause_syn,
                    ) = signature.dissolve();

                    let trait_function_id = self.insert_member(
                        trait_id,
                        &ident,
                        SymbolKind::TraitFunction,
                        Some(
                            self.create_accessibility(
                                trait_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        false,
                        generic_parameters_syn,
                        where_clause_syn,
                        handler,
                    );

                    // add the parameters
                    assert!(self
                        .storage
                        .add_component(trait_function_id, parameters));

                    // add the return type
                    if let Some(return_type) = return_type {
                        assert!(self
                            .storage
                            .add_component(trait_function_id, return_type));
                    }

                    trait_function_id
                }
                syntax_tree::item::TraitMember::Type(trait_type) => {
                    let (access_modifier, signature, where_clause, _) =
                        trait_type.dissolve();

                    let (_, ident, generic_parameters) = signature.dissolve();

                    self.insert_member(
                        trait_id,
                        &ident,
                        SymbolKind::TraitType,
                        Some(
                            self.create_accessibility(
                                trait_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        false,
                        generic_parameters,
                        where_clause,
                        handler,
                    )
                }
                syntax_tree::item::TraitMember::Constant(trait_constant) => {
                    let (access_modifier, signature, where_clause, _) =
                        trait_constant.dissolve();
                    let (_, ident, generic_parameters, _, ty) =
                        signature.dissolve();

                    let trait_constant_id = self.insert_member(
                        trait_id,
                        &ident,
                        SymbolKind::Constant,
                        Some(
                            self.create_accessibility(
                                trait_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        false,
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    assert!(self.storage.add_component(trait_constant_id, ty));

                    trait_constant_id
                }
            };

            let member_accessibility =
                self.get_component::<Accessibility>(member_id).unwrap();

            if self
                .accessibility_hierarchy_relationship(
                    parent_module_id.target_id,
                    *member_accessibility,
                    *self.get_component(trait_id).unwrap(),
                )
                .unwrap()
                == component::HierarchyRelationship::Parent
            {
                handler.receive(Box::new(SymbolIsMoreAccessibleThanParent {
                    symbol_id: member_id,
                    parent_id: trait_id,
                }));
            }
        }

        trait_id
    }

    fn create_module(
        &mut self,
        target_id: TargetID,
        name: String,
        syntax_tree: syntax_tree::target::ModuleTree,
        parent_module_id: Option<GlobalID>,
        usings_by_module_id: &mut HashMap<
            GlobalID,
            Vec<syntax_tree::item::Using>,
        >,
        implementations_by_module_id: &mut HashMap<
            GlobalID,
            Vec<syntax_tree::item::Implementation>,
        >,
        handler: &dyn Handler<Box<dyn diagnostic::Diagnostic>>,
    ) -> GlobalID {
        // create the module id that we will operate on
        let module_id = GlobalID::new(
            target_id,
            if parent_module_id.is_some() {
                self.targets_by_id.get_mut(&target_id).unwrap().generate_id()
            } else {
                ID::ROOT_MODULE
            },
        );

        // has the parent module id
        if let Some(parent_module_id) = parent_module_id {
            assert!(self
                .storage
                .add_component(module_id, Parent(parent_module_id.id)));
            assert!(self.storage.add_component(
                module_id,
                syntax_tree.signature().as_ref().map_or(
                    Accessibility::Public,
                    |x| {
                        self.create_accessibility(
                            parent_module_id,
                            &x.access_modifier,
                        )
                        .unwrap()
                    }
                )
            ));
            assert!(self
                .storage
                .get_mut::<Member>(parent_module_id)
                .unwrap()
                .insert(name.clone(), module_id.id)
                .is_none());
        } else {
            assert!(self
                .storage
                .add_component(module_id, Accessibility::Public));
        }

        assert!(self.storage.add_component(module_id, SymbolKind::Module));
        assert!(self.storage.add_component(module_id, Member::default()));
        assert!(self.storage.add_component(module_id, Name(name.clone())));

        let (_, content, submodule_by_name) = syntax_tree.dissolve();
        let (usings, items) = content.dissolve();

        usings_by_module_id.entry(module_id).or_default().extend(usings);

        // recursively create the submodules, redifinitions are handled by the
        // target parsing logic already
        for (name, submodule) in submodule_by_name {
            self.create_module(
                target_id,
                name.clone(),
                submodule,
                Some(module_id),
                usings_by_module_id,
                implementations_by_module_id,
                handler,
            );
        }

        for item in items {
            match item {
                syntax_tree::item::Item::Trait(syn) => {
                    self.create_trait(syn, module_id, handler);
                }
                syntax_tree::item::Item::Function(syn) => {
                    let (access_modifier, _, signature, body) = syn.dissolve();
                    let (
                        _,
                        ident,
                        generic_parameters,
                        parameters,
                        return_type,
                        where_clause,
                    ) = signature.dissolve();

                    let function_id = self.insert_member(
                        module_id,
                        &ident,
                        SymbolKind::Function,
                        Some(
                            self.create_accessibility(
                                module_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        false,
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    // add the parameters
                    assert!(self
                        .storage
                        .add_component(function_id, parameters));

                    // add the return type
                    if let Some(return_type) = return_type {
                        assert!(self
                            .storage
                            .add_component(function_id, return_type));
                    }

                    // add the body
                    assert!(self.storage.add_component(function_id, body));
                }
                syntax_tree::item::Item::Type(syn) => {
                    let (access_modifier, signature, definition, _) =
                        syn.dissolve();
                    let (_, ident, generic_parameters) = signature.dissolve();
                    let (_, ty, where_clause) = definition.dissolve();

                    let ty_id = self.insert_member(
                        module_id,
                        &ident,
                        SymbolKind::Type,
                        Some(
                            self.create_accessibility(
                                module_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        false,
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    assert!(self.storage.add_component(ty_id, ty));
                }
                syntax_tree::item::Item::Struct(syn) => {
                    let (access_modifier, signature, body) = syn.dissolve();
                    let (_, ident, generic_parameters, where_clause) =
                        signature.dissolve();

                    let struct_id = self.insert_member(
                        module_id,
                        &ident,
                        SymbolKind::Struct,
                        Some(
                            self.create_accessibility(
                                module_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        true,
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    assert!(self.storage.add_component(struct_id, body));
                }
                syntax_tree::item::Item::Implementation(implementation) => {
                    implementations_by_module_id
                        .entry(module_id)
                        .or_default()
                        .push(implementation);
                }
                syntax_tree::item::Item::Enum(syn) => {
                    self.create_enum(syn, module_id, handler);
                }
                syntax_tree::item::Item::Module(_) => {
                    unreachable!("should've been extracted out")
                }
                syntax_tree::item::Item::Constant(constant) => {
                    let (access_modifier, signature, definition) =
                        constant.dissolve();
                    let (_, ident, generic_parameters, _, ty) =
                        signature.dissolve();
                    let (_, expression, where_caluse, _) =
                        definition.dissolve();

                    let constant_id = self.insert_member(
                        module_id,
                        &ident,
                        SymbolKind::Constant,
                        Some(
                            self.create_accessibility(
                                module_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        false,
                        generic_parameters,
                        where_caluse,
                        handler,
                    );

                    assert!(self.storage.add_component(constant_id, ty));
                    assert!(self
                        .storage
                        .add_component(constant_id, expression));
                }
                syntax_tree::item::Item::Marker(marker) => {
                    let (access_modifier, signature, _) = marker.dissolve();
                    let (_, ident, generic_parameters, where_clause) =
                        signature.dissolve();

                    self.insert_member(
                        module_id,
                        &ident,
                        SymbolKind::Marker,
                        Some(
                            self.create_accessibility(
                                module_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        false,
                        generic_parameters,
                        where_clause,
                        handler,
                    );
                }
                syntax_tree::item::Item::Extern(syn) => {
                    let (_, calling_convention, functions) = syn.dissolve();

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

                    for function in functions.dissolve().1 {
                        let (access_modifier, signature, _) =
                            function.dissolve();

                        let (
                            _,
                            ident,
                            generic_parameters,
                            parameters,
                            return_type,
                            where_clause,
                        ) = signature.dissolve();

                        let function_id = self.insert_member(
                            module_id,
                            &ident,
                            SymbolKind::Function,
                            Some(
                                self.create_accessibility(
                                    module_id,
                                    &access_modifier,
                                )
                                .unwrap(),
                            ),
                            false,
                            generic_parameters,
                            where_clause,
                            handler,
                        );

                        // add the parameters
                        assert!(self
                            .storage
                            .add_component(function_id, parameters));

                        // add the return type
                        if let Some(return_type) = return_type {
                            assert!(self
                                .storage
                                .add_component(function_id, return_type));
                        }

                        // add the extern calling convention
                        assert!(self
                            .storage
                            .add_component(function_id, calling_convention));
                    }
                }
            }
        }

        module_id
    }
}

#[cfg(test)]
mod test;
