use std::{
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher},
    sync::mpsc::sync_channel,
    time::SystemTime,
};

use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree;
use tokio::signal::unix::signal;

use super::{GlobalID, Representation, TargetID, ID};
use crate::{
    component::{
        Accessibility, LocationSpan, Member, Name, Parent, SymbolKind,
    },
    diagnostic,
    symbol::{component::input::parent, GenericID},
    table::Target,
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

    fn create_trait(
        &mut self,
        target_id: TargetID,
        syntax_tree: syntax_tree::item::Trait,
        parent_module_id: GlobalID,
        handler: &dyn Handler<Box<dyn diagnostic::Diagnostic>>,
    ) -> GlobalID {
        let trait_id = GlobalID::new(
            target_id,
            self.targets_by_id.get_mut(&target_id).unwrap().generate_id(),
        );

        let (access_modifier, siganture, content) = syntax_tree.dissolve();
        let (_, ident, generic_parameters, where_clause) = siganture.dissolve();

        assert!(self.storage.add_component(trait_id, SymbolKind::Trait));
        assert!(self.storage.add_component(
            trait_id,
            self.create_accessibility(parent_module_id, &access_modifier)
        ));
        assert!(self
            .storage
            .add_component(trait_id, Name(ident.span.str().to_owned())));
        assert!(self
            .storage
            .add_component(trait_id, LocationSpan(ident.span.clone())));
        assert!(self.storage.add_component(trait_id, Member::default()));
        assert!(self
            .storage
            .add_component(trait_id, Parent(parent_module_id.id)));

        if let Some(generic_parameters) = generic_parameters {
            assert!(self.storage.add_component(trait_id, generic_parameters));
        }
        if let Some(where_clause) = where_clause {
            assert!(self.storage.add_component(trait_id, where_clause));
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
                    }
                )
            ));
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

        // recursively create the submodules
        for (name, submodule) in submodule_by_name {
            let submodule_id = self.create_module(
                target_id,
                name.clone(),
                submodule,
                Some(module_id),
                usings_by_module_id,
                implementations_by_module_id,
                handler,
            );

            self.storage
                .get_mut::<Member>(module_id)
                .unwrap()
                .insert(name, submodule_id.id);
        }

        for item in items {
            match item {
                syntax_tree::item::Item::Trait(syn) => {}
                syntax_tree::item::Item::Function(function) => todo!(),
                syntax_tree::item::Item::Type(_) => todo!(),
                syntax_tree::item::Item::Struct(_) => todo!(),
                syntax_tree::item::Item::Implementation(implementation) => {
                    todo!()
                }
                syntax_tree::item::Item::Enum(_) => todo!(),
                syntax_tree::item::Item::Module(module) => todo!(),
                syntax_tree::item::Item::Constant(constant) => todo!(),
                syntax_tree::item::Item::Marker(marker) => todo!(),
                syntax_tree::item::Item::Extern(_) => todo!(),
            }
        }

        module_id
    }
}
