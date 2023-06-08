use std::collections::{HashMap, HashSet};

use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::target::{File, ModulePath, Target, Using};
use pernixc_system::diagnostic::Handler;

use super::{BuildError, CoreTargetNameError, Error, Result, Table, TargetDuplicationError};
use crate::{
    error::{self, ModuleNotFound, TargetNotFound, UsingOwnModule},
    Accessibility, Module, ModuleID,
};

// Checks for the target input validity
pub(super) fn target_check(targets: &[Target]) -> std::result::Result<(), BuildError> {
    let mut duplicated_names = HashSet::new();
    let mut found_names = HashSet::new();

    for target in targets {
        if target.name() == "@core" {
            return Err(BuildError::CoreTargetName(CoreTargetNameError));
        }

        if !found_names.insert(target.name()) {
            duplicated_names.insert(target.name().to_string());
        }
    }

    if duplicated_names.is_empty() {
        Ok(())
    } else {
        Err(BuildError::TargetDuplication(TargetDuplicationError {
            duplication_target_names: duplicated_names.into_iter().collect(),
        }))
    }
}

impl Table {
    fn stem_from(&mut self, file: &File, parent_module_id: ModuleID) {
        for submodule in &file.submodules {
            let module_name = submodule.module.identifier.span.str().to_string();
            // Adds a submodule to the module list
            let id = self.modules.insert(Module {
                name: module_name.clone(),
                accessibility: Accessibility::from_syntax_tree(&submodule.module.access_modifier),
                parent_module_id: Some(parent_module_id),
                child_ids_by_name: HashMap::new(),
                usings: Vec::new(),
            });

            assert!(
                self.modules[parent_module_id]
                    .child_ids_by_name
                    .insert(module_name, id.into())
                    .is_none(),
                "Duplication detected, but it should've already been checked."
            );

            self.stem_from(&submodule.file, id);
        }
    }

    fn create_modules_from_target(&mut self, target: &Target) {
        // create a root module
        let root_module_id = self.modules.insert(Module {
            name: target.name().clone(),
            accessibility: Accessibility::Public,
            parent_module_id: None,
            child_ids_by_name: HashMap::new(),
            usings: Vec::new(),
        });

        self.root_module_ids_by_target_name
            .insert(target.name().clone(), root_module_id);

        self.stem_from(target, root_module_id);
    }

    // Creates module trees from the given targets.
    pub(super) fn create_modules(&mut self, targets: &[Target]) {
        for target in targets {
            self.create_modules_from_target(target);
        }
    }

    fn get_module_id_from_module_path(
        &self,
        module_path: &ModulePath,
        handler: &impl Handler<error::Error>,
    ) -> Result<ModuleID> {
        let mut current_module_id = None;

        for path in module_path.elements() {
            if let Some(module_id) = current_module_id {
                // search from current module id
                let Some(new_module_id) = self.modules[module_id]
                    .child_ids_by_name
                    .get(path.span.str())
                    .copied() else {
                    handler.recieve(error::Error::ModuleNotFound(
                        ModuleNotFound {
                            in_module_id: module_id,
                            unknown_module_span: path.span.clone(),
                        }
                    ));
                    return Err(Error);
                };

                current_module_id = Some(new_module_id.into_module().unwrap());
            } else {
                // search from the root
                let Some(module_id) = self.root_module_ids_by_target_name
                    .get(path.span.str())
                    .copied() else {
                    handler.recieve(error::Error::TargetNotFound(
                        TargetNotFound {
                            unknown_target_span: path.span.clone()
                        }
                    ));
                    return Err(Error);
                };

                current_module_id = Some(module_id);
            }
        }

        Ok(current_module_id.unwrap())
    }

    fn populate_usings_in_module(
        &mut self,
        module_id: ModuleID,
        usings: &[Using],
        handler: &impl Handler<error::Error>,
    ) {
        let mut using_spans_by_module_id = HashMap::new();

        for using in usings {
            // resolve module_path
            let Ok(using_module_id) = self.get_module_id_from_module_path(
                &using.module_path,
                handler) else {
                continue;
            };

            let using_span = using.span().unwrap();

            // check if using own module
            if using_module_id == module_id {
                handler.recieve(error::Error::UsingOwnModule(UsingOwnModule {
                    module_id,
                    using_span: using_span.clone(),
                }));

                continue;
            }

            // check if already using
            if let Some(previous_using_span) =
                using_spans_by_module_id.insert(using_module_id, using_span.clone())
            {
                handler.recieve(error::Error::UsingDuplication(error::UsingDuplication {
                    previous_using_span,
                    duplicate_using_span: using_span,
                }));
            }
        }

        let module = &mut self.modules[module_id];
        for module_id in using_spans_by_module_id.into_keys() {
            module.usings.push(module_id);
        }
    }

    fn populate_usings_in_submodules(
        &mut self,
        current_module_id: ModuleID,
        current_file: &File,
        handler: &impl Handler<error::Error>,
    ) {
        // populate usings for this file
        self.populate_usings_in_module(current_module_id, &current_file.usings, handler);

        // populate usings for submodules
        for submodule in &current_file.submodules {
            let module_id = self.modules[current_module_id].child_ids_by_name
                [submodule.module.identifier.span.str()]
            .into_module()
            .unwrap();

            self.populate_usings_in_submodules(module_id, &submodule.file, handler);
        }
    }

    // Populates usings in the given module.
    pub(super) fn populate_usings_in_targets(
        &mut self,
        targets: &[Target],
        handler: &impl Handler<error::Error>,
    ) {
        for target in targets {
            let root_module_id = self.root_module_ids_by_target_name[target.name()];

            self.populate_usings_in_submodules(root_module_id, target, handler);
        }
    }
}
