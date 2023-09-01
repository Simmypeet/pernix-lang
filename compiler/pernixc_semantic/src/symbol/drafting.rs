use std::collections::{hash_map::Entry, HashMap};

use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::{
    item::{Item, ModulePath},
    target::{ModuleTree, Target},
    AccessModifier,
};
use pernixc_system::{arena, diagnostic::Handler};

use super::{Accessibility, BuildError, Table, TargetNamedCoreError};
use crate::{
    error::{Error, ModuleExpected, ModuleNotFound, UsingDuplication},
    symbol::{Module, TargetNameDuplicationError, ID},
};

impl Table {
    pub(super) fn draft_target(
        &mut self,
        target: Target,
        handler: &impl Handler<Error>,
    ) -> Result<(), BuildError> {
        // target name can't be "@core"
        if target.target_name() == "@core" {
            return Err(BuildError::TargetNamedCore(TargetNamedCoreError));
        }

        // target name can't be duplicated
        if self
            .target_root_module_ids_by_name
            .contains_key(target.target_name())
        {
            return Err(BuildError::TargetNameDuplication(
                TargetNameDuplicationError {
                    name: target.target_name().to_string(),
                },
            ));
        }

        let (module_tree, target_name) = target.dissolve();

        self.draft_module_tree(None, target_name, module_tree, handler);

        todo!()
    }

    fn draft_module_tree(
        &mut self,
        parent_module_id: Option<arena::ID<Module>>,
        module_name: String,
        module_tree: ModuleTree,
        handler: &impl Handler<Error>,
    ) {
        let current_module_id = self.modules.push(Module {
            accessibility: module_tree
                .signature()
                .as_ref()
                .map_or(Accessibility::Public, |x| match &x.access_modifier {
                    AccessModifier::Public(_) => Accessibility::Public,
                    AccessModifier::Private(_) => Accessibility::Private,
                    AccessModifier::Internal(_) => Accessibility::Internal,
                }),
            name: module_name.clone(),
            parent_module_id,
            children_ids_by_name: HashMap::new(),
            usings: HashMap::new(),
            syntax_tree: None,
        });

        // add to the parent module
        if let Some(parent_module_id) = parent_module_id {
            assert!(
                self.modules[parent_module_id]
                    .children_ids_by_name
                    .insert(module_name, ID::Module(current_module_id))
                    .is_none(),
                "Duplication detected, but it should've already been checked."
            );
        } else {
            assert!(
                self.target_root_module_ids_by_name
                    .insert(module_name, current_module_id)
                    .is_none(),
                "Duplication detected, but it should've already been checked."
            );
        }

        let (.., content, submodules) = module_tree.dissolve();
        let (content_using, content_module) = content.dissolve();

        // create all submodules first
        for (name, submodule) in submodules {
            self.draft_module_tree(Some(current_module_id), name, submodule, handler);
        }

        // then create the using statements
        for using in content_using {
            let Some(module_id) = self.resolve_module_path(using.module_path(), handler) else {
                continue;
            };

            match self.modules[current_module_id].usings.entry(module_id) {
                Entry::Occupied(entry) => {
                    handler.receive(Error::UsingDuplication(UsingDuplication {
                        duplicate_span: using.span(),
                        existing_using_span: entry.get().span(),
                    }));
                }
                Entry::Vacant(entry) => {
                    entry.insert(using);
                }
            }
        }

        // then create content for the module
        for item in content_module {
            match item {
                Item::Trait(_) => todo!(),
                Item::Function(_) => todo!(),
                Item::Type(_) => todo!(),
                Item::Struct(_) => todo!(),
                Item::Implements(_) => todo!(),
                Item::Enum(_) => todo!(),
                Item::Const(_) => todo!(),
                Item::Module(_) => unreachable!("submodules should've been extracted out already"),
            }
        }
    }

    fn resolve_module_path(
        &self,
        module_path: &ModulePath,
        handler: &impl Handler<Error>,
    ) -> Option<arena::ID<Module>> {
        let mut current_module_id = None;

        for path in module_path.paths() {
            let next = match current_module_id {
                // continue searching from current module
                Some(current_module_id) => 'a: {
                    // search from current module id
                    let Some(id) = self.modules[current_module_id]
                        .children_ids_by_name
                        .get(path.span.str())
                        .copied()
                    else {
                        break 'a None;
                    };

                    // must be a module
                    let ID::Module(module_id) = id else {
                        handler.receive(Error::ModuleExpected(ModuleExpected {
                            module_path_span: path.span.clone(),
                            found_symbol_id: id,
                        }));
                        return None;
                    };

                    Some(module_id)
                }

                // search from root
                None => self
                    .target_root_module_ids_by_name
                    .get(path.span.str())
                    .copied(),
            };

            let Some(next) = next else {
                handler.receive(Error::ModuleNotFound(ModuleNotFound {
                    module_path_span: path.span.clone(),
                    serached_module_id: current_module_id,
                }));
                return None;
            };

            current_module_id = Some(next);
        }

        current_module_id
    }
}
