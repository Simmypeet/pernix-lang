use pernixc_syntax::syntax_tree;
use pernixc_system::diagnostic::Handler;

use super::Table;
use crate::{
    error::{self, ModuleExpected, ModuleNotFound},
    symbol::{GlobalItemRef, ModuleMemberRef},
};

impl Table {
    /// Resolves for the module index from the given [`syntax_tree::item::ModulePath`].
    pub fn resolve_module_path(
        &self,
        module_path: &syntax_tree::item::ModulePath,
        handler: &impl Handler<error::Error>,
    ) -> Option<usize> {
        let mut current_module_index: Option<usize> = None;

        for path in module_path.paths() {
            let next = match current_module_index {
                // continue searching from current module
                Some(current_module_index) => 'a: {
                    let Some(id) = ({
                        // search from current module id
                        self.modules[current_module_index]
                            .module_member_refs_by_name
                            .get(path.span.str())
                            .copied()
                    }) else {
                        break 'a None;
                    };

                    // must be a module
                    let ModuleMemberRef::Module(module_index) = id else {
                        handler.receive(error::Error::ModuleExpected(ModuleExpected {
                            module_path_span: path.span.clone(),
                        }));
                        return None;
                    };

                    Some(module_index)
                }

                // search from root
                None => self
                    .target_root_module_indices_by_name
                    .get(path.span.str())
                    .copied(),
            };

            let Some(next) = next else {
                handler.receive(error::Error::ModuleNotFound(ModuleNotFound {
                    module_path_span: path.span.clone(),
                    searched_module_name: current_module_index.map(|x| {
                        self.get_qualified_name(GlobalItemRef::Module(x))
                            .expect("should've been a valid index")
                    }),
                }));
                return None;
            };

            current_module_index = Some(next);
        }

        current_module_index
    }
}
