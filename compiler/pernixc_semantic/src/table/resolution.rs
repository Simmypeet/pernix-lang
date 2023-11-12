//! Contains logic related to symbol resolution.

use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree::item::ModulePath;

use super::{Error, Table};
use crate::{
    arena::ID,
    error::{self, ModuleNotFound},
    symbol::{GlobalID, Module},
};

impl Table {
    /// Resolves a module path to a module ID>
    ///
    /// # Errors
    ///
    /// - [`Error::InvalidID`]: if the `referring_site` is not a valid ID.
    /// - [`Error::SemanticError`]: if encountered a fatal semantic error e.g. a module not found.
    pub fn resolve_module_path(
        &self,
        module_path: &ModulePath,
        referring_site: GlobalID,
        handler: &dyn Handler<error::Error>,
    ) -> Result<ID<Module>, Error> {
        let mut current_module_id = None;

        // loop through the module path and resolve each module name to a module ID.
        for path in module_path.paths() {
            // the next module ID to search for.
            let next = match current_module_id {
                Some(_) => todo!(),
                None => self.root_module_ids_by_name.get(path.span.str()).copied(),
            };

            let Some(next) = next else {
                handler.receive(error::Error::ModuleNotFound(ModuleNotFound {
                    module_path: path.span.clone(),
                    searched_module_id: current_module_id,
                }));
                return Err(Error::SemanticError);
            };
        }

        Ok(current_module_id.unwrap())
    }
}
