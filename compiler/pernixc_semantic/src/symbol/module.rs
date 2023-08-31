use pernixc_syntax::syntax_tree::target::Target;

use super::{BuildError, Table, TargetNamedCoreError};
use crate::symbol::TargetNameDuplicationError;

impl Table {
    pub(super) fn create_target_module_trees(&mut self, target: Target) -> Result<(), BuildError> {
        if target.target_name() == "@core" {
            return Err(BuildError::TargetNamedCore(TargetNamedCoreError));
        }

        let (target_module_tree, target_name) = target.dissolve();

        if self
            .root_target_module_ids_by_name
            .contains_key(&target_name)
        {
            return Err(BuildError::TargetNameDuplication(
                TargetNameDuplicationError { name: target_name },
            ));
        }

        todo!()
    }
}
