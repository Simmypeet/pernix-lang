use std::{collections::HashSet, ops::Deref, path::Path};

use derive_more::From;
use pernixc_system::{arena, input::Input};
use proptest::{
    prop_assert_eq,
    test_runner::{TestCaseError, TestCaseResult},
};

use super::Table;
use crate::{Accessibility, Module, ModuleChildID};

#[derive(Debug, thiserror::Error, From)]
#[allow(missing_docs)]
pub(super) enum WorkspaceCreateError {
    #[error("{0}")]
    Io(std::io::Error),

    #[error("{0}")]
    Fmt(std::fmt::Error),
}

impl Table {
    fn create_file(
        &self,
        module_id: arena::ID<Module>,
        parent_directory: &Path,
    ) -> Result<(), WorkspaceCreateError> {
        use std::io::Write;
        let module = &self.modules[module_id];

        let file_path = {
            let mut file_path = if module.parent_module_id.is_some() {
                parent_directory.join(&module.name)
            } else {
                parent_directory.join("main")
            };
            file_path.set_extension("pnx");
            file_path
        };

        let mut file = std::fs::File::create(&file_path).unwrap();

        // write usings to the file
        for using_module in module.usings.iter().copied() {
            writeln!(
                file,
                "using {};",
                self.get_qualified_name(using_module.into())
                    .expect("should be a valid ID")
            )?;
        }

        if !self.modules.is_empty() && module.parent_module_id.is_some() {
            // create sub directory
            std::fs::create_dir(file_path.with_extension(""))?;
        }

        // write submodules
        for module_child_id in module.module_child_ids_by_name.values().copied() {
            match module_child_id {
                ModuleChildID::Module(module_id) => {
                    let submodule = self.modules[module_id].clone();

                    if module.parent_module_id.is_none() {
                        self.create_file(module_id, parent_directory)?;
                    } else {
                        self.create_file(module_id, &file_path.with_extension(""))?;
                    }

                    write!(
                        file,
                        "{} module {};",
                        match submodule.accessibility {
                            Accessibility::Private => "private",
                            Accessibility::Internal => "internal",
                            Accessibility::Public => "public",
                        },
                        submodule.name
                    )?;
                }
                ModuleChildID::Struct(_) => todo!(),
                ModuleChildID::Enum(_) => todo!(),
                ModuleChildID::Function(_) => todo!(),
                ModuleChildID::Type(_) => todo!(),
                ModuleChildID::Trait(_) => todo!(),
            }
        }

        Ok(())
    }

    fn create_target(
        &self,
        root_target_module_id: arena::ID<Module>,
        target_name: &str,
        parent_directory: &Path,
    ) -> Result<(), WorkspaceCreateError> {
        // creates target directory
        let target_directory = parent_directory.join(target_name);
        std::fs::create_dir(&target_directory)?;

        self.create_file(root_target_module_id, &target_directory)
    }

    /// Creates a workspace based on the current state of the table in a temporary directory.
    pub(super) fn create_workspace(&self) -> Result<tempfile::TempDir, WorkspaceCreateError> {
        let tempdir = tempfile::tempdir()?;

        for (name, module_id) in &self.target_root_module_ids_by_name {
            self.create_target(*module_id, name, tempdir.path())?;
        }

        Ok(tempdir)
    }
}

#[derive(Debug, Clone, Copy)]
struct SymbolRef<'a, T> {
    table: &'a Table,
    symbol_id: arena::ID<T>,
}

impl<'a> Deref for SymbolRef<'a, Module> {
    type Target = Module;

    fn deref(&self) -> &Self::Target { &self.table.modules[self.symbol_id] }
}

impl Input for Table {
    type Output = Self;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(
            self.target_root_module_ids_by_name
                .keys()
                .collect::<HashSet<_>>(),
            output
                .target_root_module_ids_by_name
                .keys()
                .collect::<HashSet<_>>()
        );

        for module_name in self.target_root_module_ids_by_name.keys() {
            let input_module_id = self.target_root_module_ids_by_name[module_name];
            let output_module_id = output.target_root_module_ids_by_name[module_name];

            SymbolRef {
                table: self,
                symbol_id: input_module_id,
            }
            .assert(&SymbolRef {
                table: output,
                symbol_id: output_module_id,
            })?;
        }

        Ok(())
    }
}

impl<'a> Input for SymbolRef<'a, Module> {
    type Output = SymbolRef<'a, Module>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(&self.name, &output.name);
        prop_assert_eq!(self.accessibility, output.accessibility);

        match (self.parent_module_id, output.parent_module_id) {
            (Some(input_parent_id), Some(output_parent_id)) => {
                // equality test based on qualified name
                prop_assert_eq!(
                    self.table
                        .get_qualified_name(input_parent_id.into())
                        .unwrap(),
                    output
                        .table
                        .get_qualified_name(output_parent_id.into())
                        .unwrap()
                );
            }
            (None, None) => {}
            _ => {
                return Err(TestCaseError::fail(
                    "`parent_module_id` mismatch".to_string(),
                ))
            }
        }

        // equality test based on qualified name
        prop_assert_eq!(
            self.usings
                .iter()
                .copied()
                .map(|x| self.table.get_qualified_name(x.into()))
                .collect::<HashSet<_>>(),
            output
                .usings
                .iter()
                .copied()
                .map(|x| output.table.get_qualified_name(x.into()))
                .collect::<HashSet<_>>()
        );

        prop_assert_eq!(
            self.module_child_ids_by_name.keys().collect::<HashSet<_>>(),
            output
                .module_child_ids_by_name
                .keys()
                .collect::<HashSet<_>>()
        );

        for module_child_name in self.module_child_ids_by_name.keys() {
            let input_module_child_id = self.module_child_ids_by_name[module_child_name];
            let output_module_child_id = output.module_child_ids_by_name[module_child_name];

            match (input_module_child_id, output_module_child_id) {
                (
                    ModuleChildID::Module(input_module_id),
                    ModuleChildID::Module(output_module_id),
                ) => SymbolRef {
                    table: self.table,
                    symbol_id: input_module_id,
                }
                .assert(&SymbolRef {
                    table: output.table,
                    symbol_id: output_module_id,
                })?,
                (ModuleChildID::Struct(_), ModuleChildID::Struct(_))
                | (ModuleChildID::Enum(_), ModuleChildID::Enum(_))
                | (ModuleChildID::Function(_), ModuleChildID::Function(_))
                | (ModuleChildID::Type(_), ModuleChildID::Type(_))
                | (ModuleChildID::Trait(_), ModuleChildID::Trait(_)) => (),
                (_, _) => {
                    return Err(TestCaseError::fail(format!(
                        "expected {input_module_child_id:?}, got {output_module_child_id:?}",
                    )))
                }
            }
        }

        Ok(())
    }
}
