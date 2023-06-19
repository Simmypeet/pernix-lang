use std::{collections::HashSet, ops::Deref, path::Path};

use pernixc_system::{arena, input::Input};
use proptest::{
    prop_assert_eq,
    test_runner::{ResultCache, TestCaseError, TestCaseResult},
};

use super::Table;
use crate::{
    ty, Accessibility, GenericParameters, Genericable, Module, ModuleChildID, Struct, WhereClause,
};

impl Table {
    fn create_file(
        &self,
        module_id: arena::ID<Module>,
        parent_directory: &Path,
    ) -> Result<(), std::io::Error> {
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

                    writeln!(
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

    fn write_generic_parameters(
        &self,
        file: &mut std::fs::File,
        generic_parameters: &GenericParameters,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        if !generic_parameters.lifetime_parameter_order.is_empty()
            || !generic_parameters.type_parameter_order.is_empty()
        {
            let generic_parameters = generic_parameters
                .lifetime_parameter_order
                .iter()
                .copied()
                .map(|x| format!("`{}", self.lifetime_parameters[x].name.clone()))
                .chain(
                    generic_parameters
                        .type_parameter_order
                        .iter()
                        .copied()
                        .map(|x| self.type_parameters[x].name.clone()),
                )
                .collect::<Vec<_>>()
                .join(", ");

            write!(file, "<{generic_parameters}>")?;
        }

        Ok(())
    }

    fn write_ty_primitive_type(
        file: &mut std::fs::File,
        primitive_type: ty::PrimitiveType,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        match primitive_type {
            ty::PrimitiveType::Float32 => write!(file, "float32"),
            ty::PrimitiveType::Float64 => write!(file, "float64"),
            ty::PrimitiveType::Int8 => write!(file, "int8"),
            ty::PrimitiveType::Int16 => write!(file, "int16"),
            ty::PrimitiveType::Int32 => write!(file, "int32"),
            ty::PrimitiveType::Int64 => write!(file, "int64"),
            ty::PrimitiveType::Uint8 => write!(file, "uint8"),
            ty::PrimitiveType::Uint16 => write!(file, "uint16"),
            ty::PrimitiveType::Uint32 => write!(file, "uint32"),
            ty::PrimitiveType::Uint64 => write!(file, "uint64"),
            ty::PrimitiveType::Bool => write!(file, "bool"),
            ty::PrimitiveType::Void => write!(file, "void"),
        }
    }

    fn write_ty_type(&self, file: &mut std::fs::File, ty: &ty::Type) -> Result<(), std::io::Error> {
        match ty {
            ty::Type::Struct(_) => todo!(),
            ty::Type::PrimitiveType(primitive_type) => {
                Self::write_ty_primitive_type(file, *primitive_type)
            }
            ty::Type::ReferenceType(_) => todo!(),
            ty::Type::TypeParameter(_) => todo!(),
            ty::Type::TraitType(_) => todo!(),
        }
    }

    fn write_where_caluse(
        &self,
        file: &mut std::fs::File,
        where_clause: &WhereClause,
    ) -> Result<(), TestCaseError> {
        use std::io::Write;

        if !where_clause.lifetime_bounds.is_empty()
            || !where_clause.type_bounds_by_trait_type.is_empty()
            || !where_clause.lifetime_bound_vecs_by_trait_type.is_empty()
            || !where_clause.type_parameter_bounds.is_empty()
        {}

        Ok(())
    }

    fn write_struct(
        &self,
        file: &mut std::fs::File,
        struct_id: arena::ID<Struct>,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;
        let struct_symbol = &self.structs[struct_id];

        write!(file, "{} struct", struct_symbol.accessibility)?;

        let test = self.scope_walker(struct_id.into()).unwrap().rev();

        // write generics parameters
        self.write_generic_parameters(file, &struct_symbol.generics.generic_parameters)?;

        write!(file, " {}", struct_symbol.name)?;

        Ok(())
    }

    fn create_target(
        &self,
        root_target_module_id: arena::ID<Module>,
        target_name: &str,
        parent_directory: &Path,
    ) -> Result<(), std::io::Error> {
        // creates target directory
        let target_directory = parent_directory.join(target_name);
        std::fs::create_dir(&target_directory)?;

        self.create_file(root_target_module_id, &target_directory)
    }

    /// Creates a workspace based on the current state of the table in a temporary directory.
    pub(super) fn create_workspace(&self) -> Result<tempfile::TempDir, std::io::Error> {
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
