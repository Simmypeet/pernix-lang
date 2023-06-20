use std::{collections::HashSet, ops::Deref, path::Path};

use pernixc_system::{arena, input::Input};
use proptest::{
    prop_assert_eq,
    test_runner::{TestCaseError, TestCaseResult},
};

use super::Table;
use crate::{
    ty::{self, ReferenceQualifier},
    Enum, EnumVariant, GenericParameters, GlobalID, LifetimeArgument, Module, ModuleChildID,
    Struct, Substitution, TraitTypeBound, TypeParameter, WhereClause,
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
                        submodule.accessibility, submodule.name
                    )?;
                }
                ModuleChildID::Struct(struct_id) => self.write_struct(&mut file, struct_id)?,
                ModuleChildID::Enum(enum_id) => self.write_enum(&mut file, enum_id)?,
                ModuleChildID::Function(_) => todo!(),
                ModuleChildID::Type(_) => todo!(),
                ModuleChildID::Trait(_) => todo!(),
            }
        }

        Ok(())
    }

    fn write_enum(
        &self,
        file: &mut std::fs::File,
        enum_id: arena::ID<Enum>,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        write!(
            file,
            "{} {} enum {{",
            self.enums[enum_id].accessibility, self.enums[enum_id].name
        )?;

        let enum_symbol = &self.enums[enum_id];

        let mut first = true;
        for variant in enum_symbol
            .variant_order
            .iter()
            .copied()
            .map(|x| &self.enum_variants[x])
        {
            if !first {
                write!(file, ", ")?;
            }

            write!(file, "{}", variant.name)?;

            first = false;
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

    fn write_ty_global_id(
        &self,
        file: &mut std::fs::File,
        global_id: GlobalID,
        substitution: &Substitution,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        let parent_to_id_scopes_vec = self.parent_to_id_scopes_vec(global_id.into()).unwrap();

        for id in parent_to_id_scopes_vec
            .into_iter()
            .map(|x| GlobalID::try_from(x).unwrap())
        {
            write!(file, "::{}", self.get_global(id).unwrap().name())?;

            if let Ok(genericable_id) = id.try_into() {
                let genericable = self.get_genericable(genericable_id).unwrap();

                if !genericable
                    .generic_parameters()
                    .lifetime_parameter_order
                    .is_empty()
                    || !genericable
                        .generic_parameters()
                        .type_parameter_order
                        .is_empty()
                {
                    write!(file, "<")?;

                    let mut is_first = true;

                    for lifetime_parameter_id in
                        &genericable.generic_parameters().lifetime_parameter_order
                    {
                        if !is_first {
                            write!(file, ", ")?;
                        }

                        match substitution.lifetime_arguments_by_parameter[lifetime_parameter_id] {
                            LifetimeArgument::Static => write!(file, "'static")?,
                            LifetimeArgument::Parameter(lt) => {
                                write!(file, "'{}", self.lifetime_parameters[lt].name)?;
                            }
                        }

                        is_first = false;
                    }

                    for type_parameter_id in &genericable.generic_parameters().type_parameter_order
                    {
                        if !is_first {
                            write!(file, ", ")?;
                        }

                        self.write_ty_type(
                            file,
                            &substitution.type_arguments_by_parameter[type_parameter_id],
                        )?;

                        is_first = false;
                    }

                    write!(file, ">")?;
                }
            }
        }

        Ok(())
    }

    fn write_ty_struct(
        &self,
        file: &mut std::fs::File,
        struct_ty: &ty::Struct,
    ) -> Result<(), std::io::Error> {
        self.write_ty_global_id(file, struct_ty.struct_id.into(), &struct_ty.substitution)
    }

    fn write_ty_trait_type(
        &self,
        file: &mut std::fs::File,
        trait_type_ty: &ty::TraitType,
    ) -> Result<(), std::io::Error> {
        self.write_ty_global_id(
            file,
            trait_type_ty.trait_type_id.into(),
            &trait_type_ty.substitution,
        )
    }

    fn write_type_parameter(
        &self,
        file: &mut std::fs::File,
        type_parameter_id: arena::ID<TypeParameter>,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        write!(file, "{}", self.type_parameters[type_parameter_id].name)?;

        Ok(())
    }

    fn write_ty_reference_type(
        &self,
        file: &mut std::fs::File,
        reference_ty: &ty::ReferenceType,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        write!(file, "&")?;

        if let Some(lifetime_argument) = reference_ty.lifetime {
            match lifetime_argument {
                LifetimeArgument::Static => write!(file, "'static ")?,
                LifetimeArgument::Parameter(lt) => {
                    write!(file, "'{} ", self.lifetime_parameters[lt].name)?;
                }
            }
        }

        if let Some(reference_qualifier) = reference_ty.qualifier {
            match reference_qualifier {
                ReferenceQualifier::Restrict => write!(file, "restrict")?,
                ReferenceQualifier::Mutable => write!(file, "mutable")?,
            }
        }

        self.write_ty_type(file, &reference_ty.operand)?;

        Ok(())
    }

    fn write_ty_type(&self, file: &mut std::fs::File, ty: &ty::Type) -> Result<(), std::io::Error> {
        match ty {
            ty::Type::Struct(struct_ty) => self.write_ty_struct(file, struct_ty),
            ty::Type::PrimitiveType(primitive_type) => {
                Self::write_ty_primitive_type(file, *primitive_type)
            }
            ty::Type::ReferenceType(reference_ty) => {
                self.write_ty_reference_type(file, reference_ty)
            }
            ty::Type::TypeParameter(type_parameter_id) => {
                self.write_type_parameter(file, *type_parameter_id)
            }
            ty::Type::TraitType(trait_type_ty) => self.write_ty_trait_type(file, trait_type_ty),
        }
    }

    #[allow(clippy::too_many_lines)]
    fn write_where_caluse(
        &self,
        file: &mut std::fs::File,
        where_clause: &WhereClause,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        if where_clause
            .lifetime_argument_vecs_by_lifetime_parameter
            .is_empty()
            && where_clause.trait_type_bounds_by_trait_type.is_empty()
            && where_clause
                .lifetime_argument_vecs_by_type_parameter
                .is_empty()
        {
            return Ok(());
        }
        write!(file, " where: ")?;

        let mut is_first = true;

        {
            for (lifetime_parameter, lifetime_arguments) in
                &where_clause.lifetime_argument_vecs_by_lifetime_parameter
            {
                if !is_first {
                    write!(file, ", ")?;
                }

                write!(
                    file,
                    "'{}",
                    self.lifetime_parameters[*lifetime_parameter].name
                )?;

                write!(file, ": ")?;

                let mut is_first_lifetime_argument = true;
                for lifetime_argument in lifetime_arguments {
                    if !is_first_lifetime_argument {
                        write!(file, " + ")?;
                    }

                    match lifetime_argument {
                        LifetimeArgument::Parameter(lt) => {
                            write!(file, "'{}", self.lifetime_parameters[*lt].name)?;
                        }
                        LifetimeArgument::Static => {
                            write!(file, "'static")?;
                        }
                    }

                    is_first_lifetime_argument = false;
                }

                is_first = false;
            }
        }

        {
            for (trait_type, trait_type_bounds) in &where_clause.trait_type_bounds_by_trait_type {
                if !is_first {
                    write!(file, ", ")?;
                }

                self.write_ty_trait_type(file, trait_type)?;

                write!(file, ": ")?;

                match trait_type_bounds {
                    TraitTypeBound::Type(ty) => self.write_ty_type(file, ty)?,
                    TraitTypeBound::LifetimeArguments(lifetime_arguments) => {
                        let mut is_first_lifetime_argument = true;
                        for lifetime_argument in lifetime_arguments {
                            if !is_first_lifetime_argument {
                                write!(file, " + ")?;
                            }

                            match lifetime_argument {
                                LifetimeArgument::Parameter(lt) => {
                                    write!(file, "'{}", self.lifetime_parameters[*lt].name)?;
                                }
                                LifetimeArgument::Static => {
                                    write!(file, "'static")?;
                                }
                            }

                            is_first_lifetime_argument = false;
                        }
                    }
                }

                is_first = false;
            }
        }

        {
            for (lifetime_parameter, bounds) in
                &where_clause.lifetime_argument_vecs_by_lifetime_parameter
            {
                if !is_first {
                    write!(file, ", ")?;
                }

                write!(
                    file,
                    "'{}: ",
                    self.lifetime_parameters[*lifetime_parameter].name
                )?;

                let mut is_first_lifetime_bound = true;
                for bound in bounds {
                    if !is_first_lifetime_bound {
                        write!(file, " + ")?;
                    }

                    match bound {
                        LifetimeArgument::Parameter(lt) => {
                            write!(file, "'{}", self.lifetime_parameters[*lt].name)?;
                        }
                        LifetimeArgument::Static => {
                            write!(file, "'static")?;
                        }
                    }

                    is_first_lifetime_bound = false;
                }

                is_first = false;
            }
        }

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

        // write generics parameters
        self.write_generic_parameters(file, &struct_symbol.generics.parameters)?;

        write!(file, " {}", struct_symbol.name)?;

        self.write_where_caluse(file, &struct_symbol.generics.where_clause)?;

        write!(file, " {{")?;

        for field in struct_symbol
            .field_order
            .iter()
            .copied()
            .map(|x| &self.fields[x])
        {
            write!(file, "{} {}: ", field.accessibility, field.name)?;
            self.write_ty_type(file, &field.ty)?;
            writeln!(file, ";")?;
        }

        writeln!(file, "}}")?;

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

impl<'a> Deref for SymbolRef<'a, Enum> {
    type Target = Enum;

    fn deref(&self) -> &Self::Target { &self.table.enums[self.symbol_id] }
}

impl<'a> Deref for SymbolRef<'a, EnumVariant> {
    type Target = EnumVariant;

    fn deref(&self) -> &Self::Target { &self.table.enum_variants[self.symbol_id] }
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

impl<'a> Input for SymbolRef<'a, EnumVariant> {
    type Output = SymbolRef<'a, EnumVariant>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(&self.name, &output.name);
        prop_assert_eq!(
            self.table.get_qualified_name(self.parent_enum_id.into()),
            output
                .table
                .get_qualified_name(output.parent_enum_id.into())
        );

        prop_assert_eq!(self.declaration_order, output.declaration_order);

        Ok(())
    }
}

impl<'a> Input for SymbolRef<'a, Enum> {
    type Output = SymbolRef<'a, Enum>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(&self.name, &output.name);
        prop_assert_eq!(&self.accessibility, &output.accessibility);
        prop_assert_eq!(
            self.table.get_qualified_name(self.parent_module_id.into()),
            output
                .table
                .get_qualified_name(output.parent_module_id.into())
        );

        prop_assert_eq!(
            self.variant_order
                .iter()
                .copied()
                .map(|x| self.table.get_qualified_name(x.into()))
                .collect::<Vec<_>>(),
            output
                .variant_order
                .iter()
                .copied()
                .map(|x| output.table.get_qualified_name(x.into()))
                .collect::<Vec<_>>()
        );

        prop_assert_eq!(
            self.variant_ids_by_name.keys().collect::<HashSet<_>>(),
            output.variant_ids_by_name.keys().collect::<HashSet<_>>()
        );

        for (input_variant_id, output_variant_id) in self
            .variant_order
            .iter()
            .copied()
            .zip(output.variant_order.iter().copied())
        {
            SymbolRef {
                table: self.table,
                symbol_id: input_variant_id,
            }
            .assert(&SymbolRef {
                table: output.table,
                symbol_id: output_variant_id,
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
                (ModuleChildID::Enum(input_enum_id), ModuleChildID::Enum(output_enum_id)) => {
                    SymbolRef {
                        table: self.table,
                        symbol_id: input_enum_id,
                    }
                    .assert(&SymbolRef {
                        table: output.table,
                        symbol_id: output_enum_id,
                    })?;
                }

                (ModuleChildID::Struct(_), ModuleChildID::Struct(_))
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
