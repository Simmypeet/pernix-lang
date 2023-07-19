use std::{collections::HashSet, ops::Deref, path::Path};

use pernixc_system::{arena, input::Input};
use proptest::{
    prop_assert_eq,
    test_runner::{TestCaseError, TestCaseResult},
};

use super::{Access, Table};
use crate::{
    ty::{self, ReferenceQualifier},
    Enum, EnumVariant, Field, Function, GenericParameters, GenericableID, GlobalID,
    LifetimeArgument, LifetimeParameter, Module, ModuleChildID, Parameter, Struct, Substitution,
    Trait, TraitFunction, TraitMemberID, TraitType, TypeParameter, WhereClause,
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
        for module_child_id in module
            .module_child_ids_by_name
            .values()
            .copied()
            .filter_map(|x| x.into_module().ok())
        {
            let submodule_name = self.modules[module_child_id].name.clone();
            let submodule_accessibility = self.modules[module_child_id].accessibility;

            if module.parent_module_id.is_none() {
                self.create_file(module_child_id, parent_directory)?;
            } else {
                self.create_file(module_child_id, &file_path.with_extension(""))?;
            }

            writeln!(file, "{submodule_accessibility} module {submodule_name};",)?;
        }

        for module_child_id in module
            .module_child_ids_by_name
            .values()
            .copied()
            .filter(|x| x.as_module().is_none())
        {
            match module_child_id {
                ModuleChildID::Module(..) => {
                    unreachable!()
                }
                ModuleChildID::Struct(struct_id) => self.write_struct(&mut file, struct_id)?,
                ModuleChildID::Enum(enum_id) => self.write_enum(&mut file, enum_id)?,
                ModuleChildID::Function(function_id) => {
                    self.write_function(&mut file, function_id)?;
                }
                ModuleChildID::Type(type_id) => self.write_type(&mut file, type_id)?,
                ModuleChildID::Trait(trait_id) => self.write_trait(&mut file, trait_id)?,
            }
        }

        Ok(())
    }

    fn write_trait(
        &self,
        file: &mut std::fs::File,
        trait_id: arena::ID<Trait>,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        let trait_symbol = &self.traits[trait_id];
        write!(
            file,
            "{} trait {}",
            trait_symbol.accessibility, trait_symbol.name
        )?;

        self.write_generic_parameters(file, &trait_symbol.generics.parameters)?;
        self.write_where_clause(file, &trait_symbol.generics.where_clause)?;

        write!(file, " {{")?;

        for trait_member in trait_symbol.trait_member_ids_by_name.values() {
            match trait_member {
                TraitMemberID::TraitFunction(trait_function_id) => {
                    self.write_trait_function(file, *trait_function_id)?;
                }
                TraitMemberID::TraitType(trait_type_id) => {
                    self.write_trait_type(file, *trait_type_id)?;
                }
            }
        }

        writeln!(file, " }}")
    }

    fn write_trait_type(
        &self,
        file: &mut std::fs::File,
        trait_type_id: arena::ID<TraitType>,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        let trait_type_symbol = &self.trait_types[trait_type_id];
        write!(file, "type {}", trait_type_symbol.name)?;
        self.write_generic_parameters(file, &trait_type_symbol.generic_parameters)?;
        writeln!(file, ";")
    }

    fn write_trait_function(
        &self,
        file: &mut std::fs::File,
        trait_function_id: arena::ID<crate::TraitFunction>,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        let trait_function_symbol = &self.trait_functions[trait_function_id];
        write!(file, "{}", trait_function_symbol.name)?;
        self.write_generic_parameters(file, &trait_function_symbol.generics.parameters)?;

        write!(file, "(")?;
        {
            let mut is_first = true;
            for parameter in trait_function_symbol
                .parameter_order
                .iter()
                .copied()
                .map(|x| &self.trait_function_parameters[x])
            {
                if !is_first {
                    write!(file, ", ")?;
                }

                if parameter.is_mutable {
                    write!(file, "mutable ")?;
                }

                write!(file, "{}: ", parameter.name)?;
                self.write_ty_type(file, &parameter.ty)?;

                is_first = false;
            }
        }
        write!(file, "): ")?;

        self.write_ty_type(file, &trait_function_symbol.return_type)?;
        self.write_where_clause(file, &trait_function_symbol.generics.where_clause)?;

        writeln!(file, ";")
    }

    fn write_type(
        &self,
        file: &mut std::fs::File,
        type_id: arena::ID<crate::Type>,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        let type_symbol = &self.types[type_id];
        write!(
            file,
            "{} type {}",
            type_symbol.accessibility, type_symbol.name
        )?;
        self.write_generic_parameters(file, &type_symbol.generic_parameters)?;
        write!(file, " = ")?;
        self.write_ty_type(file, &type_symbol.alias)?;
        writeln!(file, ";")
    }

    fn write_function(
        &self,
        file: &mut std::fs::File,
        function_id: arena::ID<Function>,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        let function_symbol = &self.functions[function_id];
        write!(
            file,
            "{} {}",
            function_symbol.accessibility, function_symbol.name
        )?;

        self.write_generic_parameters(file, &function_symbol.generics.parameters)?;

        write!(file, "(")?;
        {
            let mut is_first = true;
            for parameter in function_symbol
                .parameter_order
                .iter()
                .copied()
                .map(|x| &self.function_parameters[x])
            {
                if !is_first {
                    write!(file, ", ")?;
                }

                if parameter.is_mutable {
                    write!(file, "mutable ")?;
                }

                write!(file, "{}: ", parameter.name)?;
                self.write_ty_type(file, &parameter.ty)?;

                is_first = false;
            }
        }
        write!(file, "): ")?;

        self.write_ty_type(file, &function_symbol.return_type)?;
        self.write_where_clause(file, &function_symbol.generics.where_clause)?;

        writeln!(file, "{{}}")?;

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
            "{} enum {} {{",
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

        writeln!(file, "}}")
    }

    fn write_generic_parameters(
        &self,
        file: &mut std::fs::File,
        generic_parameters: &GenericParameters,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        if generic_parameters.lifetime_parameter_order.is_empty()
            && generic_parameters.type_parameter_order.is_empty()
        {
            return Ok(());
        }

        let generic_parameters = generic_parameters
            .lifetime_parameter_order
            .iter()
            .copied()
            .map(|x| format!("'{}", self.lifetime_parameters[x].name.clone()))
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

        Ok(())
    }

    fn write_ty_primitive_type(
        file: &mut std::fs::File,
        primitive_type: ty::Primitive,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        match primitive_type {
            ty::Primitive::Float32 => write!(file, "float32"),
            ty::Primitive::Float64 => write!(file, "float64"),
            ty::Primitive::Int8 => write!(file, "int8"),
            ty::Primitive::Int16 => write!(file, "int16"),
            ty::Primitive::Int32 => write!(file, "int32"),
            ty::Primitive::Int64 => write!(file, "int64"),
            ty::Primitive::Uint8 => write!(file, "uint8"),
            ty::Primitive::Uint16 => write!(file, "uint16"),
            ty::Primitive::Uint32 => write!(file, "uint32"),
            ty::Primitive::Uint64 => write!(file, "uint64"),
            ty::Primitive::Bool => write!(file, "bool"),
            ty::Primitive::Void => write!(file, "void"),
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
        use std::io::Write;

        self.write_ty_global_id(
            file,
            self.trait_types[trait_type_ty.trait_type_id]
                .parent_trait_id
                .into(),
            &trait_type_ty.trait_substitution,
        )?;

        let trait_type = &self.trait_types[trait_type_ty.trait_type_id];

        write!(file, "::{}", trait_type.name)?;

        if !trait_type
            .generic_parameters
            .lifetime_parameter_order
            .is_empty()
            || !trait_type
                .generic_parameters
                .type_parameter_order
                .is_empty()
        {
            write!(file, "<")?;

            let mut is_first = true;

            for lifetime_parameter_id in &trait_type.generic_parameters.lifetime_parameter_order {
                if !is_first {
                    write!(file, ", ")?;
                }

                match trait_type_ty
                    .trait_type_substitution
                    .lifetime_arguments_by_parameter[lifetime_parameter_id]
                {
                    LifetimeArgument::Static => write!(file, "'static")?,
                    LifetimeArgument::Parameter(lt) => {
                        write!(file, "'{}", self.lifetime_parameters[lt].name)?;
                    }
                }

                is_first = false;
            }

            for type_parameter_id in &trait_type.generic_parameters.type_parameter_order {
                if !is_first {
                    write!(file, ", ")?;
                }

                self.write_ty_type(
                    file,
                    &trait_type_ty
                        .trait_type_substitution
                        .type_arguments_by_parameter[type_parameter_id],
                )?;

                is_first = false;
            }

            write!(file, ">")?;
        }

        Ok(())
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
        reference_ty: &ty::Reference,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        write!(file, "&")?;

        if let Some(lifetime_argument) = reference_ty.lifetime_argument {
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
            ty::Type::Primitive(primitive_type) => {
                Self::write_ty_primitive_type(file, *primitive_type)
            }
            ty::Type::Reference(reference_ty) => self.write_ty_reference_type(file, reference_ty),
            ty::Type::Parameter(type_parameter_id) => {
                self.write_type_parameter(file, *type_parameter_id)
            }
            ty::Type::TraitType(trait_type_ty) => self.write_ty_trait_type(file, trait_type_ty),
            ty::Type::Enum(enum_id) => {
                self.write_ty_global_id(file, (*enum_id).into(), &Substitution::default())
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn write_where_clause(
        &self,
        file: &mut std::fs::File,
        where_clause: &WhereClause,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        if where_clause
            .lifetime_argument_sets_by_lifetime_parameter
            .is_empty()
            && where_clause.types_by_trait_type.is_empty()
            && where_clause.lifetime_argument_sets_by_type.is_empty()
        {
            return Ok(());
        }
        write!(file, " where: ")?;

        let mut is_first = true;

        {
            for trait_bound in &where_clause.trait_bounds {
                if !is_first {
                    write!(file, ", ")?;
                }

                self.write_ty_global_id(
                    file,
                    trait_bound.trait_id.into(),
                    &trait_bound.substitution,
                )?;

                is_first = false;
            }
        }

        {
            for (lifetime_parameter, lifetime_arguments) in
                &where_clause.lifetime_argument_sets_by_lifetime_parameter
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
            for (ty, lifetime_arguments) in &where_clause.lifetime_argument_sets_by_type {
                if !is_first {
                    write!(file, ", ")?;
                }

                self.write_ty_type(file, ty)?;

                write!(file, ": ")?;

                let mut first_trait_type_bound = true;
                for lifetime_argument in lifetime_arguments {
                    if !first_trait_type_bound {
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

                    first_trait_type_bound = false;
                }

                is_first = false;
            }
        }

        {
            for (trait_type, ty) in &where_clause.types_by_trait_type {
                if !is_first {
                    write!(file, ", ")?;
                }

                self.write_ty_trait_type(file, trait_type)?;

                write!(file, ": ")?;

                self.write_ty_type(file, ty)?;

                is_first = false;
            }
        }

        {
            for (lifetime_parameter, bounds) in
                &where_clause.lifetime_argument_sets_by_lifetime_parameter
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

        write!(
            file,
            "{} struct {}",
            struct_symbol.accessibility, struct_symbol.name
        )?;

        self.write_generic_parameters(file, &struct_symbol.generics.parameters)?;
        self.write_where_clause(file, &struct_symbol.generics.where_clause)?;

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

#[derive(Debug, Clone)]
struct Type<'a> {
    ty: &'a ty::Type,
    table: &'a Table,
}

#[allow(clippy::unnecessary_wraps)]
fn assert_substitution(
    _input_table: &Table,
    _output_table: &Table,
    _input_substitution: &Substitution,
    _output_substitution: &Substitution,
) -> TestCaseResult {
    Ok(())
}

#[allow(clippy::unnecessary_wraps)]
fn assert_where_clause(
    _input_table: &Table,
    _output_table: &Table,
    _input_where_clause: &WhereClause,
    _output_where_clause: &WhereClause,
) -> TestCaseResult {
    Ok(())
}

fn assert_generic_parameters(
    input_table: &Table,
    output_table: &Table,
    input_generic_parameters: &GenericParameters,
    output_generic_parameters: &GenericParameters,
) -> TestCaseResult {
    prop_assert_eq!(
        input_generic_parameters.lifetime_parameter_order.len(),
        output_generic_parameters.lifetime_parameter_order.len()
    );

    for (input_lifetime_parameter_id, output_lifetime_parameter_id) in input_generic_parameters
        .lifetime_parameter_order
        .iter()
        .copied()
        .zip(
            output_generic_parameters
                .lifetime_parameter_order
                .iter()
                .copied(),
        )
    {
        SymbolRef {
            table: input_table,
            symbol_id: input_lifetime_parameter_id,
        }
        .assert(&SymbolRef {
            table: output_table,
            symbol_id: output_lifetime_parameter_id,
        })?;
    }

    prop_assert_eq!(
        input_generic_parameters
            .lifetime_parameter_ids_by_name
            .keys()
            .collect::<HashSet<_>>(),
        output_generic_parameters
            .lifetime_parameter_ids_by_name
            .keys()
            .collect::<HashSet<_>>()
    );

    prop_assert_eq!(
        input_generic_parameters.type_parameter_order.len(),
        output_generic_parameters.type_parameter_order.len()
    );

    for (input_type_parameter_id, output_type_parameter_id) in input_generic_parameters
        .type_parameter_order
        .iter()
        .copied()
        .zip(
            output_generic_parameters
                .type_parameter_order
                .iter()
                .copied(),
        )
    {
        SymbolRef {
            table: input_table,
            symbol_id: input_type_parameter_id,
        }
        .assert(&SymbolRef {
            table: output_table,
            symbol_id: output_type_parameter_id,
        })?;
    }

    prop_assert_eq!(
        input_generic_parameters
            .type_parameter_ids_by_name
            .keys()
            .collect::<HashSet<_>>(),
        output_generic_parameters
            .type_parameter_ids_by_name
            .keys()
            .collect::<HashSet<_>>()
    );

    Ok(())
}

fn assert_genericable_id(
    input_table: &Table,
    output_table: &Table,
    input_id: GenericableID,
    output_id: GenericableID,
) -> TestCaseResult {
    match (input_id, output_id) {
        (GenericableID::Struct(input), GenericableID::Struct(output)) => {
            prop_assert_eq!(
                input_table.get_qualified_name(input.into()),
                output_table.get_qualified_name(output.into())
            );

            Ok(())
        }
        (GenericableID::Trait(input), GenericableID::Trait(output)) => {
            prop_assert_eq!(
                input_table.get_qualified_name(input.into()),
                output_table.get_qualified_name(output.into())
            );

            Ok(())
        }
        (GenericableID::TraitType(input), GenericableID::TraitType(output)) => {
            prop_assert_eq!(
                input_table.get_qualified_name(input.into()),
                output_table.get_qualified_name(output.into())
            );

            Ok(())
        }
        (GenericableID::TraitFunction(input), GenericableID::TraitFunction(output)) => {
            prop_assert_eq!(
                input_table.get_qualified_name(input.into()),
                output_table.get_qualified_name(output.into())
            );

            Ok(())
        }

        // Adds proper assertion for the implements
        (GenericableID::Implements(..), GenericableID::Implements(..))
        | (GenericableID::ImplementsType(..), GenericableID::ImplementsType(..))
        | (GenericableID::ImplementsFunction(..), GenericableID::ImplementsFunction(..)) => Ok(()),

        (GenericableID::Function(input), GenericableID::Function(output)) => {
            prop_assert_eq!(
                input_table.get_qualified_name(input.into()),
                output_table.get_qualified_name(output.into())
            );

            Ok(())
        }
        (GenericableID::Type(input), GenericableID::Type(output)) => {
            prop_assert_eq!(
                input_table.get_qualified_name(input.into()),
                output_table.get_qualified_name(output.into())
            );

            Ok(())
        }

        (input, output) => Err(TestCaseError::fail(format!(
            "expected {input:?}, got {output:?}"
        ))),
    }
}

impl<'a> Input for SymbolRef<'a, LifetimeParameter> {
    type Output = SymbolRef<'a, LifetimeParameter>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(&self.name, &output.name);
        let input_table = self.table;
        let output_table = output.table;

        assert_genericable_id(
            input_table,
            output_table,
            self.parent_genericable_id,
            output.parent_genericable_id,
        )
    }
}

impl<'a> Input for Type<'a> {
    type Output = Type<'a>;

    #[allow(clippy::too_many_lines)]
    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self.ty, output.ty) {
            (ty::Type::TraitType(input_trait), ty::Type::TraitType(output_trait)) => {
                prop_assert_eq!(
                    self.table
                        .get_qualified_name(input_trait.trait_type_id.into()),
                    output
                        .table
                        .get_qualified_name(output_trait.trait_type_id.into())
                );

                assert_substitution(
                    self.table,
                    output.table,
                    &input_trait.trait_substitution,
                    &output_trait.trait_substitution,
                )?;

                assert_substitution(
                    self.table,
                    output.table,
                    &input_trait.trait_type_substitution,
                    &output_trait.trait_type_substitution,
                )?;

                Ok(())
            }
            (ty::Type::Struct(input_struct), ty::Type::Struct(output_struct)) => {
                prop_assert_eq!(
                    self.table.get_qualified_name(input_struct.struct_id.into()),
                    output
                        .table
                        .get_qualified_name(output_struct.struct_id.into())
                );

                assert_substitution(
                    self.table,
                    output.table,
                    &input_struct.substitution,
                    &output_struct.substitution,
                )?;

                Ok(())
            }
            (ty::Type::Primitive(input), ty::Type::Primitive(output)) => {
                prop_assert_eq!(input, output);
                Ok(())
            }
            (
                ty::Type::Reference(input_reference_type),
                ty::Type::Reference(output_reference_type),
            ) => {
                prop_assert_eq!(
                    &input_reference_type.qualifier,
                    &output_reference_type.qualifier
                );
                match (
                    &input_reference_type.qualifier,
                    &output_reference_type.qualifier,
                ) {
                    (None, None) => (),
                    (Some(input_reference_qualifier), Some(output_reference_qualifier)) => {
                        prop_assert_eq!(input_reference_qualifier, output_reference_qualifier);
                    }
                    (input, output) => {
                        return Err(TestCaseError::fail(format!(
                            "expected {input:#?}, got {output:#?}",
                        )))
                    }
                }

                match (
                    &input_reference_type.lifetime_argument,
                    &output_reference_type.lifetime_argument,
                ) {
                    (Some(input_lifetime_argument), Some(output_lifetime_argument)) => {
                        match (input_lifetime_argument, output_lifetime_argument) {
                            (LifetimeArgument::Static, LifetimeArgument::Static) => {}
                            (
                                LifetimeArgument::Parameter(input_lifetime_parameter),
                                LifetimeArgument::Parameter(output_lifetime_parameter),
                            ) => SymbolRef {
                                table: self.table,
                                symbol_id: *input_lifetime_parameter,
                            }
                            .assert(&SymbolRef {
                                table: output.table,
                                symbol_id: *output_lifetime_parameter,
                            })?,
                            (input, output) => {
                                return Err(TestCaseError::fail(format!(
                                    "expected {input:#?}, got {output:#?}",
                                )))
                            }
                        }
                    }
                    (None, None) => (),
                    (input, output) => {
                        return Err(TestCaseError::fail(format!(
                            "expected {input:#?}, got {output:#?}",
                        )))
                    }
                }

                Ok(())
            }
            (
                ty::Type::Parameter(input_type_parameter),
                ty::Type::Parameter(output_type_parameter),
            ) => SymbolRef {
                table: self.table,
                symbol_id: *input_type_parameter,
            }
            .assert(&SymbolRef {
                table: output.table,
                symbol_id: *output_type_parameter,
            }),
            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:#?}, got {output:#?}"
            ))),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct SymbolRef<'a, T> {
    table: &'a Table,
    symbol_id: arena::ID<T>,
}

impl<'a, T> Deref for SymbolRef<'a, T>
where
    Table: Access<T>,
{
    type Target = arena::Symbol<T>;

    fn deref(&self) -> &Self::Target { self.table.get(self.symbol_id).unwrap() }
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

impl<'a> Input for SymbolRef<'a, crate::Type> {
    type Output = SymbolRef<'a, crate::Type>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(&self.name, &output.name);
        prop_assert_eq!(
            self.table.get_qualified_name(self.parent_module_id.into()),
            output
                .table
                .get_qualified_name(output.parent_module_id.into())
        );
        assert_generic_parameters(
            self.table,
            output.table,
            &self.generic_parameters,
            &output.generic_parameters,
        )?;
        Type {
            table: self.table,
            ty: &self.alias,
        }
        .assert(&Type {
            table: output.table,
            ty: &output.alias,
        })?;

        Ok(())
    }
}

impl<'a> Input for SymbolRef<'a, Trait> {
    type Output = SymbolRef<'a, Trait>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(&self.name, &output.name);
        prop_assert_eq!(self.accessibility, output.accessibility);
        prop_assert_eq!(
            self.table.get_qualified_name(self.parent_module_id.into()),
            output
                .table
                .get_qualified_name(output.parent_module_id.into())
        );

        assert_generic_parameters(
            self.table,
            output.table,
            &self.generics.parameters,
            &output.generics.parameters,
        )?;

        prop_assert_eq!(
            self.trait_member_ids_by_name.keys().collect::<HashSet<_>>(),
            output
                .trait_member_ids_by_name
                .keys()
                .collect::<HashSet<_>>()
        );

        for member_name in self.trait_member_ids_by_name.keys() {
            let input_member_id = self.trait_member_ids_by_name[member_name];
            let output_member_id = output.trait_member_ids_by_name[member_name];

            match (input_member_id, output_member_id) {
                (
                    crate::TraitMemberID::TraitFunction(input_trait_function_id),
                    crate::TraitMemberID::TraitFunction(output_trait_function_id),
                ) => SymbolRef {
                    table: self.table,
                    symbol_id: input_trait_function_id,
                }
                .assert(&SymbolRef {
                    table: output.table,
                    symbol_id: output_trait_function_id,
                })?,
                (
                    crate::TraitMemberID::TraitType(input_trait_type_id),
                    crate::TraitMemberID::TraitType(output_trait_type_id),
                ) => SymbolRef {
                    table: self.table,
                    symbol_id: input_trait_type_id,
                }
                .assert(&SymbolRef {
                    table: output.table,
                    symbol_id: output_trait_type_id,
                })?,
                (input, output) => {
                    return Err(TestCaseError::fail(format!(
                        "expected {input:#?}, got {output:#?}"
                    )));
                }
            }
        }

        Ok(())
    }
}

impl<'a> Input for SymbolRef<'a, TraitFunction> {
    type Output = SymbolRef<'a, TraitFunction>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(&self.name, &output.name);
        prop_assert_eq!(
            self.table.get_qualified_name(self.parent_trait_id.into()),
            output
                .table
                .get_qualified_name(output.parent_trait_id.into())
        );

        assert_generic_parameters(
            self.table,
            output.table,
            &self.generics.parameters,
            &output.generics.parameters,
        )?;
        assert_where_clause(
            self.table,
            output.table,
            &self.generics.where_clause,
            &output.generics.where_clause,
        )?;

        prop_assert_eq!(self.parameter_order.len(), output.parameter_order.len());

        for (input_parameter_id, output_parameter_id) in self
            .parameter_order
            .iter()
            .zip(output.parameter_order.iter())
        {
            SymbolRef {
                table: self.table,
                symbol_id: *input_parameter_id,
            }
            .assert(&SymbolRef {
                table: output.table,
                symbol_id: *output_parameter_id,
            })?;
        }

        Ok(())
    }
}

impl<'a> Input for SymbolRef<'a, TraitType> {
    type Output = SymbolRef<'a, TraitType>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(&self.name, &output.name);
        prop_assert_eq!(
            self.table.get_qualified_name(self.parent_trait_id.into()),
            output
                .table
                .get_qualified_name(output.parent_trait_id.into())
        );
        assert_generic_parameters(
            self.table,
            output.table,
            &self.generic_parameters,
            &output.generic_parameters,
        )?;

        Ok(())
    }
}

impl<'a, T> Input for SymbolRef<'a, Parameter<T>>
where
    Table: Access<Parameter<T>>,
{
    type Output = SymbolRef<'a, Parameter<T>>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(&self.name, &output.name);
        prop_assert_eq!(self.is_mutable, output.is_mutable);
        Type {
            ty: &self.ty,
            table: self.table,
        }
        .assert(&Type {
            ty: &output.ty,
            table: output.table,
        })
    }
}

impl<'a> Input for SymbolRef<'a, TypeParameter> {
    type Output = SymbolRef<'a, TypeParameter>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(&self.name, &output.name);

        assert_genericable_id(
            self.table,
            output.table,
            self.parent_genericable_id,
            output.parent_genericable_id,
        )
    }
}

impl<'a> Input for SymbolRef<'a, Function> {
    type Output = SymbolRef<'a, Function>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(&self.name, &output.name);
        prop_assert_eq!(&self.accessibility, &output.accessibility);
        prop_assert_eq!(
            self.table.get_qualified_name(self.parent_module_id.into()),
            output
                .table
                .get_qualified_name(output.parent_module_id.into())
        );

        assert_generic_parameters(
            self.table,
            output.table,
            &self.generics.parameters,
            &output.generics.parameters,
        )?;
        assert_where_clause(
            self.table,
            output.table,
            &self.generics.where_clause,
            &output.generics.where_clause,
        )?;

        prop_assert_eq!(self.parameter_order.len(), output.parameter_order.len());

        for (input_parameter_id, output_parameter_id) in self
            .parameter_order
            .iter()
            .zip(output.parameter_order.iter())
        {
            SymbolRef {
                table: self.table,
                symbol_id: *input_parameter_id,
            }
            .assert(&SymbolRef {
                table: output.table,
                symbol_id: *output_parameter_id,
            })?;
        }

        Ok(())
    }
}

impl<'a> Input for SymbolRef<'a, Field> {
    type Output = SymbolRef<'a, Field>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(&self.name, &output.name);
        prop_assert_eq!(self.accessibility, output.accessibility);
        prop_assert_eq!(
            self.table.get_qualified_name(self.parent_struct_id.into()),
            output
                .table
                .get_qualified_name(output.parent_struct_id.into())
        );

        Type {
            ty: &self.ty,
            table: self.table,
        }
        .assert(&Type {
            ty: &output.ty,
            table: output.table,
        })?;

        Ok(())
    }
}

impl<'a> Input for SymbolRef<'a, Struct> {
    type Output = SymbolRef<'a, Struct>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(&self.name, &output.name);
        prop_assert_eq!(&self.accessibility, &output.accessibility);
        prop_assert_eq!(
            self.field_order
                .iter()
                .copied()
                .map(|x| &self.table.fields[x].name)
                .collect::<Vec<_>>(),
            output
                .field_order
                .iter()
                .copied()
                .map(|x| &output.table.fields[x].name)
                .collect::<Vec<_>>()
        );

        prop_assert_eq!(
            self.field_ids_by_name.keys().collect::<HashSet<_>>(),
            output.field_ids_by_name.keys().collect::<HashSet<_>>()
        );

        assert_generic_parameters(
            self.table,
            output.table,
            &self.generics.parameters,
            &output.generics.parameters,
        )?;
        assert_where_clause(
            self.table,
            output.table,
            &self.generics.where_clause,
            &output.generics.where_clause,
        )?;

        for (input_field_id, output_field_id) in self
            .field_order
            .iter()
            .copied()
            .zip(output.field_order.iter().copied())
        {
            SymbolRef {
                table: self.table,
                symbol_id: input_field_id,
            }
            .assert(&SymbolRef {
                table: output.table,
                symbol_id: output_field_id,
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

    #[allow(clippy::too_many_lines)]
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

                (
                    ModuleChildID::Struct(input_struct_id),
                    ModuleChildID::Struct(output_struct_id),
                ) => {
                    SymbolRef {
                        table: self.table,
                        symbol_id: input_struct_id,
                    }
                    .assert(&SymbolRef {
                        table: output.table,
                        symbol_id: output_struct_id,
                    })?;
                }
                (
                    ModuleChildID::Function(input_function_id),
                    ModuleChildID::Function(output_function_id),
                ) => SymbolRef {
                    table: self.table,
                    symbol_id: input_function_id,
                }
                .assert(&SymbolRef {
                    table: output.table,
                    symbol_id: output_function_id,
                })?,
                (ModuleChildID::Trait(input_trait_id), ModuleChildID::Trait(output_trait_id)) => {
                    SymbolRef {
                        table: self.table,
                        symbol_id: input_trait_id,
                    }
                    .assert(&SymbolRef {
                        table: output.table,
                        symbol_id: output_trait_id,
                    })?;
                }
                (ModuleChildID::Type(input_type_id), ModuleChildID::Type(output_type_id)) => {
                    SymbolRef {
                        table: self.table,
                        symbol_id: input_type_id,
                    }
                    .assert(&SymbolRef {
                        table: output.table,
                        symbol_id: output_type_id,
                    })?;
                }
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
