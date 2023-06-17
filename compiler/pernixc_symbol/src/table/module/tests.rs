use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    path::Path,
};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_source::SourceFile;
use pernixc_syntax::syntax_tree::target::Target;
use pernixc_system::{
    arena::{self, Arena},
    diagnostic::Storage,
};
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq,
    strategy::{Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::{error::Error, table, Accessibility};

#[derive(Debug, Clone, PartialEq, Eq)]
struct Module {
    name: String,
    accessibility: Accessibility,
    submodule_ids_by_name: HashMap<String, arena::ID<Module>>,
    parent_module_id: Option<arena::ID<Module>>,
    usings: HashSet<arena::ID<Module>>,
}

#[derive(Debug, thiserror::Error, From)]
#[allow(missing_docs)]
enum WorkspaceCreateError {
    #[error("{0}")]
    Io(std::io::Error),

    #[error("{0}")]
    Fmt(std::fmt::Error),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Table {
    modules: Arena<Module>,
    target_root_module_ids_by_name: HashMap<String, arena::ID<Module>>,
}

impl Table {
    fn new() -> Self {
        Self {
            modules: Arena::new(),
            target_root_module_ids_by_name: HashMap::new(),
        }
    }

    fn new_module(
        &mut self,
        module_tree: ModuleTree,
        name: String,
        accessibility: Accessibility,
        parent_module_id: Option<arena::ID<Module>>,
    ) -> arena::ID<Module> {
        let module_id = self.modules.push(Module {
            name,
            accessibility,
            submodule_ids_by_name: HashMap::new(), // to be filled later
            parent_module_id,
            usings: HashSet::new(), // to be filled later
        });

        for (name, submodule) in module_tree.submodules_by_name {
            let submodule_id = self.new_module(
                submodule.tree,
                name.clone(),
                submodule.accessibility,
                Some(module_id),
            );
            self.modules[module_id]
                .submodule_ids_by_name
                .insert(name, submodule_id);
        }

        module_id
    }

    fn get_full_module_path(&self, mut module_id: arena::ID<Module>) -> String {
        let mut name = self.modules[module_id].name.clone();

        while let Some(parent_module_id) = self.modules[module_id].parent_module_id {
            name.insert_str(0, "::");
            name.insert_str(0, &self.modules[parent_module_id].name);
            module_id = parent_module_id;
        }

        name
    }

    fn new_target(&mut self, module_tree: ModuleTree, name: String) {
        let root_target_module_id =
            self.new_module(module_tree, name.clone(), Accessibility::Public, None);

        assert!(self
            .target_root_module_ids_by_name
            .insert(name, root_target_module_id)
            .is_none());
    }

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
            writeln!(file, "using {};", self.get_full_module_path(using_module))?;
        }

        if !self.modules.is_empty() && module.parent_module_id.is_some() {
            // create sub directory
            std::fs::create_dir(file_path.with_extension(""))?;
        }

        // write submodules
        for module_id in module.submodule_ids_by_name.values().copied() {
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

    fn create_workspace(&self) -> Result<tempfile::TempDir, WorkspaceCreateError> {
        let tempdir = tempfile::tempdir()?;

        for (name, module_id) in &self.target_root_module_ids_by_name {
            self.create_target(*module_id, name, tempdir.path())?;
        }

        Ok(tempdir)
    }

    fn validate(
        &self,
        module_id: arena::ID<Module>,
        output_table: &table::Table,
        output_module_id: arena::ID<crate::Module>,
    ) -> TestCaseResult {
        let module_input = &self.modules[module_id];
        let module_output = &output_table.modules[output_module_id];

        prop_assert_eq!(&module_input.name, &module_output.name);
        prop_assert_eq!(module_input.accessibility, module_output.accessibility);
        prop_assert_eq!(module_input.usings.len(), module_output.usings.len());
        prop_assert_eq!(
            module_input.submodule_ids_by_name.len(),
            module_output.child_ids_by_name.len()
        );

        for (name, input_submodule_id) in &module_input.submodule_ids_by_name {
            let output_submodule_id = module_output
                .child_ids_by_name
                .get(name)
                .unwrap()
                .into_module()
                .unwrap();

            self.validate(*input_submodule_id, output_table, output_submodule_id)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Submodule {
    accessibility: Accessibility,
    tree: ModuleTree,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ModuleTree {
    submodules_by_name: HashMap<String, Submodule>,
}

fn name() -> impl Strategy<Value = String> {
    "[a-zA-Z_][a-zA-Z0-9_]*".prop_filter("filter out illegal names", |name| {
        !matches!(name.as_str(), "@core" | "main")
    })
}

fn table() -> impl Strategy<Value = Table> {
    let leaf = Just(ModuleTree {
        submodules_by_name: HashMap::new(),
    });

    let module = leaf.prop_recursive(4, 16, 4, |inner| {
        proptest::collection::hash_map(
            name(),
            (Accessibility::arbitrary(), inner).prop_map(|(accessibility, tree)| Submodule {
                accessibility,
                tree,
            }),
            0..=4,
        )
        .prop_map(|submodules_by_name| ModuleTree { submodules_by_name })
    });

    let targets = proptest::collection::hash_map(name(), module, 1..=4);
    let table = targets.prop_map(|targets| {
        let mut table = Table::new();
        for (name, module_tree) in targets {
            table.new_target(module_tree, name);
        }
        table
    });

    let table_with_usings = table.prop_flat_map(|table| {
        let module_ids: Cow<[arena::ID<Module>]> = table.modules.ids().collect();

        (
            proptest::collection::hash_map(
                proptest::sample::select(module_ids.clone()),
                proptest::collection::hash_set(proptest::sample::select(module_ids), 0..=4),
                0..=table.modules.len(),
            )
            .prop_map(|mut x| {
                for (module_id, module_usings) in &mut x {
                    module_usings.remove(module_id);
                }

                x
            }),
            Just(table),
        )
    });

    table_with_usings.prop_map(|(usings_by_module_id, mut table)| {
        for (module_id, usings) in usings_by_module_id {
            table.modules[module_id].usings = usings;
        }
        table
    })
}

/// Enumeration of all error types that can be returned when parsing a target tree.
#[derive(Debug, EnumAsInner, From)]
enum TargetParseError {
    Lexical(pernixc_lexical::error::Error),
    Syntax(pernixc_syntax::error::Error),
    Target(pernixc_syntax::syntax_tree::target::Error),
}

proptest::proptest! {
    #[test]
    fn table_module_test(input_table in table()) {
        let tempdir = input_table.create_workspace()?;

        let mut targets = Vec::new();
        let storage: Storage<TargetParseError> = Storage::new();
        for target_name in input_table.target_root_module_ids_by_name.keys() {
            let target_root_module_name = tempdir.path().join(target_name).join("main.pnx");
            let source_file = SourceFile::load(&target_root_module_name)?;

            targets.push(Target::parse(source_file, target_name.clone(), &storage));
        }

        if !storage.as_vec().is_empty() {
            return Err(TestCaseError::fail(format!("parsing errors: {:?}",storage.as_vec())));
        }

        let mut output_table = table::Table::new();
        output_table.create_modules(&targets);
        let storage: Storage<Error> = Storage::new();
        output_table.populate_usings_in_targets(&targets, &storage);

        if !storage.as_vec().is_empty() {
            return Err(TestCaseError::fail(format!("table errors: {:?}",storage.as_vec())));
        }

        for (name, module_id) in &input_table.target_root_module_ids_by_name {
            input_table.validate(
                *module_id,
                &output_table,
                output_table.target_root_module_ids_by_name[name]
            )?;
        }
    }
}
