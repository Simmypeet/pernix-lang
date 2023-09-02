// Test for the correctness of module path using resolving
// Test for the correctness of module tree
// Test for the correctness of generic parameters drafting
// Test for the correctness of symbol drafting

use std::{
    collections::{BTreeSet, HashMap, HashSet},
    sync::atomic::AtomicUsize,
};

use pernixc_source::SourceFile;
use pernixc_syntax::syntax_tree::target::Target;
use pernixc_system::{
    arena::{self, Arena},
    diagnostic::Storage,
};
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::{
    error::Error,
    symbol::{self, Accessibility, Table, ID},
};

#[derive(Debug, Clone, PartialEq, Eq)]
struct GenericParameters {
    lifetimes: BTreeSet<String>,
    types: BTreeSet<String>,
    constants: BTreeSet<String>,
}

impl GenericParameters {
    fn validate(&self, output: &symbol::GenericParameters) -> TestCaseResult {
        prop_assert_eq!(self.lifetimes.len(), output.lifetimes.len());
        prop_assert_eq!(self.types.len(), output.types.len());
        prop_assert_eq!(self.constants.len(), output.constants.len());

        for (index, lifetime) in self.lifetimes.iter().enumerate() {
            prop_assert_eq!(
                lifetime,
                &output.lifetimes.get(arena::ID::new(index)).unwrap().name
            );
        }

        for (index, type_name) in self.types.iter().enumerate() {
            prop_assert_eq!(
                type_name,
                &output.types.get(arena::ID::new(index)).unwrap().name
            );
        }

        for (index, constant_name) in self.constants.iter().enumerate() {
            prop_assert_eq!(
                constant_name,
                &output.constants.get(arena::ID::new(index)).unwrap().name
            );
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ItemKind {
    Type(GenericParameters),
    Trait(GenericParameters),
    Struct(GenericParameters),
    Enum(GenericParameters),
    Function(GenericParameters),
    Constant,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Item {
    accessibility: Accessibility,
    kind: ItemKind,
}

struct ItemWithName<'a> {
    name: &'a str,
    item: &'a Item,
}

impl std::fmt::Display for GenericParameters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.lifetimes.is_empty() && self.types.is_empty() && self.constants.is_empty() {
            return Ok(());
        }

        write!(f, "<")?;

        let mut is_first = true;

        for lifetime in &self.lifetimes {
            if is_first {
                is_first = false;
            } else {
                write!(f, ", ")?;
            }

            write!(f, "'{lifetime}")?;
        }

        for type_name in &self.types {
            if is_first {
                is_first = false;
            } else {
                write!(f, ", ")?;
            }

            write!(f, "{type_name}")?;
        }

        for constant_name in &self.constants {
            if is_first {
                is_first = false;
            } else {
                write!(f, ", ")?;
            }

            write!(f, "{constant_name}: ()")?;
        }

        write!(f, ">")?;

        Ok(())
    }
}

impl<'a> std::fmt::Display for ItemWithName<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let accessibility_string = match self.item.accessibility {
            Accessibility::Public => "public",
            Accessibility::Private => "private",
            Accessibility::Internal => "internal",
        };
        match &self.item.kind {
            ItemKind::Type(generic_parameter) => {
                write!(
                    f,
                    "{} type {}{} = ();",
                    accessibility_string, self.name, generic_parameter
                )
            }
            ItemKind::Trait(generic_parameter) => {
                write!(
                    f,
                    "{} trait {}{} {{}}",
                    accessibility_string, self.name, generic_parameter
                )
            }
            ItemKind::Struct(generic_parameter) => {
                write!(
                    f,
                    "{} struct {}{} {{}}",
                    accessibility_string, self.name, generic_parameter
                )
            }
            ItemKind::Enum(generic_parameter) => {
                write!(
                    f,
                    "{} enum {}{} {{}}",
                    accessibility_string, self.name, generic_parameter
                )
            }
            ItemKind::Function(generic_parameter) => {
                write!(
                    f,
                    "{} function {}{}() {{}}",
                    accessibility_string, self.name, generic_parameter
                )
            }
            ItemKind::Constant => {
                write!(f, "{} const {}: () = ();", accessibility_string, self.name)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ModuleID(usize);

impl ModuleID {
    pub fn new() -> Self {
        static mut COUNTER: AtomicUsize = AtomicUsize::new(0);

        unsafe { Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)) }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Module {
    submodules: HashMap<String, (Accessibility, Module)>,
    items: HashMap<String, Item>,
    unique_id: ModuleID,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Usings {
    usings_by_module_id: HashMap<ModuleID, HashSet<ModuleID>>,
}

#[derive(Debug, Clone)]
struct ModuleRef<'a> {
    table: &'a Table,
    module_id: arena::ID<symbol::Module>,
}

impl Module {
    fn validate(
        &self,
        output: &ModuleRef,
        accessibility: Accessibility,
        name: &str,
        module_test_id_by_module_table_id: &mut HashMap<ModuleID, arena::ID<symbol::Module>>,
    ) -> TestCaseResult {
        module_test_id_by_module_table_id.insert(self.unique_id, output.module_id);

        let output_module = &output.table.modules[output.module_id];

        prop_assert_eq!(name, &output_module.name);
        prop_assert_eq!(accessibility, output_module.accessibility);

        for (item_name, item) in &self.items {
            let item_id = output.table.modules[output.module_id]
                .children_ids_by_name
                .get(item_name)
                .copied();

            let Some(item_id) = item_id else {
                return Err(TestCaseError::fail(format!(
                    "The item with the name `{item_name}` was not found.",
                )));
            };

            prop_assert_eq!(
                item.accessibility,
                output.table.get_symbol(item_id).unwrap().accessibility()
            );
            prop_assert_eq!(item_name, output.table.get_symbol(item_id).unwrap().name());

            match (&item.kind, item_id) {
                (ItemKind::Type(generic_parameters), ID::Type(type_id)) => {
                    let output_type = &output.table.types[type_id];

                    generic_parameters.validate(&output_type.generic_parameters)?;
                }

                (ItemKind::Trait(generic_parameters), ID::Trait(trait_id)) => {
                    let output_trait = &output.table.traits[trait_id];

                    generic_parameters.validate(&output_trait.generic_parameters)?;
                }

                (ItemKind::Struct(generic_parameters), ID::Struct(struct_id)) => {
                    let output_struct = &output.table.structs[struct_id];

                    generic_parameters.validate(&output_struct.generic_parameters)?;
                }

                (ItemKind::Enum(generic_parameters), ID::Enum(enum_id)) => {
                    let output_enum = &output.table.enums[enum_id];

                    generic_parameters.validate(&output_enum.generic_parameters)?;
                }

                (ItemKind::Function(generic_parameters), ID::Function(function_id)) => {
                    let output_function = &output.table.functions[function_id];

                    generic_parameters.validate(&output_function.generic_parameters)?;
                }

                (ItemKind::Constant, ID::Constant(_)) => (),

                (input, output) => {
                    return Err(TestCaseError::fail(format!(
                        "The item with the name `{item_name}` was expected to be a {input:?}, but \
                         it was a {output:?}."
                    )));
                }
            }
        }

        for (submodule_name, (accessibility, submodule)) in &self.submodules {
            let submodule_id = output_module
                .children_ids_by_name
                .get(submodule_name)
                .copied();

            let Some(ID::Module(submodule_id)) = submodule_id else {
                return Err(TestCaseError::fail(format!(
                    "The submodule with the name `{submodule_name}` was not found.",
                )));
            };

            let submodule_ref = ModuleRef {
                table: output.table,
                module_id: submodule_id,
            };

            submodule.validate(
                &submodule_ref,
                *accessibility,
                submodule_name,
                module_test_id_by_module_table_id,
            )?;
        }

        Ok(())
    }
}

fn name_strategy() -> impl Strategy<Value = String> {
    proptest::string::string_regex("[A-Z]+").unwrap()
}

fn accessibility_strategy() -> impl Strategy<Value = Accessibility> {
    prop_oneof![
        Just(Accessibility::Public),
        Just(Accessibility::Private),
        Just(Accessibility::Internal)
    ]
}

fn generic_parameters_strategy() -> impl Strategy<Value = GenericParameters> {
    let lifetime_strategy = name_strategy();
    let type_strategy = name_strategy();
    let constant_strategy = name_strategy();

    (
        proptest::collection::btree_set(lifetime_strategy, 0..=2),
        proptest::collection::btree_set(type_strategy, 0..=2),
        proptest::collection::btree_set(constant_strategy, 0..=2),
    )
        .prop_map(|(lifetimes, types, constants)| GenericParameters {
            lifetimes,
            types,
            constants,
        })
}

impl Arbitrary for Module {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        let item_strategy = (accessibility_strategy(), prop_oneof![
            generic_parameters_strategy().prop_map(ItemKind::Type),
            generic_parameters_strategy().prop_map(ItemKind::Enum),
            generic_parameters_strategy().prop_map(ItemKind::Struct),
            generic_parameters_strategy().prop_map(ItemKind::Function),
            generic_parameters_strategy().prop_map(ItemKind::Trait),
            Just(ItemKind::Constant),
        ])
            .prop_map(|(accessibility, kind)| Item {
                accessibility,
                kind,
            })
            .boxed();

        let leaf = proptest::collection::hash_map(name_strategy(), item_strategy.clone(), 4)
            .prop_map(|items| Self {
                submodules: HashMap::new(),
                items,
                unique_id: ModuleID::new(),
            });

        leaf.prop_recursive(
            4,  // levels deep
            24, // max size of each level
            6,  // items per collection
            move |inner| {
                (
                    proptest::collection::hash_map(
                        name_strategy(),
                        (accessibility_strategy(), inner),
                        0..=6,
                    ),
                    proptest::collection::hash_map(name_strategy(), item_strategy.clone(), 0..=6),
                )
                    .prop_map(|(submodules, mut items)| {
                        // remove the items that are occupied by the submodules
                        for submodule_name in submodules.keys() {
                            items.remove(submodule_name);
                        }

                        Self {
                            submodules,
                            items,
                            unique_id: ModuleID::new(),
                        }
                    })
            },
        )
        .boxed()
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn write_module(
            module: &Module,
            name: &str,
            accessibility: Accessibility,
            f: &mut std::fmt::Formatter<'_>,
        ) -> std::fmt::Result {
            write!(
                f,
                "{} module {} {{ ",
                match accessibility {
                    Accessibility::Public => "public",
                    Accessibility::Private => "private",
                    Accessibility::Internal => "internal",
                },
                name
            )?;

            for (submodule_name, (accessibility, submodule)) in &module.submodules {
                write_module(submodule, submodule_name, *accessibility, f)?;
            }

            for (name, item) in &module.items {
                write!(f, "{}", ItemWithName { name, item })?;
            }

            write!(f, "}}")?;

            Ok(())
        }

        for (name, (accessibility, module)) in &self.submodules {
            write_module(module, name, *accessibility, f)?;
        }

        for (name, item) in &self.items {
            write!(f, "{}", ItemWithName { name, item })?;
        }

        Ok(())
    }
}

proptest! {
    #[test]
    fn module_tree_drafting_test(
        module_tree in Module::arbitrary()
    ) {
        let string = module_tree.to_string();
        let source_file = SourceFile::temp(&string).unwrap();

        let storage: Storage<TargetParseError> = Storage::new();
        let target = Target::parse(&source_file, "Test".to_string(), &storage);

        if !storage.as_vec().is_empty() {
            return Err(TestCaseError::fail(format!(
                "Failed to parse the target: {storage:#?}",
            )));
        }

        let mut table = Table {
            modules: Arena::default(),
            structs: Arena::default(),
            enums: Arena::default(),
            traits: Arena::default(),
            types: Arena::default(),
            functions: Arena::default(),
            constants: Arena::default(),
            target_root_module_ids_by_name: HashMap::new(),
            states_by_drafting_symbol_refs: HashMap::new(),
        };

        let mut module_test_id_by_module_table_id = HashMap::new();
        let mut implements_syntax_tree_vecs_by_module_id = HashMap::new();

        let storage: Storage<Error> = Storage::new();

        table.draft_target(
            target,
            &mut implements_syntax_tree_vecs_by_module_id,
            &storage
        ).expect("should've no problem");

        let root_target_id = table
            .target_root_module_ids_by_name
            .get("Test")
            .copied()
            .unwrap();

        module_tree.validate(
            &ModuleRef {
                table: &table,
                module_id: root_target_id
            },
            Accessibility::Public,
            "Test",
            &mut module_test_id_by_module_table_id
        )?;
    }
}

#[derive(Debug, derive_more::From)]
pub enum TargetParseError {
    Lexical(pernixc_lexical::error::Error),
    Syntax(pernixc_syntax::error::Error),
    Target(pernixc_syntax::syntax_tree::target::Error),
}
