use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    str::FromStr,
    sync::Arc,
};

use parking_lot::RwLock;
use pernixc_base::{diagnostic::Storage, source_file::SourceFile};
use pernixc_lexical::token::KeywordKind;
use pernixc_syntax::syntax_tree::target::{self, Target};
use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::{
    arena::ID,
    error::Error,
    symbol::{self, Accessibility, ModuleMemberID, TraitMemberID},
    table::{
        draft_table, state::building::Building, transition_to_building,
        HandlerAdaptor, Index, Table,
    },
};

fn name_strategy() -> impl Strategy<Value = String> {
    "[a-zA-Z_][a-zA-Z0-9_]*".prop_filter("filter out keywords", |str| {
        KeywordKind::from_str(str).is_err()
    })
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    Module(Module),
    Trait(Trait),
    Function(Function),
    Type(Type),
    Struct(Struct),
    Enum(Enum),
    Constant(Constant),
}

impl Item {
    pub fn verify(
        &self,
        parent_module_id: ID<symbol::Module>,
        expected_name: &str,
        drafting_table: &Table<Building>,
    ) -> TestCaseResult {
        let member_id = drafting_table
            .get(parent_module_id)
            .unwrap()
            .module_child_ids_by_name
            .get(expected_name)
            .copied()
            .unwrap();

        // test if the kind matches
        match (member_id, self) {
            (ModuleMemberID::Module(id), Self::Module(out)) => out.verify(
                id,
                Some(parent_module_id),
                expected_name,
                drafting_table,
            ),
            (ModuleMemberID::Trait(id), Self::Trait(out)) => {
                out.verify(id, parent_module_id, expected_name, drafting_table)
            }
            (ModuleMemberID::Function(id), Self::Function(out)) => {
                out.verify(id, parent_module_id, expected_name, drafting_table)
            }
            (ModuleMemberID::Type(id), Self::Type(out)) => {
                out.verify(id, parent_module_id, expected_name, drafting_table)
            }
            (ModuleMemberID::Struct(id), Self::Struct(out)) => {
                out.verify(id, parent_module_id, expected_name, drafting_table)
            }
            (ModuleMemberID::Enum(id), Self::Enum(out)) => {
                out.verify(id, parent_module_id, expected_name, drafting_table)
            }
            (ModuleMemberID::Constant(id), Self::Constant(out)) => {
                out.verify(id, parent_module_id, expected_name, drafting_table)
            }

            _ => Err(TestCaseError::fail(format!(
                "expected kind: {member_id:#?}, got: {self:#?}"
            ))),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TraitMember {
    Function,
    Constant,
    Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trait {
    pub accessibility: Accessibility,
    pub trait_members: HashMap<String, TraitMember>,
}

impl Trait {
    pub fn verify(
        &self,
        self_id: ID<symbol::Trait>,
        parent_module_id: ID<symbol::Module>,
        expected_name: &str,
        drafting_table: &Table<Building>,
    ) -> TestCaseResult {
        let sym = drafting_table.get(self_id).unwrap();

        // verify name, parent module, and accessibility
        prop_assert_eq!(&sym.name, expected_name);
        prop_assert_eq!(sym.parent_module_id, parent_module_id);
        prop_assert_eq!(sym.accessibility, self.accessibility);

        prop_assert_eq!(
            self.trait_members.len(),
            sym.trait_member_ids_by_name.len()
        );

        for (name, kind) in &self.trait_members {
            let member_id =
                sym.trait_member_ids_by_name.get(name).copied().unwrap();

            match (member_id, kind) {
                (TraitMemberID::Type(id), TraitMember::Type) => {
                    let sym = drafting_table.get(id).unwrap();

                    prop_assert_eq!(&sym.name, name);
                    prop_assert_eq!(sym.parent_id, self_id);

                    drop(sym);
                }
                (TraitMemberID::Function(id), TraitMember::Function) => {
                    let sym = drafting_table.get(id).unwrap();

                    prop_assert_eq!(&sym.name, name);
                    prop_assert_eq!(sym.parent_id, self_id);

                    drop(sym);
                }
                (TraitMemberID::Constant(id), TraitMember::Constant) => {
                    let sym = drafting_table.get(id).unwrap();

                    prop_assert_eq!(&sym.name, name);
                    prop_assert_eq!(sym.parent_id, self_id);

                    drop(sym);
                }
                (found, expected) => {
                    return Err(TestCaseError::fail(format!(
                        "expected kind: {expected:#?}, got: {found:#?}"
                    )));
                }
            }
        }

        drop(sym);

        Ok(())
    }
}

impl Arbitrary for Trait {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            Accessibility::arbitrary(),
            proptest::collection::hash_map(
                name_strategy(),
                prop_oneof![
                    Just(TraitMember::Function),
                    Just(TraitMember::Constant),
                    Just(TraitMember::Type),
                ],
                0..=4,
            ),
        )
            .prop_map(|(accessibility, trait_members)| Self {
                accessibility,
                trait_members,
            })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    pub accessibility: Accessibility,
}

impl Function {
    pub fn verify(
        &self,
        self_id: ID<symbol::Function>,
        parent_module_id: ID<symbol::Module>,
        expected_name: &str,
        drafting_table: &Table<Building>,
    ) -> TestCaseResult {
        let sym = drafting_table.get(self_id).unwrap();

        // verify name, parent module, and accessibility
        prop_assert_eq!(&sym.name, expected_name);
        prop_assert_eq!(sym.parent_id, parent_module_id);
        prop_assert_eq!(sym.accessibility, self.accessibility);

        drop(sym);

        Ok(())
    }
}

impl Arbitrary for Function {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Accessibility::arbitrary()
            .prop_map(|accessibility| Self { accessibility })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Type {
    pub accessibility: Accessibility,
}

impl Type {
    pub fn verify(
        &self,
        self_id: ID<symbol::Type>,
        parent_module_id: ID<symbol::Module>,
        expected_name: &str,
        drafting_table: &Table<Building>,
    ) -> TestCaseResult {
        let sym = drafting_table.get(self_id).unwrap();

        // verify name, parent module, and accessibility
        prop_assert_eq!(&sym.name, expected_name);
        prop_assert_eq!(sym.parent_id, parent_module_id);
        prop_assert_eq!(sym.accessibility, self.accessibility);

        drop(sym);

        Ok(())
    }
}

impl Arbitrary for Type {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Accessibility::arbitrary()
            .prop_map(|accessibility| Self { accessibility })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub accessibility: Accessibility,
}

impl Struct {
    pub fn verify(
        &self,
        self_id: ID<symbol::Struct>,
        parent_module_id: ID<symbol::Module>,
        expected_name: &str,
        drafting_table: &Table<Building>,
    ) -> TestCaseResult {
        let sym = drafting_table.get(self_id).unwrap();

        // verify name, parent module, and accessibility
        prop_assert_eq!(&sym.name, expected_name);
        prop_assert_eq!(sym.parent_module_id, parent_module_id);
        prop_assert_eq!(sym.accessibility, self.accessibility);

        drop(sym);

        Ok(())
    }
}

impl Arbitrary for Struct {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Accessibility::arbitrary()
            .prop_map(|accessibility| Self { accessibility })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    pub accessibility: Accessibility,
    pub variants: HashSet<String>,
}

impl Enum {
    pub fn verify(
        &self,
        self_id: ID<symbol::Enum>,
        parent_module_id: ID<symbol::Module>,
        expected_name: &str,
        drafting_table: &Table<Building>,
    ) -> TestCaseResult {
        let sym = drafting_table.get(self_id).unwrap();

        // verify name, parent module, and accessibility
        prop_assert_eq!(&sym.name, expected_name);
        prop_assert_eq!(sym.parent_module_id, parent_module_id);
        prop_assert_eq!(sym.accessibility, self.accessibility);

        prop_assert_eq!(self.variants.len(), sym.variant_ids_by_name.len());

        for variant in &self.variants {
            let variant_id =
                sym.variant_ids_by_name.get(variant).copied().unwrap();

            let variant_sym = drafting_table.get(variant_id).unwrap();

            prop_assert_eq!(&variant_sym.name, variant);
            prop_assert_eq!(variant_sym.parent_enum_id, self_id);

            drop(variant_sym);
        }

        drop(sym);

        Ok(())
    }
}

impl Arbitrary for Enum {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            Accessibility::arbitrary(),
            proptest::collection::hash_set(name_strategy(), 0..=4),
        )
            .prop_map(|(accessibility, variants)| Self {
                accessibility,
                variants,
            })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constant {
    pub accessibility: Accessibility,
}

impl Constant {
    pub fn verify(
        &self,
        self_id: ID<symbol::Constant>,
        parent_module_id: ID<symbol::Module>,
        expected_name: &str,
        drafting_table: &Table<Building>,
    ) -> TestCaseResult {
        let sym = drafting_table.get(self_id).unwrap();

        // verify name, parent module, and accessibility
        prop_assert_eq!(&sym.name, expected_name);
        prop_assert_eq!(sym.parent_id, parent_module_id);
        prop_assert_eq!(sym.accessibility, self.accessibility);

        drop(sym);

        Ok(())
    }
}

impl Arbitrary for Constant {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Accessibility::arbitrary()
            .prop_map(|accessibility| Self { accessibility })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub usings: HashSet<Vec<String>>,
    pub accessibility: Accessibility,
    pub items: HashMap<String, Item>,
}

impl Module {
    pub fn verify(
        &self,
        self_id: ID<symbol::Module>,
        parent_module_id: Option<ID<symbol::Module>>,
        expected_name: &str,
        drafting_table: &Table<Building>,
    ) -> TestCaseResult {
        let sym = drafting_table.get(self_id).unwrap();

        prop_assert_eq!(self.usings.len(), sym.usings.len());

        for using in &self.usings {
            let using_id = drafting_table
                .get_by_qualified_name(using.iter().map(AsRef::as_ref))
                .expect("should exist")
                .into_module()
                .expect("should be module");

            prop_assert!(sym.usings.contains(&using_id));
        }

        // verify name, parent module, and accessibility
        prop_assert_eq!(&sym.name, expected_name);
        prop_assert_eq!(sym.parent_module_id, parent_module_id);
        prop_assert_eq!(sym.accessibility, self.accessibility);

        prop_assert_eq!(self.items.len(), sym.module_child_ids_by_name.len());

        for (name, item) in &self.items {
            item.verify(self_id, name, drafting_table)?;
        }

        drop(sym);

        Ok(())
    }
}

impl Arbitrary for Module {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        fn item_strategy(
            module_strategy: impl Strategy<Value = Module>,
        ) -> impl Strategy<Value = Item> {
            prop_oneof![
                module_strategy.prop_map(Item::Module),
                Function::arbitrary().prop_map(Item::Function),
                Trait::arbitrary().prop_map(Item::Trait),
                Type::arbitrary().prop_map(Item::Type),
                Struct::arbitrary().prop_map(Item::Struct),
                Enum::arbitrary().prop_map(Item::Enum),
                Constant::arbitrary().prop_map(Item::Constant),
            ]
        }

        let leaf = (
            Accessibility::arbitrary(),
            proptest::collection::hash_map(
                name_strategy(),
                item_strategy(Accessibility::arbitrary().prop_map(
                    |accessibility| Self {
                        usings: HashSet::new(),
                        accessibility,
                        items: HashMap::new(),
                    },
                )),
                0..=4,
            ),
        )
            .prop_map(|(accessibility, items)| Self {
                usings: HashSet::new(),
                accessibility,
                items,
            });

        leaf.prop_recursive(4, 16, 4, |inner| {
            (
                Accessibility::arbitrary(),
                proptest::collection::hash_map(
                    name_strategy(),
                    item_strategy(inner),
                    0..=4,
                ),
            )
                .prop_map(|(accessibility, items)| Self {
                    usings: HashSet::new(),
                    accessibility,
                    items,
                })
        })
        .boxed()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WithName<'a, T> {
    pub name: &'a str,
    pub value: &'a T,
}

impl<'a> Display for WithName<'a, Module> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} module {} {{ ", self.value.accessibility, self.name)?;

        for using in &self.value.usings {
            write!(f, "using {};", using.join("::"))?;
        }

        for (name, item) in &self.value.items {
            write!(f, "{}", WithName { name, value: item })?;
        }

        write!(f, "}}")?;

        Ok(())
    }
}

impl<'a> Display for WithName<'a, Trait> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} trait {} {{ ", self.value.accessibility, self.name)?;
        for (name, member) in &self.value.trait_members {
            match member {
                TraitMember::Function => write!(f, "function {name}();")?,
                TraitMember::Constant => write!(f, "const {name}: int32;")?,
                TraitMember::Type => write!(f, "type {name};")?,
            }
        }

        write!(f, "}}")?;

        Ok(())
    }
}

impl<'a> Display for WithName<'a, Function> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} function {}() {{}}", self.value.accessibility, self.name)
    }
}

impl<'a> Display for WithName<'a, Type> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} type {} = bool;", self.value.accessibility, self.name)
    }
}

impl<'a> Display for WithName<'a, Struct> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} struct {} {{}}", self.value.accessibility, self.name)
    }
}

impl<'a> Display for WithName<'a, Enum> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} enum {} {{ ", self.value.accessibility, self.name)?;
        for variant in &self.value.variants {
            write!(f, "{variant}, ")?;
        }
        write!(f, "}}")?;

        Ok(())
    }
}

impl<'a> Display for WithName<'a, Constant> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} const {}: int32 = 32;",
            self.value.accessibility, self.name
        )
    }
}

impl<'a> Display for WithName<'a, Item> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            Item::Module(t) => {
                write!(f, "{}", WithName { name: self.name, value: t })
            }
            Item::Trait(t) => {
                write!(f, "{}", WithName { name: self.name, value: t })
            }
            Item::Function(t) => {
                write!(f, "{}", WithName { name: self.name, value: t })
            }
            Item::Type(t) => {
                write!(f, "{}", WithName { name: self.name, value: t })
            }
            Item::Struct(t) => {
                write!(f, "{}", WithName { name: self.name, value: t })
            }
            Item::Enum(t) => {
                write!(f, "{}", WithName { name: self.name, value: t })
            }
            Item::Constant(t) => {
                write!(f, "{}", WithName { name: self.name, value: t })
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RootModule<'a>(&'a Module);

impl<'a> Display for RootModule<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for using in &self.0.usings {
            write!(f, "using {};", using.join("::"))?;
        }

        for (name, item) in &self.0.items {
            write!(f, "{}", WithName { name, value: item })?;
        }

        Ok(())
    }
}

#[derive(Debug, derive_more::From)]
pub enum ParseTargetError {
    Target(target::Error),
    Syntax(pernixc_syntax::error::Error),
    Lexical(pernixc_lexical::error::Error),
}

const ROOT_MODULE_NAME: &str = "test";

fn property_based_testing_impl(module: &Module) -> TestCaseResult {
    let source_file = Arc::new(SourceFile::temp(RootModule(module))?);
    println!("{}", RootModule(module));

    let storage = Storage::<ParseTargetError>::default();
    let target =
        Target::parse(&source_file, ROOT_MODULE_NAME.to_string(), &storage);

    if !storage.as_vec().is_empty() {
        return Err(TestCaseError::fail(format!(
            "parsing error: {:#?}",
            storage.into_vec()
        )));
    }

    let storage = Storage::<Box<dyn Error>>::new();
    let handler_adapter =
        HandlerAdaptor { handler: &storage, received: RwLock::new(false) };

    let building_table = transition_to_building(
        draft_table(rayon::iter::once(target), &handler_adapter)?,
        &handler_adapter,
    );

    if !storage.as_vec().is_empty() {
        return Err(TestCaseError::fail(format!(
            "drafting error: {:#?}",
            storage.into_vec()
        )));
    }

    let root_module_id = building_table
        .root_module_ids_by_name
        .get(ROOT_MODULE_NAME)
        .copied()
        .unwrap();

    module.verify(root_module_id, None, ROOT_MODULE_NAME, &building_table)?;

    Ok(())
}

fn get_module_by_name<'a>(
    parent_module_name: &[String],
    current_module_name: String,
    module: &'a mut Module,
    expected_module_name: &[String],
) -> Option<&'a mut Module> {
    let mut current_full_module_name = parent_module_name.to_vec();
    current_full_module_name.push(current_module_name);

    if current_full_module_name == expected_module_name {
        return Some(module);
    }

    for (name, item) in &mut module.items {
        if let Item::Module(module) = item {
            if let Some(module) = get_module_by_name(
                &current_full_module_name,
                name.to_string(),
                module,
                expected_module_name,
            ) {
                return Some(module);
            }
        }
    }

    None
}

fn get_all_modules_name(
    parent_module_name: &[String],
    parent_accessibility: Accessibility,
    current_module_name: String,
    module: &Module,
    all_modules_name: &mut Vec<(Vec<String>, Accessibility)>,
) {
    let intrinsic_accessibility =
        parent_accessibility.min(module.accessibility);

    let mut current_full_module_name = parent_module_name.to_vec();
    current_full_module_name.push(current_module_name);
    all_modules_name
        .push((current_full_module_name.clone(), intrinsic_accessibility));

    for (name, item) in &module.items {
        if let Item::Module(module) = item {
            get_all_modules_name(
                &current_full_module_name,
                intrinsic_accessibility,
                name.to_string(),
                module,
                all_modules_name,
            );
        }
    }
}

fn module_strategy_with_usings() -> impl Strategy<Value = Module> {
    let module_strategy = Module::arbitrary();

    module_strategy.prop_flat_map(move |module| {
        let mut all_module_name = Vec::new();

        get_all_modules_name(
            &[],
            Accessibility::Public,
            ROOT_MODULE_NAME.to_string(),
            &module,
            &mut all_module_name,
        );

        let module_count = all_module_name.len();

        proptest::collection::hash_map(
            proptest::sample::select(
                all_module_name
                    .clone()
                    .into_iter()
                    .map(|(name, _)| name)
                    .collect::<Vec<_>>(),
            ),
            proptest::collection::hash_set(
                proptest::sample::select(all_module_name),
                0..=module_count,
            ),
            0..=module_count,
        )
        .prop_map(move |usings_by_module_name| {
            let mut module = module.clone();

            for (module_name, usings) in usings_by_module_name {
                let module = get_module_by_name(
                    &[],
                    ROOT_MODULE_NAME.to_string(),
                    &mut module,
                    &module_name,
                )
                .expect("should be found");

                for using in usings {
                    if using.1 == Accessibility::Private {
                        continue;
                    }

                    if using.0 != module_name {
                        module.usings.insert(using.0);
                    }
                }
            }

            module
        })
    })
}

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        cases: 2048,
        max_shrink_iters: 100_000,
        ..Default::default()
    })]

    #[test]
    fn property_based_testing(
        mut module in module_strategy_with_usings()
    ) {
        module.accessibility = Accessibility::Public;
        property_based_testing_impl(&module)?;
    }
}
