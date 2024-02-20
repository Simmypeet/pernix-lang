use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    sync::Arc,
};

use parking_lot::RwLock;
use pernixc_base::{diagnostic::Storage, source_file::SourceFile};
use pernixc_syntax::syntax_tree::target::{self, Target};
use proptest::{
    arbitrary::Arbitrary,
    prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use super::Drafting;
use crate::{
    arena::ID,
    error::Error,
    symbol::{self, Accessibility, ModuleMemberID, TraitMemberID},
    table::{draft_table, HandlerAdaptor, Index, Table},
};

fn name_strategy() -> impl Strategy<Value = String> { "[a-zA-Z_][a-zA-Z0-9_]*" }

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
        drafting_table: &Table<Drafting>,
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
            (ModuleMemberID::Module(id), Self::Module(out)) => {
                out.verify(id, parent_module_id, expected_name, drafting_table)
            }
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
        drafting_table: &Table<Drafting>,
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
        drafting_table: &Table<Drafting>,
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
        drafting_table: &Table<Drafting>,
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
        drafting_table: &Table<Drafting>,
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
        drafting_table: &Table<Drafting>,
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
        drafting_table: &Table<Drafting>,
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
    pub accessibility: Accessibility,
    pub items: HashMap<String, Item>,
}

impl Module {
    pub fn verify(
        &self,
        self_id: ID<symbol::Module>,
        parent_module_id: ID<symbol::Module>,
        expected_name: &str,
        drafting_table: &Table<Drafting>,
    ) -> TestCaseResult {
        let sym = drafting_table.get(self_id).unwrap();

        // verify name, parent module, and accessibility
        prop_assert_eq!(&sym.name, expected_name);
        prop_assert_eq!(sym.parent_module_id, Some(parent_module_id));
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
                        accessibility,
                        items: HashMap::new(),
                    },
                )),
                0..=4,
            ),
        )
            .prop_map(|(accessibility, items)| Self { accessibility, items });

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
pub struct Items<'a>(&'a HashMap<String, Item>);

impl Display for Items<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (name, item) in self.0 {
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

fn property_based_testing_impl(module: &Module) -> TestCaseResult {
    let source_file = Arc::new(SourceFile::temp(Items(&module.items))?);
    let storage = Storage::<ParseTargetError>::default();
    let target = Target::parse(&source_file, "test".to_string(), &storage);

    if !storage.as_vec().is_empty() {
        return Err(TestCaseError::fail(format!(
            "parsing error: {:#?}",
            storage.into_vec()
        )));
    }

    let storage = Storage::<Box<dyn Error>>::new();
    let handler_adapter =
        HandlerAdaptor { handler: &storage, received: RwLock::new(false) };

    let drafting_table =
        draft_table(rayon::iter::once(target), &handler_adapter)?;

    if !storage.as_vec().is_empty() {
        return Err(TestCaseError::fail(format!(
            "drafting error: {:#?}",
            storage.into_vec()
        )));
    }

    let root_module_id =
        drafting_table.root_module_ids_by_name.get("test").copied().unwrap();

    prop_assert_eq!(
        drafting_table.get(root_module_id).unwrap().accessibility,
        Accessibility::Public
    );
    prop_assert_eq!(drafting_table.root_module_ids_by_name.len(), 1);

    // verify all each item
    for (name, item) in &module.items {
        item.verify(root_module_id, name, &drafting_table)?;
    }

    Ok(())
}

proptest! {
    #[test]
    fn property_based_testing(
        module in Module::arbitrary()
    ) {
        property_based_testing_impl(&module)?;
    }
}
