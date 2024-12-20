use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    str::FromStr,
    sync::Arc,
};

use parking_lot::RwLock;
use pernixc_base::{handler::Storage, source_file::SourceFile};
use pernixc_lexical::token::KeywordKind;
use pernixc_syntax::syntax_tree::target::{self, Target};
use proptest::{
    arbitrary::Arbitrary,
    prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::{
    arena::ID,
    error::Error,
    symbol::{
        self,
        table::{
            representation::{
                building::finalizing::Finalizer, draft_table,
                transition_to_building, Container, HandlerAdaptor, Index,
                Representation, RwLockContainer,
            },
            Building, Table,
        },
        Accessibility, GlobalID, ModuleMemberID, TraitMemberID,
    },
};

fn name_strategy() -> impl Strategy<Value = String> {
    "[a-zA-Z_][a-zA-Z0-9_]*".prop_filter("filter out keywords", |str| {
        KeywordKind::from_str(str).is_err()
    })
}

impl<T: Container> Representation<T> {
    fn validate_access_modifier(
        &self,
        access_modifier: AccessModifier,
        global_id: GlobalID,
    ) -> TestCaseResult {
        let found_accessibility = self.get_accessibility(global_id).unwrap();

        match (found_accessibility, access_modifier) {
            (Accessibility::Public, AccessModifier::Public) => Ok(()),

            (
                Accessibility::Scoped(scoped),
                access_modifier @ (AccessModifier::Internal
                | AccessModifier::Private),
            ) => {
                let expected = match access_modifier {
                    AccessModifier::Private => self.get_closet_module_id(
                        self.get_global(global_id)
                            .unwrap()
                            .parent_global_id()
                            .unwrap(),
                    ),
                    AccessModifier::Internal => self.get_root_module_id(
                        self.get_global(global_id)
                            .unwrap()
                            .parent_global_id()
                            .unwrap(),
                    ),
                    AccessModifier::Public => unreachable!(),
                };

                if expected.unwrap() == scoped {
                    Ok(())
                } else {
                    Err(TestCaseError::fail(format!(
                        "expected {expected:?} but found {scoped:?}"
                    )))
                }
            }

            (found, expected) => Err(TestCaseError::fail(format!(
                "expected {expected:?} but found {found:?}"
            ))),
        }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::Display,
)]
pub enum AccessModifier {
    #[display(fmt = "private")]
    Private,

    #[display(fmt = "internal")]
    Internal,

    #[display(fmt = "public")]
    Public,
}

impl Arbitrary for AccessModifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Public),
            Just(Self::Private),
            Just(Self::Internal)
        ]
        .boxed()
    }
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
        drafting_table: &Table<Building<RwLockContainer, Finalizer>>,
    ) -> TestCaseResult {
        let member_id = drafting_table
            .get(parent_module_id)
            .unwrap()
            .member_ids_by_name
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
    Function(AccessModifier),
    Constant(AccessModifier),
    Type(AccessModifier),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trait {
    pub access_modifier: AccessModifier,
    pub trait_members: HashMap<String, TraitMember>,
}

impl Trait {
    pub fn verify(
        &self,
        self_id: ID<symbol::Trait>,
        parent_module_id: ID<symbol::Module>,
        expected_name: &str,
        drafting_table: &Table<Building<RwLockContainer, Finalizer>>,
    ) -> TestCaseResult {
        let sym = drafting_table.get(self_id).unwrap();

        // verify name, parent module, and accessibility
        prop_assert_eq!(&sym.name, expected_name);
        prop_assert_eq!(sym.parent_id, parent_module_id);

        drafting_table
            .validate_access_modifier(self.access_modifier, self_id.into())?;

        prop_assert_eq!(self.trait_members.len(), sym.member_ids_by_name.len());

        for (name, kind) in &self.trait_members {
            let member_id = sym.member_ids_by_name.get(name).copied().unwrap();

            match (member_id, kind) {
                (
                    TraitMemberID::Type(id),
                    TraitMember::Type(access_modifier),
                ) => {
                    let sym = drafting_table.get(id).unwrap();

                    prop_assert_eq!(&sym.name, name);
                    prop_assert_eq!(sym.parent_id, self_id);

                    drafting_table.validate_access_modifier(
                        *access_modifier,
                        id.into(),
                    )?;

                    drop(sym);
                }
                (
                    TraitMemberID::Function(id),
                    TraitMember::Function(access_modifier),
                ) => {
                    let sym = drafting_table.get(id).unwrap();

                    prop_assert_eq!(&sym.name, name);
                    prop_assert_eq!(sym.parent_id, self_id);

                    drafting_table.validate_access_modifier(
                        *access_modifier,
                        id.into(),
                    )?;

                    drop(sym);
                }
                (
                    TraitMemberID::Constant(id),
                    TraitMember::Constant(access_modifier),
                ) => {
                    let sym = drafting_table.get(id).unwrap();

                    prop_assert_eq!(&sym.name, name);
                    prop_assert_eq!(sym.parent_id, self_id);

                    drafting_table.validate_access_modifier(
                        *access_modifier,
                        id.into(),
                    )?;

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
            AccessModifier::arbitrary(),
            proptest::collection::hash_map(
                name_strategy(),
                AccessModifier::arbitrary().prop_flat_map(|accessibility| {
                    prop_oneof![
                        Just(TraitMember::Function(accessibility)),
                        Just(TraitMember::Constant(accessibility)),
                        Just(TraitMember::Type(accessibility)),
                    ]
                }),
                0..=4,
            ),
        )
            .prop_map(|(access_modifier, mut trait_members)| {
                for mut trait_member in trait_members.values_mut() {
                    let trait_member_acc = match &mut trait_member {
                        TraitMember::Function(accessibility)
                        | TraitMember::Constant(accessibility)
                        | TraitMember::Type(accessibility) => accessibility,
                    };

                    *trait_member_acc =
                        (*trait_member_acc).min(access_modifier);
                }

                Self { access_modifier, trait_members }
            })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    pub access_modifier: AccessModifier,
}

impl Function {
    pub fn verify(
        &self,
        self_id: ID<symbol::Function>,
        parent_module_id: ID<symbol::Module>,
        expected_name: &str,
        drafting_table: &Table<Building<RwLockContainer, Finalizer>>,
    ) -> TestCaseResult {
        let sym = drafting_table.get(self_id).unwrap();

        // verify name, parent module, and accessibility
        prop_assert_eq!(&sym.name, expected_name);
        prop_assert_eq!(sym.parent_id, parent_module_id);

        drafting_table
            .validate_access_modifier(self.access_modifier, self_id.into())?;

        drop(sym);

        Ok(())
    }
}

impl Arbitrary for Function {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        AccessModifier::arbitrary()
            .prop_map(|access_modifier| Self { access_modifier })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Type {
    pub access_modifier: AccessModifier,
}

impl Type {
    pub fn verify(
        &self,
        self_id: ID<symbol::Type>,
        parent_module_id: ID<symbol::Module>,
        expected_name: &str,
        drafting_table: &Table<Building<RwLockContainer, Finalizer>>,
    ) -> TestCaseResult {
        let sym = drafting_table.get(self_id).unwrap();

        // verify name, parent module, and accessibility
        prop_assert_eq!(&sym.name, expected_name);
        prop_assert_eq!(sym.parent_id, parent_module_id);

        drafting_table
            .validate_access_modifier(self.access_modifier, self_id.into())?;

        drop(sym);

        Ok(())
    }
}

impl Arbitrary for Type {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        AccessModifier::arbitrary()
            .prop_map(|access_modifier| Self { access_modifier })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub access_modifier: AccessModifier,
}

impl Struct {
    pub fn verify(
        &self,
        self_id: ID<symbol::Struct>,
        parent_module_id: ID<symbol::Module>,
        expected_name: &str,
        drafting_table: &Table<Building<RwLockContainer, Finalizer>>,
    ) -> TestCaseResult {
        let sym = drafting_table.get(self_id).unwrap();

        // verify name, parent module, and accessibility
        prop_assert_eq!(&sym.name, expected_name);
        prop_assert_eq!(sym.parent_id, parent_module_id);

        drafting_table
            .validate_access_modifier(self.access_modifier, self_id.into())?;

        drop(sym);

        Ok(())
    }
}

impl Arbitrary for Struct {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        AccessModifier::arbitrary()
            .prop_map(|access_modifier| Self { access_modifier })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    pub access_modifier: AccessModifier,
    pub variants: HashSet<String>,
}

impl Enum {
    pub fn verify(
        &self,
        self_id: ID<symbol::Enum>,
        parent_module_id: ID<symbol::Module>,
        expected_name: &str,
        drafting_table: &Table<Building<RwLockContainer, Finalizer>>,
    ) -> TestCaseResult {
        let sym = drafting_table.get(self_id).unwrap();

        // verify name, parent module, and accessibility
        prop_assert_eq!(&sym.name, expected_name);
        prop_assert_eq!(sym.parent_id, parent_module_id);

        drafting_table
            .validate_access_modifier(self.access_modifier, self_id.into())?;

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
            AccessModifier::arbitrary(),
            proptest::collection::hash_set(name_strategy(), 0..=4),
        )
            .prop_map(|(access_modifier, variants)| Self {
                access_modifier,
                variants,
            })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constant {
    pub access_modifier: AccessModifier,
}

impl Constant {
    pub fn verify(
        &self,
        self_id: ID<symbol::Constant>,
        parent_module_id: ID<symbol::Module>,
        expected_name: &str,
        drafting_table: &Table<Building<RwLockContainer, Finalizer>>,
    ) -> TestCaseResult {
        let sym = drafting_table.get(self_id).unwrap();

        // verify name, parent module, and accessibility
        prop_assert_eq!(&sym.name, expected_name);
        prop_assert_eq!(sym.parent_id, parent_module_id);

        drafting_table
            .validate_access_modifier(self.access_modifier, self_id.into())?;

        drop(sym);

        Ok(())
    }
}

impl Arbitrary for Constant {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        AccessModifier::arbitrary()
            .prop_map(|access_modifier| Self { access_modifier })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub access_modifier: AccessModifier,
    pub items: HashMap<String, Item>,
}

impl Module {
    pub fn verify(
        &self,
        self_id: ID<symbol::Module>,
        parent_module_id: Option<ID<symbol::Module>>,
        expected_name: &str,
        drafting_table: &Table<Building<RwLockContainer, Finalizer>>,
    ) -> TestCaseResult {
        let sym = drafting_table.get(self_id).unwrap();

        // verify name, parent module, and accessibility
        prop_assert_eq!(&sym.name, expected_name);
        prop_assert_eq!(sym.parent_module_id, parent_module_id);

        drafting_table
            .validate_access_modifier(self.access_modifier, self_id.into())?;

        prop_assert_eq!(self.items.len(), sym.member_ids_by_name.len());

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
            AccessModifier::arbitrary(),
            proptest::collection::hash_map(
                name_strategy(),
                item_strategy(AccessModifier::arbitrary().prop_map(
                    |access_modifier| Self {
                        access_modifier,
                        items: HashMap::new(),
                    },
                )),
                0..=4,
            ),
        )
            .prop_map(|(access_modifier, items)| Self {
                access_modifier,
                items,
            });

        leaf.prop_recursive(4, 16, 4, |inner| {
            (
                AccessModifier::arbitrary(),
                proptest::collection::hash_map(
                    name_strategy(),
                    item_strategy(inner),
                    0..=4,
                ),
            )
                .prop_map(|(access_modifier, items)| Self {
                    access_modifier,
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
        write!(f, "{} module {} {{ ", self.value.access_modifier, self.name)?;

        for (name, item) in &self.value.items {
            write!(f, "{}", WithName { name, value: item })?;
        }

        write!(f, "}}")?;

        Ok(())
    }
}

impl<'a> Display for WithName<'a, Trait> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} trait {} {{ ", self.value.access_modifier, self.name)?;
        for (name, member) in &self.value.trait_members {
            match member {
                TraitMember::Function(accessibility) => {
                    write!(f, "{accessibility} function {name}();")?;
                }
                TraitMember::Constant(accessibility) => {
                    write!(f, "{accessibility} const {name}: int32;")?;
                }
                TraitMember::Type(accessibility) => {
                    write!(f, "{accessibility} type {name};")?;
                }
            }
        }

        write!(f, "}}")?;

        Ok(())
    }
}

impl<'a> Display for WithName<'a, Function> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} function {}() {{}}",
            self.value.access_modifier, self.name
        )
    }
}

impl<'a> Display for WithName<'a, Type> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} type {} = bool;", self.value.access_modifier, self.name)
    }
}

impl<'a> Display for WithName<'a, Struct> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} struct {} {{}}", self.value.access_modifier, self.name)
    }
}

impl<'a> Display for WithName<'a, Enum> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} enum {} {{ ", self.value.access_modifier, self.name)?;
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
            self.value.access_modifier, self.name
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
        for (name, item) in &self.0.items {
            write!(f, "{}", WithName { name, value: item })?;
        }

        Ok(())
    }
}

#[derive(Debug, derive_more::From)]
pub enum ParseTargetError {
    #[allow(dead_code)]
    Target(target::Error),
    #[allow(dead_code)]
    Syntax(pernixc_syntax::error::Error),
    #[allow(dead_code)]
    Lexical(pernixc_lexical::error::Error),
}

const ROOT_MODULE_NAME: &str = "test";

fn property_based_testing_impl(module: &Module) -> TestCaseResult {
    let source_file = Arc::new(SourceFile::new(
        RootModule(module).to_string(),
        "test".into(),
    ));

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
        draft_table(std::iter::once(target), &handler_adapter)?,
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

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        cases: 2048,
        max_shrink_iters: 100_000,
        ..Default::default()
    })]

    #[test]
    fn property_based_testing(
        mut module in Module::arbitrary()
    ) {
        module.access_modifier = AccessModifier::Public;
        property_based_testing_impl(&module)?;
    }
}
