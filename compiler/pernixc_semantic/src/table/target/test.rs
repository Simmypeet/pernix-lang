use pernixc_base::handler;

use crate::{
    component::{Accessibility, Member, Name, Parent},
    diagnostic,
    table::{
        diagnostic::{ItemRedifinition, SymbolIsMoreAccessibleThanParent},
        GlobalID, Table, ID,
    },
    test::build_target,
};

const SUBMODULE: &str = r#"
public module first {}
private module second {
    private module third {}
}
"#;

#[test]
fn submodule() {
    let target = build_target(SUBMODULE);
    let mut table = Table::default();

    table
        .representation
        .add_target(
            "test".to_string(),
            std::iter::empty(),
            target,
            &handler::Panic,
        )
        .unwrap();

    let root_module_id = table.get_by_qualified_name(["test"]).unwrap();

    assert_eq!(root_module_id.id, ID::ROOT_MODULE);
    assert_eq!(
        table.get_component::<Accessibility>(root_module_id).as_deref(),
        Some(&Accessibility::Public)
    );
    assert!(table.get_component::<Parent>(root_module_id).is_none());

    let root_members = table.get_component::<Member>(root_module_id).unwrap();

    assert_eq!(root_members.len(), 2);

    let first_module_id = GlobalID::new(
        root_module_id.target_id,
        root_members.get("first").copied().unwrap(),
    );
    let second_module_id = GlobalID::new(
        root_module_id.target_id,
        root_members.get("second").copied().unwrap(),
    );

    assert_eq!(
        first_module_id,
        table.get_by_qualified_name(["test", "first"]).unwrap()
    );
    assert_eq!(
        second_module_id,
        table.get_by_qualified_name(["test", "second"]).unwrap()
    );

    assert_eq!(
        table.get_component::<Accessibility>(first_module_id).as_deref(),
        Some(&Accessibility::Public)
    );

    assert_eq!(
        table.get_component::<Accessibility>(second_module_id).as_deref(),
        Some(&Accessibility::Scoped(root_module_id.id))
    );

    assert_eq!(
        table.get_component::<Parent>(second_module_id).as_deref(),
        Some(&Parent(root_module_id.id))
    );
    assert_eq!(
        table.get_component::<Parent>(first_module_id).as_deref(),
        Some(&Parent(root_module_id.id))
    );

    let second_members =
        table.get_component::<Member>(second_module_id).unwrap();

    assert_eq!(second_members.len(), 1);

    let third_module_id = GlobalID::new(
        root_module_id.target_id,
        second_members.get("third").copied().unwrap(),
    );

    assert_eq!(
        third_module_id,
        table.get_by_qualified_name(["test", "second", "third"]).unwrap()
    );
    assert_eq!(
        table.get_component::<Accessibility>(third_module_id).as_deref(),
        Some(&Accessibility::Scoped(second_module_id.id))
    );
}

const TRAIT_WITH_ERRORS: &str = r#"
internal trait Test {
    public function first(); // <- too accessible
    private function second();
    private type second;
}
"#;

#[test]
fn trait_with_errors() {
    let target = build_target(TRAIT_WITH_ERRORS);
    let mut table = Table::default();

    let storage = handler::Storage::<Box<dyn diagnostic::Diagnostic>>::new();

    table
        .representation
        .add_target("test".to_string(), std::iter::empty(), target, &storage)
        .unwrap();

    let trait_symbol = table.get_by_qualified_name(["test", "Test"]).unwrap();
    let first_trait_symbol =
        table.get_by_qualified_name(["test", "Test", "first"]).unwrap();

    let errors = storage.into_vec();

    assert_eq!(errors.len(), 2);

    assert!(errors.iter().any(|x| {
        x.as_any().downcast_ref::<SymbolIsMoreAccessibleThanParent>().map_or(
            false,
            |x| {
                x.parent_id == trait_symbol && x.symbol_id == first_trait_symbol
            },
        )
    }));
    assert!(errors.iter().any(|x| {
        x.as_any().downcast_ref::<ItemRedifinition>().map_or(false, |x| {
            let existing_name = table
                .get_component::<Name>(GlobalID::new(
                    x.in_id.target_id,
                    x.existing_id,
                ))
                .map_or(false, |x| x.as_str() == "second");
            let new_name = table
                .get_component::<Name>(GlobalID::new(
                    x.in_id.target_id,
                    x.new_id,
                ))
                .map_or(false, |x| x.as_str() == "second");

            existing_name && new_name && x.in_id == trait_symbol
        })
    }))
}

const SERIALIZATION: &str = r#"
public module submodule {}
public trait Trait {
    public function first();
    private function second();
}
public type TypeAlias = int32;
public struct Struct {
    public field: int32,
}
public enum Option[T] {
    Some(T),
    None,
}
public const VALUE: int32 = 42;
public marker Marker[T];
extern "C" {
    public function externalFunction();
}
"#;

#[test]
fn serialization() {
    let target = build_target(SERIALIZATION);
    let mut table = Table::default();

    table
        .representation
        .add_target(
            "test".to_string(),
            std::iter::empty(),
            target,
            &handler::Panic,
        )
        .unwrap();

    let ron = ron::ser::to_string_pretty(&table, Default::default()).unwrap();

    let deserialized: Table = ron::de::from_str(&ron).unwrap();

    println!("{ron}");

    // do brief check
    assert!(deserialized
        .get_by_qualified_name(["test", "submodule"])
        .is_some());
    assert!(deserialized.get_by_qualified_name(["test", "Trait"]).is_some());
    assert!(deserialized
        .get_by_qualified_name(["test", "Trait", "first"])
        .is_some());
    assert!(deserialized
        .get_by_qualified_name(["test", "Trait", "second"])
        .is_some());
    assert!(deserialized
        .get_by_qualified_name(["test", "TypeAlias"])
        .is_some());
    assert!(deserialized.get_by_qualified_name(["test", "Struct"]).is_some());
    assert!(deserialized
        .get_by_qualified_name(["test", "Option", "Some"])
        .is_some());
    assert!(deserialized
        .get_by_qualified_name(["test", "Option", "None"])
        .is_some());
    assert!(deserialized.get_by_qualified_name(["test", "VALUE"]).is_some());
    assert!(deserialized.get_by_qualified_name(["test", "Marker"]).is_some());
    assert!(deserialized
        .get_by_qualified_name(["test", "externalFunction"])
        .is_some());
}
