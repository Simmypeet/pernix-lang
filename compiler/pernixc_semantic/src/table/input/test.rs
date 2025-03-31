use std::sync::Arc;

use pernixc_syntax::utility::build_target;
use ron::ser::PrettyConfig;
use serde::de::DeserializeSeed;

use crate::{
    component::input::{Accessibility, Import, Member, Name, Parent},
    diagnostic::Diagnostic,
    table::{
        input::diagnostic::{
            AccessModifierIsNotAllowedInTraitImplementation, ConflictingUsing,
            ItemRedifinition, MismatchedTraitMemberAndImplementationMember,
            SymbolIsMoreAccessibleThanParent, TraitMemberKind,
            UnknownTraitImplementationMember,
        },
        resolution::diagnostic::{SymbolIsNotAccessible, SymbolNotFound},
        CompilationMetaData, GlobalID, Table, ID,
    },
};

const SUBMODULE: &str = r"
public module first:
    pass


private module second:
    private module third:
        pass
";

#[test]
fn submodule() {
    let target = build_target(SUBMODULE);
    let mut table = Table::new(Arc::new(pernixc_handler::Panic));

    table
        .representation
        .add_compilation_target_input(
            "test".to_string(),
            std::iter::empty(),
            target,
            &pernixc_handler::Panic,
        )
        .unwrap();

    let root_module_id = table.get_by_qualified_name(["test"]).unwrap();

    assert_eq!(root_module_id.id, ID::ROOT_MODULE);
    assert_eq!(
        table.storage.get::<Accessibility>(root_module_id).as_deref(),
        Some(&Accessibility::Public)
    );
    assert!(table.get::<Parent>(root_module_id).is_none());

    let root_members = table.storage.get::<Member>(root_module_id).unwrap();

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
        table.storage.get::<Accessibility>(first_module_id).as_deref(),
        Some(&Accessibility::Public)
    );

    assert_eq!(
        table.storage.get::<Accessibility>(second_module_id).as_deref(),
        Some(&Accessibility::Scoped(root_module_id.id))
    );

    assert_eq!(
        table.storage.get::<Parent>(second_module_id).as_deref(),
        Some(&Parent(Some(root_module_id.id)))
    );
    assert_eq!(
        table.storage.get::<Parent>(first_module_id).as_deref(),
        Some(&Parent(Some(root_module_id.id)))
    );

    let second_members = table.storage.get::<Member>(second_module_id).unwrap();

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
        table.storage.get::<Accessibility>(third_module_id).as_deref(),
        Some(&Accessibility::Scoped(second_module_id.id))
    );
}

const TRAIT_WITH_ERRORS: &str = r"
internal trait Test:
    public function first() // <- too accessible
    private function second()
    private type second
";

#[test]
#[allow(clippy::significant_drop_tightening)]
fn trait_with_errors() {
    let target = build_target(TRAIT_WITH_ERRORS);
    let storage =
        Arc::new(pernixc_handler::Storage::<Box<dyn Diagnostic>>::new());

    let mut table = Table::new(storage.clone());

    table
        .representation
        .add_compilation_target_input(
            "test".to_string(),
            std::iter::empty(),
            target,
            &*storage,
        )
        .unwrap();

    let trait_symbol = table.get_by_qualified_name(["test", "Test"]).unwrap();
    let first_trait_symbol =
        table.get_by_qualified_name(["test", "Test", "first"]).unwrap();

    let errors = storage.as_vec();

    assert_eq!(errors.len(), 2);

    assert!(errors.iter().any(|x| {
        x.as_any()
            .downcast_ref::<SymbolIsMoreAccessibleThanParent>()
            .is_some_and(|x| {
                x.parent_id == trait_symbol && x.symbol_id == first_trait_symbol
            })
    }));
    assert!(errors.iter().any(|x| {
        x.as_any().downcast_ref::<ItemRedifinition>().is_some_and(|x| {
            let existing_name = table
                .storage
                .get::<Name>(x.existing_id)
                .is_some_and(|x| x.as_str() == "second");
            let new_name = table
                .storage
                .get::<Name>(x.new_id)
                .is_some_and(|x| x.as_str() == "second");

            existing_name && new_name && x.in_id == trait_symbol
        })
    }));
}

const SERIALIZATION: &str = r#"
public module submodule:
    pass


public trait Trait:
    public function first()
    private function second()


public type TypeAlias = int32


public struct Struct:
    public field: int32


public enum Option[T]:
    Some(T)
    None


public const VALUE: int32 = 42


public marker Marker[T]


extern "C":
    public function externalFunction()
"#;

#[test]
fn serialization() {
    let target = build_target(SERIALIZATION);
    let storage = Arc::new(pernixc_handler::Panic);

    let mut table = Table::new(storage.clone());

    let target_id = table
        .representation
        .add_compilation_target_input(
            "test".to_string(),
            std::iter::empty(),
            target,
            &*storage,
        )
        .unwrap();

    let reflector = Table::input_reflector();
    let compilation_meta_data = CompilationMetaData { target_id };

    let ron = ron::ser::to_string_pretty(
        &table.as_library(&compilation_meta_data, &reflector),
        PrettyConfig::default(),
    )
    .unwrap();
    let mut deserialized = Table::new(storage.clone());

    let deserialized_compilation_meta_data = deserialized
        .as_incremental_library_deserializer(&reflector)
        .deserialize(&mut ron::de::Deserializer::from_str(&ron).unwrap())
        .unwrap();

    assert_eq!(deserialized_compilation_meta_data, compilation_meta_data);

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

const USINGS: &str = r"
from test::inner import (
    AnotherSymbol,
    Inaccessible,
    Symbol as Existing // collision
)

import test::anotherInner as something
import test::notFound // not found


public module inner:
    public type Symbol = int32
    public type AnotherSymbol = int32
    private type Inaccessible = int32 // inaccessible


public module anotherInner:
    pass


public struct Existing:
    pass
";

#[test]
#[allow(clippy::significant_drop_tightening)]
fn usings() {
    let target = build_target(USINGS);
    let storage =
        Arc::new(pernixc_handler::Storage::<Box<dyn Diagnostic>>::new());

    let mut table = Table::new(storage.clone());
    table
        .representation
        .add_compilation_target_input(
            "test".to_string(),
            std::iter::empty(),
            target,
            &*storage,
        )
        .unwrap();

    let errors = storage.as_vec();

    assert_eq!(errors.len(), 3);

    let root_module = table.get_by_qualified_name(["test"]).unwrap();
    let inaccessible_symbol =
        table.get_by_qualified_name(["test", "inner", "Inaccessible"]).unwrap();

    assert!(errors.iter().any(|x| {
        x.as_any().downcast_ref::<SymbolIsNotAccessible>().is_some_and(
            |error| {
                error.referring_site == root_module
                    && error.referred == inaccessible_symbol
            },
        )
    }));
    assert!(errors.iter().any(|x| {
        x.as_any().downcast_ref::<ConflictingUsing>().is_some_and(|x| {
            x.module_id == root_module
                && x.name == "Existing"
                && x.conflicting_span
                    .as_ref()
                    .is_some_and(|x| x.str() == "Existing")
        })
    }));
    assert!(errors.iter().any(|x| {
        x.as_any().downcast_ref::<SymbolNotFound>().is_some_and(|x| {
            x.searched_item_id == Some(root_module)
                && x.resolution_span.str() == "notFound"
        })
    }));

    let import = table.storage.get::<Import>(root_module).unwrap();

    assert_eq!(import.len(), 3);

    assert!(import.contains_key("AnotherSymbol"));
    assert!(import.contains_key("Inaccessible"));
    assert!(import.contains_key("something"));
}

const TRAIT_IMPLEMENTATIONS: &str = r"
public trait Trait[T]:
    public function someFunction()
    public type SomeType
    public const SOME_VALUE: int32


final const implements Trait[int32]:
    type someFunction = bool // <- not a function
    type SomeType = int32
    internal const SOME_VALUE: int32 = 42
    function unknownFunction():  // <- extraneous
        pass

";

#[test]
#[allow(clippy::significant_drop_tightening)]
fn trait_implementations() {
    let target = build_target(TRAIT_IMPLEMENTATIONS);

    let storage =
        Arc::new(pernixc_handler::Storage::<Box<dyn Diagnostic>>::new());

    let mut table = Table::new(storage.clone());
    table
        .representation
        .add_compilation_target_input(
            "test".to_string(),
            std::iter::empty(),
            target,
            &*storage,
        )
        .unwrap();

    let errors = storage.as_vec();

    assert_eq!(errors.len(), 3);

    let trait_id = table.get_by_qualified_name(["test", "Trait"]).unwrap();

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<MismatchedTraitMemberAndImplementationMember>()
        .is_some_and(|x| {
            x.found_kind == TraitMemberKind::Type
                && x.implementation_member_identifer_span.str()
                    == "someFunction"
        })));
    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<UnknownTraitImplementationMember>()
        .is_some_and(|x| {
            x.trait_id == trait_id
                && x.identifier_span.str() == "unknownFunction"
        })));
    assert!(errors.iter().any(|x| {
        x.as_any()
            .downcast_ref::<AccessModifierIsNotAllowedInTraitImplementation>()
            .is_some_and(|x| x.access_modifier_span.str() == "internal")
    }));
}

const ADT_IMPLEMENTATION: &str = r"
public struct Test:
    pass


implements Test:
    public function first():
        pass


implements Test:
    public function first():
        pass
";

#[test]
#[allow(clippy::significant_drop_tightening)]
fn adt_implementation() {
    let target = build_target(ADT_IMPLEMENTATION);

    let storage =
        Arc::new(pernixc_handler::Storage::<Box<dyn Diagnostic>>::new());

    let mut table = Table::new(storage.clone());
    table
        .representation
        .add_compilation_target_input(
            "test".to_string(),
            std::iter::empty(),
            target,
            &*storage,
        )
        .unwrap();

    let errors = storage.as_vec();

    assert_eq!(errors.len(), 1);
    assert!(errors
        .iter()
        .any(|x| x.as_any().downcast_ref::<ItemRedifinition>().is_some()));
}
