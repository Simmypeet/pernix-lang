use std::{collections::HashMap, error::Error, path::PathBuf};

use pernixc_common::source_file::SourceFile;

use crate::symbol::{
    item::{Accessibility, SymbolState, Table},
    ty::PrimitiveType,
};

#[test]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
fn module_test() -> Result<(), Box<dyn Error>> {
    // creates a new source file
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/symbol/moduleTest/main.pnx"),
        vec!["test".to_string()],
    )?;

    // creates a target parsing
    let (target_parsing, errors) = pernixc_syntax::target_parsing::parse_target(source_file)?;
    assert!(errors.is_empty());

    let mut table = Table::new();
    table.generate_modules(&target_parsing);

    let test = table
        .get_id_by_full_name(std::iter::once("test"))
        .unwrap()
        .into_module()
        .unwrap();

    let test_sub = table
        .get_id_by_full_name(["test", "sub"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();

    let test_sub_inner = table
        .get_id_by_full_name(["test", "sub", "inner"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();

    let test_sub_another_inner = table
        .get_id_by_full_name(["test", "sub", "anotherInner"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();

    assert_eq!(
        table[test].children_ids_by_name.get("sub").copied(),
        Some(test_sub.into())
    );
    assert!(table[test].parent.is_none());
    assert_eq!(table[test].accessibility, Accessibility::Public);
    assert_eq!(table[test].qualified_name, vec!["test"]);

    assert_eq!(
        table[test_sub].children_ids_by_name.get("inner").copied(),
        Some(test_sub_inner.into())
    );
    assert_eq!(
        table[test_sub]
            .children_ids_by_name
            .get("anotherInner")
            .copied(),
        Some(test_sub_another_inner.into())
    );
    assert_eq!(table[test_sub].parent, Some(test));
    assert_eq!(table[test_sub].accessibility, Accessibility::Public);
    assert_eq!(table[test_sub].qualified_name, vec!["test", "sub"]);

    assert!(table[test_sub_inner].children_ids_by_name.is_empty());
    assert_eq!(table[test_sub_inner].parent, Some(test_sub));
    assert_eq!(table[test_sub_inner].accessibility, Accessibility::Private);
    assert_eq!(table[test_sub_inner].qualified_name, vec![
        "test", "sub", "inner"
    ]);

    assert!(table[test_sub_another_inner]
        .children_ids_by_name
        .is_empty());
    assert_eq!(table[test_sub_another_inner].parent, Some(test_sub));
    assert_eq!(
        table[test_sub_another_inner].accessibility,
        Accessibility::Internal
    );
    assert_eq!(table[test_sub_another_inner].qualified_name, vec![
        "test",
        "sub",
        "anotherInner"
    ]);

    Ok(())
}

#[test]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
fn populate_test() -> Result<(), Box<dyn Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/symbol/populateTest/main.pnx"),
        vec!["test".to_string()],
    )?;

    let (target_parsing, errors) = pernixc_syntax::target_parsing::parse_target(source_file)?;
    assert!(errors.is_empty());

    let mut table = Table::new();
    let mut errors = Vec::new();
    let mut symbol_states_by_id = HashMap::new();
    table.generate_modules(&target_parsing);
    table.generate_symbols(target_parsing, &mut errors, &mut symbol_states_by_id);

    let test_module = table
        .get_id_by_full_name(std::iter::once("test"))
        .unwrap()
        .into_module()
        .unwrap();

    // enum Days
    {
        let enum_symbol = table
            .get_id_by_full_name(["test", "Days"].into_iter())
            .unwrap()
            .into_enum()
            .unwrap();

        let enum_symbol_in_module = table[test_module]
            .children_ids_by_name
            .get("Days")
            .unwrap()
            .into_enum()
            .unwrap();

        assert_eq!(enum_symbol, enum_symbol_in_module);
        assert_eq!(table[enum_symbol].qualified_name, vec!["test", "Days"]);
        assert_eq!(table[enum_symbol].parent, test_module);
        assert_eq!(table[enum_symbol].accessibility, Accessibility::Public);
        assert_eq!(
            *symbol_states_by_id.get(&enum_symbol.into()).unwrap(),
            SymbolState::Unconstructed
        );
    }

    // struct Vector2
    {
        let struct_symbol = table
            .get_id_by_full_name(["test", "Vector2"].into_iter())
            .unwrap()
            .into_struct()
            .unwrap();

        let struct_symbol_in_module = table[test_module]
            .children_ids_by_name
            .get("Vector2")
            .unwrap()
            .into_struct()
            .unwrap();

        assert_eq!(struct_symbol, struct_symbol_in_module);
        assert_eq!(table[struct_symbol].qualified_name, vec!["test", "Vector2"]);
        assert_eq!(table[struct_symbol].parent, test_module);
        assert_eq!(table[struct_symbol].accessibility, Accessibility::Internal);
        assert_eq!(
            *symbol_states_by_id.get(&struct_symbol.into()).unwrap(),
            SymbolState::Unconstructed
        );
    }

    // type FloatingPoint
    {
        let type_symbol = table
            .get_id_by_full_name(["test", "FloatingPoint"].into_iter())
            .unwrap()
            .into_type_alias()
            .unwrap();

        let type_symbol_in_module = table[test_module]
            .children_ids_by_name
            .get("FloatingPoint")
            .unwrap()
            .into_type_alias()
            .unwrap();

        assert_eq!(type_symbol, type_symbol_in_module);
        assert_eq!(table[type_symbol].qualified_name, vec![
            "test",
            "FloatingPoint"
        ]);
        assert_eq!(table[type_symbol].parent, test_module.into());
        assert_eq!(table[type_symbol].accessibility, Accessibility::Public);
        assert_eq!(
            *symbol_states_by_id.get(&type_symbol.into()).unwrap(),
            SymbolState::Unconstructed
        );
    }

    // function add(..) {..}
    {
        let function_symbol = table
            .get_id_by_full_name(["test", "add"].into_iter())
            .unwrap()
            .into_function_overload_set()
            .unwrap();

        let function_symbol_in_module = table[test_module]
            .children_ids_by_name
            .get("add")
            .unwrap()
            .into_function_overload_set()
            .unwrap();

        assert_eq!(function_symbol, function_symbol_in_module);
        assert_eq!(table[function_symbol].qualified_name, vec!["test", "add"]);
        assert_eq!(table[function_symbol].parent, test_module);
        assert_eq!(
            *symbol_states_by_id.get(&function_symbol.into()).unwrap(),
            SymbolState::Unconstructed
        );
        assert_eq!(table[function_symbol].overloads_by_id.len(), 2);
    }

    // function sub(..) {..}
    {
        let function_symbol = table
            .get_id_by_full_name(["test", "sub"].into_iter())
            .unwrap()
            .into_function_overload_set()
            .unwrap();

        let function_symbol_in_module = table[test_module]
            .children_ids_by_name
            .get("sub")
            .unwrap()
            .into_function_overload_set()
            .unwrap();

        assert_eq!(function_symbol, function_symbol_in_module);
        assert_eq!(table[function_symbol].qualified_name, vec!["test", "sub"]);
        assert_eq!(table[function_symbol].parent, test_module);
        assert_eq!(
            *symbol_states_by_id.get(&function_symbol.into()).unwrap(),
            SymbolState::Unconstructed
        );
        assert_eq!(table[function_symbol].overloads_by_id.len(), 2);
    }

    Ok(())
}

#[test]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
fn accessible_test() -> Result<(), Box<dyn Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/symbol/accessibleTest/main.pnx"),
        vec!["test".to_string()],
    )?;
    let (file_parsing, errors) = pernixc_syntax::target_parsing::parse_target(source_file)?;
    assert!(errors.is_empty());

    let mut table = Table::new();
    let mut errors = Vec::new();
    let mut symbol_states_by_id = HashMap::new();

    table.generate_modules(&file_parsing);
    table.generate_symbols(file_parsing, &mut errors, &mut symbol_states_by_id);

    let test = table
        .get_id_by_full_name(std::iter::once("test"))
        .unwrap()
        .into_module()
        .unwrap();
    let test_first = table
        .get_id_by_full_name(["test", "first"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();
    let test_second = table
        .get_id_by_full_name(["test", "second"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();
    let test_first_private_struct = table
        .get_id_by_full_name(["test", "first", "PrivateStruct"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();
    let test_private_struct = table
        .get_id_by_full_name(["test", "PrivateStruct"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();
    let test_public_struct = table
        .get_id_by_full_name(["test", "PublicStruct"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();

    assert!(table.symbol_accessible(
        test.into(),
        test_private_struct.into(),
        table[test_private_struct].accessibility
    ));
    assert!(table.symbol_accessible(
        test.into(),
        test_public_struct.into(),
        table[test_public_struct].accessibility
    ));
    assert!(!table.symbol_accessible(
        test.into(),
        test_first_private_struct.into(),
        table[test_first_private_struct].accessibility
    ));
    assert!(table.symbol_accessible(
        test_first.into(),
        test_private_struct.into(),
        table[test_private_struct].accessibility
    ));
    assert!(!table.symbol_accessible(
        test_second.into(),
        test_first_private_struct.into(),
        table[test_first_private_struct].accessibility
    ));

    Ok(())
}

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn build_test() -> Result<(), Box<dyn Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/symbol/buildTest/main.pnx"),
        vec!["test".to_string()],
    )?;
    let (target_parsing, errors) = pernixc_syntax::target_parsing::parse_target(source_file)?;
    assert!(errors.is_empty());

    let (table, errors) = Table::analyze(target_parsing);
    assert_eq!(errors.len(), 1);

    // error check
    {
        let circular_dependency_err = errors[0].as_circular_dependency().unwrap();
        assert!(circular_dependency_err.symbol_ids().contains(
            &table
                .get_id_by_full_name(["test", "B"].into_iter())
                .unwrap(),
        ));
        assert!(circular_dependency_err.symbol_ids().contains(
            &table
                .get_id_by_full_name(["test", "C"].into_iter())
                .unwrap()
        ));
    }

    // type alias check
    {
        assert_eq!(
            table[table
                .get_id_by_full_name(["test", "A"].into_iter())
                .unwrap()]
            .as_type_alias()
            .unwrap()
            .ty(),
            PrimitiveType::Float64.into()
        );
        assert_eq!(
            table[table
                .get_id_by_full_name(["test", "SomeStruct", "A"].into_iter())
                .unwrap()]
            .as_type_alias()
            .unwrap()
            .ty(),
            PrimitiveType::Float32.into()
        );
        assert_eq!(
            table[table
                .get_id_by_full_name(["test", "SomeStruct", "B"].into_iter())
                .unwrap()]
            .as_type_alias()
            .unwrap()
            .ty(),
            PrimitiveType::Float32.into()
        );
    }

    // SomeStruct field check
    {
        let some_struct = table[table
            .get_id_by_full_name(["test", "SomeStruct"].into_iter())
            .unwrap()]
        .as_struct()
        .unwrap();

        assert_eq!(some_struct.accessibility(), Accessibility::Public);
        assert_eq!(some_struct.field_member_map().len(), 3);

        let first_field = some_struct
            .field_member_map()
            .map_name_to_id("first")
            .unwrap();
        assert_eq!(
            some_struct.field_member_map()[first_field].ty,
            PrimitiveType::Float32.into()
        );

        let second_field = some_struct
            .field_member_map()
            .map_name_to_id("second")
            .unwrap();
        assert_eq!(
            some_struct.field_member_map()[second_field].ty,
            PrimitiveType::Float32.into()
        );

        let third_field = some_struct
            .field_member_map()
            .map_name_to_id("third")
            .unwrap();
        assert_eq!(
            some_struct.field_member_map()[third_field].ty,
            PrimitiveType::Float64.into()
        );
    }

    Ok(())
}
