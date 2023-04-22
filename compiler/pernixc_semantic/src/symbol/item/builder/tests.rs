use std::{error::Error, path::PathBuf};

use pernixc_common::source_file::SourceFile;

use super::Builder;
use crate::symbol::item::{builder::SymbolState, AccessModifier};

#[test]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
fn module_test() -> Result<(), Box<dyn Error>> {
    // creates a new source file
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/symbol/moduleTest/main.pnx"),
        vec!["test".to_string()],
    )?;

    // creates a target parsing
    let target_parsing = pernixc_syntax::target_parsing::parse_target(source_file)?;

    let mut builder = Builder::new();
    builder.generate_modules(&target_parsing);

    let test = builder
        .table
        .get_id_by_full_name(std::iter::once("test"))
        .unwrap()
        .into_module()
        .unwrap();

    let test_sub = builder
        .table
        .get_id_by_full_name(["test", "sub"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();

    let test_sub_inner = builder
        .table
        .get_id_by_full_name(["test", "sub", "inner"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();

    let test_sub_another_inner = builder
        .table
        .get_id_by_full_name(["test", "sub", "anotherInner"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();

    assert_eq!(
        builder.table[test].children_ids_by_name.get("sub").copied(),
        Some(test_sub.into())
    );
    assert!(builder.table[test].parent.is_none());
    assert_eq!(builder.table[test].access_modifier, AccessModifier::Public);
    assert_eq!(builder.table[test].qualified_name, vec!["test"]);

    assert_eq!(
        builder.table[test_sub]
            .children_ids_by_name
            .get("inner")
            .copied(),
        Some(test_sub_inner.into())
    );
    assert_eq!(
        builder.table[test_sub]
            .children_ids_by_name
            .get("anotherInner")
            .copied(),
        Some(test_sub_another_inner.into())
    );
    assert_eq!(builder.table[test_sub].parent, Some(test));
    assert_eq!(
        builder.table[test_sub].access_modifier,
        AccessModifier::Public
    );
    assert_eq!(builder.table[test_sub].qualified_name, vec!["test", "sub"]);

    assert!(builder.table[test_sub_inner]
        .children_ids_by_name
        .is_empty());
    assert_eq!(builder.table[test_sub_inner].parent, Some(test_sub));
    assert_eq!(
        builder.table[test_sub_inner].access_modifier,
        AccessModifier::Private
    );
    assert_eq!(builder.table[test_sub_inner].qualified_name, vec![
        "test", "sub", "inner"
    ]);

    assert!(builder.table[test_sub_another_inner]
        .children_ids_by_name
        .is_empty());
    assert_eq!(builder.table[test_sub_another_inner].parent, Some(test_sub));
    assert_eq!(
        builder.table[test_sub_another_inner].access_modifier,
        AccessModifier::Internal
    );
    assert_eq!(builder.table[test_sub_another_inner].qualified_name, vec![
        "test",
        "sub",
        "anotherInner"
    ]);

    Ok(())
}

#[test]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
fn symbol_test() -> Result<(), Box<dyn Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/symbol/symbolTest/main.pnx"),
        vec!["test".to_string()],
    )?;

    let target_parsing = pernixc_syntax::target_parsing::parse_target(source_file)?;
    let mut builder = Builder::new();
    builder.generate_modules(&target_parsing);
    builder.generate_symbols(target_parsing);

    let test_module = builder
        .table
        .get_id_by_full_name(std::iter::once("test"))
        .unwrap()
        .into_module()
        .unwrap();

    // enum Days
    {
        let enum_symbol = builder
            .table
            .get_id_by_full_name(["test", "Days"].into_iter())
            .unwrap()
            .into_enum()
            .unwrap();

        let enum_symbol_in_module = builder.table[test_module]
            .children_ids_by_name
            .get("Days")
            .unwrap()
            .into_enum()
            .unwrap();

        assert_eq!(enum_symbol, enum_symbol_in_module);
        assert_eq!(builder.table[enum_symbol].qualified_name, vec![
            "test", "Days"
        ]);
        assert_eq!(builder.table[enum_symbol].parent, test_module);
        assert_eq!(
            builder.table[enum_symbol].access_modifier,
            AccessModifier::Public
        );
        assert_eq!(
            *builder.symbol_states.get(&enum_symbol.into()).unwrap(),
            SymbolState::Unconstructed
        );
    }

    // struct Vector2
    {
        let struct_symbol = builder
            .table
            .get_id_by_full_name(["test", "Vector2"].into_iter())
            .unwrap()
            .into_struct()
            .unwrap();

        let struct_symbol_in_module = builder.table[test_module]
            .children_ids_by_name
            .get("Vector2")
            .unwrap()
            .into_struct()
            .unwrap();

        assert_eq!(struct_symbol, struct_symbol_in_module);
        assert_eq!(builder.table[struct_symbol].qualified_name, vec![
            "test", "Vector2"
        ]);
        assert_eq!(builder.table[struct_symbol].parent, test_module);
        assert_eq!(
            builder.table[struct_symbol].access_modifier,
            AccessModifier::Internal
        );
        assert_eq!(
            *builder.symbol_states.get(&struct_symbol.into()).unwrap(),
            SymbolState::Unconstructed
        );
    }

    // type FloatingPoint
    {
        let type_symbol = builder
            .table
            .get_id_by_full_name(["test", "FloatingPoint"].into_iter())
            .unwrap()
            .into_type_alias()
            .unwrap();

        let type_symbol_in_module = builder.table[test_module]
            .children_ids_by_name
            .get("FloatingPoint")
            .unwrap()
            .into_type_alias()
            .unwrap();

        assert_eq!(type_symbol, type_symbol_in_module);
        assert_eq!(builder.table[type_symbol].qualified_name, vec![
            "test",
            "FloatingPoint"
        ]);
        assert_eq!(builder.table[type_symbol].parent, test_module);
        assert_eq!(
            builder.table[type_symbol].access_modifier,
            AccessModifier::Public
        );
        assert_eq!(
            *builder.symbol_states.get(&type_symbol.into()).unwrap(),
            SymbolState::Unconstructed
        );
    }

    // function add(..) {..}
    {
        let function_symbol = builder
            .table
            .get_id_by_full_name(["test", "add"].into_iter())
            .unwrap()
            .into_function_overload_set()
            .unwrap();

        let function_symbol_in_module = builder.table[test_module]
            .children_ids_by_name
            .get("add")
            .unwrap()
            .into_function_overload_set()
            .unwrap();

        assert_eq!(function_symbol, function_symbol_in_module);
        assert_eq!(builder.table[function_symbol].qualified_name, vec![
            "test", "add"
        ]);
        assert_eq!(builder.table[function_symbol].parent, test_module);
        assert_eq!(
            *builder.symbol_states.get(&function_symbol.into()).unwrap(),
            SymbolState::Unconstructed
        );
        assert_eq!(builder.table[function_symbol].overloads_by_id.len(), 2);
    }

    // function sub(..) {..}
    {
        let function_symbol = builder
            .table
            .get_id_by_full_name(["test", "sub"].into_iter())
            .unwrap()
            .into_function_overload_set()
            .unwrap();

        let function_symbol_in_module = builder.table[test_module]
            .children_ids_by_name
            .get("sub")
            .unwrap()
            .into_function_overload_set()
            .unwrap();

        assert_eq!(function_symbol, function_symbol_in_module);
        assert_eq!(builder.table[function_symbol].qualified_name, vec![
            "test", "sub"
        ]);
        assert_eq!(builder.table[function_symbol].parent, test_module);
        assert_eq!(
            *builder.symbol_states.get(&function_symbol.into()).unwrap(),
            SymbolState::Unconstructed
        );
        assert_eq!(builder.table[function_symbol].overloads_by_id.len(), 2);
    }

    Ok(())
}
