use std::{collections::HashMap, error::Error, path::PathBuf};

use pernixc_common::source_file::SourceFile;

use crate::symbol::{
    errors::SymbolError,
    table::{SymbolState, Table},
    ty::PrimitiveType,
    Accessibility,
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
        .get_global_id_by_full_name(std::iter::once("test"))
        .unwrap()
        .into_module()
        .unwrap();

    let test_sub = table
        .get_global_id_by_full_name(["test", "sub"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();

    let test_sub_inner = table
        .get_global_id_by_full_name(["test", "sub", "inner"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();

    let test_sub_another_inner = table
        .get_global_id_by_full_name(["test", "sub", "anotherInner"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();

    assert_eq!(
        table[test].child_ids_by_name.get("sub").copied(),
        Some(test_sub.into())
    );
    assert!(table[test].parent_module_id.is_none());
    assert_eq!(table[test].accessibility, Accessibility::Public);
    assert_eq!(table.get_full_name_of(test), "test");

    assert_eq!(
        table[test_sub].child_ids_by_name.get("inner").copied(),
        Some(test_sub_inner.into())
    );
    assert_eq!(
        table[test_sub]
            .child_ids_by_name
            .get("anotherInner")
            .copied(),
        Some(test_sub_another_inner.into())
    );
    assert_eq!(table[test_sub].parent_module_id, Some(test));
    assert_eq!(table[test_sub].accessibility, Accessibility::Public);
    assert_eq!(table.get_full_name_of(test_sub), "test::sub");

    assert!(table[test_sub_inner].child_ids_by_name.is_empty());
    assert_eq!(table[test_sub_inner].parent_module_id, Some(test_sub));
    assert_eq!(table[test_sub_inner].accessibility, Accessibility::Private);
    assert_eq!(table.get_full_name_of(test_sub_inner), "test::sub::inner");

    assert!(table[test_sub_another_inner].child_ids_by_name.is_empty());
    assert_eq!(
        table[test_sub_another_inner].parent_module_id,
        Some(test_sub)
    );
    assert_eq!(
        table[test_sub_another_inner].accessibility,
        Accessibility::Internal
    );
    assert_eq!(
        table.get_full_name_of(test_sub_another_inner),
        "test::sub::anotherInner"
    );

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
    table.draft_symbols(target_parsing, &mut errors, &mut symbol_states_by_id);

    let test_module = table
        .get_global_id_by_full_name(std::iter::once("test"))
        .unwrap()
        .into_module()
        .unwrap();

    // enum Days
    {
        let enum_symbol = table
            .get_global_id_by_full_name(["test", "Days"].into_iter())
            .unwrap()
            .into_enum()
            .unwrap();

        let enum_symbol_in_module = table[test_module]
            .child_ids_by_name
            .get("Days")
            .unwrap()
            .into_enum()
            .unwrap();

        assert_eq!(enum_symbol, enum_symbol_in_module);
        assert_eq!(table.get_full_name_of(enum_symbol), "test::Days");
        assert_eq!(table[enum_symbol].parent_module_id, test_module);
        assert_eq!(table[enum_symbol].accessibility, Accessibility::Public);
    }

    // struct Vector2
    {
        let struct_symbol = table
            .get_global_id_by_full_name(["test", "Vector2"].into_iter())
            .unwrap()
            .into_struct()
            .unwrap();

        let struct_symbol_in_module = table[test_module]
            .child_ids_by_name
            .get("Vector2")
            .unwrap()
            .into_struct()
            .unwrap();

        assert_eq!(struct_symbol, struct_symbol_in_module);
        assert_eq!(table.get_full_name_of(struct_symbol), "test::Vector2");
        assert_eq!(table[struct_symbol].parent_module_id, test_module);
        assert_eq!(table[struct_symbol].accessibility, Accessibility::Internal);
    }

    // type FloatingPoint
    {
        let type_symbol = table
            .get_global_id_by_full_name(["test", "FloatingPoint"].into_iter())
            .unwrap()
            .into_type_alias()
            .unwrap();

        let type_symbol_in_module = table[test_module]
            .child_ids_by_name
            .get("FloatingPoint")
            .unwrap()
            .into_type_alias()
            .unwrap();

        assert_eq!(type_symbol, type_symbol_in_module);
        assert_eq!(table.get_full_name_of(type_symbol), "test::FloatingPoint");
        assert_eq!(table[type_symbol].type_alias_parent_id, test_module.into());
        assert_eq!(table[type_symbol].accessibility, Accessibility::Public);
        assert_eq!(
            *symbol_states_by_id.get(&type_symbol.into()).unwrap(),
            SymbolState::Drafted
        );
    }

    // function add(..) {..}
    {
        let function_symbol = table
            .get_global_id_by_full_name(["test", "add"].into_iter())
            .unwrap()
            .into_overload_set()
            .unwrap();

        let function_symbol_in_module = table[test_module]
            .child_ids_by_name
            .get("add")
            .unwrap()
            .into_overload_set()
            .unwrap();

        assert_eq!(function_symbol, function_symbol_in_module);
        assert_eq!(table.get_full_name_of(function_symbol), "test::add");
        assert_eq!(table[function_symbol].parent_module_id, test_module);
        assert_eq!(
            *symbol_states_by_id.get(&function_symbol.into()).unwrap(),
            SymbolState::Drafted
        );
        assert_eq!(table[function_symbol].overloads.len(), 2);
    }

    // function sub(..) {..}
    {
        let function_symbol = table
            .get_global_id_by_full_name(["test", "sub"].into_iter())
            .unwrap()
            .into_overload_set()
            .unwrap();

        let function_symbol_in_module = table[test_module]
            .child_ids_by_name
            .get("sub")
            .unwrap()
            .into_overload_set()
            .unwrap();

        assert_eq!(function_symbol, function_symbol_in_module);
        assert_eq!(table.get_full_name_of(function_symbol), "test::sub");
        assert_eq!(table[function_symbol].parent_module_id, test_module);
        assert_eq!(
            *symbol_states_by_id.get(&function_symbol.into()).unwrap(),
            SymbolState::Drafted
        );
        assert_eq!(table[function_symbol].overloads.len(), 2);
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
    table.draft_symbols(file_parsing, &mut errors, &mut symbol_states_by_id);

    let test = table
        .get_global_id_by_full_name(std::iter::once("test"))
        .unwrap()
        .into_module()
        .unwrap();
    let test_first = table
        .get_global_id_by_full_name(["test", "first"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();
    let test_second = table
        .get_global_id_by_full_name(["test", "second"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();
    let test_first_private_struct = table
        .get_global_id_by_full_name(["test", "first", "PrivateStruct"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();
    let test_private_struct = table
        .get_global_id_by_full_name(["test", "PrivateStruct"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();
    let test_public_struct = table
        .get_global_id_by_full_name(["test", "PublicStruct"].into_iter())
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
pub fn construct_test() -> Result<(), Box<dyn Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/symbol/constructTest/main.pnx"),
        vec!["test".to_string()],
    )?;
    let (target_parsing, errors) = pernixc_syntax::target_parsing::parse_target(source_file)?;
    assert!(errors.is_empty());

    let (table, errors) = Table::analyze(target_parsing);
    assert_eq!(errors.len(), 2);

    // error check
    {
        {
            let circular_dependency_err = errors
                .iter()
                .find_map(SymbolError::as_circular_dependency)
                .unwrap();
            assert!(circular_dependency_err.symbol_ids().contains(
                &table
                    .get_global_id_by_full_name(["test", "B"].into_iter())
                    .unwrap()
                    .into(),
            ));
            assert!(circular_dependency_err.symbol_ids().contains(
                &table
                    .get_global_id_by_full_name(["test", "C"].into_iter())
                    .unwrap()
                    .into()
            ));
        }
        {
            let overload_redefinition_err = errors
                .iter()
                .find_map(SymbolError::as_overload_redefinition)
                .unwrap();
            assert_eq!(
                table.get_full_name_of(
                    table[overload_redefinition_err.available_overload_id].parent_overload_set_id
                ),
                "test::add"
            );
        }
    }

    // type alias check
    {
        assert_eq!(
            table[table
                .get_global_id_by_full_name(["test", "A"].into_iter())
                .unwrap()
                .into_type_alias()
                .unwrap()]
            .alias,
            PrimitiveType::Float64.into()
        );
        assert_eq!(
            table[table
                .get_global_id_by_full_name(["test", "SomeStruct", "A"].into_iter())
                .unwrap()
                .into_type_alias()
                .unwrap()]
            .alias,
            PrimitiveType::Float32.into()
        );
        assert_eq!(
            table[table
                .get_global_id_by_full_name(["test", "SomeStruct", "B"].into_iter())
                .unwrap()
                .into_type_alias()
                .unwrap()]
            .alias,
            PrimitiveType::Float32.into()
        );
    }

    // SomeStruct field check
    {
        let some_struct = &table[table
            .get_global_id_by_full_name(["test", "SomeStruct"].into_iter())
            .unwrap()
            .into_struct()
            .unwrap()];

        assert_eq!(some_struct.accessibility(), Accessibility::Public);
        assert_eq!(some_struct.field_order().len(), 3);

        let first_field = some_struct
            .field_ids_by_name()
            .get("first")
            .copied()
            .unwrap();
        assert_eq!(table[first_field].ty, PrimitiveType::Float32.into());

        let second_field = some_struct
            .field_ids_by_name()
            .get("second")
            .copied()
            .unwrap();
        assert_eq!(table[second_field].ty, PrimitiveType::Float32.into());

        let third_field = some_struct
            .field_ids_by_name()
            .get("third")
            .copied()
            .unwrap();
        assert_eq!(table[third_field].ty, PrimitiveType::Float64.into());
    }

    // Boolean enum check
    {
        let boolean_enum = &table[table
            .get_global_id_by_full_name(["test", "Boolean"].into_iter())
            .unwrap()
            .into_enum()
            .unwrap()];

        let true_variant = boolean_enum.variant_ids_by_name.get("True").unwrap();
        let false_variant = boolean_enum.variant_ids_by_name.get("False").unwrap();

        assert_eq!(boolean_enum.accessibility(), Accessibility::Public);

        assert_eq!(boolean_enum.variant_ids_by_name().len(), 2);
        assert_eq!(table[*true_variant].declaration_order, 0);
        assert_eq!(table[*false_variant].declaration_order, 1);
    }

    // add Overload check
    {
        let add_overload = &table[table
            .get_global_id_by_full_name(["test", "add"].into_iter())
            .unwrap()
            .into_overload_set()
            .unwrap()];

        assert_eq!(add_overload.overloads.len(), 2);

        assert!(add_overload.overloads.iter().any(|x| {
            let overload = &table[*x];

            let first_param = &table[overload.parameter_order()[0]].type_binding;
            let second_param = &table[overload.parameter_order()[1]].type_binding;
            let return_type = overload.return_type;

            !first_param.is_mutable
                && first_param.ty == PrimitiveType::Float32.into()
                && !second_param.is_mutable
                && second_param.ty == PrimitiveType::Float32.into()
                && return_type == PrimitiveType::Float32.into()
        }));
        assert!(add_overload.overloads.iter().any(|x| {
            let overload = &table[*x];

            let first_param = &table[overload.parameter_order()[0]].type_binding;
            let second_param = &table[overload.parameter_order()[1]].type_binding;
            let return_type = overload.return_type;

            !first_param.is_mutable
                && first_param.ty == PrimitiveType::Int32.into()
                && !second_param.is_mutable
                && second_param.ty == PrimitiveType::Int32.into()
                && (return_type == PrimitiveType::Float32.into()
                    || return_type == PrimitiveType::Int32.into())
        }));
    }

    Ok(())
}
