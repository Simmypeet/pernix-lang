use std::{collections::HashMap, path::PathBuf};

use pernixc_source::SourceFile;
use pernixc_syntax::target_parsing::AllParsingError;
use pernixc_system::error_handler::ErrorVec;

use crate::symbol::{
    error::Error,
    table::{SymbolState, Table},
    ty::PrimitiveType,
    Accessibility,
};

#[test]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
fn module_test() -> Result<(), Box<dyn std::error::Error>> {
    // creates a new source file
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/symbol/moduleTest/main.pnx"),
        vec!["test".to_string()],
    )?;

    // creates a target parsing
    let error_vec: ErrorVec<AllParsingError> = ErrorVec::new();
    let target_parsing = pernixc_syntax::target_parsing::parse_target(source_file, &error_vec)?;
    assert!(error_vec.into_vec().is_empty());

    let mut table = Table::new();
    table.generate_modules(&target_parsing);

    let test_id = table
        .get_global_id_by_full_name(std::iter::once("test"))
        .unwrap()
        .into_module()
        .unwrap();

    let test_sub_id = table
        .get_global_id_by_full_name(["test", "sub"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();

    let test_sub_inner_id = table
        .get_global_id_by_full_name(["test", "sub", "inner"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();

    let test_sub_another_inner_id = table
        .get_global_id_by_full_name(["test", "sub", "anotherInner"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();

    let test = table.get_module(test_id).unwrap();
    let test_sub = table.get_module(test_sub_id).unwrap();
    let test_sub_inner = table.get_module(test_sub_inner_id).unwrap();
    let test_sub_another_inner = table.get_module(test_sub_another_inner_id).unwrap();

    assert_eq!(
        test.child_ids_by_name.get("sub").copied(),
        Some(test_sub_id.into())
    );
    assert!(test.parent_module_id.is_none());
    assert_eq!(test.accessibility, Accessibility::Public);
    assert_eq!(table.get_full_name_of(test_id)?, "test");

    assert_eq!(
        test_sub.child_ids_by_name.get("inner").copied(),
        Some(test_sub_inner_id.into())
    );
    assert_eq!(
        test_sub.child_ids_by_name.get("anotherInner").copied(),
        Some(test_sub_another_inner_id.into())
    );
    assert_eq!(test_sub.parent_module_id, Some(test_id));
    assert_eq!(test_sub.accessibility, Accessibility::Public);
    assert_eq!(table.get_full_name_of(test_sub_id)?, "test::sub");

    assert!(test_sub_inner.child_ids_by_name.is_empty());
    assert_eq!(test_sub_inner.parent_module_id, Some(test_sub_id));
    assert_eq!(test_sub_inner.accessibility, Accessibility::Private);
    assert_eq!(
        table.get_full_name_of(test_sub_inner_id)?,
        "test::sub::inner"
    );

    assert!(test_sub_another_inner.child_ids_by_name.is_empty());
    assert_eq!(test_sub_another_inner.parent_module_id, Some(test_sub_id));
    assert_eq!(
        test_sub_another_inner.accessibility,
        Accessibility::Internal
    );
    assert_eq!(
        table.get_full_name_of(test_sub_another_inner_id)?,
        "test::sub::anotherInner"
    );

    Ok(())
}

#[test]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
fn draft_test() -> Result<(), Box<dyn std::error::Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/symbol/populateTest/main.pnx"),
        vec!["test".to_string()],
    )?;

    let errors: ErrorVec<AllParsingError> = ErrorVec::new();
    let target_parsing = pernixc_syntax::target_parsing::parse_target(source_file, &errors)?;
    assert!(errors.into_vec().is_empty());

    let mut table = Table::new();
    let errors: ErrorVec<Error> = ErrorVec::new();
    let mut symbol_states_by_id = HashMap::new();
    table.generate_modules(&target_parsing);
    table.draft_symbols(target_parsing, &mut symbol_states_by_id, &errors);

    let test_module_id = table
        .get_global_id_by_full_name(std::iter::once("test"))
        .unwrap()
        .into_module()
        .unwrap();
    let test_module = table.get_module(test_module_id).unwrap();

    // enum Days
    {
        let enum_id = table
            .get_global_id_by_full_name(["test", "Days"].into_iter())
            .unwrap()
            .into_enum()
            .unwrap();

        let enum_id_in_module = test_module
            .child_ids_by_name
            .get("Days")
            .unwrap()
            .into_enum()
            .unwrap();

        let enum_sym = table.get_enum(enum_id).unwrap();

        assert_eq!(enum_id, enum_id_in_module);
        assert_eq!(table.get_full_name_of(enum_id)?, "test::Days");
        assert_eq!(enum_sym.parent_module_id, test_module_id);
        assert_eq!(enum_sym.accessibility, Accessibility::Public);
    }

    // struct Vector2
    {
        let struct_id = table
            .get_global_id_by_full_name(["test", "Vector2"].into_iter())
            .unwrap()
            .into_struct()
            .unwrap();

        let struct_id_in_module = test_module
            .child_ids_by_name
            .get("Vector2")
            .unwrap()
            .into_struct()
            .unwrap();

        let struct_sym = table.get_struct(struct_id).unwrap();

        assert_eq!(struct_id, struct_id_in_module);
        assert_eq!(table.get_full_name_of(struct_id)?, "test::Vector2");
        assert_eq!(struct_sym.parent_module_id, test_module_id);
        assert_eq!(struct_sym.accessibility, Accessibility::Internal);
    }

    // type FloatingPoint
    {
        let type_id = table
            .get_global_id_by_full_name(["test", "FloatingPoint"].into_iter())
            .unwrap()
            .into_type_alias()
            .unwrap();

        let type_id_in_module = test_module
            .child_ids_by_name
            .get("FloatingPoint")
            .unwrap()
            .into_type_alias()
            .unwrap();

        let type_sym = table.get_type_alias(type_id).unwrap();

        assert_eq!(type_id, type_id_in_module);
        assert_eq!(table.get_full_name_of(type_id)?, "test::FloatingPoint");
        assert_eq!(type_sym.type_alias_parent_id, test_module_id.into());
        assert_eq!(type_sym.accessibility, Accessibility::Public);
        assert_eq!(
            *symbol_states_by_id.get(&type_id.into()).unwrap(),
            SymbolState::Drafted
        );
    }

    // function add(..) {..}
    {
        let function_id = table
            .get_global_id_by_full_name(["test", "add"].into_iter())
            .unwrap()
            .into_overload_set()
            .unwrap();

        let function_id_in_module = test_module
            .child_ids_by_name
            .get("add")
            .unwrap()
            .into_overload_set()
            .unwrap();

        let function_sym = table.get_overload_set(function_id).unwrap();

        assert_eq!(function_id, function_id_in_module);
        assert_eq!(table.get_full_name_of(function_id)?, "test::add");
        assert_eq!(function_sym.parent_module_id, test_module_id);
        assert_eq!(
            *symbol_states_by_id.get(&function_id.into()).unwrap(),
            SymbolState::Drafted
        );
        assert_eq!(function_sym.overloads.len(), 2);
    }

    // function sub(..) {..}
    {
        let function_id = table
            .get_global_id_by_full_name(["test", "sub"].into_iter())
            .unwrap()
            .into_overload_set()
            .unwrap();

        let function_id_in_module = test_module
            .child_ids_by_name
            .get("sub")
            .unwrap()
            .into_overload_set()
            .unwrap();

        let function_sym = table.get_overload_set(function_id).unwrap();

        assert_eq!(function_id, function_id_in_module);
        assert_eq!(table.get_full_name_of(function_id)?, "test::sub");
        assert_eq!(function_sym.parent_module_id, test_module_id);
        assert_eq!(
            *symbol_states_by_id.get(&function_id.into()).unwrap(),
            SymbolState::Drafted
        );
        assert_eq!(function_sym.overloads.len(), 2);
    }

    Ok(())
}

#[test]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
fn accessible_test() -> Result<(), Box<dyn std::error::Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/symbol/accessibleTest/main.pnx"),
        vec!["test".to_string()],
    )?;
    let errors: ErrorVec<AllParsingError> = ErrorVec::new();
    let file_parsing = pernixc_syntax::target_parsing::parse_target(source_file, &errors)?;
    assert!(errors.into_vec().is_empty());

    let mut table = Table::new();
    let errors: ErrorVec<Error> = ErrorVec::new();
    let mut symbol_states_by_id = HashMap::new();

    table.generate_modules(&file_parsing);
    table.draft_symbols(file_parsing, &mut symbol_states_by_id, &errors);

    let test_id = table
        .get_global_id_by_full_name(std::iter::once("test"))
        .unwrap()
        .into_module()
        .unwrap();
    let test_first_id = table
        .get_global_id_by_full_name(["test", "first"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();
    let test_second_id = table
        .get_global_id_by_full_name(["test", "second"].into_iter())
        .unwrap()
        .into_module()
        .unwrap();
    let test_first_private_struct_id = table
        .get_global_id_by_full_name(["test", "first", "PrivateStruct"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();
    let test_private_struct_id = table
        .get_global_id_by_full_name(["test", "PrivateStruct"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();
    let test_public_struct_id = table
        .get_global_id_by_full_name(["test", "PublicStruct"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();

    let test_private_struct = table.get_struct(test_private_struct_id).unwrap();
    let test_public_struct = table.get_struct(test_public_struct_id).unwrap();
    let test_first_private_struct = table.get_struct(test_first_private_struct_id).unwrap();

    assert!(table.symbol_accessible(
        test_id.into(),
        test_private_struct_id.into(),
        test_private_struct.accessibility
    )?);
    assert!(table.symbol_accessible(
        test_id.into(),
        test_public_struct_id.into(),
        test_public_struct.accessibility
    )?);
    assert!(!table.symbol_accessible(
        test_id.into(),
        test_first_private_struct_id.into(),
        test_first_private_struct.accessibility
    )?);
    assert!(table.symbol_accessible(
        test_first_id.into(),
        test_private_struct_id.into(),
        test_private_struct.accessibility
    )?);
    assert!(!table.symbol_accessible(
        test_second_id.into(),
        test_first_private_struct_id.into(),
        test_first_private_struct.accessibility
    )?);

    Ok(())
}

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
pub fn construct_test() -> Result<(), Box<dyn std::error::Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/symbol/constructTest/main.pnx"),
        vec!["test".to_string()],
    )?;
    let errors: ErrorVec<AllParsingError> = ErrorVec::new();
    let target_parsing = pernixc_syntax::target_parsing::parse_target(source_file, &errors)?;
    assert!(errors.into_vec().is_empty());

    let errors: ErrorVec<Error> = ErrorVec::new();
    let table = Table::analyze(target_parsing, &errors);
    let errors = errors.into_vec();
    assert_eq!(errors.len(), 2);

    // error check
    {
        {
            let circular_dependency_err = errors
                .iter()
                .find_map(Error::as_circular_dependency)
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
                .find_map(Error::as_overload_redefinition)
                .unwrap();
            assert_eq!(
                table.get_full_name_of(
                    table
                        .get_overload(overload_redefinition_err.available_overload_id)
                        .unwrap()
                        .parent_overload_set_id
                )?,
                "test::add"
            );
        }
    }

    // type alias check
    {
        assert_eq!(
            table
                .get_type_alias(
                    table
                        .get_global_id_by_full_name(["test", "A"].into_iter())
                        .unwrap()
                        .into_type_alias()
                        .unwrap()
                )
                .unwrap()
                .alias,
            PrimitiveType::Float64.into()
        );
        assert_eq!(
            table
                .get_type_alias(
                    table
                        .get_global_id_by_full_name(["test", "SomeStruct", "A"].into_iter())
                        .unwrap()
                        .into_type_alias()
                        .unwrap()
                )
                .unwrap()
                .alias,
            PrimitiveType::Float32.into()
        );
        assert_eq!(
            table
                .get_type_alias(
                    table
                        .get_global_id_by_full_name(["test", "SomeStruct", "B"].into_iter())
                        .unwrap()
                        .into_type_alias()
                        .unwrap()
                )
                .unwrap()
                .alias,
            PrimitiveType::Float32.into()
        );
    }

    // SomeStruct field check
    {
        let some_struct = table
            .get_struct(
                table
                    .get_global_id_by_full_name(["test", "SomeStruct"].into_iter())
                    .unwrap()
                    .into_struct()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(some_struct.accessibility(), Accessibility::Public);
        assert_eq!(some_struct.field_order().len(), 3);

        let first_field = table
            .get_field(
                some_struct
                    .field_ids_by_name()
                    .get("first")
                    .copied()
                    .unwrap(),
            )
            .unwrap();
        assert_eq!(first_field.ty, PrimitiveType::Float32.into());

        let second_field = table
            .get_field(
                some_struct
                    .field_ids_by_name()
                    .get("second")
                    .copied()
                    .unwrap(),
            )
            .unwrap();
        assert_eq!(second_field.ty, PrimitiveType::Float32.into());

        let third_field = table
            .get_field(
                some_struct
                    .field_ids_by_name()
                    .get("third")
                    .copied()
                    .unwrap(),
            )
            .unwrap();
        assert_eq!(third_field.ty, PrimitiveType::Float64.into());
    }

    // Boolean enum check
    {
        let boolean_enum = table
            .get_enum(
                table
                    .get_global_id_by_full_name(["test", "Boolean"].into_iter())
                    .unwrap()
                    .into_enum()
                    .unwrap(),
            )
            .unwrap();

        let true_variant = table
            .get_enum_variant(*boolean_enum.variant_ids_by_name.get("True").unwrap())
            .unwrap();
        let false_variant = table
            .get_enum_variant(*boolean_enum.variant_ids_by_name.get("False").unwrap())
            .unwrap();

        assert_eq!(boolean_enum.accessibility(), Accessibility::Public);

        assert_eq!(boolean_enum.variant_ids_by_name().len(), 2);
        assert_eq!(true_variant.declaration_order, 0);
        assert_eq!(false_variant.declaration_order, 1);
    }

    // add Overload check
    {
        let add_overload = table
            .get_overload_set(
                table
                    .get_global_id_by_full_name(["test", "add"].into_iter())
                    .unwrap()
                    .into_overload_set()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(add_overload.overloads.len(), 2);

        assert!(add_overload.overloads.iter().any(|x| {
            let overload = table.get_overload(*x).unwrap();

            let first_param = table
                .get_parameter(overload.parameter_order()[0])
                .unwrap()
                .type_binding;
            let second_param = table
                .get_parameter(overload.parameter_order()[1])
                .unwrap()
                .type_binding;
            let return_type = overload.return_type;

            !first_param.is_mutable
                && first_param.ty == PrimitiveType::Float32.into()
                && !second_param.is_mutable
                && second_param.ty == PrimitiveType::Float32.into()
                && return_type == PrimitiveType::Float32.into()
        }));
        assert!(add_overload.overloads.iter().any(|x| {
            let overload = table.get_overload(*x).unwrap();

            let first_param = table
                .get_parameter(overload.parameter_order()[0])
                .unwrap()
                .type_binding;
            let second_param = table
                .get_parameter(overload.parameter_order()[1])
                .unwrap()
                .type_binding;
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
