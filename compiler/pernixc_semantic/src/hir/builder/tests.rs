use std::{error::Error, path::PathBuf, sync::Arc};

use pernixc_source::{SourceElement, SourceFile};
use pernixc_syntax::target_parsing::AllParsingError;
use pernixc_system::error_handler::ErrorVec;

use crate::{
    hir::{builder::Builder, AllHirError},
    infer::{Constraint, InferableType},
    symbol::{
        error::Error as SymbolError,
        table::Table,
        ty::{PrimitiveType, Type},
    },
};

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn numeric_literal_binding() -> Result<(), Box<dyn Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/hir/numericLiteralBinding.pnx"),
        vec!["test".to_string()],
    )?;

    let errors: ErrorVec<AllParsingError> = ErrorVec::new();
    let target = pernixc_syntax::target_parsing::parse_target(source_file, &errors)?;
    assert_eq!(errors.into_vec().len(), 0);

    let errors: ErrorVec<SymbolError> = ErrorVec::new();
    let table = Arc::new(Table::analyze(target, &errors));
    assert_eq!(errors.into_vec().len(), 0);

    let overload_set = table.get_overload_set(
        table
            .get_global_id_by_full_name(["test", "main"].into_iter())
            .unwrap()
            .into_overload_set()
            .unwrap(),
    )?;
    let overload_id = overload_set.overloads()[0];
    let mut builder = Builder::new(table, overload_id)?;
    let table = builder.container.table.clone();
    let overload = table.get_overload(overload_id)?;
    let statements = overload.syntax_tree().block_without_label.statements();
    let errors: ErrorVec<AllHirError> = ErrorVec::new();

    {
        let numeric_value = builder.bind_numeric_literal(
            statements[0]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_numeric_literal()
                .unwrap(),
            &errors,
        )?;
        assert_eq!(errors.as_vec().len(), 0);
        let type_binding = builder.get_type_binding(&numeric_value).unwrap();
        assert_eq!(
            type_binding.ty,
            InferableType::Constraint(Constraint::Number)
        );
        assert_eq!(
            numeric_value.numeric_literal_syntax_tree().span().str(),
            "1"
        );
    }
    {
        let numeric_value = builder.bind_numeric_literal(
            statements[1]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_numeric_literal()
                .unwrap(),
            &errors,
        )?;
        assert_eq!(errors.as_vec().len(), 0);
        let type_binding = builder.get_type_binding(&numeric_value).unwrap();
        assert_eq!(
            type_binding.ty,
            InferableType::Constraint(Constraint::Float)
        );
        assert_eq!(
            numeric_value.numeric_literal_syntax_tree().span().str(),
            "2.0"
        );
    }
    {
        let numeric_value = builder.bind_numeric_literal(
            statements[2]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_numeric_literal()
                .unwrap(),
            &errors,
        )?;
        assert_eq!(errors.as_vec().len(), 0);
        let type_binding = builder.get_type_binding(&numeric_value).unwrap();
        assert_eq!(
            type_binding.ty,
            InferableType::Type(Type::PrimitiveType(PrimitiveType::Float32))
        );
        assert_eq!(
            numeric_value.numeric_literal_syntax_tree().span().str(),
            "3.0f32"
        );
    }
    {
        assert!(builder
            .bind_numeric_literal(
                statements[3]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_numeric_literal()
                    .unwrap(),
                &errors,
            )
            .is_err());

        let err = {
            let mut errors = errors.as_vec_mut();
            assert_eq!(errors.len(), 1);
            errors
                .pop()
                .unwrap()
                .into_hir_error()
                .unwrap()
                .into_floating_point_literal_has_integral_suffix()
                .unwrap()
        };
        assert_eq!(err.floating_point_span.str(), "4.0i32");
    }
    {
        let numeric_value = builder.bind_numeric_literal(
            statements[4]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_numeric_literal()
                .unwrap(),
            &errors,
        )?;
        assert_eq!(errors.as_vec().len(), 0);
        let type_binding = builder.get_type_binding(&numeric_value)?;
        assert_eq!(
            type_binding.ty,
            InferableType::Type(Type::PrimitiveType(PrimitiveType::Uint8))
        );
        assert_eq!(
            numeric_value.numeric_literal_syntax_tree().span().str(),
            "5u8"
        );
    }
    {
        let numeric_value = builder.bind_numeric_literal(
            statements[5]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_numeric_literal()
                .unwrap(),
            &errors,
        )?;
        assert_eq!(errors.as_vec().len(), 0);
        let type_binding = builder.get_type_binding(&numeric_value)?;
        assert_eq!(
            type_binding.ty,
            InferableType::Type(Type::PrimitiveType(PrimitiveType::Float32))
        );
        assert_eq!(
            numeric_value.numeric_literal_syntax_tree().span().str(),
            "6f32"
        );
    }

    Ok(())
}

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn function_call_binding_test() -> Result<(), Box<dyn Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/hir/functionCallBinding.pnx"),
        vec!["test".to_string()],
    )?;

    let errors: ErrorVec<AllParsingError> = ErrorVec::new();
    let target = pernixc_syntax::target_parsing::parse_target(source_file, &errors)?;
    assert_eq!(errors.into_vec().len(), 0);

    let errors: ErrorVec<SymbolError> = ErrorVec::new();
    let table = Arc::new(Table::analyze(target, &errors));
    assert_eq!(errors.into_vec().len(), 0);

    let main_overload_set = table.get_overload_set(
        table
            .get_global_id_by_full_name(["test", "main"].into_iter())
            .unwrap()
            .into_overload_set()
            .unwrap(),
    )?;
    let main_overload_id = main_overload_set.overloads()[0];
    let some_function_overload_set_id = table
        .get_global_id_by_full_name(["test", "someFunction"].into_iter())
        .unwrap()
        .into_overload_set()
        .unwrap();

    let mut builder = Builder::new(table, main_overload_id)?;
    let table = builder.container.table.clone();
    let overload = table.get_overload(main_overload_id)?;
    let statements = overload.syntax_tree().block_without_label.statements();
    let errors: ErrorVec<AllHirError> = ErrorVec::new();

    {
        let register_id = builder.bind_function_call(
            statements[0]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_function_call()
                .unwrap(),
            &errors,
        )?;
        assert_eq!(errors.as_vec().len(), 0);
        let type_binding = builder.get_type_binding(&register_id).unwrap();
        assert_eq!(
            type_binding.ty,
            InferableType::Type(PrimitiveType::Uint32.into())
        );

        let function_binding = builder
            .container
            .registers
            .get(register_id)?
            .binding
            .as_function_call()
            .unwrap();

        assert_eq!(function_binding.arguments.len(), 2);
        assert_eq!(
            table
                .get_overload(function_binding.overload_id)?
                .parent_overload_set_id(),
            some_function_overload_set_id
        );

        assert_eq!(
            builder.get_type_binding(&function_binding.arguments[0])?.ty,
            InferableType::Type(Type::PrimitiveType(PrimitiveType::Uint32))
        );
        assert_eq!(builder.get_span(&function_binding.arguments[0])?.str(), "1");
        assert_eq!(
            builder.get_type_binding(&function_binding.arguments[1])?.ty,
            InferableType::Type(Type::PrimitiveType(PrimitiveType::Uint32))
        );
        assert_eq!(builder.get_span(&function_binding.arguments[1])?.str(), "2");
    }
    {
        assert!(builder
            .bind_function_call(
                statements[1]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_function_call()
                    .unwrap(),
                &errors,
            )
            .is_err());

        let err = {
            let mut errors = errors.as_vec_mut();
            assert_eq!(errors.len(), 1);
            errors
                .pop()
                .unwrap()
                .into_hir_error()
                .unwrap()
                .into_no_overload_with_matching_number_of_arguments()
                .unwrap()
        };

        assert_eq!(err.argument_count, 3);
        assert_eq!(err.overload_set_id, some_function_overload_set_id);
    }
    {
        assert!(builder
            .bind_function_call(
                statements[2]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_function_call()
                    .unwrap(),
                &errors,
            )
            .is_err());

        let err = {
            let mut errors = errors.as_vec_mut();
            assert_eq!(errors.len(), 1);
            errors
                .pop()
                .unwrap()
                .into_hir_error()
                .unwrap()
                .into_ambiguous_function_call()
                .unwrap()
        };

        assert_eq!(err.candidate_overloads.len(), 2);
        assert_eq!(err.overload_set_id, some_function_overload_set_id);
    }

    Ok(())
}
