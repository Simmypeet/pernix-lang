use std::{error::Error, path::PathBuf, sync::Arc};

use pernixc_source::{SourceElement, SourceFile};
use pernixc_syntax::target_parsing::{AllParsingError, TargetParsing};
use pernixc_system::error_handler::ErrorVec;

use crate::{
    hir::{
        builder::{BindingOption, BindingTarget, Builder},
        AllHirError,
    },
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
        &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource/hir/numericLiteralBinding/main.pnx"),
        vec!["test".to_string()],
    )?;

    let errors: ErrorVec<AllParsingError> = ErrorVec::new();
    let target = TargetParsing::parse(source_file, &errors)?;
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
        let type_binding = builder.get_inferable_type(&numeric_value).unwrap();
        assert_eq!(type_binding, InferableType::Constraint(Constraint::Number));
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
        let type_binding = builder.get_inferable_type(&numeric_value).unwrap();
        assert_eq!(type_binding, InferableType::Constraint(Constraint::Float));
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
        let type_binding = builder.get_inferable_type(&numeric_value).unwrap();
        assert_eq!(
            type_binding,
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
        let type_binding = builder.get_inferable_type(&numeric_value)?;
        assert_eq!(
            type_binding,
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
        let type_binding = builder.get_inferable_type(&numeric_value)?;
        assert_eq!(
            type_binding,
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
        &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource/hir/functionCallBinding/main.pnx"),
        vec!["test".to_string()],
    )?;

    let errors: ErrorVec<AllParsingError> = ErrorVec::new();
    let target = TargetParsing::parse(source_file, &errors)?;
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
    let inner_some_function_overload_set_id = table
        .get_global_id_by_full_name(["test", "inner", "someFunction"].into_iter())
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
        let type_binding = builder.get_inferable_type(&register_id).unwrap();
        assert_eq!(
            type_binding,
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
            builder.get_inferable_type(&function_binding.arguments[0])?,
            InferableType::Type(Type::PrimitiveType(PrimitiveType::Uint32))
        );
        assert_eq!(builder.get_span(&function_binding.arguments[0])?.str(), "1");
        assert_eq!(
            builder.get_inferable_type(&function_binding.arguments[1])?,
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
    {
        let register_id = builder
            .bind_function_call(
                statements[3]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_function_call()
                    .unwrap(),
                &errors,
            )
            .unwrap();

        assert_eq!(errors.as_vec().len(), 0);
        let type_binding = builder.get_inferable_type(&register_id).unwrap();
        assert_eq!(
            type_binding,
            InferableType::Type(PrimitiveType::Float32.into())
        );

        let function_binding = builder
            .container
            .registers
            .get(register_id)?
            .binding
            .as_function_call()
            .unwrap();

        assert_eq!(function_binding.arguments.len(), 1);
        assert_eq!(
            table
                .get_overload(function_binding.overload_id)?
                .parent_overload_set_id(),
            some_function_overload_set_id
        );

        assert_eq!(
            builder.get_inferable_type(&function_binding.arguments[0])?,
            InferableType::Type(Type::PrimitiveType(PrimitiveType::Float32))
        );
        assert_eq!(
            builder.get_span(&function_binding.arguments[0])?.str(),
            "4.0"
        );
    }
    {
        assert!(builder
            .bind_function_call(
                statements[4]
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
                .into_no_overload_with_matching_argument_types()
                .unwrap()
        };

        assert_eq!(err.overload_set_id, some_function_overload_set_id);
        assert_eq!(err.argument_types, [InferableType::Type(
            PrimitiveType::Int32.into()
        )]);
    }
    {
        let register_id = builder
            .bind_function_call(
                statements[5]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_function_call()
                    .unwrap(),
                &errors,
            )
            .unwrap();

        assert_eq!(errors.as_vec().len(), 0);
        let type_binding = builder.get_inferable_type(&register_id).unwrap();
        assert_eq!(
            type_binding,
            InferableType::Type(PrimitiveType::Float32.into())
        );

        let function_binding = builder
            .container
            .registers
            .get(register_id)?
            .binding
            .as_function_call()
            .unwrap();

        assert_eq!(function_binding.arguments.len(), 1);
        assert_eq!(
            table
                .get_overload(function_binding.overload_id)?
                .parent_overload_set_id(),
            some_function_overload_set_id
        );

        assert_eq!(
            builder.get_inferable_type(&function_binding.arguments[0])?,
            InferableType::Type(Type::PrimitiveType(PrimitiveType::Float32))
        );
        assert_eq!(
            builder.get_span(&function_binding.arguments[0])?.str(),
            "6f32"
        );
    }
    {
        let register_id = builder
            .bind_function_call(
                statements[6]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_function_call()
                    .unwrap(),
                &errors,
            )
            .unwrap();

        assert_eq!(errors.as_vec().len(), 0);
        let type_binding = builder.get_inferable_type(&register_id).unwrap();
        assert_eq!(
            type_binding,
            InferableType::Type(PrimitiveType::Int8.into())
        );

        let function_binding = builder
            .container
            .registers
            .get(register_id)?
            .binding
            .as_function_call()
            .unwrap();

        assert_eq!(function_binding.arguments.len(), 1);
        assert_eq!(
            table
                .get_overload(function_binding.overload_id)?
                .parent_overload_set_id(),
            some_function_overload_set_id
        );

        assert_eq!(
            builder.get_inferable_type(&function_binding.arguments[0])?,
            InferableType::Type(Type::PrimitiveType(PrimitiveType::Int8))
        );
        assert_eq!(
            builder.get_span(&function_binding.arguments[0])?.str(),
            "7i8"
        );
    }
    {
        assert!(builder
            .bind_function_call(
                statements[7]
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
                .into_no_accessible_overload()
                .unwrap()
        };

        assert_eq!(err.overload_set_id, inner_some_function_overload_set_id);
    }

    Ok(())
}

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn prefix_binding_test() -> Result<(), Box<dyn std::error::Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/hir/prefixBinding/main.pnx"),
        vec!["test".to_string()],
    )?;

    let errors: ErrorVec<AllParsingError> = ErrorVec::new();
    let target = TargetParsing::parse(source_file, &errors)?;
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
        let register_id = builder.bind_prefix(
            statements[0]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_prefix()
                .unwrap(),
            &errors,
        )?;
        assert_eq!(errors.as_vec().len(), 0);

        let type_binding = builder.get_inferable_type(&register_id).unwrap();
        assert_eq!(type_binding, InferableType::Constraint(Constraint::Signed));

        let prefix = builder
            .container
            .registers
            .get(register_id)?
            .binding
            .as_prefix()
            .unwrap();

        assert_eq!(
            builder.get_inferable_type(prefix.operand())?,
            InferableType::Constraint(Constraint::Signed)
        );

        assert_eq!(builder.get_span(prefix.operand())?.str(), "0");
    }
    {
        let register_id = builder.bind_prefix(
            statements[1]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_prefix()
                .unwrap(),
            &errors,
        )?;
        assert_eq!(errors.as_vec().len(), 0);

        let type_binding = builder.get_inferable_type(&register_id).unwrap();
        assert_eq!(type_binding, InferableType::Constraint(Constraint::Float));

        let prefix = builder
            .container
            .registers
            .get(register_id)?
            .binding
            .as_prefix()
            .unwrap();

        assert_eq!(
            builder.get_inferable_type(prefix.operand())?,
            InferableType::Constraint(Constraint::Float)
        );

        assert_eq!(builder.get_span(prefix.operand())?.str(), "1.0");
    }
    {
        let register_id = builder.bind_prefix(
            statements[2]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_prefix()
                .unwrap(),
            &errors,
        )?;
        assert_eq!(errors.as_vec().len(), 0);

        let type_binding = builder.get_inferable_type(&register_id).unwrap();
        assert_eq!(
            type_binding,
            InferableType::Type(PrimitiveType::Float32.into())
        );

        let prefix = builder
            .container
            .registers
            .get(register_id)?
            .binding
            .as_prefix()
            .unwrap();

        assert_eq!(
            builder.get_inferable_type(prefix.operand())?,
            InferableType::Type(PrimitiveType::Float32.into())
        );

        assert_eq!(builder.get_span(prefix.operand())?.str(), "2.0f32");
    }
    {
        let register_id = builder.bind_prefix(
            statements[3]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_prefix()
                .unwrap(),
            &errors,
        )?;

        assert!(builder
            .container
            .registers
            .get(register_id)?
            .binding
            .as_prefix()
            .unwrap()
            .operand
            .as_placeholder()
            .is_some());

        let err = {
            let mut errors = errors.as_vec_mut();
            assert_eq!(errors.len(), 1);
            errors
                .pop()
                .unwrap()
                .into_hir_error()
                .unwrap()
                .into_type_mismatch()
                .unwrap()
        };

        assert_eq!(err.expect, Constraint::Signed.into());
        assert_eq!(err.found, Type::PrimitiveType(PrimitiveType::Uint32).into());
    }
    {
        let register_id = builder.bind_prefix(
            statements[4]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_prefix()
                .unwrap(),
            &errors,
        )?;

        assert!(builder
            .container
            .registers
            .get(register_id)?
            .binding
            .as_prefix()
            .unwrap()
            .operand
            .as_placeholder()
            .is_some());

        let err = {
            let mut errors = errors.as_vec_mut();
            assert_eq!(errors.len(), 1);
            errors
                .pop()
                .unwrap()
                .into_hir_error()
                .unwrap()
                .into_type_mismatch()
                .unwrap()
        };

        assert_eq!(err.expect, Type::PrimitiveType(PrimitiveType::Bool).into());
        assert_eq!(err.found, Constraint::Number.into());
    }
    {
        let register_id = builder.bind_prefix(
            statements[5]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_prefix()
                .unwrap(),
            &errors,
        )?;
        assert_eq!(errors.as_vec().len(), 0);

        let type_binding = builder.get_inferable_type(&register_id).unwrap();
        assert_eq!(
            type_binding,
            InferableType::Type(PrimitiveType::Bool.into())
        );

        let prefix = builder
            .container
            .registers
            .get(register_id)?
            .binding
            .as_prefix()
            .unwrap();

        assert_eq!(
            builder.get_inferable_type(prefix.operand())?,
            InferableType::Type(PrimitiveType::Bool.into())
        );

        assert_eq!(builder.get_span(prefix.operand())?.str(), "false");
    }

    Ok(())
}

#[test]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
fn named_binding_test() -> Result<(), Box<dyn std::error::Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/hir/namedBinding/main.pnx"),
        vec!["test".to_string()],
    )?;

    let errors: ErrorVec<AllParsingError> = ErrorVec::new();
    let target = TargetParsing::parse(source_file, &errors)?;
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
    let some_enum_id = table
        .get_global_id_by_full_name(["test", "SomeEnum"].into_iter())
        .unwrap()
        .into_enum()
        .unwrap();
    let mut builder = Builder::new(table, overload_id)?;
    let table = builder.container.table.clone();
    let overload = table.get_overload(overload_id)?;
    let statements = overload.syntax_tree().block_without_label.statements();
    let errors: ErrorVec<AllHirError> = ErrorVec::new();

    {
        assert!(builder.bind_statement(&statements[0], &errors).is_ok());
        assert_eq!(errors.as_vec().len(), 0);
    }
    {
        let address = builder
            .bind_named(
                statements[1]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_named()
                    .unwrap(),
                BindingOption {
                    binding_target: BindingTarget::ForAddress,
                },
                &errors,
            )?
            .into_address_with_span()
            .unwrap()
            .address
            .into_parameter_id()
            .unwrap();

        assert_eq!(errors.as_vec().len(), 0);
        assert_eq!(
            builder
                .container
                .table()
                .get_parameter(address)
                .unwrap()
                .name(),
            "someParameter"
        );
    }
    {
        let address = builder
            .bind_named(
                statements[2]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_named()
                    .unwrap(),
                BindingOption {
                    binding_target: BindingTarget::ForAddress,
                },
                &errors,
            )?
            .into_address_with_span()
            .unwrap()
            .address
            .into_alloca_id()
            .unwrap();

        assert_eq!(errors.as_vec().len(), 0);
        let alloca = builder.container.allocas().get(address).unwrap();

        assert_eq!(
            builder.get_address_type(&address.into()),
            Type::PrimitiveType(PrimitiveType::Float32).into()
        );
        assert_eq!(alloca.identifier_token().span.str(), "value");
    }
    {
        assert!(builder
            .bind_named(
                statements[3]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_named()
                    .unwrap(),
                BindingOption {
                    binding_target: BindingTarget::ForAddress,
                },
                &errors,
            )
            .is_err());

        let error = {
            let mut errors = errors.as_vec_mut();
            assert_eq!(errors.len(), 1);
            errors
                .pop()
                .unwrap()
                .into_hir_error()
                .unwrap()
                .into_value_expected()
                .unwrap()
        };

        assert_eq!(some_enum_id, error.found_symbol.into_enum().unwrap());
    }
    {
        let enum_literal = builder
            .bind_named(
                statements[4]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_named()
                    .unwrap(),
                BindingOption {
                    binding_target: BindingTarget::ForValue,
                },
                &errors,
            )?
            .into_value()
            .unwrap()
            .into_constant()
            .unwrap()
            .into_enum_literal()
            .unwrap();

        assert_eq!(errors.as_vec().len(), 0,);

        assert_eq!(
            table
                .get_enum_variant(enum_literal.enum_variant_id())
                .unwrap()
                .parent_enum_id(),
            some_enum_id
        );
    }
    {
        assert!(builder.bind_statement(&statements[5], &errors).is_ok());
        assert_eq!(errors.as_vec().len(), 0);
    }
    {
        let address = builder
            .bind_named(
                statements[6]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_named()
                    .unwrap(),
                BindingOption {
                    binding_target: BindingTarget::ForAddress,
                },
                &errors,
            )?
            .into_address_with_span()
            .unwrap()
            .address
            .into_alloca_id()
            .unwrap();

        assert_eq!(errors.as_vec().len(), 0);
        let alloca = builder.container.allocas().get(address).unwrap();

        assert_eq!(
            builder.get_address_type(&address.into()),
            Type::PrimitiveType(PrimitiveType::Float64).into()
        );
        assert_eq!(alloca.identifier_token().span.str(), "someParameter");
    }
    Ok(())
}

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn struct_literal_binding_test() -> Result<(), Box<dyn std::error::Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource/hir/structLiteralBinding/main.pnx"),
        vec!["test".to_string()],
    )?;

    let errors: ErrorVec<AllParsingError> = ErrorVec::new();
    let target = TargetParsing::parse(source_file, &errors)?;
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
    let overload = table.get_overload(overload_id)?;
    let empty_struct_id = table
        .get_global_id_by_full_name(["test", "Empty"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();
    let struct_id = table
        .get_global_id_by_full_name(["test", "Vector3"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();
    let struct_sym = table.get_struct(struct_id).unwrap();

    let x_filed_id = struct_sym.field_ids_by_name().get("x").copied().unwrap();
    let y_filed_id = struct_sym.field_ids_by_name().get("y").copied().unwrap();
    let z_filed_id = struct_sym.field_ids_by_name().get("z").copied().unwrap();

    let mut builder = Builder::new(table.clone(), overload_id)?;
    let statements = overload.syntax_tree().block_without_label.statements();

    let errors: ErrorVec<AllHirError> = ErrorVec::new();

    {
        let register_id = builder.bind_struct_literal(
            statements[0]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_struct_literal()
                .unwrap(),
            &errors,
        )?;
        assert_eq!(errors.as_vec().len(), 0);
        let found_struct_id = builder
            .get_inferable_type(&register_id)
            .unwrap()
            .into_type()
            .unwrap()
            .into_typed_id()
            .unwrap()
            .into_struct()
            .unwrap();

        assert_eq!(found_struct_id, struct_id);
        let struct_literal_binding = builder
            .container
            .registers
            .get(register_id)
            .unwrap()
            .binding
            .as_struct_literal()
            .unwrap();

        for ((i, field_id), value_str) in (0..3)
            .zip([x_filed_id, y_filed_id, z_filed_id].into_iter())
            .zip(["6", "9", "420"].into_iter())
        {
            assert_eq!(struct_literal_binding.initializations[i].0, field_id);
            assert_eq!(
                builder
                    .get_inferable_type(&struct_literal_binding.initializations[i].1)
                    .unwrap()
                    .into_type()
                    .unwrap(),
                PrimitiveType::Float32.into()
            );
            assert_eq!(
                builder
                    .get_span(&struct_literal_binding.initializations[i].1)
                    .unwrap()
                    .str(),
                value_str
            );
        }
    }
    {
        let register_id = builder.bind_struct_literal(
            statements[1]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_struct_literal()
                .unwrap(),
            &errors,
        )?;

        let error = {
            let mut errors = errors.as_vec_mut();
            assert_eq!(errors.len(), 1);
            errors
                .pop()
                .unwrap()
                .into_hir_error()
                .unwrap()
                .into_uninitialized_fields()
                .unwrap()
        };

        assert_eq!(error.uninitialized_fields.len(), 1);
        assert_eq!(error.uninitialized_fields[0], y_filed_id);
        assert_eq!(error.struct_id, struct_id);

        let found_struct_id = builder
            .get_inferable_type(&register_id)
            .unwrap()
            .into_type()
            .unwrap()
            .into_typed_id()
            .unwrap()
            .into_struct()
            .unwrap();

        assert_eq!(found_struct_id, struct_id);

        let struct_literal_binding = builder
            .container
            .registers
            .get(register_id)
            .unwrap()
            .binding
            .as_struct_literal()
            .unwrap();

        for ((i, field_id), value_str) in (0..2)
            .zip([x_filed_id, z_filed_id].into_iter())
            .zip(["6", "42"].into_iter())
        {
            assert_eq!(struct_literal_binding.initializations[i].0, field_id);
            assert_eq!(
                builder
                    .get_inferable_type(&struct_literal_binding.initializations[i].1)
                    .unwrap()
                    .into_type()
                    .unwrap(),
                PrimitiveType::Float32.into()
            );
            assert_eq!(
                builder
                    .get_span(&struct_literal_binding.initializations[i].1)
                    .unwrap()
                    .str(),
                value_str
            );
        }
    }
    {
        let register_id = builder.bind_struct_literal(
            statements[2]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_struct_literal()
                .unwrap(),
            &errors,
        )?;

        let error = {
            let mut errors = errors.as_vec_mut();
            assert_eq!(errors.len(), 1);
            errors
                .pop()
                .unwrap()
                .into_hir_error()
                .unwrap()
                .into_duplicate_field_initialization()
                .unwrap()
        };

        assert_eq!(error.duplicate_initialization_span.str(), "z: 0");
        assert_eq!(error.previous_initialization_span.str(), "z: 6");
        assert_eq!(error.field_id, z_filed_id);
        assert_eq!(error.struct_id, struct_id);

        let found_struct_id = builder
            .get_inferable_type(&register_id)
            .unwrap()
            .into_type()
            .unwrap()
            .into_typed_id()
            .unwrap()
            .into_struct()
            .unwrap();

        assert_eq!(found_struct_id, struct_id);

        let struct_literal_binding = builder
            .container
            .registers
            .get(register_id)
            .unwrap()
            .binding
            .as_struct_literal()
            .unwrap();

        for ((i, field_id), value_str) in (0..3)
            .zip([z_filed_id, y_filed_id, x_filed_id].into_iter())
            .zip(["6", "9", "2"].into_iter())
        {
            assert_eq!(struct_literal_binding.initializations[i].0, field_id);
            assert_eq!(
                builder
                    .get_inferable_type(&struct_literal_binding.initializations[i].1)
                    .unwrap()
                    .into_type()
                    .unwrap(),
                PrimitiveType::Float32.into()
            );
            assert_eq!(
                builder
                    .get_span(&struct_literal_binding.initializations[i].1)
                    .unwrap()
                    .str(),
                value_str
            );
        }
    }
    {
        let register_id = builder.bind_struct_literal(
            statements[3]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression()
                .as_struct_literal()
                .unwrap(),
            &errors,
        )?;
        assert_eq!(errors.as_vec().len(), 0);
        let found_struct_id = builder
            .get_inferable_type(&register_id)
            .unwrap()
            .into_type()
            .unwrap()
            .into_typed_id()
            .unwrap()
            .into_struct()
            .unwrap();

        assert_eq!(found_struct_id, empty_struct_id);
        let struct_literal_binding = builder
            .container
            .registers
            .get(register_id)
            .unwrap()
            .binding
            .as_struct_literal()
            .unwrap();

        assert_eq!(struct_literal_binding.initializations.len(), 0);
    }
    Ok(())
}
