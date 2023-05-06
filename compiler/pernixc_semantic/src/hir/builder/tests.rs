use std::{error::Error, path::PathBuf, sync::Arc};

use pernixc_source::{SourceElement, SourceFile};
use pernixc_syntax::target_parsing::{AllParsingError, TargetParsing};
use pernixc_system::error_handler::ErrorVec;

use crate::{
    hir::{
        builder::{BindingOption, BindingTarget, Builder},
        value::binding::{ComparisonOperator, EqualityOperator},
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
        assert_eq!(
            err.argument_types,
            [InferableType::Type(PrimitiveType::Int32.into())]
        );
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
            builder.get_address_inferable_type(&address.into()),
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
            builder.get_address_inferable_type(&address.into()),
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

#[test]
#[allow(
    clippy::cognitive_complexity,
    clippy::too_many_lines,
    clippy::similar_names
)]
fn member_access_binding_test() -> Result<(), Box<dyn std::error::Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource/hir/memberAccessBinding/main.pnx"),
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
    let vector3_id = table
        .get_global_id_by_full_name(["test", "vector", "Vector3"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();
    let vector2_id = table
        .get_global_id_by_full_name(["test", "vector", "Vector2"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();

    let vector3_sym = table.get_struct(vector3_id).unwrap();
    let vector2_sym = table.get_struct(vector2_id).unwrap();

    let vector3_vector2_id = vector3_sym
        .field_ids_by_name()
        .get("vector2")
        .copied()
        .unwrap();
    let vector3_z_id = vector3_sym.field_ids_by_name().get("z").copied().unwrap();
    let vector2_x_id = vector2_sym.field_ids_by_name().get("x").copied().unwrap();
    let vector2_y_id = vector2_sym.field_ids_by_name().get("y").copied().unwrap();

    let mut builder = Builder::new(table.clone(), overload_id)?;
    let statements = overload.syntax_tree().block_without_label.statements();

    let errors: ErrorVec<AllHirError> = ErrorVec::new();
    let address_binding_option = BindingOption {
        binding_target: BindingTarget::ForAddress,
    };

    let first_alloca_id = {
        let alloca_id = builder
            .bind_variable_declaration_statement(
                statements[0]
                    .as_declarative()
                    .unwrap()
                    .as_variable_declaration()
                    .unwrap(),
                &errors,
            )
            .unwrap();
        assert_eq!(errors.as_vec().len(), 0);
        alloca_id
    };
    {
        let field_address = builder
            .bind_member_access(
                statements[1]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_member_access()
                    .unwrap(),
                address_binding_option,
                &errors,
            )?
            .into_address_with_span()
            .unwrap()
            .address
            .into_field_address()
            .unwrap();

        assert_eq!(errors.as_vec().len(), 0);

        assert_eq!(
            field_address.operand_address.into_alloca_id().unwrap(),
            first_alloca_id
        );
        assert_eq!(field_address.field_id, vector3_z_id);
    }
    {
        let field_address = builder
            .bind_member_access(
                statements[2]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_member_access()
                    .unwrap(),
                address_binding_option,
                &errors,
            )?
            .into_address_with_span()
            .unwrap()
            .address
            .into_field_address()
            .unwrap();

        assert_eq!(errors.as_vec().len(), 0);

        let inner_field_address = field_address.operand_address.into_field_address().unwrap();
        assert_eq!(
            inner_field_address
                .operand_address
                .into_alloca_id()
                .unwrap(),
            first_alloca_id
        );
        assert_eq!(inner_field_address.field_id, vector3_vector2_id);
        assert_eq!(field_address.field_id, vector2_x_id);
    }
    {
        let field_address = builder
            .bind_member_access(
                statements[3]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_member_access()
                    .unwrap(),
                address_binding_option,
                &errors,
            )?
            .into_address_with_span()
            .unwrap()
            .address
            .into_field_address()
            .unwrap();

        let err = {
            let mut errors = errors.as_vec_mut();
            errors
                .pop()
                .unwrap()
                .into_hir_error()
                .unwrap()
                .into_field_inaccessible()
                .unwrap()
        };
        assert_eq!(err.field_id, vector2_y_id);

        let inner_field_address = field_address.operand_address.into_field_address().unwrap();
        assert_eq!(
            inner_field_address
                .operand_address
                .into_alloca_id()
                .unwrap(),
            first_alloca_id
        );
        assert_eq!(inner_field_address.field_id, vector3_vector2_id);
        assert_eq!(field_address.field_id, vector2_y_id);
    }
    Ok(())
}

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn binary_binding_test() -> Result<(), Box<dyn std::error::Error>> {
    use crate::hir::value::binding::{ArithmeticOperator, BinaryOperator};

    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/hir/binaryBinding/main.pnx"),
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

    let number_sym = table.get_struct(
        table
            .get_global_id_by_full_name(["test", "Number"].into_iter())
            .unwrap()
            .into_struct()
            .unwrap(),
    )?;
    let number_x_field_id = number_sym.field_ids_by_name().get("x").copied().unwrap();

    let new_bool_first_id = table
        .get_overload_set(
            table
                .get_global_id_by_full_name(["test", "newBoolFirst"].into_iter())
                .unwrap()
                .into_overload_set()
                .unwrap(),
        )?
        .overloads()[0];
    let new_bool_second_id = table
        .get_overload_set(
            table
                .get_global_id_by_full_name(["test", "newBoolSecond"].into_iter())
                .unwrap()
                .into_overload_set()
                .unwrap(),
        )?
        .overloads()[0];

    let overload = table.get_overload(overload_id)?;
    let statements = overload.syntax_tree().block_without_label.statements();
    let errors: ErrorVec<AllHirError> = ErrorVec::new();

    {
        let register_id = builder
            .bind_binary(
                statements[0]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_binary()
                    .unwrap(),
                BindingOption::default(),
                &errors,
            )?
            .into_value()
            .unwrap()
            .into_register()
            .unwrap();

        assert_eq!(errors.as_vec().len(), 0);

        let binding = builder
            .container
            .registers
            .get(register_id)
            .unwrap()
            .binding
            .as_binary()
            .unwrap();

        assert_eq!(
            builder
                .get_inferable_type(&register_id)
                .unwrap()
                .into_type()
                .unwrap(),
            Type::PrimitiveType(PrimitiveType::Float32)
        );
        assert_eq!(
            builder
                .get_inferable_type(binding.left_operand())
                .unwrap()
                .into_type()
                .unwrap(),
            Type::PrimitiveType(PrimitiveType::Float32)
        );
        assert_eq!(
            builder
                .get_inferable_type(binding.right_operand())
                .unwrap()
                .into_type()
                .unwrap(),
            Type::PrimitiveType(PrimitiveType::Float32)
        );
        assert_eq!(
            binding.binary_operator,
            BinaryOperator::ArithmeticOperator(ArithmeticOperator::Add)
        );
    }
    {
        let register_id = builder
            .bind_binary(
                statements[1]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_binary()
                    .unwrap(),
                BindingOption::default(),
                &errors,
            )?
            .into_value()
            .unwrap()
            .into_register()
            .unwrap();

        assert_eq!(errors.as_vec().len(), 0);

        let binding = builder
            .container
            .registers
            .get(register_id)
            .unwrap()
            .binding
            .as_binary()
            .unwrap();

        assert_eq!(
            builder
                .get_inferable_type(&register_id)
                .unwrap()
                .into_type()
                .unwrap(),
            Type::PrimitiveType(PrimitiveType::Bool)
        );
        assert_eq!(
            builder
                .get_inferable_type(binding.left_operand())
                .unwrap()
                .into_constraint()
                .unwrap(),
            Constraint::Signed,
        );
        assert_eq!(
            builder
                .get_inferable_type(binding.right_operand())
                .unwrap()
                .into_constraint()
                .unwrap(),
            Constraint::Signed,
        );
        assert_eq!(
            binding.binary_operator,
            BinaryOperator::ComparisonOperator(ComparisonOperator::GreaterThanOrEqual)
        );
    }
    {
        let register_id = builder
            .bind_binary(
                statements[2]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_binary()
                    .unwrap(),
                BindingOption::default(),
                &errors,
            )?
            .into_value()
            .unwrap()
            .into_register()
            .unwrap();

        assert_eq!(errors.as_vec().len(), 0);

        let binding = builder
            .container
            .registers
            .get(register_id)
            .unwrap()
            .binding
            .as_binary()
            .unwrap();

        assert_eq!(
            builder
                .get_inferable_type(&register_id)
                .unwrap()
                .into_type()
                .unwrap(),
            Type::PrimitiveType(PrimitiveType::Bool)
        );
        assert_eq!(
            builder
                .get_inferable_type(binding.left_operand())
                .unwrap()
                .into_type()
                .unwrap(),
            Type::PrimitiveType(PrimitiveType::Bool)
        );
        assert_eq!(
            builder
                .get_inferable_type(binding.right_operand())
                .unwrap()
                .into_type()
                .unwrap(),
            Type::PrimitiveType(PrimitiveType::Bool)
        );
        assert_eq!(
            binding.binary_operator,
            BinaryOperator::EqualityOperator(EqualityOperator::Equal)
        );
    }
    // let number = newNumber();
    // number.x = 4;
    {
        let alloca_address = builder.bind_variable_declaration_statement(
            statements[3]
                .as_declarative()
                .unwrap()
                .as_variable_declaration()
                .unwrap(),
            &errors,
        )?;

        let field_address = builder
            .bind_binary(
                statements[4]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_binary()
                    .unwrap(),
                BindingOption {
                    binding_target: BindingTarget::ForAddress,
                },
                &errors,
            )?
            .into_address_with_span()
            .unwrap();

        assert_eq!(
            *field_address
                .address
                .as_field_address()
                .unwrap()
                .operand_address
                .as_alloca_id()
                .unwrap(),
            alloca_address
        );
        assert_eq!(
            field_address.address.as_field_address().unwrap().field_id,
            number_x_field_id
        );

        let err = {
            let mut errors = errors.as_vec_mut();
            assert_eq!(errors.len(), 1);
            errors
                .pop()
                .unwrap()
                .into_hir_error()
                .unwrap()
                .into_mutable_l_value_expected()
                .unwrap()
        };

        assert_eq!(err.expression_span.str(), "number.x");
        let store_instrcution = builder
            .container
            .control_flow_graph
            .get(builder.current_basic_block_id)?
            .instructions()
            .last()
            .unwrap()
            .as_basic()
            .unwrap()
            .as_store()
            .unwrap();

        assert_eq!(store_instrcution.address, field_address.address);
        assert_eq!(
            builder
                .get_inferable_type(&store_instrcution.value)?
                .into_type()
                .unwrap(),
            Type::PrimitiveType(PrimitiveType::Float32)
        );
    }

    // newBoolFirst() and newBoolSecond();
    {
        let pre_block_id = builder.current_basic_block_id;

        // expect phi node
        let register_id = builder
            .bind_binary(
                statements[5]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression()
                    .as_binary()
                    .unwrap(),
                BindingOption::default(),
                &errors,
            )?
            .into_value()
            .unwrap()
            .into_register()
            .unwrap();

        assert_eq!(errors.as_vec().len(), 0);

        let phi_node_binding = builder
            .container
            .registers
            .get(register_id)
            .unwrap()
            .binding
            .as_phi_node()
            .unwrap();

        let (continue_block_id, rhs_block_id) = {
            let conditional_jump = builder
                .container
                .control_flow_graph
                .get(pre_block_id)?
                .instructions()
                .last()
                .unwrap()
                .as_conditional_jump()
                .unwrap();

            (
                conditional_jump.false_jump_target,
                conditional_jump.true_jump_target,
            )
        };

        assert_eq!(builder.current_basic_block_id, continue_block_id);

        let phi_node = builder
            .container
            .registers
            .get(register_id)?
            .binding
            .as_phi_node()
            .unwrap();

        assert_eq!(phi_node.values_by_predecessor.len(), 2);
        let pre_incoming_register_id = *phi_node_binding.values_by_predecessor[&pre_block_id]
            .as_register()
            .unwrap();
        let rhs_incoming_register_id = *phi_node_binding.values_by_predecessor[&rhs_block_id]
            .as_register()
            .unwrap();

        assert_eq!(
            builder
                .container
                .registers
                .get(pre_incoming_register_id)?
                .binding
                .as_function_call()
                .unwrap()
                .overload_id,
            new_bool_first_id
        );
        assert_eq!(
            builder
                .container
                .registers
                .get(rhs_incoming_register_id)?
                .binding
                .as_function_call()
                .unwrap()
                .overload_id,
            new_bool_second_id
        );
    }

    Ok(())
}

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
pub fn block_binding_test() -> Result<(), Box<dyn std::error::Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/hir/blockBinding/main.pnx"),
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
        let value = builder
            .bind_block(
                statements[0]
                    .as_expressive()
                    .unwrap()
                    .as_imperative()
                    .unwrap()
                    .as_block()
                    .unwrap(),
                &errors,
            )?
            .into_constant()
            .unwrap()
            .into_void_constant()
            .unwrap();

        assert_eq!(errors.as_vec().len(), 0);

        assert_eq!(builder.get_span(&value).unwrap().str(), "{}");
    }
    Ok(())
}
