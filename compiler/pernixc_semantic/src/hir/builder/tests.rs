use std::{error::Error, path::PathBuf};

use pernixc_common::source_file::SourceFile;

use crate::{
    errors::SemanticError,
    hir::{
        builder::Builder,
        value::{BinaryOperator, InferrableType, IntermediateType, PrefixOperator, ValueTrait},
    },
    infer::Constraint,
    symbol::ty::{PrimitiveType, Type},
};

#[test]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
fn numeric_literal_binding_test() -> Result<(), Box<dyn Error>> {
    let file_parsing = pernixc_syntax::file_parsing::parse_files(SourceFile::load(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
            .join("hir")
            .join("numericLiteralBinding.pnx"),
        vec!["test".to_string()],
    )?)?;

    let (symbol_table, errors) = super::Table::analyze(file_parsing.into_iter());
    assert!(errors.is_empty());

    // targetting test::main()
    let mut builder = Builder::new(
        &symbol_table,
        symbol_table
            .get_id_by_full_name(["test", "main"].into_iter())
            .unwrap()
            .into_function()
            .unwrap(),
    );

    {
        // 1
        let numeric_literal = builder
            .bind_numeric_literal(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[0]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_numeric_literal()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(numeric_literal.source_span.source_code(), "1");
        assert_eq!(
            *builder
                .inference_context
                .get_inference(numeric_literal.ty.into_inference().unwrap())
                .as_type_variable()
                .unwrap()
                .constraint(),
            Constraint::Number
        );
    }

    {
        // 2.0
        let numeric_literal = builder
            .bind_numeric_literal(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[1]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_numeric_literal()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(numeric_literal.source_span.source_code(), "2.0");
        assert_eq!(
            *builder
                .inference_context
                .get_inference(numeric_literal.ty.into_inference().unwrap())
                .as_type_variable()
                .unwrap()
                .constraint(),
            Constraint::Float
        );
    }

    {
        // 3.0f32
        let numeric_literal = builder
            .bind_numeric_literal(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[2]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_numeric_literal()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(numeric_literal.source_span.source_code(), "3.0");
        assert_eq!(
            numeric_literal.ty.into_type().unwrap(),
            Type::Primitive(PrimitiveType::Float32)
        );
    }

    {
        // 4.0i32
        assert!(builder
            .bind_numeric_literal(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[3]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_numeric_literal()
                    .unwrap(),
            )
            .is_none());

        let err = std::mem::take(&mut builder.errors);

        assert_eq!(err.len(), 1);
        assert_eq!(
            err[0]
                .as_invalid_numeric_literal_suffix()
                .unwrap()
                .source_span
                .source_code(),
            "4.0i32"
        );
    }

    {
        // 5u8
        let numeric_literal = builder
            .bind_numeric_literal(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[4]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_numeric_literal()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(numeric_literal.source_span.source_code(), "5");
        assert_eq!(
            numeric_literal.ty.into_type().unwrap(),
            Type::Primitive(PrimitiveType::Uint8)
        );
    }

    {
        // 6f32
        let numeric_literal = builder
            .bind_numeric_literal(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[5]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_numeric_literal()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(numeric_literal.source_span.source_code(), "6");
        assert_eq!(
            numeric_literal.ty.into_type().unwrap(),
            Type::Primitive(PrimitiveType::Float32)
        );
    }

    Ok(())
}

#[test]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
fn prefix_binding() -> Result<(), Box<dyn Error>> {
    let file_parsing = pernixc_syntax::file_parsing::parse_files(SourceFile::load(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
            .join("hir")
            .join("prefixBinding.pnx"),
        vec!["test".to_string()],
    )?)?;

    let (symbol_table, errors) = super::Table::analyze(file_parsing.into_iter());
    assert!(errors.is_empty());

    // targetting test::main
    let mut builder = Builder::new(
        &symbol_table,
        symbol_table
            .get_id_by_full_name(["test", "main"].into_iter())
            .unwrap()
            .into_function()
            .unwrap(),
    );

    {
        // -0
        let prefix = builder
            .bind_prefix(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[0]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_prefix()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(prefix.source_span.source_code(), "-0");
        assert_eq!(prefix.prefix_operator, PrefixOperator::Negate);
        assert_eq!(prefix.ty, prefix.operand.type_binding().ty);

        assert_eq!(
            *builder
                .inference_context
                .get_inference(prefix.ty.into_inference().unwrap())
                .as_type_variable()
                .unwrap()
                .constraint(),
            Constraint::Signed
        );
    }

    {
        // -1.0
        let prefix = builder
            .bind_prefix(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[1]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_prefix()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(prefix.source_span.source_code(), "-1.0");
        assert_eq!(prefix.prefix_operator, PrefixOperator::Negate);
        assert_eq!(prefix.ty, prefix.operand.type_binding().ty);

        assert_eq!(
            *builder
                .inference_context
                .get_inference(prefix.ty.into_inference().unwrap())
                .as_type_variable()
                .unwrap()
                .constraint(),
            Constraint::Float
        );
    }

    {
        // -2.0f32
        let prefix = builder
            .bind_prefix(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[2]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_prefix()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(prefix.source_span.source_code(), "-2.0f32");
        assert_eq!(prefix.prefix_operator, PrefixOperator::Negate);

        assert_eq!(
            prefix.ty.into_type().unwrap(),
            Type::Primitive(PrimitiveType::Float32)
        );
    }

    {
        // -3u32
        assert!(builder
            .bind_prefix(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[3]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_prefix()
                    .unwrap(),
            )
            .is_none());

        let err = std::mem::take(&mut builder.errors);
        assert_eq!(err.len(), 1);
        let err = &err[0].as_invalid_prefix_operation().unwrap();

        assert_eq!(err.source_span.source_code(), "-3u32");
        assert_eq!(err.prefix_operator, PrefixOperator::Negate);
        assert_eq!(
            err.operand_type,
            Type::Primitive(PrimitiveType::Uint32).into()
        );
    }

    {
        // !4
        assert!(builder
            .bind_prefix(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[4]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_prefix()
                    .unwrap(),
            )
            .is_none());

        let err = std::mem::take(&mut builder.errors);
        assert_eq!(err.len(), 1);
        let err = &err[0].as_invalid_prefix_operation().unwrap();

        assert_eq!(err.source_span.source_code(), "!4");
        assert_eq!(err.prefix_operator, PrefixOperator::LogicalNot);
        assert_eq!(err.operand_type, Constraint::Number.into());
    }

    {
        // !false
        let prefix = builder
            .bind_prefix(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[5]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_prefix()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(prefix.source_span.source_code(), "!false");
        assert_eq!(prefix.prefix_operator, PrefixOperator::LogicalNot);
        assert_eq!(prefix.operand.type_binding().ty, prefix.ty);
        assert_eq!(
            prefix.operand.type_binding().ty,
            IntermediateType::Type(PrimitiveType::Bool.into())
        );
    }

    Ok(())
}

#[test]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
fn binary_binding_test() -> Result<(), Box<dyn Error>> {
    let file_parsing = pernixc_syntax::file_parsing::parse_files(SourceFile::load(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
            .join("hir")
            .join("binaryBinding.pnx"),
        vec!["test".to_string()],
    )?)?;

    let (symbol_table, errors) = super::Table::analyze(file_parsing.into_iter());
    assert!(errors.is_empty());

    // targetting test::main
    let mut builder = Builder::new(
        &symbol_table,
        symbol_table
            .get_id_by_full_name(["test", "main"].into_iter())
            .unwrap()
            .into_function()
            .unwrap(),
    );

    {
        // 0 + 1
        let binary = builder
            .bind_binary(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[0]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_binary()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(binary.source_span.source_code(), "0 + 1");
        assert_eq!(binary.binary_operator, BinaryOperator::Add);
        assert_eq!(
            builder.get_inferrable_type(&binary.left_operand.type_binding().ty),
            builder.get_inferrable_type(&binary.right_operand.type_binding().ty)
        );
        assert_eq!(
            builder.get_inferrable_type(&binary.type_binding.ty),
            builder.get_inferrable_type(&binary.left_operand.type_binding().ty),
        );
        assert_eq!(
            builder.get_inferrable_type(&binary.type_binding.ty),
            builder.get_inferrable_type(&binary.right_operand.type_binding().ty),
        );

        assert_eq!(
            *builder
                .inference_context
                .get_inference(binary.type_binding.ty.into_inference().unwrap())
                .as_type_variable()
                .unwrap()
                .constraint(),
            Constraint::Number
        );
    }

    {
        // 2 + -3
        let binary = builder
            .bind_binary(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[1]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_binary()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(binary.source_span.source_code(), "2 + -3");
        assert_eq!(binary.binary_operator, BinaryOperator::Add);
        assert_eq!(
            builder.get_inferrable_type(&binary.left_operand.type_binding().ty),
            builder.get_inferrable_type(&binary.right_operand.type_binding().ty)
        );
        assert_eq!(
            builder.get_inferrable_type(&binary.type_binding.ty),
            builder.get_inferrable_type(&binary.left_operand.type_binding().ty),
        );
        assert_eq!(
            builder.get_inferrable_type(&binary.type_binding.ty),
            builder.get_inferrable_type(&binary.right_operand.type_binding().ty),
        );

        assert_eq!(
            *builder
                .inference_context
                .get_inference(binary.type_binding.ty.into_inference().unwrap())
                .as_type_variable()
                .unwrap()
                .constraint(),
            Constraint::Signed
        );
    }

    {
        // -4 + 5.0
        let binary = builder
            .bind_binary(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[2]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_binary()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(binary.source_span.source_code(), "-4 + 5.0");
        assert_eq!(binary.binary_operator, BinaryOperator::Add);
        assert_eq!(
            builder.get_inferrable_type(&binary.left_operand.type_binding().ty),
            builder.get_inferrable_type(&binary.right_operand.type_binding().ty)
        );
        assert_eq!(
            builder.get_inferrable_type(&binary.type_binding.ty),
            builder.get_inferrable_type(&binary.left_operand.type_binding().ty),
        );
        assert_eq!(
            builder.get_inferrable_type(&binary.type_binding.ty),
            builder.get_inferrable_type(&binary.right_operand.type_binding().ty),
        );

        assert_eq!(
            *builder
                .inference_context
                .get_inference(binary.type_binding.ty.into_inference().unwrap())
                .as_type_variable()
                .unwrap()
                .constraint(),
            Constraint::Float
        );
    }

    {
        // 6f32 + 7
        let binary = builder
            .bind_binary(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[3]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_binary()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(binary.source_span.source_code(), "6f32 + 7");
        assert_eq!(binary.binary_operator, BinaryOperator::Add);

        assert_eq!(
            builder.get_inferrable_type(&binary.left_operand.type_binding().ty),
            builder.get_inferrable_type(&binary.right_operand.type_binding().ty)
        );
        assert_eq!(
            builder.get_inferrable_type(&binary.type_binding.ty),
            builder.get_inferrable_type(&binary.left_operand.type_binding().ty),
        );
        assert_eq!(
            builder.get_inferrable_type(&binary.type_binding.ty),
            builder.get_inferrable_type(&binary.right_operand.type_binding().ty),
        );

        assert_eq!(
            builder.get_inferrable_type(&binary.type_binding.ty),
            InferrableType::Type(PrimitiveType::Float32.into())
        );
    }

    {
        // 8 = 9
        assert!(builder
            .bind_binary(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[4]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_binary()
                    .unwrap(),
            )
            .is_none());

        let err = std::mem::take(&mut builder.errors);
        assert_eq!(err.len(), 1);
        assert!(err[0].as_assign_to_r_value().is_some());
    }

    {
        // 10 == 11
        let binary = builder
            .bind_binary(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[5]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_binary()
                    .unwrap(),
            )
            .unwrap();

        assert_eq!(binary.source_span.source_code(), "10 == 11");
        assert_eq!(binary.binary_operator, BinaryOperator::Equal);
        assert_eq!(
            builder.get_inferrable_type(&binary.left_operand.type_binding().ty),
            builder.get_inferrable_type(&binary.right_operand.type_binding().ty)
        );
        assert_eq!(
            builder.get_inferrable_type(&binary.type_binding.ty),
            InferrableType::Type(PrimitiveType::Bool.into())
        );
    }

    Ok(())
}

#[test]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
fn named_binding_test() -> Result<(), Box<dyn Error>> {
    let file_parsing = pernixc_syntax::file_parsing::parse_files(SourceFile::load(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
            .join("hir")
            .join("namedBinding.pnx"),
        vec!["test".to_string()],
    )?)?;

    let (symbol_table, errors) = super::Table::analyze(file_parsing.into_iter());
    assert!(errors.is_empty());

    // targetting test::main
    let mut builder = Builder::new(
        &symbol_table,
        symbol_table
            .get_id_by_full_name(["test", "main"].into_iter())
            .unwrap()
            .into_function()
            .unwrap(),
    );

    {
        // let value = 32;
        let variable_address = builder
            .bind_variable_declaration(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[0]
                    .as_declarative()
                    .unwrap()
                    .as_variable_declaration()
                    .unwrap(),
            )
            .unwrap();

        // no error
        assert!(builder.errors.is_empty());

        assert_eq!(
            builder.get_inferrable_type(&builder.variable_manager[variable_address.variable_id].ty),
            InferrableType::Inferring(Constraint::Number)
        );
    }

    {
        // someParameter
        let parameter = builder
            .bind_named(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[1]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_named()
                    .unwrap(),
            )
            .unwrap();

        // no error
        assert!(builder.errors.is_empty());

        let parameter = parameter.into_load().unwrap();
        assert_eq!(
            parameter.ty.into_type().unwrap(),
            PrimitiveType::Int32.into()
        );
        assert!(parameter.lvalue.is_mutable);
        assert_eq!(
            builder.function.parameters[parameter
                .lvalue
                .address
                .into_argument_address()
                .unwrap()
                .parameter_id]
                .name,
            "someParameter"
        );
    }

    {
        // value
        let variable = builder.bind_named(
            builder
                .function
                .syntax_tree
                .syntax_tree
                .block_without_label
                .statements[2]
                .as_expressive()
                .unwrap()
                .as_semi()
                .unwrap()
                .expression
                .as_named()
                .unwrap(),
        );
        let variable = variable.unwrap();

        // no error
        assert!(builder.errors.is_empty());

        let variable = variable.into_load().unwrap();
        assert_eq!(
            builder
                .get_inferrable_type(&variable.ty)
                .into_inferring()
                .unwrap(),
            Constraint::Number
        );
    }

    {
        // SomeEnum
        assert!(builder
            .bind_named(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[3]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_named()
                    .unwrap(),
            )
            .is_none());

        // error
        assert_eq!(builder.errors.len(), 1);
        let err = builder
            .errors
            .pop()
            .unwrap()
            .into_expression_expected()
            .unwrap();
        assert_eq!(err.source_span.source_code(), "SomeEnum");
    }

    {
        // SomeEnum::First
        let named = builder
            .bind_named(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[4]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_named()
                    .unwrap(),
            )
            .unwrap();

        let enum_literal = named.into_enum_literal().unwrap();
        assert_eq!(symbol_table.get(enum_literal.enum_id).name, "SomeEnum");
        assert_eq!(symbol_table.get(enum_literal.enum_variant_id).name, "First");
    }

    {
        // mutable let someParameter = 32;
        let variable_address = builder
            .bind_variable_declaration(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[5]
                    .as_declarative()
                    .unwrap()
                    .as_variable_declaration()
                    .unwrap(),
            )
            .unwrap();

        // no error
        assert!(builder.errors.is_empty());

        assert_eq!(
            builder.get_inferrable_type(&builder.variable_manager[variable_address.variable_id].ty),
            InferrableType::Inferring(Constraint::Number)
        );
    }

    {
        // someParameter = 5i64;
        let assignment = builder
            .bind_binary(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[6]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_binary()
                    .unwrap(),
            )
            .unwrap();

        let left = assignment
            .left_operand
            .into_named()
            .unwrap()
            .into_load()
            .unwrap();

        assert_eq!(
            builder.get_inferrable_type(&left.ty).into_type().unwrap(),
            PrimitiveType::Int64.into()
        );
        assert!(left.lvalue.is_mutable);
        let variable = &builder.variable_manager[left
            .lvalue
            .address
            .into_variable_address()
            .unwrap()
            .variable_id];
        assert_eq!(
            builder
                .get_inferrable_type(&variable.ty)
                .into_type()
                .unwrap(),
            PrimitiveType::Int64.into()
        );
        assert_eq!(
            variable.usage.as_user_defined_variable().unwrap().name,
            "someParameter"
        );
    }

    Ok(())
}

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn function_call_binding() -> Result<(), Box<dyn Error>> {
    let file_parsing = pernixc_syntax::file_parsing::parse_files(SourceFile::load(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
            .join("hir")
            .join("functionCallBinding.pnx"),
        vec!["test".to_string()],
    )?)?;

    let (symbol_table, errors) = super::Table::analyze(file_parsing.into_iter());
    assert!(errors.is_empty());

    let add_function_symbol_id = symbol_table
        .get_id_by_full_name(["test", "add"].into_iter())
        .unwrap()
        .into_function()
        .unwrap();

    // targetting test::main
    let mut builder = Builder::new(
        &symbol_table,
        symbol_table
            .get_id_by_full_name(["test", "main"].into_iter())
            .unwrap()
            .into_function()
            .unwrap(),
    );

    {
        // add(1, 2)
        let function_call = builder
            .bind_function_call(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[0]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_function_call()
                    .unwrap(),
            )
            .unwrap();

        // no error
        assert!(builder.errors.is_empty());

        assert_eq!(function_call.function_id, add_function_symbol_id);
        assert_eq!(function_call.arguments.len(), 2);
        assert_eq!(
            function_call.arguments[0]
                .as_numeric_literal()
                .unwrap()
                .source_span
                .source_code(),
            "1"
        );
        assert_eq!(
            function_call.arguments[1]
                .as_numeric_literal()
                .unwrap()
                .source_span
                .source_code(),
            "2"
        );
        assert_eq!(
            builder
                .get_inferrable_type(&function_call.arguments[0].type_binding().ty)
                .into_type()
                .unwrap(),
            PrimitiveType::Int32.into()
        );
        assert_eq!(
            builder
                .get_inferrable_type(&function_call.arguments[1].type_binding().ty)
                .into_type()
                .unwrap(),
            PrimitiveType::Int32.into()
        );
        assert_eq!(
            function_call.return_type.into_type().unwrap(),
            PrimitiveType::Int32.into()
        );
    }

    {
        // add(true, false)
        let function_call = builder
            .bind_function_call(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[1]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_function_call()
                    .unwrap(),
            )
            .unwrap();

        // 2 errors
        assert_eq!(builder.errors.len(), 2);

        let mut errors = std::mem::take(&mut builder.errors);
        let error = errors.remove(0).into_type_mismatch().unwrap();
        assert_eq!(error.expected, Type::Primitive(PrimitiveType::Int32).into());
        assert_eq!(error.actual, Type::Primitive(PrimitiveType::Bool).into());
        assert_eq!(error.source_span.source_code(), "true");

        let error = errors.remove(0).into_type_mismatch().unwrap();
        assert_eq!(error.expected, Type::Primitive(PrimitiveType::Int32).into());
        assert_eq!(error.actual, Type::Primitive(PrimitiveType::Bool).into());
        assert_eq!(error.source_span.source_code(), "false");

        assert_eq!(function_call.function_id, add_function_symbol_id);
    }

    {
        // add(3.0, 4)
        let function_call = builder
            .bind_function_call(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[2]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_function_call()
                    .unwrap(),
            )
            .unwrap();

        // one error
        assert_eq!(builder.errors.len(), 1);

        let error = builder.errors.remove(0).into_type_mismatch().unwrap();
        assert_eq!(error.expected, Type::Primitive(PrimitiveType::Int32).into());
        assert_eq!(error.actual.into_inferring().unwrap(), Constraint::Float);

        assert_eq!(function_call.function_id, add_function_symbol_id);
    }

    {
        // add()
        let function_call = builder
            .bind_function_call(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[3]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_function_call()
                    .unwrap(),
            )
            .unwrap();

        // one error
        assert_eq!(builder.errors.len(), 1);

        let error = builder
            .errors
            .remove(0)
            .into_argument_count_mismatch()
            .unwrap();
        assert_eq!(error.expected, 2);
        assert_eq!(error.found, 0);
        assert_eq!(function_call.function_id, add_function_symbol_id);
    }

    {
        // add(true, false, true)
        let function_call = builder
            .bind_function_call(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[4]
                    .as_expressive()
                    .unwrap()
                    .as_semi()
                    .unwrap()
                    .expression
                    .as_function_call()
                    .unwrap(),
            )
            .unwrap();

        // three errors
        assert_eq!(builder.errors.len(), 3);

        let errors = std::mem::take(&mut builder.errors);
        assert!(errors.iter().any(|f| {
            let SemanticError::ArgumentCountMismatch(argument) = f else {
                return false;
            };

            argument.expected == 2 && argument.found == 3
        }));
        assert!(errors.iter().any(|f| {
            let SemanticError::TypeMismatch(type_mismatch) = f else {
                return false;
            };

            type_mismatch.expected == Type::Primitive(PrimitiveType::Int32).into()
                && type_mismatch.actual == Type::Primitive(PrimitiveType::Bool).into()
                && type_mismatch.source_span.source_code() == "true"
        }));
        assert!(errors.iter().any(|f| {
            let SemanticError::TypeMismatch(type_mismatch) = f else {
                return false;
            };

            type_mismatch.expected == Type::Primitive(PrimitiveType::Int32).into()
                && type_mismatch.actual == Type::Primitive(PrimitiveType::Bool).into()
                && type_mismatch.source_span.source_code() == "false"
        }));

        assert_eq!(function_call.function_id, add_function_symbol_id);
    }

    Ok(())
}

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn struct_literal_binding_test() -> Result<(), Box<dyn Error>> {
    let file_parsing = pernixc_syntax::file_parsing::parse_files(SourceFile::load(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
            .join("hir")
            .join("structLiteralBinding.pnx"),
        vec!["test".to_string()],
    )?)?;

    let (symbol_table, errors) = super::Table::analyze(file_parsing.into_iter());
    assert!(errors.is_empty());

    let vector2_struct = symbol_table
        .get_id_by_full_name(["test", "Vector2"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();

    // targetting test::main
    let mut builder = Builder::new(
        &symbol_table,
        symbol_table
            .get_id_by_full_name(["test", "main"].into_iter())
            .unwrap()
            .into_function()
            .unwrap(),
    );

    {
        // Vector2 { x: 0, y: 1 }
        assert!(builder.bind_statement(
            &builder
                .function
                .syntax_tree
                .syntax_tree
                .block_without_label
                .statements[0],
        ));

        // no error
        assert!(builder.errors.is_empty());

        // should be a store instruction
        let struct_store = builder.control_flow_graph[builder.current_basic_block_id][0]
            .as_ir_instruction()
            .unwrap()
            .as_store()
            .unwrap();

        assert_eq!(
            *builder.variable_manager[struct_store.variable_id]
                .ty
                .as_type()
                .unwrap(),
            Type::TypedID(vector2_struct.into())
        );
        assert_eq!(
            struct_store
                .initialization
                .as_struct_initialization()
                .unwrap()
                .field_initializations
                .len(),
            2
        );
        assert_eq!(
            struct_store
                .initialization
                .as_struct_initialization()
                .unwrap()
                .field_initializations[0]
                .0,
            symbol_table
                .get(vector2_struct)
                .fields
                .map_name_to_id("x")
                .unwrap()
        );
        assert_eq!(
            struct_store
                .initialization
                .as_struct_initialization()
                .unwrap()
                .field_initializations[1]
                .0,
            symbol_table
                .get(vector2_struct)
                .fields
                .map_name_to_id("y")
                .unwrap()
        );
        let struct_load = builder.control_flow_graph[builder.current_basic_block_id][1]
            .as_ir_instruction()
            .unwrap()
            .as_evaluate_value()
            .unwrap()
            .as_struct_literal()
            .unwrap();

        assert_eq!(struct_load.struct_id, vector2_struct);
        assert_eq!(struct_load.variable_id, struct_store.variable_id);
    }

    {
        // let vector = Vector2 { x: 2, y: 3 }
        let variable_address = builder
            .bind_variable_declaration(
                builder
                    .function
                    .syntax_tree
                    .syntax_tree
                    .block_without_label
                    .statements[1]
                    .as_declarative()
                    .unwrap()
                    .as_variable_declaration()
                    .unwrap(),
            )
            .unwrap();

        // no error
        assert!(builder.errors.is_empty());

        // should be a store instruction
        let struct_store = builder.control_flow_graph[builder.current_basic_block_id][2]
            .as_ir_instruction()
            .unwrap()
            .as_store()
            .unwrap();

        assert_eq!(struct_store.variable_id, variable_address.variable_id);
        assert_eq!(
            *builder.variable_manager[struct_store.variable_id]
                .ty
                .as_type()
                .unwrap(),
            Type::TypedID(vector2_struct.into())
        );
    }

    Ok(())
}
