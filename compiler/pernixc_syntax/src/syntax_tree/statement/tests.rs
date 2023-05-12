use std::{error::Error, path::PathBuf};

use pernixc_lexical::token_stream::TokenStream;
use pernixc_source::SourceFile;
use pernixc_system::error_handler::Dummy;

use crate::{
    parser::Parser,
    syntax_tree::{expression::BinaryOperator, SourceElement},
};

const SOURCE_CODE: &str = "
'test: {
    let test: Some::Struct::Name = 32;
    SomeFunction;
    test = 64;
    if (test == 64) {
        test = 128;
    } else {
        test = 256;
    }

    loop { SomeFunction::Test(); }

    express 'test someValue;

    42 as SomeType;
}
";

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn statements_in_block_test() -> Result<(), Box<dyn Error>> {
    let source_file = SourceFile::new(
        PathBuf::default(),
        "test".to_string(),
        SOURCE_CODE.to_string(),
        vec!["test".to_string()],
    )?;
    let token_stream = TokenStream::tokenize(source_file.iter(), &Dummy);
    let mut cursor = token_stream.cursor();
    cursor.next_token();

    let mut parser = Parser::new(cursor)?;

    let block = parser
        .parse_expression(&Dummy)
        .unwrap()
        .into_imperative()
        .unwrap()
        .into_block()
        .unwrap();

    assert_eq!(
        block.label_specifier.unwrap().label.identifier.span().str(),
        "test"
    );
    let mut statement_iter = block.block_without_label.statements.into_iter();
    {
        let variable_declaration = statement_iter
            .next()
            .unwrap()
            .into_declarative()
            .unwrap()
            .into_variable_declaration()
            .unwrap();

        assert!(variable_declaration.mutable_keyword.is_none());
        let type_specifier = variable_declaration
            .type_annotation
            .unwrap()
            .type_specifier
            .into_qualified_identifier()
            .unwrap();
        assert_eq!(type_specifier.span().str(), "Some::Struct::Name");

        // check variable name
        assert_eq!(variable_declaration.identifier.span().str(), "test");

        // check expression
        let expression = variable_declaration
            .expression
            .into_functional()
            .unwrap()
            .into_numeric_literal()
            .unwrap();
        assert_eq!(expression.span().str(), "32");
    }

    {
        let identifier_expression = statement_iter
            .next()
            .unwrap()
            .into_expressive()
            .unwrap()
            .into_semi()
            .unwrap()
            .expression
            .into_named()
            .unwrap();

        assert_eq!(identifier_expression.span().str(), "SomeFunction");
    }

    {
        let binary_expression = statement_iter
            .next()
            .unwrap()
            .into_expressive()
            .unwrap()
            .into_semi()
            .unwrap()
            .expression
            .into_binary()
            .unwrap();

        // check lhs expression
        let left = binary_expression
            .left_operand
            .into_functional()
            .unwrap()
            .into_named()
            .unwrap();
        assert_eq!(left.span().str(), "test");

        // check assignment operator
        assert!(matches!(
            binary_expression.binary_operator,
            BinaryOperator::Assign(..)
        ));

        // check rhs expression
        let right = binary_expression
            .right_operand
            .into_functional()
            .unwrap()
            .into_numeric_literal()
            .unwrap();
        assert_eq!(right.span().str(), "64");
    }

    {
        let if_else_expression = statement_iter
            .next()
            .unwrap()
            .into_expressive()
            .unwrap()
            .into_imperative()
            .unwrap()
            .into_if_else()
            .unwrap();

        // check condition
        {
            let condition = if_else_expression
                .condition
                .into_functional()
                .unwrap()
                .into_binary()
                .unwrap();

            let left = condition
                .left_operand
                .into_functional()
                .unwrap()
                .into_named()
                .unwrap();
            assert_eq!(left.span().str(), "test");

            assert!(matches!(
                condition.binary_operator,
                BinaryOperator::Equal(..)
            ));

            let right = condition
                .right_operand
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(right.span().str(), "64");
        }

        // check then expression
        {
            let then_expression = if_else_expression.then_expression;

            let mut statement_iter = then_expression.block_without_label.statements.into_iter();

            assert!(then_expression.label_specifier.is_none());

            let binary_expression = statement_iter
                .next()
                .unwrap()
                .into_expressive()
                .unwrap()
                .into_semi()
                .unwrap()
                .expression
                .into_binary()
                .unwrap();

            let left = binary_expression
                .left_operand
                .into_functional()
                .unwrap()
                .into_named()
                .unwrap();
            assert_eq!(left.span().str(), "test");

            assert!(matches!(
                binary_expression.binary_operator,
                BinaryOperator::Assign(..)
            ));

            let right = binary_expression
                .right_operand
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(right.span().str(), "128");
        }

        // check else expression
        {
            let else_expression = if_else_expression.else_expression.unwrap().expression;

            let block = else_expression.into_block().unwrap();
            let mut statement_iter = block.block_without_label.statements.into_iter();

            assert!(block.label_specifier.is_none());

            let binary_expression = statement_iter
                .next()
                .unwrap()
                .into_expressive()
                .unwrap()
                .into_semi()
                .unwrap()
                .expression
                .into_binary()
                .unwrap();

            let left = binary_expression
                .left_operand
                .into_functional()
                .unwrap()
                .into_named()
                .unwrap();
            assert_eq!(left.span().str(), "test");

            assert!(matches!(
                binary_expression.binary_operator,
                BinaryOperator::Assign(..)
            ));

            let right = binary_expression
                .right_operand
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(right.span().str(), "256");
        }
    }

    {
        let loop_expression = statement_iter
            .next()
            .unwrap()
            .into_expressive()
            .unwrap()
            .into_imperative()
            .unwrap()
            .into_loop()
            .unwrap();

        let function_call = loop_expression.block_without_label.statements[0]
            .as_expressive()
            .unwrap()
            .as_semi()
            .unwrap()
            .expression
            .as_function_call()
            .unwrap();

        assert!(function_call.arguments.is_none());

        assert_eq!(
            function_call.qualified_identifier.span().str(),
            "SomeFunction::Test"
        );
    }

    {
        let express_statement = statement_iter
            .next()
            .unwrap()
            .into_expressive()
            .unwrap()
            .into_semi()
            .unwrap()
            .expression
            .into_express()
            .unwrap();

        assert_eq!(
            express_statement.label.unwrap().identifier.span().str(),
            "test"
        );

        let identifier_expression = express_statement
            .expression
            .unwrap()
            .into_functional()
            .unwrap()
            .into_named()
            .unwrap();

        assert_eq!(identifier_expression.span().str(), "someValue");
    }

    {
        let cast_expression = statement_iter
            .next()
            .unwrap()
            .into_expressive()
            .unwrap()
            .into_semi()
            .unwrap()
            .expression
            .into_cast()
            .unwrap();

        assert_eq!(
            cast_expression
                .type_specifier
                .into_qualified_identifier()
                .unwrap()
                .span()
                .str(),
            "SomeType"
        );

        let numeric_literal_expression = cast_expression
            .operand
            .into_functional()
            .unwrap()
            .into_numeric_literal()
            .unwrap();

        assert_eq!(numeric_literal_expression.span().str(), "42");
    }

    Ok(())
}
