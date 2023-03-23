use std::error::Error;

use pernixc_common::source_file::{SourceFile, Span, SpanEnding};
use pernixc_lexical::token_stream::TokenStream;

use crate::{
    parser::Parser,
    syntax_tree::{expression::BinaryOperatorSyntaxTree, SyntaxTree},
};

fn substr_span(source_file: &SourceFile, span: Span) -> &str {
    match span.end {
        SpanEnding::Location(end_location) => {
            &source_file.content()[span.start.byte..end_location.byte]
        }
        SpanEnding::EndOfFile => &source_file.content()[span.start.byte..],
    }
}

const SOURCE_CODE: &str = "
'test: {
    Some::Struct::Name test = 32;
    test = 64;
    if (test == 64) {
        test = 128;
    }
    else {
        test = 256;
    }

    loop SomeFunction::Test()

    express 'test someValue;
}
";

#[test]
fn statements_in_block_test() -> Result<(), Box<dyn Error>> {
    let source_file = SourceFile::new("test.pnx".to_string(), SOURCE_CODE.to_string());
    let (token_stream, _) = TokenStream::tokenize(source_file.iter());
    let mut cursor = token_stream.cursor();
    cursor.next_token();

    let mut parser = Parser::new(cursor)?;

    let block = parser
        .parse_expression()
        .unwrap()
        .into_imperative_expression()
        .unwrap()
        .into_block_expression()
        .unwrap();

    assert_eq!(
        substr_span(
            &source_file,
            block.label_specifier.unwrap().label.identifier.span
        ),
        "test"
    );
    let mut statement_iter = block.statements.into_iter();
    {
        let variable_declaration = statement_iter
            .next()
            .unwrap()
            .into_declaration()
            .unwrap()
            .into_variable_declaration()
            .unwrap();

        let type_binding = variable_declaration
            .variable_type_binding
            .into_type_binding()
            .unwrap();

        assert!(type_binding.mutable_keyword.is_none());
        let type_specifier = type_binding.type_specifier.into_qualified().unwrap();
        assert_eq!(
            substr_span(&source_file, type_specifier.span()),
            "Some::Struct::Name"
        );

        // check variable name
        assert_eq!(
            substr_span(&source_file, variable_declaration.identifier.span),
            "test"
        );

        // check expression
        let expression = variable_declaration
            .expression
            .into_functional_expression()
            .unwrap()
            .into_numeric_literal()
            .unwrap()
            .0;
        assert_eq!(substr_span(&source_file, expression.span), "32");
    }

    {
        let binary_expression = statement_iter
            .next()
            .unwrap()
            .into_expression()
            .unwrap()
            .into_functional_expresion()
            .unwrap()
            .expression
            .into_binary_expression()
            .unwrap();

        // check lhs expression
        let left = binary_expression
            .left
            .into_functional_expression()
            .unwrap()
            .into_identifier_expression()
            .unwrap()
            .0;
        assert_eq!(substr_span(&source_file, left.span()), "test");

        // check assignment operator
        assert!(matches!(
            binary_expression.operator,
            BinaryOperatorSyntaxTree::Assign(_)
        ));

        // check rhs expression
        let right = binary_expression
            .right
            .into_functional_expression()
            .unwrap()
            .into_numeric_literal()
            .unwrap()
            .0;
        assert_eq!(substr_span(&source_file, right.span()), "64");
    }

    {
        let if_else_expression = statement_iter
            .next()
            .unwrap()
            .into_expression()
            .unwrap()
            .into_imperative_expression()
            .unwrap()
            .into_if_else_expression()
            .unwrap();

        // check condition
        {
            let condition = if_else_expression
                .condition
                .into_functional_expression()
                .unwrap()
                .into_binary_expression()
                .unwrap();

            let left = condition
                .left
                .into_functional_expression()
                .unwrap()
                .into_identifier_expression()
                .unwrap()
                .0;
            assert_eq!(substr_span(&source_file, left.span()), "test");

            assert!(matches!(
                condition.operator,
                BinaryOperatorSyntaxTree::Equal(..)
            ));

            let right = condition
                .right
                .into_functional_expression()
                .unwrap()
                .into_numeric_literal()
                .unwrap()
                .0;
            assert_eq!(substr_span(&source_file, right.span()), "64");
        }

        // check then expression
        {
            let then_expression = if_else_expression
                .then_expression
                .into_imperative_expression()
                .unwrap()
                .into_block_expression()
                .unwrap();

            let mut statement_iter = then_expression.statements.into_iter();

            assert!(then_expression.label_specifier.is_none());

            let binary_expression = statement_iter
                .next()
                .unwrap()
                .into_expression()
                .unwrap()
                .into_functional_expresion()
                .unwrap()
                .expression
                .into_binary_expression()
                .unwrap();

            let left = binary_expression
                .left
                .into_functional_expression()
                .unwrap()
                .into_identifier_expression()
                .unwrap()
                .0;
            assert_eq!(substr_span(&source_file, left.span()), "test");

            assert!(matches!(
                binary_expression.operator,
                BinaryOperatorSyntaxTree::Assign(..)
            ));

            let right = binary_expression
                .right
                .into_functional_expression()
                .unwrap()
                .into_numeric_literal()
                .unwrap()
                .0;
            assert_eq!(substr_span(&source_file, right.span()), "128");
        }

        // check else expression
        {
            let else_expression = if_else_expression
                .else_expression
                .unwrap()
                .expression
                .into_imperative_expression()
                .unwrap()
                .into_block_expression()
                .unwrap();

            let mut statement_iter = else_expression.statements.into_iter();

            assert!(else_expression.label_specifier.is_none());

            let binary_expression = statement_iter
                .next()
                .unwrap()
                .into_expression()
                .unwrap()
                .into_functional_expresion()
                .unwrap()
                .expression
                .into_binary_expression()
                .unwrap();

            let left = binary_expression
                .left
                .into_functional_expression()
                .unwrap()
                .into_identifier_expression()
                .unwrap()
                .0;
            assert_eq!(substr_span(&source_file, left.span()), "test");

            assert!(matches!(
                binary_expression.operator,
                BinaryOperatorSyntaxTree::Assign(..)
            ));

            let right = binary_expression
                .right
                .into_functional_expression()
                .unwrap()
                .into_numeric_literal()
                .unwrap()
                .0;
            assert_eq!(substr_span(&source_file, right.span()), "256");
        }
    }

    {
        let loop_expression = statement_iter
            .next()
            .unwrap()
            .into_expression()
            .unwrap()
            .into_imperative_expression()
            .unwrap()
            .into_loop_expression()
            .unwrap();

        let function_call = loop_expression
            .expression
            .into_functional_expression()
            .unwrap()
            .into_function_call_expression()
            .unwrap();

        assert!(function_call.arguments.is_none());

        assert_eq!(
            substr_span(&source_file, function_call.qualified_identifier.span()),
            "SomeFunction::Test"
        );
    }

    {
        let express_statement = statement_iter
            .next()
            .unwrap()
            .into_expression()
            .unwrap()
            .into_functional_expresion()
            .unwrap()
            .expression
            .into_express_expression()
            .unwrap();

        assert_eq!(
            substr_span(
                &source_file,
                express_statement.label.unwrap().identifier.span()
            ),
            "test"
        );

        let identifier_expression = express_statement
            .expression
            .unwrap()
            .into_functional_expression()
            .unwrap()
            .into_identifier_expression()
            .unwrap()
            .0;

        assert_eq!(
            substr_span(&source_file, identifier_expression.span()),
            "someValue"
        );
    }

    Ok(())
}
