use std::error::Error;

use pernixc_common::source_file::{Iterator, Span, SpanEnding};
use pernixc_lexical::token_stream::TokenStream;

use crate::{
    parser::Parser,
    syntax_tree::{expression::BinaryOperator, SourceElement},
};

fn substr_span(source_code: &str, span: Span) -> &str {
    match span.end {
        SpanEnding::Location(end_location) => &source_code[span.start.byte..end_location.byte],
        SpanEnding::EndOfFile => &source_code[span.start.byte..],
    }
}

const SOURCE_CODE: &str = "
'test: {
    Some::Struct::Name test = 32;
    SomeFunction;
    test = 64;
    if (test == 64) {
        test = 128;
    }
    else {
        test = 256;
    }

    loop SomeFunction::Test()

    express 'test someValue;

    (SomeType)42;
}
";

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn statements_in_block_test() -> Result<(), Box<dyn Error>> {
    let (token_stream, _) = TokenStream::tokenize(Iterator::new(SOURCE_CODE));
    let mut cursor = token_stream.cursor();
    cursor.next_token();

    let mut parser = Parser::new(cursor)?;

    let block = parser
        .parse_expression()
        .unwrap()
        .into_imperative()
        .unwrap()
        .into_block()
        .unwrap();

    assert_eq!(
        substr_span(
            SOURCE_CODE,
            block.label_specifier.unwrap().label.identifier.span
        ),
        "test"
    );
    let mut statement_iter = block.statements.into_iter();
    {
        let variable_declaration = statement_iter
            .next()
            .unwrap()
            .into_declarative()
            .unwrap()
            .into_variable_declaration()
            .unwrap();

        let type_binding_specifier = variable_declaration
            .variable_type_binding_specifier
            .into_type_binding_specifier()
            .unwrap();

        assert!(type_binding_specifier.mutable_keyword.is_none());
        let type_specifier = type_binding_specifier
            .type_specifier
            .into_qualified_identifier()
            .unwrap();
        assert_eq!(
            substr_span(SOURCE_CODE, type_specifier.span()),
            "Some::Struct::Name"
        );

        // check variable name
        assert_eq!(
            substr_span(SOURCE_CODE, variable_declaration.identifier.span),
            "test"
        );

        // check expression
        let expression = variable_declaration
            .expression
            .into_functional()
            .unwrap()
            .into_numeric_literal()
            .unwrap();
        assert_eq!(substr_span(SOURCE_CODE, expression.span), "32");
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

        assert_eq!(
            substr_span(SOURCE_CODE, identifier_expression.span()),
            "SomeFunction"
        );
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
            .left
            .into_functional()
            .unwrap()
            .into_named()
            .unwrap();
        assert_eq!(substr_span(SOURCE_CODE, left.span()), "test");

        // check assignment operator
        assert!(matches!(
            binary_expression.operator,
            BinaryOperator::Assign(..)
        ));

        // check rhs expression
        let right = binary_expression
            .right
            .into_functional()
            .unwrap()
            .into_numeric_literal()
            .unwrap();
        assert_eq!(substr_span(SOURCE_CODE, right.span()), "64");
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
                .left
                .into_functional()
                .unwrap()
                .into_named()
                .unwrap();
            assert_eq!(substr_span(SOURCE_CODE, left.span()), "test");

            assert!(matches!(condition.operator, BinaryOperator::Equal(..)));

            let right = condition
                .right
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(substr_span(SOURCE_CODE, right.span()), "64");
        }

        // check then expression
        {
            let then_expression = if_else_expression
                .then_expression
                .into_imperative()
                .unwrap()
                .into_block()
                .unwrap();

            let mut statement_iter = then_expression.statements.into_iter();

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
                .left
                .into_functional()
                .unwrap()
                .into_named()
                .unwrap();
            assert_eq!(substr_span(SOURCE_CODE, left.span()), "test");

            assert!(matches!(
                binary_expression.operator,
                BinaryOperator::Assign(..)
            ));

            let right = binary_expression
                .right
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(substr_span(SOURCE_CODE, right.span()), "128");
        }

        // check else expression
        {
            let else_expression = if_else_expression
                .else_expression
                .unwrap()
                .expression
                .into_imperative()
                .unwrap()
                .into_block()
                .unwrap();

            let mut statement_iter = else_expression.statements.into_iter();

            assert!(else_expression.label_specifier.is_none());

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
                .left
                .into_functional()
                .unwrap()
                .into_named()
                .unwrap();
            assert_eq!(substr_span(SOURCE_CODE, left.span()), "test");

            assert!(matches!(
                binary_expression.operator,
                BinaryOperator::Assign(..)
            ));

            let right = binary_expression
                .right
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(substr_span(SOURCE_CODE, right.span()), "256");
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

        let function_call = loop_expression
            .expression
            .into_functional()
            .unwrap()
            .into_function_call()
            .unwrap();

        assert!(function_call.arguments.is_none());

        assert_eq!(
            substr_span(SOURCE_CODE, function_call.qualified_identifier.span()),
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
            substr_span(
                SOURCE_CODE,
                express_statement.label.unwrap().identifier.span()
            ),
            "test"
        );

        let identifier_expression = express_statement
            .expression
            .unwrap()
            .into_functional()
            .unwrap()
            .into_named()
            .unwrap();

        assert_eq!(
            substr_span(SOURCE_CODE, identifier_expression.span()),
            "someValue"
        );
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
            substr_span(
                SOURCE_CODE,
                cast_expression
                    .type_specifier
                    .into_qualified_identifier()
                    .unwrap()
                    .span()
            ),
            "SomeType"
        );

        let numeric_literal_expression = cast_expression
            .expression
            .into_functional()
            .unwrap()
            .into_numeric_literal()
            .unwrap();

        assert_eq!(
            substr_span(SOURCE_CODE, numeric_literal_expression.span()),
            "42"
        );
    }

    Ok(())
}
