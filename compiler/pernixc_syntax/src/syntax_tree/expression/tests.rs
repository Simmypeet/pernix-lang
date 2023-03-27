use pernixc_common::source_file::{Iterator, Span, SpanEnding};
use pernixc_lexical::token_stream::TokenStream;
use proptest::{
    prop_assert_eq, prop_oneof, proptest,
    strategy::{Just, Strategy},
};

use crate::{
    parser::Parser,
    syntax_tree::{expression::BinaryOperator, SourceElement},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum PrefixOperator {
    LogicalNot,
    Negate,
}

fn prefix_operator_strategy() -> impl Strategy<Value = Vec<PrefixOperator>> {
    proptest::collection::vec(
        prop_oneof![
            Just(PrefixOperator::LogicalNot),
            Just(PrefixOperator::Negate),
        ],
        0..=10,
    )
}

fn member_access_strategy() -> impl Strategy<Value = Vec<String>> {
    proptest::collection::vec(
        proptest::string::string_regex("_[a-zA-Z][_a-zA-Z0-9]*").unwrap(),
        1..=10,
    )
}

fn substr_span(source_code: &str, span: Span) -> &str {
    match span.end {
        SpanEnding::Location(end_location) => &source_code[span.start.byte..end_location.byte],
        SpanEnding::EndOfFile => &source_code[span.start.byte..],
    }
}

proptest! {
    #[test]
    fn prefix_operator_test(operators in prefix_operator_strategy()) {
        let mut string = operators.iter().map(|operator| match operator {
            PrefixOperator::LogicalNot => "!",
            PrefixOperator::Negate => "-",
        }).collect::<String>();
        string.push('1');

        let (token_stream, _) = TokenStream::tokenize(Iterator::new(&string));
        let mut cursor = token_stream.cursor();
        cursor.next_token();
        let mut parser = Parser::new(cursor).unwrap();

        let mut expression = parser.parse_primary_expression().unwrap();

        for original_operator in operators {
            let prefix_expr = expression.into_functional()
                .unwrap()
                .into_prefix()
                .unwrap();
            match prefix_expr.operator {
                super::PrefixOperator::LogicalNot(..) => prop_assert_eq!(original_operator, PrefixOperator::LogicalNot),
                super::PrefixOperator::Negate(..) => prop_assert_eq!(original_operator, PrefixOperator::Negate),
            }
            expression = *prefix_expr.operand;
        }

        expression.into_functional().unwrap().into_numeric_literal().unwrap();
    }

    #[test]
    fn member_access_test(identifiers in member_access_strategy()) {
        let string = format!("1.{}", identifiers.join("."));
        let (token_stream, _) = TokenStream::tokenize(Iterator::new(&string));
        let mut cursor = token_stream.cursor();
        cursor.next_token();
        let mut parser = Parser::new(cursor).unwrap();

        let mut expression = parser.parse_primary_expression().unwrap();

        for original_identifier in identifiers.iter().rev() {
            let member_access_expr = expression.into_functional().unwrap().into_member_access().unwrap();
            prop_assert_eq!(original_identifier, substr_span(&string, member_access_expr.identifier.span));
            expression = *member_access_expr.expression;
        }

        expression.into_functional().unwrap().into_numeric_literal().unwrap();
    }
}

#[test]
#[allow(clippy::too_many_lines)]
fn binary_operator_test() {
    let source_code = "!1 + 2 * 3 - 4 / 5 % 6";
    /*
    Expected Tree:
    - BinaryOperatorExpression
        - BiaryOperatorExpression
            - PrefixOperatorExpression
                - PrefixOperator: !
                - NumericLiteral: 1
            - BinaryOperator: +
            - BinaryOperatorExpression
                - NumericLiteral: 2
                - BinaryOperator: *
                - NumericLiteral: 3
        - BinaryOperator: -
        - BinaryOperatorExpression
            - BinaryOperatorExpression
                - NumericLiteral: 4
                - BinaryOperator: /
                - NumericLiteral: 5
            - BinaryOperator: %
            - NumericLiteral: 6
    */

    let (token_stream, _) = TokenStream::tokenize(Iterator::new(source_code));
    let mut cursor = token_stream.cursor();
    cursor.next_token();

    let mut parser = Parser::new(cursor).unwrap();

    let expression = parser
        .parse_expression()
        .unwrap()
        .into_functional()
        .unwrap()
        .into_binary()
        .unwrap();

    assert!(matches!(expression.operator, BinaryOperator::Subtract(..)));

    let left = expression
        .left
        .into_functional()
        .unwrap()
        .into_binary()
        .unwrap();
    {
        let expression = left;

        assert!(matches!(expression.operator, BinaryOperator::Add(..)));

        let left = expression
            .left
            .into_functional()
            .unwrap()
            .into_prefix()
            .unwrap();
        {
            let expression = left;
            assert!(matches!(
                expression.operator,
                super::PrefixOperator::LogicalNot(..)
            ));
            let operand = expression
                .operand
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(substr_span(source_code, operand.span()), "1");
        }

        let right = expression
            .right
            .into_functional()
            .unwrap()
            .into_binary()
            .unwrap();
        {
            let expression = right;

            assert!(matches!(expression.operator, BinaryOperator::Multiply(..)));

            let left = expression
                .left
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(substr_span(source_code, left.span()), "2");

            let right = expression
                .right
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(substr_span(source_code, right.span()), "3");
        }
    }
    let right = expression
        .right
        .into_functional()
        .unwrap()
        .into_binary()
        .unwrap();
    {
        let expression = right;

        assert!(matches!(expression.operator, BinaryOperator::Modulo(..)));

        let left = expression
            .left
            .into_functional()
            .unwrap()
            .into_binary()
            .unwrap();
        {
            let expression = left;

            assert!(matches!(expression.operator, BinaryOperator::Divide(..)));

            let left = expression
                .left
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(substr_span(source_code, left.span()), "4");

            let right = expression
                .right
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(substr_span(source_code, right.span()), "5");
        }

        let right = expression
            .right
            .into_functional()
            .unwrap()
            .into_numeric_literal()
            .unwrap();
        assert_eq!(substr_span(source_code, right.span()), "6");
    }
}

#[test]
fn identifier_expression_test() {
    let source_code = "Qualified::Identifier Hello(1, 2) World() Test { yeah: 1 }";
    let (token_stream, _) = TokenStream::tokenize(Iterator::new(source_code));
    let mut cursor = token_stream.cursor();
    cursor.next_token();

    let mut parser = Parser::new(cursor).unwrap();

    // Qualified::Identifier
    {
        let expression = parser
            .parse_primary_expression()
            .unwrap()
            .into_functional()
            .unwrap()
            .into_named()
            .unwrap();

        assert_eq!(
            substr_span(source_code, expression.span()),
            "Qualified::Identifier"
        );
    }

    // Hello(1, 2)
    {
        let mut expression = parser
            .parse_primary_expression()
            .unwrap()
            .into_functional()
            .unwrap()
            .into_function_call()
            .unwrap();

        assert_eq!(
            substr_span(source_code, expression.qualified_identifier.span()),
            "Hello"
        );

        let arguments = expression.arguments.as_mut().unwrap();
        let mut iter = arguments.elements();

        let expression = iter
            .next()
            .unwrap()
            .as_functional()
            .unwrap()
            .as_numeric_literal()
            .unwrap();

        assert_eq!(substr_span(source_code, expression.span()), "1");

        let expression = iter
            .next()
            .unwrap()
            .as_functional()
            .unwrap()
            .as_numeric_literal()
            .unwrap();

        assert_eq!(substr_span(source_code, expression.span()), "2");

        assert!(iter.next().is_none());
    }

    // World()
    {
        let expression = parser
            .parse_primary_expression()
            .unwrap()
            .into_functional()
            .unwrap()
            .into_function_call()
            .unwrap();

        assert_eq!(
            substr_span(source_code, expression.qualified_identifier.span()),
            "World"
        );

        assert!(expression.arguments.is_none());
    }

    // Test { yeah: 1 }
    {
        let mut expression = parser
            .parse_primary_expression()
            .unwrap()
            .into_functional()
            .unwrap()
            .into_struct_literal()
            .unwrap();

        assert_eq!(
            substr_span(source_code, expression.qualified_identifier.span()),
            "Test"
        );

        let mut iter = expression
            .field_initializations
            .as_mut()
            .unwrap()
            .elements();

        let field = iter.next().unwrap();

        assert_eq!(substr_span(source_code, field.identifier.span), "yeah");

        let expression = field
            .expression
            .as_functional()
            .unwrap()
            .as_numeric_literal()
            .unwrap();

        assert_eq!(substr_span(source_code, expression.span()), "1");

        assert!(iter.next().is_none());
    }
}
