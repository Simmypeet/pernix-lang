use std::error::Error;

use pernixc_common::source_file::{SourceFile, Span, SpanEnding};
use pernixc_lexical::token_stream::TokenStream;
use proptest::{
    prop_assert_eq, prop_oneof, proptest,
    strategy::{Just, Strategy},
};

use crate::{
    parser::Parser,
    syntax_tree::{
        expression::{BinaryOperatorSyntaxTree, PerfixOperatorSyntaxTree},
        SyntaxTree,
    },
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

fn substr_span(source_file: &SourceFile, span: Span) -> &str {
    match span.end {
        SpanEnding::Location(end_location) => {
            &source_file.content()[span.start.byte..end_location.byte]
        }
        SpanEnding::EndOfFile => &source_file.content()[span.start.byte..],
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

        let source_file = SourceFile::new("test".to_string(), string);
        let (token_stream, _) = TokenStream::tokenize(source_file.iter());
        let mut cursor = token_stream.cursor();
        cursor.next_token();
        let mut parser = Parser::new(cursor).unwrap();

        let mut expression = parser.parse_primary_expression().unwrap();

        for original_operator in operators {
            let prefix_expr = expression.into_functional_expression().unwrap().into_prefix_expression().unwrap();
            match prefix_expr.operator {
                PerfixOperatorSyntaxTree::LogicalNot(_) => prop_assert_eq!(original_operator, PrefixOperator::LogicalNot),
                PerfixOperatorSyntaxTree::Negate(_) => prop_assert_eq!(original_operator, PrefixOperator::Negate),
            }
            expression = *prefix_expr.operand;
        }

        expression.into_functional_expression().unwrap().into_numeric_literal().unwrap();
    }

    #[test]
    fn member_access_test(identifiers in member_access_strategy()) {
        let string = format!("1.{}", identifiers.join("."));
        let source_file = SourceFile::new("test".to_string(), string);
        let (token_stream, _) = TokenStream::tokenize(source_file.iter());
        let mut cursor = token_stream.cursor();
        cursor.next_token();
        let mut parser = Parser::new(cursor).unwrap();

        let mut expression = parser.parse_primary_expression().unwrap();

        for original_identifier in identifiers.iter().rev() {
            let member_access_expr = expression.into_functional_expression().unwrap().into_member_access_expression().unwrap();
            prop_assert_eq!(original_identifier, substr_span(&source_file, member_access_expr.identifier.span));
            expression = *member_access_expr.expression;
        }

        expression.into_functional_expression().unwrap().into_numeric_literal().unwrap();
    }
}

#[test]
fn binary_operator_test() -> Result<(), Box<dyn Error>> {
    let string = "!1 + 2 * 3 - 4 / 5 % 6".to_string();

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

    let source_file = SourceFile::new("test".to_string(), string);
    let (token_stream, _) = TokenStream::tokenize(source_file.iter());
    let mut cursor = token_stream.cursor();
    cursor.next_token();

    let mut parser = Parser::new(cursor).unwrap();

    let expression = parser
        .parse_expression()
        .unwrap()
        .into_functional_expression()
        .unwrap()
        .into_binary_expression()
        .unwrap();

    assert!(matches!(
        expression.operator,
        BinaryOperatorSyntaxTree::Subtract(_)
    ));

    let left = expression
        .left
        .into_functional_expression()
        .unwrap()
        .into_binary_expression()
        .unwrap();
    {
        let expression = left;

        assert!(matches!(
            expression.operator,
            BinaryOperatorSyntaxTree::Add(_)
        ));

        let left = expression
            .left
            .into_functional_expression()
            .unwrap()
            .into_prefix_expression()
            .unwrap();
        {
            let expression = left;
            assert!(matches!(
                expression.operator,
                PerfixOperatorSyntaxTree::LogicalNot(_)
            ));
            let operand = expression
                .operand
                .into_functional_expression()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(substr_span(&source_file, operand.span()), "1");
        }

        let right = expression
            .right
            .into_functional_expression()
            .unwrap()
            .into_binary_expression()
            .unwrap();
        {
            let expression = right;

            assert!(matches!(
                expression.operator,
                BinaryOperatorSyntaxTree::Multiply(_)
            ));

            let left = expression
                .left
                .into_functional_expression()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(substr_span(&source_file, left.span()), "2");

            let right = expression
                .right
                .into_functional_expression()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(substr_span(&source_file, right.span()), "3");
        }
    }
    let right = expression
        .right
        .into_functional_expression()
        .unwrap()
        .into_binary_expression()
        .unwrap();
    {
        let expression = right;

        assert!(matches!(
            expression.operator,
            BinaryOperatorSyntaxTree::Modulo(_)
        ));

        let left = expression
            .left
            .into_functional_expression()
            .unwrap()
            .into_binary_expression()
            .unwrap();
        {
            let expression = left;

            assert!(matches!(
                expression.operator,
                BinaryOperatorSyntaxTree::Divide(_)
            ));

            let left = expression
                .left
                .into_functional_expression()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(substr_span(&source_file, left.span()), "4");

            let right = expression
                .right
                .into_functional_expression()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(substr_span(&source_file, right.span()), "5");
        }

        let right = expression
            .right
            .into_functional_expression()
            .unwrap()
            .into_numeric_literal()
            .unwrap();
        assert_eq!(substr_span(&source_file, right.span()), "6");
    }

    Ok(())
}

#[test]
fn identifier_expression_test() -> Result<(), Box<dyn Error>> {
    let string = "Qualified::Identifier Hello(1, 2) World() Test { yeah: 1 }".to_string();
    let source_file = SourceFile::new("test".to_string(), string);
    let (token_stream, _) = TokenStream::tokenize(source_file.iter());
    let mut cursor = token_stream.cursor();
    cursor.next_token();

    let mut parser = Parser::new(cursor).unwrap();

    // Qualified::Identifier
    {
        let expression = parser
            .parse_primary_expression()
            .unwrap()
            .into_functional_expression()
            .unwrap()
            .into_identifier_expression()
            .unwrap();

        assert_eq!(
            substr_span(&source_file, expression.span()),
            "Qualified::Identifier"
        );
    }

    // Hello(1, 2)
    {
        let mut expression = parser
            .parse_primary_expression()
            .unwrap()
            .into_functional_expression()
            .unwrap()
            .into_function_call_expression()
            .unwrap();

        assert_eq!(
            substr_span(&source_file, expression.qualified_identifier.span()),
            "Hello"
        );

        let arguments = expression.arguments.as_mut().unwrap();
        let mut iter = arguments.elements();

        let expression = iter
            .next()
            .unwrap()
            .as_functional_expression()
            .unwrap()
            .as_numeric_literal()
            .unwrap();

        assert_eq!(substr_span(&source_file, expression.span()), "1");

        let expression = iter
            .next()
            .unwrap()
            .as_functional_expression()
            .unwrap()
            .as_numeric_literal()
            .unwrap();

        assert_eq!(substr_span(&source_file, expression.span()), "2");

        assert!(iter.next().is_none());
    }

    // World()
    {
        let expression = parser
            .parse_primary_expression()
            .unwrap()
            .into_functional_expression()
            .unwrap()
            .into_function_call_expression()
            .unwrap();

        assert_eq!(
            substr_span(&source_file, expression.qualified_identifier.span()),
            "World"
        );

        assert!(expression.arguments.is_none());
    }

    // Test { yeah: 1 }
    {
        let mut expression = parser
            .parse_primary_expression()
            .unwrap()
            .into_functional_expression()
            .unwrap()
            .into_struct_literal_syntax_tree()
            .unwrap();

        assert_eq!(
            substr_span(&source_file, expression.qualified_identifier.span()),
            "Test"
        );

        let mut iter = expression
            .field_initializations
            .as_mut()
            .unwrap()
            .elements();

        let field = iter.next().unwrap();

        assert_eq!(substr_span(&source_file, field.identifier.span), "yeah");

        let expression = field
            .expression
            .as_functional_expression()
            .unwrap()
            .as_numeric_literal()
            .unwrap();

        assert_eq!(substr_span(&source_file, expression.span()), "1");

        assert!(iter.next().is_none());
    }

    Ok(())
}
