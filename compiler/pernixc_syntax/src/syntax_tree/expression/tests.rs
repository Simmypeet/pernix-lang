use std::{error::Error, path::PathBuf};

use pernixc_common::source_file::SourceFile;
use pernixc_lexical::token_stream::TokenStream;
use pernixc_system::error_handler::DummyErrorHandler;
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

proptest! {
    #[test]
    fn prefix_operator_test(operators in prefix_operator_strategy()) {
        let mut string = operators.iter().map(|operator| match operator {
            PrefixOperator::LogicalNot => "!",
            PrefixOperator::Negate => "-",
        }).collect::<String>();
        string.push('1');
        let source_file = SourceFile::new(
            PathBuf::default(),
            "test".to_string(),
            string,
            vec!["test".to_string()],
        )?;

        let token_stream = TokenStream::tokenize(source_file.iter(), &DummyErrorHandler);
        let mut cursor = token_stream.cursor();
        cursor.next_token();
        let mut parser = Parser::new(cursor).unwrap();

        let mut expression = parser.parse_primary_expression(&DummyErrorHandler).unwrap();

        for original_operator in operators {
            let prefix_expr = expression.into_functional()
                .unwrap()
                .into_prefix()
                .unwrap();
            match prefix_expr.prefix_operator {
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
        let source_file = SourceFile::new(
            PathBuf::default(),
            "test".to_string(),
            string,
            vec!["test".to_string()],
        )?;
        let token_stream = TokenStream::tokenize(source_file.iter(), &DummyErrorHandler);
        let mut cursor = token_stream.cursor();
        cursor.next_token();
        let mut parser = Parser::new(cursor).unwrap();

        let mut expression = parser.parse_primary_expression(&DummyErrorHandler).unwrap();

        for original_identifier in identifiers.iter().rev() {
            let member_access_expr = expression.into_functional().unwrap().into_member_access().unwrap();
            prop_assert_eq!(original_identifier, member_access_expr.identifier.span.str());
            expression = *member_access_expr.operand;
        }

        expression.into_functional().unwrap().into_numeric_literal().unwrap();
    }
}

#[test]
#[allow(clippy::too_many_lines)]
fn binary_operator_test() -> Result<(), Box<dyn Error>> {
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
    let source_file = SourceFile::new(
        PathBuf::default(),
        "test".to_string(),
        source_code.to_string(),
        vec!["test".to_string()],
    )?;
    let token_stream = TokenStream::tokenize(source_file.iter(), &DummyErrorHandler);
    let mut cursor = token_stream.cursor();
    cursor.next_token();

    let mut parser = Parser::new(cursor).unwrap();

    let expression = parser
        .parse_expression(&DummyErrorHandler)
        .unwrap()
        .into_functional()
        .unwrap()
        .into_binary()
        .unwrap();

    assert!(matches!(
        expression.binary_operator,
        BinaryOperator::Subtract(..)
    ));

    let left = expression
        .left_operand
        .into_functional()
        .unwrap()
        .into_binary()
        .unwrap();
    {
        let expression = left;

        assert!(matches!(
            expression.binary_operator,
            BinaryOperator::Add(..)
        ));

        let left = expression
            .left_operand
            .into_functional()
            .unwrap()
            .into_prefix()
            .unwrap();
        {
            let expression = left;
            assert!(matches!(
                expression.prefix_operator,
                super::PrefixOperator::LogicalNot(..)
            ));
            let operand = expression
                .operand
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(operand.span().str(), "1");
        }

        let right = expression
            .right_operand
            .into_functional()
            .unwrap()
            .into_binary()
            .unwrap();
        {
            let expression = right;

            assert!(matches!(
                expression.binary_operator,
                BinaryOperator::Multiply(..)
            ));

            let left = expression
                .left_operand
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(left.span().str(), "2");

            let right = expression
                .right_operand
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(right.span().str(), "3");
        }
    }
    let right = expression
        .right_operand
        .into_functional()
        .unwrap()
        .into_binary()
        .unwrap();
    {
        let expression = right;

        assert!(matches!(
            expression.binary_operator,
            BinaryOperator::Modulo(..)
        ));

        let left = expression
            .left_operand
            .into_functional()
            .unwrap()
            .into_binary()
            .unwrap();
        {
            let expression = left;

            assert!(matches!(
                expression.binary_operator,
                BinaryOperator::Divide(..)
            ));

            let left = expression
                .left_operand
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(left.span().str(), "4");

            let right = expression
                .right_operand
                .into_functional()
                .unwrap()
                .into_numeric_literal()
                .unwrap();
            assert_eq!(right.span().str(), "5");
        }

        let right = expression
            .right_operand
            .into_functional()
            .unwrap()
            .into_numeric_literal()
            .unwrap();
        assert_eq!(right.span().str(), "6");
    }

    Ok(())
}

#[test]
fn identifier_expression_test() -> Result<(), Box<dyn Error>> {
    let source_code = "Qualified::Identifier Hello(1, 2) World() Test { yeah: 1 }";
    let source_file = SourceFile::new(
        PathBuf::default(),
        "test".to_string(),
        source_code.to_string(),
        vec!["test".to_string()],
    )?;
    let token_stream = TokenStream::tokenize(source_file.iter(), &DummyErrorHandler);
    let mut cursor = token_stream.cursor();
    cursor.next_token();

    let mut parser = Parser::new(cursor).unwrap();

    // Qualified::Identifier
    {
        let expression = parser
            .parse_primary_expression(&DummyErrorHandler)
            .unwrap()
            .into_functional()
            .unwrap()
            .into_named()
            .unwrap();

        assert_eq!(expression.span().str(), "Qualified::Identifier");
    }

    // Hello(1, 2)
    {
        let mut expression = parser
            .parse_primary_expression(&DummyErrorHandler)
            .unwrap()
            .into_functional()
            .unwrap()
            .into_function_call()
            .unwrap();

        assert_eq!(expression.qualified_identifier.span().str(), "Hello");

        let arguments = expression.arguments.as_mut().unwrap();
        let mut iter = arguments.elements();

        let expression = iter
            .next()
            .unwrap()
            .as_functional()
            .unwrap()
            .as_numeric_literal()
            .unwrap();

        assert_eq!(expression.span().str(), "1");

        let expression = iter
            .next()
            .unwrap()
            .as_functional()
            .unwrap()
            .as_numeric_literal()
            .unwrap();

        assert_eq!(expression.span().str(), "2");

        assert!(iter.next().is_none());
    }

    // World()
    {
        let expression = parser
            .parse_primary_expression(&DummyErrorHandler)
            .unwrap()
            .into_functional()
            .unwrap()
            .into_function_call()
            .unwrap();

        assert_eq!(expression.qualified_identifier.span().str(), "World");

        assert!(expression.arguments.is_none());
    }

    // Test { yeah: 1 }
    {
        let mut expression = parser
            .parse_primary_expression(&DummyErrorHandler)
            .unwrap()
            .into_functional()
            .unwrap()
            .into_struct_literal()
            .unwrap();

        assert_eq!(expression.qualified_identifier.span().str(), "Test");

        let mut iter = expression
            .field_initializations
            .as_mut()
            .unwrap()
            .elements();

        let field = iter.next().unwrap();

        assert_eq!(field.identifier.span().str(), "yeah");

        let expression = field
            .expression
            .as_functional()
            .unwrap()
            .as_numeric_literal()
            .unwrap();

        assert_eq!(expression.span().str(), "1");

        assert!(iter.next().is_none());
    }

    Ok(())
}
