use std::error::Error;

use pernixc_common::source_file::{SourceFile, Span, SpanEnding};
use pernixc_lexical::token_stream::TokenStream;
use proptest::{
    prop_assert_eq, prop_oneof, proptest,
    strategy::{Just, Strategy},
};

use crate::{parser::Parser, syntax_tree::expression::PerfixOperatorSyntaxTree};

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
fn binary_operator_test() -> Result<(), Box<dyn Error>> { Ok(()) }
