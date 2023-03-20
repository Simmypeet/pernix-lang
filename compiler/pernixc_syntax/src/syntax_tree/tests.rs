use pernixc_common::source_file::{SourceFile, Span, SpanEnding};
use pernixc_lexical::token_stream::TokenStream;
use proptest::{prop_assert, prop_assert_eq, proptest, strategy::Strategy};

use crate::{parser::Parser, syntax_tree::SyntaxTree};

fn substr_span(source_file: &SourceFile, span: Span) -> &str {
    match span.end {
        SpanEnding::Location(end_location) => {
            &source_file.content()[span.start.byte..end_location.byte]
        }
        SpanEnding::EndOfFile => &source_file.content()[span.start.byte..],
    }
}

fn qualified_identifier_strategy() -> impl Strategy<Value = Vec<(bool, String, bool)>> {
    proptest::collection::vec(
        (
            proptest::bool::ANY,
            proptest::string::string_regex("[a-zA-Z_][a-zA-Z0-9_]*").unwrap(),
            proptest::bool::ANY,
        ),
        1..=10,
    )
}

proptest! {
    #[test]
    fn qualified_identifier_test(identifiers in qualified_identifier_strategy()) {
        // Generates a new string from the identifiers
        let string = identifiers.iter().map(|(prepend_space, identifier, append_space)| {
            let mut string = String::new();
            if *prepend_space {
                string.push(' ');
            }
            string.push_str(identifier);
            if *append_space {
                string.push(' ');
            }
            string
        }).collect::<Vec<_>>().join("::");

        let source_file = SourceFile::new("test".to_string(), string);
        let (token_stream, _) = TokenStream::tokenize(source_file.iter());
        let mut cursor = token_stream.cursor();
        cursor.next_token();
        let mut parser = Parser::new(cursor).unwrap();

        // Parses the qualified identifiers
        let qualified_identifier = parser.parse_qualified_identifier().unwrap();

        for (original_identifier, parsed_identifier) in identifiers.iter().zip(qualified_identifier.element_iter()) {
            prop_assert_eq!(&original_identifier.1, substr_span(&source_file, parsed_identifier.span()))
        }
    }
}

fn primitive_type_specifier_strategy() -> impl Strategy<Value = &'static str> {
    // get one of the primitive type strings
    let values = vec![
        "bool", "void", "float32", "float64", "int8", "int16", "int32", "int64", "uint8", "uint16",
        "uint32", "uint64",
    ];
    proptest::sample::select(values)
}

fn qualified_type_specifier_strategy() -> impl Strategy<Value = String> {
    qualified_identifier_strategy().prop_map(|identifiers| {
        identifiers
            .iter()
            .map(|(prepend_space, identifier, append_space)| {
                let mut string = String::new();
                if *prepend_space {
                    string.push(' ');
                }
                string.push_str(identifier);
                if *append_space {
                    string.push(' ');
                }
                string
            })
            .collect::<Vec<_>>()
            .join("::")
    })
}

proptest! {
    #[test]
    fn type_specifier_test(
        primitive_type in primitive_type_specifier_strategy(),
        qualified_type in qualified_type_specifier_strategy(),
    ) {
        let source_file = SourceFile::new("test".to_string(), format!("{} {}", primitive_type, qualified_type));
        let (token_stream, _) = TokenStream::tokenize(source_file.iter());
        let mut cursor = token_stream.cursor();
        cursor.next_token();
        let mut parser = Parser::new(cursor).unwrap();

        prop_assert!(parser.parse_type_specifier().unwrap().into_primitive().is_ok());
        prop_assert!(parser.parse_type_specifier().unwrap().into_qualified().is_ok());
    }
}
