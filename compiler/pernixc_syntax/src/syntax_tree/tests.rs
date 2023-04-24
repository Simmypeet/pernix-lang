use std::path::PathBuf;

use pernixc_common::source_file::SourceFile;
use pernixc_lexical::token_stream::TokenStream;
use proptest::{prop_assert, prop_assert_eq, proptest, strategy::Strategy};

use crate::parser::Parser;

fn qualified_identifier_strategy() -> impl Strategy<Value = Vec<(bool, String, bool)>> {
    proptest::collection::vec(
        (
            proptest::bool::ANY,
            // the first character in the string will always start with an underscore to avoid a
            // collision with a keyword
            proptest::string::string_regex("_[_a-zA-Z][_a-zA-Z0-9]*").unwrap(),
            proptest::bool::ANY,
        ),
        1..=10,
    )
}

proptest! {
    #[test]
    fn qualified_identifier_test(identifiers in qualified_identifier_strategy()) {
        // Generates a new string from the identifiers
        let source_code = identifiers.iter().map(|(prepend_space, identifier, append_space)| {
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
        let source_file = SourceFile::new(
            PathBuf::default(),
            "test".to_string(),
            source_code,
            vec!["test".to_string()],
        )?;
        let (token_stream, _) = TokenStream::tokenize(source_file.iter());
        let mut cursor = token_stream.cursor();
        cursor.next_token();
        let mut parser = Parser::new(cursor).unwrap();

        // Parses the qualified identifiers
        let qualified_identifier = parser.parse_qualified_identifier().unwrap();

        for (original_identifier, parsed_identifier) in identifiers.iter().zip(qualified_identifier.identifiers().elements()) {
            prop_assert_eq!(&original_identifier.1, parsed_identifier.span.str());
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
        let source_code = format!("{primitive_type} {qualified_type}");
        let source_file = SourceFile::new(
            PathBuf::default(),
            "test".to_string(),
            source_code,
            vec!["test".to_string()],
        )?;
        let (token_stream, _) = TokenStream::tokenize(source_file.iter());
        let mut cursor = token_stream.cursor();
        cursor.next_token();
        let mut parser = Parser::new(cursor).unwrap();

        prop_assert!(parser.parse_type_specifier().unwrap().into_primitive_type_specifier().is_ok());
        prop_assert!(parser.parse_type_specifier().unwrap().into_qualified_identifier().is_ok());
    }
}
