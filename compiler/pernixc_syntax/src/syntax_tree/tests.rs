use std::path::PathBuf;

use pernixc_common::source_file::{SourceFile, Span, SpanEnding};
use pernixc_lexical::token_stream::TokenStream;

use crate::parser::Parser;

fn substr_span(source_file: &SourceFile, span: Span) -> &str {
    match span.end {
        SpanEnding::Location(end_location) => {
            &source_file.content()[span.start.byte..end_location.byte]
        }
        SpanEnding::EndOfFile => &source_file.content()[span.start.byte..],
    }
}

#[test]
fn qualified_identifier_test() -> Result<(), Box<dyn std::error::Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
            .join("qualifiedIdentifierTest.pnx"),
    )?;

    let (token_stream, _) = TokenStream::tokenize(source_file.iter());
    let mut cursor = token_stream.cursor();
    cursor.next_token();
    let mut parser = Parser::new(cursor)?;

    // One
    {
        let qualified_identifier = parser.parse_qualified_identifier().unwrap();
        let mut identifiers = qualified_identifier.element_iter();
        assert_eq!(
            substr_span(&source_file, identifiers.next().unwrap().span),
            "One"
        );
        assert!(identifiers.next().is_none());
    }

    // First::Second
    {
        let qualified_identifier = parser.parse_qualified_identifier().unwrap();
        let mut identifiers = qualified_identifier.element_iter();
        assert_eq!(
            substr_span(&source_file, identifiers.next().unwrap().span),
            "First"
        );
        assert_eq!(
            substr_span(&source_file, identifiers.next().unwrap().span),
            "Second"
        );
        assert!(identifiers.next().is_none());
    }

    // Third :: Fourth::Fifth
    {
        let qualified_identifier = parser.parse_qualified_identifier().unwrap();
        let mut identifiers = qualified_identifier.element_iter();
        assert_eq!(
            substr_span(&source_file, identifiers.next().unwrap().span),
            "Third"
        );
        assert_eq!(
            substr_span(&source_file, identifiers.next().unwrap().span),
            "Fourth"
        );
        assert_eq!(
            substr_span(&source_file, identifiers.next().unwrap().span),
            "Fifth"
        );
        assert!(identifiers.next().is_none());
    }

    Ok(())
}

#[test]
fn type_specifier_test() -> Result<(), Box<dyn std::error::Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
            .join("typeSpecifierTest.pnx"),
    )?;

    let (token_stream, _) = TokenStream::tokenize(source_file.iter());
    let mut cursor = token_stream.cursor();
    cursor.next_token();
    let mut parser = Parser::new(cursor)?;

    

    Ok(())
}
