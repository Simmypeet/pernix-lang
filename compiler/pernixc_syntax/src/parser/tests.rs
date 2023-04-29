use std::path::PathBuf;

use pernixc_lexical::{token::Token, token_stream::TokenStream};
use pernixc_source::{Location, SourceFile};
use pernixc_system::error_handler::Dummy;

use super::Parser;

#[test]
fn next_token_until_test() -> Result<(), Box<dyn std::error::Error>> {
    {
        let source_file = SourceFile::new(
            PathBuf::default(),
            "test".to_string(),
            "-(+)[+]{+}+-".to_string(),
            vec!["test".to_string()],
        )?;
        let token_stream = TokenStream::tokenize(source_file.iter(), &Dummy);
        let mut cursor = token_stream.cursor();
        cursor.next_token();

        let mut parser = Parser::new(cursor)?;
        let token = parser
            .next_token_until(
                |token| matches!(token, Token::Punctuation(p) if p.punctuation == '+'),
            )
            .unwrap()
            .as_punctuation()
            .unwrap()
            .clone();
        assert_eq!(token.punctuation, '+');
        assert_eq!(token.span.start(), 10);
        assert_eq!(token.span.start_location(), Location {
            line: 1,
            column: 11
        });
        assert_eq!(token.span.end(), 11);
        assert_eq!(token.span.end_location().unwrap(), Location {
            line: 1,
            column: 12
        });

        let token = parser.next_token().unwrap().as_punctuation().unwrap();
        assert_eq!(token.punctuation, '-');
        assert_eq!(token.span.start(), 11);
        assert_eq!(token.span.start_location(), Location {
            line: 1,
            column: 12
        });
        assert_eq!(token.span.end(), 12);
        assert_eq!(token.span.end_location(), None);
    }

    {
        let source_file = SourceFile::new(
            PathBuf::default(),
            "test".to_string(),
            "[++".to_string(),
            vec!["test".to_string()],
        )?;
        let token_stream = TokenStream::tokenize(source_file.iter(), &Dummy);
        let mut cursor = token_stream.cursor();
        cursor.next_token();

        let mut parser = Parser::new(cursor)?;
        let token = parser.next_token_until(
            |token| matches!(token, Token::Punctuation(p) if p.punctuation == '+'),
        );
        assert!(token.is_none());
        let token = parser.next_token();
        assert!(token.is_none());
    }

    Ok(())
}
