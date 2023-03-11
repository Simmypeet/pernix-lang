use std::path::PathBuf;

use pernixc_common::source_file::{Location, SourceFile, Span, SpanEnding};

use crate::token::{Keyword, Token};

#[test]
fn token_test() -> Result<(), Box<dyn std::error::Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("src")
            .join("token")
            .join("tokenTest.pnx"),
    )?;

    let mut iter = source_file.iter();

    // helloWorld @Test _1 public void
    {
        // helloWorld
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_identifier()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(1, 1, 0),
                SpanEnding::Location(Location::new(1, 11, 10))
            )
        );

        // WHITESPACE
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(1, 11, 10),
                SpanEnding::Location(Location::new(1, 12, 11))
            )
        );

        // @Test
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_identifier()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(1, 12, 11),
                SpanEnding::Location(Location::new(1, 17, 16))
            )
        );

        // WHITESPACE
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(1, 17, 16),
                SpanEnding::Location(Location::new(1, 18, 17))
            )
        );

        // _1
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_identifier()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(1, 18, 17),
                SpanEnding::Location(Location::new(1, 20, 19))
            )
        );

        // WHITESPACE
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(1, 20, 19),
                SpanEnding::Location(Location::new(1, 21, 20))
            )
        );

        // public
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_keyword()
            .cloned()
            .unwrap();
        assert_eq!(token.keyword, Keyword::Public);
        assert_eq!(
            token.span,
            Span::new(
                Location::new(1, 21, 20 ),
                SpanEnding::Location(Location::new(1, 27, 26))
            )
        );

        // WHITESPACE
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .cloned()
            .unwrap();

        assert_eq!(
            token.span,
            Span::new(
                Location::new(1, 27, 26),
                SpanEnding::Location(Location::new(1, 28, 27))
            )
        );

        // void
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_keyword()
            .cloned()
            .unwrap();
        assert_eq!(token.keyword, Keyword::Void);
        assert_eq!(
            token.span,
            Span::new(
                Location::new(1, 28, 27),
                SpanEnding::Location(Location::new(1, 32, 31))
            )
        );
    }

    Ok(())
}
