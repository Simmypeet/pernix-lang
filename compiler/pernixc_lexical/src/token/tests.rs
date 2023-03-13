use std::path::PathBuf;

use pernixc_common::source_file::{Location, SourceFile, Span, SpanEnding};

use crate::token::{CommentKind, Keyword, Token};

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
                Location::new(1, 21, 20),
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

    // line ends
    let token = Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .cloned()
        .unwrap();
    assert_eq!(
        token.span,
        Span::new(
            Location::new(1, 32, 31),
            SpanEnding::Location(Location::new(2, 1, 32))
        )
    );

    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_comment()
            .cloned()
            .unwrap();
        assert_eq!(token.kind, CommentKind::SingleLine);
        assert_eq!(
            token.span,
            Span::new(
                Location::new(2, 1, 32),
                SpanEnding::Location(Location::new(3, 1, 40))
            )
        );

        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_comment()
            .cloned()
            .unwrap();
        assert_eq!(token.kind, CommentKind::Delimited);
        assert_eq!(
            token.span,
            Span::new(
                Location::new(3, 1, 40),
                SpanEnding::Location(Location::new(4, 7, 54))
            )
        );
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(4, 7, 54),
                SpanEnding::Location(Location::new(5, 1, 55))
            )
        );
    }

    // 1231
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_numeric_literal()
            .cloned()
            .unwrap();

        assert_eq!(
            token.span,
            Span::new(
                Location::new(5, 1, 55),
                SpanEnding::Location(Location::new(5, 5, 59))
            )
        );

        assert_eq!(
            token.value_span,
            Span::new(
                Location::new(5, 1, 55),
                SpanEnding::Location(Location::new(5, 5, 59))
            )
        );

        assert!(token.suffix_span.is_none());
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(5, 5, 59),
                SpanEnding::Location(Location::new(6, 1, 60))
            )
        );
    }

    // 12.42f32
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_numeric_literal()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(6, 1, 60),
                SpanEnding::Location(Location::new(6, 9, 68))
            )
        );
        assert_eq!(
            token.value_span,
            Span::new(
                Location::new(6, 1, 60),
                SpanEnding::Location(Location::new(6, 6, 65))
            )
        );
        assert_eq!(
            token.suffix_span.unwrap(),
            Span::new(
                Location::new(6, 6, 65),
                SpanEnding::Location(Location::new(6, 9, 68))
            )
        );
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(6, 9, 68),
                SpanEnding::Location(Location::new(7, 1, 69))
            )
        );
    }

    // 123
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_numeric_literal()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(7, 1, 69),
                SpanEnding::Location(Location::new(7, 4, 72))
            )
        );
        assert_eq!(
            token.value_span,
            Span::new(
                Location::new(7, 1, 69),
                SpanEnding::Location(Location::new(7, 4, 72))
            )
        );
        assert!(token.suffix_span.is_none());
    }

    // .
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_punctuation()
            .cloned()
            .unwrap();
        assert_eq!(token.punctuation, '.');
        assert_eq!(
            token.span,
            Span::new(
                Location::new(7, 4, 72),
                SpanEnding::Location(Location::new(7, 5, 73))
            )
        );
    }

    // f32
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_identifier()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(7, 5, 73),
                SpanEnding::Location(Location::new(7, 8, 76))
            )
        );
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(7, 8, 76),
                SpanEnding::Location(Location::new(8, 1, 77))
            )
        );
    }

    // 'a'
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_character_literal()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(8, 1, 77),
                SpanEnding::Location(Location::new(8, 4, 80))
            )
        );
        assert_eq!(token.character, 'a');
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(8, 4, 80),
                SpanEnding::Location(Location::new(9, 1, 81))
            )
        );
    }

    // 'ab'
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_punctuation()
            .cloned()
            .unwrap();

        assert_eq!(token.punctuation, '\'');
        assert_eq!(
            token.span,
            Span::new(
                Location::new(9, 1, 81),
                SpanEnding::Location(Location::new(9, 2, 82))
            )
        );

        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_identifier()
            .cloned()
            .unwrap();

        assert_eq!(
            token.span,
            Span::new(
                Location::new(9, 2, 82),
                SpanEnding::Location(Location::new(9, 4, 84))
            )
        );

        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_punctuation()
            .cloned()
            .unwrap();

        assert_eq!(token.punctuation, '\'');
        assert_eq!(
            token.span,
            Span::new(
                Location::new(9, 4, 84),
                SpanEnding::Location(Location::new(9, 5, 85))
            )
        );
    }

    // line ends and a space
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(9, 5, 85),
                SpanEnding::Location(Location::new(10, 2, 87))
            )
        );
    }

    // '\0'
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_character_literal()
            .cloned()
            .unwrap();

        assert_eq!(
            token.span,
            Span::new(
                Location::new(10, 2, 87),
                SpanEnding::Location(Location::new(10, 6, 91))
            )
        );
        assert_eq!(token.character, '\0');
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(10, 6, 91),
                SpanEnding::Location(Location::new(11, 1, 92))
            )
        );
    }

    // '\1' invaid escape character
    {
        let errs = Token::tokenize(&mut iter)
            .unwrap_err()
            .into_lexical()
            .unwrap()
            .into_invalid_escape_character_sequences()
            .unwrap();

        assert_eq!(errs.len(), 1);
        assert_eq!(
            errs[0],
            Span::new(
                Location::new(11, 3, 94),
                SpanEnding::Location(Location::new(11, 4, 95))
            )
        );
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(11, 5, 96),
                SpanEnding::Location(Location::new(12, 1, 97))
            )
        );
    }

    // "Hello, world!"
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_string_literal()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(12, 1, 97),
                SpanEnding::Location(Location::new(12, 16, 112))
            )
        );
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(12, 16, 112),
                SpanEnding::Location(Location::new(13, 1, 113))
            )
        );
    }

    // "\"With escape sequences\""
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_string_literal()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(13, 1, 113),
                SpanEnding::Location(Location::new(13, 28, 140))
            )
        );
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .cloned()
            .unwrap();
        assert_eq!(
            token.span,
            Span::new(
                Location::new(13, 28, 140),
                SpanEnding::Location(Location::new(14, 1, 141))
            )
        );
    }

    // "With \1 invalid \2 escape \3 sequences"
    {
        let errs = Token::tokenize(&mut iter)
            .unwrap_err()
            .into_lexical()
            .unwrap()
            .into_invalid_escape_character_sequences()
            .unwrap();

        assert_eq!(errs.len(), 3);

        assert_eq!(
            errs[0],
            Span::new(
                Location::new(14, 8, 148),
                SpanEnding::Location(Location::new(14, 9, 149))
            )
        );

        assert_eq!(
            errs[1],
            Span::new(
                Location::new(14, 19, 159),
                SpanEnding::Location(Location::new(14, 20, 160))
            )
        );

        assert_eq!(
            errs[2],
            Span::new(
                Location::new(14, 29, 169),
                SpanEnding::Location(Location::new(14, 30, 170))
            )
        );
    }

    // end of file err
    assert!(Token::tokenize(&mut iter)
        .unwrap_err()
        .is_end_of_source_code_iterator_argument());

    Ok(())
}
