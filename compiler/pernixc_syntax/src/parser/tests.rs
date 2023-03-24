use pernixc_common::source_file::{Location, SourceFileIterator, Span, SpanEnding};
use pernixc_lexical::{
    token::{PunctuationToken, Token},
    token_stream::TokenStream,
};

use super::Parser;

#[test]
fn next_token_until_test() -> Result<(), Box<dyn std::error::Error>> {
    {
        let (token_stream, _) = TokenStream::tokenize(SourceFileIterator::new("-(+)[+]{+}+-"));
        let mut cursor = token_stream.cursor();
        cursor.next_token();

        let mut parser = Parser::new(cursor)?;
        let token = parser
            .next_token_until(
                |token| matches!(token, Token::Punctuation(p) if p.punctuation == '+'),
            )
            .unwrap()
            .as_punctuation()
            .unwrap();
        assert_eq!(*token, PunctuationToken {
            punctuation: '+',
            span:        Span::new(
                Location::new(1, 11, 10),
                SpanEnding::Location(Location::new(1, 12, 11))
            ),
        });

        let token = parser.next_token().unwrap().as_punctuation().unwrap();
        assert_eq!(*token, PunctuationToken {
            punctuation: '-',
            span:        Span::new(Location::new(1, 12, 11), SpanEnding::EndOfFile),
        });
    }

    {
        let (token_stream, _) = TokenStream::tokenize(SourceFileIterator::new("[++"));
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
