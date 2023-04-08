use std::path::PathBuf;

use pernixc_common::source_file::{Location, SourceFile, Span, SpanEnding};

use crate::token::{CommentKind, KeywordKind, Token};

#[test]
#[allow(clippy::too_many_lines)]
#[allow(clippy::cognitive_complexity)]
fn token_test() -> Result<(), Box<dyn std::error::Error>> {
    let source_file = SourceFile::load(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
            .join("tokenTest.pnx"),
        vec!["test".to_string()],
    )?;

    let mut iter = source_file.iter();

    // helloWorld @Test _1 public void
    {
        // helloWorld
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_identifier()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 1,
                column: 1,
                byte: 0
            },
            end: SpanEnding::Location(Location {
                line: 1,
                column: 11,
                byte: 10
            })
        });

        // WHITESPACE
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 1,
                column: 11,
                byte: 10
            },
            end: SpanEnding::Location(Location {
                line: 1,
                column: 12,
                byte: 11
            })
        });

        // @Test
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_identifier()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 1,
                column: 12,
                byte: 11
            },
            end: SpanEnding::Location(Location {
                line: 1,
                column: 17,
                byte: 16
            })
        });

        // WHITESPACE
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 1,
                column: 17,
                byte: 16
            },
            end: SpanEnding::Location(Location {
                line: 1,
                column: 18,
                byte: 17
            })
        });

        // _1
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_identifier()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 1,
                column: 18,
                byte: 17
            },
            end: SpanEnding::Location(Location {
                line: 1,
                column: 20,
                byte: 19
            })
        });

        // WHITESPACE
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 1,
                column: 20,
                byte: 19
            },
            end: SpanEnding::Location(Location {
                line: 1,
                column: 21,
                byte: 20
            })
        });

        // public
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_keyword()
            .copied()
            .unwrap();
        assert_eq!(token.keyword, KeywordKind::Public);
        assert_eq!(token.span, Span {
            start: Location {
                line: 1,
                column: 21,
                byte: 20
            },
            end: SpanEnding::Location(Location {
                line: 1,
                column: 27,
                byte: 26
            })
        });

        // WHITESPACE
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .copied()
            .unwrap();

        assert_eq!(token.span, Span {
            start: Location {
                line: 1,
                column: 27,
                byte: 26
            },
            end: SpanEnding::Location(Location {
                line: 1,
                column: 28,
                byte: 27
            })
        });

        // void
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_keyword()
            .copied()
            .unwrap();
        assert_eq!(token.keyword, KeywordKind::Void);
        assert_eq!(token.span, Span {
            start: Location {
                line: 1,
                column: 28,
                byte: 27
            },
            end: SpanEnding::Location(Location {
                line: 1,
                column: 32,
                byte: 31
            })
        });
    }

    // line ends
    let token = Token::tokenize(&mut iter)
        .unwrap()
        .as_white_space()
        .copied()
        .unwrap();
    assert_eq!(token.span, Span {
        start: Location {
            line: 1,
            column: 32,
            byte: 31
        },
        end: SpanEnding::Location(Location {
            line: 2,
            column: 1,
            byte: 32
        })
    });

    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_comment()
            .copied()
            .unwrap();
        assert_eq!(token.kind, CommentKind::SingleLine);
        assert_eq!(token.span, Span {
            start: Location {
                line: 2,
                column: 1,
                byte: 32
            },
            end: SpanEnding::Location(Location {
                line: 3,
                column: 1,
                byte: 40
            })
        });

        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_comment()
            .copied()
            .unwrap();
        assert_eq!(token.kind, CommentKind::Delimited);
        assert_eq!(token.span, Span {
            start: Location {
                line: 3,
                column: 1,
                byte: 40
            },
            end: SpanEnding::Location(Location {
                line: 4,
                column: 7,
                byte: 54
            })
        });
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 4,
                column: 7,
                byte: 54
            },
            end: SpanEnding::Location(Location {
                line: 5,
                column: 1,
                byte: 55
            })
        });
    }

    // 1231
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_numeric_literal()
            .copied()
            .unwrap();

        assert_eq!(token.span, Span {
            start: Location {
                line: 5,
                column: 1,
                byte: 55
            },
            end: SpanEnding::Location(Location {
                line: 5,
                column: 5,
                byte: 59
            })
        });

        assert_eq!(token.value_span, Span {
            start: Location {
                line: 5,
                column: 1,
                byte: 55
            },
            end: SpanEnding::Location(Location {
                line: 5,
                column: 5,
                byte: 59
            })
        });

        assert!(token.suffix_span.is_none());
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 5,
                column: 5,
                byte: 59
            },
            end: SpanEnding::Location(Location {
                line: 6,
                column: 1,
                byte: 60
            })
        });
    }

    // 12.42f32
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_numeric_literal()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 6,
                column: 1,
                byte: 60
            },
            end: SpanEnding::Location(Location {
                line: 6,
                column: 9,
                byte: 68
            })
        });
        assert_eq!(token.value_span, Span {
            start: Location {
                line: 6,
                column: 1,
                byte: 60
            },
            end: SpanEnding::Location(Location {
                line: 6,
                column: 6,
                byte: 65
            })
        });
        assert_eq!(token.suffix_span.unwrap(), Span {
            start: Location {
                line: 6,
                column: 6,
                byte: 65
            },
            end: SpanEnding::Location(Location {
                line: 6,
                column: 9,
                byte: 68
            })
        });
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 6,
                column: 9,
                byte: 68
            },
            end: SpanEnding::Location(Location {
                line: 7,
                column: 1,
                byte: 69
            })
        });
    }

    // 123
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_numeric_literal()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 7,
                column: 1,
                byte: 69
            },
            end: SpanEnding::Location(Location {
                line: 7,
                column: 4,
                byte: 72
            })
        });
        assert_eq!(token.value_span, Span {
            start: Location {
                line: 7,
                column: 1,
                byte: 69
            },
            end: SpanEnding::Location(Location {
                line: 7,
                column: 4,
                byte: 72
            })
        });
        assert!(token.suffix_span.is_none());
    }

    // .
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_punctuation()
            .copied()
            .unwrap();
        assert_eq!(token.punctuation, '.');
        assert_eq!(token.span, Span {
            start: Location {
                line: 7,
                column: 4,
                byte: 72
            },
            end: SpanEnding::Location(Location {
                line: 7,
                column: 5,
                byte: 73
            })
        });
    }

    // f32
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_identifier()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 7,
                column: 5,
                byte: 73
            },
            end: SpanEnding::Location(Location {
                line: 7,
                column: 8,
                byte: 76
            })
        });
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 7,
                column: 8,
                byte: 76
            },
            end: SpanEnding::Location(Location {
                line: 8,
                column: 1,
                byte: 77
            })
        });
    }

    // 'a'
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_character_literal()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 8,
                column: 1,
                byte: 77
            },
            end: SpanEnding::Location(Location {
                line: 8,
                column: 4,
                byte: 80
            })
        });
        assert_eq!(token.character, 'a');
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 8,
                column: 4,
                byte: 80
            },
            end: SpanEnding::Location(Location {
                line: 9,
                column: 1,
                byte: 81
            })
        });
    }

    // 'ab'
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_punctuation()
            .copied()
            .unwrap();

        assert_eq!(token.punctuation, '\'');
        assert_eq!(token.span, Span {
            start: Location {
                line: 9,
                column: 1,
                byte: 81
            },
            end: SpanEnding::Location(Location {
                line: 9,
                column: 2,
                byte: 82
            })
        });

        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_identifier()
            .copied()
            .unwrap();

        assert_eq!(token.span, Span {
            start: Location {
                line: 9,
                column: 2,
                byte: 82
            },
            end: SpanEnding::Location(Location {
                line: 9,
                column: 4,
                byte: 84
            })
        });

        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_punctuation()
            .copied()
            .unwrap();

        assert_eq!(token.punctuation, '\'');
        assert_eq!(token.span, Span {
            start: Location {
                line: 9,
                column: 4,
                byte: 84
            },
            end: SpanEnding::Location(Location {
                line: 9,
                column: 5,
                byte: 85
            })
        });
    }

    // line ends and a space
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 9,
                column: 5,
                byte: 85
            },
            end: SpanEnding::Location(Location {
                line: 10,
                column: 2,
                byte: 87
            })
        });
    }

    // '\0'
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_character_literal()
            .copied()
            .unwrap();

        assert_eq!(token.span, Span {
            start: Location {
                line: 10,
                column: 2,
                byte: 87
            },
            end: SpanEnding::Location(Location {
                line: 10,
                column: 6,
                byte: 91
            })
        });
        assert_eq!(token.character, '\0');
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 10,
                column: 6,
                byte: 91
            },
            end: SpanEnding::Location(Location {
                line: 11,
                column: 1,
                byte: 92
            })
        });
    }

    // '\1' invaid escape character
    {
        let errs = Token::tokenize(&mut iter)
            .unwrap_err()
            .into_lexical_error()
            .unwrap()
            .into_invalid_escape_character_sequences()
            .unwrap();

        assert_eq!(errs.spans.len(), 1);
        assert_eq!(errs.spans[0], Span {
            start: Location {
                line: 11,
                column: 3,
                byte: 94
            },
            end: SpanEnding::Location(Location {
                line: 11,
                column: 4,
                byte: 95
            })
        });
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 11,
                column: 5,
                byte: 96
            },
            end: SpanEnding::Location(Location {
                line: 12,
                column: 1,
                byte: 97
            })
        });
    }

    // "Hello, world!"
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_string_literal()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 12,
                column: 1,
                byte: 97
            },
            end: SpanEnding::Location(Location {
                line: 12,
                column: 16,
                byte: 112
            })
        });
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 12,
                column: 16,
                byte: 112
            },
            end: SpanEnding::Location(Location {
                line: 13,
                column: 1,
                byte: 113
            })
        });
    }

    // "\"With escape sequences\""
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_string_literal()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 13,
                column: 1,
                byte: 113
            },
            end: SpanEnding::Location(Location {
                line: 13,
                column: 28,
                byte: 140
            })
        });
    }

    // line ends
    {
        let token = Token::tokenize(&mut iter)
            .unwrap()
            .as_white_space()
            .copied()
            .unwrap();
        assert_eq!(token.span, Span {
            start: Location {
                line: 13,
                column: 28,
                byte: 140
            },
            end: SpanEnding::Location(Location {
                line: 14,
                column: 1,
                byte: 141
            })
        });
    }

    // "With \1 invalid \2 escape \3 sequences"
    {
        let errs = Token::tokenize(&mut iter)
            .unwrap_err()
            .into_lexical_error()
            .unwrap()
            .into_invalid_escape_character_sequences()
            .unwrap();

        assert_eq!(errs.spans.len(), 3);

        assert_eq!(errs.spans[0], Span {
            start: Location {
                line: 14,
                column: 8,
                byte: 148
            },
            end: SpanEnding::Location(Location {
                line: 14,
                column: 9,
                byte: 149
            })
        });

        assert_eq!(errs.spans[1], Span {
            start: Location {
                line: 14,
                column: 19,
                byte: 159
            },
            end: SpanEnding::Location(Location {
                line: 14,
                column: 20,
                byte: 160
            })
        });

        assert_eq!(errs.spans[2], Span {
            start: Location {
                line: 14,
                column: 29,
                byte: 169
            },
            end: SpanEnding::Location(Location {
                line: 14,
                column: 30,
                byte: 170
            })
        });
    }

    // end of file err
    assert!(Token::tokenize(&mut iter)
        .unwrap_err()
        .is_end_of_source_code_iterator_argument());

    Ok(())
}
