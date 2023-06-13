//! Contains the definition of various inputs that correspond to the definitions in defined
//! [`pernixc_lexical::token`] module.

use std::{
    fmt::{Display, Write},
    str::FromStr,
};

use derive_more::{Deref, DerefMut};
use lazy_static::lazy_static;
use pernixc_source::{SourceFile, Span};
use pernixc_system::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert, prop_assert_eq, prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};
use strum::IntoEnumIterator;

use super::KeywordKind;
use crate::token::CommentKind;

/// Represents an input for the [`super::Identifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    /// The valid identifier string.
    pub string: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.write_str(&self.string) }
}

impl Arbitrary for Identifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        "[A-Za-z_@][A-Za-z0-9_]*"
            .prop_filter_map(
                "filter out identifiers that can be used as a keyword",
                |x| {
                    if KeywordKind::from_str(x.as_ref()).is_ok() {
                        None
                    } else {
                        Some(Self { string: x })
                    }
                },
            )
            .boxed()
    }
}

impl Input for Identifier {
    type Output = super::Identifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.string.as_str(), output.span.str());
        Ok(())
    }
}

/// Represents a valid keyword input for the [`super::Keyword`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Keyword {
    /// The kind of keyword.
    pub keyword: KeywordKind,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.keyword.as_str())
    }
}

impl Arbitrary for Keyword {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        lazy_static! {
            static ref KEYWORDS: Vec<KeywordKind> = KeywordKind::iter().collect();
        }

        proptest::sample::select(KEYWORDS.as_slice())
            .prop_map(|kind| Self { keyword: kind })
            .boxed()
    }
}

impl Keyword {
    /// Verifies that the given [`super::Keyword`] complies with this input.
    #[allow(clippy::missing_errors_doc)]
    pub fn assert(&self, output: &super::Keyword) -> TestCaseResult {
        prop_assert_eq!(self.keyword, output.keyword);
        Ok(())
    }
}

/// Represents an input for the [`super::NumericLiteral`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NumericLiteral {
    /// The valid numeric literal value string.
    pub value: String,

    /// The optional suffix of the numeric literal.
    pub suffix: Option<String>,
}

impl Display for NumericLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.value)?;
        if let Some(suffix) = &self.suffix {
            f.write_str(suffix)?;
        }
        Ok(())
    }
}

impl Arbitrary for NumericLiteral {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            prop_oneof![
                proptest::num::f64::ANY.prop_filter_map(
                    "filter out negative numbers and inf",
                    |x| {
                        if x.is_finite() && x.is_sign_positive() {
                            Some(x.to_string())
                        } else {
                            None
                        }
                    }
                ),
                proptest::num::u64::ANY.prop_map(|x| x.to_string())
            ],
            proptest::option::of(Identifier::arbitrary()),
        )
            .prop_map(|(value, suffix)| Self {
                value,
                suffix: suffix.map(|x| x.string),
            })
            .boxed()
    }
}

impl NumericLiteral {
    /// Verifies that the given [`super::NumericLiteral`] complies with this input.
    #[allow(clippy::missing_errors_doc)]
    pub fn assert(&self, output: &super::NumericLiteral) -> TestCaseResult {
        prop_assert_eq!(output.value_span.str(), self.value.as_str());
        prop_assert_eq!(
            output.suffix_span.as_ref().map(Span::str),
            self.suffix.as_deref()
        );
        Ok(())
    }
}

/// Represents an input for the delimited [`super::Comment`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, DerefMut)]
pub struct DelimitedComment {
    /// The content of the delimited comment (without the `/*` and `*/`).
    pub comment_body: String,
}

impl Arbitrary for DelimitedComment {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        "[^\r]*"
            .prop_filter_map("must not contain */", |x| {
                if x.contains("*/") {
                    None
                } else {
                    Some(Self { comment_body: x })
                }
            })
            .boxed()
    }
}

impl Display for DelimitedComment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("/*")?;
        f.write_str(&self.comment_body)?;
        f.write_str("*/")
    }
}

/// Represents an input for the line [`super::Comment`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineComment {
    /// The content of the line comment (without the `//` and new line terminator).
    pub comment_body: String,
}

impl Arbitrary for LineComment {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        "[^\\n\\r]*"
            .prop_map(|body| Self { comment_body: body })
            .boxed()
    }
}

impl std::fmt::Display for LineComment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("//")?;
        f.write_str(&self.comment_body)?;
        f.write_char(SourceFile::NEW_LINE)
    }
}

/// Represents an input for the [`super::Comment`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Comment {
    Line(LineComment),
    Delimited(DelimitedComment),
}

impl Arbitrary for Comment {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            LineComment::arbitrary().prop_map(Comment::Line),
            DelimitedComment::arbitrary().prop_map(Comment::Delimited)
        ]
        .boxed()
    }
}

impl Display for Comment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Line(x) => x.fmt(f),
            Self::Delimited(x) => x.fmt(f),
        }
    }
}

impl Comment {
    /// Verifies that the given [`super::Comment`] complies with this input.
    #[allow(clippy::missing_errors_doc)]
    pub fn assert(&self, output: &super::Comment) -> TestCaseResult {
        match self {
            Self::Line(i) => {
                prop_assert_eq!(output.kind, CommentKind::Line);
                prop_assert_eq!(
                    &output.span.str()[2..output.span.str().len() - 1],
                    i.comment_body.as_str()
                );
            }
            Self::Delimited(i) => {
                prop_assert_eq!(output.kind, CommentKind::Delimited);
                prop_assert_eq!(
                    &output.span.str()[2..output.span.str().len() - 2],
                    i.comment_body.as_str()
                );
            }
        };

        Ok(())
    }
}

/// Represents an input for the [`super::Punctuation`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Punctuation {
    /// The valid punctuation character.
    pub punctuation: char,
}

impl Arbitrary for Punctuation {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        proptest::char::any()
            .prop_filter_map("allows only ascii puncutation", |x| {
                if x.is_ascii_punctuation() && x != '_' && x != '@' {
                    Some(Self { punctuation: x })
                } else {
                    None
                }
            })
            .boxed()
    }
}

impl Display for Punctuation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(self.punctuation)
    }
}

impl Punctuation {
    /// Verifies that the given [`super::Punctuation`] complies with this input.
    #[allow(clippy::missing_errors_doc)]
    pub fn assert(&self, output: &super::Punctuation) -> TestCaseResult {
        prop_assert_eq!(output.punctuation, self.punctuation);
        Ok(())
    }
}

/// Represents an input for the [`super::WhiteSpaces`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum WhiteSpaces {
    Spaces(u8),
    Tabs(u8),
    NewLines(u8),
}

impl Arbitrary for WhiteSpaces {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (1u8..4)
            .prop_flat_map(|x| {
                prop_oneof![
                    Just(Self::Spaces(x)),
                    Just(Self::Tabs(x)),
                    Just(Self::NewLines(x))
                ]
            })
            .boxed()
    }
}

impl Display for WhiteSpaces {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Spaces(x) => {
                for _ in 0..*x {
                    f.write_char(' ')?;
                }
            }
            Self::Tabs(x) => {
                for _ in 0..*x {
                    f.write_char('\t')?;
                }
            }
            Self::NewLines(x) => {
                for _ in 0..*x {
                    f.write_char('\n')?;
                }
            }
        }
        Ok(())
    }
}

impl WhiteSpaces {
    /// Verifies that the given [`super::WhiteSpaces`] complies with this input.
    #[allow(clippy::missing_errors_doc)]
    pub fn assert(self, output: &super::WhiteSpaces) -> TestCaseResult {
        match self {
            Self::Spaces(i) => {
                prop_assert_eq!(output.span.str().len(), i as usize);
                prop_assert!(output.span.str().chars().all(|x| x == ' '));
            }
            Self::Tabs(i) => {
                prop_assert_eq!(output.span.str().len(), i as usize);
                prop_assert!(output.span.str().chars().all(|x| x == '\t'));
            }
            Self::NewLines(i) => {
                prop_assert_eq!(output.span.str().len(), i as usize);
                prop_assert!(output.span.str().chars().all(|x| x == SourceFile::NEW_LINE));
            }
        }

        Ok(())
    }
}

/// Represents an input for the [`super::Identifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Token {
    Identifier(Identifier),
    Comment(Comment),
    Keyword(Keyword),
    NumericLiteral(NumericLiteral),
    WhiteSpaces(WhiteSpaces),
    Punctuation(Punctuation),
}

impl Arbitrary for Token {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Identifier::arbitrary().prop_map(Self::Identifier),
            Comment::arbitrary().prop_map(Self::Comment),
            Keyword::arbitrary().prop_map(Self::Keyword),
            NumericLiteral::arbitrary().prop_map(Self::NumericLiteral),
            WhiteSpaces::arbitrary().prop_map(Self::WhiteSpaces),
        ]
        .boxed()
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(x) => Display::fmt(x, f),
            Self::Comment(x) => Display::fmt(x, f),
            Self::Keyword(x) => Display::fmt(x, f),
            Self::NumericLiteral(x) => Display::fmt(x, f),
            Self::WhiteSpaces(x) => Display::fmt(x, f),
            Self::Punctuation(x) => Display::fmt(x, f),
        }
    }
}

impl Input for Token {
    type Output = super::Token;

    /// Verifies that the given [`super::Token`] complies with this input.
    #[allow(clippy::missing_errors_doc)]
    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Identifier(i), super::Token::Identifier(o)) => {
                i.assert(o)?;
            }
            (Self::Keyword(i), super::Token::Keyword(o)) => {
                i.assert(o)?;
            }
            (Self::NumericLiteral(i), super::Token::NumericLiteral(o)) => {
                i.assert(o)?;
            }
            (Self::Comment(i), super::Token::Comment(o)) => {
                i.assert(o)?;
            }
            (Self::WhiteSpaces(i), super::Token::WhiteSpaces(o)) => {
                i.assert(o)?;
            }
            (Self::Punctuation(i), super::Token::Punctuation(o)) => {
                i.assert(o)?;
            }
            _ => {
                return Err(TestCaseError::fail(format!(
                    "expected {self:?} got {output:?}",
                )))
            }
        }

        Ok(())
    }
}
