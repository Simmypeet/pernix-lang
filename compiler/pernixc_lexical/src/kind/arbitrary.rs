#![allow(missing_docs)]

use std::{
    borrow::Cow,
    fmt::{Display, Write},
    ops::RangeInclusive,
    str::FromStr,
    sync::OnceLock,
};

use enum_as_inner::EnumAsInner;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy, TestCaseError},
    prop_assert, prop_assert_eq, prop_oneof,
    test_runner::TestCaseResult,
};
use strum::IntoEnumIterator as _;

use super::Punctuation;
pub use super::{Identifier, Keyword, Numeric};
use crate::token::ESCAPE_SEQUENCE_BY_REPRESENTATION;

impl Arbitrary for Keyword {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        static KEYWORDS: OnceLock<Vec<Keyword>> = OnceLock::new();
        let keywrods: &'static [_] =
            KEYWORDS.get_or_init(|| Self::iter().collect());

        proptest::sample::select(keywrods).boxed()
    }
}

impl Input<Self, ()> for &Keyword {
    fn assert(self, output: Self, (): ()) -> TestCaseResult {
        prop_assert_eq!(self, output);
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NewLine {
    LF,
    Crlf,
}

impl Arbitrary for NewLine {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![Just(Self::LF), Just(Self::Crlf)].boxed()
    }
}

impl Display for NewLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LF => f.write_str("\n"),
            Self::Crlf => f.write_str("\r\n"),
        }
    }
}

impl Input<&super::NewLine, ()> for &NewLine {
    fn assert(self, _: &super::NewLine, (): ()) -> TestCaseResult { Ok(()) }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CharacterKind {
    Normal(char),
    Escaped(char),
}

impl CharacterKind {
    #[must_use]
    pub fn get_expected_value(self) -> char {
        match self {
            Self::Normal(x) => x,
            Self::Escaped(x) => ESCAPE_SEQUENCE_BY_REPRESENTATION
                .get_by_left(&x)
                .copied()
                .unwrap(),
        }
    }
}

impl Display for CharacterKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Normal(x) => f.write_char(*x),
            Self::Escaped(x) => {
                f.write_char('\\')?;
                f.write_char(*x)
            }
        }
    }
}

impl Arbitrary for CharacterKind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        static ESCAPE_CHARACTERS: OnceLock<Vec<char>> = OnceLock::new();
        let escaped_characters: &'static [_] =
            ESCAPE_CHARACTERS.get_or_init(|| {
                ESCAPE_SEQUENCE_BY_REPRESENTATION
                    .left_values()
                    .copied()
                    .collect()
            });

        prop_oneof![
            proptest::char::any()
                .prop_filter(
                    "filter out single/double quote(s) and backslash",
                    |x| *x != '\'' && *x != '"' && *x != '\\'
                )
                .prop_map(CharacterKind::Normal),
            proptest::sample::select(escaped_characters)
                .prop_map(CharacterKind::Escaped)
        ]
        .boxed()
    }
}

impl Input<&super::Character, ()> for &CharacterKind {
    fn assert(self, output: &super::Character, (): ()) -> TestCaseResult {
        prop_assert_eq!(self.get_expected_value(), **output);
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Character(pub CharacterKind);

impl Arbitrary for Character {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        CharacterKind::arbitrary_with(()).prop_map(Character).boxed()
    }
}

impl Display for Character {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('\'')?;
        self.0.fmt(f)?;
        f.write_char('\'')
    }
}

impl Input<&super::Character, ()> for &Character {
    fn assert(self, output: &super::Character, (): ()) -> TestCaseResult {
        prop_assert_eq!(self.0.get_expected_value(), **output);
        Ok(())
    }
}

impl Arbitrary for Identifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        "[A-Za-z_][A-Za-z0-9_]*"
            .prop_filter_map(
                "filter out identifiers that can be used as a keyword",
                |x| {
                    if Keyword::from_str(x.as_ref()).is_ok() {
                        None
                    } else {
                        Some(Self(x.into()))
                    }
                },
            )
            .boxed()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl Input<Self, ()> for &Identifier {
    fn assert(self, output: Self, (): ()) -> TestCaseResult {
        prop_assert_eq!(&self.0, &output.0);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct String(pub Vec<CharacterKind>);

impl Arbitrary for String {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(CharacterKind::arbitrary_with(()), 0..=32)
            .prop_map(String)
            .boxed()
    }
}

impl Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('"')?;
        for character in &self.0 {
            character.fmt(f)?;
        }
        f.write_char('"')
    }
}

impl Input<&super::String, ()> for &String {
    fn assert(self, output: &super::String, (): ()) -> TestCaseResult {
        let mut input_iter = self.0.iter().map(|x| x.get_expected_value());
        let mut output_iter = output.0.chars();

        for (input, output) in (&mut input_iter).zip(&mut output_iter) {
            prop_assert_eq!(input, output);
        }

        prop_assert!(input_iter.next().is_none());
        prop_assert!(output_iter.next().is_none());

        Ok(())
    }
}

impl Arbitrary for Punctuation {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        static CHAR_RANGES: OnceLock<[RangeInclusive<char>; 4]> =
            OnceLock::new();
        let char_ranges: &'static [_] = CHAR_RANGES.get_or_init(|| {
            [('!'..='/'), (':'..='@'), ('['..='`'), ('{'..='~')]
        });

        proptest::char::ranges(Cow::Borrowed(char_ranges))
            .prop_filter_map("allows only ascii punctuation", |x| {
                if x.is_ascii_punctuation()
                    && x != '_'
                    && x != '\''
                    && x != '"'
                    && x != '#'
                {
                    Some(Self(x))
                } else {
                    None
                }
            })
            .boxed()
    }
}

impl Display for Punctuation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(self.0)
    }
}

impl Input<Self, ()> for &Punctuation {
    fn assert(self, output: Self, (): ()) -> TestCaseResult {
        prop_assert_eq!(self, output);
        Ok(())
    }
}

impl Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.as_str())
    }
}

impl Arbitrary for Numeric {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (proptest::num::u64::ANY.prop_map(|x| x.to_string()))
            .prop_map(|value| Self(value.into()))
            .boxed()
    }
}

impl Input<Self, ()> for &Numeric {
    fn assert(self, output: Self, (): ()) -> TestCaseResult {
        prop_assert_eq!(self, output);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Kind {
    Identifier(Identifier),
    Keyword(Keyword),
    Numeric(Numeric),
    Punctuation(Punctuation),
    Character(Character),
    String(String),
    NewLine(NewLine),
}

impl Arbitrary for Kind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            3 => Identifier::arbitrary().prop_map(Self::Identifier),
            3 => Keyword::arbitrary().prop_map(Self::Keyword),
            3 => Numeric::arbitrary().prop_map(Self::Numeric),
            3 => Punctuation::arbitrary().prop_map(Self::Punctuation),
            3 => Character::arbitrary().prop_map(Self::Character),
            3 => String::arbitrary().prop_map(Self::String),
            1 => NewLine::arbitrary().prop_map(Self::NewLine),
        ]
        .boxed()
    }
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(x) => Display::fmt(x, f),
            Self::Keyword(x) => Display::fmt(x, f),
            Self::Numeric(x) => Display::fmt(x, f),
            Self::Punctuation(x) => Display::fmt(x, f),
            Self::Character(x) => Display::fmt(x, f),
            Self::String(x) => Display::fmt(x, f),
            Self::NewLine(x) => Display::fmt(x, f),
        }
    }
}

impl Input<&super::Kind, ()> for &Kind {
    fn assert(self, output: &super::Kind, (): ()) -> TestCaseResult {
        match (self, output) {
            (Kind::Identifier(x), super::Kind::Identifier(y)) => {
                x.assert(y, ())
            }

            (Kind::Keyword(x), super::Kind::Keyword(y)) => x.assert(y, ()),
            (Kind::Numeric(x), super::Kind::Numeric(y)) => x.assert(y, ()),
            (Kind::Punctuation(x), super::Kind::Punctuation(y)) => {
                x.assert(y, ())
            }
            (Kind::Character(x), super::Kind::Character(y)) => x.assert(y, ()),
            (Kind::String(x), super::Kind::String(y)) => x.assert(y, ()),
            (Kind::NewLine(x), super::Kind::NewLine(y)) => x.assert(y, ()),

            (x, y) => Err(TestCaseError::fail(format!(
                "expected {x:?}, but got {y:?}"
            ))),
        }
    }
}
