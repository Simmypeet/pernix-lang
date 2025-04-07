#![allow(missing_docs)]

use std::{
    borrow::Cow,
    fmt::{Debug, Display, Write},
    str::FromStr,
};

use codespan_reporting::files::Files;
use derive_more::{Deref, DerefMut};
use enum_as_inner::EnumAsInner;
use lazy_static::lazy_static;
use pernixc_source_file::Span;
use pernixc_test_input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};
use strum::IntoEnumIterator;

use super::{KeywordKind, ESCAPE_SEQUENCE_BY_REPRESENTATION};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    pub string: std::string::String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.string)
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

impl<F: for<'x> Files<'x, FileId = ID>, ID: Clone + Debug>
    Input<&super::Identifier<Span<ID>>, &F> for &Identifier
{
    fn assert(
        self,
        output: &super::Identifier<Span<ID>>,
        file: &F,
    ) -> TestCaseResult {
        let source = file.source(output.span.source_id.clone())?;
        prop_assert_eq!(
            self.string.as_str(),
            &source.as_ref()[output.span.range()]
        );

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Keyword {
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

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        lazy_static! {
            static ref KEYWORDS: Vec<KeywordKind> =
                KeywordKind::iter().collect();
        }

        proptest::sample::select(KEYWORDS.as_slice())
            .prop_map(|kind| Self { keyword: kind })
            .boxed()
    }
}

impl<ID: Debug> Input<&super::Keyword<Span<ID>>, ()> for &Keyword {
    fn assert(
        self,
        output: &super::Keyword<Span<ID>>,
        (): (),
    ) -> TestCaseResult {
        prop_assert_eq!(self.keyword, output.kind);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric {
    pub value: std::string::String,
}

impl Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.value)?;
        Ok(())
    }
}

impl Arbitrary for Numeric {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (proptest::num::u64::ANY.prop_map(|x| x.to_string()))
            .prop_map(|value| Self { value })
            .boxed()
    }
}

impl<F: for<'x> Files<'x, FileId = ID>, ID: Clone + Debug>
    Input<&super::Numeric<Span<ID>>, &F> for &Numeric
{
    fn assert(
        self,
        output: &super::Numeric<Span<ID>>,
        file: &F,
    ) -> TestCaseResult {
        let source = file.source(output.span.source_id.clone())?;
        prop_assert_eq!(
            self.value.as_str(),
            &source.as_ref()[output.span.range()]
        );
        Ok(())
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, DerefMut,
)]
pub struct DelimitedComment {
    pub comment_body: std::string::String,
}

impl Arbitrary for DelimitedComment {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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

/// Represents an input for the [`super::Punctuation`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Punctuation {
    /// The valid punctuation character.
    pub punctuation: char,
}

impl Arbitrary for Punctuation {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::char::ranges(Cow::Owned(vec![
            ('!'..='/'),
            (':'..='@'),
            ('['..='`'),
            ('{'..='~'),
        ]))
        .prop_filter_map("allows only ascii punctuation", |x| {
            if x.is_ascii_punctuation()
                && x != '_'
                && x != '\''
                && x != '"'
                && x != '#'
            {
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

impl<ID: Debug> Input<&super::Punctuation<Span<ID>>, ()> for &Punctuation {
    fn assert(
        self,
        output: &super::Punctuation<Span<ID>>,
        (): (),
    ) -> TestCaseResult {
        prop_assert_eq!(output.punctuation, self.punctuation);
        Ok(())
    }
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
        let escape_characters = ESCAPE_SEQUENCE_BY_REPRESENTATION
            .left_values()
            .copied()
            .collect::<Vec<_>>();

        prop_oneof![
            proptest::char::any()
                .prop_filter(
                    "filter out single/double quote(s) and backslash",
                    |x| *x != '\'' && *x != '"' && *x != '\\'
                )
                .prop_map(CharacterKind::Normal),
            proptest::sample::select(escape_characters)
                .prop_map(CharacterKind::Escaped)
        ]
        .boxed()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Character {
    pub character: CharacterKind,
}

impl Arbitrary for Character {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;
    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        CharacterKind::arbitrary()
            .prop_map(|character| Self { character })
            .boxed()
    }
}

impl Display for Character {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}'", self.character)
    }
}

impl<ID: Debug> Input<&super::Character<Span<ID>>, ()> for &Character {
    fn assert(
        self,
        output: &super::Character<Span<ID>>,
        (): (),
    ) -> TestCaseResult {
        if let Some(value) = output.value {
            prop_assert_eq!(self.character.get_expected_value(), value);
        } else {
            return Err(TestCaseError::fail(format!(
                "expected {self:?} got {output:?}",
            )));
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct String {
    pub characters: Vec<CharacterKind>,
}

impl Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('"')?;
        for character in &self.characters {
            Display::fmt(character, f)?;
        }
        f.write_char('"')
    }
}

impl Arbitrary for String {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(CharacterKind::arbitrary(), 0..=32)
            .prop_map(|characters| Self { characters })
            .boxed()
    }
}

impl<F: for<'x> Files<'x, FileId = ID>, ID: Debug + Clone>
    Input<&super::String<Span<ID>>, &F> for &String
{
    fn assert(
        self,
        output: &super::String<Span<ID>>,
        file: &F,
    ) -> TestCaseResult {
        let source = file.source(output.span.source_id.clone())?;

        let actual_value = &source.as_ref()[output.span.range()];
        let formatted = self.to_string();

        prop_assert_eq!(actual_value, formatted.as_str());

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
        prop_oneof![Just(Self::LF), Just(Self::Crlf),].boxed()
    }
}

impl Display for NewLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LF => f.write_char('\n'),
            Self::Crlf => f.write_str("\r\n"),
        }
    }
}

impl<F: for<'x> Files<'x, FileId = ID>, ID: Clone + Debug>
    Input<&super::NewLine<Span<ID>>, &F> for &NewLine
{
    fn assert(
        self,
        output: &super::NewLine<Span<ID>>,
        file: &F,
    ) -> TestCaseResult {
        let source = file.source(output.span.source_id.clone())?;
        let actual_value = &source.as_ref()[output.span.range()];

        let expected = match self {
            NewLine::LF => "\n",
            NewLine::Crlf => "\r\n",
        };

        prop_assert_eq!(actual_value, expected);

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Kind {
    Identifier(Identifier),
    Keyword(Keyword),
    Nueric(Numeric),
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
            3 => Numeric::arbitrary().prop_map(Self::Nueric),
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
            Self::Nueric(x) => Display::fmt(x, f),
            Self::Punctuation(x) => Display::fmt(x, f),
            Self::Character(x) => Display::fmt(x, f),
            Self::String(x) => Display::fmt(x, f),
            Self::NewLine(x) => Display::fmt(x, f),
        }
    }
}

impl<F: for<'x> Files<'x, FileId = ID>, ID: Clone + Debug>
    Input<&super::Kind<Span<ID>>, &F> for &Kind
{
    fn assert(
        self,
        output: &super::Kind<Span<ID>>,
        file: &F,
    ) -> TestCaseResult {
        match (self, output) {
            (Kind::Identifier(i), super::Kind::Identifier(o)) => {
                i.assert(o, file)?;
            }
            (Kind::Keyword(i), super::Kind::Keyword(o)) => {
                i.assert(o, ())?;
            }
            (Kind::Nueric(i), super::Kind::Numeric(o)) => {
                i.assert(o, file)?;
            }
            (Kind::Punctuation(i), super::Kind::Punctuation(o)) => {
                i.assert(o, ())?;
            }
            (Kind::Character(i), super::Kind::Character(o)) => {
                i.assert(o, ())?;
            }
            (Kind::String(i), super::Kind::String(o)) => {
                i.assert(o, file)?;
            }
            (Kind::NewLine(i), super::Kind::NewLine(o)) => {
                i.assert(o, file)?;
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PriorInsignificant {
    Tab(usize),
    Whitespace(usize),
    Comment(usize, std::string::String),
}

impl Arbitrary for PriorInsignificant {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            3 => (1usize..10)
                .prop_map(Self::Tab),
            3 => (1usize..10)
                .prop_map(Self::Whitespace),
            1 => (
                1usize..10,
                "[^\\n\\r]*",
            ).prop_map(|(x, y)| {
                Self::Comment(x, y)
            }),
        ]
        .boxed()
    }
}

impl Display for PriorInsignificant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tab(x) => f.write_str(&"\t".repeat(*x)),
            Self::Whitespace(x) => f.write_str(&" ".repeat(*x)),
            Self::Comment(white_count, x) => {
                f.write_str(&" ".repeat(*white_count))?;
                f.write_char('#')?;
                f.write_str(x)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub kind: Kind,
    pub prior_insignificant: Option<PriorInsignificant>,
}

impl Arbitrary for Token {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            Kind::arbitrary(),
            proptest::option::of(PriorInsignificant::arbitrary()),
        )
            .prop_filter_map(
                "comment must be followed by a new line",
                |(kind, prior_insignificant)| {
                    if matches!(
                        prior_insignificant,
                        Some(PriorInsignificant::Comment(..))
                    ) && !kind.is_new_line()
                    {
                        return None;
                    }

                    Some(Self { kind, prior_insignificant })
                },
            )
            .boxed()
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(prior_insignificant) = &self.prior_insignificant {
            Display::fmt(prior_insignificant, f)?;
        }

        Display::fmt(&self.kind, f)
    }
}

impl<F: for<'x> Files<'x, FileId = ID>, ID: Clone + Debug>
    Input<&super::Token<Span<ID>>, &F> for &Token
{
    fn assert(
        self,
        output: &super::Token<Span<ID>>,
        file: &F,
    ) -> TestCaseResult {
        match (&self.prior_insignificant, &output.prior_insignificant) {
            (Some(x), Some(y)) => {
                let source = file.source(y.source_id.clone())?;
                let actual_value = &source.as_ref()[y.range()];

                let expected = x.to_string();

                prop_assert_eq!(actual_value, expected.as_str());
            }

            (None, None) => {}

            (x, y) => {
                return Err(TestCaseError::fail(format!(
                    "expected {x:?} got {y:?}",
                )))
            }
        }

        self.kind.assert(&output.kind, file)
    }
}
