//! Contains the [`Parser`] logic.

use std::sync::Arc;

use derive_more::{Deref, DerefMut};
use displaydoc::Display;
use enum_as_inner::EnumAsInner;
use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, SourceFile, Span},
};
use pernixc_lexical::{
    token::{Punctuation, Token},
    token_stream::{Delimited, Delimiter, TokenStream, TokenTree},
};

use crate::error::{self, Found, SyntaxKind, Unexpected};

/// Contains various data structures used for representing the parser's
/// token expectations.
pub mod expect {

    use pernixc_base::handler::Handler;
    use pernixc_lexical::token;

    use super::{Parser, Reading};
    use crate::error::{self, SyntaxKind};

    /// Used for converting the [`Reading`] into the expected token.
    pub trait Expect: Into<SyntaxKind> {
        /// The expected token output;
        type Output;

        /// Converts the [`Reading`] into the expected token.
        ///
        /// # Errors
        ///
        /// Returns `Err` with original [`Reading`] if the expected token is not
        /// found.
        fn expect(&self, reading: Reading) -> Result<Self::Output, Reading>;

        /// Checks if the expected token is found in the given [`Reading`] and
        /// then invokes the given parser function.
        ///
        /// The parser position will be at the token that satisfies the
        /// expectation.
        fn verify_then_do<
            P: FnOnce(
                &mut Parser,
                &dyn Handler<error::Error>,
            ) -> Result<M, super::Error>,
            M,
        >(
            self,
            parser: P,
        ) -> super::VerifyThenDo<P, Self>
        where
            Self: Sized,
        {
            super::verify_then_do(self, parser)
        }
    }

    /// Expects the [`Reading`] to be an [`token::Identifier`] token.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Identifier;

    impl From<Identifier> for SyntaxKind {
        fn from(_: Identifier) -> Self { SyntaxKind::Identifier }
    }

    impl Expect for Identifier {
        type Output = token::Identifier;

        fn expect(&self, reading: Reading) -> Result<Self::Output, Reading> {
            match reading {
                Reading::Unit(token::Token::Identifier(ident)) => Ok(ident),
                found => Err(found),
            }
        }
    }

    /// Expects the [`Reading`] to be a [`char`] punctuation token.
    ///
    /// In case of '(' '[' '{', the [`Reading`] is expected to be
    /// [`Reading::IntoDelimited`].
    ///
    /// In case of ')' ']' '}', the [`Reading`] is expected to be
    /// [`Reading::DelimitedEnd`].
    pub type Punctuation = char;

    impl From<char> for SyntaxKind {
        fn from(c: char) -> Self { SyntaxKind::Punctuation(c) }
    }

    impl Expect for char {
        type Output = token::Punctuation;

        fn expect(&self, reading: Reading) -> Result<Self::Output, Reading> {
            match *self {
                '{' | '(' | '[' => match reading {
                    Reading::IntoDelimited(_, punc) => Ok(punc),
                    found => Err(found),
                },
                '}' | ')' | ']' => match reading {
                    Reading::DelimitedEnd(_, punc) => Ok(punc),
                    found => Err(found),
                },
                _ => match reading {
                    Reading::Unit(token::Token::Punctuation(punc))
                        if punc.punctuation == *self =>
                    {
                        Ok(punc)
                    }
                    found => Err(found),
                },
            }
        }
    }

    /// Expects the [`Reading`] to be a [`Keyword`] token of the specific
    /// kind.
    pub type Keyword = pernixc_lexical::token::KeywordKind;

    impl From<Keyword> for SyntaxKind {
        fn from(k: Keyword) -> Self { SyntaxKind::Keyword(k) }
    }

    impl Expect for Keyword {
        type Output = token::Keyword;

        fn expect(&self, reading: Reading) -> Result<Self::Output, Reading> {
            match reading {
                Reading::Unit(token::Token::Keyword(keyword))
                    if keyword.kind == *self =>
                {
                    Ok(keyword)
                }
                found => Err(found),
            }
        }
    }

    /// Expects the [`Reading`] to be a [`Numeric`] literal token.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Numeric;

    impl From<Numeric> for SyntaxKind {
        fn from(_: Numeric) -> Self { SyntaxKind::Numeric }
    }

    impl Expect for Numeric {
        type Output = token::Numeric;

        fn expect(&self, reading: Reading) -> Result<Self::Output, Reading> {
            match reading {
                Reading::Unit(token::Token::Numeric(numeric)) => Ok(numeric),
                found => Err(found),
            }
        }
    }

    /// Expects the [`Reading`] to be a [`token::String`] literal token.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct String;

    impl From<String> for SyntaxKind {
        fn from(_: String) -> Self { SyntaxKind::String }
    }

    impl Expect for String {
        type Output = token::String;

        fn expect(&self, reading: Reading) -> Result<Self::Output, Reading> {
            match reading {
                Reading::Unit(token::Token::String(string)) => Ok(string),
                found => Err(found),
            }
        }
    }

    /// Expects the [`Reading`] to be a [`token::Character`] literal token.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Character;

    impl From<Character> for SyntaxKind {
        fn from(_: Character) -> Self { SyntaxKind::Character }
    }

    impl Expect for Character {
        type Output = token::Character;

        fn expect(&self, reading: Reading) -> Result<Self::Output, Reading> {
            match reading {
                Reading::Unit(token::Token::Character(character)) => {
                    Ok(character)
                }
                found => Err(found),
            }
        }
    }

    /// Expects the [`Reading::IntoDelimited`]` with the given delimiter.
    pub type IntoDelimited = super::Delimiter;

    impl From<IntoDelimited> for SyntaxKind {
        fn from(delimiter: IntoDelimited) -> Self {
            match delimiter {
                super::Delimiter::Parenthesis => SyntaxKind::Punctuation('('),
                super::Delimiter::Brace => SyntaxKind::Punctuation('{'),
                super::Delimiter::Bracket => SyntaxKind::Punctuation('['),
            }
        }
    }

    impl Expect for IntoDelimited {
        type Output = token::Punctuation;

        fn expect(&self, reading: Reading) -> Result<Self::Output, Reading> {
            match reading {
                Reading::IntoDelimited(delimiter, punc)
                    if delimiter == *self =>
                {
                    Ok(punc)
                }
                found => Err(found),
            }
        }
    }
}

pub use expect::{
    Character as ExpectCharacter, Identifier as ExpectIdentifier,
    Keyword as ExpectKeyword, Numeric as ExpectNumeric, String as ExpectString,
};

macro_rules! expect_implements_syntax {
    ($t:ty) => {
        impl sealed::Syntax for $t {
            type Output = <$t as expect::Expect>::Output;

            fn parse_internal(
                &mut self,
                parser: &mut Parser,
                dont_skip_to_significant: bool,
                _: &dyn Handler<error::Error>,
            ) -> Result<Self::Output, sealed::Error> {
                use expect::Expect;

                let (reading, position) = if dont_skip_to_significant {
                    (parser.peek(), parser.current_index)
                } else {
                    parser.peek_significant()
                };

                let Ok(result) = self.expect(reading) else {
                    return Err(sealed::Error::ConditionNotMet);
                };

                parser.current_index = position;
                parser.forward();

                Ok(result)
            }

            fn get_errors(self, expected: &mut Vec<SyntaxKind>) {
                expected.push(self.into());
            }
        }
    };
}

expect_implements_syntax!(expect::Character);
expect_implements_syntax!(expect::Identifier);
expect_implements_syntax!(expect::Keyword);
expect_implements_syntax!(expect::Numeric);
expect_implements_syntax!(expect::String);
expect_implements_syntax!(expect::Punctuation);
expect_implements_syntax!(expect::IntoDelimited);

/**
encountered a fatal syntax error, which aborts the parsing process; the error
information has been reported to the designated error handler.
 */
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Display,
    thiserror::Error,
)]
pub struct Error;

mod sealed {
    use pernixc_base::handler::Handler;

    use super::Parser;
    use crate::error::{self, SyntaxKind};

    #[derive(
        Debug,
        Clone,
        Copy,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        Hash,
        derive_more::From,
    )]
    #[allow(missing_docs)]
    pub enum Error {
        ConditionNotMet,
        Commited(super::Error),
    }

    pub trait Syntax {
        type Output;

        /// Parses the condition and returns the output if the condition is met.
        ///
        /// # Invariants
        ///
        /// This function will be called mostly once per instance. However,
        /// the funcion takes `&mut self` to allow the implementation to call
        /// `get_errors` to collect the expected syntax kinds.
        fn parse_internal(
            &mut self,
            parser: &mut Parser,
            dont_skip_to_significant: bool,
            handler: &dyn Handler<error::Error>,
        ) -> Result<Self::Output, Error>;

        /// Collects all the expected syntax kinds by the condition.
        fn get_errors(self, expected: &mut Vec<SyntaxKind>);
    }
}

/// A trait describing how to parse a particular syntax tree.
///
/// # Note
///
/// The term "condition" is used to describe what the parser is looking for in
/// the token stream to start parsing the syntax tree. When the condition is
/// met, the parser will start parsing the syntax tree.
pub trait Syntax: sealed::Syntax {
    /// Takes the `self` and chains it with the next [`Syntax`] to create an
    /// alternative condition if the `self` fails. If the `self` fails, then
    /// the next condition will be attempted.
    fn or_else<C: Syntax<Output = Self::Output>>(
        self,
        next: C,
    ) -> OrElse<Self, C>
    where
        Self: Sized,
    {
        OrElse { previous: self, next }
    }

    /// Maps the output of the syntax tree to another output.
    fn map<M: FnOnce(Self::Output) -> O, O>(self, map_fn: M) -> Map<Self, M>
    where
        Self: Sized,
    {
        Map { condition: self, map_fn: Some(map_fn) }
    }

    /// Executes the given function after the current condition is met and
    /// successfully parsed.
    ///
    /// # Note
    ///
    /// Most of the time, you'll use tuple of syntaxes to parse multiple
    /// syntaxes at once instead of using this function.
    ///
    /// Or you can use this function to break the recusive dependency between
    /// the syntaxes.
    fn then_do<
        P: FnOnce(
            &mut Parser,
            Self::Output,
            &dyn Handler<error::Error>,
        ) -> Result<M, Error>,
        M,
    >(
        self,
        parser: P,
    ) -> ThenDo<Self, P>
    where
        Self: Sized,
    {
        ThenDo { syntax: self, parser_fn: Some(parser) }
    }

    /// If the condition is met, then the parser will parse normally; otherwise,
    /// just return `None`.
    fn or_none(self) -> OrNone<Self>
    where
        Self: Sized,
    {
        OrNone { condition: self }
    }

    /// Chains the current condition with the next condition. The next condition
    /// will be attempted if the current condition is met.
    fn and<N: Syntax>(self, next: N) -> And<Self, N>
    where
        Self: Sized,
    {
        And { previous: self, next }
    }
}

impl<T: sealed::Syntax> Syntax for T {}

/// A combinator created from [`Syntax::and`] function. See [`Syntax::and`] for
/// more information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct And<P, N> {
    previous: P,
    next: N,
}

impl<P: sealed::Syntax, N: sealed::Syntax> sealed::Syntax for And<P, N> {
    type Output = (P::Output, N::Output);

    fn parse_internal(
        &mut self,
        parser: &mut Parser,
        dont_skip_to_significant: bool,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, sealed::Error> {
        let previous_output = self.previous.parse_internal(
            parser,
            dont_skip_to_significant,
            handler,
        )?;

        let next_output = self.next.parse_internal(
            parser,
            dont_skip_to_significant,
            handler,
        )?;

        Ok((previous_output, next_output))
    }

    fn get_errors(self, expected: &mut Vec<SyntaxKind>) {
        self.previous.get_errors(expected);
        self.next.get_errors(expected);
    }
}

macro_rules! implements_syntax_tuple {
    ($($t:ident),*) => {
        impl<$($t: sealed::Syntax),*> sealed::Syntax for ($($t,)*) {
            type Output = ($($t::Output,)*);

            #[allow(non_snake_case)]
            fn parse_internal(
                &mut self,
                parser: &mut Parser,
                dont_skip_to_significant: bool,
                handler: &dyn Handler<error::Error>,
            ) -> Result<Self::Output, sealed::Error> {
                let ($(ref mut $t,)*) = *self;

                $(
                    let $t = $t.parse_internal(parser, dont_skip_to_significant, handler)?;
                )*

                Ok(($($t,)*))
            }

            #[allow(non_snake_case)]
            fn get_errors(self, expected: &mut Vec<SyntaxKind>) {
                let ($($t,)*) = self;

                $(
                    $t.get_errors(expected);
                )*
            }
        }
    };
}

implements_syntax_tuple!(A, B);
implements_syntax_tuple!(A, B, C);
implements_syntax_tuple!(A, B, C, D);
implements_syntax_tuple!(A, B, C, D, E);
implements_syntax_tuple!(A, B, C, D, E, F);
implements_syntax_tuple!(A, B, C, D, E, F, G);
implements_syntax_tuple!(A, B, C, D, E, F, G, H);
implements_syntax_tuple!(A, B, C, D, E, F, G, H, I);
implements_syntax_tuple!(A, B, C, D, E, F, G, H, I, J);
implements_syntax_tuple!(A, B, C, D, E, F, G, H, I, J, K);
implements_syntax_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);
implements_syntax_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M);
implements_syntax_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N);
implements_syntax_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O);
implements_syntax_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);

/// A combinator created from [`Syntax::or_none`] function. See
/// [`Syntax::or_none`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OrNone<S> {
    condition: S,
}

impl<S: sealed::Syntax> sealed::Syntax for OrNone<S> {
    type Output = Option<S::Output>;

    fn parse_internal(
        &mut self,
        parser: &mut Parser,
        dont_skip_to_significant: bool,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, sealed::Error> {
        match self.condition.parse_internal(
            parser,
            dont_skip_to_significant,
            handler,
        ) {
            Ok(output) => Ok(Some(output)),
            Err(sealed::Error::ConditionNotMet) => Ok(None),
            Err(sealed::Error::Commited(err)) => {
                Err(sealed::Error::Commited(err))
            }
        }
    }

    fn get_errors(self, _: &mut Vec<SyntaxKind>) {}
}

/// A combinator created from [`Syntax::map`] function. See [`Syntax::map`] for
/// more information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Map<C, M> {
    condition: C,
    map_fn: Option<M>,
}

impl<C: sealed::Syntax, M: FnOnce(C::Output) -> O, O> sealed::Syntax
    for Map<C, M>
{
    type Output = O;

    fn parse_internal(
        &mut self,
        parser: &mut Parser,
        dont_skip_to_significant: bool,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, sealed::Error> {
        let output = self.condition.parse_internal(
            parser,
            dont_skip_to_significant,
            handler,
        )?;

        let map_fn = self.map_fn.take().expect("`parse` called more than once");

        Ok(map_fn(output))
    }

    fn get_errors(self, expected: &mut Vec<SyntaxKind>) {
        self.condition.get_errors(expected);
    }
}

/// A combinator created from [`just`] function. See [`just`] for more
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Just<T>(Option<T>);

/// Creates a combinator that always returns the given result with no required
/// conditions.
pub fn just<T>(result: T) -> Just<T> { Just(Some(result)) }

impl<T> sealed::Syntax for Just<T> {
    type Output = T;

    fn parse_internal(
        &mut self,
        _: &mut Parser,
        _: bool,
        _: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, sealed::Error> {
        Ok(self.0.take().expect("`parse` called more than once"))
    }

    fn get_errors(self, _: &mut Vec<SyntaxKind>) {}
}

/// A combinator created from [`verify_then_do`] function. See
/// [`verify_then_do`] for more information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VerifyThenDo<P, E> {
    parser_fn: Option<P>,
    expect: E,
}

/// A combinator for conditionally startting the parsing process if the first
/// token meets the expectation.
///
/// The parser will stop right at the position where the expectation is met.
///
/// For example, to start parsing the if-else expression, the `if` keyword is
/// expected, and the whole if-else expression is parsed if the expectation is
/// met.
pub fn verify_then_do<
    E: expect::Expect,
    P: FnOnce(&mut Parser, &dyn Handler<error::Error>) -> Result<M, Error>,
    M,
>(
    expect: E,
    parser: P,
) -> VerifyThenDo<P, E> {
    VerifyThenDo { parser_fn: Some(parser), expect }
}

impl<
        E: expect::Expect,
        P: FnOnce(&mut Parser, &dyn Handler<error::Error>) -> Result<M, Error>,
        M,
    > sealed::Syntax for VerifyThenDo<P, E>
{
    type Output = M;

    fn parse_internal(
        &mut self,
        parser: &mut Parser,
        dont_skip_to_significant: bool,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, sealed::Error> {
        // gets the reading and the next position
        let (reading, index) = if dont_skip_to_significant {
            (parser.peek(), parser.current_index)
        } else {
            parser.peek_significant()
        };

        let Ok(_) = self.expect.expect(reading) else {
            return Err(sealed::Error::ConditionNotMet);
        };

        // the condition is considered met
        parser.current_index = index;
        parser.forward();

        let parser_fn =
            self.parser_fn.take().expect("`parse` called more than once");

        Ok(parser_fn(parser, handler)?)
    }

    fn get_errors(self, expected: &mut Vec<SyntaxKind>) {
        expected.push(self.expect.into());
    }
}

/// A combinator created from [`if_and_then`] function. See [`if_and_then`] for
/// more information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IfAndThen<P, E> {
    expect: E,
    parser_fn: Option<P>,
}

impl<
        E: expect::Expect,
        P: FnOnce(
            &mut Parser,
            E::Output,
            &dyn Handler<error::Error>,
        ) -> Result<M, Error>,
        M,
    > sealed::Syntax for IfAndThen<P, E>
{
    type Output = M;

    fn parse_internal(
        &mut self,
        parser: &mut Parser,
        dont_skip_to_significant: bool,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, sealed::Error> {
        // gets the reading and the next position
        let (reading, index) = if dont_skip_to_significant {
            (parser.peek(), parser.current_index)
        } else {
            parser.peek_significant()
        };

        let expected = self
            .expect
            .expect(reading)
            .map_err(|_| sealed::Error::ConditionNotMet)?;

        // the condition is considered met
        parser.current_index = index;
        parser.forward();

        let parser_fn =
            self.parser_fn.take().expect("`parse` called more than once");

        Ok(parser_fn(parser, expected, handler)?)
    }

    fn get_errors(self, expected: &mut Vec<SyntaxKind>) {
        expected.push(self.expect.into());
    }
}

/// A combinator created from [`Syntax::and_then`] function. See
/// [`Syntax::and_then`] for more
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ThenDo<S, P> {
    syntax: S,
    parser_fn: Option<P>,
}

impl<
        S: sealed::Syntax,
        P: FnOnce(
            &mut Parser,
            S::Output,
            &dyn Handler<error::Error>,
        ) -> Result<M, Error>,
        M,
    > sealed::Syntax for ThenDo<S, P>
{
    type Output = M;

    fn parse_internal(
        &mut self,
        parser: &mut Parser,
        dont_skip_to_significant: bool,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, sealed::Error> {
        let output = self.syntax.parse_internal(
            parser,
            dont_skip_to_significant,
            handler,
        )?;

        let parser_fn =
            self.parser_fn.take().expect("`parse` called more than once");

        Ok(parser_fn(parser, output, handler)?)
    }

    fn get_errors(self, expected: &mut Vec<SyntaxKind>) {
        self.syntax.get_errors(expected);
    }
}

/// A combinator for conditionally starting the parsing process if the first
/// token meets the expectation.
///
/// The combinator works similarly to [`if_then`], but the parser will move
/// forward after the token that satisfies the expectation and parser function
/// takes the expected token as an argument.
pub fn if_and_then<
    E: expect::Expect,
    P: FnOnce(&mut Parser, E::Output) -> Result<M, Error>,
    M,
>(
    expect: E,
    parser: P,
) -> IfAndThen<P, E> {
    IfAndThen { expect, parser_fn: Some(parser) }
}

/// A struct used for chaining the parsing conditions. If the previous condition
/// fails, then the chain will try the next condition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OrElse<P, N> {
    previous: P,
    next: N,
}

impl<P: sealed::Syntax, N: sealed::Syntax<Output = P::Output>> sealed::Syntax
    for OrElse<P, N>
{
    type Output = P::Output;

    fn parse_internal(
        &mut self,
        parser: &mut Parser,
        dont_skip_significant: bool,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, sealed::Error> {
        match self.previous.parse_internal(
            parser,
            dont_skip_significant,
            handler,
        ) {
            Ok(output) => return Ok(output),
            Err(sealed::Error::Commited(err)) => {
                return Err(sealed::Error::Commited(err))
            }
            Err(sealed::Error::ConditionNotMet) => {}
        };

        self.next.parse_internal(parser, dont_skip_significant, handler)
    }

    fn get_errors(self, expected: &mut Vec<SyntaxKind>) {
        self.previous.get_errors(expected);
        self.next.get_errors(expected);
    }
}

/// Provides a way to iterate over a token stream.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum TokenProvider<'a> {
    /// Iterating at the top level of the token stream.
    TokenStream(&'a TokenStream),

    /// Iterating inside a delimited token stream.
    Delimited(&'a Delimited),
}

impl<'a> TokenProvider<'a> {
    /// Gets the token stream of the current token provider.
    #[must_use]
    pub const fn token_stream(&self) -> &'a TokenStream {
        match self {
            TokenProvider::TokenStream(token_stream) => token_stream,
            TokenProvider::Delimited(delimited) => &delimited.token_stream,
        }
    }
}

/// Represents a single frame of the parser's stack, responsible for reading a
/// token stream in that given token stream level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Frame<'a> {
    token_provider: TokenProvider<'a>,
    current_index: usize,
}

/// Represents the read value of the [`Frame`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Reading {
    /// A singular token.
    Unit(Token),

    /// Found an opening delimiter token, which means that the parser can step
    /// into a new delimited frame.
    IntoDelimited(Delimiter, Punctuation),

    /// Found a closing delimiter token, which means that the parser should
    /// step out of the current delimited frame.
    DelimitedEnd(Delimiter, Punctuation),

    /// End of file.
    Eof,
}

impl Reading {
    /// Gets the read token inside the [`Reading`] as `Option<Token>`
    ///
    /// # Returns
    ///
    /// Returns `None` if the [`Reading`] is [`Reading::Eof`].
    #[must_use]
    pub fn into_token(self) -> Option<Token> {
        match self {
            Self::Unit(token) => Some(token),
            Self::IntoDelimited(_, pun) | Self::DelimitedEnd(_, pun) => {
                Some(Token::Punctuation(pun))
            }
            Self::Eof => None,
        }
    }
}

impl<'a> Frame<'a> {
    /// Checks if the current [`Frame`] doesn't have any more significant
    /// [`TokenTree`]s to parse.
    #[must_use]
    pub fn is_exhausted(&self) -> bool {
        let token_stream = self.token_provider.token_stream();
        for i in self.current_index..self.token_provider.token_stream().len() {
            if !matches!(
                token_stream.get(i),
                Some(TokenTree::Token(
                    Token::WhiteSpaces(..) | Token::Comment(..)
                ))
            ) {
                return false;
            }
        }
        true
    }

    /// Checks if the current [`Frame`] has reached the end of the
    /// [`TokenStream`].
    #[must_use]
    pub fn is_end(&self) -> bool {
        self.current_index >= self.token_provider.token_stream().len()
    }

    fn get_reading(&self, token: Option<&TokenTree>) -> Reading {
        token.map_or_else(
            || {
                match self.token_provider {
                    // end of file
                    TokenProvider::TokenStream(..) => Reading::Eof,
                    TokenProvider::Delimited(delimited) => {
                        Reading::DelimitedEnd(
                            delimited.delimiter,
                            delimited.close.clone(),
                        )
                    }
                }
            },
            |token| match token {
                TokenTree::Token(token) => Reading::Unit(token.clone()),
                TokenTree::Delimited(delimited) => Reading::IntoDelimited(
                    delimited.delimiter,
                    delimited.open.clone(),
                ),
            },
        )
    }

    /// Returns a [`Token`] pointing by the `current_index` of the [`Frame`].
    #[must_use]
    pub fn peek(&self) -> Reading {
        self.get_reading(
            self.token_provider.token_stream().get(self.current_index),
        )
    }

    /// Returns a [`Token`] pointing by the `current_index` with the given index
    /// offset of the [`Frame`].
    ///
    /// # Returns
    ///
    /// `None` if `offset + current_index` is less than zero or greer than
    /// `self.token_provider.token_stream().len() + 1`
    #[must_use]
    #[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
    pub fn peek_offset(&self, offset: isize) -> Option<Reading> {
        let index = self.current_index as isize + offset;
        let index = if index < 0 {
            return None;
        } else {
            index as usize
        };

        if index > self.token_provider.token_stream().len() + 1 {
            return None;
        }

        Some(self.get_reading(self.token_provider.token_stream().get(index)))
    }

    /// Returns a [`Token`] pointing by the `current_index` of the [`Frame`] and
    /// increments the `current_index` by 1.
    #[must_use = "use `forward` instead if you don't need the token and just \
                  want to increment the index"]
    pub fn next_token(&mut self) -> Reading {
        let token = self.peek();

        // increment the index
        self.forward();

        token
    }

    /// Peeks for the significant [`Reading`] from the current position
    /// (including current position).
    ///
    /// # Returns
    ///
    /// Returns the significant token and its index.
    pub fn peek_significant(&self) -> (Reading, usize) {
        for i in self.current_index..=self.token_provider.token_stream().len() {
            let reading =
                self.get_reading(self.token_provider.token_stream().get(i));

            if reading.as_unit().map_or(true, Token::is_significant) {
                return (reading, i);
            }
        }

        (
            match self.token_provider {
                // end of file
                TokenProvider::TokenStream(..) => Reading::Eof,
                TokenProvider::Delimited(delimited) => Reading::DelimitedEnd(
                    delimited.delimiter,
                    delimited.close.clone(),
                ),
            },
            self.token_provider.token_stream().len(),
        )
    }

    /// Forwards the `current_index` by 1 if the [`Frame`] is not exhausted.
    pub fn forward(&mut self) {
        // increment the index
        if !self.is_end() {
            self.current_index += 1;
        }
    }

    /// Skips any insignificant [`Token`]s, returns the next significant
    /// [`Token`] found, and increments the `current_index` afterward.
    pub fn next_significant_token(&mut self) -> Reading {
        let token = self.stop_at_significant();

        // increment the index
        self.forward();

        token
    }

    /// Makes the current [`Frame`] point to the significant [`Token`] if
    /// currently not.
    ///
    /// # Returns
    /// The significant [`Token`] if found, otherwise `None`.
    pub fn stop_at_significant(&mut self) -> Reading {
        while !self.is_end() {
            let token = self.peek();

            if !matches!(
                token,
                Reading::Unit(Token::WhiteSpaces(..) | Token::Comment(..))
            ) {
                return token;
            }

            self.forward();
        }

        match self.token_provider {
            TokenProvider::TokenStream(..) => Reading::Eof,
            TokenProvider::Delimited(delimited) => Reading::DelimitedEnd(
                delimited.delimiter,
                delimited.close.clone(),
            ),
        }
    }

    /// Makes the current position stops at the first token that satisfies the
    /// predicate.
    pub fn stop_at(&mut self, predicate: impl Fn(&Reading) -> bool) -> Reading {
        while !self.is_end() {
            let token = self.peek();

            if predicate(&token) {
                return token;
            }

            self.current_index += 1;
        }

        match self.token_provider {
            TokenProvider::TokenStream(..) => Reading::Eof,
            TokenProvider::Delimited(delimited) => Reading::DelimitedEnd(
                delimited.delimiter,
                delimited.close.clone(),
            ),
        }
    }
}

/// The parser of the compiler.
#[derive(Debug, Clone, Deref, DerefMut)]
pub struct Parser<'a> {
    #[deref]
    #[deref_mut]
    current_frame: Frame<'a>,
    stack: Vec<Frame<'a>>,

    /// The source file being parsed.
    source_file: Arc<SourceFile>,
}

/// Represents a tree that is delimited by a pair of punctuations.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DelimitedTree<T> {
    /// The opening delimiter.
    pub open: Punctuation,

    /// The inner tree inside the delimiter.
    pub tree: T,

    /// The closing delimiter.
    pub close: Punctuation,
}

impl<T> SourceElement for DelimitedTree<T> {
    fn span(&self) -> Span { self.open.span.join(&self.close.span).unwrap() }
}

/// Conditionally steps into a delimited tree of the given `delimiter` and parse
/// the `tree` inside the delimiter.
pub fn delimited_tree<S: Syntax>(
    delimiter: Delimiter,
    tree: S,
) -> impl Syntax<Output = DelimitedTree<S::Output>> {
    verify_then_do(delimiter, move |parser, handler| {
        let StepIntoTree { open, tree, close } = parser.step_into(
            delimiter,
            |parser| parser.parse(tree, handler),
            handler,
        )?;

        Ok(DelimitedTree { open, tree: tree?, close })
    })
}

/// Represents a result of [`Parser::step_into()`] function.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StepIntoTree<T> {
    /// The opening delimiter.
    pub open: Punctuation,

    /// The tree inside the delimiter.
    pub tree: Result<T, Error>,

    /// The closing delimiter.
    pub close: Punctuation,
}

impl<'a> Parser<'a> {
    /// Creates a new parser from the given token stream.
    ///
    /// The source file argument should be the source file that the token stream
    /// is from. The source file doesn't influence the parsing process, but it
    /// is used to provide better error messages.
    #[must_use]
    pub const fn new(
        token_stream: &'a TokenStream,
        source_file: Arc<SourceFile>,
    ) -> Self {
        Self {
            current_frame: Frame {
                token_provider: TokenProvider::TokenStream(token_stream),
                current_index: 0,
            },
            stack: Vec::new(),
            source_file,
        }
    }

    /// Parses the given condition and returns the output if the condition is
    /// met.
    ///
    /// The parser will skip to the nearest significant token and start testing
    /// the condition from there.
    pub fn parse<S: Syntax>(
        &mut self,
        syntax: S,
        handler: &dyn Handler<error::Error>,
    ) -> Result<S::Output, Error> {
        self.parse_full(syntax, false, handler)
    }

    fn parse_full<S: Syntax>(
        &mut self,
        mut syntax: S,
        dont_skip_to_significant: bool,
        handler: &dyn Handler<error::Error>,
    ) -> Result<S::Output, Error> {
        match syntax.parse_internal(self, dont_skip_to_significant, handler) {
            Ok(output) => return Ok(output),
            Err(sealed::Error::Commited(err)) => return Err(err),
            Err(sealed::Error::ConditionNotMet) => {}
        }

        let mut expected = Vec::new();
        syntax.get_errors(&mut expected);

        handler.receive(error::Error {
            expected_syntaxes: expected,
            found: self.reading_to_found(self.peek()),
        });

        // make the progress if the condition is not met
        if !dont_skip_to_significant {
            self.stop_at_significant();
        }
        self.forward();

        Err(Error)
    }

    /// Parses the given condition and returns the output if the condition is
    /// met.
    ///
    /// The parser will not skip to the nearest significant token like the
    /// [`Self::parse`] does.
    pub fn parse_dont_skip<S: Syntax>(
        &mut self,
        condition: S,
        handler: &dyn Handler<error::Error>,
    ) -> Result<S::Output, Error> {
        self.parse_full(condition, true, handler)
    }

    /// Steps into the [`Delimited`] token stream and parses the content within
    /// the delimiters.
    ///
    /// The parser's position must be at the delimited token stream.
    pub fn step_into<T>(
        &mut self,
        delimiter: Delimiter,
        f: impl FnOnce(&mut Self) -> Result<T, Error>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<StepIntoTree<T>, Error> {
        self.current_frame.stop_at_significant();
        let raw_token_tree = self
            .current_frame
            .token_provider
            .token_stream()
            .get(self.current_frame.current_index);

        // move after the whole delimited list
        self.current_frame.forward();

        let expected = match delimiter {
            Delimiter::Parenthesis => '(',
            Delimiter::Brace => '{',
            Delimiter::Bracket => '[',
        };

        let delimited_stream = if let Some(token_tree) = raw_token_tree {
            match token_tree {
                TokenTree::Delimited(delimited_tree)
                    if delimited_tree.delimiter == delimiter =>
                {
                    delimited_tree
                }
                found => {
                    handler.receive(error::Error {
                        expected_syntaxes: vec![SyntaxKind::Punctuation(
                            expected,
                        )],
                        found: match found {
                            TokenTree::Token(token) => {
                                Found::Unexpected(Unexpected {
                                    prior_insignificant: self
                                        .peek_offset(-1)
                                        .and_then(Reading::into_token)
                                        .filter(|x| !x.is_significant()),
                                    unexpected: token.clone(),
                                })
                            }
                            TokenTree::Delimited(delimited_tree) => {
                                Found::Unexpected(Unexpected {
                                    prior_insignificant: self
                                        .peek_offset(-1)
                                        .and_then(Reading::into_token)
                                        .filter(|x| !x.is_significant()),
                                    unexpected: Token::Punctuation(
                                        delimited_tree.open.clone(),
                                    ),
                                })
                            }
                        },
                    });

                    return Err(Error);
                }
            }
        } else {
            handler.receive(error::Error {
                expected_syntaxes: vec![SyntaxKind::Punctuation(expected)],
                found: self.reading_to_found(self.get_reading(None)),
            });

            return Err(Error);
        };

        // creates a new frame
        let new_frame = Frame {
            token_provider: TokenProvider::Delimited(delimited_stream),
            current_index: 0,
        };

        // pushes the current frame onto the stack and replaces the current
        // frame with the new one
        self.stack.push(std::mem::replace(&mut self.current_frame, new_frame));

        let open = delimited_stream.open.clone();

        let tree = f(self);

        // pops the current frame off the stack
        let new_frame = self.stack.pop().ok_or(Error)?;

        // the current frame must be at the end
        if !self.current_frame.is_exhausted() {
            let expected = match self
                .current_frame
                .token_provider
                .as_delimited()
                .unwrap()
                .delimiter
            {
                Delimiter::Parenthesis => ')',
                Delimiter::Brace => '}',
                Delimiter::Bracket => ']',
            };

            handler.receive(error::Error {
                expected_syntaxes: vec![SyntaxKind::Punctuation(expected)],
                found: self.reading_to_found(self.peek()),
            });
        }

        let close_punctuation = self
            .current_frame
            .token_provider
            .as_delimited()
            .unwrap()
            .close
            .clone();

        // replaces the current frame with the popped one
        self.current_frame = new_frame;

        Ok(StepIntoTree { open, tree, close: close_punctuation })
    }

    /// Tries to parse the given function and returns the output if the parsing
    /// is successful. Otherwise, the parser will revert to the original
    /// position.
    pub fn try_parse<M>(
        &mut self,
        parse_fn: impl FnOnce(&mut Parser) -> Result<M, Error>,
    ) -> Option<M> {
        let current_index = self.current_frame.current_index;

        match parse_fn(self) {
            Ok(result) => Some(result),
            Err(Error) => {
                self.current_frame.current_index = current_index;
                None
            }
        }
    }

    /// Converts the reading to [`Found`] for error reporting.
    pub(crate) fn reading_to_found(&self, reading: Reading) -> Found {
        reading.into_token().map_or_else(
            || Found::EndOfFile(self.source_file.clone()),
            |x| {
                Found::Unexpected(Unexpected {
                    prior_insignificant: self
                        .peek_offset(-1)
                        .and_then(Reading::into_token)
                        .filter(|x| !x.is_significant()),
                    unexpected: x,
                })
            },
        )
    }
}
