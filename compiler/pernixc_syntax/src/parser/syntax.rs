//! Contains the definition of [`Syntax`] trait and its implementations and
//! combinators.
//!
//! This makes the writing of the parser more declarative and easier to read.

use pernixc_base::handler::Handler;
use pernixc_lexical::{token::Punctuation, token_stream::Delimiter};

use super::{
    expect::{self, Expect},
    Parser, StepIntoTree,
};
use crate::error::{self, SyntaxKind};

/// An extension trait allowing the [`Expect`] to provide useful combinators
/// for [`Syntax`] trait.
pub trait ExpectExt: Expect {
    /// Conditionally invokes the closure if the expectation is met.
    fn then_do<
        F: FnOnce(&mut Parser, Self::Output, &dyn Handler<error::Error>) -> O,
        O,
    >(
        self,
        then: F,
    ) -> ThenDo<Self, F>
    where
        Self: Sized,
    {
        ThenDo { expect: self, then }
    }

    /// Instructs the parser to not skip to nearest significant token.
    fn dont_skip(self) -> DontSkip<Self>
    where
        Self: Sized,
    {
        DontSkip(self)
    }
}

impl<E: Expect> ExpectExt for E {}

/// The error that can occur while parsing the syntax.
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
    displaydoc::Display,
)]
#[allow(missing_docs)]
pub enum Error<I> {
    /**
    The condition was not met, the parsing hasn't been done. The position
    should be the same before the parsing.
    */
    ConditionNotMet(usize, I),

    /// The parser has already commited the parsing and found an error
    #[from]
    Commited(super::Error),
}

/// A trait describing how the parser should parse the syntax.
pub trait Syntax {
    /// The syntax tree output.
    type Output;

    /// The iterator type for the list of expected syntax kinds.
    type Iter: Iterator<Item = SyntaxKind>;

    /// The flag specifying whether the syntax has any condition to be met.
    const HAS_CONDITION: bool;

    /// Parses the condition and returns the output if the condition is met.
    ///
    /// # Invariants
    ///
    /// This function will be called mostly once per instance. However,
    /// the funcion takes `&mut self` to allow the implementation to call
    /// `get_errors` to collect the expected syntax kinds.
    fn step(
        self,
        parser: &mut Parser,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, Error<Self::Iter>>;

    /// Attempt to parse the `self` syntax and if it fails, parse the `next`
    /// syntax.
    fn or_else<N: Syntax<Output = Self::Output>>(
        self,
        next: N,
    ) -> OrElse<Self, N>
    where
        Self: Sized,
    {
        OrElse { previous: self, next }
    }

    /// Maps the output of the syntax to another output.
    fn map<F, O>(self, map: F) -> Map<Self, F>
    where
        Self: Sized,
        F: FnOnce(Self::Output) -> O,
    {
        Map { syntax: self, map }
    }

    /// Repeats the syntax until the condition is not met.
    fn loop_while(self) -> While<Self>
    where
        Self: Sized,
    {
        While { syntax: self }
    }

    /// Continues the parsing with the closure after the syntax is parsed.
    fn and_then_do<F, O>(self, then: F) -> AndThenDo<Self, F>
    where
        Self: Sized,
        F: FnOnce(
            &mut Parser,
            Self::Output,
            &dyn Handler<error::Error>,
        ) -> Result<O, super::Error>,
        AndThenDo<Self, F>: Syntax,
    {
        AndThenDo { syntax: self, then }
    }

    /// Returns [`None`] if the condition of the syntax is not met.
    fn or_none(self) -> OrNone<Self>
    where
        Self: Sized,
    {
        OrNone(self)
    }
}

macro_rules! expect_implements_syntax {
    ($t:ty) => {
        impl Syntax for $t {
            type Output = <$t as Expect>::Output;

            type Iter = std::iter::Once<SyntaxKind>;

            const HAS_CONDITION: bool = true;

            #[inline(always)]
            fn step(
                self,
                parser: &mut Parser,
                _: &dyn Handler<error::Error>,
            ) -> Result<Self::Output, Error<Self::Iter>> {
                let (reading, position) = parser.peek_significant();

                let Ok(result) = self.expect(reading) else {
                    return Err(Error::ConditionNotMet(
                        position,
                        std::iter::once(self.into()),
                    ));
                };

                parser.current_index = position;
                parser.forward();

                Ok(result)
            }
        }
    };
}

/// Lazily computes the [`Syntax`] when it is needed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct With<F>(pub F);

impl<F: FnOnce() -> S, S: Syntax> Syntax for With<F> {
    type Output = S::Output;

    type Iter = S::Iter;

    const HAS_CONDITION: bool = S::HAS_CONDITION;

    #[inline(always)]
    fn step(
        self,
        parser: &mut Parser,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, Error<Self::Iter>> {
        self.0().step(parser, handler)
    }
}

impl<S: Syntax> Syntax for fn() -> S {
    type Output = S::Output;

    type Iter = S::Iter;

    const HAS_CONDITION: bool = S::HAS_CONDITION;

    fn step(
        self,
        parser: &mut Parser,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, Error<Self::Iter>> {
        self().step(parser, handler)
    }
}

expect_implements_syntax!(expect::Character);
expect_implements_syntax!(expect::Identifier);
expect_implements_syntax!(expect::Keyword);
expect_implements_syntax!(expect::Numeric);
expect_implements_syntax!(expect::String);
expect_implements_syntax!(expect::Punctuation);
expect_implements_syntax!(expect::IntoDelimited);

/// Returns [`None`] if the condition of the syntax is not met.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OrNone<S>(pub S);

impl<S: Syntax> Syntax for OrNone<S> {
    type Output = Option<S::Output>;

    type Iter = std::iter::Empty<SyntaxKind>;

    const HAS_CONDITION: bool = false;

    fn step(
        self,
        parser: &mut Parser,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, Error<Self::Iter>> {
        match self.0.step(parser, handler) {
            Ok(output) => Ok(Some(output)),

            // already commited
            Err(Error::Commited(_)) => Err(Error::Commited(super::Error)),

            // not-comitted, just return None
            Err(Error::ConditionNotMet(..)) => Ok(None),
        }
    }
}

/// An or-else combinator for choosing the alternate syntax
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OrElse<P, N> {
    previous: P,
    next: N,
}

impl<P: Syntax, N: Syntax<Output = P::Output>> Syntax for OrElse<P, N> {
    type Output = P::Output;

    type Iter = std::iter::Chain<P::Iter, N::Iter>;

    const HAS_CONDITION: bool = P::HAS_CONDITION || N::HAS_CONDITION;

    #[inline(always)]
    fn step<'p, 'h>(
        self,
        parser: &'p mut Parser,
        handler: &'h dyn Handler<error::Error>,
    ) -> Result<Self::Output, Error<Self::Iter>> {
        let (previous_position, expected_previous) =
            match self.previous.step(parser, handler) {
                Ok(result) => return Ok(result),
                Err(Error::Commited(err)) => return Err(Error::Commited(err)),

                Err(Error::ConditionNotMet(i, iter)) => (i, iter),
            };

        // try again with the next syntax
        let (next_position, expected_next) =
            match self.next.step(parser, handler) {
                Ok(result) => return Ok(result),
                Err(Error::Commited(err)) => return Err(Error::Commited(err)),

                Err(Error::ConditionNotMet(i, iter)) => (i, iter),
            };

        Err(Error::ConditionNotMet(
            previous_position.max(next_position), // make the most progress
            expected_previous.chain(expected_next),
        ))
    }
}

macro_rules! implements_syntax_tuple_element {
    ($t:ident, $found_condition:ident, $iter:ident, $parser:ident, $handler:ident) => {
        match $t.step($parser, $handler) {
            Ok(output) => {
                $found_condition = $t::HAS_CONDITION;

                output
            }
            Err(Error::Commited(err)) => return Err(Error::Commited(err)),
            Err(Error::ConditionNotMet(i, iter)) => {
                if $found_condition {
                    $parser.current_index = i;
                    $handler.receive(error::Error {
                        expected_syntaxes: iter.into_iter().collect(),
                        found: $parser.reading_to_found(
                            $parser.get_reading(
                                $parser
                                    .current_frame
                                    .token_provider
                                    .token_stream()
                                    .get(i),
                            ),
                        ),
                    });

                    // make progress
                    $parser.forward();

                    return Err(Error::Commited(super::Error));
                } else {
                    // first condition
                    return Err(Error::ConditionNotMet(i, $iter::$t(iter)));
                }
            }
        }
    };
}

macro_rules! tuple_implements_syntax {
    ($iter:ident, $t:ident, $($i:ident),*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash,)]
        #[allow(missing_docs)]
        pub enum $iter<$t, $($i),*> {
            $t($t),
            $($i($i)),*
        }

        impl<$t: Iterator<Item = SyntaxKind>, $($i: Iterator<Item = SyntaxKind>),*> Iterator for $iter<$t, $($i),*> {
            type Item = SyntaxKind;

            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    $iter::A(a) => a.next(),
                    $($iter::$i(i) => i.next()),*
                }
            }
        }

        impl<$t: Syntax, $($i: Syntax),*> Syntax for ($t, $($i),*) {
            type Output = (
                $t::Output,
                $($i::Output),*
            );

            type Iter = $iter<$t::Iter, $($i::Iter),*>;

            const HAS_CONDITION: bool = A::HAS_CONDITION $(|| $i::HAS_CONDITION)*;

            #[inline(always)]
            #[allow(non_snake_case, unused_assignments)]
            fn step(
                self,
                parser: &mut Parser,
                handler: &dyn Handler<error::Error>,
            ) -> Result<Self::Output, Error<Self::Iter>> {

                let ($t, $($i),*) = self;
                let mut found_condition = false;

                Ok((
                    implements_syntax_tuple_element!($t, found_condition, $iter, parser, handler),
                    $(implements_syntax_tuple_element!($i, found_condition, $iter, parser, handler)),*
                ))
            }
        }
    };


    ($t:ident => $has_condition:ident) => {


    };
}

tuple_implements_syntax!(T1, A, B);
tuple_implements_syntax!(T2, A, B, C);
tuple_implements_syntax!(T3, A, B, C, D);
tuple_implements_syntax!(T4, A, B, C, D, E);
tuple_implements_syntax!(T5, A, B, C, D, E, F);
tuple_implements_syntax!(T6, A, B, C, D, E, F, G);
tuple_implements_syntax!(T7, A, B, C, D, E, F, G, H);
tuple_implements_syntax!(T8, A, B, C, D, E, F, G, H, I);
tuple_implements_syntax!(T9, A, B, C, D, E, F, G, H, I, J);
tuple_implements_syntax!(T10, A, B, C, D, E, F, G, H, I, J, K);
tuple_implements_syntax!(T11, A, B, C, D, E, F, G, H, I, J, K, L);
tuple_implements_syntax!(T12, A, B, C, D, E, F, G, H, I, J, K, L, M);
tuple_implements_syntax!(T13, A, B, C, D, E, F, G, H, I, J, K, L, M, N);
tuple_implements_syntax!(T14, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O);
tuple_implements_syntax!(T15, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);

/// Maps the output of the syntax to another output.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Map<S, F> {
    syntax: S,
    map: F,
}

impl<S: Syntax, F: FnOnce(S::Output) -> O, O> Syntax for Map<S, F> {
    type Output = O;

    type Iter = S::Iter;

    const HAS_CONDITION: bool = S::HAS_CONDITION;

    #[inline(always)]
    fn step(
        self,
        parser: &mut Parser,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, Error<Self::Iter>> {
        Ok((self.map)(self.syntax.step(parser, handler)?))
    }
}

/// A syntax that repeats the syntax until the condition is not met.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct While<S> {
    syntax: S,
}

impl<S: Syntax + Clone> Syntax for While<S> {
    type Output = Vec<S::Output>;

    type Iter = std::iter::Empty<SyntaxKind>;

    const HAS_CONDITION: bool = false;

    #[inline(always)]
    fn step(
        self,
        parser: &mut Parser,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, Error<Self::Iter>> {
        let mut result = Vec::new();

        loop {
            match self.syntax.clone().step(parser, handler) {
                Ok(output) => result.push(output),

                Err(Error::Commited(err)) => return Err(Error::Commited(err)),

                // should stop
                Err(Error::ConditionNotMet(..)) => break,
            }
        }

        Ok(result)
    }
}

/// Instructs the parser to not skip to nearest significant token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DontSkip<E>(E);

impl<E> Syntax for DontSkip<E>
where
    E: Expect,
{
    type Output = E::Output;

    type Iter = std::iter::Once<SyntaxKind>;

    const HAS_CONDITION: bool = true;

    #[inline(always)]
    fn step(
        self,
        parser: &mut Parser,
        _: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, Error<Self::Iter>> {
        let reading = parser.peek();
        let position = parser.current_index;

        let Ok(result) = self.0.expect(reading) else {
            return Err(Error::ConditionNotMet(
                position,
                std::iter::once(self.0.into()),
            ));
        };

        parser.forward();

        Ok(result)
    }
}

/// Continuing the parsing with the closure after the syntax is parsed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AndThenDo<S, F> {
    syntax: S,
    then: F,
}

impl<
        S: Syntax,
        F: FnOnce(
            &mut Parser,
            S::Output,
            &dyn Handler<error::Error>,
        ) -> Result<O, super::Error>,
        O,
    > Syntax for AndThenDo<S, F>
{
    type Output = O;

    type Iter = S::Iter;

    const HAS_CONDITION: bool = S::HAS_CONDITION;

    fn step(
        self,
        parser: &mut Parser,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, Error<Self::Iter>> {
        const {
            // If this assert error is triggered, make sure that the you
            // don't use `and_then_do` following after the combinators that
            // don't have any conditions (always success) such as `or_none`,
            // `loop_while`, etc.
            assert!(S::HAS_CONDITION, "the syntax must have a condition");
        }

        let result = self.syntax.step(parser, handler)?;

        Ok((self.then)(parser, result, handler)?)
    }
}

/// Conditionally calls the closure if the expection's condition is met.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ThenDo<E, F> {
    expect: E,
    then: F,
}

impl<
        E: Expect,
        F: FnOnce(
            &mut Parser,
            E::Output,
            &dyn Handler<error::Error>,
        ) -> Result<O, super::Error>,
        O,
    > Syntax for ThenDo<E, F>
{
    type Output = O;

    type Iter = std::iter::Once<SyntaxKind>;

    const HAS_CONDITION: bool = true;

    fn step(
        self,
        parser: &mut Parser,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, Error<Self::Iter>> {
        let (reading, position) = parser.peek_significant();

        let Ok(result) = self.expect.expect(reading) else {
            return Err(Error::ConditionNotMet(
                position,
                std::iter::once(self.expect.into()),
            ));
        };

        parser.current_index = position;

        let result = match (self.then)(parser, result, handler) {
            Ok(result) => result,
            Err(err) => {
                // must at least have a progress
                parser.current_index = (position + 1).max(parser.current_index);
                return Err(Error::Commited(err));
            }
        };

        Ok(result)
    }
}

/// Parses the syntax that is delimited by the given delimiter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StepInto<S> {
    delimiter: Delimiter,
    syntax: S,
}

/// Represents a syntax tree enclosed by a pair of delimiter punctuations.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DelimitedTree<S> {
    /// The open delimiter (left delimiter).
    open: Punctuation,

    /// The syntax tree.
    tree: S,

    /// The close delimiter (right delimiter).
    close: Punctuation,
}

impl<S: Syntax> Syntax for StepInto<S> {
    type Output = DelimitedTree<S::Output>;

    type Iter = std::iter::Once<SyntaxKind>;

    const HAS_CONDITION: bool = true;

    fn step(
        self,
        parser: &mut Parser,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output, Error<Self::Iter>> {
        let (open, position) = parser.peek_significant();

        let Ok(_) = self.delimiter.expect(open) else {
            return Err(Error::ConditionNotMet(
                position,
                std::iter::once(match self.delimiter {
                    Delimiter::Parenthesis => SyntaxKind::Punctuation('('),
                    Delimiter::Brace => SyntaxKind::Punctuation('{'),
                    Delimiter::Bracket => SyntaxKind::Punctuation('['),
                }),
            ));
        };

        parser.current_index = position;

        let StepIntoTree { open, tree, close } = parser
            .step_into(
                self.delimiter,
                |parser| parser.parse(self.syntax, handler),
                handler,
            )
            .expect("should be successful");

        Ok(DelimitedTree { open, tree: tree?, close })
    }
}

#[cfg(test)]
mod tests;
