//! Contains the [`Parse`] trait and various implementations for parser
//! combinators.

use std::{borrow, fmt::Debug};

use pernixc_base::handler::Handler;
use pernixc_lexical::{
    token::{KeywordKind, Punctuation},
    token_stream::{Delimiter, Location, NodeKind, TokenKind},
};

use super::StateMachine;
use crate::{
    error,
    expect::{self, Expect, Expected},
};

/// A shorthand for the [`Result`] type that is used by the parser.
pub type Result<'a, T, I> = std::result::Result<T, Unexpected<'a, I>>;

/// An extension trait for the [`Expect`] trait allowing for more parser
/// combinators.
pub trait ExpectExt: Expect {
    /// Creates a parser that does not skip insignificant tokens.
    fn no_skip(self) -> NoSkip<Self>
    where
        Self: Sized,
    {
        NoSkip(self)
    }
}

impl<T: Expect> ExpectExt for T {}

/// Created by the [`ExpectExt::no_skip`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NoSkip<E>(pub E);

impl<E: Expect> Parse for NoSkip<E>
where
    E::Output: 'static,
{
    type Output<'a> = &'a E::Output;

    type ExpectedIter = std::iter::Once<Expected>;

    fn parse<'a>(
        self,
        state_machine: &mut StateMachine<'a>,
        _: &dyn Handler<error::Error>,
    ) -> Result<'a, Self::Output<'a>, Self::ExpectedIter> {
        if let Some(token) = state_machine.next_no_skip() {
            match self.0.expect(token) {
                Ok(token) => Ok(token),
                Err(token) => Err(Unexpected {
                    found: Some((
                        token,
                        state_machine.current_token_index() - 1,
                    )),
                    node_index: state_machine.current_node_index(),
                    expected: std::iter::once(self.0.into()),
                }),
            }
        } else {
            return Err(Unexpected {
                expected: std::iter::once(self.0.into()),
                found: None,
                node_index: state_machine.current_node_index(),
            });
        }
    }
}

/// A combination trait of [`Iterator`] that iterates over [`Expected`] tokens
/// and [`Debug`].
///
/// This allows [`Result`] to be unwrapped and the expected tokens to be printed
/// in the error message.
pub trait ExpectedIterator: Iterator<Item = Expected> + Debug {}

impl<T: Iterator<Item = Expected> + Debug> ExpectedIterator for T {}

/// An unexpected token was found.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unexpected<'a, I> {
    /// The token that was found unexpectedly. If `None`, then the end of the
    /// token stream was reached. The token is a tuple of the token kind and
    /// the index of the token in the token stream.
    pub found: Option<(&'a TokenKind, usize /* token index */)>,

    /// The location of the unexpected token.
    pub node_index: usize,

    /// The iterator of [`Expected`] tokens that were expected.
    pub expected: I,
}

/// A trait for parsing the syntax tree.
pub trait Parse {
    /// The syntax tree output of the parser.
    type Output<'a>;

    /// The iterator of expected tokens.
    type ExpectedIter: ExpectedIterator;

    /// Parses the syntax tree.
    fn parse<'a>(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error<'a>>,
    ) -> Result<'a, Self::Output<'a>, Self::ExpectedIter>;

    /// Tries an alternative parser if the current parser fails.
    fn or_else<N: for<'a> Parse<Output<'a> = Self::Output<'a>>>(
        self,
        next: N,
    ) -> OrElse<Self, N>
    where
        Self: Sized,
    {
        OrElse { previous: self, next }
    }

    /// Maps the output of the parser to another output.
    fn map<O, F: for<'a> FnOnce(Self::Output<'a>) -> O>(
        self,
        map: F,
    ) -> Map<Self, F>
    where
        Self: Sized,
    {
        Map { parser: self, map }
    }

    /// Converts the output of the parser to an owned type.
    fn to_owned<O: borrow::ToOwned + 'static>(self) -> ToOwned<Self>
    where
        Self: Sized,
        Self: for<'a> Parse<Output<'a> = &'a O>,
    {
        ToOwned(self)
    }

    /// Returns `None` if the parser fails to parse and has not consumed any
    /// sucessful tokens.
    ///
    /// If the parser has consumed any sucessful tokens and then fails, then
    /// the error is returned instead of `None`.
    fn or_none(self) -> OrNone<Self>
    where
        Self: Sized,
    {
        OrNone(self)
    }

    /// Keeps parsing the parser while the **first** token matches the parser.
    ///
    /// The parser will keep parsing the token until the **first** token does
    /// not match the parser.
    fn keep_take(self) -> KeepTake<Self>
    where
        Self: Sized + Clone,
    {
        KeepTake(self)
    }
}

macro_rules! expect_implements_parse {
    ($t:ty) => {
        impl Parse for $t {
            type Output<'a> = &'a <$t as Expect>::Output;
            type ExpectedIter = std::iter::Once<Expected>;

            fn parse<'a>(
                self,
                state_machine: &mut StateMachine<'a>,
                _: &dyn Handler<error::Error>,
            ) -> Result<'a, Self::Output<'a>, Self::ExpectedIter> {
                if let Some((token, tok_index)) = state_machine.next() {
                    match self.expect(token) {
                        Ok(token) => Ok(token),
                        Err(token) => Err(Unexpected {
                            found: Some((token, tok_index)),
                            node_index: state_machine.current_node_index(),
                            expected: std::iter::once(self.into()),
                        }),
                    }
                } else {
                    return Err(Unexpected {
                        expected: std::iter::once(self.into()),
                        found: None,
                        node_index: state_machine.current_node_index(),
                    });
                }
            }
        }
    };
}

expect_implements_parse!(expect::Identifier);
expect_implements_parse!(expect::Numeric);
expect_implements_parse!(expect::String);
expect_implements_parse!(expect::Character);
expect_implements_parse!(KeywordKind);
expect_implements_parse!(char);
expect_implements_parse!(Delimiter);

/// Created by the [`Parse::or_else`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OrElse<P, N> {
    previous: P,
    next: N,
}

/// An iterator of [`Expected`] tokens that were expected by the [`OrElse`]
/// parser.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum OrElseExpectedIter<P, N> {
    Previous(P),
    Next(N),
    Both(std::iter::Chain<P, N>),
}

impl<P: Iterator<Item = Expected>, N: Iterator<Item = Expected>> Iterator
    for OrElseExpectedIter<P, N>
{
    type Item = Expected;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Previous(iter) => iter.next(),
            Self::Next(iter) => iter.next(),
            Self::Both(iter) => iter.next(),
        }
    }
}

impl<P: Parse, N: for<'a> Parse<Output<'a> = P::Output<'a>>> Parse
    for OrElse<P, N>
{
    type Output<'a> = P::Output<'a>;

    type ExpectedIter = OrElseExpectedIter<P::ExpectedIter, N::ExpectedIter>;

    fn parse<'a>(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error<'a>>,
    ) -> Result<'a, Self::Output<'a>, Self::ExpectedIter> {
        let mut next_state_machine = *state_machine;

        let previous_error = match self.previous.parse(state_machine, handler) {
            // return the output now
            Ok(output) => return Ok(output),
            Err(err) => err,
        };

        let next_error = match self.next.parse(&mut next_state_machine, handler)
        {
            // return the output now
            Ok(output) => return Ok(output),
            Err(err) => err,
        };

        match next_state_machine.eaten_tokens.cmp(&state_machine.eaten_tokens) {
            // current state_machine make more progress
            std::cmp::Ordering::Less => {
                return Err(Unexpected {
                    found: previous_error.found,
                    node_index: previous_error.node_index,
                    expected: OrElseExpectedIter::Previous(
                        previous_error.expected,
                    ),
                });
            }

            // next_state_machine make more progress
            std::cmp::Ordering::Greater => {
                *state_machine = next_state_machine;
                return Err(Unexpected {
                    found: next_error.found,
                    node_index: next_error.node_index,
                    expected: OrElseExpectedIter::Next(next_error.expected),
                });
            }

            // combination of both errors
            std::cmp::Ordering::Equal => {
                return Err(Unexpected {
                    found: previous_error.found,
                    node_index: previous_error.node_index,
                    expected: OrElseExpectedIter::Both(
                        previous_error.expected.chain(next_error.expected),
                    ),
                });
            }
        }
    }
}

impl<
        I: ExpectedIterator + 'static,
        O,
        F: for<'a> FnOnce(
            &mut StateMachine<'a>,
            &dyn Handler<error::Error>,
        ) -> Result<'a, O, I>,
    > Parse for F
{
    type Output<'a> = O;

    type ExpectedIter = Box<dyn ExpectedIterator>;

    fn parse<'a>(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<'a, Self::Output<'a>, Self::ExpectedIter> {
        self(state_machine, handler).map_err(|err| Unexpected {
            found: err.found,
            node_index: err.node_index,
            expected: Box::new(err.expected) as _,
        })
    }
}

/// Created by the [`Parse::to_owned`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ToOwned<T>(T);

impl<O: borrow::ToOwned + 'static, T: for<'a> Parse<Output<'a> = &'a O>> Parse
    for ToOwned<T>
{
    type Output<'a> = O::Owned;

    type ExpectedIter = T::ExpectedIter;

    fn parse<'a>(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error<'a>>,
    ) -> Result<'a, Self::Output<'a>, Self::ExpectedIter> {
        self.0.parse(state_machine, handler).map(|x| x.to_owned())
    }
}

/// Created by the [`Parse::map`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Map<P, F> {
    parser: P,
    map: F,
}

impl<P: Parse, F: for<'a> FnOnce(P::Output<'a>) -> O, O> Parse for Map<P, F> {
    type Output<'a> = O;

    type ExpectedIter = P::ExpectedIter;

    fn parse<'a>(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error<'a>>,
    ) -> Result<'a, Self::Output<'a>, Self::ExpectedIter> {
        let output = self.parser.parse(state_machine, handler)?;
        Ok((self.map)(output))
    }
}

macro_rules! tuple_implements_syntax {
    ($iter:ident, $t:ident, $($i:ident),*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[allow(missing_docs)]
        pub enum $iter<$t, $($i),*> {
            $t($t),
            $(
                $i($i),
            )*
        }

        impl<$t: Iterator<Item = Expected>, $($i: Iterator<Item = Expected>),*> Iterator
            for $iter<$t, $($i),*>
        {
            type Item = Expected;

            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::$t(iter) => iter.next(),
                    $(
                        Self::$i(iter) => iter.next(),
                    )*
                }
            }
        }

        impl<$t: Parse, $($i: Parse),*> Parse for ($t, $($i),*) {
            type Output<'a> = ($t::Output<'a>, $($i::Output<'a>),*);
            type ExpectedIter = $iter<$t::ExpectedIter, $($i::ExpectedIter),*>;

            #[allow(non_snake_case)]
            fn parse<'a>(
                self,
                state_machine: &mut StateMachine<'a>,
                handler: &dyn Handler<error::Error<'a>>,
            ) -> Result<'a, Self::Output<'a>, Self::ExpectedIter> {
                let ($t, $($i),*) = self;

                Ok((
                    match $t.parse(state_machine, handler) {
                        Ok(output) => output,
                        Err(err) => return Err(Unexpected {
                            found: err.found,
                            node_index: err.node_index,
                            expected: $iter::$t(err.expected),
                        }),
                    },
                    $(
                        match $i.parse(state_machine, handler) {
                            Ok(output) => output,
                            Err(err) => return Err(Unexpected {
                                found: err.found,
                                node_index: err.node_index,
                                expected: $iter::$i(err.expected),
                            }),
                        }
                    ),*
                ))
            }
        }
    };
}

tuple_implements_syntax!(TupleExpectedIter2, A, B);
tuple_implements_syntax!(TupleExpectedIter3, A, B, C);
tuple_implements_syntax!(TupleExpectedIter4, A, B, C, D);
tuple_implements_syntax!(TupleExpectedIter5, A, B, C, D, E);
tuple_implements_syntax!(TupleExpectedIter6, A, B, C, D, E, F);
tuple_implements_syntax!(TupleExpectedIter7, A, B, C, D, E, F, G);
tuple_implements_syntax!(TupleExpectedIter8, A, B, C, D, E, F, G, H);
tuple_implements_syntax!(TupleExpectedIter9, A, B, C, D, E, F, G, H, I);
tuple_implements_syntax!(TupleExpectedIter10, A, B, C, D, E, F, G, H, I, J);

/// Created by the [`Parse::or_none`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OrNone<T>(T);

impl<T: Parse> Parse for OrNone<T> {
    type Output<'a> = Option<T::Output<'a>>;
    type ExpectedIter = T::ExpectedIter;

    fn parse<'a>(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error<'a>>,
    ) -> Result<'a, Self::Output<'a>, Self::ExpectedIter> {
        let current_location = state_machine.location;
        let current_eaten = state_machine.eaten_tokens;

        match self.0.parse(state_machine, handler) {
            Ok(output) => Ok(Some(output)),
            Err(err) => {
                // if have eaten more than one token, then return the error
                // this mean that the parser has committed to the current path
                if state_machine.eaten_tokens > current_eaten + 1 {
                    return Err(err);
                }

                state_machine.location = current_location;
                state_machine.eaten_tokens = current_eaten;

                Ok(None)
            }
        }
    }
}

/// Created by the [`Parse::keep_take`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KeepTake<T>(T);

impl<T: Parse + Clone> Parse for KeepTake<T> {
    type Output<'a> = Vec<T::Output<'a>>;
    type ExpectedIter = T::ExpectedIter;

    fn parse<'a>(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error<'a>>,
    ) -> Result<'a, Self::Output<'a>, Self::ExpectedIter> {
        let mut output = Vec::new();

        while let Some(value) =
            self.0.clone().or_none().parse(state_machine, handler)?
        {
            output.push(value);
        }

        Ok(output)
    }
}

pub struct StepInto<P> {
    parser: P,
    delimiter: Delimiter,
}

#[derive(Debug, Clone)]
pub enum StepIntoExpectedIter<P> {
    MismatchedStepInto(std::iter::Once<Expected>),
    Tree(P),
}

impl<P: Iterator<Item = Expected>> Iterator for StepIntoExpectedIter<P> {
    type Item = Expected;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            StepIntoExpectedIter::MismatchedStepInto(once) => once.next(),
            StepIntoExpectedIter::Tree(p) => p.next(),
        }
    }
}

impl<P: Parse> Parse for StepInto<P> {
    type Output<'a> = (&'a Punctuation, P::Output<'a>, &'a Punctuation);
    type ExpectedIter = StepIntoExpectedIter<P::ExpectedIter>;

    fn parse<'a>(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error<'a>>,
    ) -> Result<'a, Self::Output<'a>, Self::ExpectedIter> {
        let result = state_machine.next_step_into(
            |inner_state_machine,
             location|
             -> Result<'a, Self::Output<'a>, Self::ExpectedIter> {
                match inner_state_machine.current_node().kind {
                    NodeKind::Delimited { delimited, .. } => {
                        if delimited.delimiter != self.delimiter {
                            return Err(Unexpected {
                                found: inner_state_machine
                                    .tree
                                    .get_token(&location)
                                    .map(|token| (token, location.token_index)),
                                node_index: location.node_index,
                                expected:
                                    StepIntoExpectedIter::MismatchedStepInto(
                                        std::iter::once(self.delimiter.into()),
                                    ),
                            });
                        }

                        let result = match self
                            .parser
                            .parse(inner_state_machine, handler)
                        {
                            Ok(a) => a,
                            Err(err) => {
                                return Err(Unexpected {
                                    found: err.found,
                                    node_index: err.node_index,
                                    expected: StepIntoExpectedIter::Tree(
                                        err.expected,
                                    ),
                                })
                            }
                        };

                        // should be none, no more tokens to parse
                        if let Some(found) = inner_state_machine.next() {
                            // this is a soft error, the tree is still valid
                            handler.receive(error::Error {
                                found: Some(found),
                                node_index: location.node_index,
                                expected: vec![match self.delimiter {
                                    Delimiter::Parenthesis => ')',
                                    Delimiter::Brace => '}',
                                    Delimiter::Bracket => ']',
                                }
                                .into()],
                            });
                        }

                        let (open, close) = {
                            let delimited = inner_state_machine
                                .tree
                                .get_node(location.node_index)
                                .as_delimited()
                                .unwrap()
                                .0;

                            (&delimited.open, &delimited.close)
                        };

                        Ok((open, result, close))
                    }

                    NodeKind::Root(_) => unreachable!(),
                }
            },
        );

        if let Some(result) = result {
            result
        } else {
            Err(Unexpected {
                found: state_machine
                    .tree
                    .get_token(&Location::new(
                        state_machine.current_node_index(),
                        state_machine.current_token_index() - 1,
                    ))
                    .map(|x| (x, state_machine.current_token_index() - 1)),
                node_index: state_machine.current_node_index(),
                expected: StepIntoExpectedIter::MismatchedStepInto(
                    std::iter::once(self.delimiter.into()),
                ),
            })
        }
    }
}

#[cfg(test)]
mod tests;
