//! Contains the [`Parse`] trait and various implementations for parser
//! combinators.

use std::{borrow, cmp::Ordering, fmt::Debug, vec::Drain};

use paste::paste;
use pernixc_base::handler::Handler;
use pernixc_lexical::{
    token::{KeywordKind, Punctuation},
    token_stream::{Delimiter, NodeKind, Tree},
};

use super::{StateMachine, StepIntoError};
use crate::{
    error,
    expect::{self, Expect, Expected},
};

/// A shorthand for the [`Result`] type that is used by the parser.
pub type Result<T> = std::result::Result<T, Unexpected>;

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

impl<'a, E: Expect> Parse<'a> for NoSkip<E>
where
    E::Output: 'static,
{
    type Output = &'a E::Output;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        _: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        if let Some(token) = state_machine.next_no_skip() {
            self.0.expect(token).ok_or_else(|| {
                state_machine.expected.push(self.0.into());

                Unexpected {
                    token_index: Some(state_machine.current_token_index() - 1),
                    node_index: state_machine.current_node_index(),
                }
            })
        } else {
            state_machine.expected.push(self.0.into());

            Err(Unexpected {
                token_index: None,
                node_index: state_machine.current_node_index(),
            })
        }
    }

    fn commit_count(&self) -> usize { 1 }
}

/// An unexpected token was found.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unexpected {
    /// The index of the token in the token tree where the unexpected token was
    /// found. If `None`, then the end of the token stream was reached.
    pub token_index: Option<usize>,

    /// The node index in the token tree where the unexpected token was found.
    pub node_index: usize,
}

/// A trait for parsing the syntax tree.
pub trait Parse<'a> {
    /// The syntax tree output of the parser.
    type Output;

    /// Parses the syntax tree. This is meant to be called by the implementation
    /// of the parser.
    ///
    /// # Errors
    ///
    /// Returns an [`Unexpected`] error if the parser fails to parse the syntax
    /// tree.
    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output>;

    /// The number of eaten tokens that the parser will commit to the current
    /// path. The default value is `1`.
    fn commit_count(&self) -> usize;

    /// Changes the number of commiting tokens, which is most likely 1 for
    /// most parsers.
    fn commit_in(self, count: usize) -> CommitIn<Self>
    where
        Self: Sized,
    {
        assert!(count > 0, "commit count must be greater than 0");

        CommitIn { parser: self, count }
    }

    /// Parses the syntax tree and returns the output if the parser is
    /// successful.
    fn parse_syntax(
        self,
        tree: &'a Tree<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Self::Output>
    where
        Self: Sized,
    {
        let mut state_machine = StateMachine::new(tree);

        match self.parse(&mut state_machine, handler) {
            Ok(output) => {
                // if you see this error, then some of the parser errors were
                // discarded or not properly handled
                assert!(state_machine.expected.is_empty(), "should be empty");

                Some(output)
            }
            Err(unexpected) => {
                let expected = state_machine.take_expected();
                handler.receive(
                    error::Error::new(tree, unexpected, expected).unwrap(),
                );
                None
            }
        }
    }

    /// Maps the output of the parser to another output.
    fn map<O, F: FnOnce(Self::Output) -> O>(self, map: F) -> Map<Self, F>
    where
        Self: Sized,
    {
        Map { parser: self, map }
    }

    /// Converts the output of the parser to an owned type.
    fn to_owned<O: borrow::ToOwned + 'static>(self) -> ToOwned<Self>
    where
        Self: Sized + Parse<'a, Output = &'a O>,
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
        Self: Sized,
    {
        KeepTake(self)
    }

    /// Parses all the token in the current token stream and returns a list
    /// of the parsed tokens. The parser will recover from errors and continue
    /// parsing after the error. The error will be reported to the handler.
    fn keep_take_all(self) -> KeepTakeAll<Self>
    where
        Self: Sized,
    {
        KeepTakeAll { element_parser: self }
    }

    /// Steps into a delimited token stream and parses the inner tokens.
    ///
    /// The parser expected to consume all the tokens inside the delimiter.
    fn step_into(self, delimiter: Delimiter) -> StepInto<Self>
    where
        Self: Sized,
    {
        StepInto { parser: self, delimiter }
    }

    /// Keeps parsing the parser while the **first** token matches the parser
    /// and folds the output.
    fn keep_fold<R, F>(self, rest: R, fold: F) -> KeepFold<Self, R, F>
    where
        Self: Sized,
    {
        KeepFold { initial: self, rest, fold }
    }

    /// Parses the syntax tree and reports an error if the parser fails.
    fn parse_and_report(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Self::Output>
    where
        Self: Sized,
    {
        let tree = state_machine.tree;
        match self.parse_and_drain(state_machine, handler) {
            Ok(result) => Some(result),
            Err((unexpected, expected)) => {
                handler.receive(
                    error::Error::new(
                        tree,
                        unexpected,
                        expected.collect::<Vec<_>>(),
                    )
                    .unwrap(),
                );
                None
            }
        }
    }

    /// Parses the given parser and drains the expected tokens if failed.
    ///
    /// # Errors
    ///
    /// Returns the [`Unexpected`] and the iterator of the expected tokens at
    /// the time of the error.
    fn parse_and_drain<'s>(
        self,
        state_machine: &'s mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> std::result::Result<Self::Output, (Unexpected, Drain<'s, Expected>)>
    where
        Self: Sized,
    {
        let current_expected_len = state_machine.expected_len();

        let result = self.parse(state_machine, handler);

        match result {
            Ok(result) => {
                assert_eq!(
                    state_machine.expected_len(),
                    current_expected_len,
                    "expected tokens were not consumed"
                );

                Ok(result)
            }
            Err(unexpected) => Err((
                unexpected,
                state_machine.expected.drain(current_expected_len..),
            )),
        }
    }
}

macro_rules! expect_implements_parse {
    ($t:ty) => {
        impl<'a> Parse<'a> for $t {
            type Output = &'a <$t as Expect>::Output;

            fn parse(
                self,
                state_machine: &mut StateMachine<'a>,
                _: &dyn Handler<error::Error>,
            ) -> Result<Self::Output> {
                if let Some((token, tok_index)) = state_machine.next() {
                    self.expect(token).ok_or_else(|| {
                        state_machine.expected.push(self.into());
                        Unexpected {
                            token_index: Some(tok_index),
                            node_index: state_machine.current_node_index(),
                        }
                    })
                } else {
                    state_machine.expected.push(self.into());

                    return Err(Unexpected {
                        token_index: None,
                        node_index: state_machine.current_node_index(),
                    });
                }
            }

            fn commit_count(&self) -> usize { 1 }
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

impl<
        'a,
        O,
        F: FnOnce(&mut StateMachine<'a>, &dyn Handler<error::Error>) -> Result<O>,
    > Parse<'a> for F
{
    type Output = O;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        self(state_machine, handler)
    }

    fn commit_count(&self) -> usize { 1 }
}

/// Created by the [`Parse::to_owned`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ToOwned<T>(T);

impl<'a, O: std::borrow::ToOwned + 'a, T: Parse<'a, Output = &'a O>> Parse<'a>
    for ToOwned<T>
{
    type Output = O::Owned;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        self.0.parse(state_machine, handler).map(std::borrow::ToOwned::to_owned)
    }

    fn commit_count(&self) -> usize { self.0.commit_count() }
}

/// Created by the [`Parse::map`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Map<P, F> {
    parser: P,
    map: F,
}

impl<'a, P: Parse<'a>, F: FnOnce(P::Output) -> O, O> Parse<'a> for Map<P, F> {
    type Output = O;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        Ok((self.map)(self.parser.parse(state_machine, handler)?))
    }

    fn commit_count(&self) -> usize { self.parser.commit_count() }
}

macro_rules! tuple_implements_syntax {
    ($t:ident, $($i:ident),*) => {
        impl<'a, $t: Parse<'a>, $($i: Parse<'a>),*> Parse<'a> for ($t, $($i),*) {
            type Output = ($t::Output, $($i::Output),*);

            #[allow(non_snake_case)]
            fn parse(
                self,
                state_machine: &mut StateMachine<'a>,
                handler: &dyn Handler<error::Error>,
            ) -> Result<Self::Output> {
                let ($t, $($i),*) = self;

                Ok((
                    $t.parse(state_machine, handler)?,
                    $(
                        $i.parse(state_machine, handler)?
                    ),*
                ))
            }

            // the first one is the committer
            #[allow(non_snake_case)]
            fn commit_count(&self) -> usize {
                self.0.commit_count()
            }
        }
    };
}

tuple_implements_syntax!(A, B);
tuple_implements_syntax!(A, B, C);
tuple_implements_syntax!(A, B, C, D);
tuple_implements_syntax!(A, B, C, D, E);
tuple_implements_syntax!(A, B, C, D, E, F);
tuple_implements_syntax!(A, B, C, D, E, F, G);
tuple_implements_syntax!(A, B, C, D, E, F, G, H);
tuple_implements_syntax!(A, B, C, D, E, F, G, H, I);
tuple_implements_syntax!(A, B, C, D, E, F, G, H, I, J);

/// Created by the [`Parse::or_none`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OrNone<T>(T);

impl<'a, T: Parse<'a>> Parse<'a> for OrNone<T> {
    type Output = Option<T::Output>;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        let current_location = state_machine.location;
        let current_eaten = state_machine.eaten_tokens;
        let expected_len = state_machine.expected.len();

        let commit_count = self.0.commit_count();

        match self.0.parse(state_machine, handler) {
            Ok(output) => Ok(Some(output)),
            Err(err) => {
                // if the parser has consumed token more than the commit count,
                // then return the error
                if state_machine.eaten_tokens > current_eaten + commit_count {
                    return Err(err);
                }

                state_machine.location = current_location;
                state_machine.eaten_tokens = current_eaten;
                state_machine.expected.truncate(expected_len);

                Ok(None)
            }
        }
    }

    fn commit_count(&self) -> usize { self.0.commit_count() }
}

/// Created by the [`Parse::keep_take`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KeepTake<T>(T);

impl<'a, T: Parse<'a> + Clone> Parse<'a> for KeepTake<T> {
    type Output = Vec<T::Output>;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        let mut output = Vec::new();

        while let Some(value) =
            self.0.clone().or_none().parse(state_machine, handler)?
        {
            output.push(value);
        }

        Ok(output)
    }

    fn commit_count(&self) -> usize { self.0.commit_count() }
}

/// Created by the [`Parse::step_into`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StepInto<P> {
    parser: P,
    delimiter: Delimiter,
}

impl<'a, P: Parse<'a>> Parse<'a> for StepInto<P> {
    type Output = (&'a Punctuation, P::Output, &'a Punctuation);

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        let result = state_machine.next_step_into(
            |state_machine, location| -> Result<Self::Output> {
                match state_machine.current_node().kind {
                    NodeKind::Delimited { delimited, .. } => {
                        if delimited.delimiter != self.delimiter {
                            state_machine.expected.push(self.delimiter.into());
                            return Err(Unexpected {
                                token_index: Some(location.token_index),
                                node_index: location.node_index,
                            });
                        }

                        let tree = self.parser.parse(state_machine, handler)?;

                        // should be none, no more tokens to parse
                        if let Some((_, index)) = state_machine.next() {
                            // this is a soft error, the tree is still valid

                            handler.receive(
                                error::Error::new(
                                    state_machine.tree,
                                    Unexpected {
                                        token_index: Some(index),
                                        node_index: state_machine
                                            .current_node_index(),
                                    },
                                    vec![match self.delimiter {
                                        Delimiter::Parenthesis => ')',
                                        Delimiter::Brace => '}',
                                        Delimiter::Bracket => ']',
                                    }
                                    .into()],
                                )
                                .unwrap(),
                            );
                        }

                        let (open, close) = {
                            let delimited = state_machine
                                .tree
                                .get_node(state_machine.current_node_index())
                                .unwrap()
                                .as_delimited()
                                .unwrap()
                                .0;

                            (&delimited.open, &delimited.close)
                        };

                        Ok((open, tree, close))
                    }

                    NodeKind::Root(_) => unreachable!(),
                }
            },
        );

        match result {
            Ok(result) => result,
            Err(error) => {
                state_machine.expected.push(self.delimiter.into());

                match error {
                    StepIntoError::EndOfStream => Err(Unexpected {
                        token_index: None,
                        node_index: state_machine.current_node_index(),
                    }),
                    StepIntoError::NotDelimited => Err(Unexpected {
                        token_index: Some(
                            state_machine.current_token_index() - 1,
                        ),
                        node_index: state_machine.current_node_index(),
                    }),
                }
            }
        }
    }

    // commit on the delimiter
    fn commit_count(&self) -> usize { 1 }
}

/// Created by the [`Parse::keep_fold`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KeepFold<I, R, F> {
    initial: I,
    rest: R,
    fold: F,
}

impl<
        'a,
        I: Parse<'a>,
        R: Parse<'a> + Clone,
        F: FnMut(I::Output, R::Output) -> I::Output,
    > Parse<'a> for KeepFold<I, R, F>
{
    type Output = I::Output;

    fn parse(
        mut self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        let initial = self.initial.parse(state_machine, handler)?;

        let mut output = initial;

        loop {
            let current =
                self.rest.clone().or_none().parse(state_machine, handler)?;

            if let Some(current) = current {
                output = (self.fold)(output, current);
            } else {
                break;
            }
        }

        Ok(output)
    }

    fn commit_count(&self) -> usize { self.initial.commit_count() }
}

/// Created by the [`Parse::keep_take_all`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KeepTakeAll<T> {
    element_parser: T,
}

impl<'a, T: Parse<'a> + Clone> Parse<'a> for KeepTakeAll<T> {
    type Output = Vec<T::Output>;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        let mut list = Vec::new();
        let mut recovering = false;
        let mut latest_expected_len = state_machine.expected_len();

        while state_machine.peek().is_some() {
            match self.element_parser.clone().parse(state_machine, handler) {
                Ok(element) => {
                    list.push(element);
                    recovering = false;
                }

                Err(err) => {
                    let this_expecteds =
                        state_machine.expected.drain(latest_expected_len..);

                    // not in recovery mode, report the error
                    if !recovering {
                        let this_expecteds = this_expecteds.collect::<Vec<_>>();

                        handler.receive(
                            error::Error::new(
                                state_machine.tree,
                                err,
                                this_expecteds,
                            )
                            .unwrap(),
                        );
                    }

                    recovering = true;
                }
            }

            latest_expected_len = state_machine.expected_len();
        }

        Ok(list)
    }

    fn commit_count(&self) -> usize { self.element_parser.commit_count() }
}

/// Created by the [`Branch::branch`] method.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleBranch<T>(T);

/// A trait for branching parsers.
pub trait Branch {
    /// Creates a branch parser that will try to parse multiple choices of
    /// parsers and return the output of the first successful parser.
    fn branch(self) -> TupleBranch<Self>
    where
        Self: Sized,
    {
        TupleBranch(self)
    }
}

macro_rules! implements_tuple_branch {
    ($t:ident, $($i:ident),*) => {
        paste!{
            impl<'a, $t: Parse<'a>, $($i: Parse<'a, Output = $t::Output>),*>
                Branch for ($t, $($i),*) {}

            impl<'a, $t: Parse<'a>, $($i: Parse<'a, Output = $t::Output>),*>
                Parse<'a> for TupleBranch<($t, $($i),*)> {
                type Output = $t::Output;

                #[allow(non_snake_case, unused_assignments)]
                fn parse(
                    self,
                    state_machine: &mut StateMachine<'a>,
                    handler: &dyn Handler<error::Error>,
                ) -> Result<Self::Output> {
                    let starting_location = state_machine.location;
                    let starting_eaten = state_machine.eaten_tokens;
                    let starting_expected_len = state_machine.expected.len();

                    let ($t, $($i),*) = self.0;

                    let current_len = state_machine.expected.len();
                    let mut max_err =
                        match $t.parse(state_machine, handler) {
                            // return the output now
                            Ok(output) => return Ok(output),
                            Err(err) => err,
                        };


                    let mut current_expected_range =
                        current_len..state_machine.expected.len();

                    let mut max_eaten = state_machine.eaten_tokens;
                    let mut max_location = state_machine.location;

                    $(
                        // reset the state machine to the previous state
                        state_machine.location = starting_location;
                        state_machine.eaten_tokens = starting_eaten;

                        let next_err = match $i.parse(state_machine, handler) {
                            // return the output now
                            Ok(output) => {
                                state_machine
                                    .expected
                                    .truncate(starting_expected_len);

                                return Ok(output);
                            }
                            Err(err) => err,
                        };

                        match state_machine.eaten_tokens.cmp(&max_eaten) {
                            Ordering::Equal => {
                                max_err = next_err;
                                max_location = state_machine.location;
                                max_eaten = state_machine.eaten_tokens;

                                current_expected_range =
                                    current_expected_range.start..
                                        state_machine.expected.len();
                            }

                            Ordering::Less => {
                                // remove new expected token that was added by
                                // the next parser
                                state_machine
                                    .expected
                                    .truncate(current_expected_range.end);
                            }

                            Ordering::Greater => {
                                // remove the expected tokens that were added by
                                // the previous parser
                                state_machine
                                    .expected
                                    .drain(current_expected_range.clone());

                                max_err = next_err;
                                max_location = state_machine.location;
                                max_eaten = state_machine.eaten_tokens;

                                current_expected_range =
                                    current_expected_range.start..
                                        state_machine.expected.len();
                            }
                        }
                    )*

                    state_machine.location = max_location;
                    state_machine.eaten_tokens = max_eaten;

                    return Err(max_err);
                }

                #[allow(non_snake_case)]
                fn commit_count(&self) -> usize {
                    let ($t, $($i),*) = &self.0;

                    let mut count = $t.commit_count();

                    $(
                        count = count.max($i.commit_count());
                    )*

                    count
                }
            }
        }
    };
}

/// Created by the [`Parse::commit_in`] method.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CommitIn<T> {
    parser: T,
    count: usize,
}

impl<'a, T: Parse<'a> + Clone> Parse<'a> for CommitIn<T> {
    type Output = T::Output;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        self.parser.parse(state_machine, handler)
    }

    fn commit_count(&self) -> usize { self.count }
}

implements_tuple_branch!(A, B);
implements_tuple_branch!(A, B, C);
implements_tuple_branch!(A, B, C, D);
implements_tuple_branch!(A, B, C, D, E);
implements_tuple_branch!(A, B, C, D, E, F);
implements_tuple_branch!(A, B, C, D, E, F, G);
implements_tuple_branch!(A, B, C, D, E, F, G, H);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J, K);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J, K, L);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J, K, L, M);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J, K, L, M, N);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);

#[cfg(test)]
mod tests;
