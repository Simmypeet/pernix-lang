//! A trait for representing inputs generated for testing purposes.
//!
//! This module provides a trait [`Input`] for representing inputs generated for testing purposes
//! (heavily used in lexical and syntactic analysis). An input is a value of some type that is used
//! as an argument to a function or method that is being tested. The `Input` trait provides a method
//! `assert` for verifying that the output of the function or method being tested complies with the
//! input.

use std::fmt::Debug;

use proptest::{
    prop_assert_eq,
    test_runner::{TestCaseError, TestCaseResult},
};

/// Represents an input generated for testing purposes.
pub trait Input {
    /// The output type of the input that wlll be validated against.
    type Output;

    /// Verifies that the given [`Self::Output`] complies with this input.
    ///
    /// # Errors
    /// [`proptest::test_runner::TestCaseError`]: for any reason the assertion fails.
    fn assert(&self, output: &Self::Output) -> TestCaseResult;
}

impl<T: Input + Debug> Input for Option<T>
where
    T::Output: Debug,
{
    type Output = Option<T::Output>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Some(i), Some(o)) => i.assert(o),
            (None, None) => Ok(()),
            (i, o) => Err(TestCaseError::fail(
                format!("Expected {i:?} but got {o:?}",),
            )),
        }
    }
}

impl<T: Input> Input for Box<T> {
    type Output = Box<T::Output>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult { (**self).assert(&**output) }
}

impl<T: Input> Input for Vec<T> {
    type Output = Vec<T::Output>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.len(), output.len());

        for (i, o) in self.iter().zip(output.iter()) {
            i.assert(o)?;
        }

        Ok(())
    }
}
