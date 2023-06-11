//! Contains the definition of the [`Input`] trait.

use std::fmt::Debug;

use proptest::test_runner::{TestCaseError, TestCaseResult};

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
