//! This module provides a trait [`Input`] for representing inputs generated for
//! testing purposes

use std::fmt::Debug;

use proptest::{
    prop_assert_eq,
    test_runner::{TestCaseError, TestCaseResult},
};

/// Represents an input generated for testing purposes.
pub trait Input<Output: Debug>: Debug {
    /// Verifies that the given output complies with this input.
    ///
    /// # Errors
    /// [`proptest::test_runner::TestCaseError`]: for any reason the assertion fails.
    fn assert(self, output: Output) -> TestCaseResult;
}

impl<T: Debug, U> Input<&Box<T>> for &Box<U>
where
    for<'a, 'b> &'a U: Input<&'b T>,
    Self: Debug,
{
    fn assert(self, output: &Box<T>) -> TestCaseResult {
        self.as_ref().assert(output.as_ref())
    }
}

impl<T: Debug, U: Debug + Input<T>> Input<Option<T>> for Option<U> {
    fn assert(self, output: Option<T>) -> TestCaseResult {
        match (self, output) {
            (Some(input), Some(output)) => input.assert(output),
            (None, None) => Ok(()),
            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}"
            ))),
        }
    }
}

impl<T: Debug, U: Debug> Input<&[T]> for &[U]
where
    for<'a, 'b> &'a U: Input<&'b T>,
{
    fn assert(self, output: &[T]) -> TestCaseResult {
        prop_assert_eq!(self.len(), output.len());

        for (input, output) in self.iter().zip(output.iter()) {
            input.assert(output)?;
        }

        Ok(())
    }
}

impl<T: Debug, U: Debug, V: Debug, W: Debug> Input<&(T, U)> for &(V, W)
where
    for<'a, 'b> &'a V: Input<&'b T>,
    for<'a, 'b> &'a W: Input<&'b U>,
{
    fn assert(self, output: &(T, U)) -> TestCaseResult {
        self.0.assert(&output.0)?;
        self.1.assert(&output.1)?;

        Ok(())
    }
}
