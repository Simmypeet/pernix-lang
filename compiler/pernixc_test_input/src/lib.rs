//! This module provides a trait [`Input`] for representing inputs generated for
//! testing purposes

use std::fmt::Debug;

use proptest::{
    prop_assert_eq,
    test_runner::{TestCaseError, TestCaseResult},
};

/// Represents an input generated for testing purposes.
pub trait Input<Output: Debug, Parameters>: Debug {
    /// Verifies that the given output complies with this input.
    ///
    /// # Errors
    /// [`proptest::test_runner::TestCaseError`]: for any reason the assertion fails.
    fn assert(self, output: Output, parameters: Parameters) -> TestCaseResult;
}

impl<T: Debug, U, P> Input<&Box<T>, P> for &Box<U>
where
    for<'a, 'b> &'a U: Input<&'b T, P>,
    Self: Debug,
{
    fn assert(self, output: &Box<T>, parameters: P) -> TestCaseResult {
        self.as_ref().assert(output.as_ref(), parameters)
    }
}

impl<T: Debug, U: Debug + Input<T, P>, P> Input<Option<T>, P> for Option<U> {
    fn assert(self, output: Option<T>, parameters: P) -> TestCaseResult {
        match (self, output) {
            (Some(input), Some(output)) => input.assert(output, parameters),
            (None, None) => Ok(()),
            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}"
            ))),
        }
    }
}

impl<T: Debug, U: Debug, P: Clone> Input<&[T], P> for &[U]
where
    for<'a, 'b> &'a U: Input<&'b T, P>,
{
    fn assert(self, output: &[T], parameters: P) -> TestCaseResult {
        prop_assert_eq!(self.len(), output.len());

        for (input, output) in self.iter().zip(output.iter()) {
            input.assert(output, parameters.clone())?;
        }

        Ok(())
    }
}

impl<T: Debug, U: Debug, V: Debug, W: Debug, P: Clone> Input<&(T, U), P>
    for &(V, W)
where
    for<'a, 'b> &'a V: Input<&'b T, P>,
    for<'a, 'b> &'a W: Input<&'b U, P>,
{
    fn assert(self, output: &(T, U), parameters: P) -> TestCaseResult {
        self.0.assert(&output.0, parameters.clone())?;
        self.1.assert(&output.1, parameters)?;

        Ok(())
    }
}
