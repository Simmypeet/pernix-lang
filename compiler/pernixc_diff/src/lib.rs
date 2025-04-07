//! This crate provides algorithms related to comparing the difference between
//! two values. Primarily used for incremental compilation process.

use std::ops::Range;

/// A trait used for comparing the equality of two values with additional
/// context parameters.
///
/// This trait is primarily used for computing the difference between two
/// values.
pub trait Comparable<T> {
    /// Compares the current value with another value using the provided
    /// parameters.
    fn compare(&self, other: &Self, parameters: T) -> bool;
}

/// The result of a [`single_diff`] operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SingleDiff {
    /// The range of the original value that needs to be replaced.
    pub replace_original: Range<usize>,

    /// The range of the new value that will replace the original value.
    /// This range is relative to the new value.
    pub replace_with_from_new: Range<usize>,
}

/// Computes a single diff between two slices of values.
///
/// # Example
///
/// Given two strings "abc" and "axyc", the diff would be:
/// `(1..2, 1..3)` indicating that the character range `1..2` in the first
/// string should be replaced with the character range `1..3` in the second
/// string to make them equal.
pub fn single_diff<T: Comparable<U>, U: Clone>(
    original: &[T],
    new: &[T],
    parameters: U,
) -> Option<SingleDiff> {
    let mut prefix = 0;
    while prefix < original.len()
        && prefix < new.len()
        && original[prefix].compare(&new[prefix], parameters.clone())
    {
        prefix += 1;
    }

    let mut suffix = 0;
    while suffix < (original.len() - prefix)
        && suffix < (new.len() - prefix)
        && original[original.len() - 1 - suffix]
            .compare(&new[new.len() - 1 - suffix], parameters.clone())
    {
        suffix += 1;
    }

    let replace_original = prefix..(original.len() - suffix);
    let replace_with_from_new = prefix..(new.len() - suffix);

    if replace_original.start >= replace_original.end
        && replace_with_from_new.start >= replace_with_from_new.end
    {
        return None;
    }

    Some(SingleDiff { replace_original, replace_with_from_new })
}

#[cfg(test)]
mod test;
