//! Contains the definition of [`UnorderedPair`].

/// A simple container for storing an unordered pair of elements.
///
/// This is useful for storing pairs of elements where the order of the elements
/// does not matter.
///
/// ``` rust
/// use pernixc_semantic::unordered_pair::UnorderedPair;
///
/// let pair = UnorderedPair::new(1, 2);
/// assert_eq!(pair, UnorderedPair::new(2, 1));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnorderedPair<T>(T, T);

impl<T: Ord> Into<UnorderedPair<T>> for (T, T) {
    fn into(self) -> UnorderedPair<T> { UnorderedPair::new(self.0, self.1) }
}

impl<T: Ord> UnorderedPair<T> {
    /// Creates a new [`UnorderedPair`].
    #[must_use]
    pub fn new(first: T, second: T) -> Self {
        if first <= second {
            Self(first, second)
        } else {
            Self(second, first)
        }
    }

    /// Returns the first element of the pair.
    ///
    /// The returned element is not guaranteed to be the same as the one
    /// provided as the first element in the constructor.
    #[must_use]
    pub fn first(&self) -> &T { &self.0 }

    /// Returns the second element of the pair.
    ///
    /// The returned element is not guaranteed to be the same as the one
    /// provided as the first element in the constructor.
    #[must_use]
    pub fn second(&self) -> &T { &self.1 }

    /// Destructures the unordered pair into a tuple.
    #[must_use]
    pub fn to_tuple(self) -> (T, T) { (self.0, self.1) }
}
