//! Contains the definition of [`TransitiveClosure`]

use std::cell::Cell;

use getset::{CopyGetters, Getters};

/// Used for efficiently representing an array of true/false values.
///
/// Instead of representing each boolean value as a byte, we can represent 64
/// boolean values as a single 64-bit integer.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BitSet {
    bits: Vec<Cell<u64>>,
    size: usize,
}

impl BitSet {
    /// Creates a new dynamic bitset with the given size.
    #[must_use]
    pub fn new(size: usize) -> Self {
        let bits = vec![Cell::new(0); (size + 63) / 64];
        Self { bits, size }
    }

    /// Set the bit at the given index to `true`
    ///
    /// # Returns
    ///
    /// Returns `None` if the index is out of bounds, otherwise returns `Some`
    /// with `true` if there was a change in the bitset.
    #[must_use]
    pub fn set(&self, index: usize) -> Option<bool> {
        if index >= self.size {
            return None;
        }

        let word = index / 64;
        let bit = index % 64;

        let old = self.bits[word].get();
        self.bits[word].set(old | (1 << bit));

        Some(old != self.bits[word].get())
    }

    /// Set all bits to 0
    pub fn clear(&self) {
        for word in &self.bits {
            word.set(0);
        }
    }

    /// Set the bit at the given index to `false`
    ///
    /// # Returns
    ///
    /// Returns `None` if the index is out of bounds, otherwise returns `Some`
    /// with `true` if there was a change in the bitset.
    #[must_use]
    pub fn unset(&self, index: usize) -> Option<bool> {
        if index >= self.size {
            return None;
        }

        let word = index / 64;
        let bit = index % 64;

        let old = self.bits[word].get();
        self.bits[word].set(old & !(1 << bit));

        Some(old != self.bits[word].get())
    }

    /// Get the value of the bit at the given index
    ///
    /// # Returns
    ///
    /// Returns `None` if the index is out of bounds, otherwise returns `Some`
    /// with the value of the bit.
    #[must_use]
    pub fn get(&self, index: usize) -> Option<bool> {
        if index >= self.size {
            return None;
        }

        let word = index / 64;
        let bit = index % 64;
        Some((self.bits[word].get() & (1 << bit)) != 0)
    }

    /// Perform a bitwise OR operation with another bitset
    ///
    /// # Returns
    ///
    /// Returns `None` if the bitsets have different sizes, otherwise returns
    /// `Some` with `true` if there was a change in the bitset.
    #[must_use]
    pub fn or(&self, other: &Self) -> Option<bool> {
        if self.size != other.size {
            return None;
        }

        let mut changed = false;
        for (i, word) in other.bits.iter().enumerate() {
            let old = self.bits[i].get();
            self.bits[i].set(old | word.get());
            changed |= old != self.bits[i].get();
        }

        Some(changed)
    }

    /// Perform a bitwise AND operation with another bitset
    ///
    /// # Returns
    ///
    /// Returns `None` if the bitsets have different sizes, otherwise returns
    /// `Some` with `true` if there was a change in the bitset.
    #[must_use]
    pub fn and(&self, other: &Self) -> Option<bool> {
        if self.size != other.size {
            return None;
        }

        let mut changed = false;
        for (i, word) in other.bits.iter().enumerate() {
            let old = self.bits[i].get();
            self.bits[i].set(old & word.get());
            changed |= old != self.bits[i].get();
        }

        Some(changed)
    }

    /// Checks if any bit is set to 1
    #[allow(unused)]
    #[must_use]
    pub fn is_not_zero(&self) -> bool {
        self.bits.iter().any(|word| word.get() != 0)
    }
}

/// A data structure to compute the transitive closure of a directed graph.
///
/// The transitive closure has a fixed size of nodes and is computed using the
/// Warshall's adjacency matrix algorithm.
///
/// Since most of the compiler's analysis is based on the directed graph
/// problem, this data structure is used to compute the transitive closure of
/// the graph and check if there is a path between two nodes efficiently.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct TransitiveClosure {
    /// The number of nodes in the graph
    #[get_copy = "pub"]
    size: usize,

    closure: Vec<BitSet>,
}

impl TransitiveClosure {
    /// Creates a new transitive closure matrix from a list of edges and the
    /// size of the matrix.
    #[allow(clippy::needless_range_loop, clippy::needless_pass_by_value)]
    #[must_use]
    pub fn new(
        edges: impl IntoIterator<Item = (usize, usize)> + Clone,
        size: usize,
        include_self_loops: bool,
    ) -> Option<Self> {
        let transitive_closure = vec![BitSet::new(size); size];

        if include_self_loops {
            for i in 0..size {
                transitive_closure[i].set(i)?;
            }
        }

        let result = Self { size, closure: transitive_closure };

        let mut changed = true;
        while changed {
            changed = false;

            for (from, to) in edges.clone() {
                changed |= result.closure[from].set(to)?;
                changed |= result.closure[from].or(&result.closure[to])?;
            }
        }

        Some(result)
    }

    /// Checks if there is a path from the `from` vertex to the `to` vertex.
    /// index.
    #[must_use]
    pub fn has_path(&self, from: usize, to: usize) -> Option<bool> {
        self.closure.get(from)?.get(to)
    }

    /// Returns an iterator over the vertices reachable from the given vertex
    /// index.
    #[must_use]
    pub fn reachable_from(
        &self,
        from: usize,
    ) -> Option<impl Iterator<Item = usize> + '_ + Clone> {
        if from >= self.size {
            return None;
        }

        Some(
            (0..self.size)
                .filter(move |&to| self.closure[from].get(to).unwrap()),
        )
    }
}

// Unit tests
#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use super::*;

    #[test]
    fn transitive_closure_basic() {
        let relation =
            TransitiveClosure::new([(0, 1), (1, 2)], 3, false).unwrap();

        // Check paths
        assert!(relation.has_path(0, 1).unwrap());
        assert!(relation.has_path(0, 2).unwrap());
        assert!(relation.has_path(1, 2).unwrap());

        // no self-loops
        assert!(!relation.has_path(0, 0).unwrap());
        assert!(!relation.has_path(1, 1).unwrap());
        assert!(!relation.has_path(2, 2).unwrap());
    }

    #[test]
    fn reachable_vertices() {
        let relation = TransitiveClosure::new(
            [(0, 1), (1, 2), (2, 3), (4, 5), (4, 6)],
            7,
            false,
        )
        .unwrap();

        // Check reachable vertices
        let reachable =
            relation.reachable_from(0).unwrap().collect::<HashSet<_>>();
        assert_eq!(reachable, [1, 2, 3].into_iter().collect());

        let reachable =
            relation.reachable_from(4).unwrap().collect::<HashSet<_>>();
        assert_eq!(reachable, [5, 6].into_iter().collect());

        let reachable =
            relation.reachable_from(5).unwrap().collect::<HashSet<_>>();
        assert_eq!(reachable, HashSet::new());

        // no self-loops
        for i in 0..7 {
            let reachable =
                relation.reachable_from(i).unwrap().collect::<HashSet<_>>();
            assert!(!reachable.contains(&i));
            assert!(!relation.has_path(i, i).unwrap());
        }
    }
    #[test]
    fn complex_edge() {
        let relation =
            TransitiveClosure::new([(0, 1), (2, 5), (1, 2)], 6, false).unwrap();

        // Verify paths
        assert!(relation.has_path(0, 5).unwrap());
    }

    #[test]
    fn looped_relation() {
        let relation = TransitiveClosure::new(
            [(0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 0)],
            6,
            false,
        )
        .unwrap();

        let all_nodes = (0..6).collect::<HashSet<_>>();
        for i in 0..6 {
            let reachable =
                relation.reachable_from(i).unwrap().collect::<HashSet<_>>();

            assert_eq!(reachable, all_nodes);
            assert!(relation.has_path(i, i).unwrap());
        }
    }
}
