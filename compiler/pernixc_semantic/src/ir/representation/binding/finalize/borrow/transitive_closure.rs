use std::{
    cell::Cell,
    collections::{HashMap, HashSet},
};

use getset::Getters;

/// Dynamic bitset used for efficiently computing the transitive closure matrix.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DynamicBitSet {
    bits: Vec<Cell<u64>>,
    size: usize,
}

impl DynamicBitSet {
    /// Creates a new dynamic bitset with the given size.
    pub fn new(size: usize) -> Self {
        let bits = vec![Cell::new(0); (size + 63) / 64];
        Self { bits, size }
    }

    /// Set the bit at the given index to 1
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds
    ///
    /// # Returns
    ///
    /// Returns true if there was a change in the bitset
    pub fn set(&self, index: usize) -> bool {
        if index >= self.size {
            panic!("index out of bounds");
        }

        let word = index / 64;
        let bit = index % 64;

        let old = self.bits[word].get();
        self.bits[word].set(old | (1 << bit));

        old != self.bits[word].get()
    }

    /// Set all bits to 0
    pub fn clear(&self) {
        for word in &self.bits {
            word.set(0);
        }
    }

    /// Set the bit at the given index to 0
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds
    ///
    /// # Returns
    ///
    /// Returns true if there was a change in the bitset
    #[allow(unused)]
    pub fn unset(&self, index: usize) -> bool {
        if index >= self.size {
            panic!("index out of bounds");
        }

        let word = index / 64;
        let bit = index % 64;

        let old = self.bits[word].get();
        self.bits[word].set(old & !(1 << bit));

        old != self.bits[word].get()
    }

    /// Get the value of the bit at the given index
    pub fn get(&self, index: usize) -> bool {
        if index >= self.size {
            panic!("index out of bounds");
        }

        let word = index / 64;
        let bit = index % 64;
        (self.bits[word].get() & (1 << bit)) != 0
    }

    /// Perform a bitwise OR operation with another bitset
    ///
    /// # Panics
    ///
    /// Panics if the bitsets have different sizes
    ///
    /// # Returns
    ///
    /// Returns true if there was a change in the bitset
    pub fn or(&self, other: &Self) -> bool {
        if self.size != other.size {
            panic!("bitsets have different sizes");
        }

        let mut changed = false;
        for (i, word) in other.bits.iter().enumerate() {
            let old = self.bits[i].get();
            self.bits[i].set(old | word.get());
            changed |= old != self.bits[i].get();
        }

        changed
    }

    /// Perform a bitwise AND operation with another bitset
    ///
    /// # Panics
    ///
    /// Panics if the bitsets have different sizes
    ///
    /// # Returns
    ///
    /// Returns true if there was a change in the bitset
    #[allow(unused)]
    pub fn and(&self, other: &Self) -> bool {
        if self.size != other.size {
            panic!("bitsets have different sizes");
        }

        let mut changed = false;
        for (i, word) in other.bits.iter().enumerate() {
            let old = self.bits[i].get();
            self.bits[i].set(old & word.get());
            changed |= old != self.bits[i].get();
        }

        changed
    }

    /// Checks if any bit is set to 1
    #[allow(unused)]
    pub fn is_not_zero(&self) -> bool {
        self.bits.iter().any(|word| word.get() != 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct TransitiveClosure {
    size: usize,
    transitive_closure: Vec<DynamicBitSet>,

    #[get = "pub"]
    direct_adjacency: HashMap<usize, HashSet<usize>>,

    #[get = "pub"]
    reversed_direct_adjacency: HashMap<usize, HashSet<usize>>,
}

impl TransitiveClosure {
    /// Creates a new transitive closure matrix from a list of edges and the
    /// size of the matrix.
    pub fn new(
        edges: impl IntoIterator<Item = (usize, usize)>,
        size: usize,
    ) -> Self {
        let mut direct_adjacency = HashMap::new();
        let mut reversed_direct_adjacency = HashMap::new();

        for (from, to) in edges {
            direct_adjacency
                .entry(from)
                .or_insert_with(HashSet::new)
                .insert(to);

            reversed_direct_adjacency
                .entry(to)
                .or_insert_with(HashSet::new)
                .insert(from);
        }

        let transitive_closure = vec![DynamicBitSet::new(size); size];

        let mut result = Self {
            size,
            transitive_closure,
            direct_adjacency,
            reversed_direct_adjacency,
        };

        // add self-loops
        for i in 0..size {
            result.transitive_closure[i].set(i);
        }

        result.compute_transitive_closure();

        result
    }

    /// Removes all the edges of the `for x in predecessors(node) (x, node)`
    /// and returns the set of predecessors.
    pub fn detach(&mut self, node: usize) -> HashSet<usize> {
        let predecessors =
            self.reversed_direct_adjacency.remove(&node).unwrap_or_default();

        for pred in predecessors.iter().copied() {
            let pred_adj = self.direct_adjacency.get_mut(&pred).unwrap();

            pred_adj.remove(&node);

            if pred_adj.is_empty() {
                self.direct_adjacency.remove(&pred);
            }
        }

        // recompute the transitive closure
        for i in 0..self.size {
            self.transitive_closure[i].clear();
            self.transitive_closure[i].set(i);
        }

        self.compute_transitive_closure();

        predecessors
    }

    fn compute_transitive_closure(&mut self) {
        let mut changed = true;
        while changed {
            changed = false;

            for (from, to) in self
                .direct_adjacency
                .iter()
                .flat_map(|(from, tos)| tos.iter().map(move |to| (*from, *to)))
            {
                changed |= self.transitive_closure[from].set(to);
                changed |= self.transitive_closure[from]
                    .or(&self.transitive_closure[to]);
            }
        }
    }

    /// Checks if the given node has a path to any other node (except itself).
    pub fn does_not_go_to_any_except_itself(&mut self, node: usize) -> bool {
        let expected_word_index = node / 64;
        let bit = node % 64;

        let expected = 1 << bit;

        self.transitive_closure[node].bits.iter().enumerate().all(
            |(word_idx, word)| {
                if word_idx == expected_word_index {
                    word.get() == expected
                } else {
                    word.get() == 0
                }
            },
        )
    }

    /// Checks if there is a path from the `from` vertex to the `to` vertex.
    pub fn has_path(&self, from: usize, to: usize) -> bool {
        self.transitive_closure[from].get(to)
    }

    /// Returns an iterator over the vertices reachable from the given vertex
    /// index.
    #[allow(unused)]
    pub fn reachable_from(
        &self,
        from: usize,
    ) -> impl Iterator<Item = usize> + '_ {
        if from >= self.size {
            panic!("Vertex indices out of bounds");
        }

        (0..self.size).filter(move |&to| self.transitive_closure[from].get(to))
    }

    /// Removes a set of edges from the transitive closure matrix
    ///
    /// # Performance
    ///
    /// This operation recomputes the whole transitive closure matrix.
    /// Therefore, it's better to batch the removal of edges and call this
    /// function once.
    #[allow(unused)]
    pub fn remove_edges(
        &mut self,
        edges: impl IntoIterator<Item = (usize, usize)>,
    ) {
        for remove in edges {
            let empty = self.direct_adjacency.get_mut(&remove.0).map(|set| {
                set.remove(&remove.1);
                set.is_empty()
            });

            if let Some(true) = empty {
                self.direct_adjacency.remove(&remove.0);
            }

            let empty =
                self.reversed_direct_adjacency.get_mut(&remove.1).map(|set| {
                    set.remove(&remove.0);
                    set.is_empty()
                });

            if let Some(true) = empty {
                self.reversed_direct_adjacency.remove(&remove.1);
            }
        }

        // recompute the transitive closure
        for i in 0..self.size {
            self.transitive_closure[i].clear();
            self.transitive_closure[i].set(i);
        }

        self.compute_transitive_closure();
    }

    /// Adds a new edge to the transitive closure matrix
    pub fn add_edge(&mut self, from: usize, to: usize) -> bool {
        // Check if the edge already exists
        if self.has_path(from, to) {
            return false;
        }

        self.direct_adjacency.entry(from).or_default().insert(to);
        self.reversed_direct_adjacency.entry(to).or_default().insert(from);

        if self.has_path(from, to) {
            return false;
        }

        self.transitive_closure[from].set(to);

        // incrementally update the transitive closure
        for i in 0..self.size {
            if self.transitive_closure[i].get(from) {
                self.transitive_closure[i].set(to);
                self.transitive_closure[i].or(&self.transitive_closure[to]);
            }
        }

        true
    }
}

// Unit tests
#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use super::*;

    #[test]
    fn transitive_closure() {
        let relation = TransitiveClosure::new([(0, 1), (1, 2)], 3);

        // Check paths
        assert!(relation.has_path(0, 1));
        assert!(relation.has_path(0, 2));
        assert!(relation.has_path(1, 2));

        // no self-loops
        assert!(relation.has_path(0, 0));
        assert!(relation.has_path(1, 1));
        assert!(relation.has_path(2, 2));
    }

    #[test]
    fn reachable_vertices() {
        let mut relation = TransitiveClosure::new([(0, 1), (1, 2), (2, 3)], 7);

        // Check reachable vertices
        let reachable = relation.reachable_from(0).collect::<HashSet<_>>();
        assert_eq!(reachable, [0, 1, 2, 3].into_iter().collect());

        relation.add_edge(4, 5);
        relation.add_edge(5, 6);
        relation.add_edge(6, 4);

        let reachable = relation.reachable_from(4).collect::<HashSet<_>>();
        assert_eq!(reachable, [4, 5, 6].into_iter().collect());

        let reachable = relation.reachable_from(5).collect::<HashSet<_>>();
        assert_eq!(reachable, [4, 5, 6].into_iter().collect());

        let reachable = relation.reachable_from(6).collect::<HashSet<_>>();
        assert_eq!(reachable, [4, 5, 6].into_iter().collect());
    }

    #[test]
    #[should_panic]
    fn out_of_bounds_path_check() {
        let relation = TransitiveClosure::new([(0, 1)], 2);
        relation.has_path(2, 0);
    }

    #[test]
    fn test_complex_edge() {
        let relation = TransitiveClosure::new([(0, 1), (2, 5), (1, 2)], 6);

        // Verify paths
        assert!(relation.has_path(0, 5));
    }

    #[test]
    fn add_loop_edge() {
        let mut relation = TransitiveClosure::new([(4, 5), (3, 2)], 6);

        relation.add_edge(0, 1);
        relation.add_edge(1, 2);
        relation.add_edge(2, 3);
        relation.add_edge(3, 0);

        assert!(relation.has_path(4, 5));

        let expected_reachable = [0, 1, 2, 3].into_iter().collect();

        for i in 0..4 {
            for j in 0..4 {
                assert!(relation.has_path(i, j));
            }
        }

        for i in 0..4 {
            let reachable = relation.reachable_from(i).collect::<HashSet<_>>();
            assert_eq!(reachable, expected_reachable);
        }

        for i in 0..4 {
            assert!(!relation.has_path(4, i));
            assert!(!relation.has_path(5, i));
            assert!(!relation.has_path(i, 4));
            assert!(!relation.has_path(i, 5));
        }
    }

    #[test]
    fn add_edge() {
        let mut relation = TransitiveClosure::new([(0, 1), (2, 5)], 6);

        // Add new edge
        assert!(relation.add_edge(3, 4));

        // Verify paths
        assert!(!relation.has_path(0, 5));

        assert!(relation.add_edge(1, 2));
        assert!(!relation.add_edge(2, 5));

        // Verify paths
        assert!(relation.has_path(0, 5));
        assert!(!relation.has_path(0, 3));
        assert!(!relation.has_path(0, 4));
    }

    #[test]
    fn test_complex_removal() {
        let mut closure =
            TransitiveClosure::new([(0, 1), (1, 2), (2, 3), (3, 4)], 500);

        // Initially, there's a path from 0 to 4
        assert!(closure.has_path(0, 4));

        // Remove a key edge in the middle
        closure.remove_edges([(2, 3)]);

        // Path from 0 to 4 should be broken
        assert!(!closure.has_path(0, 4));
    }
}
