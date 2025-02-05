use crate::{
    arena::{Key, ID},
    ir::representation::borrow::LocalRegion,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct LocalRegionGenerator(usize);

impl LocalRegionGenerator {
    /// Creates a new instance of [`LocalRegionGenerator`]
    pub const fn new() -> Self { Self(0) }

    /// Generate a new local region id
    pub fn next(&mut self) -> ID<LocalRegion> {
        let id = self.0;
        self.0 += 1;

        ID::from_index(id)
    }
}
