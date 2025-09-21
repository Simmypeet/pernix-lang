use pernixc_term::{inference, lifetime::Lifetime};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct LocalRegionGenerator(u64);

impl LocalRegionGenerator {
    /// Creates a new instance of [`LocalRegionGenerator`]
    pub const fn new() -> Self { Self(0) }

    /// Generate a new local region id
    pub const fn next(&mut self) -> inference::Variable<Lifetime> {
        let id = self.0;
        self.0 += 1;

        inference::Variable::new(id)
    }
}
