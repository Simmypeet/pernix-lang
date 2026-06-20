use crate::solver::Solver;

#[derive(Debug, Clone, Copy, Default)]
pub struct Universe {
    current_universe: UniverseIndex,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct UniverseIndex(usize);

impl UniverseIndex {
    #[must_use]
    pub const fn root() -> Self { Self(0) }

    #[must_use]
    pub const fn next(&self) -> Self { Self(self.0 + 1) }
}

impl Solver<'_> {
    pub async fn new_universe<T>(
        &mut self,
        f: impl AsyncFnOnce(&mut Solver) -> T,
    ) -> T {
        let current = self.universe.current_universe;
        self.universe.current_universe = current.next();

        let x = f(self).await;

        self.universe.current_universe = current;

        x
    }

    #[must_use]
    pub const fn current_universe(&self) -> UniverseIndex {
        self.universe.current_universe
    }
}
