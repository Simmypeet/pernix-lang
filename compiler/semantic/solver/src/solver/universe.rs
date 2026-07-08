use pernixc_type::r#type::universe::UniverseIndex;

use crate::solver::Solver;

#[derive(Debug, Clone, Copy, Default)]
pub struct Universe {
    current_universe: UniverseIndex,
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
