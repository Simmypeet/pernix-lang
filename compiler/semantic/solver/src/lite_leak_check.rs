use pernixc_hash::FxHashSet;
use pernixc_type::{
    predicate::Outlives,
    r#type::{Type2, skolem::SkolemizedVariable},
};

use crate::{constraints::Constraints, solver::Solver};

impl Solver<'_> {
    /// Checks constraints involving the given higher-ranked skolemized
    /// lifetimes and removes those skolems from the resulting constraints.
    ///
    /// Returns [`None`] when a skolemized lifetime outlives another lifetime.
    /// Constraints requiring another lifetime to outlive a skolemized lifetime
    /// are rewritten to require that lifetime to outlive `'static` instead.
    #[must_use]
    pub fn lite_leak_check(
        &self,
        constraints: Constraints,
        skolems: impl IntoIterator<Item = SkolemizedVariable>,
    ) -> Option<Constraints> {
        let skolems = skolems.into_iter().collect::<FxHashSet<_>>();

        if skolems.is_empty() {
            return Some(constraints);
        }

        let static_lifetime = Type2::new_static_lifetime(self.engine());
        let mut checked = Constraints::new();

        for constraint in constraints {
            let lesser_skolem = constraint
                .lesser()
                .as_skolemized_variable()
                .filter(|variable| skolems.contains(variable));
            let greater_skolem = constraint
                .greater()
                .as_skolemized_variable()
                .filter(|variable| skolems.contains(variable));

            if let Some(lesser_skolem) = lesser_skolem {
                if greater_skolem == Some(lesser_skolem) {
                    continue;
                }

                return None;
            }

            checked.insert(if greater_skolem.is_some() {
                Outlives::new(
                    constraint.lesser().clone(),
                    static_lifetime.clone(),
                )
            } else {
                constraint
            });
        }

        Some(checked)
    }
}

#[cfg(test)]
mod test;
