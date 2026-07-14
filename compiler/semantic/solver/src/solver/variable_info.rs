use pernixc_type::r#type::{
    inference::InferenceVariable, kind::TyKind, skolem::SkolemizedVariable,
    universe::UniverseIndex,
};

use crate::solver::Solver;

#[derive(Debug, Default)]
struct FreshInferenceVariable {
    counter: u64,
}

#[derive(Debug, Default)]
struct FreshSkolemizedVariable {
    counter: u64,
}

impl Solver<'_> {
    /// Creates a new skolemized variable with the given kind in the root
    /// universe.
    pub(crate) const fn fresh_skolem_variable(
        &mut self,
        kind: TyKind,
    ) -> SkolemizedVariable {
        self.fresh_skolem_variable_in_universe(kind, UniverseIndex::root())
    }

    /// Creates a new skolemized variable with the given kind and universe.
    pub(crate) const fn fresh_skolem_variable_in_universe(
        &mut self,
        kind: TyKind,
        universe: UniverseIndex,
    ) -> SkolemizedVariable {
        let id = SkolemizedVariable::new(
            self.variable_infos.skolemized_variables.counter,
            kind,
            universe,
        );

        self.variable_infos.skolemized_variables.counter += 1;

        id
    }
}

#[derive(Debug, Default)]
pub struct VariableInfos {
    inference_variables: FreshInferenceVariable,
    skolemized_variables: FreshSkolemizedVariable,
}

impl Solver<'_> {
    /// Creates a new inference variable with the given kind in the root
    /// universe.
    pub(crate) const fn fresh_inference_variable(
        &mut self,
        kind: TyKind,
    ) -> InferenceVariable {
        self.fresh_inference_variable_in_universe(kind, UniverseIndex::root())
    }

    /// Creates a new inference variable with the given kind and universe.
    pub(crate) const fn fresh_inference_variable_in_universe(
        &mut self,
        kind: TyKind,
        universe: UniverseIndex,
    ) -> InferenceVariable {
        let id = InferenceVariable::new(
            self.variable_infos.inference_variables.counter,
            kind,
            universe,
        );

        self.variable_infos.inference_variables.counter += 1;

        id
    }
}
