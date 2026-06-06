use pernixc_hash::FxHashMap;
use pernixc_type::r#type::{
    bound::BoundVariable, context::TyContext, inference::InferenceVariable,
    kind::TyKind, skolem::SkolemizedVariable,
};

use crate::solver::{Solver, UniverseIndex};

#[derive(Debug)]
struct VariableInfo {
    kind: TyKind,
    universe: UniverseIndex,
}

#[derive(Debug, Default)]
struct FreshInferenceVariable {
    counter: u64,
    infos: FxHashMap<InferenceVariable, VariableInfo>,
}

#[derive(Debug, Default)]
struct FreshSkolemizedVariable {
    counter: u64,
    infos: FxHashMap<SkolemizedVariable, VariableInfo>,
}

impl Solver<'_> {
    /// Creates a new skolemized variable with the given kind and assigns the
    /// current active universe to it.
    pub fn fresh_skolem_variable(
        &mut self,
        kind: TyKind,
    ) -> SkolemizedVariable {
        let id = SkolemizedVariable::new(
            self.variable_infos.skolemized_variables.counter,
        );

        self.variable_infos.skolemized_variables.counter += 1;
        self.variable_infos.skolemized_variables.infos.insert(
            id,
            VariableInfo { kind, universe: self.current_universe() },
        );

        id
    }
}

#[derive(Debug, Default)]
pub struct VariableInfos {
    inference_variables: FreshInferenceVariable,
    skolemized_variables: FreshSkolemizedVariable,
}

impl Solver<'_> {
    /// Creates a new inference variable with the given kind and assigns the
    /// current active universe to it.
    pub fn fresh_inference_variable(
        &mut self,
        kind: TyKind,
    ) -> InferenceVariable {
        self.fresh_inference_variable_in_universe(kind, self.current_universe())
    }

    /// Creates a new inference variable with the given kind and universe.
    pub(crate) fn fresh_inference_variable_in_universe(
        &mut self,
        kind: TyKind,
        universe: UniverseIndex,
    ) -> InferenceVariable {
        let id = InferenceVariable::new(
            self.variable_infos.inference_variables.counter,
        );

        self.variable_infos.inference_variables.counter += 1;
        self.variable_infos
            .inference_variables
            .infos
            .insert(id, VariableInfo { kind, universe });

        id
    }
}

impl Solver<'_> {
    #[must_use]
    pub(crate) fn get_inference_variable_universe(
        &self,
        inference_variable_id: InferenceVariable,
    ) -> UniverseIndex {
        self.variable_infos
            .inference_variables
            .infos
            .get(&inference_variable_id)
            .map(|x| x.universe)
            .unwrap()
    }

    #[must_use]
    pub(crate) fn get_skolemized_variable_universe(
        &self,
        skolemized_variable: SkolemizedVariable,
    ) -> UniverseIndex {
        self.variable_infos
            .skolemized_variables
            .infos
            .get(&skolemized_variable)
            .map(|x| x.universe)
            .unwrap()
    }
}

impl TyContext for Solver<'_> {
    fn get_inference_variable_kind(
        &self,
        inference_variable_id: &InferenceVariable,
    ) -> TyKind {
        self.variable_infos
            .inference_variables
            .infos
            .get(inference_variable_id)
            .map(|x| x.kind)
            .unwrap()
    }

    fn get_bound_variable_kind(&self, _: &BoundVariable) -> TyKind { todo!() }

    fn get_skolemized_variable_kind(
        &self,
        skolemized_variable: &SkolemizedVariable,
    ) -> TyKind {
        self.variable_infos
            .skolemized_variables
            .infos
            .get(skolemized_variable)
            .map(|x| x.kind)
            .unwrap()
    }
}
