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

#[derive(Debug, Default)]
pub struct VariableInfos {
    inference_variables: FreshInferenceVariable,
    skolemized_variables: FreshSkolemizedVariable,
}

impl Solver<'_> {
    pub fn get_inference_variable_universe(
        &self,
        inference_variable_id: &InferenceVariable,
    ) -> UniverseIndex {
        self.variable_infos
            .inference_variables
            .infos
            .get(inference_variable_id)
            .map(|x| x.universe)
            .unwrap()
    }

    pub fn get_skolemized_variable_universe(
        &self,
        skolemized_variable: &SkolemizedVariable,
    ) -> UniverseIndex {
        self.variable_infos
            .skolemized_variables
            .infos
            .get(skolemized_variable)
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
