use std::sync::Arc;

use pernixc_type::{
    generic_parameters::get_generic_parameters2, symbol::Symbol2, r#type::Type2,
};

use crate::{
    constraints::Constraints,
    instance_resolution::{
        InstanceSource, LexicalInstance, LexicalInstanceCandidatesKey,
        ResolveError, ResolveInstanceResult, ResolvedInstance,
    },
    solver::{OverflowError, Solver},
};

impl Solver<'_> {
    /// Resolves an instance from the candidates visible at the premise's query
    /// site.
    pub async fn resolve_lexical_instance(
        &mut self,
        trait_ref: Symbol2,
    ) -> Result<ResolveInstanceResult, OverflowError> {
        let candidates = self
            .engine()
            .query(&LexicalInstanceCandidatesKey {
                current_site: self.premise_query_site(),
            })
            .await;
        let mut applicable = Vec::new();

        for candidate in candidates.available_instances() {
            if let Some(resolved) = self
                .resolve_lexical_candidate(candidate, trait_ref.clone())
                .await?
            {
                applicable.push(resolved);
            }
        }

        match applicable.len() {
            0 => Ok(Err(ResolveError::NotFound)),
            1 => Ok(Ok(applicable.pop().unwrap())),
            _ => Ok(Err(ResolveError::Ambiguous(
                applicable
                    .into_iter()
                    .map(|(instance, _)| instance.source())
                    .collect(),
            ))),
        }
    }

    async fn resolve_lexical_candidate(
        &mut self,
        candidate: &LexicalInstance,
        trait_ref: Symbol2,
    ) -> Result<Option<(ResolvedInstance, Constraints)>, OverflowError> {
        match candidate {
            LexicalInstance::FromInstanceParameter(parameter_id) => {
                let generic_parameters = self
                    .engine()
                    .get_generic_parameters2(parameter_id.parent_id())
                    .await;
                let Some(candidate_trait_ref) = generic_parameters
                    [parameter_id.id()]
                .as_trait_ref_instance()
                .cloned() else {
                    return Ok(None);
                };
                let Some(constraints) = self
                    .match_trait_ref(candidate_trait_ref, trait_ref)
                    .await?
                else {
                    return Ok(None);
                };

                Ok(Some((
                    ResolvedInstance::new(
                        Type2::new_generic_parameter(
                            *parameter_id,
                            self.engine(),
                        ),
                        InstanceSource::FromInstanceParameterID(*parameter_id),
                        Arc::from([]),
                    ),
                    constraints,
                )))
            }
            LexicalInstance::InInstance(_) => {
                todo!("resolving an instance from its enclosing scope")
            }
            LexicalInstance::FromAssociatedInstance(_) => {
                todo!("resolving an associated lexical instance")
            }
        }
    }
}
