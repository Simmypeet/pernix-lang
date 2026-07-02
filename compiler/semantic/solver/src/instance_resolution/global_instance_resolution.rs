use std::sync::Arc;

use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_semantic_element::{
    global_instances::get_global_instances_of, import::get_import_map,
};
use pernixc_symbol::{
    GlobalSymbolID,
    kind::{Kind, get_kind},
    parent::get_closest_module_id,
};
use pernixc_type::{
    generic_parameters::{GenericParameterID, get_generic_parameters2},
    symbol::Symbol2,
    r#type::Type2,
};
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
    storage::intern::Interned,
};

use crate::{
    instance_resolution::{
        DeducedInstanceSymbol, InstanceSource, ResolveError,
        ResolveInstanceResult, ResolvedInstance,
    },
    order::{Order, get_instance_order},
    solver::{OverflowError, Solver},
};

type ApplicableGlobalInstance = (GlobalSymbolID, DeducedInstanceSymbol);

impl Solver<'_> {
    /// Resolves the most-specific applicable global instance for `trait_ref`.
    pub async fn resolve_global_instance(
        &mut self,
        trait_ref: Symbol2,
    ) -> Result<ResolveInstanceResult, OverflowError> {
        let candidate_ids = self
            .engine()
            .get_global_instance_candidates(
                self.premise_query_site(),
                trait_ref.symbol_id(),
            )
            .await;
        let mut current_maximas = self
            .find_maximal_global_instance_candidates(&candidate_ids, &trait_ref)
            .await?;

        if current_maximas.is_empty() {
            return Ok(Err(ResolveError::NotFound));
        }

        if current_maximas.len() > 1 {
            let sources = current_maximas
                .into_iter()
                .map(|(symbol_id, _)| {
                    InstanceSource::FromGlobalInstance(symbol_id)
                })
                .collect();

            return Ok(Err(ResolveError::Ambiguous(sources)));
        }

        let (symbol_id, deduction) = current_maximas.pop().unwrap();
        let generic_parameters =
            self.engine().get_generic_parameters2(symbol_id).await;
        let (substitution, constraints, soft_errors) = deduction.into_parts();

        let generic_arguments = generic_parameters.iter().map(|(id, _)| {
            substitution
                .get_generic(GenericParameterID::new(symbol_id, id))
                .expect("deduction must instantiate every generic parameter")
                .clone()
        });
        let instance =
            Type2::new_symbolic(symbol_id, generic_arguments, self.engine());

        Ok(Ok((
            ResolvedInstance::new(
                instance,
                InstanceSource::FromGlobalInstance(symbol_id),
                Arc::from(soft_errors),
            ),
            constraints,
        )))
    }

    async fn find_maximal_global_instance_candidates(
        &mut self,
        candidate_ids: &[GlobalSymbolID],
        trait_ref: &Symbol2,
    ) -> Result<Vec<ApplicableGlobalInstance>, OverflowError> {
        let mut current_maximas = Vec::new();

        for symbol_id in candidate_ids.iter().copied() {
            let Some(deduction) = self
                .deduce_instance_symbol(symbol_id, trait_ref.clone())
                .await?
            else {
                continue;
            };
            let Some((current_symbol_id, _)) = current_maximas.first() else {
                current_maximas.push((symbol_id, deduction));
                continue;
            };

            match self
                .engine()
                .get_instance_order(*current_symbol_id, symbol_id)
                .await?
                .expect(
                    "global instance candidates must implement the same trait",
                ) {
                Order::MoreGeneral => {
                    current_maximas.clear();
                    current_maximas.push((symbol_id, deduction));
                }
                Order::MoreSpecific => {}
                Order::Ambiguous => {
                    current_maximas.push((symbol_id, deduction));
                }
                Order::Incompatible => {
                    unreachable!(
                        "applicable global instances must have compatible \
                         trait references"
                    )
                }
            }
        }

        Ok(current_maximas)
    }
}

/// A key for querying the inherent instances available in a certain scope.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Decode,
    Encode,
    Query,
)]
#[value(Interned<[GlobalSymbolID]>)]
#[extend(name = get_global_instance_candidates, by_val)]
pub struct GlobalInstanceCandidatesKey {
    /// The site where the global instance candidates are being queried.
    pub current_site: GlobalSymbolID,

    /// The trait ID in which instance candidates must implement.
    pub trait_id: GlobalSymbolID,
}

#[executor(config = Config)]
async fn global_instance_candidates_executor(
    key: &GlobalInstanceCandidatesKey,
    engine: &TrackedEngine,
) -> Interned<[GlobalSymbolID]> {
    let mut candidates = Vec::default();

    let available_instances = engine
        .get_global_instances_of(key.trait_id, key.current_site.target_id)
        .await;

    candidates.extend(available_instances.iter().copied());

    // look at the current module and see for any imports
    let current_module_id = key
        .current_site
        .target_id
        .make_global(engine.get_closest_module_id(key.current_site).await);

    for symbol_id in
        engine.get_import_map(current_module_id).await.values().map(|x| x.id)
    {
        // if the imported symbol is an instance, add it to the candidates
        if engine.get_kind(symbol_id).await == Kind::Instance {
            candidates.push(symbol_id);
        }
    }

    candidates.sort();
    candidates.dedup();

    engine.intern_unsized(candidates)
}

#[distributed_slice(PERNIX_PROGRAM)]
static GLOBAL_INSTANCE_CANDIDATES_EXECUTOR: Registration<Config> =
    Registration::new::<
        GlobalInstanceCandidatesKey,
        GlobalInstanceCandidatesExecutor,
    >();
