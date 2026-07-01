use pernixc_type::{
    generic_parameters::get_generic_parameters2,
    symbol::TraitRef2,
    r#type::{
        Type2,
        bound::{Binder, BoundVariable},
        constructor::{Application, Constructor, Symbolic},
        rewrite::{RewriteContext, TypeRewriter, rewrite_type_or_clone},
        skolem::SkolemizedVariable,
    },
};
use qbice::storage::intern::Interned;

use super::{
    InstanceSource, ResolveError, ResolvedInstance,
    candidates::{
        GlobalInstanceCandidatesKey, LexicalInstanceCandidate,
        LexicalInstanceCandidatesKey,
    },
};
use crate::{
    constraints::Constraints,
    order::{Order, get_instance_order},
    solver::{OverflowError, Solver},
};

pub(super) struct Candidate {
    pub(super) value: Result<
        (Interned<Type2>, Constraints),
        std::sync::Arc<super::RecursiveError>,
    >,
    pub(super) source: InstanceSource,
}

impl Solver<'_> {
    pub(super) async fn resolve_normalized(
        &mut self,
        requested: &TraitRef2,
        request_skolems: &[Interned<Type2>],
        result_binder: Option<Binder>,
    ) -> Result<
        Result<(ResolvedInstance, Constraints), ResolveError>,
        OverflowError,
    > {
        let lexical =
            self.select_lexical_candidate(requested, request_skolems).await?;
        match lexical {
            Ok(Some(candidate)) => {
                return Ok(self.finish_candidate(
                    candidate,
                    request_skolems,
                    result_binder,
                ));
            }
            Ok(None) => {}
            Err(error) => return Ok(Err(error)),
        }

        self.resolve_global_candidates(
            requested,
            request_skolems,
            result_binder,
        )
        .await
    }

    /// Searches for applicable lexical candidates for the given [`TraitRef2`].
    async fn select_lexical_candidate(
        &mut self,
        requested: &TraitRef2,
        request_skolems: &[Interned<Type2>],
    ) -> Result<Result<Option<Candidate>, ResolveError>, OverflowError> {
        let lexical = self
            .engine()
            .query(&LexicalInstanceCandidatesKey {
                current_site: self.premise_query_site(),
            })
            .await;

        let mut matches = Vec::new();
        for candidate in lexical.iter() {
            let resolved = match candidate {
                LexicalInstanceCandidate::InstanceScope(symbol) => {
                    self.resolve_symbol_candidate(
                        symbol,
                        requested,
                        request_skolems,
                        InstanceSource::InstanceScope(symbol),
                    )
                    .await?
                }
                LexicalInstanceCandidate::Associated(symbol) => {
                    self.resolve_associated_candidate(
                        symbol,
                        requested,
                        request_skolems,
                    )
                    .await?
                }
                LexicalInstanceCandidate::InstanceParameter(parameter) => {
                    let parameters = self
                        .engine()
                        .get_generic_parameters2(parameter.parent_id())
                        .await;
                    let trait_ref = parameters
                        .instance_parameters()
                        .find(|(id, _)| *id == parameter.id())
                        .and_then(|(_, parameter)| parameter.trait_ref());
                    let Some(trait_ref) = trait_ref else { continue };
                    let trait_ref = TraitRef2::from_symbol(
                        trait_ref.clone(),
                        self.empty_binder(),
                    );
                    if !self
                        .candidate_trait_ref_matches(
                            &trait_ref,
                            requested,
                            request_skolems,
                        )
                        .await?
                    {
                        continue;
                    }
                    Some(Candidate {
                        value: Ok((
                            Type2::new_generic_parameter(
                                parameter,
                                self.engine(),
                            ),
                            Constraints::new(),
                        )),
                        source: InstanceSource::InstanceParameter(parameter),
                    })
                }
            };
            if let Some(resolved) = resolved {
                matches.push(resolved);
            }
        }

        if matches.len() > 1 {
            return Ok(Err(ResolveError::Ambiguous(
                matches.into_iter().map(|candidate| candidate.source).collect(),
            )));
        }
        Ok(Ok(matches.pop()))
    }

    async fn resolve_global_candidates(
        &mut self,
        requested: &TraitRef2,
        request_skolems: &[Interned<Type2>],
        result_binder: Option<Binder>,
    ) -> Result<
        Result<(ResolvedInstance, Constraints), ResolveError>,
        OverflowError,
    > {
        let candidates = self
            .engine()
            .query(&GlobalInstanceCandidatesKey {
                current_site: self.premise_query_site(),
                trait_id: requested.trait_id(),
            })
            .await;
        let mut selected: Option<(pernixc_symbol::GlobalSymbolID, Candidate)> =
            None;
        let mut ambiguous = Vec::new();

        for symbol in candidates.iter().copied() {
            let Some(candidate) = self
                .resolve_symbol_candidate(
                    symbol,
                    requested,
                    request_skolems,
                    InstanceSource::GlobalInstance(symbol),
                )
                .await?
            else {
                continue;
            };
            let Some((selected_symbol, _)) = &selected else {
                selected = Some((symbol, candidate));
                continue;
            };
            match self
                .engine()
                .get_instance_order(*selected_symbol, symbol)
                .await?
                .expect("matching instances implement the same trait")
            {
                Order::MoreGeneral => {
                    selected = Some((symbol, candidate));
                    ambiguous.clear();
                }
                Order::MoreSpecific => {}
                Order::Ambiguous => ambiguous.push(candidate.source),
                Order::Incompatible => {
                    unreachable!("both candidates matched the same request")
                }
            }
        }

        let Some((_, selected)) = selected else {
            return Ok(Err(ResolveError::NotFound));
        };
        if !ambiguous.is_empty() {
            ambiguous.insert(0, selected.source);
            return Ok(Err(ResolveError::Ambiguous(ambiguous.into())));
        }
        Ok(self.finish_candidate(selected, request_skolems, result_binder))
    }

    async fn resolve_symbol_candidate(
        &mut self,
        symbol: pernixc_symbol::GlobalSymbolID,
        requested: &TraitRef2,
        request_skolems: &[Interned<Type2>],
        source: InstanceSource,
    ) -> Result<Option<Candidate>, OverflowError> {
        let Some(deduction) =
            self.deduce_instance(symbol, requested, request_skolems).await?
        else {
            return Ok(None);
        };
        let value = self.finish_deduction(symbol, deduction).await?;
        Ok(Some(Candidate { value, source }))
    }

    fn finish_candidate(
        &self,
        candidate: Candidate,
        request_skolems: &[Interned<Type2>],
        result_binder: Option<Binder>,
    ) -> Result<(ResolvedInstance, Constraints), ResolveError> {
        let (value, constraints) =
            candidate.value.map_err(ResolveError::Recursive)?;
        let value = match result_binder {
            Some(binder) => {
                rebind_result(&value, request_skolems, binder, self.engine())
            }
            None => value,
        };
        Ok((ResolvedInstance::new(value, candidate.source), constraints))
    }

    async fn candidate_trait_ref_matches(
        &mut self,
        candidate: &TraitRef2,
        requested: &TraitRef2,
        request_skolems: &[Interned<Type2>],
    ) -> Result<bool, OverflowError> {
        if candidate.trait_id() != requested.trait_id()
            || candidate.generic_arguments().len()
                != requested.generic_arguments().len()
        {
            return Ok(false);
        }
        let mut constraints = crate::constraints::Constraints::new();
        for (head, subject) in candidate
            .generic_arguments()
            .iter()
            .zip(requested.generic_arguments().iter())
        {
            let Some((substitution, new_constraints)) =
                self.match_types(head, subject).await
            else {
                return Ok(false);
            };
            if substitution.iter().next().is_some() {
                return Ok(false);
            }
            constraints = constraints.union_into(new_constraints);
        }
        if request_skolems.is_empty() {
            self.all_constraints_hold(constraints).await
        } else {
            let variables =
                self.hrtb_variables_from_instantiations(request_skolems.iter());
            let Some(cleaned) =
                self.check_and_clean_hrtb_constraints(&constraints, &variables)
            else {
                return Ok(false);
            };
            self.all_constraints_hold(cleaned).await
        }
    }
}

struct RebindSkolems<'a> {
    skolems: &'a [Interned<Type2>],
    engine: &'a pernixc_qbice::TrackedEngine,
}

impl TypeRewriter for RebindSkolems<'_> {
    fn rewrite_skolemized_variable(
        &mut self,
        variable: SkolemizedVariable,
        context: RewriteContext,
    ) -> Option<Interned<Type2>> {
        self.skolems
            .iter()
            .position(|ty| matches!(&**ty, Type2::SkolemizedVariable(skolem) if *skolem == variable))
            .map(|index| {
                self.engine.intern(Type2::BoundVariable(
                    BoundVariable::new(
                        context.binder_depth().saturating_sub(1),
                        index,
                    ),
                ))
            })
    }
}

fn rebind_result(
    value: &Interned<Type2>,
    skolems: &[Interned<Type2>],
    binder: Binder,
    engine: &pernixc_qbice::TrackedEngine,
) -> Interned<Type2> {
    let rebound = rewrite_type_or_clone(
        value,
        &mut RebindSkolems { skolems, engine },
        engine,
    );
    let Type2::Application(application) = &*rebound else {
        return rebound;
    };
    let Constructor::Symbolic(symbolic) = application.constructor() else {
        return rebound;
    };
    engine.intern(Type2::Application(Application::new(
        Constructor::Symbolic(Symbolic::new(symbolic.symbol_id(), binder)),
        application.arguments().clone(),
    )))
}
