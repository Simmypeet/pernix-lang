//! Contains well-formedness checking utilities for IR elements.
//!
//! This module provides the [`WfCheckVisitor`] and related functionality for
//! performing well-formedness checks on symbolic resolutions within IR
//! assignments.

use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_type_system::{
    UnrecoverableError, constraints::Constraints, normalizer::Normalizer,
};

use crate::{
    resolution_visitor::{
        Abort, RecursiveSymbolicResolutionVisitor, ResolutionVisitable,
        SymbolicResolution, accept_recursive_symbolic_resolution_visitor,
    },
    value::Environment,
};

/// A visitor that performs well-formedness checks on all symbolic resolutions.
///
/// This visitor traverses IR elements and validates that all symbolic
/// resolutions (types, symbols, variants, etc.) are well-formed according
/// to their constraints and predicates.
pub(crate) struct WfCheckVisitor<'a, 'b, N, D> {
    value_environment: &'a Environment<'b, N>,
    handler: &'a dyn Handler<D>,
    lifetime_constraints: Constraints,
    error: Option<UnrecoverableError>,
}

impl<N, D> std::fmt::Debug for WfCheckVisitor<'_, '_, N, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WfCheckVisitor")
            .field("value_environment", &"<environment>")
            .field("handler", &"<handler>")
            .field("lifetime_constraints", &self.lifetime_constraints)
            .field("error", &self.error)
            .finish()
    }
}

impl<'a, 'b, N, D> WfCheckVisitor<'a, 'b, N, D> {
    /// Creates a new well-formedness check visitor.
    #[must_use]
    pub(crate) fn new(
        value_environment: &'a Environment<'b, N>,
        handler: &'a dyn Handler<D>,
    ) -> Self {
        Self {
            value_environment,
            lifetime_constraints: Constraints::new(),
            handler,
            error: None,
        }
    }
}

impl<N: Normalizer, D> WfCheckVisitor<'_, '_, N, D>
where
    D: pernixc_diagnostic::Report
        + From<pernixc_type_system::diagnostic::Diagnostic>,
{
    /// Entry point for checking well-formedness of a resolution.
    ///
    /// This is a helper function that wraps
    /// `accept_recursive_symbolic_resolution_visitor` and propagates any
    /// `UnrecoverableError` that occurred during visiting.
    pub(crate) async fn check_resolution(
        &mut self,
        visitable: &impl ResolutionVisitable,
    ) -> Result<(), UnrecoverableError> {
        accept_recursive_symbolic_resolution_visitor(visitable, self)
            .await
            .ok();

        // Propagate error if one occurred
        if let Some(error) = self.error.take() {
            return Err(error);
        }

        Ok(())
    }

    #[must_use]
    pub(crate) fn into_constraints(self) -> Constraints {
        self.lifetime_constraints
    }
}

impl<N: Normalizer, D> RecursiveSymbolicResolutionVisitor
    for WfCheckVisitor<'_, '_, N, D>
where
    D: pernixc_diagnostic::Report
        + From<pernixc_type_system::diagnostic::Diagnostic>,
{
    async fn visit(
        &mut self,
        resolution: SymbolicResolution<'_>,
        span: RelativeSpan,
    ) -> Result<(), Abort> {
        let engine = self.value_environment.tracked_engine();

        // Create instantiation, skip if it fails
        let Some(instantiation) = resolution.create_instantiation(engine).await
        else {
            return Ok(());
        };

        // Get all well-formedness check obligations
        for obligation in resolution.get_wf_check_obligations(engine).await {
            match self
                .value_environment
                .type_environment
                .wf_check_obligation(
                    &obligation,
                    &span,
                    &instantiation,
                    &self.handler,
                )
                .await
            {
                Ok(constraints) => {
                    self.lifetime_constraints.extend(constraints);
                }

                Err(err) => {
                    self.error = Some(err);
                    return Err(Abort);
                }
            }
        }

        Ok(())
    }
}
