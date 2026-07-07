use std::convert::Infallible;

use pernixc_type::{
    generic_parameters::GenericParameterID,
    r#type::{
        Type2,
        constructor::{
            Application, Constructor,
            rewrite::{
                AsyncTypeRewriter, RewriteContext, rewrite_type_or_clone_async,
            },
        },
        context::TyContext,
        inference::InferenceVariable,
        kind::TyKind,
        skolem::SkolemizedVariable,
    },
};
use qbice::storage::intern::Interned;

use crate::solver::{Solver, universe::UniverseIndex};

impl Solver<'_> {
    pub(super) async fn generalize_application_inference_variables(
        &mut self,
        ty: &Interned<Type2>,
        universe: UniverseIndex,
    ) -> Interned<Type2> {
        let engine = self.engine();
        let mut rewriter =
            GeneralizeInferenceVariableRewriter { solver: self, universe };

        rewrite_type_or_clone_async(ty, &mut rewriter, engine)
            .await
            .unwrap_or_else(|err| match err {})
    }
}

struct GeneralizeInferenceVariableRewriter<'solver, 'engine> {
    solver: &'solver mut Solver<'engine>,
    universe: UniverseIndex,
}

impl AsyncTypeRewriter for GeneralizeInferenceVariableRewriter<'_, '_> {
    type Error = Infallible;

    async fn rewrite_application(
        &mut self,
        application: &Application,
        _: RewriteContext,
    ) -> Result<Option<Interned<Type2>>, Self::Error> {
        if let Constructor::Lifetime(_) = application.constructor() {
            return Ok(Some(self.fresh_inference_variable(TyKind::Lifetime)));
        }

        Ok(None)
    }

    async fn rewrite_inference_variable(
        &mut self,
        variable: InferenceVariable,
        _: RewriteContext,
    ) -> Result<Option<Interned<Type2>>, Self::Error> {
        let kind = self.solver.get_inference_variable_kind(&variable);

        Ok(Some(self.fresh_inference_variable(kind)))
    }

    async fn rewrite_generic_parameter(
        &mut self,
        id: GenericParameterID,
        _: RewriteContext,
    ) -> Result<Option<Interned<Type2>>, Self::Error> {
        let ty = Type2::GenericParameter(id);

        Ok(self
            .solver
            .kind_of(&ty)
            .await
            .is_lifetime()
            .then(|| self.fresh_inference_variable(TyKind::Lifetime)))
    }

    async fn rewrite_skolemized_variable(
        &mut self,
        variable: SkolemizedVariable,
        _: RewriteContext,
    ) -> Result<Option<Interned<Type2>>, Self::Error> {
        let kind = self.solver.get_skolemized_variable_kind(&variable);

        Ok(kind.is_lifetime().then(|| self.fresh_inference_variable(kind)))
    }
}

impl GeneralizeInferenceVariableRewriter<'_, '_> {
    fn fresh_inference_variable(&mut self, kind: TyKind) -> Interned<Type2> {
        let fresh_var = self
            .solver
            .fresh_inference_variable_in_universe(kind, self.universe);

        self.solver.intern(Type2::InferenceVariable(fresh_var))
    }
}
