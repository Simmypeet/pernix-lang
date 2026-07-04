use pernixc_hash::FxHashMap;
use pernixc_qbice::TrackedEngine;
use pernixc_type::r#type::{
    Type2,
    bound::{Binder, BoundVariable},
    constructor::Constructor,
    context::TyContext,
    kind::TyKind,
    rewrite::{RewriteContext, TypeRewriter, rewrite_type_or_clone},
    skolem::SkolemizedVariable,
};
use qbice::storage::intern::Interned;

use crate::{
    instance_resolution::ResolvedInstance,
    solver::{BoundInstantiation, Solver},
};

#[cfg(test)]
mod test;

struct SkolemRebinder<'a> {
    skolem_kinds: FxHashMap<SkolemizedVariable, TyKind>,
    bound_indices: FxHashMap<SkolemizedVariable, usize>,
    bound_kinds: Vec<TyKind>,
    engine: &'a TrackedEngine,
}

impl<'a> SkolemRebinder<'a> {
    fn new(
        instantiations: &BoundInstantiation,
        existing_bound_kinds: impl IntoIterator<Item = TyKind>,
        solver: &Solver<'_>,
        engine: &'a TrackedEngine,
    ) -> Self {
        let skolem_kinds = instantiations
            .iter()
            .map(|instantiation| {
                let Type2::SkolemizedVariable(skolem) = &**instantiation else {
                    unreachable!("skolemization must produce only skolems")
                };

                (*skolem, solver.get_skolemized_variable_kind(skolem))
            })
            .collect();

        Self {
            skolem_kinds,
            bound_indices: FxHashMap::default(),
            bound_kinds: existing_bound_kinds.into_iter().collect(),
            engine,
        }
    }
}

impl TypeRewriter for SkolemRebinder<'_> {
    fn rewrite_skolemized_variable(
        &mut self,
        variable: SkolemizedVariable,
        context: RewriteContext,
    ) -> Option<Interned<Type2>> {
        let kind = *self.skolem_kinds.get(&variable)?;
        let index = if let Some(index) = self.bound_indices.get(&variable) {
            *index
        } else {
            let index = self.bound_kinds.len();
            self.bound_indices.insert(variable, index);
            self.bound_kinds.push(kind);
            index
        };

        Some(self.engine.intern(Type2::BoundVariable(BoundVariable::new(
            context.binder_depth(),
            index,
        ))))
    }
}

impl Solver<'_> {
    pub(super) fn rebind_skolems(
        &self,
        resolved_instance: ResolvedInstance,
        instantiations: &BoundInstantiation,
    ) -> ResolvedInstance {
        if instantiations.is_empty() {
            return resolved_instance;
        }

        let Type2::Application(application) = &*resolved_instance.instance
        else {
            return resolved_instance;
        };
        let Constructor::Symbolic(symbolic) = application.constructor() else {
            return resolved_instance;
        };

        let mut rebinder = SkolemRebinder::new(
            instantiations,
            symbolic.binder().kinds(),
            self,
            self.engine(),
        );
        let arguments = application
            .arguments()
            .iter()
            .map(|argument| {
                rewrite_type_or_clone(argument, &mut rebinder, self.engine())
            })
            .collect::<Vec<_>>();

        let binder =
            Binder::new(self.engine().intern_unsized(rebinder.bound_kinds));
        let instance = Type2::new_symbolic_with_binder(
            symbolic.symbol_id(),
            binder,
            arguments,
            self.engine(),
        );

        ResolvedInstance {
            instance,
            source: resolved_instance.source,
            soft_errors: resolved_instance.soft_errors,
        }
    }
}
