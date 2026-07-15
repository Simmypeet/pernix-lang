use pernixc_type::{
    symbol::Symbol2,
    r#type::{Type2, constructor::Constructor},
};

use crate::{
    instance_resolution::{
        InstanceResolutionFrame, ResolveSoftError, ResolvedInstance,
        UnsatisfiedPredicate,
    },
    rebind::{SkolemKinds, collect_skolem_kinds},
    solver::{BoundInstantiation, Solver},
};

fn rebind_soft_error(
    solver: &mut Solver<'_>,
    error: ResolveSoftError,
    skolem_kinds: &SkolemKinds,
) -> ResolveSoftError {
    match error {
        ResolveSoftError::UnsatisfiedPredicate(unsatisfied) => {
            let predicate =
                solver.rebind_predicate(&unsatisfied.predicate, skolem_kinds);
            let instance_resolution_stack = unsatisfied
                .instance_resolution_stack
                .iter()
                .map(|frame| {
                    InstanceResolutionFrame::new(
                        frame.instance_symbol,
                        solver.rebind_trait_ref(&frame.trait_ref, skolem_kinds),
                    )
                })
                .collect::<Vec<_>>();

            ResolveSoftError::UnsatisfiedPredicate(UnsatisfiedPredicate::new(
                predicate,
                unsatisfied.predicate_declaration_span,
                instance_resolution_stack.into(),
            ))
        }
    }
}

impl Solver<'_> {
    pub(super) fn rebind_skolems(
        &mut self,
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
        let skolem_kinds = collect_skolem_kinds(instantiations);
        let symbol = Symbol2::new(
            symbolic.symbol_id(),
            self.engine().intern_unsized(application.arguments().to_vec()),
        );
        let (binder, symbol) =
            self.rebind_symbol(&symbol, symbolic.binder(), &skolem_kinds);
        let soft_errors = resolved_instance
            .soft_errors
            .iter()
            .cloned()
            .map(|error| rebind_soft_error(self, error, &skolem_kinds))
            .collect::<Vec<_>>();
        let instance = Type2::new_symbolic_with_binder(
            symbol.symbol_id(),
            binder,
            symbol.generic_arguments().iter().cloned(),
            self.engine(),
        );

        ResolvedInstance {
            instance,
            source: resolved_instance.source,
            soft_errors: soft_errors.into(),
        }
    }
}

#[cfg(test)]
mod test;
