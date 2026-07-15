use pernixc_hash::FxHashMap;
use pernixc_qbice::TrackedEngine;
use pernixc_type::{
    predicate::{Equality, Marker, Predicate2, Tuple},
    symbol::{Symbol2, TraitRef2},
    r#type::{
        Type2,
        bound::{Binder, BoundVariable},
        constructor::Constructor,
        kind::TyKind,
        rewrite::{RewriteContext, TypeRewriter, rewrite_type_or_clone},
        skolem::SkolemizedVariable,
    },
};
use qbice::storage::intern::Interned;

use crate::{
    instance_resolution::{
        InstanceResolutionFrame, ResolveSoftError, ResolvedInstance,
        UnsatisfiedPredicate,
    },
    solver::{BoundInstantiation, Solver},
};

struct SkolemRebinder<'a> {
    skolem_kinds: &'a FxHashMap<SkolemizedVariable, TyKind>,
    bound_indices: FxHashMap<SkolemizedVariable, usize>,
    bound_kinds: Vec<TyKind>,
    engine: &'a TrackedEngine,
}

impl<'a> SkolemRebinder<'a> {
    fn new(
        skolem_kinds: &'a FxHashMap<SkolemizedVariable, TyKind>,
        existing_bound_kinds: impl IntoIterator<Item = TyKind>,
        engine: &'a TrackedEngine,
    ) -> Self {
        Self {
            skolem_kinds,
            bound_indices: FxHashMap::default(),
            bound_kinds: existing_bound_kinds.into_iter().collect(),
            engine,
        }
    }

    fn rebind_type(&mut self, ty: &Interned<Type2>) -> Interned<Type2> {
        rewrite_type_or_clone(ty, self, self.engine)
    }

    fn rebind_symbol(&mut self, symbol: &Symbol2) -> Symbol2 {
        let generic_arguments = symbol
            .generic_arguments()
            .iter()
            .map(|argument| self.rebind_type(argument))
            .collect::<Vec<_>>();

        Symbol2::new(
            symbol.symbol_id(),
            self.engine.intern_unsized(generic_arguments),
        )
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

fn rebind_with_binder<T>(
    skolem_kinds: &FxHashMap<SkolemizedVariable, TyKind>,
    binder: &Binder,
    engine: &TrackedEngine,
    rebind: impl FnOnce(&mut SkolemRebinder<'_>) -> T,
) -> (Binder, T) {
    let mut rebinder =
        SkolemRebinder::new(skolem_kinds, binder.kinds(), engine);
    let rebound = rebind(&mut rebinder);
    let binder = Binder::new(engine.intern_unsized(rebinder.bound_kinds));

    (binder, rebound)
}

fn rebind_trait_ref(
    trait_ref: &TraitRef2,
    skolem_kinds: &FxHashMap<SkolemizedVariable, TyKind>,
    engine: &TrackedEngine,
) -> TraitRef2 {
    let (binder, generic_arguments) = rebind_with_binder(
        skolem_kinds,
        trait_ref.binder(),
        engine,
        |rebinder| {
            trait_ref
                .generic_arguments()
                .iter()
                .map(|argument| rebinder.rebind_type(argument))
                .collect::<Vec<_>>()
        },
    );

    TraitRef2::new(
        trait_ref.trait_id(),
        engine.intern_unsized(generic_arguments),
        binder,
    )
}

fn rebind_predicate(
    predicate: &Predicate2,
    skolem_kinds: &FxHashMap<SkolemizedVariable, TyKind>,
    engine: &TrackedEngine,
) -> Predicate2 {
    match predicate {
        Predicate2::Outlives(outlives) => {
            // Outlives predicates are added to the returned constraints rather
            // than emitted as soft errors, and have no binder to extend.
            Predicate2::Outlives(outlives.clone())
        }
        Predicate2::Tuple(tuple) => {
            let (binder, operand) = rebind_with_binder(
                skolem_kinds,
                tuple.binder(),
                engine,
                |rebinder| rebinder.rebind_type(tuple.operand()),
            );

            Predicate2::Tuple(Tuple::new(binder, operand))
        }
        Predicate2::Marker(marker) => {
            let (binder, symbol) = rebind_with_binder(
                skolem_kinds,
                marker.binder(),
                engine,
                |rebinder| rebinder.rebind_symbol(marker.symbol()),
            );

            Predicate2::Marker(Marker::new(marker.polar(), binder, symbol))
        }
        Predicate2::Equality(equality) => {
            let (binder, (left, right)) = rebind_with_binder(
                skolem_kinds,
                equality.binder(),
                engine,
                |rebinder| {
                    (
                        rebinder.rebind_type(equality.left()),
                        rebinder.rebind_type(equality.right()),
                    )
                },
            );

            Predicate2::Equality(Equality::new(binder, left, right))
        }
    }
}

fn rebind_soft_error(
    error: ResolveSoftError,
    skolem_kinds: &FxHashMap<SkolemizedVariable, TyKind>,
    engine: &TrackedEngine,
) -> ResolveSoftError {
    match error {
        ResolveSoftError::UnsatisfiedPredicate(unsatisfied) => {
            let predicate =
                rebind_predicate(&unsatisfied.predicate, skolem_kinds, engine);
            let instance_resolution_stack = unsatisfied
                .instance_resolution_stack
                .iter()
                .map(|frame| {
                    InstanceResolutionFrame::new(
                        frame.instance_symbol,
                        rebind_trait_ref(
                            &frame.trait_ref,
                            skolem_kinds,
                            engine,
                        ),
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
        let skolem_kinds = instantiations
            .iter()
            .map(|instantiation| {
                let Type2::SkolemizedVariable(skolem) = &**instantiation else {
                    unreachable!("skolemization must produce only skolems")
                };

                (*skolem, skolem.kind())
            })
            .collect::<FxHashMap<_, _>>();

        let (binder, arguments) = rebind_with_binder(
            &skolem_kinds,
            symbolic.binder(),
            self.engine(),
            |rebinder| {
                application
                    .arguments()
                    .iter()
                    .map(|argument| rebinder.rebind_type(argument))
                    .collect::<Vec<_>>()
            },
        );
        let soft_errors = resolved_instance
            .soft_errors
            .iter()
            .cloned()
            .map(|error| rebind_soft_error(error, &skolem_kinds, self.engine()))
            .collect::<Vec<_>>();

        let instance = Type2::new_symbolic_with_binder(
            symbolic.symbol_id(),
            binder,
            arguments,
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
