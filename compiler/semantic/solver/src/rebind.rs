use pernixc_hash::FxHashMap;
use pernixc_qbice::TrackedEngine;
use pernixc_type::{
    predicate::{Equality, Marker, Predicate2, Tuple},
    symbol::{Symbol2, TraitRef2},
    r#type::{
        Type2,
        bound::{Binder, BoundVariable, Instantiate},
        kind::TyKind,
        rewrite::{RewriteContext, TypeRewriter, rewrite_type_or_clone},
        skolem::SkolemizedVariable,
    },
};
use qbice::storage::intern::Interned;

use crate::solver::{BoundInstantiation, Solver};

pub(crate) type SkolemKinds = FxHashMap<SkolemizedVariable, TyKind>;

pub(crate) fn collect_skolem_kinds(
    instantiations: &BoundInstantiation,
) -> SkolemKinds {
    instantiations
        .iter()
        .map(|instantiation| {
            let Type2::SkolemizedVariable(skolem) = &**instantiation else {
                unreachable!("skolemization must produce only skolems")
            };

            (*skolem, skolem.kind())
        })
        .collect()
}

struct SkolemRebinder<'a> {
    skolem_kinds: &'a SkolemKinds,
    instantiated_bound_skolem_kinds: SkolemKinds,
    bound_indices: FxHashMap<SkolemizedVariable, usize>,
    bound_kinds: Vec<TyKind>,
    engine: &'a TrackedEngine,
}

impl<'a> SkolemRebinder<'a> {
    fn new(
        skolem_kinds: &'a SkolemKinds,
        instantiated_bound_skolem_kinds: SkolemKinds,
        engine: &'a TrackedEngine,
    ) -> Self {
        Self {
            skolem_kinds,
            instantiated_bound_skolem_kinds,
            bound_indices: FxHashMap::default(),
            bound_kinds: Vec::new(),
            engine,
        }
    }

    fn rebind_type(&mut self, ty: &Interned<Type2>) -> Interned<Type2> {
        rewrite_type_or_clone(ty, self, self.engine)
    }
}

impl TypeRewriter for SkolemRebinder<'_> {
    fn rewrite_skolemized_variable(
        &mut self,
        variable: SkolemizedVariable,
        context: RewriteContext,
    ) -> Option<Interned<Type2>> {
        let kind = self
            .skolem_kinds
            .get(&variable)
            .or_else(|| self.instantiated_bound_skolem_kinds.get(&variable))
            .copied()?;
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

fn instantiate_and_rebind<T: Instantiate + Clone, R>(
    solver: &mut Solver<'_>,
    binder: &Binder,
    value: &T,
    skolem_kinds: &SkolemKinds,
    rebind: impl FnOnce(T, &mut SkolemRebinder<'_>) -> R,
) -> (Binder, R) {
    let instantiations = solver.create_skolem_instantiations(binder.kinds());
    let value = binder.instantiate(value, &instantiations, solver.engine());
    let instantiated_bound_skolem_kinds = collect_skolem_kinds(&instantiations);
    let mut rebinder = SkolemRebinder::new(
        skolem_kinds,
        instantiated_bound_skolem_kinds,
        solver.engine(),
    );
    let rebound = rebind(value, &mut rebinder);
    let binder =
        Binder::new(solver.engine().intern_unsized(rebinder.bound_kinds));

    (binder, rebound)
}

impl Solver<'_> {
    pub(crate) fn rebind_symbol(
        &mut self,
        symbol: &Symbol2,
        binder: &Binder,
        skolem_kinds: &SkolemKinds,
    ) -> (Binder, Symbol2) {
        let (binder, generic_arguments) = instantiate_and_rebind(
            self,
            binder,
            symbol.generic_arguments(),
            skolem_kinds,
            |generic_arguments, rebinder| {
                generic_arguments
                    .iter()
                    .map(|argument| rebinder.rebind_type(argument))
                    .collect::<Vec<_>>()
            },
        );
        let symbol = Symbol2::new(
            symbol.symbol_id(),
            self.engine().intern_unsized(generic_arguments),
        );

        (binder, symbol)
    }

    pub(crate) fn rebind_trait_ref(
        &mut self,
        trait_ref: &TraitRef2,
        skolem_kinds: &SkolemKinds,
    ) -> TraitRef2 {
        let symbol = Symbol2::new(
            trait_ref.trait_id(),
            trait_ref.generic_arguments().clone(),
        );
        let (binder, symbol) =
            self.rebind_symbol(&symbol, trait_ref.binder(), skolem_kinds);

        TraitRef2::from_symbol(symbol, binder)
    }

    pub(crate) fn rebind_predicate(
        &mut self,
        predicate: &Predicate2,
        skolem_kinds: &SkolemKinds,
    ) -> Predicate2 {
        match predicate {
            Predicate2::Outlives(outlives) => {
                // Outlives predicates have no binder to instantiate.
                Predicate2::Outlives(outlives.clone())
            }
            Predicate2::Tuple(tuple) => {
                let (binder, operand) = instantiate_and_rebind(
                    self,
                    tuple.binder(),
                    tuple.operand(),
                    skolem_kinds,
                    |operand, rebinder| rebinder.rebind_type(&operand),
                );

                Predicate2::Tuple(Tuple::new(binder, operand))
            }
            Predicate2::Marker(marker) => {
                let (binder, symbol) = self.rebind_symbol(
                    marker.symbol(),
                    marker.binder(),
                    skolem_kinds,
                );

                Predicate2::Marker(Marker::new(marker.polar(), binder, symbol))
            }
            Predicate2::Equality(equality) => {
                let operands: Interned<[Interned<Type2>]> =
                    self.engine().intern_unsized(vec![
                        equality.left().clone(),
                        equality.right().clone(),
                    ]);
                let (binder, operands) = instantiate_and_rebind(
                    self,
                    equality.binder(),
                    &operands,
                    skolem_kinds,
                    |operands, rebinder| {
                        operands
                            .iter()
                            .map(|operand| rebinder.rebind_type(operand))
                            .collect::<Vec<_>>()
                    },
                );

                Predicate2::Equality(Equality::new(
                    binder,
                    operands[0].clone(),
                    operands[1].clone(),
                ))
            }
        }
    }
}
