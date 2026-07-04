use std::sync::Arc;

use pernixc_qbice::create_minimal_engine as create_test_engine;
use pernixc_symbol::{GlobalSymbolID, SymbolID};
use pernixc_target::TargetID;
use pernixc_type::r#type::{
    Type2,
    bound::{Binder, BoundVariable},
    kind::TyKind,
};

use crate::{
    instance_resolution::{InstanceSource, ResolvedInstance},
    premise::Premise,
    solver::Solver,
};

const SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(1));
const NESTED_SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(2));

// input: Symbol[skolem(1), skolem(0), skolem(1)]
// premise: skolems were created in index order 0, 1
// output: for<type, lifetime> Symbol[^0.0, ^0.1, ^0.0]
#[tokio::test]
async fn bound_indices_follow_depth_first_appearance_order() {
    let engine = create_test_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let skolem_zero = Type2::new_skolemized_variable(
        solver.fresh_skolem_variable(TyKind::Lifetime),
        &engine,
    );
    let skolem_one = Type2::new_skolemized_variable(
        solver.fresh_skolem_variable(TyKind::Type),
        &engine,
    );
    let resolved = ResolvedInstance::new(
        Type2::new_symbolic(
            SYMBOL_ID,
            [skolem_one.clone(), skolem_zero.clone(), skolem_one.clone()],
            &engine,
        ),
        InstanceSource::FromInstanceScope(SYMBOL_ID),
        Arc::from([]),
    );

    let resolved =
        solver.rebind_skolems(resolved, &vec![skolem_zero, skolem_one]);

    assert_eq!(
        resolved.instance(),
        &Type2::new_symbolic_with_binder(
            SYMBOL_ID,
            Binder::new(
                engine.intern_unsized(vec![TyKind::Type, TyKind::Lifetime,])
            ),
            [
                Type2::new_bound_variable(BoundVariable::new(0, 0), &engine),
                Type2::new_bound_variable(BoundVariable::new(0, 1), &engine),
                Type2::new_bound_variable(BoundVariable::new(0, 0), &engine),
            ],
            &engine,
        )
    );
}

// input: Symbol[NestedSymbol[skolem(0)]]
// premise: skolem(0) is a request-bound lifetime
// output: for<lifetime> Symbol[NestedSymbol[^1.0]]
#[tokio::test]
async fn adds_a_binder_with_nested_depth_accounted_for() {
    let engine = create_test_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let skolem = Type2::new_skolemized_variable(
        solver.fresh_skolem_variable(TyKind::Lifetime),
        &engine,
    );
    let resolved = ResolvedInstance::new(
        Type2::new_symbolic(
            SYMBOL_ID,
            [Type2::new_symbolic(NESTED_SYMBOL_ID, [skolem.clone()], &engine)],
            &engine,
        ),
        InstanceSource::FromInstanceScope(SYMBOL_ID),
        Arc::from([]),
    );
    let resolved = solver.rebind_skolems(resolved, &vec![skolem]);

    assert_eq!(
        resolved.instance(),
        &Type2::new_symbolic_with_binder(
            SYMBOL_ID,
            Binder::new(engine.intern_unsized(vec![TyKind::Lifetime])),
            [Type2::new_symbolic(
                NESTED_SYMBOL_ID,
                [Type2::new_bound_variable(BoundVariable::new(1, 0), &engine,)],
                &engine,
            )],
            &engine,
        )
    );
}
