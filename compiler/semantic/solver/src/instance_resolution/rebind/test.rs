use std::sync::Arc;

use pernixc_qbice::{
    TrackedEngine, create_minimal_engine as create_test_engine,
};
use pernixc_symbol::{GlobalSymbolID, SymbolID};
use pernixc_target::TargetID;
use pernixc_type::{
    predicate::{Equality, Predicate2},
    symbol::TraitRef2,
    r#type::{
        Type2,
        bound::{Binder, BoundVariable},
        constructor::Primitive,
        kind::TyKind,
    },
};
use qbice::storage::intern::Interned;

use crate::{
    instance_resolution::{
        InstanceResolutionFrame, InstanceSource, ResolveSoftError,
        ResolvedInstance, UnsatisfiedPredicate,
    },
    premise::Premise,
    solver::Solver,
};

const SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(1));
const NESTED_SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(2));

fn bound(index: usize, engine: &TrackedEngine) -> Interned<Type2> {
    Type2::new_bound_variable(BoundVariable::new(0, index), engine)
}

fn trait_ref(
    generic_arguments: Vec<Interned<Type2>>,
    bound_kinds: Vec<TyKind>,
    engine: &TrackedEngine,
) -> TraitRef2 {
    TraitRef2::new(
        SYMBOL_ID,
        engine.intern_unsized(generic_arguments),
        Binder::new(engine.intern_unsized(bound_kinds)),
    )
}

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

// input: Symbol[] with unsatisfied for<type> skolem(0) = ^0.0
// premise: skolem(0) is a request-bound type
// output: Symbol[]; soft error is for<type, type> ^0.0 = ^0.1
#[tokio::test]
async fn soft_error_predicate_rebuilds_binder_in_appearance_order() {
    let engine = create_test_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let skolem = Type2::new_skolemized_variable(
        solver.fresh_skolem_variable(TyKind::Type),
        &engine,
    );
    let existing_bound =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let resolved = ResolvedInstance::new(
        Type2::new_symbolic(SYMBOL_ID, [], &engine),
        InstanceSource::FromInstanceScope(SYMBOL_ID),
        Arc::from([ResolveSoftError::UnsatisfiedPredicate(
            UnsatisfiedPredicate::new(
                Predicate2::Equality(Equality::new(
                    Binder::new(engine.intern_unsized(vec![TyKind::Type])),
                    skolem.clone(),
                    existing_bound,
                )),
                None,
                Arc::from([]),
            ),
        )]),
    );

    let resolved = solver.rebind_skolems(resolved, &vec![skolem]);

    assert_eq!(
        resolved.instance(),
        &Type2::new_symbolic_with_binder(
            SYMBOL_ID,
            Binder::new(engine.intern_unsized([])),
            [],
            &engine,
        )
    );
    assert_eq!(resolved.soft_errors(), [
        ResolveSoftError::UnsatisfiedPredicate(UnsatisfiedPredicate::new(
            Predicate2::Equality(Equality::new(
                Binder::new(
                    engine.intern_unsized(vec![TyKind::Type, TyKind::Type])
                ),
                Type2::new_bound_variable(BoundVariable::new(0, 0), &engine),
                Type2::new_bound_variable(BoundVariable::new(0, 1), &engine),
            )),
            None,
            Arc::from([]),
        ),)
    ]);
}

// input: stack frames Trait[^0.0, skolem(T), skolem('a)] and
//        Trait[skolem('a), skolem(T), ^0.1]
// premise: each trait ref has its own pre-existing binder
// output: binders use local appearance order and omit unused old variables
#[tokio::test]
async fn resolution_frames_rebind_with_independent_index_maps() {
    let engine = create_test_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let skolem_type = Type2::new_skolemized_variable(
        solver.fresh_skolem_variable(TyKind::Type),
        &engine,
    );
    let skolem_lifetime = Type2::new_skolemized_variable(
        solver.fresh_skolem_variable(TyKind::Lifetime),
        &engine,
    );
    let bool_ty = Type2::new_primitive(Primitive::Bool, &engine);
    let resolved = ResolvedInstance::new(
        Type2::new_symbolic(SYMBOL_ID, [], &engine),
        InstanceSource::FromInstanceScope(SYMBOL_ID),
        Arc::from([ResolveSoftError::UnsatisfiedPredicate(
            UnsatisfiedPredicate::new(
                Predicate2::Equality(Equality::new(
                    Binder::new(engine.intern_unsized([])),
                    bool_ty.clone(),
                    bool_ty.clone(),
                )),
                None,
                Arc::from([
                    InstanceResolutionFrame::new(
                        SYMBOL_ID,
                        trait_ref(
                            vec![
                                bound(0, &engine),
                                skolem_type.clone(),
                                skolem_lifetime.clone(),
                            ],
                            vec![TyKind::Lifetime],
                            &engine,
                        ),
                    ),
                    InstanceResolutionFrame::new(
                        NESTED_SYMBOL_ID,
                        trait_ref(
                            vec![
                                skolem_lifetime.clone(),
                                skolem_type.clone(),
                                bound(1, &engine),
                            ],
                            vec![TyKind::Type, TyKind::Type],
                            &engine,
                        ),
                    ),
                ]),
            ),
        )]),
    );

    let resolved =
        solver.rebind_skolems(resolved, &vec![skolem_type, skolem_lifetime]);
    let soft_error =
        ResolveSoftError::UnsatisfiedPredicate(UnsatisfiedPredicate::new(
            Predicate2::Equality(Equality::new(
                Binder::new(engine.intern_unsized([])),
                bool_ty.clone(),
                bool_ty,
            )),
            None,
            Arc::from([
                InstanceResolutionFrame::new(
                    SYMBOL_ID,
                    trait_ref(
                        vec![
                            bound(0, &engine),
                            bound(1, &engine),
                            bound(2, &engine),
                        ],
                        vec![TyKind::Lifetime, TyKind::Type, TyKind::Lifetime],
                        &engine,
                    ),
                ),
                InstanceResolutionFrame::new(
                    NESTED_SYMBOL_ID,
                    trait_ref(
                        vec![
                            bound(0, &engine),
                            bound(1, &engine),
                            bound(2, &engine),
                        ],
                        vec![TyKind::Lifetime, TyKind::Type, TyKind::Type],
                        &engine,
                    ),
                ),
            ]),
        ));

    assert_eq!(resolved.soft_errors(), [soft_error]);
}
