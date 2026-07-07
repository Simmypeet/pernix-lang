use pernixc_qbice::{
    TrackedEngine, create_minimal_engine as create_test_engine,
};
use pernixc_type::{
    predicate::{Equality, Predicate2},
    substitution::Substitution,
    r#type::{
        Type2,
        bound::{Binder, BoundVariable},
        constructor::{Lifetime, Mutability, Primitive},
        kind::TyKind,
    },
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    premise::Premise,
    solver::{OverflowError, Solver},
    type_relation::TypeRelation,
    unify::Unify,
};

async fn resolve(
    unifications: Vec<Unify>,
    premise: &Premise,
    engine: &TrackedEngine,
) -> Result<(Substitution, Vec<Unify>, Constraints), OverflowError> {
    Solver::new(premise, engine)
        .resolve_unification_constraints(unifications)
        .await
}

fn equality(
    left: Interned<Type2>,
    right: Interned<Type2>,
    engine: &TrackedEngine,
) -> Predicate2 {
    Predicate2::Equality(Equality::new(
        Binder::new(engine.intern_unsized(Vec::new())),
        left,
        right,
    ))
}

// input: bool == bool
// premise: {}
// output: {}, no residual unifications, {}
#[tokio::test]
async fn syntactic_equality_succeeds_without_work() {
    let engine = create_test_engine().await;
    let ty = Type2::new_primitive(Primitive::Bool, &engine);

    let (substitution, residual_unifications, constraints) =
        resolve(vec![Unify::new(ty.clone(), ty)], &Premise::default(), &engine)
            .await
            .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_unifications, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input: bool == int32
// premise: {}
// output: {}, residual bool == int32, {}
#[tokio::test]
async fn primitive_mismatch_remains_residual() {
    let engine = create_test_engine().await;
    let left = Type2::new_primitive(Primitive::Bool, &engine);
    let right = Type2::new_primitive(Primitive::Int32, &engine);

    let (substitution, residual_unifications, constraints) = resolve(
        vec![Unify::new(left.clone(), right.clone())],
        &Premise::default(),
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_unifications, vec![Unify::new(left, right)]);
    assert_eq!(constraints, Constraints::default());
}

// input: (bool) == (int32)
// premise: {}
// output: {}, residual bool == int32, {}
#[tokio::test]
async fn tuple_mismatch_keeps_decomposed_residual() {
    let engine = create_test_engine().await;
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let int32 = Type2::new_primitive(Primitive::Int32, &engine);
    let left = Type2::new_tuple([bool_type.clone()], &engine);
    let right = Type2::new_tuple([int32.clone()], &engine);

    let (substitution, residual_unifications, constraints) = resolve(
        vec![Unify::new(left.clone(), right.clone())],
        &Premise::default(),
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_unifications, vec![Unify::new(bool_type, int32)]);
    assert_eq!(constraints, Constraints::default());
}

// input: 'static == 'erased
// premise: {}
// output: {}, no residual unifications, 'static = 'erased
#[tokio::test]
async fn lifetime_mismatch_emits_equality_constraints() {
    let engine = create_test_engine().await;
    let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);
    let erased_lifetime = Type2::new_lifetime(Lifetime::Erased, &engine);

    let (substitution, residual_unifications, constraints) = resolve(
        vec![Unify::new(static_lifetime.clone(), erased_lifetime.clone())],
        &Premise::default(),
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_unifications, Vec::new());
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(static_lifetime, erased_lifetime)
    );
}

// input: ?T == bool
// premise: {}
// output: {?T -> bool}, no residual unifications, {}
#[tokio::test]
async fn unifies_inference_variable_with_known_type() {
    let engine = create_test_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let variable = solver.fresh_inference_variable(TyKind::Type);
    let inference = Type2::new_inference_variable(variable, &engine);
    let known = Type2::new_primitive(Primitive::Bool, &engine);

    let (substitution, residual_unifications, constraints) = solver
        .resolve_unification_constraints(vec![Unify::new(
            inference,
            known.clone(),
        )])
        .await
        .unwrap();

    assert_eq!(substitution, Substitution::singleton(variable, known));
    assert_eq!(residual_unifications, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input: ?lt == 'static
// premise: {}
// output: {?lt -> 'static}, no residual unifications, {}
#[tokio::test]
async fn unifies_lifetime_inference_variable() {
    let engine = create_test_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let variable = solver.fresh_inference_variable(TyKind::Lifetime);
    let inference = Type2::new_inference_variable(variable, &engine);
    let known = Type2::new_lifetime(Lifetime::Static, &engine);

    let (substitution, residual_unifications, constraints) = solver
        .resolve_unification_constraints(vec![Unify::new(
            inference,
            known.clone(),
        )])
        .await
        .unwrap();

    assert_eq!(substitution, Substitution::singleton(variable, known));
    assert_eq!(residual_unifications, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input: ?T1 == ?T2
// premise: {}
// output: {?T1 -> ?T2}, no residual unifications, {}
#[tokio::test]
async fn unifies_two_inference_variables() {
    let engine = create_test_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let left_variable = solver.fresh_inference_variable(TyKind::Type);
    let right_variable = solver.fresh_inference_variable(TyKind::Type);
    let left = Type2::new_inference_variable(left_variable, &engine);
    let right = Type2::new_inference_variable(right_variable, &engine);

    let (substitution, residual_unifications, constraints) = solver
        .resolve_unification_constraints(vec![Unify::new(left, right.clone())])
        .await
        .unwrap();

    assert_eq!(substitution, Substitution::singleton(left_variable, right));
    assert_eq!(residual_unifications, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input: solve ((?T) == (bool))
// premise: {}
// output: {?T -> bool}, {}
#[tokio::test]
async fn solve_unifies_application_arguments_eagerly() {
    let engine = create_test_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let variable = solver.fresh_inference_variable(TyKind::Type);
    let inference = Type2::new_inference_variable(variable, &engine);
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);

    let left = Type2::new_tuple([inference], &engine);
    let right = Type2::new_tuple([bool_type.clone()], &engine);

    let (substitution, residual_type_relations, constraints) = solver
        .resolve_type_relations(vec![TypeRelation::invariant(left, right)])
        .await
        .unwrap();

    assert_eq!(substitution, Substitution::singleton(variable, bool_type));
    assert_eq!(residual_type_relations, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input: ?T == (?T)
// premise: {}
// output: {}, residual ?T == (?T), {}
#[tokio::test]
async fn occur_check_failure_remains_residual() {
    let engine = create_test_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let variable = solver.fresh_inference_variable(TyKind::Type);
    let inference = Type2::new_inference_variable(variable, &engine);
    let recursive = Type2::new_tuple([inference.clone()], &engine);

    let (substitution, residual_unifications, constraints) = solver
        .resolve_unification_constraints(vec![Unify::new(
            inference.clone(),
            recursive,
        )])
        .await
        .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_unifications.len(), 1);
    assert_eq!(residual_unifications[0].left(), &inference);
    assert_eq!(constraints, Constraints::default());
}

// input: (&'static ?T) == (&'erased bool)
// premise: {}
// output: {?T -> bool}, no residual unifications, 'static = 'erased
#[tokio::test]
async fn tuple_and_reference_arguments_are_unified_invariantly() {
    let engine = create_test_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let variable = solver.fresh_inference_variable(TyKind::Type);
    let inference = Type2::new_inference_variable(variable, &engine);
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);
    let erased_lifetime = Type2::new_lifetime(Lifetime::Erased, &engine);

    let left = Type2::new_tuple(
        [Type2::new_reference(
            static_lifetime.clone(),
            inference,
            Mutability::Immutable,
            &engine,
        )],
        &engine,
    );
    let right = Type2::new_tuple(
        [Type2::new_reference(
            erased_lifetime.clone(),
            bool_type.clone(),
            Mutability::Immutable,
            &engine,
        )],
        &engine,
    );

    let (substitution, residual_unifications, constraints) = solver
        .resolve_unification_constraints(vec![Unify::new(left, right)])
        .await
        .unwrap();

    assert_eq!(substitution, Substitution::singleton(variable, bool_type));
    assert_eq!(residual_unifications, Vec::new());
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(static_lifetime, erased_lifetime)
    );
}

// input: ^0.0 == ^0.1
// premise: {}
// output: {}, residual ^0.0 == ^0.1, {}
#[tokio::test]
async fn bound_variables_must_match_exactly() {
    let engine = create_test_engine().await;
    let first = Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let second = Type2::new_bound_variable(BoundVariable::new(0, 1), &engine);

    let (substitution, residual_unifications, constraints) = resolve(
        vec![Unify::new(first.clone(), second.clone())],
        &Premise::default(),
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_unifications, vec![Unify::new(first, second)]);
    assert_eq!(constraints, Constraints::default());
}

// input: for<'a> fn() -> bool == fn() -> bool
// premise: {}
// output: {}, no residual unifications, {}
#[tokio::test]
async fn function_pointer_binders_are_related_by_type_relation() {
    let engine = create_test_engine().await;
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let left = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [],
        bool_type.clone(),
        &engine,
    );
    let right = Type2::new_function_pointer([], bool_type, &engine);

    let (substitution, residual_unifications, constraints) = resolve(
        vec![Unify::new(left.clone(), right.clone())],
        &Premise::default(),
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_unifications, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input: int32 == bool
// premise: int32 = bool
// output: {}, no residual unifications, {}
#[tokio::test]
async fn reduction_fallback_can_prove_equality() {
    let engine = create_test_engine().await;
    let mut premise = Premise::default();
    let int32 = Type2::new_primitive(Primitive::Int32, &engine);
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);

    premise.insert(equality(int32.clone(), bool_type.clone(), &engine));

    let (substitution, residual_unifications, constraints) =
        resolve(vec![Unify::new(int32, bool_type)], &premise, &engine)
            .await
            .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_unifications, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input: int32 == &'erased bool
// premise: int32 = &'static bool
// output: {}, no residual unifications, 'static = 'erased
#[tokio::test]
async fn reduction_fallback_accumulates_constraints() {
    let engine = create_test_engine().await;
    let mut premise = Premise::default();
    let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);
    let erased_lifetime = Type2::new_lifetime(Lifetime::Erased, &engine);
    let int32 = Type2::new_primitive(Primitive::Int32, &engine);
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let reduced = Type2::new_reference(
        static_lifetime.clone(),
        bool_type.clone(),
        Mutability::Immutable,
        &engine,
    );
    let subject = Type2::new_reference(
        erased_lifetime.clone(),
        bool_type.clone(),
        Mutability::Immutable,
        &engine,
    );

    premise.insert(equality(int32.clone(), reduced, &engine));

    let (substitution, residual_unifications, constraints) =
        resolve(vec![Unify::new(int32, subject)], &premise, &engine)
            .await
            .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_unifications, Vec::new());
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(static_lifetime, erased_lifetime)
    );
}
