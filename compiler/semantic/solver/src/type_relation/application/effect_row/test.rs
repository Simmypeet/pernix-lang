use std::sync::Arc;

use pernixc_qbice::{Config, Engine, InMemoryFactory, TrackedEngine};
use pernixc_symbol::{GlobalSymbolID, SymbolID, kind::Kind};
use pernixc_target::TargetID;
use pernixc_type::{
    predicate::{Equality, Predicate2},
    substitution::{Substitution, Variable},
    r#type::{
        Type2, bound::Binder, constructor::Primitive,
        inference::InferenceVariable, kind::TyKind, skolem::SkolemizedVariable,
        universe::UniverseIndex,
    },
    variance::Variance2,
};
use qbice::{
    executor, serialize::Plugin, stable_hash::SeededStableHasherBuilder,
    storage::intern::Interned,
};

use crate::{
    constraints::Constraints,
    premise::Premise,
    solver::{OverflowError, Solver},
    type_relation::TypeRelation,
};

const EFFECT_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(1));

struct EffectKindExecutor;

impl executor::Executor<pernixc_symbol::kind::Key, Config>
    for EffectKindExecutor
{
    async fn execute(
        &self,
        _: &pernixc_symbol::kind::Key,
        _: &TrackedEngine,
    ) -> Kind {
        Kind::Effect
    }
}

async fn create_engine() -> TrackedEngine {
    let mut engine = Engine::new_with(
        Plugin::default(),
        InMemoryFactory,
        SeededStableHasherBuilder::new(0),
    )
    .await
    .unwrap();

    engine.register_executor(Arc::new(EffectKindExecutor));

    Arc::new(engine).tracked().await
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

fn closed_row(
    slots: &[(&str, Interned<Type2>)],
    engine: &TrackedEngine,
) -> Interned<Type2> {
    row(slots, Type2::new_effect_row_empty(engine), engine)
}

fn row(
    slots: &[(&str, Interned<Type2>)],
    tail: Interned<Type2>,
    engine: &TrackedEngine,
) -> Interned<Type2> {
    slots.iter().rev().fold(tail, |tail, (label, signature)| {
        Type2::new_effect_row_extend(
            engine.intern_unsized((*label).to_owned()),
            signature.clone(),
            tail,
            engine,
        )
    })
}

async fn resolve_invariant(
    solver: &mut Solver<'_>,
    left: Interned<Type2>,
    right: Interned<Type2>,
) -> Result<Option<(Substitution, Constraints)>, OverflowError> {
    let (substitution, residual_relations, constraints) = solver
        .resolve_type_relations(vec![TypeRelation::invariant(left, right)])
        .await?;

    Ok(residual_relations.is_empty().then_some((substitution, constraints)))
}

async fn assert_rows_unify(
    left: Interned<Type2>,
    right: Interned<Type2>,
    engine: &TrackedEngine,
) {
    let premise = Premise::default();
    let result =
        resolve_invariant(&mut Solver::new(&premise, engine), left, right)
            .await
            .unwrap();

    assert_eq!(result, Some((Substitution::new(), Constraints::default())));
}

async fn assert_rows_do_not_unify(
    left: Interned<Type2>,
    right: Interned<Type2>,
    engine: &TrackedEngine,
) {
    let premise = Premise::default();
    let result =
        resolve_invariant(&mut Solver::new(&premise, engine), left, right)
            .await
            .unwrap();

    assert_eq!(result, None);
}

// input: {A: bool, B: int32} == {A: bool, B: int32}
// premise: {}
// output: {}, {}
#[tokio::test]
async fn identical_closed_rows_unify() {
    let engine = create_engine().await;
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let int32 = Type2::new_primitive(Primitive::Int32, &engine);
    let left =
        closed_row(&[("A", bool_type.clone()), ("B", int32.clone())], &engine);
    let right = closed_row(&[("A", bool_type), ("B", int32)], &engine);

    assert_rows_unify(left, right, &engine).await;
}

// input: {A: bool, B: int32} == {B: int32, A: bool}
// premise: distinct labels commute
// output: {}, {}
#[tokio::test]
async fn closed_rows_unify_when_labels_are_permuted() {
    let engine = create_engine().await;
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let int32 = Type2::new_primitive(Primitive::Int32, &engine);
    let left =
        closed_row(&[("A", bool_type.clone()), ("B", int32.clone())], &engine);
    let right = closed_row(&[("B", int32), ("A", bool_type)], &engine);

    assert_rows_unify(left, right, &engine).await;
}

// input: {A: bool, B: float32, A: int32} ==
//        {B: float32, A: bool, A: int32}
// premise: repeated labels pair in occurrence order
// output: {}, {}
#[tokio::test]
async fn interleaved_duplicate_labels_preserve_signature_order() {
    let engine = create_engine().await;
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let int32 = Type2::new_primitive(Primitive::Int32, &engine);
    let float32 = Type2::new_primitive(Primitive::Float32, &engine);
    let left = closed_row(
        &[
            ("A", bool_type.clone()),
            ("B", float32.clone()),
            ("A", int32.clone()),
        ],
        &engine,
    );
    let right =
        closed_row(&[("B", float32), ("A", bool_type), ("A", int32)], &engine);

    assert_rows_unify(left, right, &engine).await;
}

// input: {A: bool, A: int32} == {A: int32, A: bool}
// premise: repeated labels pair in occurrence order
// output: unification failure
#[tokio::test]
async fn reversing_duplicate_label_signatures_fails() {
    let engine = create_engine().await;
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let int32 = Type2::new_primitive(Primitive::Int32, &engine);
    let left =
        closed_row(&[("A", bool_type.clone()), ("A", int32.clone())], &engine);
    let right = closed_row(&[("A", int32), ("A", bool_type)], &engine);

    assert_rows_do_not_unify(left, right, &engine).await;
}

// input: {A: bool, A: bool} == {A: bool}
// premise: closed rows have no absorbent tail
// output: unification failure
#[tokio::test]
async fn different_label_multiplicities_fail() {
    let engine = create_engine().await;
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let left = closed_row(
        &[("A", bool_type.clone()), ("A", bool_type.clone())],
        &engine,
    );
    let right = closed_row(&[("A", bool_type)], &engine);

    assert_rows_do_not_unify(left, right, &engine).await;
}

// input: {A: bool} == {A: int32}
// premise: equal labels require equal signatures
// output: unification failure
#[tokio::test]
async fn equal_labels_with_inequivalent_signatures_fail() {
    let engine = create_engine().await;
    let left = closed_row(
        &[("A", Type2::new_primitive(Primitive::Bool, &engine))],
        &engine,
    );
    let right = closed_row(
        &[("A", Type2::new_primitive(Primitive::Int32, &engine))],
        &engine,
    );

    assert_rows_do_not_unify(left, right, &engine).await;
}

// input: {A: bool | ?E} == {B: int32, A: bool}
// premise: ?E is a bindable effect-row inference variable
// output: {?E -> {B: int32}}, {}
#[tokio::test]
async fn open_tail_absorbs_unmatched_closed_slots() {
    let engine = create_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let tail_variable = solver.fresh_inference_variable(TyKind::EffectRow);
    let tail = Type2::new_inference_variable(tail_variable, &engine);
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let int32 = Type2::new_primitive(Primitive::Int32, &engine);
    let left = row(&[("A", bool_type.clone())], tail, &engine);
    let expected_tail = closed_row(&[("B", int32.clone())], &engine);
    let right = closed_row(&[("B", int32), ("A", bool_type)], &engine);

    let result = resolve_invariant(&mut solver, left, right).await.unwrap();

    assert_eq!(
        result,
        Some((
            Substitution::singleton(tail_variable, expected_tail),
            Constraints::default()
        ))
    );
}

// input: {A: bool | ?E1@U1} == {B: int32 | ?E2@U0}
// premise: ?E1 and ?E2 are distinct and bindable
// output: both tails receive opposite prefixes over one fresh tail at U0
#[tokio::test]
async fn distinct_open_tails_share_a_fresh_lowest_universe_tail() {
    let engine = create_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let left_variable = InferenceVariable::new(
        100,
        TyKind::EffectRow,
        UniverseIndex::root().next(),
    );
    let right_variable =
        InferenceVariable::new(101, TyKind::EffectRow, UniverseIndex::root());
    let left_tail = Type2::new_inference_variable(left_variable, &engine);
    let right_tail = Type2::new_inference_variable(right_variable, &engine);
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let int32 = Type2::new_primitive(Primitive::Int32, &engine);
    let left = row(&[("A", bool_type.clone())], left_tail, &engine);
    let right = row(&[("B", int32.clone())], right_tail, &engine);

    let (substitution, constraints) =
        resolve_invariant(&mut solver, left, right)
            .await
            .unwrap()
            .expect("rows unify");

    assert_eq!(constraints, Constraints::default());
    let left_binding = substitution
        .iter()
        .find_map(|(variable, ty)| {
            (variable == Variable::Inference(left_variable)).then_some(ty)
        })
        .expect("left tail binding");
    let right_binding = substitution
        .iter()
        .find_map(|(variable, ty)| {
            (variable == Variable::Inference(right_variable)).then_some(ty)
        })
        .expect("right tail binding");
    let shared_tail = Type2::new_inference_variable(
        InferenceVariable::new(0, TyKind::EffectRow, UniverseIndex::root()),
        &engine,
    );

    assert_eq!(
        left_binding,
        &row(&[("B", int32)], shared_tail.clone(), &engine)
    );
    assert_eq!(right_binding, &row(&[("A", bool_type)], shared_tail, &engine));
}

// input: {A: bool | ?E} == {B: int32 | ?E}
// premise: both incompatible prefixes share the same tail
// output: unification failure without a recursive row binding
#[tokio::test]
async fn incompatible_rows_with_the_same_tail_terminate_and_fail() {
    let engine = create_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let tail_variable = solver.fresh_inference_variable(TyKind::EffectRow);
    let tail = Type2::new_inference_variable(tail_variable, &engine);
    let left = row(
        &[("A", Type2::new_primitive(Primitive::Bool, &engine))],
        tail.clone(),
        &engine,
    );
    let right = row(
        &[("B", Type2::new_primitive(Primitive::Int32, &engine))],
        tail,
        &engine,
    );

    let result = resolve_invariant(&mut solver, left, right).await.unwrap();

    assert_eq!(result, None);
}

// input: {A: Effect[?T]} == {A: Effect[bool]}
// premise: Effect is nominal and ?T is bindable
// output: {?T -> bool}, {}
#[tokio::test]
async fn parameterized_nominal_effect_signatures_unify_invariantly() {
    let engine = create_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let variable = solver.fresh_inference_variable(TyKind::Type);
    let inference = Type2::new_inference_variable(variable, &engine);
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let left_signature = Type2::new_symbolic(EFFECT_ID, [inference], &engine);
    let right_signature =
        Type2::new_symbolic(EFFECT_ID, [bool_type.clone()], &engine);
    let left = closed_row(&[("A", left_signature)], &engine);
    let right = closed_row(&[("A", right_signature)], &engine);

    let result = resolve_invariant(&mut solver, left, right).await.unwrap();

    assert_eq!(
        result,
        Some((
            Substitution::singleton(variable, bool_type),
            Constraints::default()
        ))
    );
}

// input: Effect[bool] R Effect[int32], R is bivariant
// premise: effect-signature arguments are always invariant
// output: the invariant bool = int32 argument relation remains unsolved
#[tokio::test]
async fn bivariant_effect_signature_relation_still_checks_arguments() {
    let engine = create_engine().await;
    let left = Type2::new_symbolic(
        EFFECT_ID,
        [Type2::new_primitive(Primitive::Bool, &engine)],
        &engine,
    );
    let right = Type2::new_symbolic(
        EFFECT_ID,
        [Type2::new_primitive(Primitive::Int32, &engine)],
        &engine,
    );
    let premise = Premise::default();

    let (substitution, residual, constraints) = Solver::new(&premise, &engine)
        .resolve_type_relations(vec![TypeRelation::new(
            left,
            right,
            Variance2::Bivariant,
        )])
        .await
        .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual.len(), 1);
    assert_eq!(residual[0].variance(), Variance2::Invariant);
    assert_eq!(constraints, Constraints::default());
}

// input: {A: bool} R {A: int32}, R in {+, -, *, =}
// premise: row equivalence ignores surrounding variance
// output: every relation remains unsolved
#[tokio::test]
async fn every_outer_variance_enforces_row_equivalence() {
    let engine = create_engine().await;

    for variance in [
        Variance2::Covariant,
        Variance2::Contravariant,
        Variance2::Bivariant,
        Variance2::Invariant,
    ] {
        let left = closed_row(
            &[("A", Type2::new_primitive(Primitive::Bool, &engine))],
            &engine,
        );
        let right = closed_row(
            &[("A", Type2::new_primitive(Primitive::Int32, &engine))],
            &engine,
        );
        let premise = Premise::default();
        let (substitution, residual, constraints) =
            Solver::new(&premise, &engine)
                .resolve_type_relations(vec![TypeRelation::new(
                    left, right, variance,
                )])
                .await
                .unwrap();

        assert_eq!(substitution, Substitution::new());
        assert_eq!(residual.len(), 1);
        assert_eq!(residual[0].variance(), Variance2::Invariant);
        assert_eq!(constraints, Constraints::default());
    }
}

// input: {?E | A: bool} = {A: bool, B: int32}
// premise: ?E is rigid on the lesser side
// output: no tail binding; original relation remains unsolved
#[tokio::test]
async fn rigid_inference_tail_is_not_bound_by_row_matching() {
    let engine = create_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let variable = solver.fresh_inference_variable(TyKind::EffectRow);
    let tail = Type2::new_inference_variable(variable, &engine);
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let int32 = Type2::new_primitive(Primitive::Int32, &engine);
    let left = row(&[("A", bool_type.clone())], tail, &engine);
    let right = closed_row(&[("A", bool_type), ("B", int32)], &engine);

    let (substitution, residual, constraints) = solver
        .resolve_type_relations(vec![TypeRelation::new_with_rigidity(
            left.clone(),
            right.clone(),
            Variance2::Invariant,
            true,
            false,
        )])
        .await
        .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(&*residual, &[TypeRelation::new_with_rigidity(
        left,
        right,
        Variance2::Invariant,
        true,
        false,
    )]);
    assert_eq!(constraints, Constraints::default());
}

// input: {!E | A: bool} = {A: bool, B: int32}
// premise: !E is an opaque skolem effect-row tail
// output: no decomposition; original relation remains unsolved
#[tokio::test]
async fn opaque_tail_preserves_the_original_relation() {
    let engine = create_engine().await;
    let opaque = Type2::new_skolemized_variable(
        SkolemizedVariable::new(0, TyKind::EffectRow, UniverseIndex::root()),
        &engine,
    );
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let int32 = Type2::new_primitive(Primitive::Int32, &engine);
    let left = row(&[("A", bool_type.clone())], opaque, &engine);
    let right = closed_row(&[("A", bool_type), ("B", int32)], &engine);
    let original = TypeRelation::invariant(left, right);
    let premise = Premise::default();

    let (substitution, residual, constraints) = Solver::new(&premise, &engine)
        .resolve_type_relations(vec![original.clone()])
        .await
        .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(&*residual, &[original]);
    assert_eq!(constraints, Constraints::default());
}

// input: {A: bool | !E} == {B: int32, A: bool}
// premise: !E = {B: int32}
// output: {}, {}
#[tokio::test]
async fn failed_row_decomposition_falls_back_to_reduction() {
    let engine = create_engine().await;
    let opaque = Type2::new_skolemized_variable(
        SkolemizedVariable::new(0, TyKind::EffectRow, UniverseIndex::root()),
        &engine,
    );
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let int32 = Type2::new_primitive(Primitive::Int32, &engine);
    let reduced_tail = closed_row(&[("B", int32.clone())], &engine);
    let left = row(&[("A", bool_type.clone())], opaque.clone(), &engine);
    let right = closed_row(&[("B", int32), ("A", bool_type)], &engine);
    let mut premise = Premise::default();
    premise.insert(equality(opaque, reduced_tail, &engine));

    let result =
        resolve_invariant(&mut Solver::new(&premise, &engine), left, right)
            .await
            .unwrap();

    assert_eq!(result, Some((Substitution::new(), Constraints::default())));
}
