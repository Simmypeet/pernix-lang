use std::sync::Arc;

use pernixc_qbice::{
    Config, Engine, InMemoryFactory, TrackedEngine,
    create_minimal_engine as create_engine,
};
use pernixc_symbol::{GlobalSymbolID, kind::Kind};
use pernixc_type::{
    predicate::{Equality, Predicate2, Subtype},
    substitution::Substitution,
    r#type::{
        Type2,
        bound::{Binder, BoundVariable},
        constructor::{Lifetime, Mutability, Primitive},
        kind::TyKind,
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
    type_relation::{Step, TypeRelation},
};

struct AssociatedTypeKindExecutor;

impl executor::Executor<pernixc_symbol::kind::Key, Config>
    for AssociatedTypeKindExecutor
{
    async fn execute(
        &self,
        _: &pernixc_symbol::kind::Key,
        _: &TrackedEngine,
    ) -> Kind {
        Kind::InstanceAssociatedType
    }
}

async fn create_engine_with_associated_type_kind() -> TrackedEngine {
    let mut engine = Engine::new_with(
        Plugin::default(),
        InMemoryFactory,
        SeededStableHasherBuilder::new(0),
    )
    .await
    .unwrap();

    engine.register_executor(Arc::new(AssociatedTypeKindExecutor));

    Arc::new(engine).tracked().await
}

async fn destructure_application(
    lesser: &Interned<Type2>,
    greater: &Interned<Type2>,
    engine: &TrackedEngine,
) -> Result<Option<Step>, OverflowError> {
    let Type2::Application(lesser_application) = &**lesser else {
        panic!("expected application");
    };
    let Type2::Application(greater_application) = &**greater else {
        panic!("expected application");
    };

    let relation = TypeRelation::new(
        lesser.clone(),
        greater.clone(),
        Variance2::Covariant,
    );

    Solver::new(&Premise::default(), engine)
        .handle_application(&relation, lesser_application, greater_application)
        .await
}

async fn resolve_one(
    lesser: Interned<Type2>,
    greater: Interned<Type2>,
    variance: Variance2,
    engine: &TrackedEngine,
) -> Result<Constraints, OverflowError> {
    let (substitution, residual_subtypes, constraints) =
        Solver::new(&Premise::default(), engine)
            .resolve_subtypes(vec![Subtype::new(lesser, greater, variance)])
            .await?;

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_subtypes, Vec::new());

    Ok(constraints)
}

async fn resolve_step(
    lesser: Interned<Type2>,
    greater: Interned<Type2>,
    variance: Variance2,
    engine: &TrackedEngine,
) -> Result<Step, OverflowError> {
    Solver::new(&Premise::default(), engine)
        .resolve_type_relations(vec![TypeRelation::new(
            lesser, greater, variance,
        )])
        .await
}

fn contains_variable(ty: &Interned<Type2>) -> bool {
    match &**ty {
        Type2::InferenceVariable(_) | Type2::SkolemizedVariable(_) => true,
        Type2::Application(application) => {
            application.arguments().iter().any(contains_variable)
        }
        Type2::GenericParameter(_) | Type2::BoundVariable(_) => false,
    }
}

fn assert_no_variables_in_step(
    substitution: &Substitution,
    residual_subtypes: &[TypeRelation],
    constraints: &Constraints,
) {
    assert!(substitution.iter().all(|(_, ty)| !contains_variable(ty)));
    assert!(residual_subtypes.iter().all(|subtype| {
        !contains_variable(subtype.lesser())
            && !contains_variable(subtype.greater())
    }));
    assert!(constraints.clone().into_iter().all(|constraint| {
        !contains_variable(constraint.lesser())
            && !contains_variable(constraint.greater())
    }));
}

// input: ?a = bool with both sides rigid
// premise: ?a is on the lesser side
// output: no ?a := bool substitution; relation remains stuck
#[tokio::test]
async fn rigid_inference_does_not_bind_lesser_inference_variable() {
    let engine = create_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let variable = solver.fresh_inference_variable(TyKind::Type);
    let inference = Type2::new_inference_variable(variable, &engine);
    let known = Type2::new_primitive(Primitive::Bool, &engine);

    let (substitution, residual_subtypes, constraints) = solver
        .resolve_type_relations(vec![TypeRelation::new_rigid(
            inference.clone(),
            known,
            Variance2::Invariant,
        )])
        .await
        .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_subtypes.len(), 1);
    assert_eq!(residual_subtypes[0].left(), &inference);
    assert_eq!(constraints, Constraints::default());
}

// input: bool = ?a with only the lesser side rigid
// premise: ?a is on the non-rigid greater side
// output: ?a := bool
#[tokio::test]
async fn lesser_rigidity_does_not_block_greater_inference_variable() {
    let engine = create_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let variable = solver.fresh_inference_variable(TyKind::Type);
    let inference = Type2::new_inference_variable(variable, &engine);
    let known = Type2::new_primitive(Primitive::Bool, &engine);

    let (substitution, residual_subtypes, constraints) = solver
        .resolve_type_relations(vec![TypeRelation::new_with_rigidity(
            known.clone(),
            inference,
            Variance2::Invariant,
            true,
            false,
        )])
        .await
        .unwrap();

    assert_eq!(substitution, Substitution::singleton(variable, known));
    assert!(residual_subtypes.is_empty());
    assert_eq!(constraints, Constraints::default());
}

async fn resolve(
    type_relations: Vec<TypeRelation>,
    premise: &Premise,
    engine: &TrackedEngine,
) -> Result<(Substitution, Vec<TypeRelation>, Constraints), OverflowError> {
    resolve_with(&mut Solver::new(premise, engine), type_relations).await
}

async fn resolve_with(
    solver: &mut Solver<'_>,
    type_relations: Vec<TypeRelation>,
) -> Result<(Substitution, Vec<TypeRelation>, Constraints), OverflowError> {
    let (substitution, residual_relations, constraints) =
        solver.resolve_type_relations(type_relations).await?;

    Ok((
        substitution,
        residual_relations.iter().cloned().collect(),
        constraints,
    ))
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
// output: {}, no residual relations, {}
#[tokio::test]
async fn syntactic_equality_succeeds_without_work() {
    let engine = create_engine().await;
    let ty = Type2::new_primitive(Primitive::Bool, &engine);

    let (substitution, residual_relations, constraints) = resolve(
        vec![TypeRelation::invariant(ty.clone(), ty)],
        &Premise::default(),
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_relations, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input: bool == int32
// premise: {}
// output: {}, residual bool == int32, {}
#[tokio::test]
async fn primitive_mismatch_remains_residual() {
    let engine = create_engine().await;
    let left = Type2::new_primitive(Primitive::Bool, &engine);
    let right = Type2::new_primitive(Primitive::Int32, &engine);

    let (substitution, residual_relations, constraints) = resolve(
        vec![TypeRelation::invariant(left.clone(), right.clone())],
        &Premise::default(),
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_relations, vec![TypeRelation::invariant(left, right)]);
    assert_eq!(constraints, Constraints::default());
}

// input: (bool) == (int32)
// premise: {}
// output: {}, residual bool == int32, {}
#[tokio::test]
async fn tuple_mismatch_keeps_decomposed_residual() {
    let engine = create_engine().await;
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let int32 = Type2::new_primitive(Primitive::Int32, &engine);
    let left = Type2::new_tuple([bool_type.clone()], &engine);
    let right = Type2::new_tuple([int32.clone()], &engine);

    let (substitution, residual_relations, constraints) = resolve(
        vec![TypeRelation::invariant(left.clone(), right.clone())],
        &Premise::default(),
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_relations, vec![TypeRelation::invariant(
        bool_type, int32
    )]);
    assert_eq!(constraints, Constraints::default());
}

// input: 'static == 'erased
// premise: {}
// output: {}, no residual relations, 'static = 'erased
#[tokio::test]
async fn lifetime_mismatch_emits_equality_constraints() {
    let engine = create_engine().await;
    let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);
    let erased_lifetime = Type2::new_lifetime(Lifetime::Erased, &engine);

    let (substitution, residual_relations, constraints) = resolve(
        vec![TypeRelation::invariant(
            static_lifetime.clone(),
            erased_lifetime.clone(),
        )],
        &Premise::default(),
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_relations, Vec::new());
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(static_lifetime, erased_lifetime)
    );
}

// input: ?T == bool
// premise: {}
// output: {?T -> bool}, no residual relations, {}
#[tokio::test]
async fn unifies_inference_variable_with_known_type() {
    let engine = create_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let variable = solver.fresh_inference_variable(TyKind::Type);
    let inference = Type2::new_inference_variable(variable, &engine);
    let known = Type2::new_primitive(Primitive::Bool, &engine);

    let (substitution, residual_relations, constraints) = resolve_with(
        &mut solver,
        vec![TypeRelation::invariant(inference, known.clone())],
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::singleton(variable, known));
    assert_eq!(residual_relations, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input: ?lt == 'static
// premise: {}
// output: {?lt -> 'static}, no residual relations, {}
#[tokio::test]
async fn unifies_lifetime_inference_variable() {
    let engine = create_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let variable = solver.fresh_inference_variable(TyKind::Lifetime);
    let inference = Type2::new_inference_variable(variable, &engine);
    let known = Type2::new_lifetime(Lifetime::Static, &engine);

    let (substitution, residual_relations, constraints) = resolve_with(
        &mut solver,
        vec![TypeRelation::invariant(inference, known.clone())],
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::singleton(variable, known));
    assert_eq!(residual_relations, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input: ?T1 == ?T2
// premise: {}
// output: {?T1 -> ?T2}, no residual relations, {}
#[tokio::test]
async fn unifies_two_inference_variables() {
    let engine = create_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let left_variable = solver.fresh_inference_variable(TyKind::Type);
    let right_variable = solver.fresh_inference_variable(TyKind::Type);
    let left = Type2::new_inference_variable(left_variable, &engine);
    let right = Type2::new_inference_variable(right_variable, &engine);

    let (substitution, residual_relations, constraints) = resolve_with(
        &mut solver,
        vec![TypeRelation::invariant(left, right.clone())],
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::singleton(left_variable, right));
    assert_eq!(residual_relations, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input: solve ((?T) == (bool))
// premise: {}
// output: {?T -> bool}, {}
#[tokio::test]
async fn solve_unifies_application_arguments_eagerly() {
    let engine = create_engine().await;
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
    assert!(residual_type_relations.is_empty());
    assert_eq!(constraints, Constraints::default());
}

// input: ?T == (?T)
// premise: {}
// output: {}, residual ?T == (?T), {}
#[tokio::test]
async fn occur_check_failure_remains_residual() {
    let engine = create_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let variable = solver.fresh_inference_variable(TyKind::Type);
    let inference = Type2::new_inference_variable(variable, &engine);
    let recursive = Type2::new_tuple([inference.clone()], &engine);

    let (substitution, residual_relations, constraints) = resolve_with(
        &mut solver,
        vec![TypeRelation::invariant(inference.clone(), recursive)],
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_relations.len(), 1);
    assert_eq!(residual_relations[0].left(), &inference);
    assert_eq!(constraints, Constraints::default());
}

// input: (&'static ?T) == (&'erased bool)
// premise: {}
// output: {?T -> bool}, no residual relations, 'static = 'erased
#[tokio::test]
async fn tuple_and_reference_arguments_are_unified_invariantly() {
    let engine = create_engine().await;
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

    let (substitution, residual_relations, constraints) =
        resolve_with(&mut solver, vec![TypeRelation::invariant(left, right)])
            .await
            .unwrap();

    assert_eq!(substitution, Substitution::singleton(variable, bool_type));
    assert_eq!(residual_relations, Vec::new());
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(static_lifetime, erased_lifetime)
    );
}

// input: for<'a> fn() -> bool == fn() -> bool
// premise: {}
// output: {}, no residual relations, {}
#[tokio::test]
async fn function_pointer_binders_are_related_by_type_relation() {
    let engine = create_engine().await;
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
    let left = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [],
        bool_type.clone(),
        &engine,
    );
    let right = Type2::new_function_pointer([], bool_type, &engine);

    let (substitution, residual_relations, constraints) = resolve(
        vec![TypeRelation::invariant(left.clone(), right.clone())],
        &Premise::default(),
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_relations, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input: int32 == bool
// premise: int32 = bool
// output: {}, no residual relations, {}
#[tokio::test]
async fn reduction_fallback_can_prove_equality() {
    let engine = create_engine().await;
    let mut premise = Premise::default();
    let int32 = Type2::new_primitive(Primitive::Int32, &engine);
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);

    premise.insert(equality(int32.clone(), bool_type.clone(), &engine));

    let (substitution, residual_relations, constraints) = resolve(
        vec![TypeRelation::invariant(int32, bool_type)],
        &premise,
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_relations, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input: int32 == &'erased bool
// premise: int32 = &'static bool
// output: {}, no residual relations, 'static = 'erased
#[tokio::test]
async fn reduction_fallback_accumulates_constraints() {
    let engine = create_engine().await;
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

    let (substitution, residual_relations, constraints) = resolve(
        vec![TypeRelation::invariant(int32, subject)],
        &premise,
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_relations, Vec::new());
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(static_lifetime, erased_lifetime)
    );
}

// input: ?a = bool with only the greater side rigid
// premise: ?a is on the non-rigid lesser side
// output: ?a := bool
#[tokio::test]
async fn greater_rigidity_does_not_block_lesser_inference_variable() {
    let engine = create_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let variable = solver.fresh_inference_variable(TyKind::Type);
    let inference = Type2::new_inference_variable(variable, &engine);
    let known = Type2::new_primitive(Primitive::Bool, &engine);

    let (substitution, residual_subtypes, constraints) = solver
        .resolve_type_relations(vec![TypeRelation::new_with_rigidity(
            inference,
            known.clone(),
            Variance2::Invariant,
            false,
            true,
        )])
        .await
        .unwrap();

    assert_eq!(substitution, Substitution::singleton(variable, known));
    assert!(residual_subtypes.is_empty());
    assert_eq!(constraints, Constraints::default());
}

#[tokio::test]
async fn instance_associated_arguments_must_be_solved_immediately() {
    let engine = create_engine().await;
    let common_instance = Type2::new_primitive(Primitive::Bool, &engine);
    let lesser = Type2::new_instance_associated(
        GlobalSymbolID::default(),
        common_instance.clone(),
        [Type2::new_primitive(Primitive::Int32, &engine)],
        &engine,
    );
    let greater = Type2::new_instance_associated(
        GlobalSymbolID::default(),
        common_instance,
        [Type2::new_primitive(Primitive::Float32, &engine)],
        &engine,
    );

    assert_eq!(
        destructure_application(&lesser, &greater, &engine).await.unwrap(),
        None
    );
}

#[tokio::test]
async fn instance_associated_arguments_do_not_bind_inference_variables() {
    let engine = create_engine().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let variable = solver.fresh_inference_variable(TyKind::Type);
    let inference = Type2::new_inference_variable(variable, &engine);
    let common_instance = Type2::new_primitive(Primitive::Bool, &engine);
    let lesser = Type2::new_instance_associated(
        GlobalSymbolID::default(),
        common_instance.clone(),
        [inference],
        &engine,
    );
    let greater = Type2::new_instance_associated(
        GlobalSymbolID::default(),
        common_instance,
        [Type2::new_primitive(Primitive::Int32, &engine)],
        &engine,
    );

    let Type2::Application(lesser_application) = &*lesser else {
        panic!("expected application");
    };
    let Type2::Application(greater_application) = &*greater else {
        panic!("expected application");
    };

    let relation = TypeRelation::new(
        lesser.clone(),
        greater.clone(),
        Variance2::Covariant,
    );
    let step = solver
        .handle_application(&relation, lesser_application, greater_application)
        .await
        .unwrap();

    assert_eq!(step, None);
}

// input: I::Assoc[?a] = I::Assoc[int32]
// premise: instance-associated arguments are destructured rigidly
// output: no ?a := int32 substitution; original relation remains stuck
#[tokio::test]
async fn instance_associated_equality_does_not_unify_inference_arguments() {
    let engine = create_engine_with_associated_type_kind().await;
    let premise = Premise::default();
    let mut solver = Solver::new(&premise, &engine);
    let variable = solver.fresh_inference_variable(TyKind::Type);
    let inference = Type2::new_inference_variable(variable, &engine);
    let common_instance = Type2::new_primitive(Primitive::Bool, &engine);
    let lesser = Type2::new_instance_associated(
        GlobalSymbolID::default(),
        common_instance.clone(),
        [inference],
        &engine,
    );
    let greater = Type2::new_instance_associated(
        GlobalSymbolID::default(),
        common_instance,
        [Type2::new_primitive(Primitive::Int32, &engine)],
        &engine,
    );

    let (substitution, residual_subtypes, constraints) = solver
        .resolve_type_relations(vec![TypeRelation::new(
            lesser.clone(),
            greater.clone(),
            Variance2::Invariant,
        )])
        .await
        .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(&*residual_subtypes, &[TypeRelation::new(
        lesser,
        greater,
        Variance2::Invariant
    )]);
    assert_eq!(constraints, Constraints::default());
}

#[tokio::test]
async fn solved_instance_associated_arguments_are_not_deferred() {
    let engine = create_engine().await;
    let common_instance = Type2::new_primitive(Primitive::Bool, &engine);
    let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);
    let erased_lifetime = Type2::new_lifetime(Lifetime::Erased, &engine);
    let lesser = Type2::new_instance_associated(
        GlobalSymbolID::default(),
        common_instance.clone(),
        [static_lifetime.clone()],
        &engine,
    );
    let greater = Type2::new_instance_associated(
        GlobalSymbolID::default(),
        common_instance,
        [erased_lifetime.clone()],
        &engine,
    );

    let (substitution, residual_subtypes, constraints) =
        destructure_application(&lesser, &greater, &engine)
            .await
            .unwrap()
            .expect("arguments should solve immediately");

    assert_eq!(substitution, Substitution::new());
    assert!(residual_subtypes.is_empty());
    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(static_lifetime, erased_lifetime)
    );
}

// input: ('static) <: ('erased) @ Covariant
// premise: {}
// output: 'static: 'erased
#[tokio::test]
async fn tuple_arguments_follow_parent_covariance() {
    let engine = create_engine().await;
    let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);
    let erased_lifetime = Type2::new_lifetime(Lifetime::Erased, &engine);

    let constraints = resolve_one(
        Type2::new_tuple(vec![static_lifetime.clone()], &engine),
        Type2::new_tuple(vec![erased_lifetime.clone()], &engine),
        Variance2::Covariant,
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(
        constraints,
        Constraints::lifetimes_outlives(static_lifetime, erased_lifetime)
    );
}

// input: ('static) <: ('erased) @ Contravariant
// premise: {}
// output: 'erased: 'static
#[tokio::test]
async fn tuple_arguments_flip_under_parent_contravariance() {
    let engine = create_engine().await;
    let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);
    let erased_lifetime = Type2::new_lifetime(Lifetime::Erased, &engine);

    let constraints = resolve_one(
        Type2::new_tuple(vec![static_lifetime.clone()], &engine),
        Type2::new_tuple(vec![erased_lifetime.clone()], &engine),
        Variance2::Contravariant,
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(
        constraints,
        Constraints::lifetimes_outlives(erased_lifetime, static_lifetime)
    );
}

// input: ('static) <: ('erased) @ Invariant
// premise: {}
// output: 'static = 'erased
#[tokio::test]
async fn tuple_arguments_become_equal_under_parent_invariance() {
    let engine = create_engine().await;
    let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);
    let erased_lifetime = Type2::new_lifetime(Lifetime::Erased, &engine);

    let constraints = resolve_one(
        Type2::new_tuple(vec![static_lifetime.clone()], &engine),
        Type2::new_tuple(vec![erased_lifetime.clone()], &engine),
        Variance2::Invariant,
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(static_lifetime, erased_lifetime)
    );
}

// input: ('static) <: ('erased) @ Bivariant
// premise: {}
// output: {}
#[tokio::test]
async fn tuple_arguments_are_ignored_under_parent_bivariance() {
    let engine = create_engine().await;

    let constraints = resolve_one(
        Type2::new_tuple(
            vec![Type2::new_lifetime(Lifetime::Static, &engine)],
            &engine,
        ),
        Type2::new_tuple(
            vec![Type2::new_lifetime(Lifetime::Erased, &engine)],
            &engine,
        ),
        Variance2::Bivariant,
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(constraints, Constraints::default());
}

// input: &mut &'static bool <: &mut &'erased bool @ Covariant
// premise: {}
// output: 'static = 'erased
#[tokio::test]
async fn mutable_reference_pointees_are_invariant() {
    let engine = create_engine().await;
    let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);
    let erased_lifetime = Type2::new_lifetime(Lifetime::Erased, &engine);
    let common_reference_lifetime =
        Type2::new_lifetime(Lifetime::Static, &engine);
    let bool_type = Type2::new_primitive(Primitive::Bool, &engine);

    let constraints = resolve_one(
        Type2::new_reference(
            common_reference_lifetime.clone(),
            Type2::new_reference(
                static_lifetime.clone(),
                bool_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            Mutability::Mutable,
            &engine,
        ),
        Type2::new_reference(
            common_reference_lifetime,
            Type2::new_reference(
                erased_lifetime.clone(),
                bool_type,
                Mutability::Immutable,
                &engine,
            ),
            Mutability::Mutable,
            &engine,
        ),
        Variance2::Covariant,
        &engine,
    )
    .await
    .unwrap();

    assert_eq!(
        constraints,
        Constraints::lifetimes_eq(static_lifetime, erased_lifetime)
    );
}

// input:
// for<'a> fn(&'a u32, &'a u32) -> () <:
// for<'b, 'c> fn(&'b u32, &'c u32) -> () @ Covariant
// premise: {}
// output: {}
#[tokio::test]
async fn higher_ranked_lifetime_arguments_can_split() {
    let engine = create_engine().await;
    let u32_type = Type2::new_primitive(Primitive::Uint32, &engine);
    let lhs_lifetime =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let rhs_first_lifetime =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let rhs_second_lifetime =
        Type2::new_bound_variable(BoundVariable::new(0, 1), &engine);

    let lesser = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [
            Type2::new_reference(
                lhs_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            Type2::new_reference(
                lhs_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
        ],
        Type2::new_tuple([], &engine),
        &engine,
    );
    let greater = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        2,
        [
            Type2::new_reference(
                rhs_first_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            Type2::new_reference(
                rhs_second_lifetime,
                u32_type,
                Mutability::Immutable,
                &engine,
            ),
        ],
        Type2::new_tuple([], &engine),
        &engine,
    );

    let (substitution, residual_subtypes, constraints) =
        resolve_step(lesser, greater, Variance2::Covariant, &engine)
            .await
            .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert!(residual_subtypes.is_empty());
    assert_eq!(constraints, Constraints::default());
}

// input:
// for<'a> fn(&'a u32, &'a u32) -> &'a u32 <:
// for<'b, 'c> fn(&'b u32, &'c u32) -> &'b u32 @ Covariant
// premise: {}
// output: stuck subtype problem
#[tokio::test]
async fn higher_ranked_lifetime_return_cannot_split_argument_identity() {
    let engine = create_engine().await;
    let u32_type = Type2::new_primitive(Primitive::Uint32, &engine);
    let lhs_lifetime =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let rhs_first_lifetime =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let rhs_second_lifetime =
        Type2::new_bound_variable(BoundVariable::new(0, 1), &engine);

    let lesser = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [
            Type2::new_reference(
                lhs_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            Type2::new_reference(
                lhs_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
        ],
        Type2::new_reference(
            lhs_lifetime,
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );
    let greater = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        2,
        [
            Type2::new_reference(
                rhs_first_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            Type2::new_reference(
                rhs_second_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
        ],
        Type2::new_reference(
            rhs_first_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );

    let (_, residual_subtypes, _) =
        resolve_step(lesser, greater, Variance2::Covariant, &engine)
            .await
            .unwrap();

    assert!(!residual_subtypes.is_empty());
}

// input: for<'a> fn(&'a u32) -> () <: fn(&'static u32) -> () @ Covariant
// premise: {}
// output: {}
#[tokio::test]
async fn mixed_ranked_and_unranked_function_pointers_destructure() {
    let engine = create_engine().await;
    let u32_type = Type2::new_primitive(Primitive::Uint32, &engine);
    let ranked_lifetime =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);

    let lesser = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type2::new_reference(
            ranked_lifetime,
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        )],
        Type2::new_tuple([], &engine),
        &engine,
    );
    let greater = Type2::new_function_pointer(
        [Type2::new_reference(
            static_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        )],
        Type2::new_tuple([], &engine),
        &engine,
    );

    let (_, residual_subtypes, _) =
        resolve_step(lesser, greater, Variance2::Covariant, &engine)
            .await
            .unwrap();

    assert!(residual_subtypes.is_empty());
}

// input: fn(&'static u32) -> () <: for<'a> fn(&'a u32) -> () @ Covariant
// premise: {}
// output: stuck subtype problem
#[tokio::test]
async fn covariant_hrtb_rejects_skolem_to_external_leak() {
    let engine = create_engine().await;
    let u32_type = Type2::new_primitive(Primitive::Uint32, &engine);
    let ranked_lifetime =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);

    let lesser = Type2::new_function_pointer(
        [Type2::new_reference(
            static_lifetime,
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        )],
        Type2::new_tuple([], &engine),
        &engine,
    );
    let greater = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type2::new_reference(
            ranked_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        )],
        Type2::new_tuple([], &engine),
        &engine,
    );

    let (_, residual_subtypes, _) =
        resolve_step(lesser, greater, Variance2::Covariant, &engine)
            .await
            .unwrap();

    assert!(!residual_subtypes.is_empty());
}

// input:
// fn(&'static u32) -> () <: for<'a> fn(&'a u32) -> () @ Contravariant
// premise: {}
// output: {}
#[tokio::test]
async fn contravariant_top_level_variance_flips_hrtb_sides() {
    let engine = create_engine().await;
    let u32_type = Type2::new_primitive(Primitive::Uint32, &engine);
    let ranked_lifetime =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);

    let lesser = Type2::new_function_pointer(
        [Type2::new_reference(
            static_lifetime,
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        )],
        Type2::new_tuple([], &engine),
        &engine,
    );
    let greater = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type2::new_reference(
            ranked_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        )],
        Type2::new_tuple([], &engine),
        &engine,
    );

    let (_, residual_subtypes, _) =
        resolve_step(lesser, greater, Variance2::Contravariant, &engine)
            .await
            .unwrap();

    assert!(residual_subtypes.is_empty());
}

// input:
// for<'a> fn(&'a u32) -> &'a u32 <:
// for<'b> fn(&'b u32) -> &'b u32 @ Invariant
// premise: {}
// output: {}
#[tokio::test]
async fn invariant_hrtb_uses_independent_directional_runs() {
    let engine = create_engine().await;
    let u32_type = Type2::new_primitive(Primitive::Uint32, &engine);
    let lhs_lifetime =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let rhs_lifetime =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);

    let lesser = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type2::new_reference(
            lhs_lifetime.clone(),
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        )],
        Type2::new_reference(
            lhs_lifetime,
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );
    let greater = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type2::new_reference(
            rhs_lifetime.clone(),
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        )],
        Type2::new_reference(
            rhs_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );

    let (substitution, residual_subtypes, constraints) =
        resolve_step(lesser, greater, Variance2::Invariant, &engine)
            .await
            .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert!(residual_subtypes.is_empty());
    assert_eq!(constraints, Constraints::default());
}

// input: fn() -> &'static u32 <: for<'a> fn() -> &'a u32 @ Covariant
// premise: {}
// output: no inference or skolem variables in the returned step
#[tokio::test]
async fn hrtb_step_does_not_expose_internal_variables() {
    let engine = create_engine().await;
    let u32_type = Type2::new_primitive(Primitive::Uint32, &engine);
    let ranked_lifetime =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);

    let lesser = Type2::new_function_pointer(
        [Type2::new_tuple([], &engine)],
        Type2::new_reference(
            static_lifetime,
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );
    let greater = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type2::new_tuple([], &engine)],
        Type2::new_reference(
            ranked_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );

    let (substitution, residual_subtypes, constraints) =
        resolve_step(lesser, greater, Variance2::Covariant, &engine)
            .await
            .unwrap();

    assert_no_variables_in_step(
        &substitution,
        &residual_subtypes,
        &constraints,
    );
}

// input: fn() -> &'static u32 <: for<'a> fn() -> &'a u32 @ Covariant
// premise: {}
// output: 'static: 'static
#[tokio::test]
async fn external_to_skolem_return_obligation_rewrites_to_static() {
    let engine = create_engine().await;
    let u32_type = Type2::new_primitive(Primitive::Uint32, &engine);
    let ranked_lifetime =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let static_lifetime = Type2::new_lifetime(Lifetime::Static, &engine);

    let lesser = Type2::new_function_pointer(
        [Type2::new_tuple([], &engine)],
        Type2::new_reference(
            static_lifetime.clone(),
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );
    let greater = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type2::new_tuple([], &engine)],
        Type2::new_reference(
            ranked_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );

    let (_, residual_subtypes, constraints) =
        resolve_step(lesser, greater, Variance2::Covariant, &engine)
            .await
            .unwrap();

    assert!(residual_subtypes.is_empty());
    assert_eq!(
        constraints,
        Constraints::lifetimes_outlives(
            static_lifetime.clone(),
            static_lifetime
        )
    );
}

// input:
// for<'a> fn(&'a u32) -> &'a u32 <:
// for<'b> fn(&'b u32) -> &'b u32 @ Bivariant
// premise: {}
// output: {}
#[tokio::test]
async fn bivariant_hrtb_function_pointers_do_not_emit_work() {
    let engine = create_engine().await;
    let u32_type = Type2::new_primitive(Primitive::Uint32, &engine);
    let lhs_lifetime =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let rhs_lifetime =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);

    let lesser = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type2::new_reference(
            lhs_lifetime.clone(),
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        )],
        Type2::new_reference(
            lhs_lifetime,
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );
    let greater = Type2::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type2::new_reference(
            rhs_lifetime.clone(),
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        )],
        Type2::new_reference(
            rhs_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );

    let (substitution, residual_subtypes, constraints) =
        resolve_step(lesser, greater, Variance2::Bivariant, &engine)
            .await
            .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert!(residual_subtypes.is_empty());
    assert_eq!(constraints, Constraints::default());
}
