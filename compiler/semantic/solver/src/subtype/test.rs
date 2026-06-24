use pernixc_qbice::{TrackedEngine, create_minimal_engine as create_engine};
use pernixc_symbol::GlobalSymbolID;
use pernixc_type::{
    predicate::Subtype,
    substitution::Substitution,
    r#type::{
        Type,
        bound::BoundVariable,
        constructor::{Lifetime, Mutability, Primitive},
    },
    variance::Variance,
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    premise::Premise,
    solver::{OverflowError, Solver},
    subtype::Step,
};

async fn destructure_application(
    lesser: &Interned<Type>,
    greater: &Interned<Type>,
    engine: &TrackedEngine,
) -> Result<Option<Step>, OverflowError> {
    let Type::Application(lesser_application) = &**lesser else {
        panic!("expected application");
    };
    let Type::Application(greater_application) = &**greater else {
        panic!("expected application");
    };

    Solver::new(&Premise::default(), engine)
        .handle_application(
            lesser,
            greater,
            lesser_application,
            greater_application,
            Variance::Covariant,
        )
        .await
}

async fn resolve_one(
    lesser: Interned<Type>,
    greater: Interned<Type>,
    variance: Variance,
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
    lesser: Interned<Type>,
    greater: Interned<Type>,
    variance: Variance,
    engine: &TrackedEngine,
) -> Result<Step, OverflowError> {
    Solver::new(&Premise::default(), engine)
        .resolve_subtypes(vec![Subtype::new(lesser, greater, variance)])
        .await
}

fn contains_variable(ty: &Interned<Type>) -> bool {
    match &**ty {
        Type::InferenceVariable(_) | Type::SkolemizedVariable(_) => true,
        Type::Application(application) => {
            application.arguments().iter().any(contains_variable)
        }
        Type::GenericParameter(_) | Type::BoundVariable(_) => false,
    }
}

fn assert_no_variables_in_step(
    substitution: &Substitution,
    residual_subtypes: &[Subtype],
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

#[tokio::test]
async fn instance_associated_arguments_must_be_solved_immediately() {
    let engine = create_engine().await;
    let common_instance = Type::new_primitive(Primitive::Bool, &engine);
    let lesser = Type::new_instance_associated(
        GlobalSymbolID::default(),
        common_instance.clone(),
        [Type::new_primitive(Primitive::Int32, &engine)],
        &engine,
    );
    let greater = Type::new_instance_associated(
        GlobalSymbolID::default(),
        common_instance,
        [Type::new_primitive(Primitive::Float32, &engine)],
        &engine,
    );

    assert_eq!(
        destructure_application(&lesser, &greater, &engine).await.unwrap(),
        None
    );
}

#[tokio::test]
async fn solved_instance_associated_arguments_are_not_deferred() {
    let engine = create_engine().await;
    let common_instance = Type::new_primitive(Primitive::Bool, &engine);
    let static_lifetime = Type::new_lifetime(Lifetime::Static, &engine);
    let erased_lifetime = Type::new_lifetime(Lifetime::Erased, &engine);
    let lesser = Type::new_instance_associated(
        GlobalSymbolID::default(),
        common_instance.clone(),
        [static_lifetime.clone()],
        &engine,
    );
    let greater = Type::new_instance_associated(
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
    assert_eq!(residual_subtypes, Vec::new());
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
    let static_lifetime = Type::new_lifetime(Lifetime::Static, &engine);
    let erased_lifetime = Type::new_lifetime(Lifetime::Erased, &engine);

    let constraints = resolve_one(
        Type::new_tuple(vec![static_lifetime.clone()], &engine),
        Type::new_tuple(vec![erased_lifetime.clone()], &engine),
        Variance::Covariant,
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
    let static_lifetime = Type::new_lifetime(Lifetime::Static, &engine);
    let erased_lifetime = Type::new_lifetime(Lifetime::Erased, &engine);

    let constraints = resolve_one(
        Type::new_tuple(vec![static_lifetime.clone()], &engine),
        Type::new_tuple(vec![erased_lifetime.clone()], &engine),
        Variance::Contravariant,
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
    let static_lifetime = Type::new_lifetime(Lifetime::Static, &engine);
    let erased_lifetime = Type::new_lifetime(Lifetime::Erased, &engine);

    let constraints = resolve_one(
        Type::new_tuple(vec![static_lifetime.clone()], &engine),
        Type::new_tuple(vec![erased_lifetime.clone()], &engine),
        Variance::Invariant,
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
        Type::new_tuple(
            vec![Type::new_lifetime(Lifetime::Static, &engine)],
            &engine,
        ),
        Type::new_tuple(
            vec![Type::new_lifetime(Lifetime::Erased, &engine)],
            &engine,
        ),
        Variance::Bivariant,
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
    let static_lifetime = Type::new_lifetime(Lifetime::Static, &engine);
    let erased_lifetime = Type::new_lifetime(Lifetime::Erased, &engine);
    let common_reference_lifetime =
        Type::new_lifetime(Lifetime::Static, &engine);
    let bool_type = Type::new_primitive(Primitive::Bool, &engine);

    let constraints = resolve_one(
        Type::new_reference(
            common_reference_lifetime.clone(),
            Type::new_reference(
                static_lifetime.clone(),
                bool_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            Mutability::Mutable,
            &engine,
        ),
        Type::new_reference(
            common_reference_lifetime,
            Type::new_reference(
                erased_lifetime.clone(),
                bool_type,
                Mutability::Immutable,
                &engine,
            ),
            Mutability::Mutable,
            &engine,
        ),
        Variance::Covariant,
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
    let u32_type = Type::new_primitive(Primitive::Uint32, &engine);
    let lhs_lifetime =
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let rhs_first_lifetime =
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let rhs_second_lifetime =
        Type::new_bound_variable(BoundVariable::new(0, 1), &engine);

    let lesser = Type::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [
            Type::new_reference(
                lhs_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            Type::new_reference(
                lhs_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
        ],
        Type::new_tuple([], &engine),
        &engine,
    );
    let greater = Type::new_function_pointer_with_higher_ranked_lifetimes(
        2,
        [
            Type::new_reference(
                rhs_first_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            Type::new_reference(
                rhs_second_lifetime,
                u32_type,
                Mutability::Immutable,
                &engine,
            ),
        ],
        Type::new_tuple([], &engine),
        &engine,
    );

    let (substitution, residual_subtypes, constraints) =
        resolve_step(lesser, greater, Variance::Covariant, &engine)
            .await
            .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_subtypes, Vec::new());
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
    let u32_type = Type::new_primitive(Primitive::Uint32, &engine);
    let lhs_lifetime =
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let rhs_first_lifetime =
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let rhs_second_lifetime =
        Type::new_bound_variable(BoundVariable::new(0, 1), &engine);

    let lesser = Type::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [
            Type::new_reference(
                lhs_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            Type::new_reference(
                lhs_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
        ],
        Type::new_reference(
            lhs_lifetime,
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );
    let greater = Type::new_function_pointer_with_higher_ranked_lifetimes(
        2,
        [
            Type::new_reference(
                rhs_first_lifetime.clone(),
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
            Type::new_reference(
                rhs_second_lifetime,
                u32_type.clone(),
                Mutability::Immutable,
                &engine,
            ),
        ],
        Type::new_reference(
            rhs_first_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );

    let (_, residual_subtypes, _) =
        resolve_step(lesser, greater, Variance::Covariant, &engine)
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
    let u32_type = Type::new_primitive(Primitive::Uint32, &engine);
    let ranked_lifetime =
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let static_lifetime = Type::new_lifetime(Lifetime::Static, &engine);

    let lesser = Type::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type::new_reference(
            ranked_lifetime,
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        )],
        Type::new_tuple([], &engine),
        &engine,
    );
    let greater = Type::new_function_pointer(
        [Type::new_reference(
            static_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        )],
        Type::new_tuple([], &engine),
        &engine,
    );

    let (_, residual_subtypes, _) =
        resolve_step(lesser, greater, Variance::Covariant, &engine)
            .await
            .unwrap();

    assert_eq!(residual_subtypes, Vec::new());
}

// input: fn(&'static u32) -> () <: for<'a> fn(&'a u32) -> () @ Covariant
// premise: {}
// output: stuck subtype problem
#[tokio::test]
async fn covariant_hrtb_rejects_skolem_to_external_leak() {
    let engine = create_engine().await;
    let u32_type = Type::new_primitive(Primitive::Uint32, &engine);
    let ranked_lifetime =
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let static_lifetime = Type::new_lifetime(Lifetime::Static, &engine);

    let lesser = Type::new_function_pointer(
        [Type::new_reference(
            static_lifetime,
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        )],
        Type::new_tuple([], &engine),
        &engine,
    );
    let greater = Type::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type::new_reference(
            ranked_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        )],
        Type::new_tuple([], &engine),
        &engine,
    );

    let (_, residual_subtypes, _) =
        resolve_step(lesser, greater, Variance::Covariant, &engine)
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
    let u32_type = Type::new_primitive(Primitive::Uint32, &engine);
    let ranked_lifetime =
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let static_lifetime = Type::new_lifetime(Lifetime::Static, &engine);

    let lesser = Type::new_function_pointer(
        [Type::new_reference(
            static_lifetime,
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        )],
        Type::new_tuple([], &engine),
        &engine,
    );
    let greater = Type::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type::new_reference(
            ranked_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        )],
        Type::new_tuple([], &engine),
        &engine,
    );

    let (_, residual_subtypes, _) =
        resolve_step(lesser, greater, Variance::Contravariant, &engine)
            .await
            .unwrap();

    assert_eq!(residual_subtypes, Vec::new());
}

// input:
// for<'a> fn(&'a u32) -> &'a u32 <:
// for<'b> fn(&'b u32) -> &'b u32 @ Invariant
// premise: {}
// output: {}
#[tokio::test]
async fn invariant_hrtb_uses_independent_directional_runs() {
    let engine = create_engine().await;
    let u32_type = Type::new_primitive(Primitive::Uint32, &engine);
    let lhs_lifetime =
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let rhs_lifetime =
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine);

    let lesser = Type::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type::new_reference(
            lhs_lifetime.clone(),
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        )],
        Type::new_reference(
            lhs_lifetime,
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );
    let greater = Type::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type::new_reference(
            rhs_lifetime.clone(),
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        )],
        Type::new_reference(
            rhs_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );

    let (substitution, residual_subtypes, constraints) =
        resolve_step(lesser, greater, Variance::Invariant, &engine)
            .await
            .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_subtypes, Vec::new());
    assert_eq!(constraints, Constraints::default());
}

// input: fn() -> &'static u32 <: for<'a> fn() -> &'a u32 @ Covariant
// premise: {}
// output: no inference or skolem variables in the returned step
#[tokio::test]
async fn hrtb_step_does_not_expose_internal_variables() {
    let engine = create_engine().await;
    let u32_type = Type::new_primitive(Primitive::Uint32, &engine);
    let ranked_lifetime =
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let static_lifetime = Type::new_lifetime(Lifetime::Static, &engine);

    let lesser = Type::new_function_pointer(
        [Type::new_tuple([], &engine)],
        Type::new_reference(
            static_lifetime,
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );
    let greater = Type::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type::new_tuple([], &engine)],
        Type::new_reference(
            ranked_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );

    let (substitution, residual_subtypes, constraints) =
        resolve_step(lesser, greater, Variance::Covariant, &engine)
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
    let u32_type = Type::new_primitive(Primitive::Uint32, &engine);
    let ranked_lifetime =
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let static_lifetime = Type::new_lifetime(Lifetime::Static, &engine);

    let lesser = Type::new_function_pointer(
        [Type::new_tuple([], &engine)],
        Type::new_reference(
            static_lifetime.clone(),
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );
    let greater = Type::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type::new_tuple([], &engine)],
        Type::new_reference(
            ranked_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );

    let (_, residual_subtypes, constraints) =
        resolve_step(lesser, greater, Variance::Covariant, &engine)
            .await
            .unwrap();

    assert_eq!(residual_subtypes, Vec::new());
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
    let u32_type = Type::new_primitive(Primitive::Uint32, &engine);
    let lhs_lifetime =
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let rhs_lifetime =
        Type::new_bound_variable(BoundVariable::new(0, 0), &engine);

    let lesser = Type::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type::new_reference(
            lhs_lifetime.clone(),
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        )],
        Type::new_reference(
            lhs_lifetime,
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );
    let greater = Type::new_function_pointer_with_higher_ranked_lifetimes(
        1,
        [Type::new_reference(
            rhs_lifetime.clone(),
            u32_type.clone(),
            Mutability::Immutable,
            &engine,
        )],
        Type::new_reference(
            rhs_lifetime,
            u32_type,
            Mutability::Immutable,
            &engine,
        ),
        &engine,
    );

    let (substitution, residual_subtypes, constraints) =
        resolve_step(lesser, greater, Variance::Bivariant, &engine)
            .await
            .unwrap();

    assert_eq!(substitution, Substitution::new());
    assert_eq!(residual_subtypes, Vec::new());
    assert_eq!(constraints, Constraints::default());
}
