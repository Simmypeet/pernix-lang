use std::sync::{atomic::AtomicU64, Arc};

use pernixc_query::Engine;
use pernixc_term::{
    inference,
    r#type::{Primitive, Type},
    tuple::{Element, Tuple},
};
use pernixc_type_system::environment::Premise;

use super::{InferenceContext, UnifyError};
use crate::inference_context::{constraint, table::Inference};

/// Helper function to create unique inference variables
fn create_type_inference() -> inference::Variable<Type> {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    inference::Variable::new(
        COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
    )
}

/// Helper function to create int32 type
fn int32_type() -> Type { Type::Primitive(Primitive::Int32) }

/// Helper function to create float32 type
fn float32_type() -> Type { Type::Primitive(Primitive::Float32) }

/// Helper function to create uint32 type
fn uint32_type() -> Type { Type::Primitive(Primitive::Uint32) }

/// Helper function to create a tuple type with given elements
fn tuple_type(elements: Vec<Type>) -> Type {
    Type::Tuple(Tuple {
        elements: elements
            .into_iter()
            .map(|t| Element::new(t, false))
            .collect(),
    })
}

/// Helper to register an inference variable in the context
fn register_inference_variable(
    context: &mut InferenceContext,
    var: inference::Variable<Type>,
    constraint: constraint::Type,
) {
    context.type_table.register(var, constraint);
}

/// Helper to check if an inference variable is resolved to a known type
fn get_known_type(
    context: &InferenceContext,
    var: inference::Variable<Type>,
) -> Option<Type> {
    match context.type_table.get_inference(var)? {
        Inference::Known(ty) => Some(ty.clone()),
        Inference::Inferring(_) => None,
    }
}

#[tokio::test]
async fn inference_variable_unification_with_known_type() {
    let mut context = InferenceContext::default();
    let engine = Arc::new(Engine::default());
    let premise = Premise::default();

    // Create an inference variable
    let inference_var = create_type_inference();
    register_inference_variable(
        &mut context,
        inference_var,
        constraint::Type::All(true),
    );

    // Create types
    let inference_type = Type::Inference(inference_var);
    let known_type = int32_type();

    // Unify inference variable with known type
    let result = context
        .unify(&inference_type, &known_type, &premise, &engine.tracked())
        .await;
    assert!(result.is_ok());

    // Check that the inference variable is now resolved to the known type
    let resolved_type = get_known_type(&context, inference_var);
    assert_eq!(resolved_type, Some(int32_type()));
}

#[tokio::test]
async fn two_inference_variables_unification() {
    let mut context = InferenceContext::default();
    let engine = Arc::new(Engine::default());
    let premise = Premise::default();

    // Create two inference variables
    let inference_var1 = create_type_inference();
    let inference_var2 = create_type_inference();
    register_inference_variable(
        &mut context,
        inference_var1,
        constraint::Type::All(true),
    );
    register_inference_variable(
        &mut context,
        inference_var2,
        constraint::Type::All(true),
    );

    // Create types from variables
    let inference_type1 = Type::Inference(inference_var1);
    let inference_type2 = Type::Inference(inference_var2);

    // Unify the two inference variables
    let result = context
        .unify(&inference_type1, &inference_type2, &premise, &engine.tracked())
        .await;
    assert!(result.is_ok());

    // Both variables should now point to the same constraint
    // (They are unified but not necessarily resolved to a concrete type
    // yet)
    let inference1 = context.type_table.get_inference(inference_var1);
    let inference2 = context.type_table.get_inference(inference_var2);
    assert!(inference1.is_some());
    assert!(inference2.is_some());
}

#[tokio::test]
async fn tuple_unification() {
    let mut context = InferenceContext::default();
    let engine = Arc::new(Engine::default());
    let premise = Premise::default();

    // Create an inference variable
    let inference_var = create_type_inference();
    register_inference_variable(
        &mut context,
        inference_var,
        constraint::Type::All(true),
    );
    let inference_type = Type::Inference(inference_var);

    // Create tuple types: (int32, ?T) and (int32, float32)
    let tuple_with_inference = tuple_type(vec![int32_type(), inference_type]);
    let tuple_with_concrete = tuple_type(vec![int32_type(), float32_type()]);

    // Unify the tuples
    let result = context
        .unify(
            &tuple_with_inference,
            &tuple_with_concrete,
            &premise,
            &engine.tracked(),
        )
        .await;
    assert!(result.is_ok());

    // Check that the inference variable is resolved to float32
    let resolved_type = get_known_type(&context, inference_var);
    assert_eq!(resolved_type, Some(float32_type()));
}

#[tokio::test]
async fn nested_tuple_unification() {
    let mut context = InferenceContext::default();
    let engine = Arc::new(Engine::default());
    let premise = Premise::default();

    // Create inference variables
    let inference_var1 = create_type_inference();
    let inference_var2 = create_type_inference();
    register_inference_variable(
        &mut context,
        inference_var1,
        constraint::Type::All(true),
    );
    register_inference_variable(
        &mut context,
        inference_var2,
        constraint::Type::All(true),
    );

    // Create nested tuple types: ((int32, ?T1), ?T2) and ((int32, float32),
    // uint32)
    let inner_tuple_with_inference =
        tuple_type(vec![int32_type(), Type::Inference(inference_var1)]);
    let outer_tuple_with_inference = tuple_type(vec![
        inner_tuple_with_inference,
        Type::Inference(inference_var2),
    ]);

    let inner_tuple_concrete = tuple_type(vec![int32_type(), float32_type()]);
    let outer_tuple_concrete =
        tuple_type(vec![inner_tuple_concrete, uint32_type()]);

    // Unify the nested tuples
    let result = context
        .unify(
            &outer_tuple_with_inference,
            &outer_tuple_concrete,
            &premise,
            &engine.tracked(),
        )
        .await;
    assert!(result.is_ok());

    // Check that both inference variables are resolved correctly
    assert_eq!(get_known_type(&context, inference_var1), Some(float32_type()));
    assert_eq!(get_known_type(&context, inference_var2), Some(uint32_type()));
}

#[tokio::test]
async fn cyclic_inference_detection() {
    let mut context = InferenceContext::default();
    let engine = Arc::new(Engine::default());
    let premise = Premise::default();

    // Create an inference variable
    let inference_var = create_type_inference();
    register_inference_variable(
        &mut context,
        inference_var,
        constraint::Type::All(true),
    );

    // Create a cyclic type: ?T and (?T, int32)
    // This would create a cycle where ?T depends on itself
    let inference_type = Type::Inference(inference_var);
    let cyclic_tuple = tuple_type(vec![inference_type.clone(), int32_type()]);

    // Try to unify ?T with (?T, int32) - should detect cycle
    let result = context
        .unify(&inference_type, &cyclic_tuple, &premise, &engine.tracked())
        .await;

    assert!(matches!(result, Err(UnifyError::CyclicTypeInference(_))));
}

#[tokio::test]
async fn cyclic_inference_with_tuple_elements() {
    let mut context = InferenceContext::default();
    let engine = Arc::new(Engine::default());
    let premise = Premise::default();

    // Create two inference variables
    let inference_var1 = create_type_inference();
    let inference_var2 = create_type_inference();
    register_inference_variable(
        &mut context,
        inference_var1,
        constraint::Type::All(true),
    );
    register_inference_variable(
        &mut context,
        inference_var2,
        constraint::Type::All(true),
    );

    // Create cyclic dependency: ?T1 -> (?T2, int32) and ?T2 -> (?T1,
    // float32)
    let type1 = Type::Inference(inference_var1);
    let type2 = Type::Inference(inference_var2);

    let tuple_with_type2 = tuple_type(vec![type2, int32_type()]);
    let tuple_with_type1 = tuple_type(vec![type1, float32_type()]);

    // First unification: ?T1 = (?T2, int32)
    let result1 = context
        .unify(
            &Type::Inference(inference_var1),
            &tuple_with_type2,
            &premise,
            &engine.tracked(),
        )
        .await;
    assert!(result1.is_ok());

    // Second unification: ?T2 = (?T1, float32) - should detect cycle
    let result2 = context
        .unify(
            &Type::Inference(inference_var2),
            &tuple_with_type1,
            &premise,
            &engine.tracked(),
        )
        .await;
    assert!(matches!(result2, Err(UnifyError::CyclicTypeInference(_))));
}

#[tokio::test]
async fn multiple_inference_variables_in_tuple() {
    let mut context = InferenceContext::default();
    let engine = Arc::new(Engine::default());
    let premise = Premise::default();

    // Create multiple inference variables
    let inference_var1 = create_type_inference();
    let inference_var2 = create_type_inference();
    let inference_var3 = create_type_inference();
    register_inference_variable(
        &mut context,
        inference_var1,
        constraint::Type::All(true),
    );
    register_inference_variable(
        &mut context,
        inference_var2,
        constraint::Type::All(true),
    );
    register_inference_variable(
        &mut context,
        inference_var3,
        constraint::Type::All(true),
    );

    // Create tuple types: (?T1, ?T2, ?T3) and (int32, float32, uint32)
    let tuple_with_inferences = tuple_type(vec![
        Type::Inference(inference_var1),
        Type::Inference(inference_var2),
        Type::Inference(inference_var3),
    ]);
    let tuple_concrete =
        tuple_type(vec![int32_type(), float32_type(), uint32_type()]);

    // Unify the tuples
    let result = context
        .unify(
            &tuple_with_inferences,
            &tuple_concrete,
            &premise,
            &engine.tracked(),
        )
        .await;
    assert!(result.is_ok());

    // Check that all inference variables are resolved correctly
    assert_eq!(get_known_type(&context, inference_var1), Some(int32_type()));
    assert_eq!(get_known_type(&context, inference_var2), Some(float32_type()));
    assert_eq!(get_known_type(&context, inference_var3), Some(uint32_type()));
}

#[test]
fn partial_tuple_unification() {
    let test = async {
        let mut context = InferenceContext::default();
        let engine = Arc::new(Engine::default());
        let premise = Premise::default();

        // Create an inference variable
        let inference_var = create_type_inference();
        register_inference_variable(
            &mut context,
            inference_var,
            constraint::Type::All(true),
        );

        // Create tuples with different lengths - should fail
        let tuple_short =
            tuple_type(vec![int32_type(), Type::Inference(inference_var)]);
        let tuple_long =
            tuple_type(vec![int32_type(), float32_type(), uint32_type()]);

        // Try to unify tuples of different lengths
        let result = context
            .unify(&tuple_short, &tuple_long, &premise, &engine.tracked())
            .await;
        assert!(matches!(result, Err(UnifyError::IncompatibleTypes { .. })));
    };

    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap()
        .block_on(test);
}

#[tokio::test]
async fn inference_variable_with_constraint_mismatch() {
    let mut context = InferenceContext::default();
    let engine = Arc::new(Engine::default());
    let premise = Premise::default();

    // Create an inference variable with integer constraint
    let inference_var = create_type_inference();
    register_inference_variable(
        &mut context,
        inference_var,
        constraint::Type::Integer,
    );

    // Try to unify with a float type (should respect constraint)
    let inference_type = Type::Inference(inference_var);
    let float_type = float32_type();

    // This should fail if the constraint system is working correctly
    // Note: Depending on implementation, this might succeed if float32
    // satisfies Integer constraint or fail if constraints are
    // strictly enforced during unification
    let result = context
        .unify(&inference_type, &float_type, &premise, &engine.tracked())
        .await;
    assert!(matches!(result, Err(UnifyError::UnsatisfiedConstraint { .. })));
}
