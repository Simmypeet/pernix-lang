#![allow(clippy::similar_names)]

use std::sync::{atomic::AtomicUsize, Arc};

use pernixc_hash::HashMap;
use pernixc_query_derive::Key;
use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    Deserialize, Serialize,
};
use pernixc_stable_hash::StableHash;

use crate::{
    database::{query_tracker::Tracked, Database},
    runtime::{
        executor::{CyclicError, Executor},
        persistence::{
            serde::{DynamicRegistry as _, SelfRegistry},
            Persistence,
        },
    },
    Engine,
};

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct Variable(String);

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct NegateVariable(String);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NegateVariableExecutor;

impl Executor<NegateVariable> for NegateVariableExecutor {
    fn execute(
        &self,
        engine: &Tracked,
        key: NegateVariable,
    ) -> Result<i32, CyclicError> {
        Ok(-engine.query(&Variable(key.0))?)
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct SumNegatedVariable {
    pub a: String,
    pub b: String,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, StableHash,
)]
pub struct SumNegatedVariableExecutor;

impl Executor<SumNegatedVariable> for SumNegatedVariableExecutor {
    fn execute(
        &self,
        engine: &Tracked,
        key: SumNegatedVariable,
    ) -> Result<i32, CyclicError> {
        Ok(engine.query(&NegateVariable(key.a.clone()))?
            + engine.query(&NegateVariable(key.b))?)
    }
}

#[test]
fn negate_variable() {
    let mut engine = Engine::default();

    engine.set_input(&Variable("a".to_string()), 100);
    engine.set_input(&Variable("b".to_string()), 200);
    assert_eq!(engine.database.snapshot().version, 0);

    engine.runtime.executor.register(Arc::new(NegateVariableExecutor));
    engine.runtime.executor.register(Arc::new(SumNegatedVariableExecutor));

    let value = engine
        .tracked()
        .query(&SumNegatedVariable { a: "a".to_string(), b: "b".to_string() });

    assert_eq!(value, Ok(-300));

    engine.set_input(&Variable("a".to_string()), 200);
    engine.set_input(&Variable("b".to_string()), 300);
    assert_eq!(engine.database.snapshot().version, 1);

    let value = engine
        .tracked()
        .query(&SumNegatedVariable { a: "a".to_string(), b: "b".to_string() });
    assert_eq!(value, Ok(-500));

    engine.set_input(&Variable("a".to_string()), -300);
    engine.set_input(&Variable("b".to_string()), -300);

    assert_eq!(engine.database.snapshot().version, 2);
    let value = engine
        .tracked()
        .query(&SumNegatedVariable { a: "a".to_string(), b: "b".to_string() });

    assert_eq!(value, Ok(600)); // -(-300) + -(-300) = 300 + 300 = 600
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct TrackedComputation(String);

#[derive(Debug, Default)]
pub struct TrackedExecutor {
    pub call_count: AtomicUsize,
}

impl TrackedExecutor {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<TrackedComputation> for TrackedExecutor {
    fn execute(
        &self,
        engine: &Tracked,
        key: TrackedComputation,
    ) -> Result<i32, CyclicError> {
        // Increment the call counter to track executor invocations
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Perform computation based on input variable
        let input_value = engine.query(&Variable(key.0))?;
        Ok(input_value * 2)
    }
}

#[test]
fn skip_when_input_unchanged() {
    let mut engine = Engine::default();

    // Set initial input
    engine.set_input(&Variable("x".to_string()), 42);
    assert_eq!(engine.database.snapshot().version, 0);

    // Create tracked executor to count invocations
    let tracked_executor = TrackedExecutor::default();
    let executor_arc = Arc::new(tracked_executor);

    // Register the tracked executor
    engine.runtime.executor.register(executor_arc.clone());

    // First query - should compute and call executor
    let result1 = engine.tracked().query(&TrackedComputation("x".to_string()));
    assert_eq!(result1, Ok(84)); // 42 * 2
    assert_eq!(executor_arc.get_call_count(), 1);

    // Second query with same input - should skip computation and return cached
    // result
    let result2 = engine.tracked().query(&TrackedComputation("x".to_string()));
    assert_eq!(result2, Ok(84)); // Same result
    assert_eq!(executor_arc.get_call_count(), 1); // Executor NOT called again

    // Now change the input - should trigger recomputation
    engine.set_input(&Variable("x".to_string()), 100);
    assert_eq!(engine.database.snapshot().version, 1); // Version should increment

    // Query after input change - should compute and call executor again
    let result4 = engine.tracked().query(&TrackedComputation("x".to_string()));
    assert_eq!(result4, Ok(200)); // 100 * 2
    assert_eq!(executor_arc.get_call_count(), 2); // Executor called again

    // Query again with unchanged input - should skip computation again
    let result5 = engine.tracked().query(&TrackedComputation("x".to_string()));
    assert_eq!(result5, Ok(200)); // Same result
    assert_eq!(executor_arc.get_call_count(), 2); // Executor NOT called again
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct AbsVariable(String);

#[derive(Debug, Default)]
pub struct TrackedAbsExecutor {
    pub call_count: AtomicUsize,
}

impl TrackedAbsExecutor {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<AbsVariable> for TrackedAbsExecutor {
    fn execute(
        &self,
        engine: &Tracked,
        key: AbsVariable,
    ) -> Result<i32, CyclicError> {
        // Increment the call counter to track executor invocations
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Compute absolute value
        let input_value = engine.query(&Variable(key.0))?;
        Ok(input_value.abs())
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct AddTwoAbsVariable {
    pub x: String,
    pub y: String,
}

#[derive(Debug, Default)]
pub struct TrackedAddTwoAbsExecutor {
    pub call_count: AtomicUsize,
}

impl TrackedAddTwoAbsExecutor {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<AddTwoAbsVariable> for TrackedAddTwoAbsExecutor {
    fn execute(
        &self,
        engine: &Tracked,
        key: AddTwoAbsVariable,
    ) -> Result<i32, CyclicError> {
        // Increment the call counter to track executor invocations
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Compute sum of absolute values
        Ok(engine.query(&AbsVariable(key.x.clone()))?
            + engine.query(&AbsVariable(key.y))?)
    }
}

#[test]
fn skip_when_intermediate_result_unchanged() {
    let mut engine = Engine::default();

    // Set initial inputs - both positive values
    engine.set_input(&Variable("x".to_string()), 400);
    engine.set_input(&Variable("y".to_string()), 300);
    assert_eq!(engine.database.snapshot().version, 0);

    // Create tracked executors to count invocations
    let abs_executor = Arc::new(TrackedAbsExecutor::default());
    let add_executor = Arc::new(TrackedAddTwoAbsExecutor::default());

    // Register the tracked executors
    engine.runtime.executor.register(abs_executor.clone());
    engine.runtime.executor.register(add_executor.clone());

    // First query - should compute everything from scratch
    let result1 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() });
    assert_eq!(result1, Ok(700)); // abs(400) + abs(300) = 400 + 300 = 700
    assert_eq!(abs_executor.get_call_count(), 2); // Called for both x and y
    assert_eq!(add_executor.get_call_count(), 1); // Called once

    // Query again with same inputs - should skip all computation
    let result2 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() });
    assert_eq!(result2, Ok(700)); // Same result
    assert_eq!(abs_executor.get_call_count(), 2); // NOT called again
    assert_eq!(add_executor.get_call_count(), 1); // NOT called again

    // Change x from 400 to -400 (abs value stays the same)
    engine.set_input(&Variable("x".to_string()), -400);
    assert_eq!(engine.database.snapshot().version, 1); // Version should increment

    // Query after input change - abs executor should be called for x, but add
    // executor should be skipped because the result of abs(x) hasn't
    // changed
    let result3 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() });
    assert_eq!(result3, Ok(700)); // abs(-400) + abs(300) = 400 + 300 = 700 (same result!)
    assert_eq!(abs_executor.get_call_count(), 3); // Called again for x only
    assert_eq!(add_executor.get_call_count(), 1); // NOT called again because abs values are the same

    // Change y from 300 to -300 (abs value stays the same)
    engine.set_input(&Variable("y".to_string()), -300);
    assert_eq!(engine.database.snapshot().version, 2); // Version should increment again

    // Query after second input change - abs executor should be called for y,
    // but add executor still skipped
    let result4 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() });
    assert_eq!(result4, Ok(700)); // abs(-400) + abs(-300) = 400 + 300 = 700 (still same result!)
    assert_eq!(abs_executor.get_call_count(), 4); // Called again for y only
    assert_eq!(add_executor.get_call_count(), 1); // STILL not called because both abs values are the same

    // Now change x to a value that actually changes the abs result
    engine.set_input(&Variable("x".to_string()), 500);
    assert_eq!(engine.database.snapshot().version, 3); // Version should increment

    // Query after meaningful change - both executors should be called
    let result5 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() });
    assert_eq!(result5, Ok(800)); // abs(500) + abs(-300) = 500 + 300 = 800
    assert_eq!(abs_executor.get_call_count(), 5); // Called for x
    assert_eq!(add_executor.get_call_count(), 2); // Finally called again because abs(x) changed

    // Query again - should skip everything
    let result6 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() });
    assert_eq!(result6, Ok(800)); // Same result
    assert_eq!(abs_executor.get_call_count(), 5); // NOT called
    assert_eq!(add_executor.get_call_count(), 2); // NOT called
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct SquareVariable(String);

#[derive(Debug, Default)]
pub struct TrackedSquareExecutor {
    pub call_count: AtomicUsize,
}

impl TrackedSquareExecutor {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<SquareVariable> for TrackedSquareExecutor {
    fn execute(
        &self,
        engine: &Tracked,
        key: SquareVariable,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let input_value = engine.query(&Variable(key.0))?;
        Ok(input_value * input_value)
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct ComplexComputation(String);

#[derive(Debug, Default)]
pub struct TrackedComplexExecutor {
    pub call_count: AtomicUsize,
}

impl TrackedComplexExecutor {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<ComplexComputation> for TrackedComplexExecutor {
    fn execute(
        &self,
        engine: &Tracked,
        key: ComplexComputation,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // Complex computation: abs(x) + square(x)
        Ok(engine.query(&AbsVariable(key.0.clone()))?
            + engine.query(&SquareVariable(key.0))?)
    }
}

#[test]
fn multi_layer_dependency_skipping() {
    let mut engine = Engine::default();

    // Set initial input
    engine.set_input(&Variable("z".to_string()), 5);
    assert_eq!(engine.database.snapshot().version, 0);

    // Create tracked executors
    let abs_executor = Arc::new(TrackedAbsExecutor::default());
    let square_executor = Arc::new(TrackedSquareExecutor::default());
    let complex_executor = Arc::new(TrackedComplexExecutor::default());

    // Register executors
    engine.runtime.executor.register(abs_executor.clone());
    engine.runtime.executor.register(square_executor.clone());
    engine.runtime.executor.register(complex_executor.clone());

    // First query - everything computed from scratch
    let result1 = engine.tracked().query(&ComplexComputation("z".to_string()));
    assert_eq!(result1, Ok(30)); // abs(5) + square(5) = 5 + 25 = 30
    assert_eq!(abs_executor.get_call_count(), 1);
    assert_eq!(square_executor.get_call_count(), 1);
    assert_eq!(complex_executor.get_call_count(), 1);

    // Change z from 5 to -5
    // abs(-5) = 5 (unchanged), but square(-5) = 25 (unchanged too!)
    // So the complex computation result should be the same: 5 + 25 = 30
    engine.set_input(&Variable("z".to_string()), -5);
    assert_eq!(engine.database.snapshot().version, 1);

    let result2 = engine.tracked().query(&ComplexComputation("z".to_string()));
    assert_eq!(result2, Ok(30)); // abs(-5) + square(-5) = 5 + 25 = 30 (same!)
    assert_eq!(abs_executor.get_call_count(), 2); // Called for abs(-5)
    assert_eq!(square_executor.get_call_count(), 2); // Called for square(-5)
    assert_eq!(complex_executor.get_call_count(), 1); // NOT called because both dependencies are unchanged!

    // Change to a different value that actually changes the result
    engine.set_input(&Variable("z".to_string()), 3);
    assert_eq!(engine.database.snapshot().version, 2);

    let result3 = engine.tracked().query(&ComplexComputation("z".to_string()));
    assert_eq!(result3, Ok(12)); // abs(3) + square(3) = 3 + 9 = 12
    assert_eq!(abs_executor.get_call_count(), 3); // Called for abs(3)
    assert_eq!(square_executor.get_call_count(), 3); // Called for square(3)
    assert_eq!(complex_executor.get_call_count(), 2); // Called because dependencies changed

    // Query again - everything should be cached
    let result4 = engine.tracked().query(&ComplexComputation("z".to_string()));
    assert_eq!(result4, Ok(12)); // Same result
    assert_eq!(abs_executor.get_call_count(), 3); // NOT called
    assert_eq!(square_executor.get_call_count(), 3); // NOT called
    assert_eq!(complex_executor.get_call_count(), 2); // NOT called
}

// Test to demonstrate how the incremental query system should handle
// complex dependencies and potential cycles in practice
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct TypeCheckQuery(String);

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct DependencyQuery(String);

#[derive(Debug, Default)]
pub struct TypeCheckExecutor {
    pub call_count: AtomicUsize,
}

impl TypeCheckExecutor {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<TypeCheckQuery> for TypeCheckExecutor {
    fn execute(
        &self,
        engine: &Tracked,
        key: TypeCheckQuery,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Simulate type checking that depends on other type information
        let base_value = engine.query(&Variable(key.0.clone()))?;
        let dependency_value = engine.query(&DependencyQuery(key.0))?;

        Ok(base_value + dependency_value)
    }
}

#[derive(Debug, Default)]
pub struct DependencyExecutor {
    pub call_count: AtomicUsize,
}

impl DependencyExecutor {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<DependencyQuery> for DependencyExecutor {
    fn execute(
        &self,
        engine: &Tracked,
        key: DependencyQuery,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Simulate dependency resolution
        let base_value = engine.query(&Variable(key.0))?;
        Ok(base_value * 2)
    }
}

#[test]
fn incremental_compilation_simulation() {
    let mut engine = Engine::default();

    // Set up input values (representing source code)
    engine.set_input(&Variable("module_a".to_string()), 10);
    engine.set_input(&Variable("module_b".to_string()), 20);

    let type_check_executor = Arc::new(TypeCheckExecutor::default());
    let dependency_executor = Arc::new(DependencyExecutor::default());

    engine.runtime.executor.register(Arc::clone(&type_check_executor));
    engine.runtime.executor.register(Arc::clone(&dependency_executor));

    // First compilation: everything computed from scratch
    let result_a =
        engine.tracked().query(&TypeCheckQuery("module_a".to_string()));
    let result_b =
        engine.tracked().query(&TypeCheckQuery("module_b".to_string()));

    assert_eq!(result_a, Ok(30)); // 10 + (10 * 2) = 30
    assert_eq!(result_b, Ok(60)); // 20 + (20 * 2) = 60

    // Both executors should have been called for both modules
    assert_eq!(type_check_executor.get_call_count(), 2);
    assert_eq!(dependency_executor.get_call_count(), 2);

    // Simulate incremental change: only module_a changes
    engine.set_input(&Variable("module_a".to_string()), 15);

    // Reset call counts to track incremental behavior
    type_check_executor
        .call_count
        .store(0, std::sync::atomic::Ordering::SeqCst);
    dependency_executor
        .call_count
        .store(0, std::sync::atomic::Ordering::SeqCst);

    // Re-query: only module_a should be recomputed
    let new_result_a =
        engine.tracked().query(&TypeCheckQuery("module_a".to_string()));
    let cached_result_b =
        engine.tracked().query(&TypeCheckQuery("module_b".to_string()));

    assert_eq!(new_result_a, Ok(45)); // 15 + (15 * 2) = 45
    assert_eq!(cached_result_b, Ok(60)); // Same as before, should be cached

    // Only module_a related computations should be called
    assert_eq!(type_check_executor.get_call_count(), 1); // Only for module_a
    assert_eq!(dependency_executor.get_call_count(), 1); // Only for module_a
}

// Test cases for cyclic dependency handling
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
#[scc_value(Default::default())]
pub struct CyclicQueryA;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
#[scc_value(Default::default())]
pub struct CyclicQueryB;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct DependentQuery;

#[derive(Debug, Default)]
pub struct CyclicExecutorA {
    pub call_count: AtomicUsize,
}

impl CyclicExecutorA {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<CyclicQueryA> for CyclicExecutorA {
    fn execute(
        &self,
        engine: &Tracked,
        _key: CyclicQueryA,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This creates a cycle: A depends on B, B depends on A
        let b_value = engine.query(&CyclicQueryB)?;
        Ok(b_value + 10)
    }
}

#[derive(Debug, Default)]
pub struct CyclicExecutorB {
    pub call_count: AtomicUsize,
}

impl CyclicExecutorB {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<CyclicQueryB> for CyclicExecutorB {
    fn execute(
        &self,
        engine: &Tracked,
        _key: CyclicQueryB,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This completes the cycle: B depends on A, A depends on B
        let a_value = engine.query(&CyclicQueryA)?;
        Ok(a_value + 20)
    }
}

#[derive(Debug, Default)]
pub struct DependentExecutor {
    pub call_count: AtomicUsize,
}

impl DependentExecutor {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<DependentQuery> for DependentExecutor {
    fn execute(
        &self,
        engine: &Tracked,
        _key: DependentQuery,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This query depends on the cyclic queries
        let a_value = engine.query(&CyclicQueryA)?;
        let b_value = engine.query(&CyclicQueryB)?;
        Ok(a_value + b_value + 100)
    }
}

// Executor for DependentQuery that depends on conditional cyclic queries
#[derive(Debug, Default)]
pub struct ConditionalDependentExecutor {
    pub call_count: AtomicUsize,
}

impl ConditionalDependentExecutor {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<DependentQuery> for ConditionalDependentExecutor {
    fn execute(
        &self,
        engine: &Tracked,
        _key: DependentQuery,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This query depends on the conditional cyclic queries
        let a_value = engine.query(&ConditionalCyclicQueryA)?;
        let b_value = engine.query(&ConditionalCyclicQueryB)?;

        Ok(a_value + b_value + 100)
    }
}

#[test]
fn cyclic_dependency_returns_default_values() {
    let mut engine = Engine::default();

    let executor_a = Arc::new(CyclicExecutorA::default());
    let executor_b = Arc::new(CyclicExecutorB::default());

    engine.runtime.executor.register(Arc::clone(&executor_a));
    engine.runtime.executor.register(Arc::clone(&executor_b));

    // When we query CyclicQueryA, it should detect the cycle A -> B -> A
    // and return default values (0 for i32) without calling the executors
    let result_a = engine.tracked().query(&CyclicQueryA);
    let result_b = engine.tracked().query(&CyclicQueryB);

    // Both should return default values (0 for i32)
    assert_eq!(result_a, Ok(0));
    assert_eq!(result_b, Ok(0));

    // The executors will be called and increment their call counts,
    // but they will receive CyclicError when trying to query their dependencies
    // and return early without completing their computation

    // Both executors should be called exactly once during cycle detection:
    // A is called first, then B is called, then when B tries to call A again,
    // the cycle is detected and CyclicError is returned without calling A again
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);
}

#[test]
fn dependent_query_uses_cyclic_default_values() {
    let mut engine = Engine::default();

    let executor_a = Arc::new(CyclicExecutorA::default());
    let executor_b = Arc::new(CyclicExecutorB::default());
    let executor_dependent = Arc::new(DependentExecutor::default());

    engine.runtime.executor.register(Arc::clone(&executor_a));
    engine.runtime.executor.register(Arc::clone(&executor_b));
    engine.runtime.executor.register(Arc::clone(&executor_dependent));

    // Query the dependent query, which depends on the cyclic queries
    let result = engine.tracked().query(&DependentQuery);

    // DependentQuery should execute and use the default values from the cyclic
    // queries Default values: CyclicQueryA = 0, CyclicQueryB = 0
    // DependentQuery = 0 + 0 + 100 = 100
    assert_eq!(result, Ok(100));

    // When DependentQuery queries CyclicQueryA, the executor for CyclicQueryA
    // will be called and try to query CyclicQueryB, which triggers cycle
    // detection. Both cyclic executors will be called exactly once during
    // cycle detection.
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);

    // The dependent executor should have been called once
    assert_eq!(executor_dependent.get_call_count(), 1);
}

#[test]
fn comprehensive_cyclic_dependency_behavior() {
    let mut engine = Engine::default();

    let executor_a = Arc::new(CyclicExecutorA::default());
    let executor_b = Arc::new(CyclicExecutorB::default());
    let executor_dependent = Arc::new(DependentExecutor::default());

    engine.runtime.executor.register(Arc::clone(&executor_a));
    engine.runtime.executor.register(Arc::clone(&executor_b));
    engine.runtime.executor.register(Arc::clone(&executor_dependent));

    // First, query CyclicQueryA directly to trigger cycle detection
    let result_a = engine.tracked().query(&CyclicQueryA);
    assert_eq!(result_a, Ok(0)); // Should return default value

    // Check call counts after first cycle detection
    let initial_a_calls = executor_a.get_call_count();
    let initial_b_calls = executor_b.get_call_count();

    // Both executors should have been called exactly once during cycle
    // detection
    assert_eq!(initial_a_calls, 1);
    assert_eq!(initial_b_calls, 1);

    // Now query CyclicQueryB - it should return cached default value
    let result_b = engine.tracked().query(&CyclicQueryB);
    assert_eq!(result_b, Ok(0)); // Should return default value

    // Call counts shouldn't change because values are cached
    assert_eq!(executor_a.get_call_count(), initial_a_calls);
    assert_eq!(executor_b.get_call_count(), initial_b_calls);

    // Now query DependentQuery - it should use the cached default values
    let result_dependent = engine.tracked().query(&DependentQuery);
    assert_eq!(result_dependent, Ok(100)); // 0 + 0 + 100 = 100

    // The dependent executor should be called once
    assert_eq!(executor_dependent.get_call_count(), 1);

    // The cyclic executors' call counts shouldn't change
    assert_eq!(executor_a.get_call_count(), initial_a_calls);
    assert_eq!(executor_b.get_call_count(), initial_b_calls);

    // Query everything again - all should be cached
    let result_a2 = engine.tracked().query(&CyclicQueryA);
    let result_b2 = engine.tracked().query(&CyclicQueryB);
    let result_dependent2 = engine.tracked().query(&DependentQuery);

    assert_eq!(result_a2, Ok(0));
    assert_eq!(result_b2, Ok(0));
    assert_eq!(result_dependent2, Ok(100));

    // No additional executor calls should be made
    assert_eq!(executor_a.get_call_count(), initial_a_calls);
    assert_eq!(executor_b.get_call_count(), initial_b_calls);
    assert_eq!(executor_dependent.get_call_count(), 1);
}

// Test cases for conditional cyclic dependency handling
// These queries create cycles only when certain input conditions are met
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
#[scc_value(Default::default())]
pub struct ConditionalCyclicQueryA;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
#[scc_value(Default::default())]
pub struct ConditionalCyclicQueryB;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct CycleControlVariable;

#[derive(Debug, Default)]
pub struct ConditionalCyclicExecutorA {
    pub call_count: AtomicUsize,
}

impl ConditionalCyclicExecutorA {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }

    pub fn reset_call_count(&self) {
        self.call_count.store(0, std::sync::atomic::Ordering::SeqCst);
    }
}

impl Executor<ConditionalCyclicQueryA> for ConditionalCyclicExecutorA {
    fn execute(
        &self,
        engine: &Tracked,
        _key: ConditionalCyclicQueryA,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Read the control variable to determine whether to create a cycle
        let control_value = engine.query(&CycleControlVariable)?;

        if control_value == 1 {
            // When control_value is 1, create a cycle by querying B
            let b_value = engine.query(&ConditionalCyclicQueryB)?;
            Ok(b_value + 10)
        } else {
            // When control_value is not 1, no cycle - just return a computed
            // value
            Ok(control_value * 100)
        }
    }
}

#[derive(Debug, Default)]
pub struct ConditionalCyclicExecutorB {
    pub call_count: AtomicUsize,
}

impl ConditionalCyclicExecutorB {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }

    pub fn reset_call_count(&self) {
        self.call_count.store(0, std::sync::atomic::Ordering::SeqCst);
    }
}

impl Executor<ConditionalCyclicQueryB> for ConditionalCyclicExecutorB {
    fn execute(
        &self,
        engine: &Tracked,
        _key: ConditionalCyclicQueryB,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Read the control variable to determine whether to create a cycle
        let control_value = engine.query(&CycleControlVariable)?;

        if control_value == 1 {
            // When control_value is 1, complete the cycle by querying A
            let a_value = engine.query(&ConditionalCyclicQueryA)?;
            Ok(a_value + 20)
        } else {
            // When control_value is not 1, no cycle - just return a computed
            // value
            Ok(control_value * 200)
        }
    }
}

#[test]
fn conditional_cyclic_dependency() {
    let mut engine = Engine::default();

    let executor_a = Arc::new(ConditionalCyclicExecutorA::default());
    let executor_b = Arc::new(ConditionalCyclicExecutorB::default());

    engine.runtime.executor.register(Arc::clone(&executor_a));
    engine.runtime.executor.register(Arc::clone(&executor_b));

    // Phase 1: Set control value to create NO cycle (control_value != 1)
    engine.set_input(&CycleControlVariable, 5);

    // Query both A and B - they should compute normal values without cycles
    let result_a = engine.tracked().query(&ConditionalCyclicQueryA);
    let result_b = engine.tracked().query(&ConditionalCyclicQueryB);

    // Expected values: A = 5 * 100 = 500, B = 5 * 200 = 1000
    assert_eq!(result_a, Ok(500));
    assert_eq!(result_b, Ok(1000));

    // Both executors should have been called once each
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);

    // Phase 2: Change control value to CREATE a cycle (control_value == 1)
    engine.set_input(&CycleControlVariable, 1);
    executor_a.reset_call_count();
    executor_b.reset_call_count();

    // Query A - this should trigger cycle detection and return default values
    let result_a_cyclic = engine.tracked().query(&ConditionalCyclicQueryA);
    let result_b_cyclic = engine.tracked().query(&ConditionalCyclicQueryB);

    // Both should return default values (0 for i32) due to cycle detection
    assert_eq!(result_a_cyclic, Ok(0));
    assert_eq!(result_b_cyclic, Ok(0));

    // Both executors should be called exactly once during cycle detection
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);

    // Phase 3: Change control value back to break the cycle (control_value !=
    // 1)
    engine.set_input(&CycleControlVariable, 3);
    executor_a.reset_call_count();
    executor_b.reset_call_count();

    // Query both A and B - they should recompute and return normal values again
    let result_a_normal = engine.tracked().query(&ConditionalCyclicQueryA);
    let result_b_normal = engine.tracked().query(&ConditionalCyclicQueryB);

    // Expected values: A = 3 * 100 = 300, B = 3 * 200 = 600
    assert_eq!(result_a_normal, Ok(300));
    assert_eq!(result_b_normal, Ok(600));

    // Both executors should have been called once each for recomputation
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);

    // Phase 4: Create cycle again with a different control value
    // (control_value // == 1)
    engine.set_input(&CycleControlVariable, 1);
    executor_a.reset_call_count();
    executor_b.reset_call_count();

    // Query A - cycle should be detected again and default values returned
    let result_a_cyclic2 = engine.tracked().query(&ConditionalCyclicQueryA);
    let result_b_cyclic2 = engine.tracked().query(&ConditionalCyclicQueryB);

    // Both should return default values (0 for i32) due to cycle detection
    assert_eq!(result_a_cyclic2, Ok(0));
    assert_eq!(result_b_cyclic2, Ok(0));

    // Both executors should be called exactly once during cycle detection
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);
}

#[test]
fn conditional_cyclic_with_dependent_query() {
    let mut engine = Engine::default();

    let executor_a = Arc::new(ConditionalCyclicExecutorA::default());
    let executor_b = Arc::new(ConditionalCyclicExecutorB::default());
    let executor_dependent = Arc::new(ConditionalDependentExecutor::default());

    engine.runtime.executor.register(Arc::clone(&executor_a));
    engine.runtime.executor.register(Arc::clone(&executor_b));
    engine.runtime.executor.register(Arc::clone(&executor_dependent));

    // Phase 1: No cycle - dependent query should use computed values

    engine.set_input(&CycleControlVariable, 2);

    let result_dependent = engine.tracked().query(&DependentQuery);

    // DependentQuery = A + B + 100 = (2*100) + (2*200) + 100 = 200 + 400 + 100
    // = 700
    assert_eq!(result_dependent, Ok(700));

    // All executors should have been called
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);
    assert_eq!(executor_dependent.get_call_count(), 1);

    // Phase 2: Create cycle - dependent query should use default values

    engine.set_input(&CycleControlVariable, 1);
    executor_a.reset_call_count();
    executor_b.reset_call_count();

    // Reset dependent executor call count to track new computation
    executor_dependent.call_count.store(0, std::sync::atomic::Ordering::SeqCst);

    let result_dependent_cyclic = engine.tracked().query(&DependentQuery);

    // DependentQuery should use default values: 0 + 0 + 100 = 100
    assert_eq!(result_dependent_cyclic, Ok(100));

    // Cyclic executors called once each during cycle detection
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);
    // Dependent executor called once to compute with new (default) values
    assert_eq!(executor_dependent.get_call_count(), 1);

    // Phase 3: Break cycle again - dependent query should use computed values

    engine.set_input(&CycleControlVariable, 4);
    executor_a.reset_call_count();
    executor_b.reset_call_count();
    executor_dependent.call_count.store(0, std::sync::atomic::Ordering::SeqCst); // Query A and B to ensure they're computed
    let _debug_a = engine.tracked().query(&ConditionalCyclicQueryA);
    let _debug_b = engine.tracked().query(&ConditionalCyclicQueryB);

    let result_dependent_normal = engine.tracked().query(&DependentQuery);

    // DependentQuery = A + B + 100 = (4*100) + (4*200) + 100 = 400 + 800 + 100
    // = 1300
    assert_eq!(result_dependent_normal, Ok(1300));

    // All executors should have been called for recomputation
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);
    assert_eq!(executor_dependent.get_call_count(), 1);
}

#[test]
#[allow(clippy::too_many_lines)]
fn database_serialization_deserialization() {
    // Step 1: Set up original engine with input data and executors
    let mut original_engine = Engine::default();

    // Set up input variables
    original_engine.set_input(&Variable("x".to_string()), 100);
    original_engine.set_input(&Variable("y".to_string()), 200);
    original_engine.set_input(&Variable("z".to_string()), 300);

    // Register executors
    let tracked_executor = Arc::new(TrackedExecutor::default());
    let abs_executor = Arc::new(TrackedAbsExecutor::default());
    let add_executor = Arc::new(TrackedAddTwoAbsExecutor::default());

    original_engine.runtime.executor.register(tracked_executor.clone());
    original_engine.runtime.executor.register(abs_executor.clone());
    original_engine.runtime.executor.register(add_executor.clone());

    // Step 2: Perform queries to populate the database with computed values
    let result1 = original_engine
        .tracked()
        .query(&TrackedComputation("x".to_string()))
        .unwrap();
    let result2 =
        original_engine.tracked().query(&AbsVariable("y".to_string())).unwrap();
    let result3 = original_engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "z".to_string() })
        .unwrap();

    // Verify initial results
    assert_eq!(result1, 200); // 100 * 2
    assert_eq!(result2, 200); // abs(200)
    assert_eq!(result3, 400); // abs(100) + abs(300) = 100 + 300

    // Verify executors were called
    assert_eq!(tracked_executor.get_call_count(), 1);
    assert_eq!(abs_executor.get_call_count(), 3); // Called for y, x, and z
    assert_eq!(add_executor.get_call_count(), 1);

    // Step 3: Serialize the database state
    let mut serde_config = SelfRegistry::default();

    // Register all Key types used in this test
    serde_config.register::<Variable>();
    serde_config.register::<TrackedComputation>();
    serde_config.register::<AbsVariable>();
    serde_config.register::<AddTwoAbsVariable>();

    let mut serializer = BinarySerializer::new(Vec::new());
    original_engine.database.serialize(&mut serializer, &serde_config).unwrap();

    // Step 4: Create a new engine instance
    let mut new_engine = Engine::default();

    // Register the same executors to the new engine
    let new_tracked_executor = Arc::new(TrackedExecutor::default());
    let new_abs_executor = Arc::new(TrackedAbsExecutor::default());
    let new_add_executor = Arc::new(TrackedAddTwoAbsExecutor::default());

    new_engine.runtime.executor.register(new_tracked_executor.clone());
    new_engine.runtime.executor.register(new_abs_executor.clone());
    new_engine.runtime.executor.register(new_add_executor.clone());

    // Step 5: Deserialize the saved state into the new engine
    let mut deserializer =
        BinaryDeserializer::new(std::io::Cursor::new(serializer.into_inner()));

    new_engine.database =
        Database::deserialize(&mut deserializer, &serde_config).unwrap();

    // Step 6: Verify that queries return the same results without recomputation
    let new_result1 = new_engine
        .tracked()
        .query(&TrackedComputation("x".to_string()))
        .unwrap();
    let new_result2 =
        new_engine.tracked().query(&AbsVariable("y".to_string())).unwrap();
    let new_result3 = new_engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "z".to_string() })
        .unwrap();

    // Results should be identical
    assert_eq!(new_result1, result1);
    assert_eq!(new_result2, result2);
    assert_eq!(new_result3, result3);

    // Executors should NOT have been called (values should be cached)
    assert_eq!(new_tracked_executor.get_call_count(), 0);
    assert_eq!(new_abs_executor.get_call_count(), 0);
    assert_eq!(new_add_executor.get_call_count(), 0);

    // Step 7: Test that incremental updates work correctly after
    // deserialization Change input and verify that only affected
    // computations are re-executed
    //
    // Version is incremented
    new_engine.set_input(&Variable("x".to_string()), 150);

    // Query again - should trigger recomputation for affected queries only
    let updated_result1 = new_engine
        .tracked()
        .query(&TrackedComputation("x".to_string()))
        .unwrap();
    let updated_result2 =
        new_engine.tracked().query(&AbsVariable("y".to_string())).unwrap();
    let updated_result3 = new_engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "z".to_string() })
        .unwrap();

    // Verify updated results
    assert_eq!(updated_result1, 300); // 150 * 2
    assert_eq!(updated_result2, 200); // abs(200) - unchanged
    assert_eq!(updated_result3, 450); // abs(150) + abs(300) = 150 + 300

    // Verify correct incremental recomputation:
    // - TrackedExecutor should be called once for x
    // - AbsExecutor should be called once for x (y is unchanged)
    // - AddExecutor should be called once due to x change
    assert_eq!(new_tracked_executor.get_call_count(), 1);
    assert_eq!(new_abs_executor.get_call_count(), 1); // Only for x
    assert_eq!(new_add_executor.get_call_count(), 1);

    // Step 8: Test adding completely new queries after deserialization
    new_engine.set_input(&Variable("w".to_string()), 50);

    let new_query_result = new_engine
        .tracked()
        .query(&TrackedComputation("w".to_string()))
        .unwrap();
    assert_eq!(new_query_result, 100); // 50 * 2

    // TrackedExecutor should be called one more time for the new variable
    assert_eq!(new_tracked_executor.get_call_count(), 2);
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Key,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
#[value(HashMap<String, i32>)]
pub struct VariableMap;

#[derive(Debug, Default)]
pub struct VariableMapExecutor {
    pub call_count: AtomicUsize,
}

impl Executor<VariableMap> for VariableMapExecutor {
    fn execute(
        &self,
        _: &Tracked,
        _: VariableMap,
    ) -> Result<HashMap<String, i32>, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Simulate a complex computation that returns a map of variables
        let mut result = HashMap::default();
        result.insert("a".to_string(), 1);
        result.insert("b".to_string(), 2);
        result.insert("c".to_string(), 3);

        Ok(result)
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Key,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
#[value(Option<i32>)]
pub struct GetValue(String);

pub struct GetValueExecutor {
    pub call_count: AtomicUsize,
}

impl Executor<GetValue> for GetValueExecutor {
    fn execute(
        &self,
        engine: &Tracked,
        key: GetValue,
    ) -> Result<Option<i32>, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Retrieve the value from the VariableMap
        let variable_map = engine.query(&VariableMap)?;
        Ok(variable_map.get(&key.0).copied())
    }
}

#[test]
fn persistence_variable_map_query() {
    let mut engine = Engine::default();

    // Create and register the VariableMapExecutor
    let variable_map_executor = Arc::new(VariableMapExecutor::default());
    engine.runtime.executor.register(variable_map_executor.clone());

    // Create and register the GetValueExecutor
    let get_value_executor =
        Arc::new(GetValueExecutor { call_count: AtomicUsize::new(0) });
    engine.runtime.executor.register(get_value_executor.clone());

    let mut serde_extension = SelfRegistry::default();
    serde_extension.register::<VariableMap>();
    serde_extension.register::<GetValue>();

    let tempdir = tempfile::tempdir().unwrap();

    let mut persistence = Persistence::new(
        tempdir.path().to_path_buf(),
        Arc::new(serde_extension),
    )
    .unwrap();
    persistence.register_skip_key::<GetValue>();

    engine.runtime.persistence = Some(persistence);

    let a = engine.tracked().query(&GetValue("a".to_string())).unwrap();
    let b = engine.tracked().query(&GetValue("b".to_string())).unwrap();
    let c = engine.tracked().query(&GetValue("c".to_string())).unwrap();

    assert_eq!(a, Some(1));
    assert_eq!(b, Some(2));
    assert_eq!(c, Some(3));

    assert_eq!(
        get_value_executor.call_count.load(std::sync::atomic::Ordering::SeqCst),
        3
    ); // Called for a, b, c
    assert_eq!(
        variable_map_executor
            .call_count
            .load(std::sync::atomic::Ordering::SeqCst),
        1
    ); // Called once to compute the VariableMap

    // save to persistence
    engine.try_save_database().unwrap();

    // load the persistence
    let database =
        engine.runtime.persistence.as_ref().unwrap().load_database().unwrap();
    engine.database = database;

    // reset the call counts
    variable_map_executor
        .call_count
        .store(0, std::sync::atomic::Ordering::SeqCst);
    get_value_executor.call_count.store(0, std::sync::atomic::Ordering::SeqCst);

    // Query again after loading from persistence
    let a2 = engine.tracked().query(&GetValue("a".to_string())).unwrap();
    let b2 = engine.tracked().query(&GetValue("b".to_string())).unwrap();
    let c2 = engine.tracked().query(&GetValue("c".to_string())).unwrap();

    assert_eq!(a2, a);
    assert_eq!(b2, b);
    assert_eq!(c2, c);

    // The VariableMapExecutor should NOT have been called again
    assert_eq!(
        variable_map_executor
            .call_count
            .load(std::sync::atomic::Ordering::SeqCst),
        0
    ); // Should be cached

    // The GetValueExecutor should be called again for each variable
    // as it's skipped in persistence
    assert_eq!(
        get_value_executor.call_count.load(std::sync::atomic::Ordering::SeqCst),
        3
    ); // Called for a, b, c again
}

#[test]
fn persistence_input_invalidation() {
    let mut engine = Engine::default();

    // Create and register the GetValueExecutor
    let get_value_executor =
        Arc::new(GetValueExecutor { call_count: AtomicUsize::new(0) });
    engine.runtime.executor.register(get_value_executor.clone());

    let mut serde_extension = SelfRegistry::default();
    serde_extension.register::<VariableMap>();
    serde_extension.register::<GetValue>();

    let tempdir = tempfile::tempdir().unwrap();

    let mut persistence = Persistence::new(
        tempdir.path().to_path_buf(),
        Arc::new(serde_extension),
    )
    .unwrap();

    // Don't save variable map to persistence
    persistence.register_skip_key::<VariableMap>();

    engine.runtime.persistence = Some(persistence);

    engine.set_input(
        &VariableMap,
        [("a".to_string(), 1), ("b".to_string(), 2), ("c".to_string(), 3)]
            .into_iter()
            .collect(),
    );

    let a = engine.tracked().query(&GetValue("a".to_string())).unwrap();
    let b = engine.tracked().query(&GetValue("b".to_string())).unwrap();
    let c = engine.tracked().query(&GetValue("c".to_string())).unwrap();

    assert_eq!(a, Some(1));
    assert_eq!(b, Some(2));
    assert_eq!(c, Some(3));

    assert_eq!(
        get_value_executor.call_count.load(std::sync::atomic::Ordering::SeqCst),
        3
    ); // Called for a, b, c

    let first_version = engine.database.snapshot().version;

    // save to persistence
    engine.try_save_database().unwrap();

    // load the persistence
    let database =
        engine.runtime.persistence.as_ref().unwrap().load_database().unwrap();
    engine.database = database;

    // reset the call counts
    get_value_executor.call_count.store(0, std::sync::atomic::Ordering::SeqCst);

    // Query again after loading from persistence
    let a2 = engine.tracked().query(&GetValue("a".to_string())).unwrap();
    let b2 = engine.tracked().query(&GetValue("b".to_string())).unwrap();
    let c2 = engine.tracked().query(&GetValue("c".to_string())).unwrap();

    assert_eq!(a2, a);
    assert_eq!(b2, b);
    assert_eq!(c2, c);

    // The GetValueExecutor should NOT have been called again since the
    // VariableMap hasn't changed
    assert_eq!(
        get_value_executor.call_count.load(std::sync::atomic::Ordering::SeqCst),
        0
    );

    // version should be the same as before saving
    assert_eq!(engine.database.snapshot().version, first_version);

    // save the current state to persistence
    engine.try_save_database().unwrap();

    // load it back again
    let database =
        engine.runtime.persistence.as_ref().unwrap().load_database().unwrap();
    engine.database = database;

    // change the input for VariableMap
    engine.set_input(
        &VariableMap,
        [("a".to_string(), 10), ("b".to_string(), 20), ("c".to_string(), 30)]
            .into_iter()
            .collect(),
    );

    // Query again after changing the input
    let a3 = engine.tracked().query(&GetValue("a".to_string())).unwrap();
    let b3 = engine.tracked().query(&GetValue("b".to_string())).unwrap();
    let c3 = engine.tracked().query(&GetValue("c".to_string())).unwrap();

    assert_eq!(a3, Some(10));
    assert_eq!(b3, Some(20));
    assert_eq!(c3, Some(30));

    // The GetValueExecutor should be called again for each variable
    assert_eq!(
        get_value_executor.call_count.load(std::sync::atomic::Ordering::SeqCst),
        3
    ); // Called for a, b, c again

    // version should be incremented
    assert!(engine.database.snapshot().version > first_version);
}
