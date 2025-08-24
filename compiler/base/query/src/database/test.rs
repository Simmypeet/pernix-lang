use std::sync::{
    atomic::{AtomicI32, AtomicUsize},
    Arc,
};

use dashmap::DashSet;
use pernixc_hash::{HashMap, HashSet};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    database::TrackedEngine,
    runtime::{
        executor::{CyclicError, Executor},
        persistence::{
            serde::{DynamicRegistry as _, SelfRegistry},
            Persistence,
        },
    },
    Engine, Key,
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
    async fn execute(
        &self,
        engine: &TrackedEngine,
        key: &NegateVariable,
    ) -> Result<i32, CyclicError> {
        Ok(-engine.query(&Variable(key.0.to_string())).await?)
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
    async fn execute(
        &self,
        engine: &TrackedEngine,
        key: &SumNegatedVariable,
    ) -> Result<i32, CyclicError> {
        let a = engine.query(&NegateVariable(key.a.clone())).await?;
        let b = engine.query(&NegateVariable(key.b.clone())).await?;

        Ok(a + b)
    }
}

#[tokio::test]
async fn negate_variable() {
    let mut engine = Arc::new(Engine::default());

    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(Variable("a".to_string()), 100).await;
            x.set_input(Variable("b".to_string()), 200).await;
        })
        .await;

    assert_eq!(engine.version(), 1);

    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::new(NegateVariableExecutor));
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::new(SumNegatedVariableExecutor));

    let value = engine
        .tracked()
        .query(&SumNegatedVariable { a: "a".to_string(), b: "b".to_string() })
        .await
        .unwrap();

    assert_eq!(value, -300);

    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(Variable("a".to_string()), 200).await;
            x.set_input(Variable("b".to_string()), 300).await;
        })
        .await;

    assert_eq!(engine.version(), 2);

    let value = engine
        .tracked()
        .query(&SumNegatedVariable { a: "a".to_string(), b: "b".to_string() })
        .await
        .unwrap();

    assert_eq!(value, -500);

    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(Variable("a".to_string()), -300).await;
            x.set_input(Variable("b".to_string()), -300).await;
        })
        .await;

    assert_eq!(engine.version(), 3);
    let value = engine
        .tracked()
        .query(&SumNegatedVariable { a: "a".to_string(), b: "b".to_string() })
        .await
        .unwrap();

    assert_eq!(value, 600); // -(-300) + -(-300) = 300 + 300 = 600
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
    async fn execute(
        &self,
        engine: &TrackedEngine,
        key: &TrackedComputation,
    ) -> Result<i32, CyclicError> {
        // Increment the call counter to track executor invocations
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Perform computation based on input variable
        let input_value = engine.query(&Variable(key.0.clone())).await?;
        Ok(input_value * 2)
    }
}

#[tokio::test]
async fn skip_when_input_unchanged() {
    let mut engine = Arc::new(Engine::default());

    // set the initial input
    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(Variable("x".to_string()), 42).await;
        })
        .await;

    assert_eq!(engine.version(), 1);

    // Create tracked executor to count invocations
    let tracked_executor = TrackedExecutor::default();
    let executor_arc = Arc::new(tracked_executor);

    // Register the tracked executor
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(executor_arc.clone());

    // First query - should compute and call executor
    let result1 = engine
        .tracked()
        .query(&TrackedComputation("x".to_string()))
        .await
        .unwrap();
    assert_eq!(result1, 84); // 42 * 2
    assert_eq!(executor_arc.get_call_count(), 1);

    // Second query with same input - should skip computation and return cached
    // result
    let result2 = engine
        .tracked()
        .query(&TrackedComputation("x".to_string()))
        .await
        .unwrap();

    assert_eq!(result2, 84); // Same result
    assert_eq!(executor_arc.get_call_count(), 1); // Executor NOT called again

    // Now change the input - should trigger recomputation
    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(Variable("x".to_string()), 100).await;
        })
        .await;

    assert_eq!(engine.version(), 2); // Version should increment

    // Query after input change - should compute and call executor again
    let result4 = engine
        .tracked()
        .query(&TrackedComputation("x".to_string()))
        .await
        .unwrap();
    assert_eq!(result4, 200); // 100 * 2
    assert_eq!(executor_arc.get_call_count(), 2); // Executor called again

    // Query again with unchanged input - should skip computation again
    let result5 = engine
        .tracked()
        .query(&TrackedComputation("x".to_string()))
        .await
        .unwrap();
    assert_eq!(result5, 200); // Same result
    assert_eq!(executor_arc.get_call_count(), 2); // Executor NOT called again

    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            // Set input again but same value
            x.set_input(Variable("x".to_string()), 100).await;
        })
        .await;

    assert_eq!(engine.version(), 2); // Version should NOT increment

    // Query again with unchanged input - should skip computation again
    let result6 = engine
        .tracked()
        .query(&TrackedComputation("x".to_string()))
        .await
        .unwrap();

    assert_eq!(result6, 200); // Same result
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
    async fn execute(
        &self,
        engine: &TrackedEngine,
        key: &AbsVariable,
    ) -> Result<i32, CyclicError> {
        // Increment the call counter to track executor invocations
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Compute absolute value
        let input_value = engine.query(&Variable(key.0.to_string())).await?;

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
    async fn execute(
        &self,
        engine: &TrackedEngine,
        key: &AddTwoAbsVariable,
    ) -> Result<i32, CyclicError> {
        // Increment the call counter to track executor invocations
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Compute sum of absolute values
        Ok(engine.query(&AbsVariable(key.x.clone())).await?
            + engine.query(&AbsVariable(key.y.clone())).await?)
    }
}

#[tokio::test]
async fn skip_when_intermediate_result_unchanged() {
    let mut engine = Arc::new(Engine::default());

    // Set initial inputs - both positive values
    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(Variable("x".to_string()), 400).await;
            x.set_input(Variable("y".to_string()), 300).await;
        })
        .await;

    assert_eq!(engine.version(), 1);

    // Create tracked executors to count invocations
    let abs_executor = Arc::new(TrackedAbsExecutor::default());
    let add_executor = Arc::new(TrackedAddTwoAbsExecutor::default());

    // Register the tracked executors
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(abs_executor.clone());
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(add_executor.clone());

    // First query - should compute everything from scratch
    let result1 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() })
        .await
        .unwrap();
    assert_eq!(result1, 700); // abs(400) + abs(300) = 400 + 300 = 700
    assert_eq!(abs_executor.get_call_count(), 2); // Called for both x and y
    assert_eq!(add_executor.get_call_count(), 1); // Called once

    // Query again with same inputs - should skip all computation
    let result2 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() })
        .await
        .unwrap();

    assert_eq!(result2, 700); // Same result
    assert_eq!(abs_executor.get_call_count(), 2); // NOT called again
    assert_eq!(add_executor.get_call_count(), 1); // NOT called again

    // Change x from 400 to -400 (abs value stays the same)
    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(Variable("x".to_string()), -400).await;
        })
        .await;
    assert_eq!(engine.version(), 2); // Version should increment

    // Query after input change - abs executor should be called for x, but add
    // executor should be skipped because the result of abs(x) hasn't
    // changed
    let result3 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() })
        .await
        .unwrap();
    assert_eq!(result3, 700); // abs(-400) + abs(300) = 400 + 300 = 700 (same result!)
    assert_eq!(abs_executor.get_call_count(), 3); // Called again for x only
    assert_eq!(add_executor.get_call_count(), 1); // NOT called again because abs values are the same

    // Change y from 300 to -300 (abs value stays the same)
    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(Variable("y".to_string()), -300).await;
        })
        .await;
    assert_eq!(engine.version(), 3); // Version should increment again

    // Query after second input change - abs executor should be called for y,
    // but add executor still skipped
    let result4 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() })
        .await
        .unwrap();
    assert_eq!(result4, 700); // abs(-400) + abs(-300) = 400 + 300 = 700 (still same result!)
    assert_eq!(abs_executor.get_call_count(), 4); // Called again for y only
    assert_eq!(add_executor.get_call_count(), 1); // STILL not called because both abs values are the same

    // Now change x to a value that actually changes the abs result
    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(Variable("x".to_string()), 500).await;
        })
        .await;
    assert_eq!(engine.version(), 4); // Version should increment

    // Query after meaningful change - both executors should be called
    let result5 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() })
        .await
        .unwrap();
    assert_eq!(result5, 800); // abs(500) + abs(-300) = 500 + 300 = 800
    assert_eq!(abs_executor.get_call_count(), 5); // Called for x
    assert_eq!(add_executor.get_call_count(), 2); // Finally called again because abs(x) changed

    // Query again - should skip everything
    let result6 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() })
        .await
        .unwrap();
    assert_eq!(result6, 800); // Same result
    assert_eq!(abs_executor.get_call_count(), 5); // NOT called
    assert_eq!(add_executor.get_call_count(), 2); // NOT called
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
#[scc_value(i32::default())]
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
#[scc_value(i32::default())]
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
    async fn execute(
        &self,
        engine: &TrackedEngine,
        _key: &CyclicQueryA,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This creates a cycle: A depends on B, B depends on A
        let b_value = engine.query(&CyclicQueryB).await?;

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
    async fn execute(
        &self,
        engine: &TrackedEngine,
        _key: &CyclicQueryB,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This completes the cycle: B depends on A, A depends on B
        let a_value = engine.query(&CyclicQueryA).await?;
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
    async fn execute(
        &self,
        engine: &TrackedEngine,
        _key: &DependentQuery,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This query depends on the cyclic queries
        let a_value = engine.query(&CyclicQueryA).await?;
        let b_value = engine.query(&CyclicQueryB).await?;

        Ok(a_value + b_value + 100)
    }
}

#[tokio::test]
async fn cyclic_dependency_returns_default_values() {
    let mut engine = Arc::new(Engine::default());

    let executor_a = Arc::new(CyclicExecutorA::default());
    let executor_b = Arc::new(CyclicExecutorB::default());

    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::clone(&executor_a));
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::clone(&executor_b));

    // When we query CyclicQueryA, it should detect the cycle A -> B -> A
    // and return default values (0 for i32) without calling the executors
    let result_a = engine.tracked().query(&CyclicQueryA).await.unwrap();
    let result_b = engine.tracked().query(&CyclicQueryB).await.unwrap();

    // Both should return default values (0 for i32)
    assert_eq!(result_a, 0);
    assert_eq!(result_b, 0);

    // The executors will be called and increment their call counts,
    // but they will receive CyclicError when trying to query their dependencies
    // and return early without completing their computation

    // Both executors should be called exactly once during cycle detection:
    // A is called first, then B is called, then when B tries to call A again,
    // the cycle is detected and CyclicError is returned without calling A again
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);
}

#[tokio::test]
async fn dependent_query_uses_cyclic_default_values() {
    let mut engine = Arc::new(Engine::default());

    let executor_a = Arc::new(CyclicExecutorA::default());
    let executor_b = Arc::new(CyclicExecutorB::default());
    let executor_dependent = Arc::new(DependentExecutor::default());

    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::clone(&executor_a));
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::clone(&executor_b));
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::clone(&executor_dependent));

    // Query the dependent query, which depends on the cyclic queries
    let result = engine.tracked().query(&DependentQuery).await.unwrap();

    // DependentQuery should execute and use the default values from the cyclic
    // queries Default values: CyclicQueryA = 0, CyclicQueryB = 0
    // DependentQuery = 0 + 0 + 100 = 100
    assert_eq!(result, 100);

    // When DependentQuery queries CyclicQueryA, the executor for CyclicQueryA
    // will be called and try to query CyclicQueryB, which triggers cycle
    // detection. Both cyclic executors will be called exactly once during
    // cycle detection.
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);

    // The dependent executor should have been called once
    assert_eq!(executor_dependent.get_call_count(), 1);

    // Try calling the dependent query again
    let result_again = engine.tracked().query(&DependentQuery).await.unwrap();

    // It should return the same result without calling the executors again
    assert_eq!(result_again, 100);

    // The call counts should remain the same since the result is cached
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);
    assert_eq!(executor_dependent.get_call_count(), 1);
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
    async fn execute(
        &self,
        engine: &TrackedEngine,
        _key: &DependentQuery,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This query depends on the conditional cyclic queries
        let a_value = engine.query(&ConditionalCyclicQueryA).await?;
        let b_value = engine.query(&ConditionalCyclicQueryB).await?;

        Ok(a_value + b_value + 100)
    }
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
#[scc_value(i32::default())]
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
#[scc_value(i32::default())]
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
    async fn execute(
        &self,
        engine: &TrackedEngine,
        _key: &ConditionalCyclicQueryA,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Read the control variable to determine whether to create a cycle
        let control_value = engine.query(&CycleControlVariable).await?;

        Ok(if control_value == 1 {
            // When control_value is 1, create a cycle by querying B
            let b_value = engine.query(&ConditionalCyclicQueryB).await?;

            b_value + 10
        } else {
            // When control_value is not 1, no cycle - just return a computed
            // value
            control_value * 100
        })
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
    async fn execute(
        &self,
        engine: &TrackedEngine,
        _key: &ConditionalCyclicQueryB,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Read the control variable to determine whether to create a cycle
        let control_value = engine.query(&CycleControlVariable).await?;

        Ok(if control_value == 1 {
            // When control_value is 1, complete the cycle by querying A
            let a_value = engine.query(&ConditionalCyclicQueryA).await?;

            a_value + 20
        } else {
            // When control_value is not 1, no cycle - just return a computed
            // value
            control_value * 200
        })
    }
}

#[tokio::test]
#[allow(clippy::similar_names)]
async fn conditional_cyclic_dependency() {
    let mut engine = Arc::new(Engine::default());

    let executor_a = Arc::new(ConditionalCyclicExecutorA::default());
    let executor_b = Arc::new(ConditionalCyclicExecutorB::default());

    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::clone(&executor_a));
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::clone(&executor_b));

    // Phase 1: Set control value to create NO cycle (control_value != 1)
    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(CycleControlVariable, 5).await;
        })
        .await;

    // Query both A and B - they should compute normal values without cycles
    let result_a =
        engine.tracked().query(&ConditionalCyclicQueryA).await.unwrap();
    let result_b =
        engine.tracked().query(&ConditionalCyclicQueryB).await.unwrap();

    // Expected values: A = 5 * 100 = 500, B = 5 * 200 = 1000
    assert_eq!(result_a, 500);
    assert_eq!(result_b, 1000);

    // Both executors should have been called once each
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);

    // Phase 2: Change control value to CREATE a cycle (control_value == 1)
    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(CycleControlVariable, 1).await;
        })
        .await;

    executor_a.reset_call_count();
    executor_b.reset_call_count();

    // Query A - this should trigger cycle detection and return default values
    let result_a_cyclic =
        engine.tracked().query(&ConditionalCyclicQueryA).await.unwrap();
    let result_b_cyclic =
        engine.tracked().query(&ConditionalCyclicQueryB).await.unwrap();

    // Both should return default values (0 for i32) due to cycle detection
    assert_eq!(result_a_cyclic, 0);
    assert_eq!(result_b_cyclic, 0);

    // Both executors should be called exactly once during cycle detection
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);

    // Phase 3: Change control value back to break the cycle (control_value !=
    // 1)
    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(CycleControlVariable, 3).await;
        })
        .await;

    executor_a.reset_call_count();
    executor_b.reset_call_count();

    // Query both A and B - they should recompute and return normal values again
    let result_a_normal =
        engine.tracked().query(&ConditionalCyclicQueryA).await.unwrap();
    let result_b_normal =
        engine.tracked().query(&ConditionalCyclicQueryB).await.unwrap();

    // Expected values: A = 3 * 100 = 300, B = 3 * 200 = 600
    assert_eq!(result_a_normal, 300);
    assert_eq!(result_b_normal, 600);

    // Both executors should have been called once each for recomputation
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);

    // Phase 4: Create cycle again with a different control value
    // (control_value // == 1)
    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(CycleControlVariable, 1).await;
        })
        .await;

    executor_a.reset_call_count();
    executor_b.reset_call_count();

    // Query A - cycle should be detected again and default values returned
    let result_a_cyclic2 =
        engine.tracked().query(&ConditionalCyclicQueryA).await.unwrap();
    let result_b_cyclic2 =
        engine.tracked().query(&ConditionalCyclicQueryB).await.unwrap();

    // Both should return default values (0 for i32) due to cycle detection
    assert_eq!(result_a_cyclic2, 0);
    assert_eq!(result_b_cyclic2, 0);

    // Both executors should be called exactly once during cycle detection
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);
}

#[tokio::test]
async fn conditional_cyclic_with_dependent_query() {
    let mut engine = Arc::new(Engine::default());

    let executor_a = Arc::new(ConditionalCyclicExecutorA::default());
    let executor_b = Arc::new(ConditionalCyclicExecutorB::default());
    let executor_dependent = Arc::new(ConditionalDependentExecutor::default());

    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::clone(&executor_a));
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::clone(&executor_b));
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::clone(&executor_dependent));

    // Phase 1: No cycle - dependent query should use computed values

    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(CycleControlVariable, 2).await;
        })
        .await;

    let result_dependent =
        engine.tracked().query(&DependentQuery).await.unwrap();

    // DependentQuery = A + B + 100 = (2*100) + (2*200) + 100 = 200 + 400 + 100
    // = 700
    assert_eq!(result_dependent, 700);

    // All executors should have been called
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);
    assert_eq!(executor_dependent.get_call_count(), 1);

    // Phase 2: Create cycle - dependent query should use default values

    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(CycleControlVariable, 1).await;
        })
        .await;

    executor_a.reset_call_count();
    executor_b.reset_call_count();

    // Reset dependent executor call count to track new computation
    executor_dependent.call_count.store(0, std::sync::atomic::Ordering::SeqCst);

    let result_dependent_cyclic =
        engine.tracked().query(&DependentQuery).await.unwrap();

    // DependentQuery should use default values: 0 + 0 + 100 = 100
    assert_eq!(result_dependent_cyclic, 100);

    // Cyclic executors called once each during cycle detection
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);
    // Dependent executor called once to compute with new (default) values
    assert_eq!(executor_dependent.get_call_count(), 1);

    // Phase 3: Break cycle again - dependent query should use computed values

    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(CycleControlVariable, 4).await;
        })
        .await;

    executor_a.reset_call_count();
    executor_b.reset_call_count();
    executor_dependent.call_count.store(0, std::sync::atomic::Ordering::SeqCst); // Query A and B to ensure they're computed
    let _debug_a = engine.tracked().query(&ConditionalCyclicQueryA).await;
    let _debug_b = engine.tracked().query(&ConditionalCyclicQueryB).await;

    let result_dependent_normal =
        engine.tracked().query(&DependentQuery).await.unwrap();

    // DependentQuery = A + B + 100 = (4*100) + (4*200) + 100 = 400 + 800 + 100
    // = 1300
    assert_eq!(result_dependent_normal, 1300);

    // All executors should have been called for recomputation
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);
    assert_eq!(executor_dependent.get_call_count(), 1);
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
#[value(Arc<HashMap<String, i32>>)]
pub struct VariableMap;

#[derive(Debug, Default)]
pub struct VariableMapExecutor {
    pub call_count: AtomicUsize,
}

impl Executor<VariableMap> for VariableMapExecutor {
    async fn execute(
        &self,
        _: &TrackedEngine,
        _: &VariableMap,
    ) -> Result<Arc<HashMap<String, i32>>, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Simulate a complex computation that returns a map of variables
        let mut result = HashMap::default();
        result.insert("a".to_string(), 1);
        result.insert("b".to_string(), 2);
        result.insert("c".to_string(), 3);

        Ok(Arc::new(result))
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

#[derive(Debug)]
pub struct GetValueExecutor {
    pub call_count: AtomicUsize,
}

impl Executor<GetValue> for GetValueExecutor {
    async fn execute(
        &self,
        engine: &TrackedEngine,
        key: &GetValue,
    ) -> Result<Option<i32>, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Retrieve the value from the VariableMap
        let variable_map = engine.query(&VariableMap).await?;
        Ok(variable_map.get(&key.0).copied())
    }
}

#[tokio::test]
async fn persistence_variable_map_query() {
    let mut engine = Arc::new(Engine::default());

    // Create and register the VariableMapExecutor
    let variable_map_executor = Arc::new(VariableMapExecutor::default());
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(variable_map_executor.clone());

    // Create and register the GetValueExecutor
    let get_value_executor =
        Arc::new(GetValueExecutor { call_count: AtomicUsize::new(0) });
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(get_value_executor.clone());

    let mut serde_extension = SelfRegistry::default();
    serde_extension.register::<VariableMap>();
    serde_extension.register::<GetValue>();

    let tempdir = tempfile::tempdir().unwrap();

    let mut persistence = Persistence::new(
        tempdir.path().to_path_buf(),
        Arc::new(serde_extension),
    )
    .unwrap();
    persistence.skip_cache_value::<GetValue>();

    Arc::get_mut(&mut engine).unwrap().runtime.persistence = Some(persistence);

    let a = engine.tracked().query(&GetValue("a".to_string())).await.unwrap();
    let b = engine.tracked().query(&GetValue("b".to_string())).await.unwrap();
    let c = engine.tracked().query(&GetValue("c".to_string())).await.unwrap();

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
    Arc::get_mut(&mut engine).unwrap().save_database().unwrap();

    // load the persistence
    let database = Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .persistence
        .as_ref()
        .unwrap()
        .load_database()
        .unwrap();
    Arc::get_mut(&mut engine).unwrap().database = database;

    // reset the call counts
    variable_map_executor
        .call_count
        .store(0, std::sync::atomic::Ordering::SeqCst);
    get_value_executor.call_count.store(0, std::sync::atomic::Ordering::SeqCst);

    // Query again after loading from persistence
    let a2 = engine.tracked().query(&GetValue("a".to_string())).await.unwrap();
    let b2 = engine.tracked().query(&GetValue("b".to_string())).await.unwrap();
    let c2 = engine.tracked().query(&GetValue("c".to_string())).await.unwrap();

    assert_eq!(a2, a);
    assert_eq!(b2, b);
    assert_eq!(c2, c);

    // The GetValueExecutor should be called again for each variable
    // as it's skipped in persistence
    assert_eq!(
        get_value_executor.call_count.load(std::sync::atomic::Ordering::SeqCst),
        3
    ); // Called for a, b, c again

    // The VariableMapExecutor should NOT have been called again
    assert_eq!(
        variable_map_executor
            .call_count
            .load(std::sync::atomic::Ordering::SeqCst),
        0
    ); // Should be cached
}

#[tokio::test]
#[allow(clippy::too_many_lines)]
async fn persistence_input_invalidation() {
    let mut engine = Arc::new(Engine::default());

    // Create and register the GetValueExecutor
    let get_value_executor =
        Arc::new(GetValueExecutor { call_count: AtomicUsize::new(0) });
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(get_value_executor.clone());

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
    persistence.skip_cache_value::<VariableMap>();

    Arc::get_mut(&mut engine).unwrap().runtime.persistence = Some(persistence);

    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(
                VariableMap,
                Arc::new(
                    [
                        ("a".to_string(), 1),
                        ("b".to_string(), 2),
                        ("c".to_string(), 3),
                    ]
                    .into_iter()
                    .collect(),
                ),
            )
            .await;
        })
        .await;

    let a = engine.tracked().query(&GetValue("a".to_string())).await.unwrap();
    let b = engine.tracked().query(&GetValue("b".to_string())).await.unwrap();
    let c = engine.tracked().query(&GetValue("c".to_string())).await.unwrap();

    assert_eq!(a, Some(1));
    assert_eq!(b, Some(2));
    assert_eq!(c, Some(3));

    assert_eq!(
        get_value_executor.call_count.load(std::sync::atomic::Ordering::SeqCst),
        3
    ); // Called for a, b, c

    let first_version = engine.version();

    // save to persistence
    Arc::get_mut(&mut engine).unwrap().save_database().unwrap();

    // load the persistence
    let database = Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .persistence
        .as_ref()
        .unwrap()
        .load_database()
        .unwrap();
    Arc::get_mut(&mut engine).unwrap().database = database;

    // reset the call counts
    get_value_executor.call_count.store(0, std::sync::atomic::Ordering::SeqCst);

    // Query again after loading from persistence
    let a2 = engine.tracked().query(&GetValue("a".to_string())).await.unwrap();
    let b2 = engine.tracked().query(&GetValue("b".to_string())).await.unwrap();
    let c2 = engine.tracked().query(&GetValue("c".to_string())).await.unwrap();

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
    assert_eq!(engine.version(), first_version);

    // save the current state to persistence
    Arc::get_mut(&mut engine).unwrap().save_database().unwrap();

    // load it back again
    let database = Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .persistence
        .as_ref()
        .unwrap()
        .load_database()
        .unwrap();
    Arc::get_mut(&mut engine).unwrap().database = database;

    // change the input for VariableMap
    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(
                VariableMap,
                Arc::new(
                    [
                        ("a".to_string(), 10),
                        ("b".to_string(), 20),
                        ("c".to_string(), 30),
                    ]
                    .into_iter()
                    .collect(),
                ),
            )
            .await;
        })
        .await;

    // Query again after changing the input
    let a3 = engine.tracked().query(&GetValue("a".to_string())).await.unwrap();
    let b3 = engine.tracked().query(&GetValue("b".to_string())).await.unwrap();
    let c3 = engine.tracked().query(&GetValue("c".to_string())).await.unwrap();

    assert_eq!(a3, Some(10));
    assert_eq!(b3, Some(20));
    assert_eq!(c3, Some(30));

    // The GetValueExecutor should be called again for each variable
    assert_eq!(
        get_value_executor.call_count.load(std::sync::atomic::Ordering::SeqCst),
        3
    ); // Called for a, b, c again

    // version should be incremented
    assert!(engine.version() > first_version);
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Key,
    Serialize,
    Deserialize,
    StableHash,
)]
#[value(i32)]
#[always_reverify]
pub struct ImpureKey;

#[derive(Debug)]
pub struct ImpureExecutor {
    pub call_count: AtomicI32,
}

impl Executor<ImpureKey> for ImpureExecutor {
    async fn execute(
        &self,
        _engine: &TrackedEngine,
        _key: &ImpureKey,
    ) -> Result<i32, CyclicError> {
        // Increment the call counter to track executor invocations
        let value =
            self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // The value is based on the external state, so it can change
        // between calls, simulating an impure function
        Ok(value)
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    Key,
)]
#[value(i32)]
pub struct DoubleImpureKey;

#[derive(Debug)]
pub struct DoubleImpureExecutor {
    pub call_count: AtomicI32,
}

impl Executor<DoubleImpureKey> for DoubleImpureExecutor {
    async fn execute(
        &self,
        engine: &TrackedEngine,
        _: &DoubleImpureKey,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        let value = engine.query(&ImpureKey).await?;

        Ok(value * 2)
    }
}

#[tokio::test]
async fn always_reverify_impure() {
    let mut engine = Arc::new(Engine::default());
    let mut serde_extension = SelfRegistry::default();

    serde_extension.register::<ImpureKey>();
    serde_extension.register::<DoubleImpureKey>();

    let tempdir = tempfile::tempdir().unwrap();
    let mut persistence = Persistence::new(
        tempdir.path().to_path_buf(),
        Arc::new(serde_extension),
    )
    .unwrap();

    persistence.skip_cache_value::<ImpureKey>();

    Arc::get_mut(&mut engine).unwrap().runtime.persistence = Some(persistence);

    let impure_executor =
        Arc::new(ImpureExecutor { call_count: AtomicI32::new(0) });
    let double_impure_executor =
        Arc::new(DoubleImpureExecutor { call_count: AtomicI32::new(0) });

    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::clone(&impure_executor));
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::clone(&double_impure_executor));

    let result = engine.tracked().query(&DoubleImpureKey).await;

    assert_eq!(result.unwrap(), 0); // First call, should return 0

    let result = engine.tracked().query(&DoubleImpureKey).await;

    assert_eq!(result.unwrap(), 0); // Second call, should still return 0

    assert_eq!(
        impure_executor.call_count.load(std::sync::atomic::Ordering::SeqCst),
        1
    ); // Impure executor called once
    assert_eq!(
        double_impure_executor
            .call_count
            .load(std::sync::atomic::Ordering::SeqCst),
        1
    );

    // Now forcefully increment the database version
    Arc::get_mut(&mut engine).unwrap().increment_version();

    let result = engine.tracked().query(&DoubleImpureKey).await.unwrap();
    assert_eq!(result, 2);
    let result = engine.tracked().query(&DoubleImpureKey).await.unwrap();
    assert_eq!(result, 2); // Should still return 2

    assert_eq!(
        impure_executor.call_count.load(std::sync::atomic::Ordering::SeqCst),
        2
    ); // Impure executor called again
    assert_eq!(
        double_impure_executor
            .call_count
            .load(std::sync::atomic::Ordering::SeqCst),
        2
    ); // Double impure executor called again

    // Save the database to persistence
    Arc::get_mut(&mut engine).unwrap().save_database().unwrap();

    // load the persistence
    Arc::get_mut(&mut engine).unwrap().database = Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .persistence
        .as_ref()
        .unwrap()
        .load_database()
        .unwrap();

    // Forcefully increment the database version again
    Arc::get_mut(&mut engine).unwrap().increment_version();

    let result = engine.tracked().query(&DoubleImpureKey).await.unwrap();
    assert_eq!(result, 4); // Should return 4 after re-evaluation
    let result = engine.tracked().query(&DoubleImpureKey).await.unwrap();
    assert_eq!(result, 4); // Should still return 4

    assert_eq!(
        impure_executor.call_count.load(std::sync::atomic::Ordering::SeqCst),
        3
    ); // Impure executor called again
    assert_eq!(
        double_impure_executor
            .call_count
            .load(std::sync::atomic::Ordering::SeqCst),
        3
    ); // Double impure executor called again
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Key,
    StableHash,
)]
#[value(Arc<[i32]>)]
pub struct DependencyListKey;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Key,
    StableHash,
)]
#[value(i32)]
#[always_reverify]
pub struct DoubleKey(pub i32);

#[derive(Debug, Default)]
pub struct DoubleKeyExecutor {
    pub executed: DashSet<i32>,
}

impl Executor<DoubleKey> for DoubleKeyExecutor {
    async fn execute(
        &self,
        _engine: &TrackedEngine,
        key: &DoubleKey,
    ) -> Result<i32, CyclicError> {
        // Simulate some computation
        let value = key.0 * 2;

        // Track which keys have been executed
        self.executed.insert(key.0);

        Ok(value)
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Key,
    StableHash,
)]
#[value(i32)]
pub struct DoubleAll;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct DoubleAllExecutor;

impl Executor<DoubleAll> for DoubleAllExecutor {
    async fn execute(
        &self,
        engine: &TrackedEngine,
        _key: &DoubleAll,
    ) -> Result<i32, CyclicError> {
        // Query the DependencyListKey to get the list of dependencies
        let dependencies = engine.query(&DependencyListKey).await?;
        let mut total = 0;

        for &value in dependencies.iter() {
            // For each dependency, query the DoubleKey executor
            let double_value = engine.query(&DoubleKey(value)).await?;
            total += double_value;
        }

        Ok(total)
    }
}

#[tokio::test]
async fn reverify_change_dependencies() {
    let mut engine = Arc::new(Engine::default());

    // Register the executors
    let double_key_executor = Arc::new(DoubleKeyExecutor::default());
    let double_all_executor = Arc::new(DoubleAllExecutor);

    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::clone(&double_key_executor));
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::clone(&double_all_executor));

    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            // Set initial dependencies
            x.set_input(DependencyListKey, Arc::new([1, 2, 3, 4])).await;
        })
        .await;

    // Query DoubleAll, which should compute the sum of double values
    let result = engine.tracked().query(&DoubleAll).await.unwrap();
    assert_eq!(result, 20); // (1*2) + (2*2) + (3*2) + (4*2) = 20

    // Check that the DoubleKey executor was called for each dependency
    assert_eq!(
        double_key_executor
            .executed
            .iter()
            .map(|x| *x.key())
            .collect::<HashSet<_>>(),
        [1, 2, 3, 4].into_iter().collect()
    );

    // Reset the executed set for the next test
    double_key_executor.executed.clear();

    // Change the dependencies to a new set
    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(DependencyListKey, Arc::new([5, 6, 7])).await;
        })
        .await;

    // Query DoubleAll again, which should now compute the new dependencies
    let result = engine.tracked().query(&DoubleAll).await.unwrap();
    assert_eq!(result, 36); // (5*2) + (6*2) + (7*2) = 36

    double_key_executor.executed.clear();

    // increment the version to force re-verification for `always_reverify`
    Arc::get_mut(&mut engine).unwrap().increment_version();

    // Query DoubleAll again, which should recompute the dependencies
    let result = engine.tracked().query(&DoubleAll).await.unwrap();

    assert_eq!(result, 36); // Should still return 36 since dependencies didn't change

    // Check that the DoubleKey executor was called again for the new
    // dependencies
    assert_eq!(
        double_key_executor
            .executed
            .iter()
            .map(|x| *x.key())
            .collect::<HashSet<_>>(),
        [5, 6, 7].into_iter().collect()
    ); // Only the new dependencies should be executed
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, StableHash, Key)]
#[value(Arc<str>)]
pub struct ReadKey;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, StableHash, Key)]
#[value(i32)]
pub struct GetVariableMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct GetVariableMapExecutor;

impl Executor<GetVariableMap> for GetVariableMapExecutor {
    async fn execute(
        &self,
        engine: &TrackedEngine,
        _key: &GetVariableMap,
    ) -> Result<i32, CyclicError> {
        let read_key = engine.query(&ReadKey).await?;
        engine.query(&IndexValue(read_key)).await
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default, StableHash, Key)]
#[value(i32)]
pub struct IndexValue(pub Arc<str>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct IndexValueExecutor;

impl Executor<IndexValue> for IndexValueExecutor {
    async fn execute(
        &self,
        engine: &TrackedEngine,
        key: &IndexValue,
    ) -> Result<i32, CyclicError> {
        // Read the VariableMap and return the value for the given key
        let variable_map = engine.query(&VariableMap).await?;
        let value = variable_map.get(key.0.as_ref()).unwrap();

        Ok(*value)
    }
}

#[tokio::test]
async fn panic_indexing() {
    let mut engine = Arc::new(Engine::default());

    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(
                VariableMap,
                Arc::new(
                    [
                        ("a".to_string(), 1),
                        ("b".to_string(), 2),
                        ("c".to_string(), 3),
                    ]
                    .into_iter()
                    .collect(),
                ),
            )
            .await;

            x.set_input(ReadKey, "a".to_string().into()).await;
        })
        .await;

    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::new(GetVariableMapExecutor));
    Arc::get_mut(&mut engine)
        .unwrap()
        .runtime
        .executor
        .register(Arc::new(IndexValueExecutor));

    // Query the GetVariableMap executor
    let result = engine.tracked().query(&GetVariableMap).await;

    assert_eq!(result.unwrap(), 1); // Should return the value for "a"

    Arc::get_mut(&mut engine)
        .unwrap()
        .input_session(async |x| {
            x.set_input(
                VariableMap,
                Arc::new(
                    [
                        ("c".to_string(), 1),
                        ("d".to_string(), 2),
                        ("e".to_string(), 3),
                    ]
                    .into_iter()
                    .collect(),
                ),
            )
            .await;

            x.set_input(ReadKey, "e".to_string().into()).await;
        })
        .await;

    // Query again after changing the VariableMap
    let result = engine.tracked().query(&GetVariableMap).await;

    assert_eq!(result.unwrap(), 3); // Should return the value for "e"
}
