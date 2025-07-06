use std::sync::{atomic::AtomicUsize, Arc};

use pernixc_hash::HashMap;
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
    fn execute(
        &self,
        engine: &TrackedEngine,
        key: &NegateVariable,
    ) -> Result<Arc<i32>, CyclicError> {
        Ok(Arc::new(-*engine.query(&Variable(key.0.to_string()))?))
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
        engine: &TrackedEngine,
        key: &SumNegatedVariable,
    ) -> Result<Arc<i32>, CyclicError> {
        let a = engine.query(&NegateVariable(key.a.clone()))?;
        let b = engine.query(&NegateVariable(key.b.clone()))?;

        Ok(Arc::new(*a + *b))
    }
}

#[test]
fn negate_variable() {
    let mut engine = Engine::default();

    engine.input_session(|x| {
        x.set_input(Variable("a".to_string()), Arc::new(100));
        x.set_input(Variable("b".to_string()), Arc::new(200));
    });

    assert_eq!(engine.version(), 1);

    engine.runtime.executor.register(Arc::new(NegateVariableExecutor));
    engine.runtime.executor.register(Arc::new(SumNegatedVariableExecutor));

    let value = engine
        .tracked()
        .query(&SumNegatedVariable { a: "a".to_string(), b: "b".to_string() })
        .unwrap();

    assert_eq!(*value, -300);

    engine.input_session(|x| {
        x.set_input(Variable("a".to_string()), Arc::new(200));
        x.set_input(Variable("b".to_string()), Arc::new(300));
    });

    assert_eq!(engine.version(), 2);

    let value = engine
        .tracked()
        .query(&SumNegatedVariable { a: "a".to_string(), b: "b".to_string() })
        .unwrap();

    assert_eq!(*value, -500);

    engine.input_session(|x| {
        x.set_input(Variable("a".to_string()), Arc::new(-300));
        x.set_input(Variable("b".to_string()), Arc::new(-300));
    });

    assert_eq!(engine.version(), 3);
    let value = engine
        .tracked()
        .query(&SumNegatedVariable { a: "a".to_string(), b: "b".to_string() })
        .unwrap();

    assert_eq!(*value, 600); // -(-300) + -(-300) = 300 + 300 = 600
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
        engine: &TrackedEngine,
        key: &TrackedComputation,
    ) -> Result<Arc<i32>, CyclicError> {
        // Increment the call counter to track executor invocations
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Perform computation based on input variable
        let input_value = *engine.query(&Variable(key.0.clone()))?;
        Ok(Arc::new(input_value * 2))
    }
}

#[test]
fn skip_when_input_unchanged() {
    let mut engine = Engine::default();

    // set the initial input
    engine.input_session(|x| {
        x.set_input(Variable("x".to_string()), Arc::new(42));
    });

    assert_eq!(engine.version(), 1);

    // Create tracked executor to count invocations
    let tracked_executor = TrackedExecutor::default();
    let executor_arc = Arc::new(tracked_executor);

    // Register the tracked executor
    engine.runtime.executor.register(executor_arc.clone());

    // First query - should compute and call executor
    let result1 =
        engine.tracked().query(&TrackedComputation("x".to_string())).unwrap();
    assert_eq!(*result1, 84); // 42 * 2
    assert_eq!(executor_arc.get_call_count(), 1);

    // Second query with same input - should skip computation and return cached
    // result
    let result2 =
        engine.tracked().query(&TrackedComputation("x".to_string())).unwrap();

    assert_eq!(*result2, 84); // Same result
    assert_eq!(executor_arc.get_call_count(), 1); // Executor NOT called again

    // Now change the input - should trigger recomputation
    engine.input_session(|x| {
        x.set_input(Variable("x".to_string()), Arc::new(100));
    });

    assert_eq!(engine.version(), 2); // Version should increment

    // Query after input change - should compute and call executor again
    let result4 =
        engine.tracked().query(&TrackedComputation("x".to_string())).unwrap();
    assert_eq!(*result4, 200); // 100 * 2
    assert_eq!(executor_arc.get_call_count(), 2); // Executor called again

    // Query again with unchanged input - should skip computation again
    let result5 =
        engine.tracked().query(&TrackedComputation("x".to_string())).unwrap();
    assert_eq!(*result5, 200); // Same result
    assert_eq!(executor_arc.get_call_count(), 2); // Executor NOT called again

    engine.input_session(|x| {
        // Set input again but same value
        x.set_input(Variable("x".to_string()), Arc::new(100));
    });

    assert_eq!(engine.version(), 2); // Version should NOT increment

    // Query again with unchanged input - should skip computation again
    let result6 =
        engine.tracked().query(&TrackedComputation("x".to_string())).unwrap();

    assert_eq!(*result6, 200); // Same result
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
        engine: &TrackedEngine,
        key: &AbsVariable,
    ) -> Result<Arc<i32>, CyclicError> {
        // Increment the call counter to track executor invocations
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Compute absolute value
        let input_value = engine.query(&Variable(key.0.to_string()))?;

        Ok(Arc::new(input_value.abs()))
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
        engine: &TrackedEngine,
        key: &AddTwoAbsVariable,
    ) -> Result<Arc<i32>, CyclicError> {
        // Increment the call counter to track executor invocations
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Compute sum of absolute values
        Ok(Arc::new(
            *engine.query(&AbsVariable(key.x.clone()))?
                + *engine.query(&AbsVariable(key.y.clone()))?,
        ))
    }
}

#[test]
fn skip_when_intermediate_result_unchanged() {
    let mut engine = Engine::default();

    // Set initial inputs - both positive values
    engine.input_session(|x| {
        x.set_input(Variable("x".to_string()), Arc::new(400));
        x.set_input(Variable("y".to_string()), Arc::new(300));
    });

    assert_eq!(engine.version(), 1);

    // Create tracked executors to count invocations
    let abs_executor = Arc::new(TrackedAbsExecutor::default());
    let add_executor = Arc::new(TrackedAddTwoAbsExecutor::default());

    // Register the tracked executors
    engine.runtime.executor.register(abs_executor.clone());
    engine.runtime.executor.register(add_executor.clone());

    // First query - should compute everything from scratch
    let result1 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() })
        .unwrap();
    assert_eq!(*result1, 700); // abs(400) + abs(300) = 400 + 300 = 700
    assert_eq!(abs_executor.get_call_count(), 2); // Called for both x and y
    assert_eq!(add_executor.get_call_count(), 1); // Called once

    // Query again with same inputs - should skip all computation
    let result2 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() })
        .unwrap();

    assert_eq!(*result2, 700); // Same result
    assert_eq!(abs_executor.get_call_count(), 2); // NOT called again
    assert_eq!(add_executor.get_call_count(), 1); // NOT called again

    // Change x from 400 to -400 (abs value stays the same)
    engine.input_session(|x| {
        x.set_input(Variable("x".to_string()), Arc::new(-400));
    });
    assert_eq!(engine.version(), 2); // Version should increment

    // Query after input change - abs executor should be called for x, but add
    // executor should be skipped because the result of abs(x) hasn't
    // changed
    let result3 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() })
        .unwrap();
    assert_eq!(*result3, 700); // abs(-400) + abs(300) = 400 + 300 = 700 (same result!)
    assert_eq!(abs_executor.get_call_count(), 3); // Called again for x only
    assert_eq!(add_executor.get_call_count(), 1); // NOT called again because abs values are the same

    // Change y from 300 to -300 (abs value stays the same)
    engine.input_session(|x| {
        x.set_input(Variable("y".to_string()), Arc::new(-300));
    });
    assert_eq!(engine.version(), 3); // Version should increment again

    // Query after second input change - abs executor should be called for y,
    // but add executor still skipped
    let result4 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() })
        .unwrap();
    assert_eq!(*result4, 700); // abs(-400) + abs(-300) = 400 + 300 = 700 (still same result!)
    assert_eq!(abs_executor.get_call_count(), 4); // Called again for y only
    assert_eq!(add_executor.get_call_count(), 1); // STILL not called because both abs values are the same

    // Now change x to a value that actually changes the abs result
    engine.input_session(|x| {
        x.set_input(Variable("x".to_string()), Arc::new(500));
    });
    assert_eq!(engine.version(), 4); // Version should increment

    // Query after meaningful change - both executors should be called
    let result5 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() })
        .unwrap();
    assert_eq!(*result5, 800); // abs(500) + abs(-300) = 500 + 300 = 800
    assert_eq!(abs_executor.get_call_count(), 5); // Called for x
    assert_eq!(add_executor.get_call_count(), 2); // Finally called again because abs(x) changed

    // Query again - should skip everything
    let result6 = engine
        .tracked()
        .query(&AddTwoAbsVariable { x: "x".to_string(), y: "y".to_string() })
        .unwrap();
    assert_eq!(*result6, 800); // Same result
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
#[scc_value(Arc::default())]
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
#[scc_value(Arc::default())]
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
        engine: &TrackedEngine,
        _key: &CyclicQueryA,
    ) -> Result<Arc<i32>, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This creates a cycle: A depends on B, B depends on A
        let b_value = *engine.query(&CyclicQueryB)?;

        Ok(Arc::new(b_value + 10))
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
        engine: &TrackedEngine,
        _key: &CyclicQueryB,
    ) -> Result<Arc<i32>, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This completes the cycle: B depends on A, A depends on B
        let a_value = *engine.query(&CyclicQueryA)?;
        Ok(Arc::new(a_value + 20))
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
        engine: &TrackedEngine,
        _key: &DependentQuery,
    ) -> Result<Arc<i32>, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This query depends on the cyclic queries
        let a_value = *engine.query(&CyclicQueryA)?;
        let b_value = *engine.query(&CyclicQueryB)?;

        Ok(Arc::new(a_value + b_value + 100))
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
    let result_a = *engine.tracked().query(&CyclicQueryA).unwrap();
    let result_b = *engine.tracked().query(&CyclicQueryB).unwrap();

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
    let result = *engine.tracked().query(&DependentQuery).unwrap();

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
    let result_again = *engine.tracked().query(&DependentQuery).unwrap();

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
    fn execute(
        &self,
        engine: &TrackedEngine,
        _key: &DependentQuery,
    ) -> Result<Arc<i32>, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This query depends on the conditional cyclic queries
        let a_value = *engine.query(&ConditionalCyclicQueryA)?;
        let b_value = *engine.query(&ConditionalCyclicQueryB)?;

        Ok(Arc::new(a_value + b_value + 100))
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
#[scc_value(Arc::default())]
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
#[scc_value(Arc::default())]
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
        engine: &TrackedEngine,
        _key: &ConditionalCyclicQueryA,
    ) -> Result<Arc<i32>, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Read the control variable to determine whether to create a cycle
        let control_value = *engine.query(&CycleControlVariable)?;

        Ok(Arc::new(if control_value == 1 {
            // When control_value is 1, create a cycle by querying B
            let b_value = *engine.query(&ConditionalCyclicQueryB)?;

            b_value + 10
        } else {
            // When control_value is not 1, no cycle - just return a computed
            // value
            control_value * 100
        }))
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
        engine: &TrackedEngine,
        _key: &ConditionalCyclicQueryB,
    ) -> Result<Arc<i32>, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Read the control variable to determine whether to create a cycle
        let control_value = *engine.query(&CycleControlVariable)?;

        Ok(Arc::new(if control_value == 1 {
            // When control_value is 1, complete the cycle by querying A
            let a_value = *engine.query(&ConditionalCyclicQueryA)?;

            a_value + 20
        } else {
            // When control_value is not 1, no cycle - just return a computed
            // value
            control_value * 200
        }))
    }
}

#[test]
#[allow(clippy::similar_names)]
fn conditional_cyclic_dependency() {
    let mut engine = Engine::default();

    let executor_a = Arc::new(ConditionalCyclicExecutorA::default());
    let executor_b = Arc::new(ConditionalCyclicExecutorB::default());

    engine.runtime.executor.register(Arc::clone(&executor_a));
    engine.runtime.executor.register(Arc::clone(&executor_b));

    // Phase 1: Set control value to create NO cycle (control_value != 1)
    engine.input_session(|x| {
        x.set_input(CycleControlVariable, Arc::new(5));
    });

    // Query both A and B - they should compute normal values without cycles
    let result_a = *engine.tracked().query(&ConditionalCyclicQueryA).unwrap();
    let result_b = *engine.tracked().query(&ConditionalCyclicQueryB).unwrap();

    // Expected values: A = 5 * 100 = 500, B = 5 * 200 = 1000
    assert_eq!(result_a, 500);
    assert_eq!(result_b, 1000);

    // Both executors should have been called once each
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);

    // Phase 2: Change control value to CREATE a cycle (control_value == 1)
    engine.input_session(|x| {
        x.set_input(CycleControlVariable, Arc::new(1));
    });

    executor_a.reset_call_count();
    executor_b.reset_call_count();

    // Query A - this should trigger cycle detection and return default values
    let result_a_cyclic =
        *engine.tracked().query(&ConditionalCyclicQueryA).unwrap();
    let result_b_cyclic =
        *engine.tracked().query(&ConditionalCyclicQueryB).unwrap();

    // Both should return default values (0 for i32) due to cycle detection
    assert_eq!(result_a_cyclic, 0);
    assert_eq!(result_b_cyclic, 0);

    // Both executors should be called exactly once during cycle detection
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);

    // Phase 3: Change control value back to break the cycle (control_value !=
    // 1)
    engine.input_session(|x| {
        x.set_input(CycleControlVariable, Arc::new(3));
    });

    executor_a.reset_call_count();
    executor_b.reset_call_count();

    // Query both A and B - they should recompute and return normal values again
    let result_a_normal =
        *engine.tracked().query(&ConditionalCyclicQueryA).unwrap();
    let result_b_normal =
        *engine.tracked().query(&ConditionalCyclicQueryB).unwrap();

    // Expected values: A = 3 * 100 = 300, B = 3 * 200 = 600
    assert_eq!(result_a_normal, 300);
    assert_eq!(result_b_normal, 600);

    // Both executors should have been called once each for recomputation
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);

    // Phase 4: Create cycle again with a different control value
    // (control_value // == 1)
    engine.input_session(|x| {
        x.set_input(CycleControlVariable, Arc::new(1));
    });

    executor_a.reset_call_count();
    executor_b.reset_call_count();

    // Query A - cycle should be detected again and default values returned
    let result_a_cyclic2 =
        *engine.tracked().query(&ConditionalCyclicQueryA).unwrap();
    let result_b_cyclic2 =
        *engine.tracked().query(&ConditionalCyclicQueryB).unwrap();

    // Both should return default values (0 for i32) due to cycle detection
    assert_eq!(result_a_cyclic2, 0);
    assert_eq!(result_b_cyclic2, 0);

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

    engine.input_session(|x| {
        x.set_input(CycleControlVariable, Arc::new(2));
    });

    let result_dependent = *engine.tracked().query(&DependentQuery).unwrap();

    // DependentQuery = A + B + 100 = (2*100) + (2*200) + 100 = 200 + 400 + 100
    // = 700
    assert_eq!(result_dependent, 700);

    // All executors should have been called
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);
    assert_eq!(executor_dependent.get_call_count(), 1);

    // Phase 2: Create cycle - dependent query should use default values

    engine.input_session(|x| {
        x.set_input(CycleControlVariable, Arc::new(1));
    });

    executor_a.reset_call_count();
    executor_b.reset_call_count();

    // Reset dependent executor call count to track new computation
    executor_dependent.call_count.store(0, std::sync::atomic::Ordering::SeqCst);

    let result_dependent_cyclic =
        *engine.tracked().query(&DependentQuery).unwrap();

    // DependentQuery should use default values: 0 + 0 + 100 = 100
    assert_eq!(result_dependent_cyclic, 100);

    // Cyclic executors called once each during cycle detection
    assert_eq!(executor_a.get_call_count(), 1);
    assert_eq!(executor_b.get_call_count(), 1);
    // Dependent executor called once to compute with new (default) values
    assert_eq!(executor_dependent.get_call_count(), 1);

    // Phase 3: Break cycle again - dependent query should use computed values

    engine.input_session(|x| {
        x.set_input(CycleControlVariable, Arc::new(4));
    });

    executor_a.reset_call_count();
    executor_b.reset_call_count();
    executor_dependent.call_count.store(0, std::sync::atomic::Ordering::SeqCst); // Query A and B to ensure they're computed
    let _debug_a = engine.tracked().query(&ConditionalCyclicQueryA);
    let _debug_b = engine.tracked().query(&ConditionalCyclicQueryB);

    let result_dependent_normal =
        *engine.tracked().query(&DependentQuery).unwrap();

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
#[value(HashMap<String, i32>)]
pub struct VariableMap;

#[derive(Debug, Default)]
pub struct VariableMapExecutor {
    pub call_count: AtomicUsize,
}

impl Executor<VariableMap> for VariableMapExecutor {
    fn execute(
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
    fn execute(
        &self,
        engine: &TrackedEngine,
        key: &GetValue,
    ) -> Result<Arc<Option<i32>>, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Retrieve the value from the VariableMap
        let variable_map = engine.query(&VariableMap)?;
        Ok(Arc::new(variable_map.get(&key.0).copied()))
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
    persistence.skip_cache_value::<GetValue>();

    engine.runtime.persistence = Some(persistence);

    let a = *engine.tracked().query(&GetValue("a".to_string())).unwrap();
    let b = *engine.tracked().query(&GetValue("b".to_string())).unwrap();
    let c = *engine.tracked().query(&GetValue("c".to_string())).unwrap();

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
    engine.save_database().unwrap();

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
    let a2 = *engine.tracked().query(&GetValue("a".to_string())).unwrap();
    let b2 = *engine.tracked().query(&GetValue("b".to_string())).unwrap();
    let c2 = *engine.tracked().query(&GetValue("c".to_string())).unwrap();

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
    persistence.skip_cache_value::<VariableMap>();

    engine.runtime.persistence = Some(persistence);

    engine.input_session(|x| {
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
        );
    });

    let a = *engine.tracked().query(&GetValue("a".to_string())).unwrap();
    let b = *engine.tracked().query(&GetValue("b".to_string())).unwrap();
    let c = *engine.tracked().query(&GetValue("c".to_string())).unwrap();

    assert_eq!(a, Some(1));
    assert_eq!(b, Some(2));
    assert_eq!(c, Some(3));

    assert_eq!(
        get_value_executor.call_count.load(std::sync::atomic::Ordering::SeqCst),
        3
    ); // Called for a, b, c

    let first_version = engine.version();

    // save to persistence
    engine.save_database().unwrap();

    // load the persistence
    let database =
        engine.runtime.persistence.as_ref().unwrap().load_database().unwrap();
    engine.database = database;

    // reset the call counts
    get_value_executor.call_count.store(0, std::sync::atomic::Ordering::SeqCst);

    // Query again after loading from persistence
    let a2 = *engine.tracked().query(&GetValue("a".to_string())).unwrap();
    let b2 = *engine.tracked().query(&GetValue("b".to_string())).unwrap();
    let c2 = *engine.tracked().query(&GetValue("c".to_string())).unwrap();

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
    engine.save_database().unwrap();

    // load it back again
    let database =
        engine.runtime.persistence.as_ref().unwrap().load_database().unwrap();
    engine.database = database;

    // change the input for VariableMap
    engine.input_session(|x| {
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
        );
    });

    // Query again after changing the input
    let a3 = *engine.tracked().query(&GetValue("a".to_string())).unwrap();
    let b3 = *engine.tracked().query(&GetValue("b".to_string())).unwrap();
    let c3 = *engine.tracked().query(&GetValue("c".to_string())).unwrap();

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
