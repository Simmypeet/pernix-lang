use std::sync::{atomic::AtomicUsize, Arc};

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    database::TrackedEngine,
    runtime::executor::{CyclicError, Executor},
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
        engine: &mut TrackedEngine,
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
        engine: &mut TrackedEngine,
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
    let input_lock = engine.input_lock();

    input_lock.set_input(Variable("a".to_string()), Arc::new(100));
    input_lock.set_input(Variable("b".to_string()), Arc::new(200));

    drop(input_lock);

    assert_eq!(engine.version(), 1);

    engine.runtime.executor.register(Arc::new(NegateVariableExecutor));
    engine.runtime.executor.register(Arc::new(SumNegatedVariableExecutor));

    let value = engine
        .tracked()
        .query(&SumNegatedVariable { a: "a".to_string(), b: "b".to_string() })
        .unwrap();

    assert_eq!(*value, -300);

    let input_lock = engine.input_lock();

    input_lock.set_input(Variable("a".to_string()), Arc::new(200));
    input_lock.set_input(Variable("b".to_string()), Arc::new(300));

    drop(input_lock);

    assert_eq!(engine.version(), 2);

    let value = engine
        .tracked()
        .query(&SumNegatedVariable { a: "a".to_string(), b: "b".to_string() })
        .unwrap();

    assert_eq!(*value, -500);

    let input_lock = engine.input_lock();

    input_lock.set_input(Variable("a".to_string()), Arc::new(-300));
    input_lock.set_input(Variable("b".to_string()), Arc::new(-300));

    drop(input_lock);

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
        engine: &mut TrackedEngine,
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

    let input_lock = engine.input_lock();

    // Set initial input
    input_lock.set_input(Variable("x".to_string()), Arc::new(42));

    drop(input_lock);

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
    let input_lock = engine.input_lock();
    input_lock.set_input(Variable("x".to_string()), Arc::new(100));

    drop(input_lock);

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

    let input_lock = engine.input_lock();

    // Set input again but same value
    input_lock.set_input(Variable("x".to_string()), Arc::new(100));

    drop(input_lock);

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
        engine: &mut TrackedEngine,
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
        engine: &mut TrackedEngine,
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

    let input_lock = engine.input_lock();

    // Set initial inputs - both positive values
    input_lock.set_input(Variable("x".to_string()), Arc::new(400));
    input_lock.set_input(Variable("y".to_string()), Arc::new(300));

    drop(input_lock);

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
    let input_lock = engine.input_lock();
    input_lock.set_input(Variable("x".to_string()), Arc::new(-400));
    drop(input_lock);
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
    let input_lock = engine.input_lock();
    input_lock.set_input(Variable("y".to_string()), Arc::new(-300));
    drop(input_lock);
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
    let input_lock = engine.input_lock();
    input_lock.set_input(Variable("x".to_string()), Arc::new(500));
    drop(input_lock);
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
        engine: &mut TrackedEngine,
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
        engine: &mut TrackedEngine,
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
        engine: &mut TrackedEngine,
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

/*
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
        engine: &mut TrackedEngine,
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
*/
