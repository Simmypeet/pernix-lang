#![allow(clippy::similar_names)]

use std::sync::{atomic::AtomicUsize, Arc};

use pernixc_query_derive::Key;

use crate::{
    executor::{CyclicError, Executor},
    Database,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct Variable(&'static str);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct NegateVariable(&'static str);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NegateVariableExecutor;

impl Executor<NegateVariable> for NegateVariableExecutor {
    fn execute(
        &self,
        db: &Database,
        key: NegateVariable,
    ) -> Result<i32, CyclicError> {
        Ok(-db.query(&Variable(key.0))?)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct SumNegatedVariable {
    pub a: &'static str,
    pub b: &'static str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SumNegatedVariableExecutor;

impl Executor<SumNegatedVariable> for SumNegatedVariableExecutor {
    fn execute(
        &self,
        db: &Database,
        key: SumNegatedVariable,
    ) -> Result<i32, CyclicError> {
        Ok(db.query(&NegateVariable(key.a))?
            + db.query(&NegateVariable(key.b))?)
    }
}

#[test]
fn negate_variable() {
    let mut db = Database::default();

    db.set_input(&Variable("a"), 100);
    db.set_input(&Variable("b"), 200);
    assert_eq!(db.version(), 0);

    db.register_executor(Arc::new(NegateVariableExecutor));
    db.register_executor(Arc::new(SumNegatedVariableExecutor));

    let value = db.query(&SumNegatedVariable { a: "a", b: "b" });

    assert_eq!(value, Ok(-300));

    db.set_input(&Variable("a"), 200);
    assert_eq!(db.version(), 1);

    let value = db.query(&SumNegatedVariable { a: "a", b: "b" });
    assert_eq!(value, Ok(-400));
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct TrackedComputation(&'static str);

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
        db: &Database,
        key: TrackedComputation,
    ) -> Result<i32, CyclicError> {
        // Increment the call counter to track executor invocations
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Perform computation based on input variable
        let input_value = db.query(&Variable(key.0))?;
        Ok(input_value * 2)
    }
}

#[test]
fn skip_when_input_unchanged() {
    let mut db = Database::default();

    // Set initial input
    db.set_input(&Variable("x"), 42);
    assert_eq!(db.version(), 0);

    // Create tracked executor to count invocations
    let tracked_executor = TrackedExecutor::default();
    let executor_arc = Arc::new(tracked_executor);

    // Register the tracked executor
    db.register_executor(executor_arc.clone());

    // First query - should compute and call executor
    let result1 = db.query(&TrackedComputation("x"));
    assert_eq!(result1, Ok(84)); // 42 * 2
    assert_eq!(executor_arc.get_call_count(), 1);

    // Second query with same input - should skip computation and return cached
    // result
    let result2 = db.query(&TrackedComputation("x"));
    assert_eq!(result2, Ok(84)); // Same result
    assert_eq!(executor_arc.get_call_count(), 1); // Executor NOT called again

    // Now change the input - should trigger recomputation
    db.set_input(&Variable("x"), 100);
    assert_eq!(db.version(), 1); // Version should increment

    // Query after input change - should compute and call executor again
    let result4 = db.query(&TrackedComputation("x"));
    assert_eq!(result4, Ok(200)); // 100 * 2
    assert_eq!(executor_arc.get_call_count(), 2); // Executor called again

    // Query again with unchanged input - should skip computation again
    let result5 = db.query(&TrackedComputation("x"));
    assert_eq!(result5, Ok(200)); // Same result
    assert_eq!(executor_arc.get_call_count(), 2); // Executor NOT called again
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct AbsVariable(&'static str);

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
        db: &Database,
        key: AbsVariable,
    ) -> Result<i32, CyclicError> {
        // Increment the call counter to track executor invocations
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Compute absolute value
        let input_value = db.query(&Variable(key.0))?;
        Ok(input_value.abs())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct AddTwoAbsVariable {
    pub x: &'static str,
    pub y: &'static str,
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
        db: &Database,
        key: AddTwoAbsVariable,
    ) -> Result<i32, CyclicError> {
        // Increment the call counter to track executor invocations
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Compute sum of absolute values
        Ok(db.query(&AbsVariable(key.x))? + db.query(&AbsVariable(key.y))?)
    }
}

#[test]
fn skip_when_intermediate_result_unchanged() {
    let mut db = Database::default();

    // Set initial inputs - both positive values
    db.set_input(&Variable("x"), 400);
    db.set_input(&Variable("y"), 300);
    assert_eq!(db.version(), 0);

    // Create tracked executors to count invocations
    let abs_executor = Arc::new(TrackedAbsExecutor::default());
    let add_executor = Arc::new(TrackedAddTwoAbsExecutor::default());

    // Register the tracked executors
    db.register_executor(abs_executor.clone());
    db.register_executor(add_executor.clone());

    // First query - should compute everything from scratch
    let result1 = db.query(&AddTwoAbsVariable { x: "x", y: "y" });
    assert_eq!(result1, Ok(700)); // abs(400) + abs(300) = 400 + 300 = 700
    assert_eq!(abs_executor.get_call_count(), 2); // Called for both x and y
    assert_eq!(add_executor.get_call_count(), 1); // Called once

    // Query again with same inputs - should skip all computation
    let result2 = db.query(&AddTwoAbsVariable { x: "x", y: "y" });
    assert_eq!(result2, Ok(700)); // Same result
    assert_eq!(abs_executor.get_call_count(), 2); // NOT called again
    assert_eq!(add_executor.get_call_count(), 1); // NOT called again

    // Change x from 400 to -400 (abs value stays the same)
    db.set_input(&Variable("x"), -400);
    assert_eq!(db.version(), 1); // Version should increment

    // Query after input change - abs executor should be called for x, but add
    // executor should be skipped because the result of abs(x) hasn't
    // changed
    let result3 = db.query(&AddTwoAbsVariable { x: "x", y: "y" });
    assert_eq!(result3, Ok(700)); // abs(-400) + abs(300) = 400 + 300 = 700 (same result!)
    assert_eq!(abs_executor.get_call_count(), 3); // Called again for x only
    assert_eq!(add_executor.get_call_count(), 1); // NOT called again because abs values are the same

    // Change y from 300 to -300 (abs value stays the same)
    db.set_input(&Variable("y"), -300);
    assert_eq!(db.version(), 2); // Version should increment again

    // Query after second input change - abs executor should be called for y,
    // but add executor still skipped
    let result4 = db.query(&AddTwoAbsVariable { x: "x", y: "y" });
    assert_eq!(result4, Ok(700)); // abs(-400) + abs(-300) = 400 + 300 = 700 (still same result!)
    assert_eq!(abs_executor.get_call_count(), 4); // Called again for y only
    assert_eq!(add_executor.get_call_count(), 1); // STILL not called because both abs values are the same

    // Now change x to a value that actually changes the abs result
    db.set_input(&Variable("x"), 500);
    assert_eq!(db.version(), 3); // Version should increment

    // Query after meaningful change - both executors should be called
    let result5 = db.query(&AddTwoAbsVariable { x: "x", y: "y" });
    assert_eq!(result5, Ok(800)); // abs(500) + abs(-300) = 500 + 300 = 800
    assert_eq!(abs_executor.get_call_count(), 5); // Called for x
    assert_eq!(add_executor.get_call_count(), 2); // Finally called again because abs(x) changed

    // Query again - should skip everything
    let result6 = db.query(&AddTwoAbsVariable { x: "x", y: "y" });
    assert_eq!(result6, Ok(800)); // Same result
    assert_eq!(abs_executor.get_call_count(), 5); // NOT called
    assert_eq!(add_executor.get_call_count(), 2); // NOT called
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct SquareVariable(&'static str);

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
        db: &Database,
        key: SquareVariable,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let input_value = db.query(&Variable(key.0))?;
        Ok(input_value * input_value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct ComplexComputation(&'static str);

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
        db: &Database,
        key: ComplexComputation,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // Complex computation: abs(x) + square(x)
        Ok(db.query(&AbsVariable(key.0))? + db.query(&SquareVariable(key.0))?)
    }
}

#[test]
fn multi_layer_dependency_skipping() {
    let mut db = Database::default();

    // Set initial input
    db.set_input(&Variable("z"), 5);
    assert_eq!(db.version(), 0);

    // Create tracked executors
    let abs_executor = Arc::new(TrackedAbsExecutor::default());
    let square_executor = Arc::new(TrackedSquareExecutor::default());
    let complex_executor = Arc::new(TrackedComplexExecutor::default());

    // Register executors
    db.register_executor(abs_executor.clone());
    db.register_executor(square_executor.clone());
    db.register_executor(complex_executor.clone());

    // First query - everything computed from scratch
    let result1 = db.query(&ComplexComputation("z"));
    assert_eq!(result1, Ok(30)); // abs(5) + square(5) = 5 + 25 = 30
    assert_eq!(abs_executor.get_call_count(), 1);
    assert_eq!(square_executor.get_call_count(), 1);
    assert_eq!(complex_executor.get_call_count(), 1);

    // Change z from 5 to -5
    // abs(-5) = 5 (unchanged), but square(-5) = 25 (unchanged too!)
    // So the complex computation result should be the same: 5 + 25 = 30
    db.set_input(&Variable("z"), -5);
    assert_eq!(db.version(), 1);

    let result2 = db.query(&ComplexComputation("z"));
    assert_eq!(result2, Ok(30)); // abs(-5) + square(-5) = 5 + 25 = 30 (same!)
    assert_eq!(abs_executor.get_call_count(), 2); // Called for abs(-5)
    assert_eq!(square_executor.get_call_count(), 2); // Called for square(-5)
    assert_eq!(complex_executor.get_call_count(), 1); // NOT called because both dependencies are unchanged!

    // Change to a different value that actually changes the result
    db.set_input(&Variable("z"), 3);
    assert_eq!(db.version(), 2);

    let result3 = db.query(&ComplexComputation("z"));
    assert_eq!(result3, Ok(12)); // abs(3) + square(3) = 3 + 9 = 12
    assert_eq!(abs_executor.get_call_count(), 3); // Called for abs(3)
    assert_eq!(square_executor.get_call_count(), 3); // Called for square(3)
    assert_eq!(complex_executor.get_call_count(), 2); // Called because dependencies changed

    // Query again - everything should be cached
    let result4 = db.query(&ComplexComputation("z"));
    assert_eq!(result4, Ok(12)); // Same result
    assert_eq!(abs_executor.get_call_count(), 3); // NOT called
    assert_eq!(square_executor.get_call_count(), 3); // NOT called
    assert_eq!(complex_executor.get_call_count(), 2); // NOT called
}

// Test to demonstrate how the incremental query system should handle
// complex dependencies and potential cycles in practice
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct TypeCheckQuery(&'static str);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct DependencyQuery(&'static str);

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
        db: &Database,
        key: TypeCheckQuery,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Simulate type checking that depends on other type information
        let base_value = db.query(&Variable(key.0))?;
        let dependency_value = db.query(&DependencyQuery(key.0))?;

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
        db: &Database,
        key: DependencyQuery,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Simulate dependency resolution
        let base_value = db.query(&Variable(key.0))?;
        Ok(base_value * 2)
    }
}

#[test]
fn incremental_compilation_simulation() {
    let mut db = Database::default();

    // Set up input values (representing source code)
    db.set_input(&Variable("module_a"), 10);
    db.set_input(&Variable("module_b"), 20);

    let type_check_executor = Arc::new(TypeCheckExecutor::default());
    let dependency_executor = Arc::new(DependencyExecutor::default());

    db.register_executor(Arc::clone(&type_check_executor));
    db.register_executor(Arc::clone(&dependency_executor));

    // First compilation: everything computed from scratch
    let result_a = db.query(&TypeCheckQuery("module_a"));
    let result_b = db.query(&TypeCheckQuery("module_b"));

    assert_eq!(result_a, Ok(30)); // 10 + (10 * 2) = 30
    assert_eq!(result_b, Ok(60)); // 20 + (20 * 2) = 60

    // Both executors should have been called for both modules
    assert_eq!(type_check_executor.get_call_count(), 2);
    assert_eq!(dependency_executor.get_call_count(), 2);

    // Simulate incremental change: only module_a changes
    db.set_input(&Variable("module_a"), 15);

    // Reset call counts to track incremental behavior
    type_check_executor
        .call_count
        .store(0, std::sync::atomic::Ordering::SeqCst);
    dependency_executor
        .call_count
        .store(0, std::sync::atomic::Ordering::SeqCst);

    // Re-query: only module_a should be recomputed
    let new_result_a = db.query(&TypeCheckQuery("module_a"));
    let cached_result_b = db.query(&TypeCheckQuery("module_b"));

    assert_eq!(new_result_a, Ok(45)); // 15 + (15 * 2) = 45
    assert_eq!(cached_result_b, Ok(60)); // Same as before, should be cached

    // Only module_a related computations should be called
    assert_eq!(type_check_executor.get_call_count(), 1); // Only for module_a
    assert_eq!(dependency_executor.get_call_count(), 1); // Only for module_a
}

// Test cases for cyclic dependency handling
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct CyclicQueryA;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct CyclicQueryB;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
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
        db: &Database,
        _key: CyclicQueryA,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This creates a cycle: A depends on B, B depends on A
        let b_value = db.query(&CyclicQueryB)?;
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
        db: &Database,
        _key: CyclicQueryB,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This completes the cycle: B depends on A, A depends on B
        let a_value = db.query(&CyclicQueryA)?;
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
        db: &Database,
        _key: DependentQuery,
    ) -> Result<i32, CyclicError> {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This query depends on the cyclic queries
        let a_value = db.query(&CyclicQueryA)?;
        let b_value = db.query(&CyclicQueryB)?;
        Ok(a_value + b_value + 100)
    }
}

#[test]
fn cyclic_dependency_returns_default_values() {
    let mut db = Database::default();

    let executor_a = Arc::new(CyclicExecutorA::default());
    let executor_b = Arc::new(CyclicExecutorB::default());

    db.register_executor(Arc::clone(&executor_a));
    db.register_executor(Arc::clone(&executor_b));

    // When we query CyclicQueryA, it should detect the cycle A -> B -> A
    // and return default values (0 for i32) without calling the executors
    let result_a = db.query(&CyclicQueryA);
    let result_b = db.query(&CyclicQueryB);

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
    let mut db = Database::default();

    let executor_a = Arc::new(CyclicExecutorA::default());
    let executor_b = Arc::new(CyclicExecutorB::default());
    let executor_dependent = Arc::new(DependentExecutor::default());

    db.register_executor(Arc::clone(&executor_a));
    db.register_executor(Arc::clone(&executor_b));
    db.register_executor(Arc::clone(&executor_dependent));

    // Query the dependent query, which depends on the cyclic queries
    let result = db.query(&DependentQuery);

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
    let mut db = Database::default();

    let executor_a = Arc::new(CyclicExecutorA::default());
    let executor_b = Arc::new(CyclicExecutorB::default());
    let executor_dependent = Arc::new(DependentExecutor::default());

    db.register_executor(Arc::clone(&executor_a));
    db.register_executor(Arc::clone(&executor_b));
    db.register_executor(Arc::clone(&executor_dependent));

    // First, query CyclicQueryA directly to trigger cycle detection
    let result_a = db.query(&CyclicQueryA);
    assert_eq!(result_a, Ok(0)); // Should return default value

    // Check call counts after first cycle detection
    let initial_a_calls = executor_a.get_call_count();
    let initial_b_calls = executor_b.get_call_count();

    // Both executors should have been called exactly once during cycle
    // detection
    assert_eq!(initial_a_calls, 1);
    assert_eq!(initial_b_calls, 1);

    // Now query CyclicQueryB - it should return cached default value
    let result_b = db.query(&CyclicQueryB);
    assert_eq!(result_b, Ok(0)); // Should return default value

    // Call counts shouldn't change because values are cached
    assert_eq!(executor_a.get_call_count(), initial_a_calls);
    assert_eq!(executor_b.get_call_count(), initial_b_calls);

    // Now query DependentQuery - it should use the cached default values
    let result_dependent = db.query(&DependentQuery);
    assert_eq!(result_dependent, Ok(100)); // 0 + 0 + 100 = 100

    // The dependent executor should be called once
    assert_eq!(executor_dependent.get_call_count(), 1);

    // The cyclic executors' call counts shouldn't change
    assert_eq!(executor_a.get_call_count(), initial_a_calls);
    assert_eq!(executor_b.get_call_count(), initial_b_calls);

    // Query everything again - all should be cached
    let result_a2 = db.query(&CyclicQueryA);
    let result_b2 = db.query(&CyclicQueryB);
    let result_dependent2 = db.query(&DependentQuery);

    assert_eq!(result_a2, Ok(0));
    assert_eq!(result_b2, Ok(0));
    assert_eq!(result_dependent2, Ok(100));

    // No additional executor calls should be made
    assert_eq!(executor_a.get_call_count(), initial_a_calls);
    assert_eq!(executor_b.get_call_count(), initial_b_calls);
    assert_eq!(executor_dependent.get_call_count(), 1);
}
