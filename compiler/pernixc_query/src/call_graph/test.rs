use std::sync::{atomic::AtomicUsize, Arc};

use pernixc_query_derive::Key;

use crate::{executor::Executor, Database};

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
    fn execute(&self, db: &Database, key: NegateVariable) -> i32 {
        -db.query(&Variable(key.0))
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
    fn execute(&self, db: &Database, key: SumNegatedVariable) -> i32 {
        db.query(&NegateVariable(key.a)) + db.query(&NegateVariable(key.b))
    }
}

#[test]
fn test_negate_variable() {
    let mut db = Database::default();

    db.set_input(&Variable("a"), 100);
    db.set_input(&Variable("b"), 200);
    assert_eq!(db.version(), 0);

    db.register_executor(Arc::new(NegateVariableExecutor));
    db.register_executor(Arc::new(SumNegatedVariableExecutor));

    let value = db.query(&SumNegatedVariable { a: "a", b: "b" });

    assert_eq!(value, -300);

    db.set_input(&Variable("a"), 200);
    assert_eq!(db.version(), 1);

    let value = db.query(&SumNegatedVariable { a: "a", b: "b" });
    assert_eq!(value, -400);
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
    fn execute(&self, db: &Database, key: TrackedComputation) -> i32 {
        // Increment the call counter to track executor invocations
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Perform computation based on input variable
        let input_value = db.query(&Variable(key.0));
        input_value * 2
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
    assert_eq!(result1, 84); // 42 * 2
    assert_eq!(executor_arc.get_call_count(), 1);

    // Second query with same input - should skip computation and return cached
    // result
    let result2 = db.query(&TrackedComputation("x"));
    assert_eq!(result2, 84); // Same result
    assert_eq!(executor_arc.get_call_count(), 1); // Executor NOT called again

    // Now change the input - should trigger recomputation
    db.set_input(&Variable("x"), 100);
    assert_eq!(db.version(), 1); // Version should increment

    // Query after input change - should compute and call executor again
    let result4 = db.query(&TrackedComputation("x"));
    assert_eq!(result4, 200); // 100 * 2
    assert_eq!(executor_arc.get_call_count(), 2); // Executor called again

    // Query again with unchanged input - should skip computation again
    let result5 = db.query(&TrackedComputation("x"));
    assert_eq!(result5, 200); // Same result
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
    fn execute(&self, db: &Database, key: AbsVariable) -> i32 {
        // Increment the call counter to track executor invocations
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Compute absolute value
        let input_value = db.query(&Variable(key.0));
        input_value.abs()
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
    fn execute(&self, db: &Database, key: AddTwoAbsVariable) -> i32 {
        // Increment the call counter to track executor invocations
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Compute sum of absolute values
        db.query(&AbsVariable(key.x)) + db.query(&AbsVariable(key.y))
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
    assert_eq!(result1, 700); // abs(400) + abs(300) = 400 + 300 = 700
    assert_eq!(abs_executor.get_call_count(), 2); // Called for both x and y
    assert_eq!(add_executor.get_call_count(), 1); // Called once

    // Query again with same inputs - should skip all computation
    let result2 = db.query(&AddTwoAbsVariable { x: "x", y: "y" });
    assert_eq!(result2, 700); // Same result
    assert_eq!(abs_executor.get_call_count(), 2); // NOT called again
    assert_eq!(add_executor.get_call_count(), 1); // NOT called again

    // Change x from 400 to -400 (abs value stays the same)
    db.set_input(&Variable("x"), -400);
    assert_eq!(db.version(), 1); // Version should increment

    // Query after input change - abs executor should be called for x, but add
    // executor should be skipped because the result of abs(x) hasn't
    // changed
    let result3 = db.query(&AddTwoAbsVariable { x: "x", y: "y" });
    assert_eq!(result3, 700); // abs(-400) + abs(300) = 400 + 300 = 700 (same result!)
    assert_eq!(abs_executor.get_call_count(), 3); // Called again for x only
    assert_eq!(add_executor.get_call_count(), 1); // NOT called again because abs values are the same

    // Change y from 300 to -300 (abs value stays the same)
    db.set_input(&Variable("y"), -300);
    assert_eq!(db.version(), 2); // Version should increment again

    // Query after second input change - abs executor should be called for y,
    // but add executor still skipped
    let result4 = db.query(&AddTwoAbsVariable { x: "x", y: "y" });
    assert_eq!(result4, 700); // abs(-400) + abs(-300) = 400 + 300 = 700 (still same result!)
    assert_eq!(abs_executor.get_call_count(), 4); // Called again for y only
    assert_eq!(add_executor.get_call_count(), 1); // STILL not called because both abs values are the same

    // Now change x to a value that actually changes the abs result
    db.set_input(&Variable("x"), 500);
    assert_eq!(db.version(), 3); // Version should increment

    // Query after meaningful change - both executors should be called
    let result5 = db.query(&AddTwoAbsVariable { x: "x", y: "y" });
    assert_eq!(result5, 800); // abs(500) + abs(-300) = 500 + 300 = 800
    assert_eq!(abs_executor.get_call_count(), 5); // Called for x
    assert_eq!(add_executor.get_call_count(), 2); // Finally called again because abs(x) changed

    // Query again - should skip everything
    let result6 = db.query(&AddTwoAbsVariable { x: "x", y: "y" });
    assert_eq!(result6, 800); // Same result
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
    fn execute(&self, db: &Database, key: SquareVariable) -> i32 {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let input_value = db.query(&Variable(key.0));
        input_value * input_value
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
    fn execute(&self, db: &Database, key: ComplexComputation) -> i32 {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // Complex computation: abs(x) + square(x)
        db.query(&AbsVariable(key.0)) + db.query(&SquareVariable(key.0))
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
    assert_eq!(result1, 30); // abs(5) + square(5) = 5 + 25 = 30
    assert_eq!(abs_executor.get_call_count(), 1);
    assert_eq!(square_executor.get_call_count(), 1);
    assert_eq!(complex_executor.get_call_count(), 1);

    // Change z from 5 to -5
    // abs(-5) = 5 (unchanged), but square(-5) = 25 (unchanged too!)
    // So the complex computation result should be the same: 5 + 25 = 30
    db.set_input(&Variable("z"), -5);
    assert_eq!(db.version(), 1);

    let result2 = db.query(&ComplexComputation("z"));
    assert_eq!(result2, 30); // abs(-5) + square(-5) = 5 + 25 = 30 (same!)
    assert_eq!(abs_executor.get_call_count(), 2); // Called for abs(-5)
    assert_eq!(square_executor.get_call_count(), 2); // Called for square(-5)
    assert_eq!(complex_executor.get_call_count(), 1); // NOT called because both dependencies are unchanged!

    // Change to a different value that actually changes the result
    db.set_input(&Variable("z"), 3);
    assert_eq!(db.version(), 2);

    let result3 = db.query(&ComplexComputation("z"));
    assert_eq!(result3, 12); // abs(3) + square(3) = 3 + 9 = 12
    assert_eq!(abs_executor.get_call_count(), 3); // Called for abs(3)
    assert_eq!(square_executor.get_call_count(), 3); // Called for square(3)
    assert_eq!(complex_executor.get_call_count(), 2); // Called because dependencies changed

    // Query again - everything should be cached
    let result4 = db.query(&ComplexComputation("z"));
    assert_eq!(result4, 12); // Same result
    assert_eq!(abs_executor.get_call_count(), 3); // NOT called
    assert_eq!(square_executor.get_call_count(), 3); // NOT called
    assert_eq!(complex_executor.get_call_count(), 2); // NOT called
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct CyclicNodeA(&'static str);

#[derive(Debug, Default)]
pub struct TrackedCyclicNodeAExecutor {
    pub call_count: AtomicUsize,
}

impl TrackedCyclicNodeAExecutor {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<CyclicNodeA> for TrackedCyclicNodeAExecutor {
    fn execute(&self, db: &Database, key: CyclicNodeA) -> i32 {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This will create a cycle: A -> B -> A
        db.query(&CyclicNodeB(key.0)) + 1
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct CyclicNodeB(&'static str);

#[derive(Debug, Default)]
pub struct TrackedCyclicNodeBExecutor {
    pub call_count: AtomicUsize,
}

impl TrackedCyclicNodeBExecutor {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<CyclicNodeB> for TrackedCyclicNodeBExecutor {
    fn execute(&self, db: &Database, key: CyclicNodeB) -> i32 {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This completes the cycle: B -> A
        db.query(&CyclicNodeA(key.0)) + 2
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct ThreeWayCyclicNodeA(&'static str);

#[derive(Debug, Default)]
pub struct TrackedThreeWayCyclicNodeAExecutor {
    pub call_count: AtomicUsize,
}

impl TrackedThreeWayCyclicNodeAExecutor {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<ThreeWayCyclicNodeA> for TrackedThreeWayCyclicNodeAExecutor {
    fn execute(&self, db: &Database, key: ThreeWayCyclicNodeA) -> i32 {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This will create a 3-way cycle: A -> B -> C -> A
        db.query(&ThreeWayCyclicNodeB(key.0)) + 10
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct ThreeWayCyclicNodeB(&'static str);

#[derive(Debug, Default)]
pub struct TrackedThreeWayCyclicNodeBExecutor {
    pub call_count: AtomicUsize,
}

impl TrackedThreeWayCyclicNodeBExecutor {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<ThreeWayCyclicNodeB> for TrackedThreeWayCyclicNodeBExecutor {
    fn execute(&self, db: &Database, key: ThreeWayCyclicNodeB) -> i32 {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // B -> C
        db.query(&ThreeWayCyclicNodeC(key.0)) + 20
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct ThreeWayCyclicNodeC(&'static str);

#[derive(Debug, Default)]
pub struct TrackedThreeWayCyclicNodeCExecutor {
    pub call_count: AtomicUsize,
}

impl TrackedThreeWayCyclicNodeCExecutor {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<ThreeWayCyclicNodeC> for TrackedThreeWayCyclicNodeCExecutor {
    fn execute(&self, db: &Database, key: ThreeWayCyclicNodeC) -> i32 {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // C -> A (completes the cycle)
        db.query(&ThreeWayCyclicNodeA(key.0)) + 30
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct SelfReferencingNode(&'static str);

#[derive(Debug, Default)]
pub struct TrackedSelfReferencingExecutor {
    pub call_count: AtomicUsize,
}

impl TrackedSelfReferencingExecutor {
    pub fn get_call_count(&self) -> usize {
        self.call_count.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Executor<SelfReferencingNode> for TrackedSelfReferencingExecutor {
    fn execute(&self, db: &Database, key: SelfReferencingNode) -> i32 {
        self.call_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        // This creates a direct self-reference cycle
        db.query(&SelfReferencingNode(key.0)) + 100
    }
}

#[test]
#[allow(clippy::similar_names)]
fn cyclic_dependency_two_way_cycle() {
    let mut db = Database::default();

    // Create tracked executors to count invocations
    let cyclic_a_executor = Arc::new(TrackedCyclicNodeAExecutor::default());
    let cyclic_b_executor = Arc::new(TrackedCyclicNodeBExecutor::default());

    // Register the tracked executors
    db.register_executor(cyclic_a_executor.clone());
    db.register_executor(cyclic_b_executor.clone());

    // Query A, which should trigger the cycle A -> B -> A
    let result_a = db.query(&CyclicNodeA("cycle"));

    // Due to the cycle, the system should return default values (0 for i32)
    // When A queries B, B tries to query A (cycle detected), A gets default(0)
    // So B = 0 + 2 = 2, then A = 2 + 1 = 3
    assert_eq!(result_a, 3);

    // Check that executors were called - should be exactly 1 each since cycle
    // is detected
    assert_eq!(cyclic_a_executor.get_call_count(), 1);
    assert_eq!(cyclic_b_executor.get_call_count(), 1);

    // Query B directly to see consistent behavior
    let result_b = db.query(&CyclicNodeB("cycle"));

    // B should now be cached and return the computed value
    // Since A was defaulted to 0 during cycle detection, B = 0 + 2 = 2
    assert_eq!(result_b, 2);

    // No additional executor calls should happen due to caching
    assert_eq!(cyclic_a_executor.get_call_count(), 1);
    assert_eq!(cyclic_b_executor.get_call_count(), 1);

    // Query A again - should return cached value
    let result_a2 = db.query(&CyclicNodeA("cycle"));
    assert_eq!(result_a2, 3); // Same as before

    // Still no additional calls
    assert_eq!(cyclic_a_executor.get_call_count(), 1);
    assert_eq!(cyclic_b_executor.get_call_count(), 1);
}

#[test]
#[allow(clippy::similar_names)]
fn cyclic_dependency_three_way_cycle() {
    let mut db = Database::default();

    // Create tracked executors to count invocations
    let node_a_executor =
        Arc::new(TrackedThreeWayCyclicNodeAExecutor::default());
    let node_b_executor =
        Arc::new(TrackedThreeWayCyclicNodeBExecutor::default());
    let node_c_executor =
        Arc::new(TrackedThreeWayCyclicNodeCExecutor::default());

    // Register the tracked executors
    db.register_executor(node_a_executor.clone());
    db.register_executor(node_b_executor.clone());
    db.register_executor(node_c_executor.clone());

    // Query A, which should trigger the cycle A -> B -> C -> A
    let result_a = db.query(&ThreeWayCyclicNodeA("three_way"));

    // The cycle will be detected, and default values will be used
    // When C queries A, A gets default(0), so C = 0 + 30 = 30
    // Then B = 30 + 20 = 50, and finally A = 50 + 10 = 60
    assert_eq!(result_a, 60);

    // Check that all executors were called exactly once
    assert_eq!(node_a_executor.get_call_count(), 1);
    assert_eq!(node_b_executor.get_call_count(), 1);
    assert_eq!(node_c_executor.get_call_count(), 1);

    // Query B and C to see the propagated values
    let result_b = db.query(&ThreeWayCyclicNodeB("three_way"));
    let result_c = db.query(&ThreeWayCyclicNodeC("three_way"));

    // B = 30 + 20 = 50 (from C returning 30)
    // C = 0 + 30 = 30 (from A defaulting to 0)
    assert_eq!(result_b, 50);
    assert_eq!(result_c, 30);

    // No additional executor calls due to caching
    assert_eq!(node_a_executor.get_call_count(), 1);
    assert_eq!(node_b_executor.get_call_count(), 1);
    assert_eq!(node_c_executor.get_call_count(), 1);
}

#[test]
fn cyclic_dependency_self_reference() {
    let mut db = Database::default();

    // Create tracked executor to count invocations
    let self_ref_executor = Arc::new(TrackedSelfReferencingExecutor::default());

    // Register the tracked executor
    db.register_executor(self_ref_executor.clone());

    // Query the self-referencing node
    let result = db.query(&SelfReferencingNode("self"));

    // The self-reference should be detected immediately
    // Node gets default(0) from itself, so result = 0 + 100 = 100
    assert_eq!(result, 100);

    // Should be called exactly once
    assert_eq!(self_ref_executor.get_call_count(), 1);

    // Query again - should return cached value
    let result2 = db.query(&SelfReferencingNode("self"));
    assert_eq!(result2, 100);

    // No additional calls
    assert_eq!(self_ref_executor.get_call_count(), 1);
}

#[test]
#[allow(clippy::similar_names)]
fn cyclic_dependency_with_different_inputs() {
    let mut db = Database::default();

    // Create tracked executors
    let node_a_executor = Arc::new(TrackedCyclicNodeAExecutor::default());
    let node_b_executor = Arc::new(TrackedCyclicNodeBExecutor::default());

    // Register executors
    db.register_executor(node_a_executor.clone());
    db.register_executor(node_b_executor.clone());

    // Query with first input
    let result_a1 = db.query(&CyclicNodeA("input1"));
    assert_eq!(result_a1, 3); // A = B + 1 = 2 + 1 = 3 (B = 0 + 2 from cycle)

    // Query with second input (different cycle)
    let result_a2 = db.query(&CyclicNodeA("input2"));
    assert_eq!(result_a2, 3); // Same logic, different instance

    // Should have 2 calls for A and 2 calls for B (one for each input)
    assert_eq!(node_a_executor.get_call_count(), 2);
    assert_eq!(node_b_executor.get_call_count(), 2);

    // Query first input again - should be cached
    let result_a1_cached = db.query(&CyclicNodeA("input1"));
    assert_eq!(result_a1_cached, 3);

    // No additional calls
    assert_eq!(node_a_executor.get_call_count(), 2);
    assert_eq!(node_b_executor.get_call_count(), 2);
}

#[test]
#[allow(clippy::similar_names)]
fn cyclic_dependency_mixed_with_regular_queries() {
    let mut db = Database::default();

    // Set up some regular non-cyclic inputs
    db.set_input(&Variable("x"), 10);
    db.set_input(&Variable("y"), 20);

    // Create executors for both cyclic and non-cyclic queries
    let node_a_executor = Arc::new(TrackedCyclicNodeAExecutor::default());
    let node_b_executor = Arc::new(TrackedCyclicNodeBExecutor::default());
    let abs_executor = Arc::new(TrackedAbsExecutor::default());

    // Register executors
    db.register_executor(node_a_executor.clone());
    db.register_executor(node_b_executor.clone());
    db.register_executor(abs_executor.clone());

    // First, query regular non-cyclic computation
    let abs_result = db.query(&AbsVariable("x"));
    assert_eq!(abs_result, 10); // abs(10) = 10
    assert_eq!(abs_executor.get_call_count(), 1);

    // Then query cyclic computation
    let cyclic_result = db.query(&CyclicNodeA("mixed"));
    assert_eq!(cyclic_result, 3); // A = B + 1 = 2 + 1 = 3 (B = 0 + 2 from cycle)
    assert_eq!(node_a_executor.get_call_count(), 1);
    assert_eq!(node_b_executor.get_call_count(), 1);

    // Query regular computation again - should be cached
    let abs_result2 = db.query(&AbsVariable("x"));
    assert_eq!(abs_result2, 10);
    assert_eq!(abs_executor.get_call_count(), 1); // No additional call

    // Query cyclic computation again - should be cached
    let cyclic_result2 = db.query(&CyclicNodeA("mixed"));
    assert_eq!(cyclic_result2, 3);
    assert_eq!(node_a_executor.get_call_count(), 1); // No additional call
    assert_eq!(node_b_executor.get_call_count(), 1); // No additional call

    // Change input and verify regular queries update but cyclic ones don't
    db.set_input(&Variable("x"), -15);
    assert_eq!(db.version(), 1);

    let abs_result3 = db.query(&AbsVariable("x"));
    assert_eq!(abs_result3, 15); // abs(-15) = 15
    assert_eq!(abs_executor.get_call_count(), 2); // Called again

    // Cyclic queries should remain cached (they don't depend on inputs)
    let cyclic_result3 = db.query(&CyclicNodeA("mixed"));
    assert_eq!(cyclic_result3, 3);
    assert_eq!(node_a_executor.get_call_count(), 1); // Still no additional call
    assert_eq!(node_b_executor.get_call_count(), 1); // Still no additional call
}
