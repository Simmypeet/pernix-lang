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
