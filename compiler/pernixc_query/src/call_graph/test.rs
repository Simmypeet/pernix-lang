use std::sync::Arc;

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
