use std::sync::Arc;

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
