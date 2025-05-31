use std::sync::Arc;

use serde::{Deserialize, Serialize};

use super::Executor;
use crate::{Database, Key};

#[derive(
    Key,
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
)]
#[value(String)]
#[pernixc_query(crate)]
struct Test;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct MyTestExecutor;

impl Executor<Test> for MyTestExecutor {
    fn execute(
        &self,
        _: &Database,
        Test: Test,
    ) -> Result<String, super::CyclicError> {
        Ok("it works".to_string())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct MySecondTestExecutor;

impl Executor<Test> for MySecondTestExecutor {
    fn execute(
        &self,
        _: &Database,
        Test: Test,
    ) -> Result<String, super::CyclicError> {
        Ok("it works for the second time".to_string())
    }
}

#[test]
fn registry() {
    let mut db = Database::default();
    assert!(db.runtime.executor.register(Arc::new(MyTestExecutor)).is_none());

    let first_executor = db.runtime.executor.get::<Test>().unwrap();
    assert_eq!(first_executor.execute(&db, Test), Ok("it works".to_string()));

    let old_executor =
        db.runtime.executor.register(Arc::new(MySecondTestExecutor)).unwrap();

    assert!(Arc::ptr_eq(&old_executor, &first_executor));

    let second_executor = db.runtime.executor.get::<Test>().unwrap();
    assert_eq!(
        second_executor.execute(&db, Test),
        Ok("it works for the second time".to_string())
    );
}
