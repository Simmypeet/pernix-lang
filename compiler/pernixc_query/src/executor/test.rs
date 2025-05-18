use std::sync::Arc;

use serde::Serialize;

use super::{Executor, Registry};
use crate::Key;

#[derive(
    Key, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize,
)]
#[value(String)]
#[pernixc_query(crate)]
struct Test;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct MyTestExecutor;

impl Executor<Test> for MyTestExecutor {
    fn execute(&self, Test: Test) -> String { "it works".to_string() }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct MySecondTestExecutor;

impl Executor<Test> for MySecondTestExecutor {
    fn execute(&self, Test: Test) -> String {
        "it works for the second time".to_string()
    }
}

#[test]
fn registry() {
    let registry = Registry::default();
    assert!(registry.register(Arc::new(MyTestExecutor)).is_none());

    let first_executor = registry.get::<Test>().unwrap();
    assert_eq!(first_executor.execute(Test), "it works");

    let old_executor =
        registry.register(Arc::new(MySecondTestExecutor)).unwrap();

    assert!(Arc::ptr_eq(&old_executor, &first_executor));

    let second_executor = registry.get::<Test>().unwrap();
    assert_eq!(second_executor.execute(Test), "it works for the second time");
}
