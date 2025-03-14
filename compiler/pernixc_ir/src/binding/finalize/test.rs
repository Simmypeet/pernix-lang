use pernixc_handler::Storage;
use pernixc_syntax::{syntax_tree, utility::parse};
use pernixc_table::{diagnostic::Diagnostic, Table};

use crate::binding::{
    diagnostic::NotAllFlowPathsReturnAValue,
    test::{build_table, BindExt, CreateBinderAtExt},
};

fn contains_not_all_flow_paths_return_a_value(
    table: &Table,
    storage: &Storage<Box<dyn Diagnostic>>,
) {
    let vec = storage.as_vec();
    assert_eq!(vec.len(), 1);
    assert_eq!(
        vec[0]
            .as_any()
            .downcast_ref::<NotAllFlowPathsReturnAValue>()
            .unwrap()
            .callable_id,
        table.get_by_qualified_name(["test", "test"]).unwrap()
    );
}

const FUNCTION_DECLARATION: &str = r"
public function test() -> bool:
    panic
";

#[test]
fn empty_function_not_return() {
    let table = build_table(FUNCTION_DECLARATION);
    let binder = table.create_binder_at(["test", "test"]);

    let storage: Storage<Box<dyn Diagnostic>> = Storage::new();
    let _ = binder.finalize(&storage);

    contains_not_all_flow_paths_return_a_value(&table, &storage);
}

#[test]
fn empty_loop_can_dont_return() {
    let table = build_table(FUNCTION_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let storage: Storage<Box<dyn Diagnostic>> = Storage::new();

    binder.bind_as_rvalue_success(
        &parse::<syntax_tree::expression::block::Loop>("loop:\n\tpass"),
    );
    let _ = binder.finalize(&storage);

    assert!(storage.as_vec().is_empty());
}

#[test]
fn loop_with_break_not_return() {
    let table = build_table(FUNCTION_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let storage: Storage<Box<dyn Diagnostic>> = Storage::new();

    binder.bind_as_rvalue_success(
        &parse::<syntax_tree::expression::block::Loop>("loop:\n\tbreak"),
    );
    let _ = binder.finalize(&storage);

    contains_not_all_flow_paths_return_a_value(&table, &storage);
}

const EXHAUSTIVE_MATCH_ALL_RETURN: &str = r"
match true:
    true:  return true
    false: return false
";

#[test]
fn exhaustive_match_all_return() {
    let table = build_table(FUNCTION_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let storage: Storage<Box<dyn Diagnostic>> = Storage::new();

    binder.bind_as_rvalue_success(&parse::<
        syntax_tree::expression::block::Match,
    >(EXHAUSTIVE_MATCH_ALL_RETURN));
    let _ = binder.finalize(&storage);

    assert!(storage.as_vec().is_empty());
}

const NOT_ALL_ARMS_RETURN: &str = r"
match (true, false):
    (true, false): return true
    (false, false): return false
    (a, true): ()
";

#[test]
fn not_all_arms_return() {
    let table = build_table(FUNCTION_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let storage: Storage<Box<dyn Diagnostic>> = Storage::new();

    binder.bind_as_rvalue_success(&parse::<
        syntax_tree::expression::block::Match,
    >(NOT_ALL_ARMS_RETURN));
    let _ = binder.finalize(&storage);

    contains_not_all_flow_paths_return_a_value(&table, &storage);
}
