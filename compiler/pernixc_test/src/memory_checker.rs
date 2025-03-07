//! Tests for the memory checker.

use pernixc_builder::utility::build_table;
use pernixc_memory_checker::diagnostic::{
    MoveInLoop, MovedOutValueFromMutableReference, UseAfterMove,
};
use pernixc_source_file::Span;

const USE_AFTER_PARTIAL_MOVE: &str = r"
public function consume[T](x: T):
    pass


public function partialMove[T](x: (T, T)):
    consume(x.0)
    consume(x)
";

#[test]
fn use_after_partial_move() {
    let (_, errs) = build_table(USE_AFTER_PARTIAL_MOVE);

    assert_eq!(errs.len(), 1);

    let error = errs[0].as_any().downcast_ref::<UseAfterMove>().unwrap();

    assert_eq!(error.use_span.str(), "x");
    assert_eq!(error.move_span.str(), "x.0");
}

const USE_AFTER_PARTIAL_MOVE_IN_BRANCH: &str = r"
public function consume[T](x: T):
    pass


public function partialMove[T](x: (T, T)):
    if true:
        consume(x.0)

    
    consume(x)
";

#[test]
fn use_after_partial_move_in_branch() {
    let (_, errs) = build_table(USE_AFTER_PARTIAL_MOVE_IN_BRANCH);

    assert_eq!(errs.len(), 1);

    let error = errs[0].as_any().downcast_ref::<UseAfterMove>().unwrap();

    assert_eq!(error.use_span.str(), "x");
    assert_eq!(error.move_span.str(), "x.0");
}

const PARTIAL_MOVE_DIFFERENT_PART_IN_DIFFERENT_BRANCHES: &str = r"
public function consume[T](x: T):
    pass


public function partialMove[T](x: (T, T, T, T), cond: bool):
    match (cond, cond):
        (true, false): 
            consume(x.0)
        
        (false, false):
            consume(x.1)
        
        (.., true):
            consume(x.2)
        
    consume(x.3)

";

#[test]
fn partial_moves_different_part_in_different_branches() {
    assert!(build_table(PARTIAL_MOVE_DIFFERENT_PART_IN_DIFFERENT_BRANCHES)
        .1
        .is_empty());
}

const DROPED_MOVED_OUT_OF_REFERENCE: &str = r"
public function consume[T](x: T):
    pass


public function movedOutOfReference[T](mut x: T):
    scope:
        let y = &mut x
        consume(*y)
";

#[test]
fn droped_moved_out_of_reference() {
    let (_, errs) = build_table(DROPED_MOVED_OUT_OF_REFERENCE);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MovedOutValueFromMutableReference>()
        .unwrap();

    assert_eq!(error.moved_out_value_span.str(), "*y");
    assert_eq!(error.reassignment_span, None);
}

const REASSIGNED_MOVED_OUT_OF_REFERENCE: &str = r"
public function consume[T](x: T):
    pass


public function test[T](mut x: T, mut y: T):
    scope: 
        let mut z = &mut x
        consume(*z)
        z = &mut y

";

#[test]
fn reassigned_moved_out_of_reference() {
    let (_, errs) = build_table(REASSIGNED_MOVED_OUT_OF_REFERENCE);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MovedOutValueFromMutableReference>()
        .unwrap();

    assert_eq!(error.moved_out_value_span.str(), "*z");
    assert_eq!(
        error.reassignment_span.as_ref().map(Span::str),
        Some("z = &mut y")
    );
}

const RESTORED_MOVED_OUT_OF_REFERENCE: &str = r"
public function consume[T](x: T):
    pass


public function test[T](mut x: T, mut y: T, cond: bool):
    let mut ref = &mut x
    consume(*ref)

    match (cond, cond):
        (true, false): 
            *ref = y

        (false, false):
            *ref = y
        
        (.., true):
            *ref = y
";

#[test]
fn restored_moved_out_of_reference() {
    assert!(build_table(RESTORED_MOVED_OUT_OF_REFERENCE).1.is_empty());
}

const MOVED_OUT_IN_LOOP: &str = r"
public function consume[T](x: T):
    pass


public function test[T](mut x: T, cond: bool):
    while cond:
        consume(x)
";

#[test]
fn moved_out_in_loop() {
    let (_, errs) = build_table(MOVED_OUT_IN_LOOP);

    assert_eq!(errs.len(), 1);

    let error = errs[0].as_any().downcast_ref::<MoveInLoop>().unwrap();

    assert_eq!(error.moved_value_span.str(), "x");
}

const MOVED_OUT_IN_LOOP_BUT_RESTORED: &str = r"
public function create[T]() -> T:
    panic


public function consume[T](x: T):
    pass


public function test[T](mut x: (T, T), cond: bool):
    while cond:
        consume(x)

        x.0 = create()
        x.1 = create()
";

#[test]
fn moved_out_in_loop_but_restored() {
    assert!(build_table(MOVED_OUT_IN_LOOP_BUT_RESTORED).1.is_empty());
}

const MOVED_OUT_IN_INNER_LOOP: &str = r"
public function create[T]() -> T:
    panic


public function consume[T](x: T):
    pass


public function test[T](cond: bool):
    while cond:
        let value = create[T]()

        while cond:
            consume(value)
";

#[test]
fn moved_out_in_inner_loop() {
    let (_, errs) = build_table(MOVED_OUT_IN_INNER_LOOP);

    assert_eq!(errs.len(), 1);

    let error = errs[0].as_any().downcast_ref::<MoveInLoop>().unwrap();

    assert_eq!(error.moved_value_span.str(), "value");
}

const MOVED_OUT_LOOP_LOCAL: &str = r"
public function create[T]() -> T:
    panic


public function consume[T](x: T):
    pass


public function test[T](cond: bool):
    while cond:
        let x = create[T]()
        consume(x)
";

#[test]
fn moved_out_loop_local() {
    assert!(build_table(MOVED_OUT_LOOP_LOCAL).1.is_empty());
}

const DEREFERENCE_MOVED_MUTABLE_REFERENCE_OF_COPYABLE_TYPE: &str = r"
public function consume[T](..: T):
    pass


public function test():
    let mut x = 0
    let ref = &mut x

    let movedToRef = ref

    consume(*ref)

";

#[test]
fn dereference_moved_mutable_reference_of_copyable_type() {
    let (_, errs) =
        build_table(DEREFERENCE_MOVED_MUTABLE_REFERENCE_OF_COPYABLE_TYPE);

    assert_eq!(errs.len(), 1);

    let error = errs[0].as_any().downcast_ref::<UseAfterMove>().unwrap();

    assert_eq!(error.use_span.str(), "*ref");
    assert_eq!(error.move_span.str(), "ref");
}

const RETURN_MOVED_VALUE: &str = r"
public function create[T]() -> T:
    panic


public function test[T](param: T) -> T:
    let movedTo = param
    return param

";

#[test]
fn return_moved_value() {
    let (_, errs) = build_table(RETURN_MOVED_VALUE);

    assert_eq!(errs.len(), 1);

    let error = errs[0].as_any().downcast_ref::<UseAfterMove>().unwrap();

    assert_eq!(error.use_span.str(), "param");
    assert_eq!(error.move_span.str(), "param");
}

const RETURN_NON_COPYABLE: &str = r"
public function create[T]() -> T:
    panic


public function test[T]() -> T:
    let value = create[T]()
    return value
";

#[test]
fn return_non_copyable() {
    assert!(build_table(RETURN_NON_COPYABLE).1.is_empty());
}
