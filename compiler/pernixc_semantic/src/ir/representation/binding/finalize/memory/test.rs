use crate::{
    error::{MoveInLoop, MovedOutValueFromMutableReference, UseAfterMove},
    symbol::table::representation::test::build_table,
};

const USE_AFTER_PARTIAL_MOVE: &str = r#"
public function consume[T](x: T) {}

public function partialMove[T](x: (T, T)) {
    consume(x.0);
    consume(x);
}
"#;

#[test]
fn use_after_partial_move() {
    let (_, errs) = build_table(USE_AFTER_PARTIAL_MOVE).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0].as_any().downcast_ref::<UseAfterMove>().unwrap();

    assert_eq!(error.use_span.str(), "x");
    assert_eq!(error.move_span.str(), "x.0");
}

const USE_AFTER_PARTIAL_MOVE_IN_BRANCH: &str = r#"
public function consume[T](x: T) {}

public function partialMove[T](x: (T, T)) {
    if (true) {
        consume(x.0);
    }

    consume(x);
}
"#;

#[test]
fn use_after_partial_move_in_branch() {
    let (_, errs) = build_table(USE_AFTER_PARTIAL_MOVE_IN_BRANCH).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0].as_any().downcast_ref::<UseAfterMove>().unwrap();

    assert_eq!(error.use_span.str(), "x");
    assert_eq!(error.move_span.str(), "x.0");
}

const PARTIAL_MOVE_DIFFERENT_PART_IN_DIFFERENT_BRANCHES: &str = r#"
public function consume[T](x: T) {}

public function partialMove[T](x: (T, T, T, T), cond: bool) {
    match (cond, cond) {
        (true, false): {
            consume(x.0);
        },
        (false, false): {
            consume(x.1);
        },
        (.., true): {
            consume(x.2);
        },
    }

    consume(x.3);
}
"#;

#[test]
fn partial_moves_different_part_in_different_branches() {
    assert!(
        build_table(PARTIAL_MOVE_DIFFERENT_PART_IN_DIFFERENT_BRANCHES).is_ok()
    );
}

const DROPED_MOVED_OUT_OF_REFERENCE: &str = r#"
public function consume[T](x: T) {}

public function movedOutOfReference[T](mutable x: T) {
    {
        let y = &mutable x;
        consume(*y);
    }
}
"#;

#[test]
fn droped_moved_out_of_reference() {
    let (_, errs) = build_table(DROPED_MOVED_OUT_OF_REFERENCE).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MovedOutValueFromMutableReference>()
        .unwrap();

    assert_eq!(error.moved_out_value_span.str(), "*y");
    assert_eq!(error.reassignment_span, None);
}

const REASSIGNED_MOVED_OUT_OF_REFERENCE: &str = r#"
public function consume[T](x: T) {}

public function test[T](mutable x: T, mutable y: T) {
    {
        let mutable z = &mutable x;
        consume(*z);
        z = &mutable y;
    }
}
"#;

#[test]
fn reassigned_moved_out_of_reference() {
    let (_, errs) = build_table(REASSIGNED_MOVED_OUT_OF_REFERENCE).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MovedOutValueFromMutableReference>()
        .unwrap();

    assert_eq!(error.moved_out_value_span.str(), "*z");
    assert_eq!(
        error.reassignment_span.as_ref().map(|x| x.str()),
        Some("z = &mutable y")
    );
}

const RESTORED_MOVED_OUT_OF_REFERENCE: &str = r#"
public function consume[T](x: T) {}

public function test[T](mutable x: T, mutable y: T, cond: bool) {
    let mutable ref = &mutable x;
    consume(*ref);

    match (cond, cond) {
        (true, false): {
            *ref = y;
        },
        (false, false): {
            *ref = y;
        },
        (.., true): {
            *ref = y;
        },
    }
}
"#;

#[test]
fn restored_moved_out_of_reference() {
    assert!(build_table(RESTORED_MOVED_OUT_OF_REFERENCE).is_ok());
}

const MOVED_OUT_IN_LOOP: &str = r#"
public function consume[T](x: T) {}

public function test[T](mutable x: T, cond: bool) {
    while (cond) {
        consume(x);
    }
}
"#;

#[test]
fn moved_out_in_loop() {
    let (_, errs) = build_table(MOVED_OUT_IN_LOOP).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0].as_any().downcast_ref::<MoveInLoop>().unwrap();

    assert_eq!(error.moved_value_span.str(), "x");
}

const MOVED_OUT_IN_LOOP_BUT_RESTORED: &str = r#"
public function create[T](): T { panic; }

public function consume[T](x: T) {}

public function test[T](mutable x: (T, T), cond: bool) {
    while (cond) {
        consume(x);

        x.0 = create();
        x.1 = create();
    }
}
"#;

#[test]
fn moved_out_in_loop_but_restored() {
    assert!(build_table(MOVED_OUT_IN_LOOP_BUT_RESTORED).is_ok());
}

const MOVED_OUT_IN_INNER_LOOP: &str = r#"
public function create[T](): T { panic; }

public function consume[T](x: T) {}

public function test[T](cond: bool) {
    while (cond) {
        let value = create[T]();

        while (cond) {
            consume(value);
        }
    }
}
"#;

#[test]
fn moved_out_in_inner_loop() {
    let (_, errs) = build_table(MOVED_OUT_IN_INNER_LOOP).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0].as_any().downcast_ref::<MoveInLoop>().unwrap();

    assert_eq!(error.moved_value_span.str(), "value");
}

const MOVED_OUT_LOOP_LOCAL: &str = r#"
public function create[T](): T { panic; }

public function consume[T](x: T) {}

public function test[T](cond: bool) {
    while (cond) {
        let x = create[T]();
        consume(x);
    }
}
"#;

#[test]
fn moved_out_loop_local() {
    assert!(build_table(MOVED_OUT_LOOP_LOCAL).is_ok());
}

const DEREFERENCE_MOVED_MUTABLE_REFERENCE_OF_COPYABLE_TYPE: &str = r#"
public function consume[T](..: T) {}

public function test() {
    let mutable x = 0;
    let ref = &mutable x;

    let movedToRef = ref;

    consume(*ref);
}
"#;

#[test]
fn dereference_moved_mutable_reference_of_copyable_type() {
    let (_, errs) =
        build_table(DEREFERENCE_MOVED_MUTABLE_REFERENCE_OF_COPYABLE_TYPE)
            .unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0].as_any().downcast_ref::<UseAfterMove>().unwrap();

    assert_eq!(error.use_span.str(), "*ref");
    assert_eq!(error.move_span.str(), "ref");
}
