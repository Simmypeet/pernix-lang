use crate::{
    error::{
        MoveInLoop, MovedOutValueFromMutableReference, MovedOutWhileBorrowed,
        MutablyAccessWhileBorrowed, UseAfterMove,
        VariableDoesNotLiveLongEnough,
    },
    ir,
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

    dbg!(&errs);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MovedOutValueFromMutableReference>()
        .unwrap();

    assert_eq!(error.moved_out_value_span.str(), "*z");
    assert_eq!(
        error.reassignment_span.as_ref().map(|x| x.str()),
        Some("&mutable y")
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

const VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH: &str = r#"
public function consume[T](x: T) {}

public function test() {
    let outer = 0;
    let mutable ref = &outer;

    {
        let inner = 0;
        ref = &inner;
    }

    consume(*ref);
}
"#;

#[test]
fn variable_does_not_live_long_enough() {
    let (_, errs) =
        build_table(VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough<ir::Model>>()
        .unwrap();

    assert_eq!(error.variable_span.str(), "inner");
    assert_eq!(error.for_lifetime, None);
    assert_eq!(error.instantiation_span.str(), "*ref");
}

const MOVED_OUT_WHILE_BORROWED: &str = r#"
public function consume[T](x: T) {}

public function test[T](mutable x: (T, T)) {
    let y = &x;
    
    consume(x.0);
    consume(y);
}
"#;

#[test]
fn moved_out_while_borrowed() {
    let (_, errs) = build_table(MOVED_OUT_WHILE_BORROWED).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<MovedOutWhileBorrowed>().unwrap();

    assert_eq!(error.borrow_usage_span.str(), "y");
    assert_eq!(error.moved_out_span.str(), "x.0");
}

const REBORROW: &str = r#"
public function test['a, 'b, T](
    mutable x: &'a T,
    y: &'b (T, T)
)
where
    'b: 'a,
    T: 'a + 'b,
{
    x = &y->0;
}
"#;

#[test]
fn reborrow() {
    assert!(build_table(REBORROW).is_ok());
}

const INVARIANT_LIFETIME: &str = r#"
public function consume[T](x: T) {}

public function assign['a, T] (
    reference: &'a mutable T,
    value: T
)
where
    T: 'a
{
    *reference = value;
}

public function test() {
    let outer =  32;
    let mutable ref = &outer;

    {
        let inner = 0;
        assign(&mutable ref, &inner);
    }

    consume(*ref);
}
"#;

#[test]
fn invariant_lifetime() {
    let (_, errs) = build_table(INVARIANT_LIFETIME).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough<ir::Model>>()
        .unwrap();

    assert_eq!(error.variable_span.str(), "inner");
    assert_eq!(error.for_lifetime, None);
    assert_eq!(error.instantiation_span.str(), "*ref");
}

const MUTABLY_ACCESS_WHILE_BORROWED: &str = r#"
public function consume[T](x: T) {}

public function test() {
    let mutable number = 1;
    let numberRef = &number;

    number = 2;

    consume(*numberRef);
}
"#;

#[test]
fn mutably_access_while_borrowed() {
    let (_, errs) = build_table(MUTABLY_ACCESS_WHILE_BORROWED).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error =
        &errs[0].as_any().downcast_ref::<MutablyAccessWhileBorrowed>().unwrap();

    assert_eq!(error.borrow_span.as_ref().map(|x| x.str()), Some("&number"));
    assert_eq!(error.mutable_access.str(), "2");
    assert_eq!(error.borrow_usage_span.str(), "*numberRef");
}

const VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH_IN_LOOP: &str = r#"
public function test(cond: bool) {
    let outer = 1;
    let mutable numberRef = &outer;

   while (cond) {
        let inner = 0;
        numberRef = &inner;
    }

    let a = *numberRef;
}
"#;

#[test]
fn variable_does_not_live_long_enough_in_loop() {
    let (_, errs) =
        build_table(VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH_IN_LOOP).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = &errs[0]
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough<ir::Model>>()
        .unwrap();

    assert_eq!(error.variable_span.str(), "inner");
    assert_eq!(error.for_lifetime, None);
    assert_eq!(error.instantiation_span.str(), "*numberRef");
}

const VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH_IN_INNER_LOOP: &str = r#"
public function test(cond: bool) {
    let outer = 1;
    let mutable numberRef = &outer;

    while (cond) {
        let inner = 0;
        numberRef = &inner;

        while (cond) {
            let innerInner = 0;
            numberRef = &innerInner;
        }
    }

    let a = *numberRef;
}
"#;

#[test]
fn variable_does_not_live_long_enough_in_inner_loop() {
    let (_, errs) =
        build_table(VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH_IN_INNER_LOOP)
            .unwrap_err();

    assert_eq!(errs.len(), 2);

    assert!(errs.iter().any(|x| x
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough<ir::Model>>()
        .map_or(false, |x| {
            x.variable_span.str() == "inner"
                && x.for_lifetime.is_none()
                && x.instantiation_span.str() == "*numberRef"
        })));

    assert!(errs.iter().any(|x| x
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough<ir::Model>>()
        .map_or(false, |x| {
            x.variable_span.str() == "innerInner"
                && x.for_lifetime.is_none()
                && x.instantiation_span.str() == "*numberRef"
        })));
}

const RESET_BORROWED_REFERENCE_WHEN_BREAK: &str = r#"
public function test(cond: bool) {
    let outer = 1;
    let mutable numberRef = &outer;

    loop {    
        let inner = 2;
        numberRef = &inner;

        if (cond) {
            numberRef = &outer;
            break;
        }
    }

    let a = *numberRef;
}
"#;

#[test]
fn reset_borrowed_reference_when_break() {
    assert!(build_table(RESET_BORROWED_REFERENCE_WHEN_BREAK).is_ok());
}

const MUTABLY_ACCESS_MORE_THAN_ONCE_IN_FUNCTION: &str = r#"
// fake vector
public struct Vector[T] {}

implements[T] Vector[T] {
    public function new(): this { panic; }
    public function push['a](self: &'a mutable this, value: T)
    where
        T: 'a
    { 
        panic; 
    }
}

public function main() {
    let mutable number = 0;
    let mutable vector = Vector::new();

    Vector::push(&mutable vector, &mutable number);
    Vector::push(&mutable vector, &mutable number);
}
"#;

#[test]
fn mutably_access_more_than_once_in_function() {
    let (_, errs) =
        build_table(MUTABLY_ACCESS_MORE_THAN_ONCE_IN_FUNCTION).unwrap_err();

    dbg!(&errs);

    assert_eq!(errs.len(), 1);

    let error =
        &errs[0].as_any().downcast_ref::<MutablyAccessWhileBorrowed>().unwrap();

    assert_eq!(
        error.borrow_span.as_ref().map(|x| x.str()),
        Some("&mutable number")
    );
    assert_eq!(error.mutable_access.str(), "&mutable number");
    assert_eq!(
        error.borrow_usage_span.str(),
        "Vector::push(&mutable vector, &mutable number)"
    );
}
