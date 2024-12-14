use pernixc_base::handler::Panic;

use crate::{
    error::{
        AccessWhileMutablyBorrowed, MoveInLoop,
        MovedOutValueFromMutableReference, MovedOutWhileBorrowed,
        MutablyAccessWhileImmutablyBorrowed, UnsatisfiedPredicate,
        UseAfterMove, VariableDoesNotLiveLongEnough,
    },
    ir,
    symbol::table::{
        representation::test::{build_table, parse},
        resolution::Config,
    },
    type_system::predicate::{Outlives, Predicate},
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

    assert_eq!(errs.len(), 2);

    errs.iter().any(|x| {
        x.as_any().downcast_ref::<MovedOutWhileBorrowed>().map_or(false, |x| {
            x.moved_out_span.str() == "x.0" && x.borrow_usage_span.str() == "y"
        })
    });

    errs.iter().any(|x| {
        x.as_any().downcast_ref::<MovedOutWhileBorrowed>().map_or(false, |x| {
            x.moved_out_span.str() == "x"
                && x.borrow_usage_span.str() == "consume(y)"
        })
    });
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
    dbg!(&errs);

    let error = &errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(
        error.immutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&number")
    );
    assert_eq!(error.mutable_access_span.str(), "number = 2");
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
public struct Vector[T] {
}

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

    assert_eq!(errs.len(), 1);

    let error =
        &errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&mutable number")
    );
    assert_eq!(error.access_span.str(), "&mutable number");
    assert_eq!(
        error.borrow_usage_span.str(),
        "Vector::push(&mutable vector, &mutable number)"
    );
}

const MUTABLY_ACCESS_MORE_THAN_ONCE_IN_FUNCTION_WITH_VARIABLE: &str = r#"
// fake vector
public struct Vector[T] {
}

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

    let v = &mutable vector;
    let n = &mutable number;
    Vector::push(v, n);

    let v = &mutable vector;
    let n = &mutable number;
    Vector::push(v, n);
}
"#;

#[test]
fn mutably_access_more_than_once_in_function_with_variable() {
    let (_, errs) =
        build_table(MUTABLY_ACCESS_MORE_THAN_ONCE_IN_FUNCTION_WITH_VARIABLE)
            .unwrap_err();

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&mutable number")
    );
    assert_eq!(error.access_span.str(), "&mutable number");
    assert_eq!(error.borrow_usage_span.str(), "v");
}

const AN_ALIASED_FORMULATION: &str = r#"
// fake vector
public struct Vector[T] {
}

implements[T] Vector[T] {
    public function new(): this { panic; }
    public function push['a](self: &'a mutable this, value: T)
    where
        T: 'a
    {
        panic;
    }
}

public function take[T](..: T) {}

public function main() {
    let mutable x = 22;
    let mutable v = Vector::new();
    let r = &mutable v;
    let p = &x;       // 1. `x` is borrowed here to create `p`
    r->push(p);        // 2. `p` is stored into `v`, but through `r`
    x += 1;           // <-- Error! can't mutate `x` while borrowed
    take(v);          // 3. the reference to `x` is later used here
}
"#;

#[test]
fn an_aliased_formulation() {
    let (_, errs) = build_table(AN_ALIASED_FORMULATION).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(
        error.immutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&x")
    );
    assert_eq!(error.mutable_access_span.str(), "x += 1");
    assert_eq!(error.borrow_usage_span.str(), "v");
}

// the test case is lifted from
// https://smallcultfollowing.com/babysteps/blog/2023/09/22/polonius-part-1/
const POLONIUS_ONE_EXAMPLE: &str = r#"
// only (D) is an error
public function main() {
    let mutable x = 22;
    let mutable y = 44;
    let mutable p = &x; // Loan L0, borrowing `x`
    y += 1;                  // (A) Mutate `y` -- is this ok?
    let mutable q = &y; // Loan L1, borrowing `y`
    if (true) {
        p = q;               // `p` now points at `y`
        x += 1;              // (B) Mutate `x` -- is this ok?
    } else {
        y += 1;              // (C) Mutate `y` -- is this ok?
    }
    y += 1;                  // (D) Mutate `y` -- is this ok?
    let a = *p;           // use `p` again here
}
"#;

#[test]
fn polonius_one_example() {
    let (_, errs) = build_table(POLONIUS_ONE_EXAMPLE).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(
        error.immutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&y")
    );
    assert_eq!(error.mutable_access_span.str(), "y += 1");
    assert_eq!(error.borrow_usage_span.str(), "*p");
}

const STRUCT_INFERENCE_NO_ERROR: &str = r#"
public struct MyPair[T, U] {
    public first: T,
    public second: U,
}

public function print[T](value: T) {}

public function main() {
    let mutable outer = 32;
    {
        let mutable inner = 64;

        let pair = MyPair {
            first: &outer,
            second: &inner,
        };

        outer = 32;

        print(*pair.second);
    }
}
"#;

#[test]
fn struct_inference_no_error() {
    assert!(build_table(STRUCT_INFERENCE_NO_ERROR).is_ok());
}

const STRUCT_INFERENCE_WITH_ERROR: &str = r#"
public struct MyPair[T, U] {
    public first: T,
    public second: U,
}

public function print[T](value: T) {}

public function main() {
    let mutable outer = 32;
    {
        let mutable inner = 64;

        let pair = MyPair {
            first: &outer,
            second: &inner,
        };

        outer = 32;

        print(*pair.first);
    }
}
"#;

#[test]
fn struct_inference_with_error() {
    let (_, errs) = build_table(STRUCT_INFERENCE_WITH_ERROR).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(
        error.immutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&outer")
    );
    assert_eq!(error.mutable_access_span.str(), "outer = 32");
    assert_eq!(error.borrow_usage_span.str(), "*pair.first");
}

const STRUCT_INFERENCE_WITH_LIFETIME_FLOW: &str = r#"
public struct MyPair['a, 'b, T, U]
where
    T: 'a,
    U: 'b,
    'a: 'b, // 'a flows into 'b
{
    public first: &'a T,
    public second: &'b U,
}

public function print[T](value: T) {}

public function main() {
    let mutable outer = 32;
    {
        let mutable inner = 64;

        let pair = MyPair {
            first: &outer,
            second: &inner,
        };

        outer = 32;

        // since 'first' flows into 'second', this will error
        print(*pair.second);
    }
}
"#;

#[test]
fn struct_inference_with_lifetime_flow() {
    let (_, errs) =
        build_table(STRUCT_INFERENCE_WITH_LIFETIME_FLOW).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(
        error.immutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&outer")
    );
    assert_eq!(error.mutable_access_span.str(), "outer = 32");
    assert_eq!(error.borrow_usage_span.str(), "*pair.second");
}

const USE_MUTABLE_REF_TWICE: &str = r#"
public function print[T](value: T) {}

public function main() {
    let mutable x = 1;
    let ref = &mutable x;

    *ref = 1;
    *ref = 2;
}
"#;

#[test]
fn use_mutable_ref_twice() {
    assert!(build_table(USE_MUTABLE_REF_TWICE).is_ok());
}

const RETURN_LOCAL_REFERENCE: &str = r#"
public function test['a](param: &'a int32): &'a int32 {
    let local = 0;
    return &local;
}
"#;

#[test]
fn return_local_reference() {
    let (table, errs) = build_table(RETURN_LOCAL_REFERENCE).unwrap_err();

    assert_eq!(errs.len(), 1);

    let test_function = table.get_by_qualified_name(["test", "test"]).unwrap();

    let error = errs[0]
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough<ir::Model>>()
        .unwrap();

    assert_eq!(error.variable_span.str(), "local");
    assert_eq!(
        error.for_lifetime,
        Some(
            table
                .resolve_lifetime(
                    &parse("'a"),
                    test_function,
                    Config::default(),
                    &Panic
                )
                .unwrap()
        )
    );
    assert_eq!(error.instantiation_span.str(), "return &local");
}

const RETURN_WRONG_LIFETIME: &str = r#"
public function test['a, 'b](
    first: &'a int32,
    second: &'b int32,
): &'a int32 {
    return second;
}
"#;

#[test]
fn return_wrong_lifetime() {
    let (table, errs) = build_table(RETURN_WRONG_LIFETIME).unwrap_err();

    assert_eq!(errs.len(), 1);
    dbg!(&errs);

    let test_function = table.get_by_qualified_name(["test", "test"]).unwrap();

    let error = errs[0]
        .as_any()
        .downcast_ref::<UnsatisfiedPredicate<ir::Model>>()
        .unwrap();

    let a_lt = table
        .resolve_lifetime(
            &parse("'a"),
            test_function,
            Config::default(),
            &Panic,
        )
        .unwrap();
    let b_lt = table
        .resolve_lifetime(
            &parse("'b"),
            test_function,
            Config::default(),
            &Panic,
        )
        .unwrap();

    assert_eq!(
        error.predicate,
        Predicate::LifetimeOutlives(Outlives::new(b_lt, a_lt))
    );
}

const RETURN_CORRECT_LIFETIME: &str = r#"
public function test['a, 'b](
    test: &'a (int32, int32, int32),
): &'b int32 
where
    'a: 'b
{
    match (true, true) {
        (true, false): return &test->0,
        (false, false): return &test->1,
        (.., true):    return &test->2,
    }
}
"#;

#[test]
fn return_correct_lifetime() {
    assert!(build_table(RETURN_CORRECT_LIFETIME).is_ok());
}
