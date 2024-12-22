use pernixc_base::handler::Panic;

use crate::{
    error::{
        AccessWhileMutablyBorrowed, MovedOutWhileBorrowed,
        MutablyAccessWhileImmutablyBorrowed, UnsatisfiedPredicate,
        VariableDoesNotLiveLongEnough,
    },
    ir::{self, representation::borrow::UniversalRegion},
    symbol::table::{
        representation::test::{build_table, parse},
        resolution::Config,
    },
    type_system::predicate::{Outlives, Predicate},
};

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

    errs.iter().any(|x| {
        x.as_any().downcast_ref::<MovedOutWhileBorrowed>().map_or(false, |x| {
            x.moved_out_span.str() == "x.0"
                && x.usage.as_local().map_or(false, |x| x.str() == "y")
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

    let error = &errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(
        error.immutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&number")
    );
    assert_eq!(error.mutable_access_span.str(), "number = 2");
    assert_eq!(error.usage.as_local().map(|x| x.str()), Some("*numberRef"));
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
    Vector::push(&mutable vector,  &mutable number);
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
        error.borrow_usage.as_local().map(|x| x.str()),
        Some("Vector::push(&mutable vector,  &mutable number)")
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
    assert_eq!(error.borrow_usage.as_local().map(|x| x.str()), Some("v"));
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
    let c = v;          // 3. the reference to `x` is later used here
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
    assert_eq!(error.usage.as_local().map(|x| x.str()), Some("v"));
}

// the test case is lifted from
// https://smallcultfollowing.com/babysteps/blog/2023/09/22/polonius-part-1/
const POLONIUS_ONE_EXAMPLE: &str = r#"
public function consume[T](x: T) {}

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
    consume(*p);           // use `p` again here
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
    assert_eq!(error.usage.as_local().map(|x| x.str()), Some("*p"));
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
    assert_eq!(error.usage.as_local().map(|x| x.str()), Some("*pair.first"));
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
    assert_eq!(error.usage.as_local().map(|x| x.str()), Some("*pair.second"));
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

const INVALIDATED_UNIVERSAL_REGIONS: &str = r#"
// fake vector
public struct Vector[T] {
}

implements[T] Vector[T] {
    public function new(): this { panic; }
    public function clear['a](self: &'a mutable this)
    where
        T: 'a
    {
        panic;
    }
    public function push['a](self: &'a mutable this, value: T)
    where
        T: 'a
    {
        panic;
    }
}

public function test['a, 'b](
    numbers: &'b mutable Vector[&'a mutable int32],
    number: &'a mutable int32,
)
where
    'a: 'b
{
    numbers->push(&mutable *number);
    *number;
}
"#;

#[test]
fn invalidated_universal_regions() {
    let (table, errs) = build_table(INVALIDATED_UNIVERSAL_REGIONS).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    let function = table.get_by_qualified_name(["test", "test"]).unwrap();

    let a_lt = table
        .resolve_lifetime::<ir::Model>(
            &parse("'a"),
            function,
            Config::default(),
            &Panic,
        )
        .unwrap();
    let b_lt = table
        .resolve_lifetime::<ir::Model>(
            &parse("'b"),
            function,
            Config::default(),
            &Panic,
        )
        .unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&mutable *number")
    );
    assert_eq!(error.access_span.str(), "*number");

    let by_universal_region =
        error.borrow_usage.as_by_universal_regions().unwrap();

    assert_eq!(by_universal_region.len(), 2);

    assert!(by_universal_region.contains(&UniversalRegion::LifetimeParameter(
        a_lt.into_parameter().unwrap()
    )));
    assert!(by_universal_region.contains(&UniversalRegion::LifetimeParameter(
        b_lt.into_parameter().unwrap()
    )));
}

const VALID_USE_OF_UNIVERSAL_REGIONS: &str = r#"

// fake vector
public struct Vector[T] {
}

implements[T] Vector[T] {
    public function new(): this { panic; }
    public function clear['a](self: &'a mutable this)
    where
        T: 'a
    {
        panic;
    }
    public function push['a](self: &'a mutable this, value: T)
    where
        T: 'a
    {
        panic;
    }
}


public function consume[T](..: T) {}

public function test['a, 'b](
    numbers: &'b mutable Vector[&'a mutable int32],
    number: &'a mutable int32,
)
where
    'a: 'b
{
    numbers->push(&mutable *number);
    numbers->clear();
}
"#;

#[test]
fn valid_use_of_universal_regions() {
    assert!(build_table(VALID_USE_OF_UNIVERSAL_REGIONS).is_ok());
}

const STRUCT_REGION_INFERENCE: &str = r#"
public struct RefWrapper['a, T]
where
    T: 'a
{
    public ref: &'a T,
}

public function test['a, 'b](
    ref: &'a int32
): RefWrapper['b, int32] {
    let test = 32;
    let another = &test;

    return RefWrapper { ref: ref };
}
"#;

#[test]
fn struct_region_inference() {
    let (table, errs) = build_table(STRUCT_REGION_INFERENCE).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<UnsatisfiedPredicate<ir::Model>>()
        .unwrap();

    let function = table.get_by_qualified_name(["test", "test"]).unwrap();

    let a_lt = table
        .resolve_lifetime(&parse("'a"), function, Config::default(), &Panic)
        .unwrap();

    let b_lt = table
        .resolve_lifetime(&parse("'b"), function, Config::default(), &Panic)
        .unwrap();

    assert_eq!(
        error.predicate,
        Predicate::LifetimeOutlives(Outlives::new(a_lt, b_lt))
    );

    assert_eq!(
        error.instantiation_span.str(),
        "return RefWrapper { ref: ref }"
    );
}

const RETURN_INVALIDATED_UNIVERSAL_REGIONS: &str = r#"
public function test['a](
    ref: &'a mutable int32
): (&'a mutable int32, &'a mutable int32) {
    return (&mutable *ref, &mutable *ref);
}
"#;

#[test]
fn return_invalidated_universal_regions() {
    let (_, errs) =
        build_table(RETURN_INVALIDATED_UNIVERSAL_REGIONS).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&mutable *ref")
    );
    assert_eq!(error.access_span.str(), "&mutable *ref");
    assert_eq!(
        error.borrow_usage.as_local().map(|x| x.str()),
        Some("(&mutable *ref, &mutable *ref)")
    );
}

const ARRAY_INFERENCE: &str = r#"
public function consume[T](..: T) {}

public function test() {
    let mutable x = 0;
    let array = [&mutable x, &mutable x];

}
"#;

#[test]
fn array_inference() {
    let (_, errs) = build_table(ARRAY_INFERENCE).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&mutable x")
    );

    assert_eq!(error.access_span.str(), "&mutable x");
    assert_eq!(
        error.borrow_usage.as_local().map(|x| x.str()),
        Some("[&mutable x, &mutable x]")
    );
}

const PHI_INFERENCE_VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH: &str = r#"
public function test(cond: bool) {
    let outer = 1;

    let ref = 'result: {
        let inner = 2;
        if (true) {
            express 'result &inner;
        } else if (true) {
            let anotherInner = 3;
            express 'result &anotherInner;
         }

         express &outer;
    };
}
"#;

#[test]
fn phi_inference_variable_does_not_live_long_enough() {
    let (_, errs) =
        build_table(PHI_INFERENCE_VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH)
            .unwrap_err();

    assert_eq!(errs.len(), 2);

    assert!(errs.iter().any(|x| x
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough<ir::Model>>()
        .map_or(false, |x| {
            x.variable_span.str() == "inner" && x.for_lifetime.is_none()
        })));

    assert!(errs.iter().any(|x| x
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough<ir::Model>>()
        .map_or(false, |x| {
            x.variable_span.str() == "anotherInner" && x.for_lifetime.is_none()
        })));
}

const PHI_INFERENCE_USE_INVALIDATED_REFERENCE: &str = r#"
public function test(cond: bool) {
    let mutable first = 1;
    let mutable second = 1;

    let ref = 'result: {
        if (cond) {
            express 'result &first;
        } else {
            express 'result &second;
        }
    };

    first = 2;       // <- Error: first is borrowed by ref
    let copy = *ref; // <- Error: ref is invalidated
}
"#;

#[test]
fn phi_inference_use_invalidated_reference() {
    let (_, errs) =
        build_table(PHI_INFERENCE_USE_INVALIDATED_REFERENCE).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(
        error.immutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&first")
    );
    assert_eq!(error.mutable_access_span.str(), "first = 2");
    assert_eq!(error.usage.as_local().map(|x| x.str()), Some("*ref"));
}

const INVALIDATED_BORROWS_IN_LOOP: &str = r#"
// fake vector
public struct Vector[T] {
}

implements[T] Vector[T] {
    public function new(): this { panic; }
    public function clear['a](self: &'a mutable this)
    where
        T: 'a
    {
        panic;
    }
    public function push['a](self: &'a mutable this, value: T)
    where
        T: 'a
    {
        panic;
    }
}

public function main(cond: bool) {
    let mutable vector = Vector::new();
    let mutable number = 0;

    while (cond) {
        vector.push(&mutable number);
    }
}
"#;

#[test]
fn invalidated_borrows_in_loop() {
    let (_, errs) = build_table(INVALIDATED_BORROWS_IN_LOOP).unwrap_err();

    dbg!(&errs);
    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&mutable number")
    );
    assert_eq!(error.access_span.str(), "&mutable number");
    assert_eq!(error.borrow_usage.as_local().map(|x| x.str()), Some("vector"));
}

const INVALIDATED_BORROWS_IN_LOOP_WITH_BREAK: &str = r#"
// fake vector
public struct Vector[T] {
}

implements[T] Vector[T] {
    public function new(): this { panic; }
    public function clear['a](self: &'a mutable this)
    where
        T: 'a
    {
        panic;
    }
    public function push['a](self: &'a mutable this, value: T)
    where
        T: 'a
    {
        panic;
    }
}

public function main(cond: bool) {
    let mutable vector = Vector::new();
    let mutable number = 0;

    while (cond) {
        vector.push(&mutable number);
        break;
    }
}
"#;

#[test]
fn invalidated_borrows_in_loop_with_break() {
    assert!(build_table(INVALIDATED_BORROWS_IN_LOOP_WITH_BREAK).is_ok());
}

const MUTABLY_BORROW_IN_LOOP: &str = r#"
// fake Vector
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

public function main(cond: bool) {
    while (cond) {
        let mutable vector = Vector::new();
        let mutable number = 0;

        vector.push(&mutable number);
    }
}
"#;

#[test]
fn mutably_borrow_in_loop() {
    assert!(build_table(MUTABLY_BORROW_IN_LOOP).is_ok());
}

const VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH_IN_LOOP_2: &str = r#"
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

public function main(cond: bool) {
    let mutable vector = Vector::new();
    while (cond) {
        let mutable number = 0;
        vector.push(&mutable number);
    }
}
"#;

#[test]
fn variable_does_not_live_long_enough_in_loop_2() {
    let (_, errs) =
        build_table(VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH_IN_LOOP_2).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough<ir::Model>>()
        .unwrap();

    assert_eq!(error.variable_span.str(), "mutable number");
    assert_eq!(error.for_lifetime, None);
    assert_eq!(error.instantiation_span.str(), "vector");
}

const BORROW_IN_LOOP: &str = r#"
public function main() {
    let outer = 32;
    let mutable test = &outer;

    while (true) {
        let inner = 32;
        test = &inner;

        let copy = *test;
    }
}
"#;

#[test]
fn borrow_in_loop() {
    assert!(build_table(BORROW_IN_LOOP).is_ok());
}

const POSSIBLE_USE_OF_GOING_OUT_OF_SCOPE_REFERENCE: &str = r#"
public function test(cond: bool) {
    let outer = 1;
    let mutable ref = &outer;

    while (true) {
        let inner = 2;
        if (true) {
            ref = &inner;
        }
        let copy = *ref;
    }
}
"#;

#[test]
fn possible_use_of_going_out_of_scope_reference() {
    let (_, errs) =
        build_table(POSSIBLE_USE_OF_GOING_OUT_OF_SCOPE_REFERENCE).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough<ir::Model>>()
        .unwrap();

    assert_eq!(error.variable_span.str(), "inner");
    assert_eq!(error.for_lifetime, None);
    assert_eq!(error.instantiation_span.str(), "*ref");
}

const ASSIGN_MUTABLE_REFERENCE_DOES_NOT_INVALIDATE: &str = r#"
public function main() {
    let mutable first = 32;
    let mutable second = 64;

    let mutable ref = &mutable first;
	let anoher = &mutable *ref;

    ref = &mutable second;
	let x = *anoher;
}
"#;

#[test]
fn mutable_reference_from_difference_places() {
    assert!(build_table(ASSIGN_MUTABLE_REFERENCE_DOES_NOT_INVALIDATE).is_ok());
}

const PUSH_TWO_MUTABLE_REFERENCES_FROM_DIFFERENT_PLACES: &str = r#"
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
    let mutable first = 32;
    let mutable second = 64;
    let mutable vector = Vector::new();

    let mutable ref = &mutable first;
    vector.push(&mutable *ref);

    ref = &mutable second;
    vector.push(&mutable *ref);
}
"#;

#[test]
fn push_two_mutable_references_from_different_places() {
    assert!(
        build_table(PUSH_TWO_MUTABLE_REFERENCES_FROM_DIFFERENT_PLACES).is_ok()
    );
}

const PUSH_TWO_MUTABLE_REFERENCES_FROM_DIFFERENT_WITH_COND: &str = r#"
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

public function main(cond: bool) {
    let mutable first = 32;
    let mutable second = 64;
    let mutable vector = Vector::new();

    let mutable ref = &mutable first;
    vector.push(&mutable *ref);

    if (cond) { 
        ref = &mutable second;
    } else {
        return;
    }
    vector.push(&mutable *ref);
}
"#;

#[test]
fn push_two_mutable_references_from_different_places_with_cond() {
    assert!(build_table(PUSH_TWO_MUTABLE_REFERENCES_FROM_DIFFERENT_WITH_COND)
        .is_ok());
}

const PUSH_TWO_MUTABLE_REFERENCES_FROM_DIFFERENT_PLACES_ERROR: &str = r#"
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

public function main(cond: bool) {
    let mutable first = 32;
    let mutable second = 64;
    let mutable vector = Vector::new();

    let mutable ref = &mutable first;
    vector.push(&mutable *ref);

    if (cond) { 
        ref = &mutable second;
    } else {
        // no reassignment, will error
    }
    vector.push(&mutable *ref);
}
"#;

#[test]
fn push_two_mutable_references_from_different_places_error() {
    let (_, errs) =
        build_table(PUSH_TWO_MUTABLE_REFERENCES_FROM_DIFFERENT_PLACES_ERROR)
            .unwrap_err();

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&mutable *ref")
    );
    assert_eq!(error.access_span.str(), "&mutable *ref");
    assert_eq!(error.borrow_usage.as_local().map(|x| x.str()), Some("vector"));
}

const REASSIGNED_REFERENCE: &str = r#"
public function print[T](value: T) {}

public function main() {
    let mutable x = 22;
    let y = 44;
    let mutable p = &x;
    p = &y;
    x += 1;
    print(*p);
}
"#;

#[test]
fn reassign_reference() {
    assert!(build_table(REASSIGNED_REFERENCE).is_ok());
}

const INVALIDATED_IMMUTABLE_REFERENCE_USED_IN_LOOP: &str = r#"
public function consume[T](x: T) {}

public function cond(): bool { panic; }

public function main() {
    let mutable test = 32;
    let r = &test;

    while (cond()) {
        // in first iteration, r is fine, no error
        // in second iteration, r is invalidated error should be raised
        consume(*r); 

        // r is invalidated here
        test = 64;
    }
}
"#;

#[test]
fn invalidated_immutable_reference_used_in_loop() {
    let (_, errs) =
        build_table(INVALIDATED_IMMUTABLE_REFERENCE_USED_IN_LOOP).unwrap_err();

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(
        error.immutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&test")
    );
    assert_eq!(error.mutable_access_span.str(), "test = 64");
    assert_eq!(error.usage.as_local().map(|x| x.str()), Some("*r"));
}

const MUTABLY_ACCESS_WHILE_MUTABLY_BORROWED: &str = r#"
public function testa() {
	let mutable test = 32;
	let refm = &mutable test;

	test = 64;

	*refm = 64;
}
"#;

#[test]
fn mutably_access_while_mutably_borrowed() {
    let (_, errs) =
        build_table(MUTABLY_ACCESS_WHILE_MUTABLY_BORROWED).unwrap_err();

    dbg!(&errs);

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(|x| x.str()),
        Some("&mutable test")
    );

    assert_eq!(error.access_span.str(), "test = 64");
    assert_eq!(
        error.borrow_usage.as_local().map(|x| x.str()),
        Some("*refm = 64")
    );
}

const VECTOR_PUSH_TWO_TIMES: &str = r#"
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
    let mutable vector = Vector::new();
    let mutable number = 0;

    vector.push(number);
    vector.push(number);
}
"#;

#[test]
fn vector_push_two_times() {
    assert!(build_table(VECTOR_PUSH_TWO_TIMES).is_ok());
}
