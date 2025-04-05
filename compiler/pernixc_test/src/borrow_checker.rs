//! Tests for borrow checker.

use pernixc_borrow_checker::{
    diagnostic::{
        AccessWhileMutablyBorrowed, MovedOutWhileBorrowed,
        MutablyAccessWhileImmutablyBorrowed, VariableDoesNotLiveLongEnough,
    },
    NonStaticUniversalRegion, UniversalRegion,
};
use pernixc_builder::utility::build_table;
use pernixc_semantic::{
    component::derived::{
        generic_parameters::{GenericParameters, LifetimeParameterID},
        ir::model::Model as IRModel,
    },
    term::{
        lifetime::Lifetime,
        predicate::{Outlives, Predicate},
    },
};
use pernixc_source_file::GlobalSpan;
use pernixc_type_system::diagnostic::{
    ImplementationIsNotGeneralEnough, UnsatisfiedPredicate,
};

const VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH: &str = r"
public function consume[T](x: T):
    pass

public function test():
    let outer = 0
    let mut ref = &outer

    scope:
        let inner = 0
        ref = &inner

    consume(*ref)

";

#[test]
fn variable_does_not_live_long_enough() {
    let (_, errs) = build_table(VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough>()
        .unwrap();

    assert_eq!(error.variable_span.str(), "inner");
    assert_eq!(error.borrow_span.str(), "&inner");
    assert_eq!(error.usage.as_local().map(GlobalSpan::str), Some("*ref"));
}

const MOVED_OUT_WHILE_BORROWED: &str = r"
public function consume[T](x: T):
    pass

public function test[T](mut x: (T, T)):
    let y = &x

    consume(x.0)

    consume(y)
";

#[test]
fn moved_out_while_borrowed() {
    let (_, errs) = build_table(MOVED_OUT_WHILE_BORROWED);

    assert_eq!(errs.len(), 1);

    errs.iter().any(|x| {
        x.as_any().downcast_ref::<MovedOutWhileBorrowed>().is_some_and(|x| {
            x.moved_out_span.str() == "x.0"
                && x.usage.as_local().is_some_and(|x| x.str() == "y")
        })
    });
}

const REBORROW: &str = r"
public function test['a, 'b, T](
    mut x: &'a T,
    y: &'b (T, T)
):
    where:
        'b: 'a
        T: 'a + 'b

    x = &y->0
";

#[test]
fn reborrow() {
    assert!(build_table(REBORROW).1.is_empty());
}

const INVARIANT_LIFETIME: &str = r"
public function consume[T](x: T):
    pass

public function assign['a, T] (
    reference: &'a mut T,
    value: T
):
    where:
        T: 'a

    *reference = value


public function test():
    let outer =  32
    let mut ref = &outer

    scope:
        let inner = 0
        assign(&mut ref, &inner)

    consume(*ref)

";

#[test]
fn invariant_lifetime() {
    let (_, errs) = build_table(INVARIANT_LIFETIME);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough>()
        .unwrap();

    assert_eq!(error.variable_span.str(), "inner");
    assert_eq!(error.borrow_span.str(), "&inner");
    assert_eq!(error.usage.as_local().map(GlobalSpan::str), Some("*ref"));
}

const MUTABLY_ACCESS_WHILE_BORROWED: &str = r"
public function consume[T](x: T):
    pass

public function test():
    let mut number = 1
    let numberRef = &number

    number = 2

    consume(*numberRef)

";

#[test]
fn mutably_access_while_borrowed() {
    let (_, errs) = build_table(MUTABLY_ACCESS_WHILE_BORROWED);

    assert_eq!(errs.len(), 1);

    let error = &errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(
        error.immutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&number")
    );
    assert_eq!(error.mutable_access_span.str(), "number = 2");
    assert_eq!(error.usage.as_local().map(GlobalSpan::str), Some("*numberRef"));
}

const VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH_IN_LOOP: &str = r"
public function test(cond: bool):
    let outer = 1
    let mut numberRef = &outer

    while cond:
        let inner = 0
        numberRef = &inner
    

    let a = *numberRef
";

#[test]
fn variable_does_not_live_long_enough_in_loop() {
    let (_, errs) = build_table(VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH_IN_LOOP);

    assert_eq!(errs.len(), 1);

    let error = &errs[0]
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough>()
        .unwrap();

    assert_eq!(error.variable_span.str(), "inner");
    assert_eq!(error.borrow_span.str(), "&inner");
    assert_eq!(error.usage.as_local().map(GlobalSpan::str), Some("*numberRef"));
}

const VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH_IN_INNER_LOOP: &str = r"
public function test(cond: bool):
    let outer = 1
    let mut numberRef = &outer

    while cond:
        let inner = 0
        numberRef = &inner

        while cond:
            let innerInner = 0
            numberRef = &innerInner


    let a = *numberRef
";

#[test]
fn variable_does_not_live_long_enough_in_inner_loop() {
    let (_, errs) =
        build_table(VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH_IN_INNER_LOOP);

    assert_eq!(errs.len(), 2);

    assert!(errs.iter().any(|x| x
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough>()
        .is_some_and(|x| {
            x.variable_span.str() == "inner"
                && x.borrow_span.str() == "&inner"
                && x.usage.as_local().map(GlobalSpan::str) == Some("*numberRef")
        })));

    assert!(errs.iter().any(|x| x
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough>()
        .is_some_and(|x| {
            x.variable_span.str() == "innerInner"
                && x.borrow_span.str() == "&innerInner"
                && x.usage.as_local().map(GlobalSpan::str) == Some("*numberRef")
        })));
}

const RESET_BORROWED_REFERENCE_WHEN_BREAK: &str = r"
public function test(cond: bool):
    let outer = 1
    let mut numberRef = &outer

    loop:
        let inner = 2
        numberRef = &inner

        if cond:
            numberRef = &outer
            break

    let a = *numberRef
";

#[test]
fn reset_borrowed_reference_when_break() {
    assert!(build_table(RESET_BORROWED_REFERENCE_WHEN_BREAK).1.is_empty());
}

const MUTABLY_ACCESS_MORE_THAN_ONCE_IN_FUNCTION: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T

implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a


public function main():
    let mut number = 0
    let mut vector = Vector::new()

    Vector::push(&mut vector, &mut number)
    Vector::push(&mut vector,  &mut number)

";

#[test]
fn mutably_access_more_than_once_in_function() {
    let (_, errs) = build_table(MUTABLY_ACCESS_MORE_THAN_ONCE_IN_FUNCTION);

    assert_eq!(errs.len(), 1);

    let error =
        &errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&mut number")
    );
    assert_eq!(error.access_span.str(), "&mut number");
    assert_eq!(
        error.borrow_usage.as_local().map(GlobalSpan::str),
        Some("Vector::push(&mut vector,  &mut number)")
    );
}

const MUTABLY_ACCESS_MORE_THAN_ONCE_IN_FUNCTION_WITH_VARIABLE: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T

implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

public function main():
    let mut number = 0
    let mut vector = Vector::new()

    let v = &mut vector
    let n = &mut number
    Vector::push(v, n)

    let v = &mut vector
    let n = &mut number
    Vector::push(v, n)

";

#[test]
fn mutably_access_more_than_once_in_function_with_variable() {
    let (_, errs) =
        build_table(MUTABLY_ACCESS_MORE_THAN_ONCE_IN_FUNCTION_WITH_VARIABLE);

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&mut number")
    );
    assert_eq!(error.access_span.str(), "&mut number");
    assert_eq!(error.borrow_usage.as_local().map(GlobalSpan::str), Some("v"));
}

const AN_ALIASED_FORMULATION: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T

implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

public function take[T](..: T):
    pass

public function main():
    let mut x = 22
    let mut v = Vector::new()
    let r = &mut v
    r->push(&x)        // 1. `&x` is stored into `v`, but throungh `r`
    x += 1             // 2. <-- Error! can't mutate `x` while borrowed
    let c = v          // 3. the reference to `x` is later used here
";

#[test]
fn an_aliased_formulation() {
    let (_, errs) = build_table(AN_ALIASED_FORMULATION);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(error.immutable_borrow_span.as_ref().map(GlobalSpan::str), Some("&x"));
    assert_eq!(error.mutable_access_span.str(), "x += 1");
    assert_eq!(error.usage.as_local().map(GlobalSpan::str), Some("v"));
}

// the test case is lifted from
// https://smallcultfollowing.com/babysteps/blog/2023/09/22/polonius-part-1/
const POLONIUS_ONE_EXAMPLE: &str = r"
public function consume[T](x: T):
    pass

// only (D) is an error
public function main():
    let mut x = 22
    let mut y = 44
    let mut p = &x  // Loan L0, borrowing `x`
    y += 1          // (A) Mutate `y` -- is this ok?
    let mut q = &y  // Loan L1, borrowing `y`
    if true:
        p = q       // `p` now points at `y`
        x += 1      // (B) Mutate `x` -- is this ok?
    else:
        y += 1      // (C) Mutate `y` -- is this ok?
    
    y += 1          // (D) Mutate `y` -- is this ok?
    consume(*p)     // use `p` again here
";

#[test]
fn polonius_one_example() {
    let (_, errs) = build_table(POLONIUS_ONE_EXAMPLE);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(error.immutable_borrow_span.as_ref().map(GlobalSpan::str), Some("&y"));
    assert_eq!(error.mutable_access_span.str(), "y += 1");
    assert_eq!(error.usage.as_local().map(GlobalSpan::str), Some("*p"));
}

const STRUCT_INFERENCE_NO_ERROR: &str = r"
public struct MyPair[T, U]:
    public first: T
    public second: U


public function print[T](value: T):
    pass

public function main():
    let mut outer = 32
    scope: 
        let mut inner = 64

        let pair = MyPair {
            first: &outer,
            second: &inner,
        }

        outer = 32

        print(*pair.second)
";

#[test]
fn struct_inference_no_error() {
    assert!(build_table(STRUCT_INFERENCE_NO_ERROR).1.is_empty());
}

const STRUCT_INFERENCE_WITH_ERROR: &str = r"
public struct MyPair[T, U]:
    public first: T
    public second: U


public function print[T](value: T):
    pass


public function main():
    let mut outer = 32
    
    scope:
        let mut inner = 64

        let pair = MyPair {
            first: &outer,
            second: &inner,
        }

        outer = 32

        print(*pair.first)
";

#[test]
fn struct_inference_with_error() {
    let (_, errs) = build_table(STRUCT_INFERENCE_WITH_ERROR);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(
        error.immutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&outer")
    );
    assert_eq!(error.mutable_access_span.str(), "outer = 32");
    assert_eq!(error.usage.as_local().map(GlobalSpan::str), Some("*pair.first"));
}

const STRUCT_INFERENCE_WITH_LIFETIME_FLOW: &str = r"
public struct MyPair['a, 'b, T, U]:
    where:
        T: 'a
        U: 'b
        'a: 'b // 'a flows into 'b

    public first: &'a T
    public second: &'b U


public function print[T](value: T):
    pass


public function main():
    let mut outer = 32

    scope:
        let mut inner = 64

        let pair = MyPair {
            first: &outer,
            second: &inner,
        }

        outer = 32

        // since 'first' flows into 'second', this will error
        print(*pair.second)
";

#[test]
fn struct_inference_with_lifetime_flow() {
    let (_, errs) = build_table(STRUCT_INFERENCE_WITH_LIFETIME_FLOW);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(
        error.immutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&outer")
    );
    assert_eq!(error.mutable_access_span.str(), "outer = 32");
    assert_eq!(error.usage.as_local().map(GlobalSpan::str), Some("*pair.second"));
}

const USE_MUTABLE_REF_TWICE: &str = r"
public function print[T](value: T):
    pass


public function main():
    let mut x = 1
    let ref = &mut x

    *ref = 1
    *ref = 2
";

#[test]
fn use_mutable_ref_twice() {
    assert!(build_table(USE_MUTABLE_REF_TWICE).1.is_empty());
}

const RETURN_LOCAL_REFERENCE: &str = r"
public function test['a](param: &'a int32) -> &'a int32:
    let local = 0
    return &local
";

#[test]
fn return_local_reference() {
    let (table, errs) = build_table(RETURN_LOCAL_REFERENCE);

    assert_eq!(errs.len(), 1);

    let test_function = table.get_by_qualified_name(["test", "test"]).unwrap();

    let error = errs[0]
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough>()
        .unwrap();

    let generic_parameters =
        table.query::<GenericParameters>(test_function).unwrap();

    let a_id = generic_parameters.lifetime_parameter_ids_by_name()["a"];

    assert_eq!(error.variable_span.str(), "local");
    assert_eq!(error.borrow_span.str(), "&local");
    assert!(error.usage.as_by_universal_regions().is_some_and(|x| x
        == &[UniversalRegion::NonStatic(NonStaticUniversalRegion::Named(
            LifetimeParameterID::new(test_function, a_id)
        ))]));
}

const RETURN_WRONG_LIFETIME: &str = r"
public function test['a, 'b](
    first: &'a int32,
    second: &'b int32,
) -> &'a int32:
    return &*second
";

#[test]
fn return_wrong_lifetime() {
    let (table, errs) = build_table(RETURN_WRONG_LIFETIME);

    assert_eq!(errs.len(), 1);

    let test_function = table.get_by_qualified_name(["test", "test"]).unwrap();

    let error = errs[0]
        .as_any()
        .downcast_ref::<UnsatisfiedPredicate<IRModel>>()
        .unwrap();

    let generic_params =
        table.query::<GenericParameters>(test_function).unwrap();

    let a_id = generic_params.lifetime_parameter_ids_by_name()["a"];
    let b_id = generic_params.lifetime_parameter_ids_by_name()["b"];

    assert_eq!(
        error.predicate,
        Predicate::LifetimeOutlives(Outlives::new(
            Lifetime::Parameter(LifetimeParameterID::new(test_function, b_id)),
            Lifetime::Parameter(LifetimeParameterID::new(test_function, a_id)),
        ))
    );
}

const RETURN_CORRECT_LIFETIME: &str = r"
public function test['a, 'b](
    test: &'a (int32, int32, int32),
) -> &'b int32:
    where:
        'a: 'b

    match (true, true):
        (true, false):  return &test->0
        (false, false): return &test->1
        (.., true):     return &test->2
";

#[test]
fn return_correct_lifetime() {
    assert!(build_table(RETURN_CORRECT_LIFETIME).1.is_empty());
}

const INVALIDATED_UNIVERSAL_REGIONS: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T


implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function clear['a](self: &'a mut this):
        where:
            T: 'a

        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

        panic

public function test['a, 'b](
    numbers: &'b mut Vector[&'a mut int32],
    number: &'a mut int32,
):
    where:
        'a: 'b

    numbers->push(&mut *number)
    *number

";

#[test]
fn invalidated_universal_regions() {
    let (table, errs) = build_table(INVALIDATED_UNIVERSAL_REGIONS);

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    let function = table.get_by_qualified_name(["test", "test"]).unwrap();

    let generic_params = table.query::<GenericParameters>(function).unwrap();

    let a_id = generic_params.lifetime_parameter_ids_by_name()["a"];
    let b_id = generic_params.lifetime_parameter_ids_by_name()["b"];

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&mut *number")
    );
    assert_eq!(error.access_span.str(), "*number");

    let by_universal_region =
        error.borrow_usage.as_by_universal_regions().unwrap();

    assert_eq!(by_universal_region.len(), 2);

    assert!(by_universal_region.contains(&UniversalRegion::NonStatic(
        NonStaticUniversalRegion::Named(LifetimeParameterID::new(
            function, a_id
        ))
    )));
    assert!(by_universal_region.contains(&UniversalRegion::NonStatic(
        NonStaticUniversalRegion::Named(LifetimeParameterID::new(
            function, b_id
        ))
    )));
}

const VALID_USE_OF_UNIVERSAL_REGIONS: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T


implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function clear['a](self: &'a mut this):
        where:
            T: 'a

        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

        panic


public function consume[T](..: T):
    pass


public function test['a, 'b](
    numbers: &'b mut Vector[&'a mut int32],
    number: &'a mut int32,
):
    where:
        'a: 'b

    numbers->push(&mut *number)
    numbers->clear()
";

#[test]
fn valid_use_of_universal_regions() {
    assert!(build_table(VALID_USE_OF_UNIVERSAL_REGIONS).1.is_empty());
}

const STRUCT_REGION_INFERENCE: &str = r"
public struct RefWrapper['a, T]:
    where:
        T: 'a

    public ref: &'a T


public function test['a, 'b](
    ref: &'a int32
) -> RefWrapper['b, int32]:
    let test = 32
    let another = &test

    return RefWrapper { ref: ref }
";

#[test]
fn struct_region_inference() {
    let (table, errs) = build_table(STRUCT_REGION_INFERENCE);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<UnsatisfiedPredicate<IRModel>>()
        .unwrap();

    let function = table.get_by_qualified_name(["test", "test"]).unwrap();

    let generic_params = table.query::<GenericParameters>(function).unwrap();

    let a_lt = Lifetime::Parameter(LifetimeParameterID::new(
        function,
        generic_params.lifetime_parameter_ids_by_name()["a"],
    ));
    let b_lt = Lifetime::Parameter(LifetimeParameterID::new(
        function,
        generic_params.lifetime_parameter_ids_by_name()["b"],
    ));

    assert_eq!(
        error.predicate,
        Predicate::LifetimeOutlives(Outlives::new(a_lt, b_lt))
    );

    assert_eq!(
        error.instantiation_span.str(),
        "return RefWrapper { ref: ref }"
    );
}

const REGISTER_USE_INVALIDATED_LIFETIMES: &str = r"
public function test['a](
    ref: &'a mut int32
): 
    let a =  (&mut *ref, &mut *ref)

";

#[test]
fn register_use_invalidated_lifetimes() {
    let (_, errs) = build_table(REGISTER_USE_INVALIDATED_LIFETIMES);

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&mut *ref")
    );

    assert_eq!(error.access_span.str(), "&mut *ref");

    assert_eq!(
        error.borrow_usage.as_local().map(GlobalSpan::str),
        Some("(&mut *ref, &mut *ref)")
    );
}

const ARRAY_INFERENCE: &str = r"
public function consume[T](..: T):
    pass

public function test():
    let mut x = 0
    let array = [&mut x, &mut x]
";

#[test]
fn array_inference() {
    let (_, errs) = build_table(ARRAY_INFERENCE);

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&mut x")
    );

    assert_eq!(error.access_span.str(), "&mut x");
    assert_eq!(
        error.borrow_usage.as_local().map(GlobalSpan::str),
        Some("[&mut x, &mut x]")
    );
}

const PHI_INFERENCE_VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH: &str = r"
public function test(cond: bool):
    let outer = 1

    let ref = scope 'result:
        let inner = 2

        if true:
            express 'result &inner
        else if true:
            let anotherInner = 3
            express 'result &anotherInner
         
        express &outer
";

#[test]
fn phi_inference_variable_does_not_live_long_enough() {
    let (_, errs) =
        build_table(PHI_INFERENCE_VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH);

    assert_eq!(errs.len(), 2);

    assert!(errs.iter().any(|x| x
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough>()
        .is_some_and(|x| {
            x.variable_span.str() == "inner" && x.borrow_span.str() == "&inner"
        })));

    assert!(errs.iter().any(|x| x
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough>()
        .is_some_and(|x| {
            x.variable_span.str() == "anotherInner"
                && x.borrow_span.str() == "&anotherInner"
        })));
}

const PHI_INFERENCE_USE_INVALIDATED_REFERENCE: &str = r"
public function test(cond: bool):
    let mut first = 1
    let mut second = 1

    let ref = scope 'result:
        if cond:
            express 'result &first
        else: 
            express 'result &second

    first = 2        // <- Error: first is borrowed by ref
    let copy = *ref  // <- Error: ref is invalidated
";

#[test]
fn phi_inference_use_invalidated_reference() {
    let (_, errs) = build_table(PHI_INFERENCE_USE_INVALIDATED_REFERENCE);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(
        error.immutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&first")
    );
    assert_eq!(error.mutable_access_span.str(), "first = 2");
    assert_eq!(error.usage.as_local().map(GlobalSpan::str), Some("*ref"));
}

const INVALIDATED_BORROWS_IN_LOOP: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T


implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function clear['a](self: &'a mut this):
        where:
            T: 'a

        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

        panic

public function main(cond: bool):
    let mut vector = Vector::new()
    let mut number = 0

    while cond:
        vector.push(&mut number)
";

#[test]
fn invalidated_borrows_in_loop() {
    let (_, errs) = build_table(INVALIDATED_BORROWS_IN_LOOP);

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&mut number")
    );
    assert_eq!(error.access_span.str(), "&mut number");
    assert_eq!(error.borrow_usage.as_local().map(GlobalSpan::str), Some("vector"));
}

const INVALIDATED_BORROWS_IN_LOOP_WITH_BREAK: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T


implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function clear['a](self: &'a mut this):
        where:
            T: 'a

        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

        panic


public function main(cond: bool):
    let mut vector = Vector::new()
    let mut number = 0

    while cond:
        vector.push(&mut number)
        break
";

#[test]
fn invalidated_borrows_in_loop_with_break() {
    assert!(build_table(INVALIDATED_BORROWS_IN_LOOP_WITH_BREAK).1.is_empty());
}

const MUTABLY_BORROW_IN_LOOP: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T

implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

public function main(cond: bool):
    while cond:
        let mut vector = Vector::new()
        let mut number = 0

        vector.push(&mut number)
";

#[test]
fn mutably_borrow_in_loop() {
    assert!(build_table(MUTABLY_BORROW_IN_LOOP).1.is_empty());
}

const VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH_IN_LOOP_2: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T

implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

public function main(cond: bool):
    let mut vector = Vector::new()

    while cond:
        let mut number = 0
        vector.push(&number)
";

#[test]
fn variable_does_not_live_long_enough_in_loop_2() {
    let (_, errs) = build_table(VARIABLE_DOES_NOT_LIVE_LONG_ENOUGH_IN_LOOP_2);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough>()
        .unwrap();

    assert_eq!(error.variable_span.str(), "mut number");
    assert_eq!(error.borrow_span.str(), "&number");
    assert_eq!(error.usage.as_local().map(GlobalSpan::str), Some("vector"));
}

const BORROW_IN_LOOP: &str = r"
public function main():
    let outer = 32
    let mut test = &outer

    while true:
        let inner = 32
        test = &inner

        let copy = *test
";

#[test]
fn borrow_in_loop() {
    assert!(build_table(BORROW_IN_LOOP).1.is_empty());
}

const POSSIBLE_USE_OF_GOING_OUT_OF_SCOPE_REFERENCE: &str = r"
public function test(cond: bool):
    let outer = 1
    let mut ref = &outer

    while (true):
        let inner = 2
        if true:
            ref = &inner
        
        let copy = *ref
";

#[test]
fn possible_use_of_going_out_of_scope_reference() {
    let (_, errs) = build_table(POSSIBLE_USE_OF_GOING_OUT_OF_SCOPE_REFERENCE);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough>()
        .unwrap();

    assert_eq!(error.variable_span.str(), "inner");
    assert_eq!(error.borrow_span.str(), "&inner");
    assert_eq!(error.usage.as_local().map(GlobalSpan::str), Some("*ref"));
}

const ASSIGN_MUTABLE_REFERENCE_DOES_NOT_INVALIDATE: &str = r"
public function main():
    let mut first = 32
    let mut second = 64

    let mut ref = &mut first
	let anoher = &mut *ref

    ref = &mut second
	let x = *anoher
";

#[test]
fn mutable_reference_from_difference_places() {
    assert!(build_table(ASSIGN_MUTABLE_REFERENCE_DOES_NOT_INVALIDATE)
        .1
        .is_empty());
}

const PUSH_TWO_MUTABLE_REFERENCES_FROM_DIFFERENT_PLACES: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T

implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

public function main():
    let mut first = 32
    let mut second = 64
    let mut vector = Vector::new()

    let mut ref = &mut first
    vector.push(&mut *ref)

    ref = &mut second
    vector.push(&mut *ref)
";

#[test]
fn push_two_mutable_references_from_different_places() {
    assert!(build_table(PUSH_TWO_MUTABLE_REFERENCES_FROM_DIFFERENT_PLACES)
        .1
        .is_empty());
}

const PUSH_TWO_MUTABLE_REFERENCES_FROM_DIFFERENT_WITH_COND: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T

implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

public function main(cond: bool):
    let mut first = 32
    let mut second = 64
    let mut vector = Vector::new()

    let mut ref = &mut first
    vector.push(&mut *ref)

    if cond:
        ref = &mut second
    else:
        return

    vector.push(&mut *ref)

";

#[test]
fn push_two_mutable_references_from_different_places_with_cond() {
    assert!(build_table(PUSH_TWO_MUTABLE_REFERENCES_FROM_DIFFERENT_WITH_COND)
        .1
        .is_empty());
}

const PUSH_TWO_MUTABLE_REFERENCES_FROM_DIFFERENT_PLACES_ERROR: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T

implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

public function main(cond: bool):
    let mut first = 32
    let mut second = 64
    let mut vector = Vector::new()

    let mut ref = &mut first
    vector.push(&mut *ref)

    if cond:
        ref = &mut second
    else:
        // no reassignment, will error
        pass

    
    vector.push(&mut *ref)
";

#[test]
fn push_two_mutable_references_from_different_places_error() {
    let (_, errs) =
        build_table(PUSH_TWO_MUTABLE_REFERENCES_FROM_DIFFERENT_PLACES_ERROR);

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&mut *ref")
    );
    assert_eq!(error.access_span.str(), "&mut *ref");
    assert_eq!(error.borrow_usage.as_local().map(GlobalSpan::str), Some("vector"));
}

const REASSIGNED_REFERENCE: &str = r"
public function print[T](value: T):
    pass
    

public function main():
    let mut x = 22
    let y = 44
    let mut p = &x
    p = &y
    x += 1
    print(*p)
";

#[test]
fn reassign_reference() {
    assert!(build_table(REASSIGNED_REFERENCE).1.is_empty());
}

const INVALIDATED_IMMUTABLE_REFERENCE_USED_IN_LOOP: &str = r"
public function consume[T](x: T):
    pass

public function cond() -> bool:
    panic

public function main():
    let mut test = 32
    let r = &test

    while cond():
        // in first iteration, r is fine, no error
        // in second iteration, r is invalidated error should be raised
        consume(*r)

        // r is invalidated here
        test = 64
";

#[test]
fn invalidated_immutable_reference_used_in_loop() {
    let (_, errs) = build_table(INVALIDATED_IMMUTABLE_REFERENCE_USED_IN_LOOP);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(
        error.immutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&test")
    );
    assert_eq!(error.mutable_access_span.str(), "test = 64");
    assert_eq!(error.usage.as_local().map(GlobalSpan::str), Some("*r"));
}

const MUTABLY_ACCESS_WHILE_MUTABLY_BORROWED: &str = r"
public function testa():
	let mut test = 32
	let refm = &mut test

	test = 64

	*refm = 64
";

#[test]
fn mutably_access_while_mutably_borrowed() {
    let (_, errs) = build_table(MUTABLY_ACCESS_WHILE_MUTABLY_BORROWED);

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&mut test")
    );

    assert_eq!(error.access_span.str(), "test = 64");
    assert_eq!(
        error.borrow_usage.as_local().map(GlobalSpan::str),
        Some("*refm = 64")
    );
}

const VECTOR_PUSH_TWO_TIMES: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T

implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

public function main():
    let mut vector = Vector::new()
    let mut number = 0

    vector.push(number)
    vector.push(number)
";

#[test]
fn vector_push_two_times() {
    assert!(build_table(VECTOR_PUSH_TWO_TIMES).1.is_empty());
}

const POLONIUS_TWO_EXAMPLE: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T

implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

public function consume[T](x: T):
    pass

public function main():
    let mut v = Vector::new()
    let p = &mut v
    let mut x = 1

    x = 2

    p->push(&x)

    x = 3

    consume(v)
";

#[test]
fn polonius_two_example() {
    let (_, errs) = build_table(POLONIUS_TWO_EXAMPLE);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(error.immutable_borrow_span.as_ref().map(GlobalSpan::str), Some("&x"));
    assert_eq!(error.mutable_access_span.str(), "x = 3");
    assert_eq!(error.usage.as_local().map(GlobalSpan::str), Some("v"));
}

/// these test cases are taken from
/// <https://github.com/rust-lang/polonius/blob/0a754a9e1916c0e7d9ba23668ea33249c7a7b59e/inputs/vec-push-ref/vec-push-ref.rs#L5>
const VEC_PUSH_REF_ONE: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T

implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

public function consume[T](x: T):
    pass

public function main(cond: bool):
    let mut x = 1
    let mut v = Vector::new()
    let p = &x

    if cond:
        v.push(p)
        x = 2
    else:
        x = 3
    
    consume(v)
";

#[test]
fn vec_push_ref_one() {
    let (_, errs) = build_table(VEC_PUSH_REF_ONE);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(error.immutable_borrow_span.as_ref().map(GlobalSpan::str), Some("&x"));
    assert_eq!(error.mutable_access_span.str(), "x = 2");
    assert_eq!(error.usage.as_local().map(GlobalSpan::str), Some("v"));
}

const VEC_PUSH_REF_TWO: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T

implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

public function consume[T](x: T):
    pass

public function main(cond: bool):
    let mut x = 1
    let mut v = Vector::new()
    let p = &x

    if cond:
        v.push(p)
    else:
        x = 2

    x = 3

    consume(v)
";

#[test]
fn vec_push_ref_two() {
    let (_, errs) = build_table(VEC_PUSH_REF_TWO);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(error.immutable_borrow_span.as_ref().map(GlobalSpan::str), Some("&x"));
    assert_eq!(error.mutable_access_span.str(), "x = 3");
    assert_eq!(error.usage.as_local().map(GlobalSpan::str), Some("v"));
}

const VEC_PUSH_REF_THREE: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T

implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

public function consume[T](x: T):
    pass

public function main(cond: bool):
    let mut x = 1
    let mut v = Vector::new()
    let p = &x

    if cond:
        v.push(p)
    else:
        x = 1

    consume(v)
";

#[test]
fn vec_push_ref_three() {
    assert!(build_table(VEC_PUSH_REF_THREE).1.is_empty());
}

const RETURN_WRONG_LIFETIME_WITH_JUMP: &str = r"
public function test['a, 'b](in: &'a int32) -> &'b int32:
	let result = in

	scope: // <-  this creates a jump in cfg
        pass

	return result
";

#[test]
fn return_wrong_lifetime_with_jump() {
    let (table, errs) = build_table(RETURN_WRONG_LIFETIME_WITH_JUMP);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<UnsatisfiedPredicate<IRModel>>()
        .unwrap();

    let function = table.get_by_qualified_name(["test", "test"]).unwrap();

    let generic_params = table.query::<GenericParameters>(function).unwrap();

    let a_lt = Lifetime::Parameter(LifetimeParameterID::new(
        function,
        generic_params.lifetime_parameter_ids_by_name()["a"],
    ));
    let b_lt = Lifetime::Parameter(LifetimeParameterID::new(
        function,
        generic_params.lifetime_parameter_ids_by_name()["b"],
    ));

    assert_eq!(
        error.predicate,
        Predicate::LifetimeOutlives(Outlives::new(a_lt, b_lt))
    );
}

const VEC_PUSH_LIST_REFS: &str = r"
public enum Option[T]:
	Some(T)
	None


implements[T] Option[T]:
	public function isSome['a](self: &'a this ) -> bool:
        where:
            T: 'a
	
		match self:
			case Some(_): return true
			case None:    return false
		
	

	public function asRef['a](self: &'a this) -> Option[&'a T]:
        where:
            T: 'a

		match self:
			case Some(value): return Option::Some(value)
			case None: return Option::None


    public function asMutable['a](
        self: &'a mut this
    ) -> Option[&'a mut T]:
        where:
            T: 'a

        match self:
            case Some(value): return Option::Some(value)
            case None: return Option::None


    public function unwrap(self: this) -> T:
        match self:
            case Some(value): return value
            case None: panic


// fake vector
public struct Vector[T]:
    private _marker: phantom T

implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a

public struct List[T]:
	public value: T
    
    // NOTE: this will create infinite sized struct in the future
	public next: Option[List[T]] 


public function createList[T]() -> List[T]:
	panic
    

public function getRefs['a, T](
	mut list: &'a mut List[T]
) -> Vector[&'a mut T]:
    where:
        T: 'a

	let mut vector = Vector::new()

	loop:
		vector.push(&mut list->value)

		match (&mut list->next):
			case Some(next):
				list = next

			case None: return vector

";

#[test]
fn vec_push_list_refs() {
    assert!(build_table(VEC_PUSH_LIST_REFS).1.is_empty());
}

const COMPLEX_LOOP: &str = r"
public function consume[T](..: T):
    panic

public function create['a]() -> &'a mut int32:
    panic

public function test(cond: bool):
	let mut a = 32
	let mut x = &mut a
	let mut q = create()    // create() No.1

	loop:
		// first iteration -> p refers to the `a` variable
		// second iteration -> p refers to the `create() No.2` function
		let p = &mut *x

		if cond:
            break
	
		// before loop -> q refers to the `create() No.1` function
		// first iteration -> q refers to the `a` variable
		// second iteration -> q refers to the `create() No.2` function
		q = p

		x = create() // create() No.2


	consume(&mut a)

	// q might refer to the `a` variable if the loop is executed twice 
	// (full first iteration, and second iteration hits the break)
	consume(q)
";

#[test]
fn complex_loop() {
    let (_, errs) = build_table(COMPLEX_LOOP);

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&mut a")
    );

    assert_eq!(error.access_span.str(), "&mut a");

    assert_eq!(error.borrow_usage.as_local().map(GlobalSpan::str), Some("q"));
}

const CONDITIONAL_CONTROL_FLOW_ACCROSS_FUNCTIONS: &str = r"
from core import Copy


public enum Option[T]:
	Some(T)
	None


implements[T] Option[T]:
	public function isSome['a](self: &'a this ) -> bool:
        where:
            T: 'a
	
		match self:
			case Some(_): return true
			case None:    return false
			

	public function asRef['a](self: &'a this) -> Option[&'a T]:
        where:
            T: 'a

		match self:
			case Some(value): return Option::Some(value)
			case None: return Option::None


    public function asMutable['a](
        self: &'a mut this
    ) -> Option[&'a mut T]:
        where:
            T: 'a

        match self:
            case Some(value): return Option::Some(value)
            case None: return Option::None


    public function unwrap(self: this) -> T:
        match self:
            case Some(value): return value
            case None: panic


// fake vector
public struct Vector[T]:
    private _marker: phantom T


implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a


public struct HashMap[K, V]:
	private test: phantom (K, V)


implements[K, V] HashMap[K, V]:
	public function new() -> this:
		return this {
			test: phantom
		}

	public function insert['a](
		self: &'a mut this, 
		key: K, 
		value: V
	) -> Option[V]:
        where:
            K: 'a
            V: 'a 

		panic


	public function get['a, 'b](
		self: &'a this, 
		key: &'b K
	) -> Option[&'a V]:
        where:
            K: 'a + 'b
            V: 'a

		panic


	public function getOrDefault['a](
		self: &'a mut this, 
		key: K,
		default: V
	) -> &'a V:
        where:
            K: Copy + 'a
            V: 'a

		match self->get(&key):
			case Some(value):
				return value

			case None:
				self->insert(key, default)
				return self->get(&key).unwrap()
";

#[test]
fn conditional_control_flow_accross_functions() {
    assert!(build_table(CONDITIONAL_CONTROL_FLOW_ACCROSS_FUNCTIONS)
        .1
        .is_empty());
}

const RETURN_INVALIDATED_UNIVERSAL_REGIONS_2: &str = r"
// fake vector
public struct Vector[T]:
    private _marker: phantom T

implements[T] Vector[T]:
    public function new() -> this:
        panic

    public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a


public function test['a, 'b](
	vector: &'a mut Vector[&'b mut int32],
	number: &'b mut int32
) -> &'b mut int32:
    where:
        'b: 'a

	// mutable ref number has already been stored in vector
	vector->push(&mut *number)

	// the caller can use the extra mutable reference, which is invalid
	return &mut *number
";

#[test]
fn return_invalidated_universal_regions_2() {
    let (table, errs) = build_table(RETURN_INVALIDATED_UNIVERSAL_REGIONS_2);

    assert_eq!(errs.len(), 1);

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    let function = table.get_by_qualified_name(["test", "test"]).unwrap();

    let generic_params = table.query::<GenericParameters>(function).unwrap();

    let a_lt = LifetimeParameterID::new(
        function,
        generic_params.lifetime_parameter_ids_by_name()["a"],
    );
    let b_lt = LifetimeParameterID::new(
        function,
        generic_params.lifetime_parameter_ids_by_name()["b"],
    );

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&mut *number")
    );
    assert_eq!(error.access_span.str(), "&mut *number");

    let by_universal_region =
        error.borrow_usage.as_by_universal_regions().unwrap();

    assert_eq!(by_universal_region.len(), 2);

    assert!(by_universal_region.contains(&UniversalRegion::NonStatic(
        NonStaticUniversalRegion::Named(a_lt)
    )));
    assert!(by_universal_region.contains(&UniversalRegion::NonStatic(
        NonStaticUniversalRegion::Named(b_lt)
    )));
}

const UNIVERSAL_REGIONS_THROUGH_TRAIT_PREDICATES: &str = r"
public trait Fizz['a, T, U]:
    public function doSomething['b](self: &'b T, t: &'a mut U):
        where:
            T: 'b
            U: 'a


public function test['a, 'b, T, U](t: &'b T, u: &'a mut U):
    where:
        trait Fizz['a, T, U]
        T: 'b
        U: 'a

    t->doSomething(&mut *u)
    t->doSomething(&mut *u)

";

#[test]
fn universal_regions_through_trait_predicates() {
    let (table, errs) = build_table(UNIVERSAL_REGIONS_THROUGH_TRAIT_PREDICATES);

    assert_eq!(errs.len(), 1);

    let function = table.get_by_qualified_name(["test", "test"]).unwrap();
    let generic_params = table.query::<GenericParameters>(function).unwrap();

    let a_lt = LifetimeParameterID::new(
        function,
        generic_params.lifetime_parameter_ids_by_name()["a"],
    );

    let error =
        errs[0].as_any().downcast_ref::<AccessWhileMutablyBorrowed>().unwrap();

    assert_eq!(
        error.mutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&mut *u")
    );

    assert_eq!(error.access_span.str(), "&mut *u");

    let by_universal_region =
        error.borrow_usage.as_by_universal_regions().unwrap();

    assert_eq!(by_universal_region, &[UniversalRegion::NonStatic(
        NonStaticUniversalRegion::Named(a_lt)
    ),]);
}

const BORROW_USAGE_IN_DROP: &str = r"
from core import Drop


public struct DropWrapper[T]:
    private value: T


implements[T] DropWrapper[T]:
    public function new(value: T) -> this:
        return this { value: value }


implements[T] Drop[DropWrapper[T]]:
    function drop['b](self: &'b mut DropWrapper[T]):
        where:
            DropWrapper[T]: 'b 


public function main():
    let mut number = 32
    let wrapper = DropWrapper::new(&number)

    number += 23
";

#[test]
fn borrow_usage_in_drop() {
    let (_, errs) = build_table(BORROW_USAGE_IN_DROP);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<MutablyAccessWhileImmutablyBorrowed>()
        .unwrap();

    assert_eq!(
        error.immutable_borrow_span.as_ref().map(GlobalSpan::str),
        Some("&number")
    );

    assert_eq!(error.mutable_access_span.str(), "number += 23");

    assert!(error.usage.is_drop());
}

const BORROW_USAGE_IN_DROP_BUT_NO_DROP: &str = r"
from core import Drop, NoDrop


public struct DropWrapper[T]:
    private value: T


implements[T] DropWrapper[T]:
    public function new(value: T) -> this:
        return this { value: value }


implements[T] Drop[DropWrapper[T]]:
    function drop['b](self: &'b mut DropWrapper[T]):
        where:
            DropWrapper[T]: 'b 


public function main():
    let mut number = 32
    let wrapper = NoDrop {
        value: DropWrapper::new(&number)
    }
     
    number += 23
";

#[test]
fn borrow_usage_in_drop_but_no_drop() {
    assert!(build_table(BORROW_USAGE_IN_DROP_BUT_NO_DROP).1.is_empty());
}

const UNIVERSAL_REGIONS_ALWAYS_FLOWS: &str = r"
public struct Test['a]:
	public first: &'a int32


public function test['a, 'b](test: &'b mut Test['a], cond: bool):
    where:
        'a: 'b

	let inner = 32
	let copy = test->first

	test->first = &inner
	test->first = copy

";

#[test]
fn universal_regions_always_flows() {
    let (table, errs) = build_table(UNIVERSAL_REGIONS_ALWAYS_FLOWS);

    assert_eq!(errs.len(), 1);

    let err = errs[0]
        .as_any()
        .downcast_ref::<VariableDoesNotLiveLongEnough>()
        .unwrap();

    let function = table.get_by_qualified_name(["test", "test"]).unwrap();

    let generic_params = table.query::<GenericParameters>(function).unwrap();

    let a_lt = LifetimeParameterID::new(
        function,
        generic_params.lifetime_parameter_ids_by_name()["a"],
    );
    let b_lt = LifetimeParameterID::new(
        function,
        generic_params.lifetime_parameter_ids_by_name()["b"],
    );

    assert_eq!(err.borrow_span.str(), "&inner");
    assert_eq!(err.variable_span.str(), "inner");

    let universal_regions = err.usage.as_by_universal_regions().unwrap();

    assert_eq!(universal_regions.len(), 2);

    assert!(universal_regions.contains(&UniversalRegion::NonStatic(
        NonStaticUniversalRegion::Named(a_lt)
    )));
    assert!(universal_regions.contains(&UniversalRegion::NonStatic(
        NonStaticUniversalRegion::Named(b_lt)
    )));
}

const IMPLEMENTATION_IS_NOT_GENERAL_ENOUGH: &str = r"
from core import Copy, Drop

public struct Vector[T]:
	private x: phantom T


implements[T] Vector[T]:
	public function new() -> this:
		return this { x: phantom }


	public function push['a](self: &'a mut this, value: T):
        where:
            T: 'a


public trait DoSomething['a, T]:
	public type Output

	public function doSomething['b](
		self: &'b mut T, 
		object: &'a mut int32
	):
        where:
            T: 'b


implements['a, 'c] DoSomething['a, Vector[&'c mut int32]]:
    where:
        'a: 'c

	type Output = int32

	function doSomething['b](
		self: &'b mut Vector[&'c mut int32], 
		object: &'a mut int32
	):
        where:
            Vector[&'c mut int32]: 'b 

        self->push(&mut *object)


public function use['a, T](object: &'a mut T):
    where:
        trait for['x] DoSomething['x, T]
        T: 'a

	let mut number = 32
	
	object->doSomething(&mut number)
	object->doSomething(&mut number)


public function main():
	let mut vector = Vector[&mut int32]::new()

	use(&mut vector)
";

#[test]
fn implementation_is_not_general_enough() {
    let (_, errs) = build_table(IMPLEMENTATION_IS_NOT_GENERAL_ENOUGH);

    assert_eq!(errs.len(), 1);

    let error = errs[0]
        .as_any()
        .downcast_ref::<ImplementationIsNotGeneralEnough<IRModel>>()
        .unwrap();

    assert_eq!(error.instantiation_span.str(), "use(&mut vector)");
}
