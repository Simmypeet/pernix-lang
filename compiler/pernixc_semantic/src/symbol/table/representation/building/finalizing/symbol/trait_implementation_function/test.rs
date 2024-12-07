use pernixc_base::handler;

use crate::{
    error::{
        FunctionSignatureIncompatibilityReason, FunctionSignaturePart,
        IncompatibleFunctionSignatureInImplementation,
        MismatchedFunctionParameterCountInImplementation,
    },
    symbol::table::{
        representation::{
            test::{build_table, parse},
            Index,
        },
        resolution::Config,
    },
};

const TESTING_TRAIT_SOURCE: &str = r#"
public trait Test['a, T]
where
    T: 'a + 'static
{
    public function test(self: &'static T): &'a T;
}
"#;

const MISMATCHED_PARAMETER_COUNT_SOURCE: &str = r#"
implements['a, T] Test['a, T] 
where
    T: 'a + 'static
{
    public function test(self: &'static T, extra: int32): &'a T {
        panic;
    }
}
"#;

#[test]
fn mismatched_parameter_count() {
    let (table, errors) = build_table(format!(
        "{TESTING_TRAIT_SOURCE}\n\n{MISMATCHED_PARAMETER_COUNT_SOURCE}"
    ))
    .unwrap_err();

    assert_eq!(errors.len(), 1, "{errors:#?}");

    let error = errors.first().unwrap();
    let error = error
        .as_any()
        .downcast_ref::<MismatchedFunctionParameterCountInImplementation>()
        .unwrap();

    let expected_function_id = table
        .get_by_qualified_name(["test", "Test", "test"])
        .unwrap()
        .into_trait_function()
        .unwrap();

    assert_eq!(error.expected_count, 1);
    assert_eq!(error.found_count, 2);
    assert_eq!(error.trait_function_id, expected_function_id);
}

const MISMATCHED_TYPE_SOURCE: &str = r#"
implements['a, T] Test['a, T] 
where
    T: 'a + 'static
{
    public function test(self: &'static int32): &'a int32 {
        panic;
    }
}
"#;

#[test]
fn mismatched_type() {
    let (table, errors) = build_table(format!(
        "{TESTING_TRAIT_SOURCE}\n\n{MISMATCHED_TYPE_SOURCE}"
    ))
    .unwrap_err();

    assert_eq!(errors.len(), 2, "{errors:#?}");

    let test_trait_id = table
        .get_by_qualified_name(["test", "Test"])
        .unwrap()
        .into_trait()
        .unwrap();
    let trait_sym = table.get(test_trait_id).unwrap();

    // should have only one implementation
    let trait_implementation_id =
        trait_sym.implementations.iter().copied().next().unwrap();

    let found_type = table
        .resolve_type(
            &parse("&'static int32"),
            trait_implementation_id.into(),
            Config::default(),
            &handler::Panic,
        )
        .unwrap();
    let expected_type = table
        .resolve_type(
            &parse("&'static T"),
            trait_implementation_id.into(),
            Config::default(),
            &handler::Panic,
        )
        .unwrap();

    use FunctionSignatureIncompatibilityReason::*;
    use FunctionSignaturePart::*;

    assert!(errors.iter().any(|x| {
        x.as_any()
            .downcast_ref::<IncompatibleFunctionSignatureInImplementation>()
            .map_or(false, |x| {
                x.found_parameter_type == found_type
                    && x.expected_parameter_type == expected_type
                    && x.part == Parameter
                    && x.reason == IncompatibleType
            })
    }));

    let found_type = table
        .resolve_type(
            &parse("&'a int32"),
            trait_implementation_id.into(),
            Config::default(),
            &handler::Panic,
        )
        .unwrap();
    let expected_type = table
        .resolve_type(
            &parse("&'a T"),
            trait_implementation_id.into(),
            Config::default(),
            &handler::Panic,
        )
        .unwrap();

    assert!(errors.iter().any(|x| {
        x.as_any()
            .downcast_ref::<IncompatibleFunctionSignatureInImplementation>()
            .map_or(false, |x| {
                x.found_parameter_type == found_type
                    && x.expected_parameter_type == expected_type
                    && x.part == Return
                    && x.reason == IncompatibleType
            })
    }));
}

const SUBTYPING_SOURCE: &str = r#"
implements['a, T] Test['a, T] 
where
    T: 'a + 'static
{
    public function test(self: &'a T): &'static T {
        panic;
    }
}
"#;

#[test]
fn subtyping() {
    assert!(build_table(format!(
        "{TESTING_TRAIT_SOURCE}\n\n{SUBTYPING_SOURCE}"
    ))
    .is_ok())
}
