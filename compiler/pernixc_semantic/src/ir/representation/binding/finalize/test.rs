use pernixc_base::handler::{Panic, Storage};

use crate::{
    error::{self, NotAllFlowPathsReturnAValue},
    ir::representation::binding::{
        expression::{Bind, Config, Target},
        test::{parse_expression, TestTemplate},
    },
    symbol::CallableID,
    type_system::term::r#type::{Primitive, Type},
};

impl TestTemplate {
    fn contains_not_all_flow_paths_return_a_value(
        &self,
        storage: &Storage<Box<dyn error::Error>>,
    ) {
        let vec = storage.as_vec();
        assert_eq!(vec.len(), 1);
        assert_eq!(
            vec[0]
                .as_any()
                .downcast_ref::<NotAllFlowPathsReturnAValue>()
                .unwrap()
                .callable_id,
            CallableID::Function(self.function_id)
        );
    }
}

#[test]
fn empty_function_not_return() {
    let test_tempalte =
        TestTemplate::new_with_return_type(Type::Primitive(Primitive::Bool));

    let (binder, storage) = test_tempalte.create_binder();

    binder.finalize(&storage);

    test_tempalte.contains_not_all_flow_paths_return_a_value(&storage);
}

#[test]
fn empty_loop_can_dont_return() {
    let test_tempalte =
        TestTemplate::new_with_return_type(Type::Primitive(Primitive::Bool));

    let (mut binder, storage) = test_tempalte.create_binder();

    binder
        .bind(
            &parse_expression("loop {}"),
            Config { target: Target::Statement },
            &Panic,
        )
        .unwrap();

    binder.finalize(&storage);

    assert!(storage.as_vec().is_empty());
}

#[test]
fn loop_with_break_not_return() {
    let test_tempalte =
        TestTemplate::new_with_return_type(Type::Primitive(Primitive::Bool));

    let (mut binder, storage) = test_tempalte.create_binder();

    binder
        .bind(
            &parse_expression("loop { break; }"),
            Config { target: Target::Statement },
            &Panic,
        )
        .unwrap();

    binder.finalize(&storage);

    test_tempalte.contains_not_all_flow_paths_return_a_value(&storage);
}

#[test]
fn exhaustive_match_all_return() {
    let test_tempalte =
        TestTemplate::new_with_return_type(Type::Primitive(Primitive::Bool));

    let (mut binder, storage) = test_tempalte.create_binder();

    binder
        .bind(
            &parse_expression(
                "match (true) { true: return true, false: return false }",
            ),
            Config { target: Target::Statement },
            &Panic,
        )
        .unwrap();

    binder.finalize(&storage);

    assert!(storage.as_vec().is_empty());
}

#[test]
fn not_all_arms_return() {
    const SOURCE: &str = r#"
        match (true, false) {
            (true, false): return true,
            (false, false): return false,
            (a, true): (),
        }
    "#;
    let test_tempalte =
        TestTemplate::new_with_return_type(Type::Primitive(Primitive::Bool));

    let (mut binder, storage) = test_tempalte.create_binder();

    binder
        .bind(
            &parse_expression(SOURCE),
            Config { target: Target::Statement },
            &Panic,
        )
        .unwrap();

    binder.finalize(&storage);

    test_tempalte.contains_not_all_flow_paths_return_a_value(&storage);
}
