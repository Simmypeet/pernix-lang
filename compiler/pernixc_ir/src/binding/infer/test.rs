use std::sync::Arc;

use pernixc_handler::Panic;
use pernixc_semantic::Table;
use pernixc_semantic::term::r#type::{Primitive, Type};
use pernixc_type_system::environment::Premise;

use super::{Context, InferenceVariable};
use crate::model::Constraint;

impl Context {
    fn assert_all_same_constraint<'a>(
        &self,
        inference_types: impl IntoIterator<Item = &'a Type<super::Model>>,
        expected_constraint: Constraint,
    ) {
        let mut inference_variables = inference_types.into_iter();
        let Some(first) = inference_variables.next() else {
            return;
        };
        let first_inference = *self
            .get_inference(*first.as_inference().unwrap())
            .unwrap()
            .as_inferring()
            .unwrap();

        assert_eq!(
            self.type_inference_context
                .constraints
                .get(first_inference)
                .copied()
                .unwrap(),
            expected_constraint
        );

        for inference_variable in inference_variables {
            let inference = *self
                .get_inference(*inference_variable.as_inference().unwrap())
                .unwrap()
                .as_inferring()
                .unwrap();

            assert_eq!(
                self.type_inference_context
                    .constraints
                    .get(inference)
                    .copied()
                    .unwrap(),
                expected_constraint
            );
            assert_eq!(inference, first_inference);
        }
    }

    fn assert_all_same_known<'a>(
        &self,
        inference_types: impl IntoIterator<Item = &'a Type<super::Model>>,
        expected_known: &Type<super::Model>,
    ) {
        let inference_variables = inference_types.into_iter();

        for inference_variable in inference_variables {
            let inference = self
                .get_inference(*inference_variable.as_inference().unwrap())
                .unwrap()
                .as_known()
                .unwrap();

            assert_eq!(inference, expected_known);
        }
    }
}

#[test]
fn simple_inferring_constraint() {
    /*
     * example case:
     *
     * let a = 32;
     * // a is inferred as number
     * let mutable b = a;
     * // b and a are the same type inferred as number
     * b = 64.0;
     * // b and a are the same type inferred as floating
     * let mutable c = 128;
     * // c is inferred as number
     * c = b;
     * // c, b, and a are the same type inferred as floating
     * c = 256.0f64;
     * // c, b, and a are the same type inferred as known float64
     */

    let table = Table::new(Arc::new(Panic));
    let mut inference = Context::default();

    let a = Type::Inference(InferenceVariable::new());
    assert!(inference
        .type_inference_context
        .register(a.as_inference().copied().unwrap(), Constraint::Number,));
    let b = Type::Inference(InferenceVariable::new());
    assert!(inference
        .type_inference_context
        .register(b.as_inference().copied().unwrap(), Constraint::Floating,));
    assert!(inference.unify_type(&a, &b, &Premise::default(), &table,).is_ok());

    inference.assert_all_same_constraint([&a, &b], Constraint::Floating);

    let c = Type::Inference(InferenceVariable::new());
    assert!(inference
        .type_inference_context
        .register(c.as_inference().copied().unwrap(), Constraint::Number,));
    assert!(inference.unify_type(&b, &c, &Premise::default(), &table,).is_ok());

    inference.assert_all_same_constraint([&a, &b, &c], Constraint::Floating);

    assert!(inference
        .unify_type(
            &c,
            &Type::Primitive(Primitive::Float64),
            &Premise::default(),
            &table,
        )
        .is_ok());
    inference.assert_all_same_known(
        [&a, &b, &c],
        &Type::Primitive(Primitive::Float64),
    );
}
