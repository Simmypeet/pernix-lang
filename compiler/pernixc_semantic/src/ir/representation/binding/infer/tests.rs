use super::{Context, InferenceVariable};
use crate::{
    symbol::table::{representation::Insertion, Building, Table},
    type_system::{
        observer,
        term::r#type::{self, Primitive, Type},
    },
};

impl Context {
    fn assert_all_same_constraint<'a>(
        &self,
        inference_types: impl IntoIterator<Item = &'a Type<super::Model>>,
        expected_constraint: r#type::Constraint,
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

    let mut table = Table::<Building>::default();

    let Insertion { id: module_id, duplication } =
        table.create_root_module("test".to_string());
    let active_premise = table.get_active_premise(module_id.into()).unwrap();
    assert!(duplication.is_none());

    let mut inference = Context::default();

    let a = Type::Inference(InferenceVariable::new());
    assert!(inference.type_inference_context.register(
        a.as_inference().copied().unwrap(),
        r#type::Constraint::Number,
    ));
    let b = Type::Inference(InferenceVariable::new());
    assert!(inference.type_inference_context.register(
        b.as_inference().copied().unwrap(),
        r#type::Constraint::Floating,
    ));
    assert!(inference
        .unify_type(&a, &b, active_premise.clone(), &table, observer::NO_OP)
        .is_ok());

    inference
        .assert_all_same_constraint([&a, &b], r#type::Constraint::Floating);

    let c = Type::Inference(InferenceVariable::new());
    assert!(inference.type_inference_context.register(
        c.as_inference().copied().unwrap(),
        r#type::Constraint::Number,
    ));
    assert!(inference
        .unify_type(&b, &c, active_premise.clone(), &table, observer::NO_OP)
        .is_ok());

    inference
        .assert_all_same_constraint([&a, &b, &c], r#type::Constraint::Floating);

    assert!(inference
        .unify_type(
            &c,
            &Type::Primitive(Primitive::Float64),
            active_premise,
            &table,
            observer::NO_OP
        )
        .is_ok());
    inference.assert_all_same_known(
        [&a, &b, &c],
        &Type::Primitive(Primitive::Float64),
    );
}
