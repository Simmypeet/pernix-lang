use crate::{
    infer::{Constraint, InferenceContext},
    symbol::ty::PrimitiveType,
};

#[test]
fn inference_test() {
    let mut infer_ctx = InferenceContext::new();

    // let expr1 = 1; // Number
    // let expr2 = 1.0; // Float
    // let expr3 = -2; // Signed
    // expr1 + expr2; // expr1 ~ expr2
    // let x: f32 = expr1; // expr1 ~ f32

    let expr_id1 = infer_ctx.add_inference(Constraint::Number);
    let expr_id2 = infer_ctx.add_inference(Constraint::Float);
    let expr_id3 = infer_ctx.add_inference(Constraint::Signed);

    infer_ctx.unify(expr_id1, expr_id2).unwrap();

    assert_eq!(
        infer_ctx
            .get_inference(expr_id1)
            .as_type_variable()
            .unwrap()
            .constraint,
        Constraint::Float
    );
    assert_eq!(
        infer_ctx
            .get_inference(expr_id2)
            .as_type_variable()
            .unwrap()
            .constraint,
        Constraint::Float
    );
    assert_eq!(
        infer_ctx
            .get_inference(expr_id3)
            .as_type_variable()
            .unwrap()
            .constraint,
        Constraint::Signed
    );

    infer_ctx
        .unify_with_concrete(expr_id1, PrimitiveType::Float32.into())
        .unwrap();

    assert_eq!(
        infer_ctx
            .get_inference(expr_id1)
            .as_inferred()
            .unwrap()
            .as_primitive()
            .unwrap(),
        &PrimitiveType::Float32
    );
    assert_eq!(
        infer_ctx
            .get_inference(expr_id2)
            .as_inferred()
            .unwrap()
            .as_primitive()
            .unwrap(),
        &PrimitiveType::Float32
    );

    assert_eq!(
        infer_ctx
            .get_inference(expr_id3)
            .as_type_variable()
            .unwrap()
            .constraint,
        Constraint::Signed
    );
}