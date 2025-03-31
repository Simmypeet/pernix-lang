use pernixc_arena::ID;
use pernixc_semantic::component::derived::fields::{self, Field};
use pernixc_syntax::{syntax_tree, utility::parse};

use crate::binding::{
    diagnostic::{
        DuplicatedFieldInitialization, FieldIsNotAccessible, FieldNotFound,
        UninitializedFields,
    },
    test::{build_table, BindExt, CreateBinderAtExt},
};

const STRUCT_DECLARATION: &str = r"
public struct Vector2:
    public x: int32
    public y: int32


private module inner:
    public struct Vector2:
        public x: int32 
        private y: int32


public function test():
    pass
";

#[test]
fn struct_expression() {
    const STRUCT_EXPRESSION: &str = "Vector2 { x: 32, y: 64 }";

    let table = build_table(STRUCT_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let struct_id = table.get_by_qualified_name(["test", "Vector2"]).unwrap();
    let fields = table.query::<fields::Fields>(struct_id).unwrap();

    let x_field_id = fields.field_ids_by_name["x"];
    let y_field_id = fields.field_ids_by_name["y"];

    let register_id = binder
        .bind_as_rvalue_success(
            &parse::<syntax_tree::expression::unit::Struct>(STRUCT_EXPRESSION),
        )
        .into_register()
        .unwrap();

    let assert_struct_initializer =
        |field_id: ID<Field>, expected_string: &str| {
            let register = binder
                .intermediate_representation
                .values
                .registers
                .get(register_id)
                .unwrap();
            let struct_initializer = register.assignment.as_struct().unwrap();

            let numeric_literal = struct_initializer
                .initializers_by_field_id
                .get(&field_id)
                .unwrap()
                .as_literal()
                .unwrap()
                .as_numeric()
                .unwrap();

            assert_eq!(numeric_literal.integer_string, expected_string);
            assert!(numeric_literal.decimal_stirng.is_none());
        };

    assert_struct_initializer(x_field_id, "32");
    assert_struct_initializer(y_field_id, "64");
}

#[test]
fn struct_uninitialized_field_error() {
    const STRUCT_EXPRESSION: &str = "Vector2 { x: 32 }";

    let table = build_table(STRUCT_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let struct_id = table.get_by_qualified_name(["test", "Vector2"]).unwrap();
    let fields = table.query::<fields::Fields>(struct_id).unwrap();

    let y_field_id = fields.field_ids_by_name["y"];

    let (_, errors) = binder.bind_as_rvalue_error(&parse::<
        syntax_tree::expression::unit::Struct,
    >(STRUCT_EXPRESSION));

    assert_eq!(errors.len(), 1);
    let error =
        errors[0].as_any().downcast_ref::<UninitializedFields>().unwrap();

    assert_eq!(error.uninitialized_fields.len(), 1);
    assert!(error.uninitialized_fields.contains(&y_field_id));
}

#[test]
fn struct_duplicated_initialization_error() {
    const STRUCT_EXPRESSION: &str = "Vector2 { x: 32, y: 64, x: 128 }";

    let table = build_table(STRUCT_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let struct_id = table.get_by_qualified_name(["test", "Vector2"]).unwrap();
    let fields = table.query::<fields::Fields>(struct_id).unwrap();

    let x_field_id = fields.field_ids_by_name["x"];

    let (_, errors) = binder.bind_as_rvalue_error(&parse::<
        syntax_tree::expression::unit::Struct,
    >(STRUCT_EXPRESSION));

    assert_eq!(errors.len(), 1);
    let error = errors[0]
        .as_any()
        .downcast_ref::<DuplicatedFieldInitialization>()
        .unwrap();

    assert_eq!(error.field_id, x_field_id);
    assert_eq!(error.struct_id, struct_id);
    assert_eq!(error.prior_initialization_span.str(), "x: 32");
    assert_eq!(error.duplicate_initialization_span.str(), "x: 128");
}

#[test]
fn struct_unknown_field_error() {
    const STRUCT_EXPRESSION: &str = "Vector2 { x: 32, y: 64, z: 128 }";

    let table = build_table(STRUCT_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let struct_id = table.get_by_qualified_name(["test", "Vector2"]).unwrap();

    let (_, errors) = binder.bind_as_rvalue_error(&parse::<
        syntax_tree::expression::unit::Struct,
    >(STRUCT_EXPRESSION));

    assert_eq!(errors.len(), 1);
    let error = errors[0].as_any().downcast_ref::<FieldNotFound>().unwrap();

    assert_eq!(error.struct_id, struct_id);
    assert_eq!(error.identifier_span.str(), "z");
}

#[test]
fn struct_field_is_not_accessible_error() {
    const STRUCT_EXPRESSION: &str = "inner::Vector2 { x: 32, y: 64 }";

    let table = build_table(STRUCT_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let struct_id =
        table.get_by_qualified_name(["test", "inner", "Vector2"]).unwrap();
    let fields = table.query::<fields::Fields>(struct_id).unwrap();

    let y_field_id = fields.field_ids_by_name["y"];

    let (_, errors) = binder.bind_as_rvalue_error(&parse::<
        syntax_tree::expression::unit::Struct,
    >(STRUCT_EXPRESSION));

    assert_eq!(errors.len(), 1);
    let error =
        errors[0].as_any().downcast_ref::<FieldIsNotAccessible>().unwrap();

    assert_eq!(error.struct_id, struct_id);
    assert_eq!(error.field_id, y_field_id);
}
