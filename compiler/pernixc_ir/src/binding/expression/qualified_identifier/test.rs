use pernixc_handler::Panic;
use pernixc_syntax::{syntax_tree, utility::parse};
use pernixc_semantic::term::r#type::Qualifier;

use crate::binding::{
    diagnostic::ExpectedAssociatedValue,
    test::{build_table, BindExt, CreateBinderAtExt, Template},
};

#[test]
fn named_load() {
    const VARIABLE_DECLARATION: &str = "let mut x: int32 = 32";
    const VARIABLE_LOAD: &str = "x";

    let test_template = Template::new();
    let mut binder = test_template.create_binder();

    let (variable_address, _) = binder
        .bind_variable_declaration(
            &parse::<syntax_tree::statement::VariableDeclaration>(
                VARIABLE_DECLARATION,
            ),
            &Panic,
        )
        .unwrap();

    let qualified_identifier =
        parse::<syntax_tree::QualifiedIdentifier>(VARIABLE_LOAD);
    let register_id = binder
        .bind_as_rvalue_success(&qualified_identifier)
        .into_register()
        .unwrap();

    assert_eq!(
        binder
            .intermediate_representation
            .values
            .registers
            .get(register_id)
            .unwrap()
            .assignment
            .as_load()
            .unwrap()
            .address,
        variable_address
    );

    let lvalue = binder.bind_as_lvalue_success(&qualified_identifier);

    assert_eq!(lvalue.address, variable_address);
    assert_eq!(lvalue.qualifier, Qualifier::Mutable);
}

const ENUM_DECLARATION: &str = r"
public enum Sample:
    First(int32)
    Second


public function test():
    panic
";

#[test]
fn qualified_identifier_variant() {
    const SECOND_EXPRESSION: &str = "Sample::Second";

    let table = build_table(ENUM_DECLARATION);
    let second_variant_id =
        table.get_by_qualified_name(["test", "Sample", "Second"]).unwrap();

    let mut binder = table.create_binder_at(["test", "test"]);

    let second_register = binder
        .bind_as_rvalue_success(&parse::<syntax_tree::QualifiedIdentifier>(
            SECOND_EXPRESSION,
        ))
        .into_register()
        .unwrap();

    let second_assignment = binder.intermediate_representation.values.registers
        [second_register]
        .assignment
        .as_variant()
        .unwrap();

    assert_eq!(second_assignment.variant_id, second_variant_id);
    assert!(second_assignment.associated_value.is_none());
}

#[test]
fn qualified_identifier_variant_expected_associated_value_error() {
    const FIRST_EXPRESSION: &str = "Sample::First";

    let table = build_table(ENUM_DECLARATION);
    let first_variant_id =
        table.get_by_qualified_name(["test", "Sample", "First"]).unwrap();

    let mut binder = table.create_binder_at(["test", "test"]);

    let (value, errors) = binder.bind_as_rvalue_error(&parse::<
        syntax_tree::QualifiedIdentifier,
    >(FIRST_EXPRESSION));

    assert_eq!(errors.len(), 1);

    let error =
        errors[0].as_any().downcast_ref::<ExpectedAssociatedValue>().unwrap();

    assert_eq!(error.variant_id, first_variant_id);

    let first_register = value.into_register().unwrap();
    let first_assignment = binder.intermediate_representation.values.registers
        [first_register]
        .assignment
        .as_variant()
        .unwrap();

    assert_eq!(first_assignment.variant_id, first_variant_id);
    assert!(first_assignment.associated_value.is_some());
}
