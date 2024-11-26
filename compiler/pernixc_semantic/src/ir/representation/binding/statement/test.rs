use crate::ir::representation::binding::test::{
    parse_statement, TestTemplate,
};

#[test]
fn variable_declaration_with_type_annotation() {
    let template = TestTemplate::new();
    let (mut binder, storage) = template.create_binder();

    const SOURCE: &str = "let mutable x: int32 = 32;";
    let statement =
        parse_statement(SOURCE).into_variable_declaration().unwrap();

    let (new_alloca_id, _) =
        binder.bind_variable_declaration(&statement, &storage).unwrap();

    let named = binder.stack.search("x").unwrap();

    assert_eq!(new_alloca_id, named.load_address);

    assert!(named.mutable);
}
