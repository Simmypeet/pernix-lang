use pernixc_handler::Panic;
use pernixc_syntax::utility::parse;

use crate::test::Template;

#[test]
fn variable_declaration_with_type_annotation() {
    const SOURCE: &str = "let mut x: int32 = 32";

    let template = Template::new();
    let mut binder = template.create_binder();

    let (new_alloca_id, _) =
        binder.bind_variable_declaration(&parse(SOURCE), &Panic).unwrap();

    let named = binder.stack.search("x").unwrap();

    assert_eq!(new_alloca_id, named.load_address);

    assert!(named.mutable);
}
