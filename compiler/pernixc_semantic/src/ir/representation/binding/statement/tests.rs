use crate::ir::{
    instruction::{AllocaAllocation, Instruction},
    representation::binding::tests::{parse_statement, TestTemplate},
};

#[test]
fn variable_declaration_with_type_annotation() {
    let template = TestTemplate::new();
    let (mut binder, storage) = template.create_binder();

    const SOURCE: &str = "let mutable x: int32 = 32;";
    let statement =
        parse_statement(SOURCE).into_variable_declaration().unwrap();

    let alloca_id =
        binder.bind_variable_declaration(&statement, &storage).unwrap();

    let found_alloca_id = binder
        .current_block()
        .instructions()
        .iter()
        .find_map(|x| {
            let Instruction::AllocaAllocation(AllocaAllocation { id }) = x
            else {
                return None;
            };
            Some(*id)
        })
        .unwrap();

    let named = binder.stack.search("x").unwrap();

    assert_eq!(found_alloca_id, alloca_id);
    assert_eq!(named.name, "x");
    assert_eq!(named.load_address.clone().into_alloca().unwrap(), alloca_id);
    assert!(named.mutable);
}
