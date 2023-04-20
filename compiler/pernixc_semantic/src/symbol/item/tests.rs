use std::{error::Error, path::PathBuf};

use pernixc_common::source_file::SourceFile;

use crate::symbol::{
    item::AccessModifier,
    ty::{PrimitiveType, Type},
};

#[test]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
fn basic_symbol_table_test() -> Result<(), Box<dyn Error>> {
    let file_parsing = pernixc_syntax::file_parsing::parse_files(SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
            .join("symbol")
            .join("basicSymbolTable")
            .join("main.pnx"),
        vec!["test".to_string()],
    )?)?;

    let (symbol_table, errors) = super::Table::analyze(file_parsing.into_iter());
    assert!(errors.is_empty());

    let (test_module_symbol_id, test_module_symbol) = symbol_table
        .get_id_by_full_name(std::iter::once("test"))
        .map(|id| {
            (
                id.into_module().unwrap(),
                &symbol_table[id.into_module().unwrap()],
            )
        })
        .unwrap();

    assert_eq!(test_module_symbol.name, "test");
    assert_eq!(test_module_symbol.children_ids_by_name.len(), 3);

    // module sub
    let sub_module_symbol_id = test_module_symbol.children_ids_by_name["sub"]
        .into_module()
        .unwrap();
    let sub_module_symbol = &symbol_table[sub_module_symbol_id];

    assert_eq!(sub_module_symbol.name, "sub");
    assert_eq!(sub_module_symbol.parent_id, Some(test_module_symbol_id));
    assert_eq!(sub_module_symbol.access_modifier, AccessModifier::Public);
    assert_eq!(sub_module_symbol.children_ids_by_name.len(), 1);

    // enum sub::SomeType
    let sub_some_type_enum_symbol_id = sub_module_symbol.children_ids_by_name["SomeType"]
        .into_enum()
        .unwrap();
    let sub_some_type_enum_symbol = &symbol_table[sub_some_type_enum_symbol_id];

    assert_eq!(sub_some_type_enum_symbol.name, "SomeType");
    assert_eq!(sub_some_type_enum_symbol.parent_id, sub_module_symbol_id);
    assert_eq!(
        sub_some_type_enum_symbol.access_modifier,
        AccessModifier::Public
    );
    assert_eq!(sub_some_type_enum_symbol.variant_ids_by_name.len(), 2);

    // struct SomeStruct {}
    let struct_symbol_id = test_module_symbol.children_ids_by_name["SomeType"]
        .into_struct()
        .unwrap();
    let struct_symbol = &symbol_table[struct_symbol_id];

    assert_eq!(struct_symbol.name, "SomeType");
    assert_eq!(struct_symbol.parent_id, test_module_symbol_id);
    assert_eq!(struct_symbol.access_modifier, AccessModifier::Public);

    assert_eq!(struct_symbol.field_ids_by_name.len(), 2);

    let field = &struct_symbol.fields_by_id[&struct_symbol.field_ids_by_name["someNumber"]];
    assert_eq!(field.access_modifier(), AccessModifier::Public);
    assert_eq!(field.name(), "someNumber");
    assert_eq!(field.ty(), Type::Primitive(PrimitiveType::Int32));

    let field = &struct_symbol.fields_by_id[&struct_symbol.field_ids_by_name["someType"]];
    assert_eq!(field.access_modifier(), AccessModifier::Public);
    assert_eq!(field.name(), "someType");
    assert_eq!(
        field.ty(),
        Type::TypedID(sub_some_type_enum_symbol_id.into())
    );

    // function Test
    let test_function_symbol_id = test_module_symbol.children_ids_by_name["Test"]
        .into_function()
        .unwrap();
    let test_function_symbol = &symbol_table[test_function_symbol_id];

    assert_eq!(test_function_symbol.name, "Test");
    assert_eq!(test_function_symbol.parent_id, test_module_symbol_id);
    assert_eq!(test_function_symbol.access_modifier, AccessModifier::Public);
    assert_eq!(
        test_function_symbol.return_type,
        Type::Primitive(PrimitiveType::Void)
    );
    assert_eq!(test_function_symbol.parameter_ids_by_name.len(), 4);

    let parameter =
        &test_function_symbol.parameters_by_id[&test_function_symbol.parameter_ids_by_name["a"]];
    assert_eq!(parameter.name(), "a");
    assert_eq!(
        parameter.type_binding().ty(),
        Type::TypedID(struct_symbol_id.into())
    );
    assert!(!parameter.type_binding().is_mutable());

    let parameter = &test_function_symbol
        .parameters_by_id
        .get(&test_function_symbol.parameter_ids_by_name["b"])
        .unwrap();
    assert_eq!(parameter.name(), "b");
    assert_eq!(
        parameter.type_binding().ty(),
        Type::TypedID(struct_symbol_id.into())
    );
    assert!(!parameter.type_binding().is_mutable());

    let parameter = &test_function_symbol
        .parameters_by_id
        .get(&test_function_symbol.parameter_ids_by_name["c"])
        .unwrap();
    assert_eq!(parameter.name(), "c");
    assert_eq!(
        parameter.type_binding().ty(),
        Type::TypedID(sub_some_type_enum_symbol_id.into())
    );
    assert!(!parameter.type_binding().is_mutable());

    let parameter = &test_function_symbol
        .parameters_by_id
        .get(&test_function_symbol.parameter_ids_by_name["d"])
        .unwrap();
    assert_eq!(parameter.name(), "d");
    assert_eq!(
        parameter.type_binding().ty(),
        Type::TypedID(sub_some_type_enum_symbol_id.into())
    );
    assert!(!parameter.type_binding().is_mutable());

    Ok(())
}
