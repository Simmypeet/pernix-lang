use std::{error::Error, path::PathBuf};

use pernixc_common::source_file::SourceFile;

use crate::symbol::{
    ty::{PrimitiveType, Type, TypeBinding},
    AccessModifier, Field, LocalVariable,
};

#[test]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
fn basic_symbol_table_test() -> Result<(), Box<dyn Error>> {
    let file_parsing = pernixc_syntax::file_parsing::parse_files(SourceFile::load(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
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
                symbol_table.index(id.into_module().unwrap()),
            )
        })
        .unwrap();

    assert_eq!(test_module_symbol.name, "test");
    assert_eq!(test_module_symbol.children_symbol_ids_by_name.len(), 3);

    // module sub
    let sub_module_symbol_id = test_module_symbol.children_symbol_ids_by_name["sub"]
        .into_module()
        .unwrap();
    let sub_module_symbol = symbol_table.index(sub_module_symbol_id);

    assert_eq!(sub_module_symbol.name, "sub");
    assert_eq!(sub_module_symbol.parent_id, Some(test_module_symbol_id));
    assert_eq!(sub_module_symbol.access_modifier, AccessModifier::Public);
    assert_eq!(sub_module_symbol.children_symbol_ids_by_name.len(), 1);

    // enum sub::SomeType
    let sub_some_type_enum_symbol_id = sub_module_symbol.children_symbol_ids_by_name["SomeType"]
        .into_enum()
        .unwrap();
    let sub_some_type_enum_symbol = symbol_table.index(sub_some_type_enum_symbol_id);

    assert_eq!(sub_some_type_enum_symbol.name, "SomeType");
    assert_eq!(sub_some_type_enum_symbol.parent_id, sub_module_symbol_id);
    assert_eq!(
        sub_some_type_enum_symbol.access_modifier,
        AccessModifier::Public
    );
    assert_eq!(
        sub_some_type_enum_symbol.variant_symbol_ids_by_name.len(),
        2
    );

    // struct SomeStruct {}
    let struct_symbol_id = test_module_symbol.children_symbol_ids_by_name["SomeType"]
        .into_struct()
        .unwrap();
    let struct_symbol = symbol_table.index(struct_symbol_id);

    assert_eq!(struct_symbol.name, "SomeType");
    assert_eq!(struct_symbol.parent_id, test_module_symbol_id);
    assert_eq!(struct_symbol.access_modifier, AccessModifier::Public);

    assert_eq!(struct_symbol.fields.len(), 2);
    assert_eq!(struct_symbol.fields[0.into()], Field {
        name: "someNumber".to_string(),
        ty: Type::Primitive(PrimitiveType::Int32),
        access_modifier: AccessModifier::Public,
    });
    assert_eq!(struct_symbol.fields[1.into()], Field {
        name: "someType".to_string(),   
        ty: Type::TypedID(sub_some_type_enum_symbol_id.into()),
        access_modifier: AccessModifier::Public,
    });

    // function Test
    let test_function_symbol_id = test_module_symbol.children_symbol_ids_by_name["Test"]
        .into_function()
        .unwrap();
    let test_function_symbol = symbol_table.index(test_function_symbol_id);

    assert_eq!(test_function_symbol.name, "Test");
    assert_eq!(test_function_symbol.parent_id, test_module_symbol_id);
    assert_eq!(test_function_symbol.access_modifier, AccessModifier::Public);
    assert_eq!(
        test_function_symbol.return_type,
        Type::Primitive(PrimitiveType::Void)
    );
    assert_eq!(test_function_symbol.parameters.len(), 4);
    assert_eq!(test_function_symbol.parameters[0.into()], LocalVariable {
        name: "a".to_string(),
        type_binding: TypeBinding {
            is_mutable: false,
            ty: Type::TypedID(struct_symbol_id.into()),
        },
    });
    assert_eq!(test_function_symbol.parameters[1.into()], LocalVariable {
        name: "b".to_string(),
        type_binding: TypeBinding {
            is_mutable: false,
            ty: Type::TypedID(struct_symbol_id.into()),
        },
    });
    assert_eq!(test_function_symbol.parameters[2.into()], LocalVariable {
        name: "c".to_string(),
        type_binding: TypeBinding {
            is_mutable: false,
            ty: Type::TypedID(sub_some_type_enum_symbol_id.into()),
        },
    });
    assert_eq!(test_function_symbol.parameters[3.into()], LocalVariable {
        name: "d".to_string(),
        type_binding: TypeBinding {
            is_mutable: false,
            ty: Type::TypedID(sub_some_type_enum_symbol_id.into())
        },
    });

    Ok(())
}
