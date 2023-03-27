use std::{error::Error, path::PathBuf};

use pernixc_common::source_file::SourceFile;

use crate::symbol::{
    ty::{PrimitiveType, Type, TypeBinding},
    AccessModifier, FieldSymbol, VariableSymbol,
};

#[test]
fn basic_symbol_table_test() -> Result<(), Box<dyn Error>> {
    let file_parsing = pernixc_syntax::file_parsing::parse_files(SourceFile::load(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("resource")
            .join("basicSymbolTable")
            .join("main.pnx"),
        vec!["test".to_string()],
    )?)?;

    let (symbol_table, errors) = super::GlobalSymbolTable::analyze(file_parsing.into_iter());
    assert!(errors.is_empty());

    let (test_module_symbol_index, test_module_symbol) = symbol_table
        .get_by_qualified_name(["test"].into_iter())
        .expect("test should be found");
    let test_module_symbol = test_module_symbol
        .as_module_symbol()
        .expect("test should be a module");

    assert_eq!(test_module_symbol.name, "test");
    assert_eq!(test_module_symbol.children_sybol_indices_by_name.len(), 3);

    // module sub
    let sub_module_symbol_index = *test_module_symbol
        .children_sybol_indices_by_name
        .get("sub")
        .expect("sub should be found");
    let sub_module_symbol = symbol_table
        .get_by_index(sub_module_symbol_index)
        .expect("sub should be found")
        .as_module_symbol()
        .expect("should be struct");

    assert_eq!(sub_module_symbol.name, "sub");
    assert_eq!(
        sub_module_symbol.parent_index,
        Some(test_module_symbol_index)
    );
    assert_eq!(sub_module_symbol.access_modifier, AccessModifier::Public);
    assert_eq!(sub_module_symbol.children_sybol_indices_by_name.len(), 1);

    // enum sub::SomeType
    let sub_some_type_enum_symbol_index = *sub_module_symbol
        .children_sybol_indices_by_name
        .get("SomeType")
        .expect("sub::SomeType should be found");
    let sub_some_type_enum_symbol = symbol_table
        .get_by_index(sub_some_type_enum_symbol_index)
        .expect("sub::SomeType should be found")
        .as_enum_symbol()
        .expect("should be enum");

    assert_eq!(sub_some_type_enum_symbol.name, "SomeType");
    assert_eq!(
        sub_some_type_enum_symbol.parent_index,
        sub_module_symbol_index
    );
    assert_eq!(
        sub_some_type_enum_symbol.access_modifier,
        AccessModifier::Public
    );
    assert_eq!(
        sub_some_type_enum_symbol
            .variant_symbol_indices_by_name
            .len(),
        2
    );

    // struct SomeStruct {}
    let struct_symbol_index = *test_module_symbol
        .children_sybol_indices_by_name
        .get("SomeType")
        .expect("SomeType should be found");
    let struct_symbol = symbol_table
        .get_by_index(struct_symbol_index)
        .expect("SomeType should be found")
        .as_struct_symbol()
        .expect("should be struct");

    assert_eq!(struct_symbol.name, "SomeType");
    assert_eq!(struct_symbol.parent_index, test_module_symbol_index);
    assert_eq!(struct_symbol.access_modifier, AccessModifier::Public);

    assert_eq!(struct_symbol.fields.len(), 2);
    assert_eq!(
        *struct_symbol.fields.get_by_index(0).expect("should exist"),
        FieldSymbol {
            name: "someNumber".to_string(),
            ty: Type::Primitive(PrimitiveType::Int32),
            access_modifier: AccessModifier::Public,
        }
    );
    assert_eq!(
        *struct_symbol.fields.get_by_index(1).expect("should exist"),
        FieldSymbol {
            name: "someType".to_string(),
            ty: Type::UserDefinedSymbolIndex(sub_some_type_enum_symbol_index),
            access_modifier: AccessModifier::Public,
        }
    );

    // function Test
    let test_function_symbol_index = *test_module_symbol
        .children_sybol_indices_by_name
        .get("Test")
        .expect("Test should be found");
    let test_function_symbol = symbol_table
        .get_by_index(test_function_symbol_index)
        .expect("Test should be found")
        .as_function_symbol()
        .expect("should be function");

    assert_eq!(test_function_symbol.name, "Test");
    assert_eq!(test_function_symbol.parent_index, test_module_symbol_index);
    assert_eq!(test_function_symbol.access_modifier, AccessModifier::Public);
    assert_eq!(
        test_function_symbol.return_type,
        Type::Primitive(PrimitiveType::Void)
    );
    assert_eq!(test_function_symbol.parameters.len(), 4);
    assert_eq!(
        *test_function_symbol
            .parameters
            .get_by_index(0)
            .expect("should exist"),
        VariableSymbol {
            name: "a".to_string(),
            type_binding_specifier: TypeBinding {
                is_mutable: false,
                ty: Type::UserDefinedSymbolIndex(struct_symbol_index),
            },
        }
    );
    assert_eq!(
        *test_function_symbol
            .parameters
            .get_by_index(1)
            .expect("should exist"),
        VariableSymbol {
            name: "b".to_string(),
            type_binding_specifier: TypeBinding {
                is_mutable: false,
                ty: Type::UserDefinedSymbolIndex(struct_symbol_index),
            },
        }
    );
    assert_eq!(
        *test_function_symbol
            .parameters
            .get_by_index(2)
            .expect("should exist"),
        VariableSymbol {
            name: "c".to_string(),
            type_binding_specifier: TypeBinding {
                is_mutable: false,
                ty: Type::UserDefinedSymbolIndex(sub_some_type_enum_symbol_index),
            },
        }
    );
    assert_eq!(
        *test_function_symbol
            .parameters
            .get_by_index(3)
            .expect("should exist"),
        VariableSymbol {
            name: "d".to_string(),
            type_binding_specifier: TypeBinding {
                is_mutable: false,
                ty: Type::UserDefinedSymbolIndex(sub_some_type_enum_symbol_index)
            },
        }
    );

    Ok(())
}
