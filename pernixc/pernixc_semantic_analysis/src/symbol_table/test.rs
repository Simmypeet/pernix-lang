use pernixc_common::source_file::SourceFile;
use pernixc_lexical_analysis::token_stream::TokenStream;
use pernixc_syntactic_analysis::abstract_syntax_tree::{declaration::AccessModifier, FileAST};

use crate::{
    error::SemanticError,
    symbol_table::{PrimitiveTypeUnit, Symbol, TypeAnnotationSymbol, TypeUnitSymbol},
};

use super::builder::SymbolTableBuilder;

static SOURCE_FILE: &str = "
namespace Outer::Inner 
{
    namespace More {}

    class TestClass 
    {
        public void Main()  {}

        public void Main(int32 a) {}

        public void Test() {}

        public void Test() {}

        public int32 myField;
    }

    class More {}

    class A 
    {
        public B field;
    }

    class B 
    {
        public C field;
    }

    class C 
    {
        public A field;
    }
}

";

#[test]
fn symbol_table_builder_test() {
    let source_file = SourceFile::new(SOURCE_FILE.to_string(), "test.pernix".to_string());
    let token_stream = TokenStream::tokenize(&source_file);
    let file_ast = FileAST::parse(&token_stream.0);
    let mut symbol_table_builder = SymbolTableBuilder::new();
    symbol_table_builder.add_from_file_ast(&file_ast.0);

    let (symbol_table, errors) = symbol_table_builder.build();

    assert_eq!(errors.len(), 3);
    {
        let func_redeclaration = |error: &SemanticError| match error {
            SemanticError::FunctionRedeclaration { redeclaration_name } => {
                redeclaration_name.value == "Test"
            }
            _ => false,
        };

        let name_conflict = |error: &SemanticError| match error {
            SemanticError::DeclarationNameConflictWithNamespace { name_conflict } => {
                name_conflict.value == "More"
            }
            _ => false,
        };

        let recursive_type = |error: &SemanticError| match error {
            SemanticError::RecursiveType {
                recursive_class_symbol_ids,
            } => recursive_class_symbol_ids.len() == 3,
            _ => false,
        };

        let mut func_redeclaration_found = false;
        let mut name_conflict_found = false;
        let mut recursive_type_found = false;

        for error in &errors {
            if func_redeclaration(error) {
                func_redeclaration_found = true;
            } else if name_conflict(error) {
                name_conflict_found = true;
            } else if recursive_type(error) {
                recursive_type_found = true;
            }
        }

        assert!(func_redeclaration_found);
        assert!(name_conflict_found);
        assert!(recursive_type_found);
    }

    assert!(matches!(
        symbol_table.get_by_full_qualified_name("Outer").unwrap(),
        Symbol::Namespace
    ));

    assert!(matches!(
        symbol_table
            .get_by_full_qualified_name("Outer::Inner")
            .unwrap(),
        Symbol::Namespace
    ));

    assert!(matches!(
        symbol_table
            .get_by_full_qualified_name("Outer::Inner::More")
            .unwrap(),
        Symbol::Namespace
    ));

    {
        let symbol = symbol_table
            .get_by_full_qualified_name("Outer::Inner::TestClass")
            .unwrap();

        match symbol {
            Symbol::ClassSymbol(class_symbol) => {
                assert_eq!(class_symbol.name, "TestClass");
                assert_eq!(class_symbol.fields.len(), 1);
                assert_eq!(class_symbol.fields.get("myField").unwrap().name, "myField");
                assert!(matches!(
                    class_symbol.fields.get("myField").unwrap().access_modifier,
                    AccessModifier::Public
                ));
                assert!(matches!(
                    class_symbol
                        .fields
                        .get("myField")
                        .unwrap()
                        .type_annotation_symbol,
                    TypeAnnotationSymbol::TypeUnitSymbol(TypeUnitSymbol::PrimitiveTypeUnit(
                        PrimitiveTypeUnit::Int32
                    ))
                ));
            }
            _ => panic!("Expected class symbol"),
        }
    }

    {
        let symbol = symbol_table
            .get_by_full_qualified_name("Outer::Inner::TestClass::Main")
            .unwrap();

        match symbol {
            Symbol::OverloadSetSymbol(overload_set) => {
                assert_eq!(overload_set.name, "Main");
                assert_eq!(overload_set.functions.len(), 2);
            }
            _ => panic!("Expected function symbol"),
        }
    }

    {
        let symbol = symbol_table
            .get_by_full_qualified_name("Outer::Inner::TestClass::Test")
            .unwrap();

        match symbol {
            Symbol::OverloadSetSymbol(overload_set) => {
                assert_eq!(overload_set.name, "Test");
                assert_eq!(overload_set.functions.len(), 1);
            }
            _ => panic!("Expected function symbol"),
        }
    }

    {
        let symbol = symbol_table
            .get_by_full_qualified_name("Outer::Inner::A")
            .unwrap();

        match symbol {
            Symbol::ClassSymbol(class_symbol) => {
                assert_eq!(class_symbol.name, "A");
                assert_eq!(class_symbol.fields.len(), 1);
                assert_eq!(class_symbol.fields.get("field").unwrap().name, "field");
                assert!(matches!(
                    class_symbol.fields.get("field").unwrap().access_modifier,
                    AccessModifier::Public
                ));
                {
                    match &class_symbol
                        .fields
                        .get("field")
                        .unwrap()
                        .type_annotation_symbol
                    {
                        TypeAnnotationSymbol::TypeUnitSymbol(
                            TypeUnitSymbol::UserDefinedTypeUnit(symbol_id),
                        ) => {
                            let symbol = symbol_table.get_by_id(*symbol_id).unwrap();
                            match symbol {
                                Symbol::ClassSymbol(class_symbol) => {
                                    assert_eq!(class_symbol.name, "B");
                                }
                                _ => panic!("Expected class symbol"),
                            }
                        }
                        _ => panic!("Expected class type unit"),
                    }
                }
            }
            _ => panic!("Expected class symbol"),
        }
    }

    {
        let symbol = symbol_table
            .get_by_full_qualified_name("Outer::Inner::B")
            .unwrap();

        match symbol {
            Symbol::ClassSymbol(class_symbol) => {
                assert_eq!(class_symbol.name, "B");
                assert_eq!(class_symbol.fields.len(), 1);
                assert_eq!(class_symbol.fields.get("field").unwrap().name, "field");
                assert!(matches!(
                    class_symbol.fields.get("field").unwrap().access_modifier,
                    AccessModifier::Public
                ));
                {
                    match &class_symbol
                        .fields
                        .get("field")
                        .unwrap()
                        .type_annotation_symbol
                    {
                        TypeAnnotationSymbol::TypeUnitSymbol(
                            TypeUnitSymbol::UserDefinedTypeUnit(symbol_id),
                        ) => {
                            let symbol = symbol_table.get_by_id(*symbol_id).unwrap();
                            match symbol {
                                Symbol::ClassSymbol(class_symbol) => {
                                    assert_eq!(class_symbol.name, "C");
                                }
                                _ => panic!("Expected class symbol"),
                            }
                        }
                        _ => panic!("Expected class type unit"),
                    }
                }
            }
            _ => panic!("Expected class symbol"),
        }
    }

    {
        let symbol = symbol_table
            .get_by_full_qualified_name("Outer::Inner::C")
            .unwrap();

        match symbol {
            Symbol::ClassSymbol(class_symbol) => {
                assert_eq!(class_symbol.name, "C");
                assert_eq!(class_symbol.fields.len(), 1);
                assert_eq!(class_symbol.fields.get("field").unwrap().name, "field");
                assert!(matches!(
                    class_symbol.fields.get("field").unwrap().access_modifier,
                    AccessModifier::Public
                ));
                {
                    match &class_symbol
                        .fields
                        .get("field")
                        .unwrap()
                        .type_annotation_symbol
                    {
                        TypeAnnotationSymbol::TypeUnitSymbol(
                            TypeUnitSymbol::UserDefinedTypeUnit(symbol_id),
                        ) => {
                            let symbol = symbol_table.get_by_id(*symbol_id).unwrap();
                            match symbol {
                                Symbol::ClassSymbol(class_symbol) => {
                                    assert_eq!(class_symbol.name, "A");
                                }
                                _ => panic!("Expected class symbol"),
                            }
                        }
                        _ => panic!("Expected class type unit"),
                    }
                }
            }
            _ => panic!("Expected class symbol"),
        }
    }
}
