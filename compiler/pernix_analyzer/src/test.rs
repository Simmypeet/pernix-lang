use pernix_parser::Parser;
use pernix_project::source_code::SourceCode;

use crate::symbol::table::{FunctionSymbolTable, TypeSymbolTable};

#[test]
fn binder_function_populate_test() {
    let source_code = "
    int32 Minus() {}
    namespace Test {
        int32 Add() {}
        int32 Multiply() {}
    }
    int32 Divide() {}
    ";
    let source_code =
        SourceCode::new(source_code.to_string(), "test.pernix".to_string());
    let mut parser = Parser::new(&source_code);
    let ast = parser.parse_file();

    let type_table = TypeSymbolTable::new();
    let mut func_table = FunctionSymbolTable::new();
    assert!(func_table.populate(&ast, &type_table).is_ok());

    assert!(func_table.get("Minus").is_some());
    assert!(func_table.get("Test.Add").is_some());
    assert!(func_table.get("Test.Multiply").is_some());
    assert!(func_table.get("Divide").is_some());
}
