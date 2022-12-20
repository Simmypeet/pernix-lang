use pernix_parser::Parser;
use pernix_project::source_code::SourceCode;

use super::{FunctionSymbolTable, TypeSymbolTable};

#[test]
fn type_declaration_table() {
    let source_code = "
    int32 Equal() {}
    namespace Arithmetic 
    {
        int32 Add() {}
        int32 Multiply() {}
        int32 Divide() {}
        int32 Subtract() {}
    }
    int32 NotEqual() {}
    ";
    let source_code = SourceCode::new(source_code.to_string(), String::new());
    let mut parser = Parser::new(&source_code);
    let ast = parser.parse_file();

    let type_table = TypeSymbolTable::new();
    let mut function_table = FunctionSymbolTable::new();
    match function_table.populate(&ast, &type_table) {
        Ok(_) => {
            assert!(function_table.get("Equal").is_some());
            assert!(function_table.get("Arithmetic.Add").is_some());
            assert!(function_table.get("Arithmetic.Multiply").is_some());
            assert!(function_table.get("Arithmetic.Divide").is_some());
            assert!(function_table.get("Arithmetic.Subtract").is_some());
            assert!(function_table.get("NotEqual").is_some());
        }
        Err(_) => {}
    }
}
