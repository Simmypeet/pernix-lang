use pernix_parser::Parser;
use pernix_project::source_code::SourceCode;

use super::{FunctionSymbolTable, TypeSymbolTable};

#[test]
fn type_declaration_table() {
    let source_code = "
    function Equal() : bool {}
    namespace Arithmetic 
    {
        function Add() : int32 {}
        function Multiply() : int32 {}
        function Divide() : int32 {}
        function Subtract() : int32{}
    }
    function NotEqual() : bool {}
    ";
    let source_code = SourceCode::new(source_code.to_string(), String::new());
    let mut parser = Parser::new(&source_code);
    let ast = parser.parse_file().unwrap();

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
