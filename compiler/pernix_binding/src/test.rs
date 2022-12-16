use pernix_parser::Parser;
use pernix_project::source_code::SourceCode;

use crate::FunctionDeclarationTableBuilder;

#[test]
fn binder_function_populate_test() {
    let source_code = "
    function Minus() : int32 {}
    namespace Test {
        function Add() : int32 {}
        function Multiply() : int32 {}
    }
    function Divide() : int32 {}
    ";
    let source_code =
        SourceCode::new(source_code.to_string(), "test.pernix".to_string());
    let mut parser = Parser::new(&source_code);
    let ast = parser.parse_file().unwrap();

    let mut binder = FunctionDeclarationTableBuilder::new(&ast, &source_code);
    binder.populate_function(&ast.using_directives(), &ast.declarations(), "");

    assert!(binder.function_table.get("Minus").is_some());
    assert!(binder.function_table.get("Test.Add").is_some());
    assert!(binder.function_table.get("Test.Multiply").is_some());
    assert!(binder.function_table.get("Divide").is_some());
}
