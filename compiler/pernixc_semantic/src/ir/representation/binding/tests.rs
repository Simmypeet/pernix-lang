use std::{fmt::Display, sync::Arc};

use pernixc_base::{
    handler::{Counter, Storage},
    source_file::SourceFile,
};
use pernixc_lexical::token_stream::TokenStream;
use pernixc_syntax::{parser::Parser, syntax_tree};

use super::Binder;
use crate::{
    arena::ID,
    error,
    symbol::{
        table::{representation::Insertion, resolution, Building, Table},
        Accessibility, Function, FunctionDefinition, FunctionTemplate,
        GenericDeclaration, Module,
    },
    type_system::observer,
};

#[derive(Debug)]
pub struct TestTemplate {
    pub table: Table<Building>,
    pub function_id: ID<Function>,
    pub test_module_id: ID<Module>,
}

impl TestTemplate {
    pub fn new() -> Self {
        let mut table = Table::default();

        let Insertion { id: test_module_id, duplication } =
            table.create_root_module("test".to_string());

        assert!(duplication.is_none());

        let Insertion { id: function_id, duplication } = table
            .insert_member(
                "Test".to_string(),
                Accessibility::Public,
                test_module_id,
                None,
                GenericDeclaration::default(),
                FunctionTemplate::<FunctionDefinition>::default(),
            )
            .unwrap();

        assert!(duplication.is_none());

        Self { table, function_id, test_module_id }
    }

    pub fn create_binder<'a>(
        &'a self,
    ) -> (
        Binder<'a, Building, resolution::NoOp, observer::NoOp>,
        Storage<Box<dyn error::Error>>,
    ) {
        let storage: Storage<Box<dyn error::Error>> = Storage::new();

        let binder = Binder::new_function(
            &self.table,
            resolution::NoOp,
            observer::NoOp,
            self.function_id,
            std::iter::empty(),
            &storage,
        )
        .unwrap();

        assert!(storage.as_vec().is_empty(), "{storage:?}");

        (binder, storage)
    }
}

pub fn parse_expression(
    source: impl Display,
) -> syntax_tree::expression::Expression {
    let counter = Counter::default();

    let source_file =
        Arc::new(SourceFile::new(source.to_string(), "test".into()));
    let token_stream = TokenStream::tokenize(&source_file, &counter);

    // no error
    assert_eq!(counter.count(), 0);

    let mut parser = Parser::new(&token_stream, source_file);
    let expression = parser.parse_expression(&counter).unwrap();

    // no error
    assert_eq!(counter.count(), 0);

    expression
}

pub fn parse_statement(
    source: impl Display,
) -> syntax_tree::statement::Statement {
    let counter = Counter::default();

    let source_file =
        Arc::new(SourceFile::new(source.to_string(), "test".into()));
    let token_stream = TokenStream::tokenize(&source_file, &counter);

    // no error
    assert_eq!(counter.count(), 0);

    let mut parser = Parser::new(&token_stream, source_file);
    let statement = parser.parse_statement(&counter).unwrap();

    // no error
    assert_eq!(counter.count(), 0);

    statement
}
