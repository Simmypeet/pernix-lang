use std::{fmt::Display, sync::Arc};

use pernixc_base::{
    diagnostic::{Counter, Storage},
    source_file::SourceFile,
};
use pernixc_lexical::token_stream::TokenStream;
use pernixc_syntax::{parser::Parser, syntax_tree};

use crate::{
    arena::ID,
    error::{
        Error, FloatingPointLiteralHasIntegralSuffix, InvalidNumericSuffix,
    },
    ir::representation::building::{
        binder::{
            expression::{Config, Target},
            Binder,
        },
        infer::TypeConstraint,
    },
    semantic::term::r#type::{Primitive, Type},
    symbol::{
        table::{
            representation::{
                building::finalizing::Finalizer, Insertion, RwLockContainer,
            },
            resolution::NoOpObserver,
            Building, Table,
        },
        Accessibility, Function, FunctionDefinition, FunctionTemplate,
        GenericDeclaration,
    },
};

fn create_expression_syntax(
    source: impl Display,
) -> syntax_tree::expression::Expression {
    let counter = Counter::default();

    let source_file = Arc::new(SourceFile::temp(source).unwrap());
    let token_stream = TokenStream::tokenize(&source_file, &counter);

    // no error
    assert_eq!(counter.count(), 0);

    let mut parser = Parser::new(&token_stream);
    let expression = parser.parse_expression(&counter).unwrap();

    // no error
    assert_eq!(counter.count(), 0);

    expression
}

fn create_dummy_function(
) -> (Table<Building<RwLockContainer, Finalizer>>, ID<Function>) {
    let mut table = Table::default();

    let Insertion { id: test_module_id, duplication } =
        table.create_root_module("test".to_string());

    assert!(duplication.is_none());

    let Insertion { id: function_id, duplication } = table
        .insert_member(
            "test".to_string(),
            Accessibility::Public,
            test_module_id,
            None,
            GenericDeclaration::default(),
            FunctionTemplate::<FunctionDefinition>::default(),
        )
        .unwrap();

    assert!(duplication.is_none());

    (table, function_id)
}

#[test]
fn bind_numeric_literal_suffix() {
    const SOURCE: &str = r"432u64";

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();
    let expression = create_expression_syntax(SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_numeric()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        NoOpObserver,
        function_id,
        std::iter::empty(),
        false,
        &storage,
    )
    .unwrap();

    let numeric_value = binder
        .bind_numeric(&expression, Config { target: Target::Value })
        .unwrap()
        .into_value()
        .unwrap()
        .into_literal()
        .unwrap()
        .into_numeric()
        .unwrap();

    assert!(numeric_value.decimal.is_none());
    assert_eq!(numeric_value.numeric, "432");
    assert_eq!(numeric_value.r#type, Type::Primitive(Primitive::Uint64));
}

#[test]
fn bind_numeric_literal_float_infer() {
    const SOURCE: &str = r"32.0";

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();
    let expression = create_expression_syntax(SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_numeric()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        NoOpObserver,
        function_id,
        std::iter::empty(),
        false,
        &storage,
    )
    .unwrap();

    let numeric_value = binder
        .bind_numeric(&expression, Config { target: Target::Value })
        .unwrap()
        .into_value()
        .unwrap()
        .into_literal()
        .unwrap()
        .into_numeric()
        .unwrap();

    assert_eq!(numeric_value.numeric, "32");
    assert_eq!(numeric_value.decimal.as_ref().map(AsRef::as_ref), Some("0"));

    let inference_variable = numeric_value.r#type.into_inference().unwrap();
    let constraint_id = binder
        .inference_context
        .get_inference(inference_variable)
        .cloned()
        .unwrap()
        .into_inferring()
        .unwrap();

    assert_eq!(
        binder.inference_context.get_constraint::<Type<_>>(constraint_id),
        Some(&TypeConstraint::Floating)
    );
}

#[test]
fn bind_numeric_literal_number_infer() {
    const SOURCE: &str = r"32";

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();
    let expression = create_expression_syntax(SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_numeric()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        NoOpObserver,
        function_id,
        std::iter::empty(),
        false,
        &storage,
    )
    .unwrap();

    let numeric_value = binder
        .bind_numeric(&expression, Config { target: Target::Value })
        .unwrap()
        .into_value()
        .unwrap()
        .into_literal()
        .unwrap()
        .into_numeric()
        .unwrap();

    assert_eq!(numeric_value.numeric, "32");
    assert!(numeric_value.decimal.is_none());

    let inference_variable = numeric_value.r#type.into_inference().unwrap();
    let constraint_id = binder
        .inference_context
        .get_inference(inference_variable)
        .cloned()
        .unwrap()
        .into_inferring()
        .unwrap();

    assert_eq!(
        binder.inference_context.get_constraint::<Type<_>>(constraint_id),
        Some(&TypeConstraint::Number)
    );
}

#[test]
fn invalid_numeric_literal_suffix() {
    const SOURCE: &str = r"32goofy";

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();
    let expression = create_expression_syntax(SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_numeric()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        NoOpObserver,
        function_id,
        std::iter::empty(),
        false,
        &storage,
    )
    .unwrap();

    assert!(binder
        .bind_numeric(&expression, Config { target: Target::Value })
        .is_err());

    let mut storage = storage.into_vec();
    assert_eq!(storage.len(), 1);
    let error = storage.pop().unwrap();

    let error = error.as_any().downcast_ref::<InvalidNumericSuffix>().unwrap();
    assert_eq!(error.suffix_span.str(), "goofy");
}

#[test]
fn floating_point_literal_has_integral_suffix() {
    const SOURCE: &str = r"32.0i64";

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();
    let expression = create_expression_syntax(SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_numeric()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        NoOpObserver,
        function_id,
        std::iter::empty(),
        false,
        &storage,
    )
    .unwrap();

    assert!(binder
        .bind_numeric(&expression, Config { target: Target::Value })
        .is_err());

    let mut storage = storage.into_vec();
    assert_eq!(storage.len(), 1);
    let error = storage.pop().unwrap();

    assert!(error
        .as_any()
        .downcast_ref::<FloatingPointLiteralHasIntegralSuffix>()
        .is_some());
}

#[test]
fn bind_boolean_literal() {
    const TRUE_SOURCE: &str = "true";
    const FALSE_SOURCE: &str = "false";

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();

    let true_expression = create_expression_syntax(TRUE_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_boolean()
        .unwrap();
    let false_expression = create_expression_syntax(FALSE_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_boolean()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        NoOpObserver,
        function_id,
        std::iter::empty(),
        false,
        &storage,
    )
    .unwrap();

    let true_literal = binder
        .bind_boolean(&true_expression, Config { target: Target::Value })
        .unwrap();
    let false_literal = binder
        .bind_boolean(&false_expression, Config { target: Target::Value })
        .unwrap();

    assert!(
        true_literal
            .into_value()
            .unwrap()
            .into_literal()
            .unwrap()
            .into_boolean()
            .unwrap()
            .value
    );

    assert!(
        !false_literal
            .into_value()
            .unwrap()
            .into_literal()
            .unwrap()
            .into_boolean()
            .unwrap()
            .value
    );
}
