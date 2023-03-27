use std::error::Error;

use pernixc_common::source_file::{Iterator, Span, SpanEnding};
use pernixc_lexical::token_stream::TokenStream;

use crate::{
    parser::Parser,
    syntax_tree::{item::AccessModifier, PrimitiveTypeSpecifier, SourceElement, TypeSpecifier},
};

const STRUCT_SOURCE_CODE: &str = "
public struct Test 
{
public:
private:
    int32 test;
    some::other::type test2;
}
";

fn substr_span(source_code: &str, span: Span) -> &str {
    match span.end {
        SpanEnding::Location(end_location) => &source_code[span.start.byte..end_location.byte],
        SpanEnding::EndOfFile => &source_code[span.start.byte..],
    }
}

#[test]
fn struct_item_test() -> Result<(), Box<dyn Error>> {
    let (token_stream, _) = TokenStream::tokenize(Iterator::new(STRUCT_SOURCE_CODE));
    let mut cursor = token_stream.cursor();
    cursor.next_token();
    let mut parser = Parser::new(cursor)?;

    let struct_item = parser.parse_item().unwrap().into_struct().unwrap();

    assert!(matches!(
        struct_item.access_modifier,
        AccessModifier::Public(..)
    ));

    assert_eq!(
        substr_span(STRUCT_SOURCE_CODE, struct_item.identifier.span),
        "Test"
    );

    let mut field_group_iter = struct_item.field_groups.into_iter();

    {
        let field_group = field_group_iter.next().unwrap();
        assert!(matches!(
            field_group.access_modifier,
            AccessModifier::Public(..)
        ));
        let mut field_iter = field_group.fields.iter();
        assert!(field_iter.next().is_none());
    }

    {
        let field_group = field_group_iter.next().unwrap();
        assert!(matches!(
            field_group.access_modifier,
            AccessModifier::Private(..)
        ));
        let mut field_iter = field_group.fields.into_iter();
        {
            let field = field_iter.next().unwrap();
            assert_eq!(
                substr_span(STRUCT_SOURCE_CODE, field.identifier.span),
                "test"
            );
            assert!(matches!(
                field.type_specifier,
                TypeSpecifier::PrimitiveTypeSpecifier(PrimitiveTypeSpecifier::Int32(..))
            ));
        }
        {
            let field = field_iter.next().unwrap();
            assert_eq!(
                substr_span(STRUCT_SOURCE_CODE, field.identifier.span),
                "test2"
            );
            assert_eq!(
                substr_span(
                    STRUCT_SOURCE_CODE,
                    field
                        .type_specifier
                        .into_qualified_identifier()
                        .unwrap()
                        .span()
                ),
                "some::other::type"
            );
        }
        assert!(field_iter.next().is_none());
    }

    assert!(field_group_iter.next().is_none());

    Ok(())
}
