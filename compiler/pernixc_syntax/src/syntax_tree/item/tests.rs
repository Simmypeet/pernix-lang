use std::{error::Error, path::PathBuf};

use pernixc_common::source_file::{SourceElement, SourceFile};
use pernixc_lexical::token_stream::TokenStream;
use pernixc_system::error_handler::DummyErrorHandler;

use crate::{
    parser::Parser,
    syntax_tree::{item::AccessModifier, PrimitiveTypeSpecifier, TypeSpecifier},
};

const STRUCT_SOURCE_CODE: &str = "
public struct Test {
    public let test: int32;
    private let test2: some::other::ty;
}
";

#[test]
fn struct_item_test() -> Result<(), Box<dyn Error>> {
    let source_file = SourceFile::new(
        PathBuf::default(),
        "test".to_string(),
        STRUCT_SOURCE_CODE.to_string(),
        vec!["test".to_string()],
    )?;
    let token_stream = TokenStream::tokenize(source_file.iter(), &DummyErrorHandler);
    let mut cursor = token_stream.cursor();
    cursor.next_token();
    let mut parser = Parser::new(cursor)?;

    let struct_item = parser
        .parse_item(&DummyErrorHandler)
        .unwrap()
        .into_struct()
        .unwrap();

    assert!(matches!(
        struct_item.signature.access_modifier,
        AccessModifier::Public(..)
    ));

    assert_eq!(struct_item.signature.identifier.span().str(), "Test");

    let mut member_iter = struct_item.body.members.into_iter();
    {
        {
            let field = member_iter.next().unwrap().into_field().unwrap();
            assert_eq!(field.access_modifier.span().str(), "public");
            assert_eq!(field.identifier.span().str(), "test");
            assert!(matches!(
                field.type_annotation.type_specifier,
                TypeSpecifier::PrimitiveTypeSpecifier(PrimitiveTypeSpecifier::Int32(..))
            ));
        }
        {
            let field = member_iter.next().unwrap().into_field().unwrap();
            assert_eq!(field.access_modifier.span().str(), "private");
            assert_eq!(field.identifier.span().str(), "test2");
            assert_eq!(
                field
                    .type_annotation
                    .type_specifier
                    .into_qualified_identifier()
                    .unwrap()
                    .span()
                    .str(),
                "some::other::ty"
            );
        }
        assert!(member_iter.next().is_none());
    }

    Ok(())
}
