use std::{error::Error, path::PathBuf};

use pernixc_common::source_file::{SourceElement, SourceFile};
use pernixc_lexical::token_stream::TokenStream;

use crate::{
    parser::Parser,
    syntax_tree::{item::AccessModifier, PrimitiveTypeSpecifier, TypeSpecifier},
};

const STRUCT_SOURCE_CODE: &str = "
public struct Test 
{
public:
private:
    let test: int32;
    let test2: some::other::ty;
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
    let (token_stream, _) = TokenStream::tokenize(source_file.iter());
    let mut cursor = token_stream.cursor();
    cursor.next_token();
    let mut parser = Parser::new(cursor)?;

    let struct_item = parser.parse_item().unwrap().into_struct().unwrap();

    assert!(matches!(
        struct_item.access_modifier,
        AccessModifier::Public(..)
    ));

    assert_eq!(struct_item.identifier.span().str(), "Test");

    let mut field_group_iter = struct_item.member_groups.into_iter();

    {
        let field_group = field_group_iter.next().unwrap();
        assert!(matches!(
            field_group.access_modifier,
            AccessModifier::Public(..)
        ));
        let mut field_iter = field_group.members.iter();
        assert!(field_iter.next().is_none());
    }

    {
        let field_group = field_group_iter.next().unwrap();
        assert!(matches!(
            field_group.access_modifier,
            AccessModifier::Private(..)
        ));
        let mut field_iter = field_group.members.into_iter();
        {
            let field = field_iter.next().unwrap().into_field().unwrap();
            assert_eq!(field.identifier.span().str(), "test");
            assert!(matches!(
                field.type_annotation.type_specifier,
                TypeSpecifier::PrimitiveTypeSpecifier(PrimitiveTypeSpecifier::Int32(..))
            ));
        }
        {
            let field = field_iter.next().unwrap().into_field().unwrap();
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
        assert!(field_iter.next().is_none());
    }

    assert!(field_group_iter.next().is_none());

    Ok(())
}
