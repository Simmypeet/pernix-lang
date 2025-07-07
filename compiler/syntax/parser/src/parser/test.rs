use std::{
    collections::HashSet,
    fmt::{Debug, Display},
};

use enum_as_inner::EnumAsInner;
use flexstr::SharedStr;
use pernixc_lexical::{
    token,
    tree::{DelimiterKind, RelativeLocation},
};
use pernixc_source_file::{GlobalSourceID, SourceFile, SourceMap};
use pernixc_target::TargetID;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, TestCaseError},
    prop_assert, prop_assert_eq, prop_oneof,
    strategy::Strategy,
    test_runner::TestCaseResult,
};

use crate::{
    abstract_tree::{abstract_tree, AbstractTree, First, Second, Tag},
    expect::{self, Expected},
    parser::{ast, Parser as _},
};

abstract_tree! {
    struct BasicSequence {
        public_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Public,

        struct_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Struct,

        identifier: token::Identifier<RelativeLocation>
            = expect::Identifier,

        semicolon: token::Punctuation<RelativeLocation>
            = ';'
    }
}

fn parse_token_tree(
    source_map: &mut SourceMap,
    source_code: &str,
) -> (pernixc_lexical::tree::Tree, GlobalSourceID) {
    let source_id = source_map.register(
        TargetID::Local,
        SourceFile::new(source_code.to_string(), "test".into()),
    );
    let source =
        source_map.get(TargetID::Local.make_global(source_id)).unwrap();

    let tree = pernixc_lexical::tree::Tree::from_source(
        source.content(),
        TargetID::Local.make_global(source_id),
        &pernixc_handler::Panic,
    );

    (tree, TargetID::Local.make_global(source_id))
}

fn check_basic_sequence(
    basic_sequence: &BasicSequence,
    expected_name: SharedStr,
) {
    assert_eq!(
        basic_sequence.public_keyword().map(|x| x.kind),
        Some(expect::Keyword::Public)
    );

    assert_eq!(
        basic_sequence.struct_keyword().map(|x| x.kind),
        Some(expect::Keyword::Struct)
    );

    assert_eq!(
        basic_sequence.identifier().map(|x| x.kind.0),
        Some(expected_name)
    );

    assert_eq!(basic_sequence.semicolon().map(|x| x.kind.0), Some(';'));
}

#[test]
fn basic_sequence() {
    let mut source_map = SourceMap::new();
    let (tree, _) = parse_token_tree(&mut source_map, "public struct Foo;");

    let (tree, errors) = BasicSequence::parse(&tree);
    let tree = tree.unwrap();

    check_basic_sequence(&tree, "Foo".into());

    assert!(errors.is_empty());
}

#[test]
fn basic_sequence_missing_semicolon() {
    let mut source_map = SourceMap::new();
    let (tree, _) = parse_token_tree(&mut source_map, "public struct Foo");

    let (tree, errors) = BasicSequence::parse(&tree);
    let tree = tree.unwrap();

    assert_eq!(errors.len(), 1);

    assert_eq!(
        tree.public_keyword().map(|x| x.kind),
        Some(expect::Keyword::Public)
    );

    assert_eq!(
        tree.struct_keyword().map(|x| x.kind),
        Some(expect::Keyword::Struct)
    );

    assert_eq!(tree.identifier().map(|x| x.kind.0), Some("Foo".into()));

    assert_eq!(tree.semicolon(), None);
}

abstract_tree! {
    #[derive(Debug)]
    struct TwoBasicSequences {
        first: Tag<BasicSequence, First> = ast::<Tag<BasicSequence, First>>(),
        second: Tag<BasicSequence, Second> = ast::<Tag<BasicSequence, Second>>(), }
}

#[test]
fn two_basic_sequences() {
    let mut source_map = SourceMap::new();
    let (tree, _) = parse_token_tree(
        &mut source_map,
        "public struct Foo; public struct Bar;",
    );

    let (tree, errors) = TwoBasicSequences::parse(&tree);
    let tree = tree.unwrap();

    assert!(errors.is_empty());

    check_basic_sequence(&tree.first().unwrap(), "Foo".into());
    check_basic_sequence(&tree.second().unwrap(), "Bar".into());
}

abstract_tree! {
    #{fragment = expect::Fragment::Indentation}
    struct Body {
        private_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Private,
        identifier: token::Identifier<RelativeLocation>
            = expect::Identifier,
        colon: token::Punctuation<RelativeLocation>
            = ':',
        int32_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Int32,
    }
}

abstract_tree! {
    struct SequenceWithFragment {
        public_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Public,
        struct_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Struct,
        identifier: token::Identifier<RelativeLocation>
            = expect::Identifier,
        body: Body = ast::<Body>(),
    }
}

const SEQUENCE_WITH_FRAGMENT: &str = "
public struct Foo: 
    private bar: int32
";

#[test]
fn sequence_with_fragment() {
    let mut source_map = SourceMap::new();
    let (tree, _) = parse_token_tree(&mut source_map, SEQUENCE_WITH_FRAGMENT);

    let (tree, errors) = SequenceWithFragment::parse(&tree);
    let tree = tree.unwrap();

    assert!(errors.is_empty());

    assert_eq!(
        tree.public_keyword().map(|x| x.kind),
        Some(expect::Keyword::Public)
    );

    assert_eq!(
        tree.struct_keyword().map(|x| x.kind),
        Some(expect::Keyword::Struct)
    );

    assert_eq!(tree.identifier().map(|x| x.kind.0), Some("Foo".into()));

    let body = tree.body().unwrap();

    assert_eq!(
        body.private_keyword().map(|x| x.kind),
        Some(expect::Keyword::Private)
    );

    assert_eq!(body.identifier().map(|x| x.kind.0), Some("bar".into()));

    assert_eq!(body.colon().map(|x| x.kind.0), Some(':'));

    assert_eq!(
        body.int32_keyword().map(|x| x.kind),
        Some(expect::Keyword::Int32)
    );
}

abstract_tree! {
    struct Int32Reference {
        ampersand: token::Punctuation<RelativeLocation>
            = '&',

        mut_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Mut.optional(),

        int32_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Int32
    }
}

#[test]
fn mutable_int32_reference() {
    let mut source_map = SourceMap::new();
    let (tree, _) = parse_token_tree(&mut source_map, "&mut int32");

    let (tree, errors) = Int32Reference::parse(&tree);
    let tree = tree.unwrap();

    assert!(errors.is_empty());

    assert!(tree.ampersand().is_some_and(|x| *x.kind == '&'));
    assert!(tree.mut_keyword().is_some_and(|x| x.kind == expect::Keyword::Mut));
    assert!(tree
        .int32_keyword()
        .is_some_and(|x| x.kind == expect::Keyword::Int32));
}

#[test]
fn int32_reference() {
    let mut source_map = SourceMap::new();
    let (tree, _) = parse_token_tree(&mut source_map, "&int32");

    let (tree, errors) = Int32Reference::parse(&tree);
    let tree = tree.unwrap();

    assert!(errors.is_empty());

    assert!(tree.ampersand().is_some_and(|x| *x.kind == '&'));
    assert!(tree.mut_keyword().is_none());
    assert!(tree
        .int32_keyword()
        .is_some_and(|x| x.kind == expect::Keyword::Int32));
}

#[test]
fn int32_reference_error() {
    let mut source_map = SourceMap::new();
    let (token_tree, _) = parse_token_tree(&mut source_map, "&bool");

    let (tree, errors) = Int32Reference::parse(&token_tree);
    let tree = tree.unwrap();

    assert_eq!(errors.len(), 1);

    let error = &errors[0];

    // found bool at the error
    assert!(token_tree[error.at.branch_id].nodes[error.at.node_index]
        .as_leaf()
        .and_then(|x| x.kind.as_keyword())
        .is_some_and(|x| *x == expect::Keyword::Bool));

    // expected either `int32` or `bool`
    assert_eq!(
        error.expecteds.iter().copied().collect::<HashSet<_>>(),
        [
            Expected::Keyword(expect::Keyword::Mut),
            Expected::Keyword(expect::Keyword::Int32)
        ]
        .into_iter()
        .collect()
    );

    assert!(tree.ampersand().is_some_and(|x| *x.kind == '&'));
    assert!(tree.mut_keyword().is_none());
    assert!(tree.int32_keyword().is_none());
}

#[test]
fn int32_reference_missing_int32() {
    let mut source_map = SourceMap::new();
    let (token_tree, _) = parse_token_tree(&mut source_map, "&mut");

    let (tree, errors) = Int32Reference::parse(&token_tree);
    let tree = tree.unwrap();

    assert_eq!(errors.len(), 1);

    let error = &errors[0];

    // found bool at the error
    assert_eq!(token_tree[error.at.branch_id].nodes.len(), error.at.node_index);

    // expected `int32`
    assert_eq!(
        error.expecteds.iter().copied().collect::<HashSet<_>>(),
        std::iter::once(Expected::Keyword(expect::Keyword::Int32)).collect()
    );

    assert!(tree.ampersand().is_some_and(|x| *x.kind == '&'));
    assert!(tree.mut_keyword().is_some_and(|x| x.kind == expect::Keyword::Mut));
    assert!(tree.int32_keyword().is_none());
}

abstract_tree! {
    #[derive(Debug, EnumAsInner)]
    enum Primitive {
        Int32(token::Keyword<RelativeLocation> = expect::Keyword::Int32),
        Uint32(token::Keyword<RelativeLocation> = expect::Keyword::Uint32),
        Bool(token::Keyword<RelativeLocation> = expect::Keyword::Bool),
        Float32(token::Keyword<RelativeLocation> = expect::Keyword::Float32),
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::Display,
)]
enum PrimitiveRef {
    #[display("int32")]
    Int32,
    #[display("uint32")]
    Uint32,
    #[display("bool")]
    Bool,
    #[display("float32")]
    Float32,
}

impl Arbitrary for PrimitiveRef {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Int32),
            Just(Self::Uint32),
            Just(Self::Bool),
            Just(Self::Float32),
        ]
        .boxed()
    }
}

impl Input<&Primitive, ()> for &PrimitiveRef {
    fn assert(
        self,
        output: &Primitive,
        (): (),
    ) -> proptest::test_runner::TestCaseResult {
        match (self, output) {
            (PrimitiveRef::Int32, Primitive::Int32(token)) => {
                prop_assert_eq!(token.kind, expect::Keyword::Int32);
                Ok(())
            }
            (PrimitiveRef::Uint32, Primitive::Uint32(token)) => {
                prop_assert_eq!(token.kind, expect::Keyword::Uint32);
                Ok(())
            }
            (PrimitiveRef::Bool, Primitive::Bool(token)) => {
                prop_assert_eq!(token.kind, expect::Keyword::Bool);
                Ok(())
            }
            (PrimitiveRef::Float32, Primitive::Float32(token)) => {
                prop_assert_eq!(token.kind, expect::Keyword::Float32);
                Ok(())
            }

            _ => Err(TestCaseError::fail(format!(
                "expected {self:?} found {output:?}"
            ))),
        }
    }
}

abstract_tree! {
    #[derive(Debug)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Bracket)}
    struct Array {
        r#type: Type = ast::<Type>(),
        x: token::Identifier<RelativeLocation> = expect::IdentifierValue::X,
        length: token::Numeric<RelativeLocation> = expect::Numeric,
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::Display,
)]
#[display("[{type} x {length}]")]
struct ArrayRef {
    pub r#type: Box<TypeRef>,
    pub length: usize,
}

impl Arbitrary for ArrayRef {
    type Parameters = Option<BoxedStrategy<TypeRef>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let ty = args.unwrap_or_else(TypeRef::arbitrary);

        (ty.prop_map(Box::new), proptest::num::usize::ANY)
            .prop_map(|(r#type, length)| Self { r#type, length })
            .boxed()
    }
}

impl Input<&Array, ()> for &ArrayRef {
    fn assert(
        self,
        output: &Array,
        (): (),
    ) -> proptest::test_runner::TestCaseResult {
        Some(&*self.r#type).assert(output.r#type().as_ref(), ())?;

        let result = output.length().and_then(|x| x.kind.parse::<usize>().ok());
        prop_assert_eq!(result, Some(self.length));

        Ok(())
    }
}

abstract_tree! {
    #[derive(Debug)]
    struct Reference {
        ampersand: token::Punctuation<RelativeLocation> = '&',
        mut_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Mut.optional(),
        r#type: Type
            = ast::<Type>()
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::Display,
)]
#[display("&{}{type}", if *mut_keyword { "mut " } else { "" })]
struct ReferenceRef {
    mut_keyword: bool,
    r#type: Box<TypeRef>,
}

impl Arbitrary for ReferenceRef {
    type Parameters = Option<BoxedStrategy<TypeRef>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let ty = args.unwrap_or_else(TypeRef::arbitrary);

        (ty.prop_map(Box::new), proptest::bool::ANY)
            .prop_map(|(r#type, mut_keyword)| Self { mut_keyword, r#type })
            .boxed()
    }
}

impl Input<&Reference, ()> for &ReferenceRef {
    fn assert(
        self,
        output: &Reference,
        (): (),
    ) -> proptest::test_runner::TestCaseResult {
        prop_assert_eq!(self.mut_keyword, output.mut_keyword().is_some());
        Some(&*self.r#type).assert(output.r#type().as_ref(), ())?;

        Ok(())
    }
}

abstract_tree! {
    #[derive(Debug)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Parenthesis)}
    struct Tuple {
        types: #[multi] Type = ast::<Type>().repeat_all_with_separator(',')
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::Display,
)]
#[display(
    "({}{})", 
    self.types
        .iter()
        .map(std::string::ToString::to_string)
        .collect::<Vec<_>>()
        .join(", "),
    if self.trailing_comma { "," } else { "" }
)]
struct TupleRef {
    types: Vec<TypeRef>,
    trailing_comma: bool,
}

impl Arbitrary for TupleRef {
    type Parameters = Option<BoxedStrategy<TypeRef>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(types: Self::Parameters) -> Self::Strategy {
        let types = types.unwrap_or_else(TypeRef::arbitrary);

        (proptest::collection::vec(types, 0..=10), proptest::bool::ANY)
            .prop_map(|(types, trailing_comma)| Self {
                trailing_comma: trailing_comma && !types.is_empty(),
                types,
            })
            .boxed()
    }
}

impl Input<&Tuple, ()> for &TupleRef {
    fn assert(
        self,
        output: &Tuple,
        (): (),
    ) -> proptest::test_runner::TestCaseResult {
        let types = output.types().collect::<Vec<_>>();

        self.types.assert(&types, ())?;

        Ok(())
    }
}

abstract_tree! {
    #[derive(Debug, EnumAsInner)]
    enum Type {
        Primitive(Primitive = ast::<Primitive>()),
        Array(Array = ast::<Array>()),
        Reference(Reference = ast::<Reference>()),
        Tuple(Tuple = ast::<Tuple>())
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::Display,
)]
enum TypeRef {
    Primitive(PrimitiveRef),
    Array(ArrayRef),
    Reference(ReferenceRef),
    Tuple(TupleRef),
}

impl Arbitrary for TypeRef {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = PrimitiveRef::arbitrary().prop_map(Self::Primitive);

        leaf.prop_recursive(10, 50, 5, |ty| {
            prop_oneof![
                ArrayRef::arbitrary_with(Some(ty.clone()))
                    .prop_map(Self::Array),
                ReferenceRef::arbitrary_with(Some(ty.clone()))
                    .prop_map(Self::Reference),
                TupleRef::arbitrary_with(Some(ty)).prop_map(Self::Tuple),
            ]
        })
        .boxed()
    }
}

impl Input<&Type, ()> for &TypeRef {
    fn assert(
        self,
        output: &Type,
        (): (),
    ) -> proptest::test_runner::TestCaseResult {
        match (self, output) {
            (TypeRef::Primitive(primitive_ref), Type::Primitive(primitive)) => {
                primitive_ref.assert(primitive, ())
            }
            (TypeRef::Array(array_ref), Type::Array(array)) => {
                array_ref.assert(array, ())
            }
            (TypeRef::Reference(reference_ref), Type::Reference(reference)) => {
                reference_ref.assert(reference, ())
            }
            (TypeRef::Tuple(tuple_ref), Type::Tuple(tuple)) => {
                tuple_ref.assert(tuple, ())
            }

            _ => Err(TestCaseError::fail(format!(
                "expected {self:?} found {output:?}"
            ))),
        }
    }
}

fn verify_type_ref<TR: Display, TAst: AbstractTree + Debug>(
    ast_ref: &TR,
) -> TestCaseResult
where
    for<'x, 'y> &'x TR: Input<&'y TAst, ()>,
{
    let mut source_map = SourceMap::new();

    let source = ast_ref.to_string();
    let (token_tree, _) = parse_token_tree(&mut source_map, &source);

    let (tree, errors) = TAst::parse(&token_tree);
    let tree = tree.unwrap();

    prop_assert!(errors.is_empty(), "{errors:?}");

    ast_ref.assert(&tree, ())
}

proptest::proptest! {
    #[test]
    fn nested_type(
        type_ref in TypeRef::arbitrary()
    ) {
        verify_type_ref::<TypeRef, Type>(&type_ref)?;
    }
}

abstract_tree! {
    struct TrailingOptional {
        ampersand: token::Punctuation<RelativeLocation>
            = '&',
        mut_keyword: token::Keyword<RelativeLocation>
            = expect::Keyword::Mut.optional(),
    }
}

#[test]
fn trailing_optional() {
    let mut source_map = SourceMap::new();
    let (token_tree, _) = parse_token_tree(&mut source_map, "&");

    let (tree, errors) = TrailingOptional::parse(&token_tree);
    let tree = tree.unwrap();

    assert!(errors.is_empty());

    assert!(tree.ampersand().is_some_and(|x| *x.kind == '&'));
    assert!(tree.mut_keyword().is_none());
}

abstract_tree! {
    #[derive(Debug)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Bracket)}
    struct Types {
        types: #[multi] Type = ast::<Type>().repeat(),
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::Display,
)]
#[display(
    "[{}]", 
    self.types
        .iter()
        .map(std::string::ToString::to_string)
        .collect::<Vec<_>>().join(" ")
)]
struct TypesRef {
    types: Vec<TypeRef>,
}

impl Arbitrary for TypesRef {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(TypeRef::arbitrary(), 0..10)
            .prop_map(|types| Self { types })
            .boxed()
    }
}

impl Input<&Types, ()> for &TypesRef {
    fn assert(
        self,
        output: &Types,
        (): (),
    ) -> proptest::test_runner::TestCaseResult {
        let types = output.types().collect::<Vec<_>>();

        self.types.assert(&types, ())
    }
}

proptest::proptest! {
    #[test]
    fn types(
        types_ref in TypesRef::arbitrary()
    ) {
        verify_type_ref::<TypesRef, Types>(&types_ref)?;
    }
}

abstract_tree! {
    #[derive(Debug)]
    struct Abcd {
        a: token::Punctuation<RelativeLocation> = '+',
        b: token::Punctuation<RelativeLocation> = '-',
        c: token::Punctuation<RelativeLocation> = '*',
        d: token::Punctuation<RelativeLocation> = '/',

        semicolon: token::Punctuation<RelativeLocation>
            = ';'
    }
}

abstract_tree! {
    #[derive(Debug)]
    struct Abc {
        a: token::Punctuation<RelativeLocation> = '+',
        b: token::Punctuation<RelativeLocation> = '-',
        c: token::Punctuation<RelativeLocation> = '*',

        semicolon: token::Punctuation<RelativeLocation>
            = ';'
    }
}

abstract_tree! {
    #[derive(Debug, EnumAsInner)]
    enum AbcOrAbcd {
        #[allow(unused)]
        Abc(Abc = ast::<Abc>()),

        #[allow(unused)]
        Abcd(Abcd = ast::<Abcd>())
    }
}

#[test]
fn error_choice_choose_most_progress() {
    let mut source_map = SourceMap::new();
    let (token_tree, _) = parse_token_tree(&mut source_map, "a b c d e");

    let (tree, errors) = AbcOrAbcd::parse(&token_tree);
    let tree = tree.unwrap().into_abcd().unwrap();

    assert_eq!(errors.len(), 1);

    assert_eq!(
        errors[0].expecteds.iter().copied().collect::<HashSet<_>>(),
        std::iter::once(Expected::Punctuation(';')).collect()
    );

    assert!(token_tree[errors[0].at.branch_id]
        .nodes
        .get(errors[0].at.node_index)
        .is_some_and(|x| x
            .as_leaf()
            .and_then(|x| x.kind.as_identifier())
            .is_some_and(|x| x.as_str() == "e")));

    assert_eq!(tree.a().map(|x| x.kind.0), Some('a'));
    assert_eq!(tree.b().map(|x| x.kind.0), Some('b'));
    assert_eq!(tree.c().map(|x| x.kind.0), Some('c'));
    assert_eq!(tree.d().map(|x| x.kind.0), Some('d'));

    assert!(tree.semicolon().is_none());
}

abstract_tree! {
    #[derive(Debug)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Bracket)}
    struct TypesAll {
        types: #[multi] Type = ast::<Type>().repeat_all(),
    }
}

#[test]
fn repeat_all_error_recovery() {
    let mut source_map = SourceMap::new();
    let (token_tree, _) =
        parse_token_tree(&mut source_map, "[int32 &x float32]");

    let (tree, errors) = TypesAll::parse(&token_tree);
    let tree = tree.unwrap();

    assert_eq!(errors.len(), 1);
    assert_eq!(tree.inner_tree().nodes.len(), 4);

    // `None` ast_info means an error node
    assert!(tree.inner_tree().nodes[2]
        .as_branch()
        .is_some_and(|x| x.ast_info.is_none()));

    // 2 successful parses, 1 partially successful parse, 1 error node
    let types = tree.types().collect::<Vec<_>>();
    assert_eq!(types.len(), 3);

    assert!(types[0].as_primitive().is_some_and(Primitive::is_int32));
    assert!(types[1].as_reference().is_some_and(|x| {
        x.mut_keyword().is_none() && x.r#type().is_none()
    }));
    assert!(types[2].as_primitive().is_some_and(Primitive::is_float32));
}

#[test]
fn reference_missing_type() {
    let mut source_map = SourceMap::new();
    let (token_tree, _) = parse_token_tree(&mut source_map, "&x");

    let (tree, errors) = Type::parse(&token_tree);
    let tree = tree.unwrap();

    assert_eq!(errors.len(), 1);
    let error = &errors[0];

    let token_node = token_tree[error.at.branch_id]
        .nodes
        .get(error.at.node_index)
        .unwrap()
        .as_leaf();

    assert!(token_node.is_some_and(|x| {
        x.kind.as_identifier().is_some_and(|x| x.as_str() == "x")
    }));

    assert!(
        error.expecteds.iter().copied().collect::<HashSet<_>>()
            == [
                expect::Keyword::Int32.into(),
                expect::Keyword::Uint32.into(),
                expect::Keyword::Float32.into(),
                expect::Keyword::Bool.into(),
                expect::Keyword::Mut.into(),
                expect::Fragment::Delimited(DelimiterKind::Bracket).into(),
                expect::Fragment::Delimited(DelimiterKind::Parenthesis).into(),
                '&'.into(),
            ]
            .into_iter()
            .collect::<HashSet<_>>()
    );

    assert!(tree.as_reference().is_some_and(|x| {
        x.mut_keyword().is_none() && x.r#type().is_none()
    }));
}

abstract_tree! {
    #[derive(Debug)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Bracket)}
    struct TypesPlus {
        types: #[multi] Type = ast::<Type>().repeat_with_separator('+'),
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::Display,
)]
#[display(
    "[{}]", 
    self.types
        .iter()
        .map(std::string::ToString::to_string)
        .collect::<Vec<_>>().join(" + ")
)]
struct TypesPlusRef {
    types: Vec<TypeRef>,
}

impl Arbitrary for TypesPlusRef {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(TypeRef::arbitrary(), 0..10)
            .prop_map(|types| Self { types })
            .boxed()
    }
}

impl Input<&TypesPlus, ()> for &TypesPlusRef {
    fn assert(
        self,
        output: &TypesPlus,
        (): (),
    ) -> proptest::test_runner::TestCaseResult {
        let types = output.types().collect::<Vec<_>>();

        self.types.assert(&types, ())
    }
}

proptest::proptest! {
    #[test]
    fn types_plus(
        types_ref in TypesPlusRef::arbitrary()
    ) {
        verify_type_ref::<TypesPlusRef, TypesPlus>(&types_ref)?;
    }
}
