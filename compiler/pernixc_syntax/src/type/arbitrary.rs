#![allow(missing_docs)]

use pernixc_parser::expect;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy},
    prop_oneof,
};

use crate::{arbitrary::Lifetime, reference};

reference! {
    #[derive(Debug, Clone, Copy, derive_more::Display)]
    pub enum Primitive for super::Primitive {
        #[display("bool")]
        #{prop_assert(|x| x.kind == expect::Keyword::Bool)}
        Bool,

        #[display("float32")]
        #{prop_assert(|x| x.kind == expect::Keyword::Float32)}
        Float32,

        #[display("float64")]
        #{prop_assert(|x| x.kind == expect::Keyword::Float64)}
        Float64,

        #[display("int8")]
        #{prop_assert(|x| x.kind == expect::Keyword::Int8)}
        Int8,

        #[display("int16")]
        #{prop_assert(|x| x.kind == expect::Keyword::Int16)}
        Int16,

        #[display("int32")]
        #{prop_assert(|x| x.kind == expect::Keyword::Int32)}
        Int32,

        #[display("int64")]
        #{prop_assert(|x| x.kind == expect::Keyword::Int64)}
        Int64,

        #[display("uint8")]
        #{prop_assert(|x| x.kind == expect::Keyword::Uint8)}
        Uint8,

        #[display("uint16")]
        #{prop_assert(|x| x.kind == expect::Keyword::Uint16)}
        Uint16,

        #[display("uint32")]
        #{prop_assert(|x| x.kind == expect::Keyword::Uint32)}
        Uint32,

        #[display("uint64")]
        #{prop_assert(|x| x.kind == expect::Keyword::Uint64)}
        Uint64,

        #[display("usize")]
        #{prop_assert(|x| x.kind == expect::Keyword::Usize)}
        Usize,

        #[display("isize")]
        #{prop_assert(|x| x.kind == expect::Keyword::Isize)}
        Isize,
    }
}

impl Arbitrary for Primitive {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Bool),
            Just(Self::Float32),
            Just(Self::Float64),
            Just(Self::Int8),
            Just(Self::Int16),
            Just(Self::Int32),
            Just(Self::Int64),
            Just(Self::Uint8),
            Just(Self::Uint16),
            Just(Self::Uint32),
            Just(Self::Uint64),
            Just(Self::Usize),
            Just(Self::Isize),
        ]
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "&{}{}{type}",
        lifetime.as_ref().map(|x| format!("{x} ")).unwrap_or_default(),
        if *mut_keyword { "mut " } else { "" },
    )]
    pub struct Reference for super::Reference {
        pub lifetime (Option<Lifetime>),
        pub mut_keyword (bool),
        pub r#type (Box<Type>)
    }
}

impl Arbitrary for Reference {
    type Parameters = Option<BoxedStrategy<Type>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(ty: Self::Parameters) -> Self::Strategy {
        let ty = ty.unwrap_or_else(Type::arbitrary);

        (
            proptest::option::of(Lifetime::arbitrary()),
            bool::arbitrary(),
            ty.prop_map(Box::new),
        )
            .prop_map(|(lifetime, mut_keyword, r#type)| Self {
                lifetime,
                mut_keyword,
                r#type,
            })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    pub enum Type for super::Type {
        Primitive(Primitive),
        Reference(Reference),
    }
}

impl Arbitrary for Type {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        let leaf = Primitive::arbitrary().prop_map(Type::Primitive);

        leaf.prop_recursive(4, 20, 5, |inner| {
            prop_oneof![Reference::arbitrary_with(Some(inner))
                .prop_map(Self::Reference),]
        })
        .boxed()
    }
}
