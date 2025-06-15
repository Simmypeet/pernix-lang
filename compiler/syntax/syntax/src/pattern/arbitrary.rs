use std::fmt::{Debug, Display};

use pernixc_lexical::kind::arbitrary::{Identifier, Numeric};
use pernixc_parser::{abstract_tree::AbstractTree, expect};
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy as _},
    prop_assert_eq, prop_oneof,
};

use crate::{arbitrary::ReferenceOf, reference};

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "{}{}{identifier}",
        if *mutable_keyword { "mut " } else { "" },
        reference_of.as_ref().map_or_else(String::default, ToString::to_string)
    )]
    pub struct Named for super::Named {
        pub mutable_keyword (bool),
        pub reference_of (Option<ReferenceOf>),

        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Identifier)
    }
}

impl Arbitrary for Named {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            bool::arbitrary(),
            proptest::option::of(ReferenceOf::arbitrary()),
            Identifier::arbitrary(),
        )
            .prop_map(|(mutable_keyword, reference_of, identifier)| Self {
                mutable_keyword,
                reference_of,
                identifier,
            })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, Copy, derive_more::Display)]
    #[display("..")]
    pub struct Wildcard for super::Wildcard {}
}

impl Arbitrary for Wildcard {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Just(Self {}).boxed()
    }
}

#[derive(Debug, Clone, derive_more::Display)]
#[display("{identifier}: {pattern}")]
pub struct FieldAssociation<P> {
    pub identifier: Identifier,
    pub pattern: P,
}

impl<IP: Debug, OP: Debug + AbstractTree>
    Input<&super::FieldAssociation<OP>, ()> for &FieldAssociation<IP>
where
    for<'x, 'y> &'x IP: Input<&'y OP, ()>,
{
    fn assert(
        self,
        output: &super::FieldAssociation<OP>,
        (): (),
    ) -> proptest::test_runner::TestCaseResult {
        Some(&self.identifier)
            .assert(output.identifier().as_ref().map(|x| &x.kind), ())?;

        Some(&self.pattern).assert(output.pattern().as_ref(), ())
    }
}

impl<P: Debug + Arbitrary<Strategy = BoxedStrategy<P>> + 'static> Arbitrary
    for FieldAssociation<P>
{
    type Parameters = Option<BoxedStrategy<P>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        (Identifier::arbitrary(), arg.unwrap_or_else(P::arbitrary))
            .prop_map(|(identifier, pattern)| Self { identifier, pattern })
            .boxed()
    }
}

#[derive(Debug, Clone, derive_more::Display)]
pub enum Field<P> {
    Wildcard(Wildcard),
    Named(Named),
    Association(FieldAssociation<P>),
}

impl<P: Debug + Arbitrary<Strategy = BoxedStrategy<P>> + 'static> Arbitrary
    for Field<P>
{
    type Parameters = Option<BoxedStrategy<P>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Wildcard::arbitrary().prop_map(Field::Wildcard),
            Named::arbitrary().prop_map(Field::Named),
            FieldAssociation::<P>::arbitrary_with(arg)
                .prop_map(Field::Association),
        ]
        .boxed()
    }
}

impl<IP: Debug, OP: Debug + AbstractTree> Input<&super::Field<OP>, ()>
    for &Field<IP>
where
    for<'x, 'y> &'x IP: Input<&'y OP, ()>,
{
    fn assert(
        self,
        output: &super::Field<OP>,
        (): (),
    ) -> proptest::test_runner::TestCaseResult {
        match (self, output) {
            (Field::Wildcard(_), super::Field::Wildcard(_)) => Ok(()),
            (Field::Named(left), super::Field::Named(right)) => {
                left.assert(right, ())
            }
            (
                Field::Association(left),
                super::Field::FieldAssociation(right),
            ) => left.assert(right, ()),

            _ => Err(proptest::test_runner::TestCaseError::fail(format!(
                "Expected {output:?}, but got {self:?}"
            ))),
        }
    }
}

#[derive(Debug, Clone, derive_more::Display)]
#[display(bound(P: Display))]
#[display(
    "{{ {} }}",
    self.fields
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ")
)]
pub struct Struct<P> {
    pub fields: Vec<Field<P>>,
}

impl<P: Debug + Arbitrary<Strategy = BoxedStrategy<P>> + 'static> Arbitrary
    for Struct<P>
where
    P::Strategy: 'static,
{
    type Parameters = Option<BoxedStrategy<P>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(Field::arbitrary_with(arg), 0..10)
            .prop_map(|fields| Self { fields })
            .boxed()
    }
}

impl<PI: Debug, PO: Debug + AbstractTree> Input<&super::Struct<PO>, ()>
    for &Struct<PI>
where
    for<'x, 'y> &'x PI: Input<&'y PO, ()>,
{
    fn assert(
        self,
        output: &super::Struct<PO>,
        (): (),
    ) -> proptest::test_runner::TestCaseResult {
        self.fields
            .as_slice()
            .assert(output.fields().collect::<Vec<_>>().as_slice(), ())?;

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone, Copy, derive_more::Display)]
    pub enum Boolean for super::Boolean {
        #[display("true")]
        #{prop_assert(|x| x.kind == expect::Keyword::True)}
        True,

        #[display("false")]
        #{prop_assert(|x| x.kind == expect::Keyword::False)}
        False,
    }
}

impl Arbitrary for Boolean {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![Just(Self::True), Just(Self::False),].boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "{}{numeric}",
        if *minus { "-" } else { "" },
    )]
    pub struct Integer for super::Integer {
        pub minus (bool),

        #{map_input_assert(numeric, &numeric.kind)}
        pub numeric (Numeric)
    }
}

impl Arbitrary for Integer {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (bool::arbitrary(), Numeric::arbitrary())
            .prop_map(|(minus, numeric)| Self { minus, numeric })
            .boxed()
    }
}

#[derive(Debug, Clone, derive_more::Display)]
#[display(
    "{}{pattern}",
    if *ellipsis { "..." } else { "" },
)]
pub struct Unpackable<P> {
    pub ellipsis: bool,
    pub pattern: P,
}

impl<P: Debug + Arbitrary<Strategy = BoxedStrategy<P>> + 'static> Arbitrary
    for Unpackable<P>
{
    type Parameters = Option<BoxedStrategy<P>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        (bool::arbitrary(), arg.unwrap_or_else(P::arbitrary))
            .prop_map(|(ellipsis, pattern)| Self { ellipsis, pattern })
            .boxed()
    }
}

impl<IP: Debug, OP: Debug + AbstractTree> Input<&super::Unpackable<OP>, ()>
    for &Unpackable<IP>
where
    for<'x, 'y> &'x IP: Input<&'y OP, ()>,
{
    fn assert(
        self,
        output: &super::Unpackable<OP>,
        (): (),
    ) -> proptest::test_runner::TestCaseResult {
        prop_assert_eq!(self.ellipsis, output.ellipsis().is_some());

        Some(&self.pattern).assert(output.pattern().as_ref(), ())
    }
}

#[derive(Debug, Clone, derive_more::Display)]
#[display(bound(P: Display))]
#[display(
    "({})",
    self.types
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ")
)]
pub struct Tuple<P> {
    pub types: Vec<Unpackable<P>>,
}

impl<P: Debug + Arbitrary<Strategy = BoxedStrategy<P>> + 'static> Arbitrary
    for Tuple<P>
where
    P::Strategy: 'static,
{
    type Parameters = Option<BoxedStrategy<P>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(Unpackable::arbitrary_with(arg), 0..10)
            .prop_map(|types| Self { types })
            .boxed()
    }
}

impl<IP: Debug, OP: Debug + AbstractTree> Input<&super::Tuple<OP>, ()>
    for &Tuple<IP>
where
    for<'x, 'y> &'x IP: Input<&'y OP, ()>,
{
    fn assert(
        self,
        output: &super::Tuple<OP>,
        (): (),
    ) -> proptest::test_runner::TestCaseResult {
        self.types
            .as_slice()
            .assert(output.types().collect::<Vec<_>>().as_slice(), ())?;

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    pub enum Irrefutable for super::Irrefutable {
        Struct(Struct<Self>),
        Named(Named),
        Tuple(Tuple<Self>),
        Wildcard(Wildcard),
    }
}

impl Arbitrary for Irrefutable {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            Named::arbitrary().prop_map(Irrefutable::Named),
            Wildcard::arbitrary().prop_map(Irrefutable::Wildcard),
        ];

        leaf.prop_recursive(4, 40, 10, |inner| {
            prop_oneof![
                Struct::arbitrary_with(Some(inner.clone()))
                    .prop_map(Irrefutable::Struct),
                Tuple::arbitrary_with(Some(inner)).prop_map(Irrefutable::Tuple),
            ]
        })
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display("({pattern})")]
    pub struct EnumAssociation for super::EnumAssociation {
        pub pattern (Box<Refutable>)
    }
}

impl Arbitrary for EnumAssociation {
    type Parameters = Option<BoxedStrategy<Refutable>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        arg.unwrap_or_else(Refutable::arbitrary)
            .prop_map(|pattern| Self { pattern: Box::new(pattern) })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "case {identifier}{}",
        association.as_ref().map_or_else(String::default, ToString::to_string)
    )]
    pub struct Enum for super::Enum {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Identifier),

        pub association (Option<EnumAssociation>),
    }
}

impl Arbitrary for Enum {
    type Parameters = Option<BoxedStrategy<Refutable>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(EnumAssociation::arbitrary_with(arg)),
        )
            .prop_map(|(identifier, association)| Self {
                identifier,
                association,
            })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    pub enum Refutable for super::Refutable {
        Boolean(Boolean),
        Integer(Integer),
        Struct(Struct<Self>),
        Enum(Enum),
        Named(Named),
        Tuple(Tuple<Self>),
        Wildcard(Wildcard),
    }
}

impl Arbitrary for Refutable {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            Boolean::arbitrary().prop_map(Refutable::Boolean),
            Integer::arbitrary().prop_map(Refutable::Integer),
            Named::arbitrary().prop_map(Refutable::Named),
            Wildcard::arbitrary().prop_map(Refutable::Wildcard),
        ];

        leaf.prop_recursive(4, 40, 10, |inner| {
            prop_oneof![
                Struct::arbitrary_with(Some(inner.clone()))
                    .prop_map(Refutable::Struct),
                Enum::arbitrary_with(Some(inner.clone()))
                    .prop_map(Refutable::Enum),
                Tuple::arbitrary_with(Some(inner)).prop_map(Refutable::Tuple),
            ]
        })
        .boxed()
    }
}
