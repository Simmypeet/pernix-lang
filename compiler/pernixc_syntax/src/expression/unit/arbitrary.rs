use enum_as_inner::EnumAsInner;
use pernixc_lexical::kind::{arbitrary, arbitrary::Identifier};
use pernixc_parser::expect;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just},
    prop_oneof,
    strategy::Strategy,
};

use crate::{
    arbitrary::QualifiedIdentifier, expression::arbitrary::Expression,
    r#type::arbitrary::Type, reference,
};

reference! {
    #[derive(Debug, Clone, derive_more::Display, EnumAsInner)]
    pub enum Unit for super::Unit {
        Boolean(Boolean),
        Numeric(Numeric),
        Parenthesized(Parenthesized),
        QualifiedIdentifier(QualifiedIdentifier),
        Struct(Struct),
        Array(Array),
        Phantom(Phantom),
        Panic(Panic),
    }
}

impl Arbitrary for Unit {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((expr, ty, qi): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Boolean::arbitrary_with(()).prop_map(Unit::Boolean),
            Numeric::arbitrary_with(()).prop_map(Unit::Numeric),
            qi.clone()
                .unwrap_or_else(|| {
                    QualifiedIdentifier::arbitrary_with((
                        ty.clone(),
                        expr.clone(),
                    ))
                })
                .prop_map(Unit::QualifiedIdentifier),
            Parenthesized::arbitrary_with(expr.clone())
                .prop_map(Unit::Parenthesized),
            Struct::arbitrary_with((ty, expr.clone(), qi))
                .prop_map(Unit::Struct),
            Array::arbitrary_with(expr).prop_map(Unit::Array),
            Just(Self::Phantom(Phantom {})),
            Just(Self::Panic(Panic {}))
        ]
        .boxed()
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
        False
    }
}

impl Arbitrary for Boolean {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        prop_oneof![Just(Self::True), Just(Self::False),].boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(".{digits}")]
    pub struct Decimal for super::Decimal {
        #{map_input_assert(digits, &digits.kind)}
        pub digits (arbitrary::Numeric)
    }
}

impl Arbitrary for Decimal {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        arbitrary::Numeric::arbitrary()
            .prop_map(|digits| Self { digits })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "{numeric}{}{}",
        decimal.as_ref().map_or_else(String::default, ToString::to_string),
        identifier.as_ref().map_or_else(String::default, ToString::to_string)
    )]
    pub struct Numeric for super::Numeric {
        #{map_input_assert(numeric, &numeric.kind)}
        pub numeric (arbitrary::Numeric),

        pub decimal (Option<Decimal>),

        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Option<Identifier>)
    }
}

impl Arbitrary for Numeric {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            arbitrary::Numeric::arbitrary(),
            proptest::option::of(Decimal::arbitrary()),
            proptest::option::of(Identifier::arbitrary()),
        )
            .prop_map(|(numeric, decimal, identifier)| Self {
                numeric,
                decimal,
                identifier,
            })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "{}{expression}",
        if *ellipsis { "..." } else { ""},
    )]
    pub struct Unpackable for super::Unpackable {
        pub ellipsis (bool),
        pub expression (Box<Expression>)
    }
}

impl Arbitrary for Unpackable {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(expr: Self::Parameters) -> Self::Strategy {
        let expr = expr.unwrap_or_else(Expression::arbitrary);

        (bool::arbitrary(), expr.prop_map(Box::new))
            .prop_map(|(ellipsis, expression)| Self { ellipsis, expression })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "({})",
        unpackables.iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ")
    )]
    pub struct Parenthesized for super::Parenthesized {
        pub unpackables (Vec<Unpackable>),
    }
}

impl Arbitrary for Parenthesized {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(expr: Self::Parameters) -> Self::Strategy {
        let unpackable = Unpackable::arbitrary_with(expr);

        proptest::collection::vec(unpackable, 0..10)
            .prop_map(|unpackables| Self { unpackables })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display("{identifier}: {expression}")]
    pub struct FieldInitializer for super::FieldInitializer{
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Identifier),

        pub expression (Box<Expression>),
    }
}

impl Arbitrary for FieldInitializer {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(expr: Self::Parameters) -> Self::Strategy {
        let expr = expr.unwrap_or_else(Expression::arbitrary);

        (Identifier::arbitrary(), expr.prop_map(Box::new))
            .prop_map(|(identifier, expression)| Self {
                identifier,
                expression,
            })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "{{ {} }}",
        initializers
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ")
    )]
    pub struct FieldInitializerBody for super::FieldInitializerBody{
        pub initializers (Vec<FieldInitializer>),
    }
}

impl Arbitrary for FieldInitializerBody {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(expr: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(FieldInitializer::arbitrary_with(expr), 0..10)
            .prop_map(|initializers| Self { initializers })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display("{qualified_identifier} {field_initializer_body}")]
    pub struct Struct for super::Struct {
        pub qualified_identifier (QualifiedIdentifier),
        pub field_initializer_body (FieldInitializerBody),
    }
}

impl Arbitrary for Struct {
    type Parameters = (
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((ty, expr, qi): Self::Parameters) -> Self::Strategy {
        (
            qi.unwrap_or_else(|| {
                QualifiedIdentifier::arbitrary_with((ty.clone(), expr.clone()))
            }),
            FieldInitializerBody::arbitrary_with(expr),
        )
            .prop_map(|(qualified_identifier, field_initializer_body)| Self {
                qualified_identifier,
                field_initializer_body,
            })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, Copy, derive_more::Display)]
    #[display("phantom")]
    pub struct Phantom for super::Phantom {}
}

impl Arbitrary for Phantom {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        Just(Self {}).boxed()
    }
}

reference! {
    #[derive(Debug, Clone, Copy, derive_more::Display)]
    #[display("panic")]
    pub struct Panic for super::Panic {}
}

impl Arbitrary for Panic {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Just(Self {}).boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "[{}]",
        expressions
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ")
    )]
    pub struct Array for super::Array {
        pub expressions (Vec<Expression>),
    }
}

impl Arbitrary for Array {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(expr: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(
            expr.unwrap_or_else(Expression::arbitrary),
            0..10,
        )
        .prop_map(|expressions| Self { expressions })
        .boxed()
    }
}
