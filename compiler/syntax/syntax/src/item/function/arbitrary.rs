use std::fmt::Write as _;

use pernixc_lexical::kind::arbitrary::Identifier;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy as _},
    prop_oneof,
};

use crate::{
    arbitrary::{
        AccessModifier, IndentDisplay, IntoSeparated, QualifiedIdentifier,
    },
    item::{arbitrary::Body, generic_parameters::arbitrary::GenericParameters},
    pattern::arbitrary::Irrefutable,
    predicate::arbitrary::HigherRankedLifetimes,
    r#type::arbitrary::Type,
    reference,
    statement::arbitrary::Statement,
};

reference! {
    #[derive(Debug, Clone)]
    pub struct Signature for super::Signature {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Identifier),
        pub generic_parameters (Option<GenericParameters>),
        pub parameters (Parameters),
        pub return_type (Option<ReturnType>),
        pub do_effect (Option<DoEffect>),
    }
}

impl Arbitrary for Signature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(GenericParameters::arbitrary()),
            Parameters::arbitrary(),
            proptest::option::of(ReturnType::arbitrary()),
            proptest::option::of(DoEffect::arbitrary()),
        )
            .prop_map(
                |(
                    identifier,
                    generic_parameters,
                    parameters,
                    return_type,
                    do_effect,
                )| {
                    Self {
                        identifier,
                        generic_parameters,
                        parameters,
                        return_type,
                        do_effect,
                    }
                },
            )
            .boxed()
    }
}

impl IndentDisplay for Signature {
    fn indent_fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(formatter, "function {}", self.identifier)?;

        if let Some(generic_parameters) = &self.generic_parameters {
            generic_parameters.indent_fmt(formatter, indent)?;
        }

        self.parameters.indent_fmt(formatter, indent)?;

        if let Some(return_type) = &self.return_type {
            return_type.indent_fmt(formatter, indent)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Function for super::Function {
        pub access_modifier (AccessModifier),
        pub unsafe_keyword (bool),
        pub const_keyword (bool),
        pub signature (Signature),
        pub body (Body<Statement>),
    }
}

impl Arbitrary for Function {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            proptest::bool::ANY,
            proptest::bool::ANY,
            Signature::arbitrary(),
            Body::arbitrary(),
        )
            .prop_map(
                |(
                    access_modifier,
                    unsafe_keyword,
                    const_keyword,
                    signature,
                    body,
                )| {
                    Self {
                        access_modifier,
                        unsafe_keyword,
                        const_keyword,
                        signature,
                        body,
                    }
                },
            )
            .boxed()
    }
}

impl IndentDisplay for Function {
    fn indent_fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(formatter, "{} ", self.access_modifier)?;

        if self.unsafe_keyword {
            write!(formatter, "unsafe ")?;
        }

        if self.const_keyword {
            write!(formatter, "const ")?;
        }

        self.signature.indent_fmt(formatter, indent)?;
        self.body.indent_fmt(formatter, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Parameter for super::Parameter {
        pub irrefutable_pattern (Irrefutable),
        pub r#type (Type),
    }
}

impl Arbitrary for Parameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (Irrefutable::arbitrary(), Type::arbitrary())
            .prop_map(|(irrefutable_pattern, ty)| Self {
                irrefutable_pattern,
                r#type: ty,
            })
            .boxed()
    }
}

impl IndentDisplay for Parameter {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "{}", self.irrefutable_pattern)?;
        f.write_str(": ")?;
        self.r#type.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub enum ParameterKind for super::ParameterKind{
        Regular(Parameter),

        #{prop_assert(|x| true)}
        Variadic,
    }
}

impl Arbitrary for ParameterKind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            6 => Parameter::arbitrary().prop_map(Self::Regular),
            1 => Just(Self::Variadic),
        ]
        .boxed()
    }
}

impl IndentDisplay for ParameterKind {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Regular(parameter) => parameter.indent_fmt(f, indent),
            Self::Variadic => write!(f, "..."),
        }
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Parameters for super::Parameters {
        pub parameters (Vec<ParameterKind>)

    }
}

impl Arbitrary for Parameters {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(ParameterKind::arbitrary(), 0..6)
            .prop_map(|parameters| Self { parameters })
            .boxed()
    }
}

impl IndentDisplay for Parameters {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_char('(')?;
        self.parameters.into_separated(", ").indent_fmt(f, indent)?;
        f.write_char(')')
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct ReturnType for super::ReturnType {
        pub r#type (Type),
    }
}

impl Arbitrary for ReturnType {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Type::arbitrary().prop_map(|r#type| Self { r#type }).boxed()
    }
}

impl IndentDisplay for ReturnType {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str(" -> ")?;
        self.r#type.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct EffectUnit for super::EffectUnit {
        pub higher_ranked_lifetimes (Option<HigherRankedLifetimes>),
        pub qualified_identifier (QualifiedIdentifier)
    }
}

impl Arbitrary for EffectUnit {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(HigherRankedLifetimes::arbitrary()),
            QualifiedIdentifier::arbitrary(),
        )
            .prop_map(|(higher_ranked_lifetimes, qualified_identifier)| Self {
                higher_ranked_lifetimes,
                qualified_identifier,
            })
            .boxed()
    }
}

impl IndentDisplay for EffectUnit {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            write!(f, "{higher_ranked_lifetimes} ")?;
        }

        self.qualified_identifier.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct EffectUnitList for super::EffectUnitList {
        pub effect_units (Vec<EffectUnit>)
    }
}

impl Arbitrary for EffectUnitList {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(EffectUnit::arbitrary(), 1..6)
            .prop_map(|effect_units| Self { effect_units })
            .boxed()
    }
}

impl IndentDisplay for EffectUnitList {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.effect_units.into_separated(" + ").indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct ParenthesizedEffectUnitList
        for super::ParenthesizedEffectUnitList {
        pub effect_units (Vec<EffectUnit>)
    }
}

impl Arbitrary for ParenthesizedEffectUnitList {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(EffectUnit::arbitrary(), 1..6)
            .prop_map(|effect_units| Self { effect_units })
            .boxed()
    }
}

impl IndentDisplay for ParenthesizedEffectUnitList {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_char('(')?;
        self.effect_units.into_separated(" + ").indent_fmt(f, indent)?;
        f.write_char(')')
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub enum EffectUnitListKind for super::EffectUnitListKind {
        EffectUnitList(EffectUnitList),
        ParenthesizedEffectUnitList(ParenthesizedEffectUnitList)
    }
}

impl Arbitrary for EffectUnitListKind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            3 => EffectUnitList::arbitrary()
                .prop_map(Self::EffectUnitList),
            1 => ParenthesizedEffectUnitList::arbitrary()
                .prop_map(Self::ParenthesizedEffectUnitList),
        ]
        .boxed()
    }
}

impl IndentDisplay for EffectUnitListKind {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::EffectUnitList(effect_unit_list) => {
                effect_unit_list.indent_fmt(f, indent)
            }
            Self::ParenthesizedEffectUnitList(
                parenthesized_effect_unit_list,
            ) => parenthesized_effect_unit_list.indent_fmt(f, indent),
        }
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct DoEffect for super::DoEffect {
        pub effect_unit_list (EffectUnitListKind)
    }
}

impl Arbitrary for DoEffect {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        EffectUnitListKind::arbitrary()
            .prop_map(|effect_unit_list| Self { effect_unit_list })
            .boxed()
    }
}

impl IndentDisplay for DoEffect {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, " do ")?;
        self.effect_unit_list.indent_fmt(f, indent)
    }
}
