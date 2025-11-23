use std::fmt::{Display, Write};

use pernixc_lexical::kind::arbitrary::Identifier;
use pernixc_parser::abstract_tree::AbstractTree;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy},
    test_runner::TestCaseResult,
};

use crate::{
    arbitrary::{IndentDisplay, IntoSeparated, LifetimeParameter},
    expression::arbitrary::Expression,
    predicate::arbitrary::TypeBound,
    reference,
    r#type::arbitrary::Type,
};

#[derive(Debug, Clone)]
pub struct Default<T> {
    pub value: T,
}

impl<T: std::fmt::Debug, U: std::fmt::Debug + AbstractTree>
    Input<&super::Default<U>, ()> for &Default<T>
where
    for<'x, 'y> &'x T: Input<&'y U, ()>,
{
    fn assert(self, output: &super::Default<U>, (): ()) -> TestCaseResult {
        Some(&self.value).assert(output.value().as_ref(), ())
    }
}

impl<T: IndentDisplay> IndentDisplay for Default<T> {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, " = ")?;
        self.value.indent_fmt(f, indent)
    }
}

impl<T: Arbitrary<Strategy = BoxedStrategy<T>> + 'static> Arbitrary
    for Default<T>
{
    type Parameters = Option<BoxedStrategy<T>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        args.unwrap_or_else(T::arbitrary)
            .prop_map(|value| Self { value })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct ConstantParameter for super::ConstantParameter {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Identifier),
        pub r#type (Type),
        pub default (Option<Default<Expression>>),
    }
}

impl Arbitrary for ConstantParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            Type::arbitrary(),
            proptest::option::of(Default::arbitrary()),
        )
            .prop_map(|(identifier, r#type, default)| Self {
                identifier,
                r#type,
                default,
            })
            .boxed()
    }
}

impl IndentDisplay for ConstantParameter {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "const {}: ", self.identifier)?;
        self.r#type.indent_fmt(f, indent)?;

        if let Some(default) = self.default.as_ref() {
            default.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct TypeParameterBound for super::TypeParameterBound {
        pub bounds (Vec<TypeBound>),
    }
}

impl IndentDisplay for TypeParameterBound {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, ": ")?;
        self.bounds.into_separated(" + ").indent_fmt(f, indent)
    }
}

impl Arbitrary for TypeParameterBound {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(TypeBound::arbitrary(), 1..=6)
            .prop_map(|bounds| Self { bounds })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct TypeParameter for super::TypeParameter {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Identifier),
        pub bound (Option<TypeParameterBound>),
        pub default (Option<Default<Type>>),
    }
}

impl Arbitrary for TypeParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(TypeParameterBound::arbitrary()),
            proptest::option::of(Default::arbitrary()),
        )
            .prop_map(|(identifier, bound, default)| Self {
                identifier,
                bound,
                default,
            })
            .boxed()
    }
}

impl IndentDisplay for TypeParameter {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "{}", self.identifier)?;

        if let Some(bounds) = self.bound.as_ref() {
            bounds.indent_fmt(f, indent)?;
        }

        if let Some(type_parameter) = self.default.as_ref() {
            type_parameter.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub enum GenericParameter for super::GenericParameter {
        Lifetime(LifetimeParameter),
        Type(TypeParameter),
        Constant(ConstantParameter),
    }
}

impl Arbitrary for GenericParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::prop_oneof![
            LifetimeParameter::arbitrary().prop_map(Self::Lifetime),
            TypeParameter::arbitrary().prop_map(Self::Type),
            ConstantParameter::arbitrary().prop_map(Self::Constant),
        ]
        .boxed()
    }
}

impl IndentDisplay for GenericParameter {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Lifetime(l) => Display::fmt(l, f),
            Self::Type(t) => t.indent_fmt(f, indent),
            Self::Constant(c) => c.indent_fmt(f, indent),
        }
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct GenericParameters for super::GenericParameters {
        pub parameters (Vec<GenericParameter>)
    }
}

impl Arbitrary for GenericParameters {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(GenericParameter::arbitrary(), 1..=6)
            .prop_map(|parameters| Self { parameters })
            .boxed()
    }
}

impl IndentDisplay for GenericParameters {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_char('[')?;
        self.parameters.into_separated(", ").indent_fmt(f, indent)?;
        f.write_char(']')
    }
}
