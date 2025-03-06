use std::fmt::{Display, Write};

use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy, TestCaseError},
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    expression::strategy::Expression,
    predicate::strategy::{BoundList, TypeBound},
    r#type::strategy::Type,
    strategy::{
        ConnectedList, ConstantPunctuation, Identifier, IndentDisplay,
        LifetimeParameter,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantParameter {
    pub identifier: Identifier,
    pub ty: Type,
    pub default: Option<Expression>,
}

impl Input<&super::ConstantParameter> for &ConstantParameter {
    fn assert(self, output: &super::ConstantParameter) -> TestCaseResult {
        self.identifier.assert(&output.identifier)?;
        self.ty.assert(&output.r#type)?;
        self.default.as_ref().assert(output.default.as_ref().map(|x| &x.value))
    }
}

impl Arbitrary for ConstantParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            Type::arbitrary(),
            proptest::option::of(Expression::arbitrary()),
        )
            .prop_map(|(identifier, ty, default)| Self {
                identifier,
                ty,
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
        self.ty.indent_fmt(f, indent)?;

        if let Some(default) = self.default.as_ref() {
            write!(f, " = ")?;
            default.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeParameter {
    pub identifier: Identifier,
    pub bounds: Option<BoundList<TypeBound>>,
    pub default: Option<Type>,
}

impl Input<&super::TypeParameter> for &TypeParameter {
    fn assert(self, output: &super::TypeParameter) -> TestCaseResult {
        self.identifier.assert(&output.identifier)?;
        self.bounds
            .as_ref()
            .assert(output.bounds.as_ref().map(|x| &x.bounds))?;
        self.default.as_ref().assert(output.default.as_ref().map(|x| &x.value))
    }
}

impl Arbitrary for TypeParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(BoundList::arbitrary_with(
                TypeBound::arbitrary(),
            )),
            proptest::option::of(Type::arbitrary()),
        )
            .prop_map(|(identifier, bounds, ty)| Self {
                identifier,
                bounds,
                default: ty,
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

        if let Some(bounds) = self.bounds.as_ref() {
            write!(f, ": ")?;
            bounds.indent_fmt(f, indent)?;
        }

        if let Some(type_parameter) = self.default.as_ref() {
            write!(f, " = ")?;
            type_parameter.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GenericParameter {
    Lifetime(LifetimeParameter),
    Type(TypeParameter),
    Const(ConstantParameter),
}

impl Input<&super::GenericParameter> for &GenericParameter {
    fn assert(self, output: &super::GenericParameter) -> TestCaseResult {
        match (self, output) {
            (
                GenericParameter::Lifetime(i),
                super::GenericParameter::Lifetime(o),
            ) => i.assert(o),
            (GenericParameter::Type(i), super::GenericParameter::Type(o)) => {
                i.assert(o)
            }
            (
                GenericParameter::Const(i),
                super::GenericParameter::Constant(o),
            ) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, found {output:?}"
            ))),
        }
    }
}

impl Arbitrary for GenericParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::prop_oneof![
            LifetimeParameter::arbitrary().prop_map(Self::Lifetime),
            TypeParameter::arbitrary().prop_map(Self::Type),
            ConstantParameter::arbitrary().prop_map(Self::Const),
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
            Self::Const(c) => c.indent_fmt(f, indent),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericParameters {
    pub parameter_list:
        Option<ConnectedList<GenericParameter, ConstantPunctuation<','>>>,
}

impl Input<&super::GenericParameters> for &GenericParameters {
    fn assert(self, output: &super::GenericParameters) -> TestCaseResult {
        self.parameter_list.as_ref().assert(output.connected_list.as_ref())
    }
}

impl Arbitrary for GenericParameters {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            GenericParameter::arbitrary(),
            ConstantPunctuation::arbitrary(),
        ))
        .prop_map(|parameter_list| Self { parameter_list })
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

        if let Some(parameter_list) = &self.parameter_list {
            parameter_list.indent_fmt(f, indent)?;
        }

        f.write_char(']')
    }
}
