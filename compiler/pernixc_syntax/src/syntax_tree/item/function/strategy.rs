use std::fmt::{Display, Write};

use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy, TestCaseError},
    prop_assert_eq, prop_oneof,
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    item::{generic_parameter::strategy::GenericParameters, strategy::Body},
    pattern::strategy::Irrefutable,
    r#type::strategy::Type,
    statement::strategy::Statement,
    strategy::{
        AccessModifier, ConnectedList, ConstantPunctuation, Identifier,
        IndentDisplay,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    identifier: Identifier,
    generic_parameters: Option<GenericParameters>,
    parameters: Parameters,
    return_type: Option<ReturnType>,
}

impl Input<&super::Signature> for &Signature {
    fn assert(self, output: &super::Signature) -> TestCaseResult {
        self.identifier.assert(&output.identifier)?;
        self.generic_parameters
            .as_ref()
            .assert(output.generic_parameters.as_ref())?;
        self.parameters.assert(&output.parameters)?;
        self.return_type.as_ref().assert(output.return_type.as_ref())
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
        )
            .prop_map(
                |(identifier, generic_parameters, parameters, return_type)| {
                    Self {
                        identifier,
                        generic_parameters,
                        parameters,
                        return_type,
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    pub access_modifier: AccessModifier,
    pub is_unsafe: bool,
    pub is_const: bool,
    pub signature: Signature,
    pub body: Body<Statement>,
}

impl Input<&super::Function> for &Function {
    fn assert(self, output: &super::Function) -> TestCaseResult {
        self.access_modifier.assert(&output.access_modifier)?;
        prop_assert_eq!(self.is_unsafe, output.unsafe_keyword.is_some());
        prop_assert_eq!(self.is_const, output.const_keyword.is_some());
        self.signature.assert(&output.signature)?;
        self.body.assert(&output.body)
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
                |(access_modifier, is_unsafe, is_const, signature, body)| {
                    Self {
                        access_modifier,
                        is_unsafe,
                        is_const,
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

        if self.is_unsafe {
            write!(formatter, "unsafe ")?;
        }

        if self.is_const {
            write!(formatter, "const ")?;
        }

        self.signature.indent_fmt(formatter, indent)?;
        self.body.indent_fmt(formatter, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter {
    pub irrefutable_pattern: Irrefutable,
    pub r#type: Type,
}

impl Input<&super::Parameter> for &Parameter {
    fn assert(self, output: &super::Parameter) -> TestCaseResult {
        self.irrefutable_pattern.assert(&output.irrefutable_pattern)?;
        self.r#type.assert(&output.r#type)
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
        Display::fmt(&self.irrefutable_pattern, f)?;
        f.write_str(": ")?;
        self.r#type.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParameterKind {
    Regular(Parameter),
    VarArgs,
}

impl Input<&super::ParameterKind> for &ParameterKind {
    fn assert(self, output: &super::ParameterKind) -> TestCaseResult {
        match (self, output) {
            (ParameterKind::Regular(i), super::ParameterKind::Regular(o)) => {
                i.assert(o)
            }
            (ParameterKind::VarArgs, super::ParameterKind::VarArgs(_)) => {
                Ok(())
            }

            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for ParameterKind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            6 => Parameter::arbitrary().prop_map(Self::Regular),
            1 => Just(Self::VarArgs),
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
            Self::VarArgs => write!(f, "..."),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameters {
    pub parameter_list:
        Option<ConnectedList<ParameterKind, ConstantPunctuation<','>>>,
}

impl Input<&super::Parameters> for &Parameters {
    fn assert(self, output: &super::Parameters) -> TestCaseResult {
        self.parameter_list.as_ref().assert(output.connected_list.as_ref())
    }
}

impl Arbitrary for Parameters {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            ParameterKind::arbitrary(),
            ConstantPunctuation::arbitrary(),
        ))
        .prop_map(|parameter_list| Self { parameter_list })
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
        if let Some(parameter_list) = &self.parameter_list {
            parameter_list.indent_fmt(f, indent)?;
        }
        f.write_char(')')
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReturnType {
    pub r#type: Type,
}

impl Input<&super::ReturnType> for &ReturnType {
    fn assert(self, output: &super::ReturnType) -> TestCaseResult {
        self.r#type.assert(&output.r#type)
    }
}

impl Arbitrary for ReturnType {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Type::arbitrary().prop_map(|ty| Self { r#type: ty }).boxed()
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
