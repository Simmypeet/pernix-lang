use std::fmt::Write;

use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy},
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    item::{generic_parameter::strategy::GenericParameters, strategy::Body},
    r#type::strategy::Type,
    strategy::{AccessModifier, Identifier, IndentDisplay},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
}

impl Input<&super::Signature> for &Signature {
    fn assert(self, output: &super::Signature) -> TestCaseResult {
        self.identifier.assert(&output.identifier)?;
        self.generic_parameters
            .as_ref()
            .assert(output.generic_parameters.as_ref())
    }
}

impl Arbitrary for Signature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(GenericParameters::arbitrary()),
        )
            .prop_map(|(identifier, generic_parameters)| Self {
                identifier,
                generic_parameters,
            })
            .boxed()
    }
}

impl IndentDisplay for Signature {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "enum {}", self.identifier)?;

        if let Some(generic_parameters) = self.generic_parameters.as_ref() {
            generic_parameters.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    pub identifier: Identifier,
    pub association: Option<Type>,
}

impl Input<&super::Variant> for &Variant {
    fn assert(self, output: &super::Variant) -> TestCaseResult {
        self.identifier.assert(&output.identifier)?;
        self.association
            .as_ref()
            .assert(output.association.as_ref().map(|x| &x.tree))
    }
}

impl Arbitrary for Variant {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (Identifier::arbitrary(), proptest::option::of(Type::arbitrary()))
            .prop_map(|(identifier, variant_association)| Self {
                identifier,
                association: variant_association,
            })
            .boxed()
    }
}

impl IndentDisplay for Variant {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "{}", self.identifier)?;

        if let Some(variant_association) = self.association.as_ref() {
            f.write_char('(')?;
            variant_association.indent_fmt(f, indent)?;
            f.write_char(')')?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum {
    pub access_modifier: AccessModifier,
    pub signature: Signature,
    pub body: Body<Variant>,
}

impl Input<&super::Enum> for &Enum {
    fn assert(self, output: &super::Enum) -> TestCaseResult {
        self.access_modifier.assert(&output.access_modifier)?;
        self.signature.assert(&output.signature)?;
        self.body.assert(&output.body)
    }
}

impl Arbitrary for Enum {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (AccessModifier::arbitrary(), Signature::arbitrary(), Body::arbitrary())
            .prop_map(|(access_modifier, signature, body)| Self {
                access_modifier,
                signature,
                body,
            })
            .boxed()
    }
}

impl IndentDisplay for Enum {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "{} ", self.access_modifier)?;
        self.signature.indent_fmt(f, indent)?;
        self.body.indent_fmt(f, indent)
    }
}
