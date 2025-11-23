use std::fmt::Write;

use pernixc_lexical::kind::arbitrary::Identifier;
use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy as _};

use crate::{
    arbitrary::{AccessModifier, IndentDisplay},
    item::{arbitrary::Body, generic_parameters::arbitrary::GenericParameters},
    reference,
    r#type::arbitrary::Type,
};

reference! {
    #[derive(Debug, Clone)]
    pub struct Signature for super::Signature {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Identifier),
        pub generic_parameters (Option<GenericParameters>),
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

reference! {
    #[derive(Debug, Clone )]
    pub struct VariantAssociation for super::VariantAssociation {
        pub r#type (Type),
    }
}

impl Arbitrary for VariantAssociation {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Type::arbitrary().prop_map(|r#type| Self { r#type }).boxed()
    }
}

impl IndentDisplay for VariantAssociation {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_char('(')?;
        self.r#type.indent_fmt(f, indent)?;
        f.write_char(')')
    }
}

reference! {
    #[derive(Debug, Clone )]
    pub struct Variant for super::Variant {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Identifier),
        pub association (Option<VariantAssociation>),
    }
}

impl Arbitrary for Variant {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(VariantAssociation::arbitrary()),
        )
            .prop_map(|(identifier, association)| Self {
                identifier,
                association,
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

        if let Some(association) = self.association.as_ref() {
            association.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Enum for super::Enum {
        pub access_modifier (AccessModifier),
        pub signature (Signature),
        pub body (Body<Variant>),
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
