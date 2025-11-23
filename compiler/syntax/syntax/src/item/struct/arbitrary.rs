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
        write!(f, "struct {}", self.identifier)?;

        if let Some(generic_parameters) = self.generic_parameters.as_ref() {
            generic_parameters.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone )]
    pub struct Field for super::Field {
        pub access_modifier (AccessModifier),
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Identifier),
        pub r#type (Type),
    }
}

impl Arbitrary for Field {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            Identifier::arbitrary(),
            Type::arbitrary(),
        )
            .prop_map(|(access_modifier, identifier, r#type)| Self {
                access_modifier,
                identifier,
                r#type,
            })
            .boxed()
    }
}

impl IndentDisplay for Field {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "{} {}: ", self.access_modifier, self.identifier)?;

        self.r#type.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Struct for super::Struct {
        pub access_modifier (AccessModifier),
        pub signature (Signature),
        pub body (Body<Field>),
    }
}

impl Arbitrary for Struct {
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

impl IndentDisplay for Struct {
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
