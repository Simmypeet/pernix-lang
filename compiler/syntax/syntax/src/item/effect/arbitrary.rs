use pernixc_lexical::kind::arbitrary::Identifier;
use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy as _};

use crate::{
    arbitrary::IndentDisplay,
    item::{function, generic_parameters::arbitrary::GenericParameters},
    reference,
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
        formatter: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(formatter, "effect {}", self.identifier)?;

        if let Some(generic_parameters) = &self.generic_parameters {
            generic_parameters.indent_fmt(formatter, indent)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Operation for super::Operation {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Identifier),
        pub generic_parameters (Option<GenericParameters>),
        pub parameters (function::arbitrary::Parameters),
        pub return_type (Option<function::arbitrary::ReturnType>),
    }
}

impl Arbitrary for Operation {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(GenericParameters::arbitrary()),
            function::arbitrary::Parameters::arbitrary(),
            proptest::option::of(function::arbitrary::ReturnType::arbitrary()),
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

impl IndentDisplay for Operation {
    fn indent_fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(formatter, "do {}", self.identifier)?;

        if let Some(generic_parameters) = &self.generic_parameters {
            generic_parameters.indent_fmt(formatter, indent)?;
        }

        self.parameters.indent_fmt(formatter, indent)?;

        if let Some(return_type) = &self.return_type {
            write!(formatter, " ")?;
            return_type.indent_fmt(formatter, indent)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Effect for super::Effect {
        pub access_modifier (crate::arbitrary::AccessModifier),
        pub signature (Signature),
        pub body (crate::item::arbitrary::Body<Operation>),
    }
}

impl Arbitrary for Effect {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            crate::arbitrary::AccessModifier::arbitrary(),
            Signature::arbitrary(),
            crate::item::arbitrary::Body::<Operation>::arbitrary(),
        )
            .prop_map(|(access_modifier, signature, body)| Self {
                access_modifier,
                signature,
                body,
            })
            .boxed()
    }
}

impl IndentDisplay for Effect {
    fn indent_fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(formatter, "{} ", self.access_modifier)?;
        self.signature.indent_fmt(formatter, indent)?;
        self.body.indent_fmt(formatter, indent)?;

        Ok(())
    }
}
