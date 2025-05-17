use pernixc_lexical::kind::arbitrary::Identifier;
use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy as _};

use crate::{
    arbitrary::{AccessModifier, IndentDisplay},
    item::{
        arbitrary::TrailingWhereClause,
        generic_parameters::arbitrary::GenericParameters,
    },
    r#type::arbitrary::Type as TypeTerm,
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
        write!(formatter, "type {}", self.identifier)?;

        if let Some(generic_parameters) = &self.generic_parameters {
            generic_parameters.indent_fmt(formatter, indent)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Body for super::Body {
        pub r#type (TypeTerm),
        pub trailing_where_clause (Option<TrailingWhereClause>),
    }
}

impl Arbitrary for Body {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            TypeTerm::arbitrary(),
            proptest::option::of(TrailingWhereClause::arbitrary()),
        )
            .prop_map(|(r#type, trailing_where_clause)| Self {
                r#type,
                trailing_where_clause,
            })
            .boxed()
    }
}

impl IndentDisplay for Body {
    fn indent_fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        formatter.write_str(" = ")?;
        self.r#type.indent_fmt(formatter, indent)?;

        if let Some(trailing_where_clause) = &self.trailing_where_clause {
            trailing_where_clause.indent_fmt(formatter, indent)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Type for super::Type {
        pub access_modifier (AccessModifier),
        pub signature (Signature),
        pub body (Body),
    }
}

impl Arbitrary for Type {
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

impl IndentDisplay for Type {
    fn indent_fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(formatter, "{} ", self.access_modifier)?;
        self.signature.indent_fmt(formatter, indent)?;
        self.body.indent_fmt(formatter, indent)
    }
}
