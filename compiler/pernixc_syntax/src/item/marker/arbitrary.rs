use pernixc_lexical::kind::arbitrary::Identifier;
use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy as _};

use crate::{
    arbitrary::{AccessModifier, IndentDisplay},
    item::{
        arbitrary::TrailingWhereClause,
        generic_parameters::arbitrary::GenericParameters,
    },
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
        write!(formatter, "marker {}", self.identifier)?;

        if let Some(generic_parameters) = self.generic_parameters.as_ref() {
            generic_parameters.indent_fmt(formatter, indent)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Marker for super::Marker {
        pub access_modifier (AccessModifier),
        pub signature (Signature),
        pub trailing_where_clause (Option<TrailingWhereClause>),
    }
}

impl Arbitrary for Marker {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            Signature::arbitrary(),
            proptest::option::of(TrailingWhereClause::arbitrary()),
        )
            .prop_map(|(access_modifier, signature, trailing_where_clause)| {
                Self { access_modifier, signature, trailing_where_clause }
            })
            .boxed()
    }
}

impl IndentDisplay for Marker {
    fn indent_fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(formatter, "{} ", self.access_modifier)?;
        self.signature.indent_fmt(formatter, indent)?;

        if let Some(trailing_where_clause) = &self.trailing_where_clause {
            trailing_where_clause.indent_fmt(formatter, indent)?;
        }

        Ok(())
    }
}
