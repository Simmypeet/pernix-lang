use pernixc_lexical::kind::arbitrary::Identifier;
use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy as _};

use crate::{
    arbitrary::{AccessModifier, IndentDisplay},
    expression::{arbitrary::Expression, terminator::arbitrary::Terminator},
    item::{
        arbitrary::TrailingWhereClause,
        generic_parameters::arbitrary::GenericParameters,
    },
    reference,
    r#type::arbitrary::Type,
};

reference! {
    #[derive(Debug, Clone)]
    pub struct Signature for super::Signature {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Identifier),
        pub generic_parameters (Option<GenericParameters>),
        pub r#type (Type),
    }
}

impl Arbitrary for Signature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(GenericParameters::arbitrary()),
            Type::arbitrary(),
        )
            .prop_map(|(identifier, generic_parameters, r#type)| Self {
                identifier,
                generic_parameters,
                r#type,
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
        write!(formatter, "const {}", self.identifier)?;

        if let Some(generic_parameters) = &self.generic_parameters {
            generic_parameters.indent_fmt(formatter, indent)?;
        }

        formatter.write_str(": ")?;
        self.r#type.indent_fmt(formatter, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Body for super::Body {
        pub expression (Expression),
        pub trailing_where_clause (Option<TrailingWhereClause>),
    }
}

impl Arbitrary for Body {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            Expression::arbitrary().prop_filter(
                "filter block appear last",
                |x| !match x {
                    Expression::Binary(binary) => {
                        let node = binary
                            .chain
                            .last()
                            .map_or_else(|| &binary.first, |x| &x.node);

                        node.is_block()
                    }
                    Expression::Terminator(terminator) => {
                        let binary = match terminator {
                            Terminator::Return(a) => a.binary.as_ref(),
                            Terminator::Continue(_) => None,
                            Terminator::Express(a) => a.binary.as_ref(),
                            Terminator::Break(a) => a.binary.as_ref(),
                        };

                        let Some(binary) = binary else {
                            return true;
                        };

                        let node = binary
                            .chain
                            .last()
                            .map_or_else(|| &binary.first, |x| &x.node);

                        node.is_block()
                    }
                },
            ),
            proptest::option::of(TrailingWhereClause::arbitrary()),
        )
            .prop_map(|(expression, trailing_where_clause)| Self {
                expression,
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
        self.expression.indent_fmt(formatter, indent)?;

        if let Some(trailing_where_clause) = &self.trailing_where_clause {
            trailing_where_clause.indent_fmt(formatter, indent)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Constant for super::Constant {
        pub access_modifier (AccessModifier),
        pub signature (Signature),
        pub body (Body),
    }
}

impl Arbitrary for Constant {
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

impl IndentDisplay for Constant {
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
