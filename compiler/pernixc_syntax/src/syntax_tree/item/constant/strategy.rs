use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy},
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    expression::{strategy::Expression, terminator::strategy::Terminator},
    item::{
        generic_parameter::strategy::GenericParameters,
        where_clause::strategy::WhereClause,
    },
    r#type::strategy::Type,
    strategy::{
        write_indent_line_for_indent_display, AccessModifier, Identifier,
        IndentDisplay,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
    pub r#type: Type,
}

impl Input<&super::Signature> for &Signature {
    fn assert(self, output: &super::Signature) -> TestCaseResult {
        self.identifier.assert(&output.identifier)?;
        self.generic_parameters
            .as_ref()
            .assert(output.generic_parameters.as_ref())?;
        self.r#type.assert(&output.r#type)
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

        write!(formatter, ": ")?;

        self.r#type.indent_fmt(formatter, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constant {
    access_modifier: AccessModifier,
    signature: Signature,
    body: Body,
}

impl Input<&super::Constant> for &Constant {
    fn assert(self, output: &super::Constant) -> TestCaseResult {
        self.access_modifier.assert(&output.access_modifier)?;
        self.signature.assert(&output.signature)?;
        self.body.assert(&output.body)
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Body {
    pub expression: Expression,
    pub trailing_where_clause: Option<WhereClause>,
}

impl Input<&super::Body> for &Body {
    fn assert(self, output: &super::Body) -> TestCaseResult {
        self.expression.assert(&output.expression)?;
        self.trailing_where_clause.as_ref().assert(
            output.trailing_where_clause.as_ref().map(|x| &x.where_clause),
        )
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
                            .map_or_else(|| &binary.first, |x| &x.1);

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
                            .map_or_else(|| &binary.first, |x| &x.1);

                        node.is_block()
                    }
                },
            ),
            proptest::option::of(WhereClause::arbitrary()),
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

        if let Some(where_clause) = &self.trailing_where_clause {
            writeln!(formatter, ":")?;
            write_indent_line_for_indent_display(
                formatter,
                where_clause,
                indent + 1,
            )?;
        }

        Ok(())
    }
}
