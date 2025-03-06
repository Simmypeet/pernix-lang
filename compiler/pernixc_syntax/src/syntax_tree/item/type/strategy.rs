use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy},
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    self,
    item::{
        generic_parameter::strategy::GenericParameters,
        where_clause::strategy::WhereClause,
    },
    strategy::{
        write_indent_line_for_indent_display, AccessModifier, Identifier,
        IndentDisplay,
    },
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Type {
    access_modifier: AccessModifier,
    signature: Signature,
    r#type: syntax_tree::r#type::strategy::Type,
    where_clause: Option<WhereClause>,
}

impl Input<&super::Type> for &Type {
    fn assert(self, output: &super::Type) -> TestCaseResult {
        self.access_modifier.assert(&output.access_modifier)?;
        self.signature.assert(&output.signature)?;
        self.r#type.assert(&output.body.r#type)?;
        self.where_clause.as_ref().assert(
            output.trailing_where_clause.as_ref().map(|x| &x.where_clause),
        )
    }
}

impl Arbitrary for Type {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            Signature::arbitrary(),
            syntax_tree::r#type::strategy::Type::arbitrary(),
            proptest::option::of(WhereClause::arbitrary()),
        )
            .prop_map(|(access_modifier, signature, r#type, where_clause)| {
                Self { access_modifier, signature, r#type, where_clause }
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
        formatter.write_str(" = ")?;
        self.r#type.indent_fmt(formatter, indent)?;

        if let Some(where_clause) = &self.where_clause {
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
