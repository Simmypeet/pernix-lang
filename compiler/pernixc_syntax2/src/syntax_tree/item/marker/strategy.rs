use pernixc_test_input::Input;
use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy};

use crate::syntax_tree::{
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
    identifier: Identifier,
    generic_parameters: Option<GenericParameters>,
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

impl Input<&super::Signature> for &Signature {
    fn assert(
        self,
        output: &super::Signature,
    ) -> proptest::test_runner::TestCaseResult {
        self.identifier.assert(&output.identifier)?;
        self.generic_parameters
            .as_ref()
            .assert(output.generic_parameters.as_ref())
    }
}

impl IndentDisplay for Signature {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "marker {}", self.identifier)?;

        if let Some(generic_parameters) = self.generic_parameters.as_ref() {
            generic_parameters.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Marker {
    pub access_modifier: AccessModifier,
    pub signature: Signature,
    pub trailing_where_clause: Option<WhereClause>,
}

impl Input<&super::Marker> for &Marker {
    fn assert(
        self,
        output: &super::Marker,
    ) -> proptest::test_runner::TestCaseResult {
        self.access_modifier.assert(&output.access_modifier)?;
        self.signature.assert(&output.signature)?;
        self.trailing_where_clause.as_ref().assert(
            output.trailing_where_clause.as_ref().map(|x| &x.where_clause),
        )
    }
}

impl Arbitrary for Marker {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            Signature::arbitrary(),
            proptest::option::of(WhereClause::arbitrary()),
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
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "{} ", self.access_modifier)?;
        self.signature.indent_fmt(f, indent)?;

        if let Some(where_clause) = self.trailing_where_clause.as_ref() {
            writeln!(f, ":")?;
            write_indent_line_for_indent_display(f, where_clause, indent + 1)?;
        }

        Ok(())
    }
}
