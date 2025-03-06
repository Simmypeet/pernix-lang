use std::fmt::Write;

use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy},
    prop_oneof,
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    item::{
        generic_parameter::strategy::GenericParameters,
        where_clause::strategy::WhereClause,
    },
    r#type::strategy::Type,
    strategy::{
        write_indent_line_for_indent_display, AccessModifier, Identifier,
        IndentDisplay, Passable,
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
pub struct Body {
    pub where_clause: Option<WhereClause>,
    pub variant_list: Vec<Passable<Variant>>,
}

impl Input<&super::Body> for &Body {
    fn assert(self, output: &super::Body) -> TestCaseResult {
        self.where_clause.as_ref().assert(output.where_clause.as_ref())?;
        self.variant_list.assert(&output.variants)
    }
}

impl Arbitrary for Body {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let variant = prop_oneof![
            4 => Variant::arbitrary().prop_map(Passable::SyntaxTree),
            1 => Just(Passable::Pass),
        ];

        (
            proptest::option::of(WhereClause::arbitrary()),
            proptest::collection::vec(variant, 1..=10),
        )
            .prop_map(|(where_clause, variant_list)| Self {
                where_clause,
                variant_list,
            })
            .boxed()
    }
}

impl IndentDisplay for Body {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        writeln!(f, ":")?;

        if let Some(where_clause) = self.where_clause.as_ref() {
            write_indent_line_for_indent_display(f, where_clause, indent + 1)?;
        }

        for variant in &self.variant_list {
            write_indent_line_for_indent_display(f, variant, indent + 1)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum {
    pub access_modifier: AccessModifier,
    pub signature: Signature,
    pub body: Body,
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
