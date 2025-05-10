use std::fmt::Debug;

use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy, TestCaseError},
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    item::{
        constant, function, generic_parameter::strategy::GenericParameters,
        r#type, strategy::Body, where_clause::strategy::WhereClause,
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
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "trait {}", self.identifier)?;

        if let Some(generic_parameters) = self.generic_parameters.as_ref() {
            generic_parameters.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberTemplate<T> {
    pub access_modifier: AccessModifier,
    pub signature: T,
    pub trailing_where_clause: Option<WhereClause>,
}

impl<T: Debug, O: Debug> Input<&super::MemberTemplate<O>> for &MemberTemplate<T>
where
    for<'x, 'y> &'x T: Input<&'y O>,
{
    fn assert(self, output: &super::MemberTemplate<O>) -> TestCaseResult {
        self.access_modifier.assert(&output.access_modifier)?;
        self.signature.assert(&output.signature)?;
        self.trailing_where_clause.as_ref().assert(
            output.trailing_where_clause.as_ref().map(|x| &x.where_clause),
        )
    }
}

impl<T: Arbitrary + 'static> Arbitrary for MemberTemplate<T>
where
    T::Strategy: 'static,
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            T::arbitrary(),
            proptest::option::of(WhereClause::arbitrary()),
        )
            .prop_map(|(access_modifier, signature, trailing_where_clause)| {
                Self { access_modifier, signature, trailing_where_clause }
            })
            .boxed()
    }
}

impl<T: IndentDisplay> IndentDisplay for MemberTemplate<T> {
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Member {
    Function(MemberTemplate<function::strategy::Signature>),
    Type(MemberTemplate<r#type::strategy::Signature>),
    Constant(MemberTemplate<constant::strategy::Signature>),
}

impl Input<&super::Member> for &Member {
    fn assert(self, output: &super::Member) -> TestCaseResult {
        match (self, output) {
            (Member::Function(self_), super::Member::Function(output)) => {
                self_.assert(output)
            }
            (Member::Type(self_), super::Member::Type(output)) => {
                self_.assert(output)
            }
            (Member::Constant(self_), super::Member::Constant(output)) => {
                self_.assert(output)
            }
            _ => Err(TestCaseError::fail(format!(
                "Expected {output:?}, got {self:?}"
            ))),
        }
    }
}

impl Arbitrary for Member {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::prop_oneof![
            4 => MemberTemplate::<function::strategy::Signature>::arbitrary()
                .prop_map(Member::Function),
            1 => MemberTemplate::<r#type::strategy::Signature>::arbitrary()
                .prop_map(Member::Type),
            1 => MemberTemplate::<constant::strategy::Signature>::arbitrary()
                .prop_map(Member::Constant),
        ]
        .boxed()
    }
}

impl IndentDisplay for Member {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Function(member) => member.indent_fmt(f, indent),
            Self::Type(member) => member.indent_fmt(f, indent),
            Self::Constant(member) => member.indent_fmt(f, indent),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trait {
    pub access_modifier: AccessModifier,
    pub signature: Signature,
    pub body: Body<Member>,
}

impl Input<&super::Trait> for &Trait {
    fn assert(self, output: &super::Trait) -> TestCaseResult {
        self.access_modifier.assert(&output.access_modifier)?;
        self.signature.assert(&output.signature)?;
        self.body.assert(&output.body)
    }
}

impl Arbitrary for Trait {
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

impl IndentDisplay for Trait {
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
