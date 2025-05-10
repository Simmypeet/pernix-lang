use std::fmt::{Debug, Write};

use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy, TestCaseError},
    prop_oneof,
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    item::{
        self, constant, function,
        generic_parameter::strategy::GenericParameters, r#type,
        where_clause::strategy::WhereClause,
    },
    statement::strategy::Statement,
    strategy::{
        write_indent_line_for_indent_display, AccessModifier, IndentDisplay,
        QualifiedIdentifier,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub is_final: bool,
    pub generic_parameters: Option<GenericParameters>,
    pub qualified_identifier: QualifiedIdentifier,
}

impl Arbitrary for Signature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            bool::arbitrary(),
            proptest::option::of(GenericParameters::arbitrary()),
            QualifiedIdentifier::arbitrary(),
        )
            .prop_map(|(is_final, generic_parameters, qualified_identifier)| {
                Self { is_final, generic_parameters, qualified_identifier }
            })
            .boxed()
    }
}

impl Input<&super::Signature> for &Signature {
    fn assert(self, output: &super::Signature) -> TestCaseResult {
        assert_eq!(self.is_final, output.final_keyword.is_some());
        self.generic_parameters
            .as_ref()
            .assert(output.generic_parameters.as_ref())?;
        self.qualified_identifier.assert(&output.qualified_identifier)
    }
}

impl IndentDisplay for Signature {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if self.is_final {
            write!(f, "final ")?;
        }

        write!(f, "implements")?;

        if let Some(generic_parameters) = &self.generic_parameters {
            generic_parameters.indent_fmt(f, indent)?;
        }

        f.write_char(' ')?;
        self.qualified_identifier.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberTemplate<S, B> {
    pub access_modifier: Option<AccessModifier>,
    pub signature: S,
    pub body: B,
}

impl<So: Debug, Bo: Debug, S: Debug, B: Debug>
    Input<&super::MemberTemplate<So, Bo>> for &MemberTemplate<S, B>
where
    for<'x, 'y> &'x S: Input<&'y So>,
    for<'x, 'y> &'x B: Input<&'y Bo>,
{
    fn assert(self, output: &super::MemberTemplate<So, Bo>) -> TestCaseResult {
        self.access_modifier
            .as_ref()
            .assert(output.access_modifier.as_ref())?;
        self.signature.assert(&output.signature)?;
        self.body.assert(&output.body)
    }
}

impl<S: IndentDisplay, B: IndentDisplay> IndentDisplay
    for MemberTemplate<S, B>
{
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if let Some(access_modifier) = &self.access_modifier {
            write!(f, "{access_modifier} ")?;
        }

        self.signature.indent_fmt(f, indent)?;
        self.body.indent_fmt(f, indent)
    }
}

impl<S: Arbitrary + 'static, B: Arbitrary + 'static> Arbitrary
    for MemberTemplate<S, B>
where
    S::Strategy: 'static,
    B::Strategy: 'static,
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(AccessModifier::arbitrary()),
            S::arbitrary(),
            B::arbitrary(),
        )
            .prop_map(|(access_modifier, signature, body)| Self {
                access_modifier,
                signature,
                body,
            })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionSignature {
    pub is_unsafe: bool,
    pub is_const: bool,
    pub signature: function::strategy::Signature,
}

impl Arbitrary for FunctionSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            bool::arbitrary(),
            bool::arbitrary(),
            function::strategy::Signature::arbitrary(),
        )
            .prop_map(|(is_unsafe, is_const, signature)| Self {
                is_unsafe,
                is_const,
                signature,
            })
            .boxed()
    }
}

impl Input<&super::FunctionSignature> for &FunctionSignature {
    fn assert(self, output: &super::FunctionSignature) -> TestCaseResult {
        assert_eq!(self.is_unsafe, output.unsafe_keyword.is_some());
        assert_eq!(self.is_const, output.const_keyword.is_some());
        self.signature.assert(&output.signature)
    }
}

impl IndentDisplay for FunctionSignature {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if self.is_unsafe {
            write!(f, "unsafe ")?;
        }

        if self.is_const {
            write!(f, "const ")?;
        }

        self.signature.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Member {
    Constant(
        MemberTemplate<constant::strategy::Signature, constant::strategy::Body>,
    ),
    Function(
        MemberTemplate<FunctionSignature, item::strategy::Body<Statement>>,
    ),
    Type(MemberTemplate<r#type::strategy::Signature, r#type::strategy::Body>),
}

impl Input<&super::Member> for &Member {
    fn assert(self, output: &super::Member) -> TestCaseResult {
        match (self, output) {
            (Member::Constant(a), super::Member::Constant(b)) => a.assert(b),
            (Member::Function(a), super::Member::Function(b)) => a.assert(b),
            (Member::Type(a), super::Member::Type(b)) => a.assert(b),
            _ => Err(TestCaseError::fail(format!(
                "Expected {output:?}, got {self:?}",
            ))),
        }
    }
}

impl Arbitrary for Member {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            1 => MemberTemplate::arbitrary()
                .prop_map(Member::Constant),
            1 => MemberTemplate::arbitrary()
                .prop_map(Member::Function),
            1 => MemberTemplate::arbitrary()
                .prop_map(Member::Type),
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
            Self::Constant(member) => member.indent_fmt(f, indent),
            Self::Function(member) => member.indent_fmt(f, indent),
            Self::Type(member) => member.indent_fmt(f, indent),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NegativeBody {
    pub trailing_where_clause: Option<WhereClause>,
}

impl Input<&super::NegativeBody> for &NegativeBody {
    fn assert(self, output: &super::NegativeBody) -> TestCaseResult {
        self.trailing_where_clause.as_ref().assert(
            output.trailing_where_clause.as_ref().map(|x| &x.where_clause),
        )
    }
}

impl Arbitrary for NegativeBody {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::option::of(WhereClause::arbitrary())
            .prop_map(|trailing_where_clause| Self { trailing_where_clause })
            .boxed()
    }
}

impl IndentDisplay for NegativeBody {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str(" delete")?;

        if let Some(trailing_where_clause) = &self.trailing_where_clause {
            writeln!(f, ":")?;
            write_indent_line_for_indent_display(
                f,
                trailing_where_clause,
                indent + 1,
            )?;
        }

        Ok(())
    }
}

pub type PositiveBody = item::strategy::Body<Member>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Body {
    Positive(PositiveBody),
    Negative(NegativeBody),
}

impl Input<&super::Body> for &Body {
    fn assert(self, output: &super::Body) -> TestCaseResult {
        match (self, output) {
            (Body::Positive(a), super::Body::Positive(b)) => a.assert(b),
            (Body::Negative(a), super::Body::Negative(b)) => a.assert(b),
            _ => Err(TestCaseError::fail(format!(
                "Expected {output:?}, got {self:?}",
            ))),
        }
    }
}

impl Arbitrary for Body {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            4 => PositiveBody::arbitrary().prop_map(Body::Positive),
            1 => NegativeBody::arbitrary().prop_map(Body::Negative),
        ]
        .boxed()
    }
}

impl IndentDisplay for Body {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Positive(body) => body.indent_fmt(f, indent),
            Self::Negative(body) => body.indent_fmt(f, indent),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Implements {
    pub signature: Signature,
    pub body: Body,
}

impl Input<&super::Implements> for &Implements {
    fn assert(self, output: &super::Implements) -> TestCaseResult {
        self.signature.assert(&output.signature)?;
        self.body.assert(&output.body)
    }
}

impl Arbitrary for Implements {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (Signature::arbitrary(), Body::arbitrary())
            .prop_map(|(signature, body)| Self { signature, body })
            .boxed()
    }
}

impl IndentDisplay for Implements {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.signature.indent_fmt(f, indent)?;
        self.body.indent_fmt(f, indent)
    }
}
