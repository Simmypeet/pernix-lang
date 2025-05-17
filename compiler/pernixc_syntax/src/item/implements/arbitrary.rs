use std::fmt::Write as _;

use pernixc_parser::abstract_tree::AbstractTree;
use pernixc_test_input::Input;
use proptest::{prelude::*, test_runner::TestCaseResult};

use crate::{
    arbitrary::{AccessModifier, IndentDisplay, QualifiedIdentifier},
    item::{
        self, arbitrary::TrailingWhereClause, constant, function,
        generic_parameters::arbitrary::GenericParameters, r#type,
    },
    reference,
    statement::arbitrary::Statement,
};

reference! {
    #[derive(Debug, Clone)]
    pub struct Signature for super::Signature {
        pub final_keyword (bool),
        pub const_keyword (bool),
        pub generic_parameters (Option<GenericParameters>),
        pub qualified_identifier (QualifiedIdentifier)
    }
}

impl Arbitrary for Signature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            bool::arbitrary(),
            bool::arbitrary(),
            proptest::option::of(GenericParameters::arbitrary()),
            QualifiedIdentifier::arbitrary(),
        )
            .prop_map(
                |(
                    final_keyword,
                    const_keyword,
                    generic_parameters,
                    qualified_identifier,
                )| {
                    Self {
                        final_keyword,
                        const_keyword,
                        generic_parameters,
                        qualified_identifier,
                    }
                },
            )
            .boxed()
    }
}

impl IndentDisplay for Signature {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if self.final_keyword {
            write!(f, "final ")?;
        }

        if self.const_keyword {
            write!(f, "const ")?;
        }

        write!(f, "implements")?;

        if let Some(generic_parameters) = &self.generic_parameters {
            generic_parameters.indent_fmt(f, indent)?;
        }

        f.write_char(' ')?;
        self.qualified_identifier.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone)]
pub struct MemberTemplate<S, B> {
    pub access_modifier: Option<AccessModifier>,
    pub signature: S,
    pub body: B,
}

impl<
        So: std::fmt::Debug + AbstractTree + 'static,
        Bo: std::fmt::Debug + AbstractTree + 'static,
        S: std::fmt::Debug,
        B: std::fmt::Debug,
    > Input<&super::MemberTemplate<So, Bo>, ()> for &MemberTemplate<S, B>
where
    for<'x, 'y> &'x S: Input<&'y So, ()>,
    for<'x, 'y> &'x B: Input<&'y Bo, ()>,
{
    fn assert(
        self,
        output: &super::MemberTemplate<So, Bo>,
        (): (),
    ) -> TestCaseResult {
        self.access_modifier
            .as_ref()
            .assert(output.access_modifier().as_ref(), ())?;

        Some(&self.signature).assert(output.signature().as_ref(), ())?;
        Some(&self.body).assert(output.body().as_ref(), ())
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

reference! {
    #[derive(Debug, Clone)]
    pub struct FunctionSignature for super::FunctionSignature {
        pub unsafe_keyword (bool),
        pub const_keyword (bool),
        pub signature (function::arbitrary::Signature),
    }
}

impl Arbitrary for FunctionSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            bool::arbitrary(),
            bool::arbitrary(),
            function::arbitrary::Signature::arbitrary(),
        )
            .prop_map(|(unsafe_keyword, const_keyword, signature)| Self {
                unsafe_keyword,
                const_keyword,
                signature,
            })
            .boxed()
    }
}

impl IndentDisplay for FunctionSignature {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if self.unsafe_keyword {
            write!(f, "unsafe ")?;
        }

        if self.const_keyword {
            write!(f, "const ")?;
        }

        self.signature.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub enum Member for super::Member {
        Constant(
            MemberTemplate<
                constant::arbitrary::Signature,
                constant::arbitrary::Body,
            >
        ),
        Function(
            MemberTemplate<FunctionSignature, item::arbitrary::Body<Statement>>
        ),
        Type(
            MemberTemplate<
                r#type::arbitrary::Signature,
                r#type::arbitrary::Body
            >
        ),
    }
}

impl Arbitrary for Member {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            MemberTemplate::arbitrary().prop_map(Member::Constant),
            MemberTemplate::arbitrary().prop_map(Member::Function),
            MemberTemplate::arbitrary().prop_map(Member::Type),
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

reference! {
    #[derive(Debug, Clone)]
    pub struct NegativeBody for super::NegativeBody {
        pub trailing_where_clause (Option<TrailingWhereClause>)
    }
}

impl Arbitrary for NegativeBody {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::option::of(TrailingWhereClause::arbitrary())
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
            trailing_where_clause.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}

pub type PositiveBody = item::arbitrary::Body<Member>;

reference! {
    #[derive(Debug, Clone)]
    pub enum Body for super::Body {
        Positive(PositiveBody),
        Negative(NegativeBody),
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

reference! {
    #[derive(Debug, Clone)]
    pub struct Implements for super::Implements {
        pub signature (Signature),
        pub body (Body),
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
