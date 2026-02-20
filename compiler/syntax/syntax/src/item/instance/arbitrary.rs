use pernixc_lexical::kind::arbitrary::Identifier;
use pernixc_parser::abstract_tree::AbstractTree;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy as _},
    prop_oneof,
};

use crate::{
    arbitrary::{IndentDisplay, QualifiedIdentifier},
    item::{
        self,
        arbitrary::{Body, HigherRankedLifetimes},
        constant, function,
        generic_parameters::arbitrary::GenericParameters,
        r#type,
    },
    reference,
    statement::arbitrary::Statement,
};

reference! {
    #[derive(Debug, Clone)]
    pub struct TraitRef for super::super::TraitRef {
        pub higher_ranked_lifetimes (Option<HigherRankedLifetimes>),
        pub qualified_identifier (QualifiedIdentifier),
    }
}

impl Arbitrary for TraitRef {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(HigherRankedLifetimes::arbitrary()),
            QualifiedIdentifier::arbitrary(),
        )
            .prop_map(|(higher_ranked_lifetimes, qualified_identifier)| Self {
                higher_ranked_lifetimes,
                qualified_identifier,
            })
            .boxed()
    }
}

impl IndentDisplay for TraitRef {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            write!(f, "{higher_ranked_lifetimes} ")?;
        }

        self.qualified_identifier.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Signature for super::Signature {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Identifier),
        pub generic_parameters (Option<GenericParameters>),
        pub trait_ref (TraitRef),
    }
}

impl Arbitrary for Signature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(GenericParameters::arbitrary()),
            TraitRef::arbitrary(),
        )
            .prop_map(|(identifier, generic_parameters, trait_ref)| Self {
                identifier,
                generic_parameters,
                trait_ref,
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
        write!(formatter, "instance {}", self.identifier)?;

        if let Some(generic_parameters) = &self.generic_parameters {
            generic_parameters.indent_fmt(formatter, indent)?;
        }

        formatter.write_str(": ")?;
        self.trait_ref.indent_fmt(formatter, indent)
    }
}

#[derive(Debug, Clone)]
pub struct MemberTemplate<S, B> {
    pub signature: S,
    pub body: B,
}

impl<
    SO: std::fmt::Debug + 'static + AbstractTree,
    BO: std::fmt::Debug + 'static + AbstractTree,
    S: std::fmt::Debug,
    B: std::fmt::Debug,
> Input<&super::MemberTemplate<SO, BO>, ()> for &MemberTemplate<S, B>
where
    for<'x, 'y> &'x S: Input<&'y SO, ()>,
    for<'x, 'y> &'x B: Input<&'y BO, ()>,
{
    fn assert(
        self,
        output: &super::MemberTemplate<SO, BO>,
        (): (),
    ) -> proptest::test_runner::TestCaseResult {
        Some(&self.signature).assert(output.signature().as_ref(), ())?;
        Some(&self.body).assert(output.body().as_ref(), ())
    }
}

impl<S: 'static + Arbitrary, B: 'static + Arbitrary> Arbitrary
    for MemberTemplate<S, B>
where
    <S as Arbitrary>::Strategy: 'static,
    <B as Arbitrary>::Strategy: 'static,
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (S::arbitrary(), B::arbitrary())
            .prop_map(|(signature, body)| Self { signature, body })
            .boxed()
    }
}

impl<S: IndentDisplay, B: IndentDisplay> IndentDisplay
    for MemberTemplate<S, B>
{
    fn indent_fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.signature.indent_fmt(formatter, indent)?;
        self.body.indent_fmt(formatter, indent)
    }
}

pub type ConstantMember =
    MemberTemplate<constant::arbitrary::Signature, constant::arbitrary::Body>;

pub type FunctionMember = MemberTemplate<
    function::arbitrary::Signature,
    item::arbitrary::Body<Statement>,
>;

pub type TypeMember =
    MemberTemplate<r#type::arbitrary::Signature, r#type::arbitrary::Body>;

reference! {
    #[derive(Debug, Clone)]
    pub enum Member for super::Member {
        Constant(ConstantMember),
        Function(FunctionMember),
        Type(TypeMember),
    }
}

impl Arbitrary for Member {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            ConstantMember::arbitrary().prop_map(Self::Constant),
            FunctionMember::arbitrary().prop_map(Self::Function),
            TypeMember::arbitrary().prop_map(Self::Type),
        ]
        .boxed()
    }
}

impl IndentDisplay for Member {
    fn indent_fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Constant(member) => member.indent_fmt(formatter, indent),
            Self::Function(member) => member.indent_fmt(formatter, indent),
            Self::Type(member) => member.indent_fmt(formatter, indent),
        }
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Instance for super::Instance {
        pub signature (Signature),
        pub body (Body<Member>),
    }
}

impl Arbitrary for Instance {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (Signature::arbitrary(), Body::arbitrary())
            .prop_map(|(signature, body)| Self { signature, body })
            .boxed()
    }
}

impl IndentDisplay for Instance {
    fn indent_fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.signature.indent_fmt(formatter, indent)?;
        self.body.indent_fmt(formatter, indent)
    }
}
