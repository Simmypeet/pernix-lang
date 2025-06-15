use pernixc_lexical::kind::arbitrary::Identifier;
use pernixc_parser::abstract_tree::AbstractTree;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy as _},
    prop_oneof,
};

use crate::{
    arbitrary::{AccessModifier, IndentDisplay},
    item::{
        arbitrary::{Body, TrailingWhereClause},
        constant::{self, arbitrary::Signature as ConstantSignature},
        function::{self, arbitrary::Signature as FunctionSignature},
        generic_parameters::arbitrary::GenericParameters,
        r#type::{self, arbitrary::Signature as TypeSignature},
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
        write!(formatter, "trait {}", self.identifier)?;

        if let Some(generic_parameters) = &self.generic_parameters {
            generic_parameters.indent_fmt(formatter, indent)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct MemberTemplate<T> {
    pub access_modifier: AccessModifier,
    pub signature: T,
    pub trailing_where_clause: Option<TrailingWhereClause>,
}

impl<I: std::fmt::Debug, O: std::fmt::Debug + 'static + AbstractTree>
    Input<&super::MemberTemplate<O>, ()> for &MemberTemplate<I>
where
    for<'x, 'y> &'x I: Input<&'y O, ()>,
{
    fn assert(
        self,
        output: &super::MemberTemplate<O>,
        (): (),
    ) -> proptest::test_runner::TestCaseResult {
        Some(&self.access_modifier)
            .assert(output.access_modifier().as_ref(), ())?;
        Some(&self.signature).assert(output.signature().as_ref(), ())?;
        self.trailing_where_clause
            .as_ref()
            .assert(output.trailing_where_clause().as_ref(), ())
    }
}

impl<T: 'static + Arbitrary + IndentDisplay> Arbitrary for MemberTemplate<T>
where
    <T as Arbitrary>::Strategy: 'static,
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            T::arbitrary(),
            proptest::option::of(TrailingWhereClause::arbitrary()),
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

reference! {
    #[derive(Debug, Clone)]
    pub enum Member for super::Member {
        Type(MemberTemplate<r#type::arbitrary::Signature>),
        Function(MemberTemplate<function::arbitrary::Signature>),
        Constant(MemberTemplate<constant::arbitrary::Signature>),
    }
}

impl Arbitrary for Member {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            MemberTemplate::<TypeSignature>::arbitrary().prop_map(Self::Type),
            MemberTemplate::<FunctionSignature>::arbitrary()
                .prop_map(Self::Function),
            MemberTemplate::<ConstantSignature>::arbitrary()
                .prop_map(Self::Constant),
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
            Self::Type(member) => member.indent_fmt(formatter, indent),
            Self::Function(member) => member.indent_fmt(formatter, indent),
            Self::Constant(member) => member.indent_fmt(formatter, indent),
        }
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Trait for super::Trait {
        pub access_modifier (AccessModifier),
        pub signature (Signature),
        pub body (Body<Member>),
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
        formatter: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(formatter, "{} ", self.access_modifier)?;
        self.signature.indent_fmt(formatter, indent)?;
        self.body.indent_fmt(formatter, indent)
    }
}
