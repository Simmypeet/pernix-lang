use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy as _};

use crate::{
    arbitrary::{IndentDisplay, Label, QualifiedIdentifier},
    expression::arbitrary::Expression,
    r#type::arbitrary::Type,
    reference,
    statement::arbitrary::{Statement, Statements},
};

reference! {
    #[derive(Debug, Clone)]
    pub enum Block for super::Block {
        Scope(Scope),
    }
}

impl Arbitrary for Block {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        Scope::arbitrary_with(args).prop_map(Self::Scope).boxed()
    }
}

impl IndentDisplay for Block {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Scope(scope) => scope.indent_fmt(f, indent),
        }
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Scope for super::Scope{
        pub unsafe_keyword (bool),
        pub label (Option<Label>),
        pub statements (Statements)
    }
}

impl Arbitrary for Scope {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::bool::ANY,
            proptest::option::of(Label::arbitrary()),
            Statements::arbitrary_with(args),
        )
            .prop_map(|(unsafe_keyword, label, statements)| Self {
                unsafe_keyword,
                label,
                statements,
            })
            .boxed()
    }
}

impl IndentDisplay for Scope {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if self.unsafe_keyword {
            f.write_str("unsafe ")?;
        }

        f.write_str("scope")?;

        if let Some(label) = &self.label {
            write!(f, " {label}")?;
        }

        self.statements.indent_fmt(f, indent)
    }
}
