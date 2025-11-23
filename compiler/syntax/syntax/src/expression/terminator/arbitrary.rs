use std::fmt::{Display, Write};

use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy as _},
    prop_oneof,
};

use crate::{
    arbitrary::{IndentDisplay, Label, QualifiedIdentifier},
    expression::{arbitrary::Expression, binary::arbitrary::Binary},
    reference,
    statement::arbitrary::Statement,
    r#type::arbitrary::Type,
};

reference! {
    #[derive(Debug, Clone)]
    pub enum Terminator for super::Terminator {
        Return(Return),
        Continue(Continue),
        Express(Express),
        Break(Break),
    }
}

impl Arbitrary for Terminator {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Return::arbitrary_with(args.clone()).prop_map(Self::Return),
            Continue::arbitrary().prop_map(Self::Continue),
            Express::arbitrary_with(args.clone()).prop_map(Self::Express),
            Break::arbitrary_with(args).prop_map(Self::Break),
        ]
        .boxed()
    }
}

impl IndentDisplay for Terminator {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Return(syn) => syn.indent_fmt(f, indent),
            Self::Continue(syn) => syn.fmt(f),
            Self::Express(syn) => syn.indent_fmt(f, indent),
            Self::Break(syn) => syn.indent_fmt(f, indent),
        }
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Return for super::Return {
        pub binary (Option<Binary>)
    }
}

impl Arbitrary for Return {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::option::of(Binary::arbitrary_with(args))
            .prop_map(|binary| Self { binary })
            .boxed()
    }
}

impl IndentDisplay for Return {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("return")?;

        if let Some(binary) = &self.binary {
            f.write_char(' ')?;
            binary.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "continue{}",
        label.as_ref().map(|x| format!(" {x}")).unwrap_or_default()
    )]
    pub struct Continue for super::Continue {
        pub label (Option<Label>)
    }
}

impl Arbitrary for Continue {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::option::of(Label::arbitrary())
            .prop_map(|label| Self { label })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Express for super::Express {
        pub label (Option<Label>),
        pub binary (Option<Binary>)
    }
}

impl Arbitrary for Express {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(Label::arbitrary()),
            proptest::option::of(Binary::arbitrary_with(args)),
        )
            .prop_map(|(label, binary)| Self { label, binary })
            .boxed()
    }
}

impl IndentDisplay for Express {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("express")?;

        if let Some(label) = &self.label {
            write!(f, " {label}")?;
        }

        if let Some(binary) = &self.binary {
            f.write_char(' ')?;
            binary.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Break for super::Break {
        pub label (Option<Label>),
        pub binary (Option<Binary>)
    }
}

impl Arbitrary for Break {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(Label::arbitrary()),
            proptest::option::of(Binary::arbitrary_with(args)),
        )
            .prop_map(|(label, binary)| Self { label, binary })
            .boxed()
    }
}

impl IndentDisplay for Break {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("break")?;

        if let Some(label) = &self.label {
            write!(f, " {label}")?;
        }

        if let Some(binary) = &self.binary {
            f.write_char(' ')?;
            binary.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}
