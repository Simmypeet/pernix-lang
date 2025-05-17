use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy as _};

use crate::{
    arbitrary::{
        write_indent_line_for_indent_display, AccessModifier, IndentDisplay,
        Passable,
    },
    item::{arbitrary::TrailingWhereClause, function::arbitrary::Signature},
    reference,
};

reference! {
    #[derive(Debug, Clone)]
    pub struct Function for super::Function{
        pub access_modifier (AccessModifier),
        pub signature (Signature),
        pub trailing_where_clause (Option<TrailingWhereClause>)
    }
}

impl Arbitrary for Function {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            Signature::arbitrary(),
            proptest::option::of(TrailingWhereClause::arbitrary()),
        )
            .prop_map(|(access_modifier, signature, trailing_where_clause)| {
                Self { access_modifier, signature, trailing_where_clause }
            })
            .boxed()
    }
}

impl IndentDisplay for Function {
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
    pub struct Body for super::Body {
        pub functions (Vec<Passable<Function>>),
    }
}

impl Arbitrary for Body {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(Passable::arbitrary(), 1..6)
            .prop_map(|functions| Self { functions })
            .boxed()
    }
}

impl IndentDisplay for Body {
    fn indent_fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        writeln!(formatter, ":")?;
        for function in &self.functions {
            write_indent_line_for_indent_display(
                formatter,
                function,
                indent + 1,
            )?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Extern for super::Extern {
        #{map_input_assert(convention, &convention.kind)}
        pub convention (pernixc_lexical::kind::arbitrary::String),
        pub body (Body),
    }
}

impl Arbitrary for Extern {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            pernixc_lexical::kind::arbitrary::String::arbitrary(),
            Body::arbitrary(),
        )
            .prop_map(|(convention, body)| Self { convention, body })
            .boxed()
    }
}

impl IndentDisplay for Extern {
    fn indent_fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(formatter, "extern {}", self.convention)?;
        self.body.indent_fmt(formatter, indent)?;

        Ok(())
    }
}
