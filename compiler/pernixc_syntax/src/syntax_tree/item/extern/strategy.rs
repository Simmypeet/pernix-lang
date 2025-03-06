use pernixc_lexical::token;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy},
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    item::{function, where_clause::strategy::WhereClause},
    strategy::{
        write_indent_line_for_indent_display, AccessModifier, IndentDisplay,
        Passable,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    pub access_modifier: AccessModifier,
    pub signature: function::strategy::Signature,
    pub trailing_where_clause: Option<WhereClause>,
}

impl Arbitrary for Function {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            function::strategy::Signature::arbitrary(),
            proptest::option::of(WhereClause::arbitrary()),
        )
            .prop_map(|(access_modifier, signature, trailing_where_clause)| {
                Self { access_modifier, signature, trailing_where_clause }
            })
            .boxed()
    }
}

impl Input<&super::Function> for &Function {
    fn assert(self, output: &super::Function) -> TestCaseResult {
        self.access_modifier.assert(&output.access_modifier)?;
        self.signature.assert(&output.signature)?;
        self.trailing_where_clause.as_ref().assert(
            output.trailing_where_clause.as_ref().map(|x| &x.where_clause),
        )
    }
}

impl IndentDisplay for Function {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "{} ", self.access_modifier)?;

        self.signature.indent_fmt(f, indent)?;

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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Extern {
    pub convention: token::strategy::String,
    pub functions: Vec<Passable<Function>>,
}

impl Arbitrary for Extern {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            token::strategy::String::arbitrary(),
            proptest::collection::vec(Passable::<Function>::arbitrary(), 1..10),
        )
            .prop_map(|(convention, functions)| Self { convention, functions })
            .boxed()
    }
}

impl Input<&super::Extern> for &Extern {
    fn assert(self, output: &super::Extern) -> TestCaseResult {
        self.convention.assert(&output.convention)?;
        self.functions.assert(&output.functions)
    }
}

impl IndentDisplay for Extern {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        writeln!(f, "extern {}:", self.convention)?;

        for function in &self.functions {
            write_indent_line_for_indent_display(f, function, indent + 1)?;
        }

        Ok(())
    }
}
