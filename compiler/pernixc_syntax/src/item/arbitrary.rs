use pernixc_parser::abstract_tree::AbstractTree;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy as _},
    prop_oneof,
    test_runner::TestCaseResult,
};

use super::where_clause::arbitrary::WhereClause;
use crate::{
    arbitrary::{
        write_indent_line_for_indent_display, IndentDisplay, Passable,
    },
    reference,
};

reference! {
    #[derive(Debug, Clone)]
    pub struct TrailingWhereClause for super::TrailingWhereClause {
        pub where_clause (WhereClause),
    }
}

impl Arbitrary for TrailingWhereClause {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        WhereClause::arbitrary()
            .prop_map(|where_clause| Self { where_clause })
            .boxed()
    }
}

impl IndentDisplay for TrailingWhereClause {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        writeln!(f, ":")?;
        write_indent_line_for_indent_display(f, &self.where_clause, indent + 1)
    }
}

#[derive(Debug, Clone)]
pub struct Body<T> {
    pub where_clause: Option<WhereClause>,
    pub members: Vec<Passable<T>>,
}

impl<O: std::fmt::Debug + AbstractTree, T: std::fmt::Debug>
    Input<&super::Body<O>, ()> for &Body<T>
where
    for<'x, 'y> &'x T: Input<&'y O, ()>,
{
    fn assert(self, output: &super::Body<O>, (): ()) -> TestCaseResult {
        self.where_clause
            .as_ref()
            .assert(output.where_clause().as_ref(), ())?;
        self.members.assert(output.members().collect::<Vec<_>>().as_slice(), ())
    }
}

impl<T: 'static + Arbitrary + Clone> Arbitrary for Body<T>
where
    <T as Arbitrary>::Strategy: 'static,
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let variant = prop_oneof![
            4 => T::arbitrary().prop_map(Passable::Line),
            1 => Just(Passable::Pass),
        ];

        (
            proptest::option::of(WhereClause::arbitrary()),
            proptest::collection::vec(variant, 1..=10),
        )
            .prop_map(|(where_clause, members)| Self { where_clause, members })
            .boxed()
    }
}

impl<T: IndentDisplay> IndentDisplay for Body<T> {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        writeln!(f, ":")?;

        if let Some(where_clause) = self.where_clause.as_ref() {
            write_indent_line_for_indent_display(f, where_clause, indent + 1)?;
        }

        for member in &self.members {
            write_indent_line_for_indent_display(f, member, indent + 1)?;
        }

        Ok(())
    }
}
