use pernixc_test_input::Input;
use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy};

use crate::syntax_tree::{
    predicate::strategy::Predicate,
    strategy::{write_indent_line_for_indent_display, IndentDisplay, Passable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WhereClause {
    pub predicates: Vec<Passable<Predicate>>,
}

impl Arbitrary for WhereClause {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(Passable::<Predicate>::arbitrary(), 1..10)
            .prop_map(|predicates| Self { predicates })
            .boxed()
    }
}

impl Input<&super::WhereClause> for &WhereClause {
    fn assert(
        self,
        output: &super::WhereClause,
    ) -> proptest::test_runner::TestCaseResult {
        self.predicates.assert(&output.predicates)
    }
}

impl IndentDisplay for WhereClause {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        writeln!(f, "where:")?;

        for predicate in &self.predicates {
            write_indent_line_for_indent_display(f, predicate, indent + 1)?;
        }

        Ok(())
    }
}
