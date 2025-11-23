use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy};

use crate::{
    arbitrary::{
        IndentDisplay, Passable, write_indent_line_for_indent_display,
    },
    predicate::arbitrary::Predicate,
    reference,
};

reference! {
    #[derive(Debug, Clone)]
    pub struct Predicates for super::Predicates {
        pub predicates (Vec<Passable<Predicate>>)
    }
}

impl Arbitrary for Predicates {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(Passable::<Predicate>::arbitrary(), 1..10)
            .prop_map(|predicates| Self { predicates })
            .boxed()
    }
}

impl IndentDisplay for Predicates {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        writeln!(f, ":")?;
        for predicate in &self.predicates {
            write_indent_line_for_indent_display(f, predicate, indent + 1)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct WhereClause for super::WhereClause {
        pub predicates (Predicates)
    }
}

impl Arbitrary for WhereClause {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Predicates::arbitrary()
            .prop_map(|predicates| Self { predicates })
            .boxed()
    }
}

impl IndentDisplay for WhereClause {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "where")?;
        self.predicates.indent_fmt(f, indent)
    }
}
