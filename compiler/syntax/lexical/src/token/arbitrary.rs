#![allow(missing_docs)]

use std::fmt::{Debug, Display};

use enum_as_inner::EnumAsInner;
use pernixc_source_file::{Location, SourceMap, Span};
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy},
    prop_assert_eq, prop_oneof,
    test_runner::TestCaseError,
};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum PriorInsignificant {
    Tab(usize),
    Whitespace(usize),
}

impl Arbitrary for PriorInsignificant {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            3 => (1usize..10)
                .prop_map(Self::Tab),
            3 => (1usize..10)
                .prop_map(Self::Whitespace),
        ]
        .boxed()
    }
}

impl Display for PriorInsignificant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tab(x) => f.write_str(&"\t".repeat(*x)),
            Self::Whitespace(x) => f.write_str(&" ".repeat(*x)),
        }
    }
}

impl<L: Location<C> + std::fmt::Debug, C: Clone>
    Input<&Span<L>, (&SourceMap, C)> for &PriorInsignificant
{
    fn assert(
        self,
        output: &Span<L>,
        (source_map, location_context): (&SourceMap, C),
    ) -> proptest::test_runner::TestCaseResult {
        let source_file = source_map.get(output.source_id).unwrap();

        let absolute_span =
            output.to_absolute_span(&source_file, location_context);
        let prior_insignificant_source =
            &source_file.content()[absolute_span.range()];

        let expected_prior_insignificant = self.to_string();

        // check if the prior insignificant source is equal to the expected
        // string
        prop_assert_eq!(
            prior_insignificant_source,
            expected_prior_insignificant.as_str()
        );

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token<T> {
    pub kind: T,
    pub prior_insignificant: Option<PriorInsignificant>,
}

impl<T: Arbitrary> Arbitrary for Token<T>
where
    T::Strategy: 'static,
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (T::arbitrary(), proptest::option::of(PriorInsignificant::arbitrary()))
            .prop_map(|(kind, prior_insignificant)| Self {
                kind,
                prior_insignificant,
            })
            .boxed()
    }
}

impl<T: Display> Display for Token<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(prior_insignificant) = &self.prior_insignificant {
            Display::fmt(prior_insignificant, f)?;
        }

        Display::fmt(&self.kind, f)
    }
}

impl<
        't,
        'u,
        T: Debug + Display,
        U: Debug,
        L: Debug + Location<C>,
        C: Clone,
    > Input<&'u super::Token<U, L>, (&SourceMap, C)> for &'t Token<T>
where
    &'t T: Input<&'u U, ()>,
{
    fn assert(
        self,
        output: &'u super::Token<U, L>,
        (source_map, location_context): (&SourceMap, C),
    ) -> proptest::test_runner::TestCaseResult {
        self.kind.assert(&output.kind, ())?;

        prop_assert_eq!(
            output.prior_insignificant.as_ref().map(|_| output.span.source_id),
            output.prior_insignificant.as_ref().map(|x| x.source_id)
        );

        let source_file = &source_map.get(output.span.source_id).unwrap();

        let absolute_span =
            output.span.to_absolute_span(source_file, location_context.clone());
        let token_source = &source_file.content()[absolute_span.range()];

        // check if the token source is equal to the expected string
        let token_expected_str = self.kind.to_string();
        prop_assert_eq!(token_source, token_expected_str.as_str());

        match (&self.prior_insignificant, &output.prior_insignificant) {
            (
                Some(input_prior_insignificant),
                Some(output_prior_insignificant),
            ) => {
                let absolute_span = output_prior_insignificant
                    .to_absolute_span(source_file, location_context);
                let prior_insignificant_source =
                    &source_file.content()[absolute_span.range()];

                let expected_prior_insignificant =
                    input_prior_insignificant.to_string();

                // check if the prior insignificant source is equal to the
                // expected string
                prop_assert_eq!(
                    prior_insignificant_source,
                    expected_prior_insignificant.as_str()
                );
            }

            (None, None) => {}

            (x, y) => {
                return Err(TestCaseError::fail(format!(
                    "expected {x:?} got {y:?}",
                )))
            }
        }

        Ok(())
    }
}
