use proptest::{
    arbitrary::Arbitrary,
    proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};
use strum::IntoEnumIterator;

use crate::{
    semantic::{
        model::{Default, Model},
        normalizer::NoOp,
        predicate,
        term::{
            r#type::{Primitive, Type},
            GenericArguments, Tuple, TupleElement,
        },
        Environment,
    },
    symbol::table::{Building, Table},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct PrimitivesTuple {
    pub primitives: Vec<Primitive>,
}

impl PrimitivesTuple {
    pub fn create_tuple_type<M: Model>(&self) -> Type<M> {
        Type::Tuple(Tuple {
            elements: self
                .primitives
                .iter()
                .copied()
                .map(|x| TupleElement {
                    term: Type::Primitive(x),
                    is_unpacked: false,
                })
                .collect(),
        })
    }
}

impl Arbitrary for PrimitivesTuple {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let primitives = Primitive::iter().collect::<Vec<_>>();

        proptest::collection::vec(proptest::sample::select(primitives), 0..=10)
            .prop_map(|primitives| Self { primitives })
            .boxed()
    }
}

fn check_primitives_tuple_copyable_impl(
    tuple: PrimitivesTuple,
) -> TestCaseResult {
    let mut table = Table::<Building>::default();
    table.initialize_core();

    let core_module_id =
        table.root_module_ids_by_name.get("core").copied().unwrap();
    let copy_trait_id = table
        .get_by_qualified_name(["core", "Copy"].into_iter())
        .unwrap()
        .into_trait()
        .unwrap();

    let tuple_type = tuple.create_tuple_type::<Default>();

    let active_premise =
        table.get_active_premise(core_module_id.into()).unwrap();

    let satisfiability = predicate::Trait::satisfies(
        copy_trait_id,
        true,
        &GenericArguments {
            lifetimes: Vec::new(),
            types: vec![tuple_type],
            constants: Vec::new(),
        },
        &Environment {
            premise: &active_premise,
            table: &table,
            normalizer: &NoOp,
        },
    )?;

    let Some(satisfiability) = satisfiability else {
        return Err(TestCaseError::fail("unsatisfied"));
    };

    assert_eq!(satisfiability.lifetime_constraints.len(), 0);

    Ok(())
}

proptest! {
    #[test]
    fn check_primitives_tuple_copyable(
        tuple in PrimitivesTuple::arbitrary()
    ) {
        check_primitives_tuple_copyable_impl(tuple).unwrap();
   }
}
