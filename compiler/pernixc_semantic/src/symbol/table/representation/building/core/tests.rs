use proptest::{
    arbitrary::Arbitrary,
    proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};
use strum::IntoEnumIterator;

use crate::{
    symbol::table::{Building, Table},
    type_system::{
        environment::Environment,
        model::{Default, Model},
        normalizer, observer, predicate,
        term::{
            r#type::{Primitive, Type},
            GenericArguments, Tuple, TupleElement,
        },
        Compute,
    },
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
    tuple: &PrimitivesTuple,
) -> TestCaseResult {
    let table = Table::<Building>::default();

    let core_module_id =
        table.root_module_ids_by_name.get("core").copied().unwrap();
    let copy_marker_id = table
        .get_by_qualified_name(["core", "Copy"].into_iter())
        .unwrap()
        .into_marker()
        .unwrap();

    let tuple_type = tuple.create_tuple_type::<Default>();

    let active_premise =
        table.get_active_premise(core_module_id.into()).unwrap();
    let (environment, _) = Environment::new_with(
        active_premise,
        &table,
        normalizer::NO_OP,
        observer::NO_OP,
    );

    let marker_predicate = predicate::PositiveMarker {
        id: copy_marker_id,
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![tuple_type],
            constants: Vec::new(),
        },
    };

    let satisfiability = marker_predicate.query(&environment)?;

    let Some(satisfiability) = satisfiability else {
        return Err(TestCaseError::fail("unsatisfied"));
    };

    assert_eq!(satisfiability.constraints.len(), 0);

    Ok(())
}

proptest! {
    #[test]
    fn check_primitives_tuple_copyable(
        tuple in PrimitivesTuple::arbitrary()
    ) {
        check_primitives_tuple_copyable_impl(&tuple)?;
   }
}
