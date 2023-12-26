use proptest::{
    arbitrary::Arbitrary,
    prop_oneof,
    strategy::{BoxedStrategy, Strategy},
};

use super::{Constant, Primitive};

impl Arbitrary for Primitive {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            proptest::num::i8::ANY.prop_map(Self::Int8),
            proptest::num::i16::ANY.prop_map(Self::Int16),
            proptest::num::i32::ANY.prop_map(Self::Int32),
            proptest::num::i64::ANY.prop_map(Self::Int64),
            proptest::num::u8::ANY.prop_map(Self::Uint8),
            proptest::num::u16::ANY.prop_map(Self::Uint16),
            proptest::num::u32::ANY.prop_map(Self::Uint32),
            proptest::num::u64::ANY.prop_map(Self::Uint64),
            proptest::num::usize::ANY.prop_map(Self::Usize),
            proptest::num::isize::ANY.prop_map(Self::Isize),
            proptest::bool::ANY.prop_map(Self::Bool),
        ]
        .boxed()
    }
}

impl Arbitrary for Constant {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Primitive::arbitrary().prop_map(Self::Primitive).boxed()
    }
}
