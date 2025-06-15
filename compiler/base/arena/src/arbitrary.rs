//! Arbitrary implementations for [`ID`].

use proptest::{
    num::u64,
    prelude::{Arbitrary, BoxedStrategy, Strategy},
};

use crate::ID;

impl<T: 'static> Arbitrary for ID<T> {
    type Strategy = BoxedStrategy<Self>;
    type Parameters = ();

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        u64::ANY.prop_map(Self::new).boxed()
    }
}
