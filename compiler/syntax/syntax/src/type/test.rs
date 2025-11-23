use proptest::prelude::Arbitrary;

use crate::{
    test::verify_ref,
    r#type::{Type, arbitrary},
};

proptest::proptest! {
    #[test]
    fn r#type(
        reference in arbitrary::Type::arbitrary()
    ) {
        verify_ref::<_, Type>(&reference)?;
    }
}
