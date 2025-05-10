use proptest::prelude::Arbitrary;

use crate::{
    r#type::{arbitrary, Type},
    test::verify_ref,
};

proptest::proptest! {
    #[test]
    fn r#type(
        reference in arbitrary::Type::arbitrary()
    ) {
        verify_ref::<_, Type>(&reference)?;
    }
}
