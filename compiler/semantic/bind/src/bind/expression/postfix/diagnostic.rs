use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    bind::expression::postfix::access::diagnostic::{
        CannotIndexPastUnpackedTuple, FieldNotFound, TooLargeTupleIndex,
        TupleIndexOutOfBounds, UnexpectedTypeForAccess,
    },
    diagnostic_enum,
    pattern::bind::diagnostic::FieldIsNotAccessible,
};

diagnostic_enum! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        StableHash,
        Serialize,
        Deserialize,
    )]
    pub enum Diagnostic {
        FieldNotFound(FieldNotFound),
        UnexpectedTypeForAccess(UnexpectedTypeForAccess),
        FieldIsNotAccessible(FieldIsNotAccessible),
        TooLargeTupleIndex(TooLargeTupleIndex),
        CannotIndexPastUnpackedTuple(CannotIndexPastUnpackedTuple),
        TupleIndexOutOfBounds(TupleIndexOutOfBounds),
    }
}
