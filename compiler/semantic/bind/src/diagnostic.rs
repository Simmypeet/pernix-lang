use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_more::From,
)]
pub enum Diagnostic {
    Resolution(pernixc_resolution::diagnostic::Diagnostic),
    TypeSystem(pernixc_type_system::diagnostic::Diagnostic),
}
