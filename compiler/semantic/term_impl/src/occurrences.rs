use pernixc_lexical::tree::RelativeSpan;
use pernixc_resolution::qualified_identifier::Resolution;
use pernixc_stable_hash::StableHash;
use pernixc_term::{constant::Constant, lifetime::Lifetime, r#type::Type};

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, StableHash, Default,
)]
#[allow(missing_docs)]
pub struct Occurrences {
    pub types: Vec<(Type, pernixc_syntax::r#type::Type)>,
    pub lifetimes: Vec<(Lifetime, pernixc_syntax::Lifetime)>,
    pub constants: Vec<(Constant, pernixc_syntax::expression::Expression)>,

    pub resolutions: Vec<(Resolution, RelativeSpan)>,

    pub unpacked_types: Vec<(Type, pernixc_syntax::r#type::Type)>,
    pub unpacked_constants:
        Vec<(Constant, pernixc_syntax::expression::Expression)>,

    pub constant_types: Vec<(Type, pernixc_syntax::r#type::Type)>,
}
