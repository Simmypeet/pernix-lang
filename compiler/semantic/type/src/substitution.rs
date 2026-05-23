use pernixc_hash::FxHashMap;
use qbice::storage::intern::Interned;

use crate::r#type::{Type, inference::InferenceVariable};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Substitution(FxHashMap<InferenceVariable, Interned<Type>>);

impl Substitution {
    #[must_use]
    pub fn new() -> Self { Self::default() }

    #[must_use]
    pub fn singleton(variable: InferenceVariable, ty: Interned<Type>) -> Self {
        let mut map = FxHashMap::default();
        map.insert(variable, ty);
        Self(map)
    }
}
