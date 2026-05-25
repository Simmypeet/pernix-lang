use pernixc_hash::FxHashMap;
use pernixc_qbice::Interner;
use qbice::storage::intern::Interned;

use crate::r#type::{
    Type,
    constructor::rewrite::{RewriteContext, TypeRewriter, rewrite_type},
    inference::InferenceVariable,
};

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

    /// Applies this substitution to a type, replacing every free
    /// [`InferenceVariable`] with its mapped type.
    ///
    /// Variables that have no mapping are left unchanged.
    #[must_use]
    pub fn apply(
        &self,
        ty: &Interned<Type>,
        interner: &impl Interner,
    ) -> Interned<Type> {
        let mut rewriter = self;
        rewrite_type(ty, &mut rewriter, interner)
    }

    pub fn merge(&mut self, other: &Self) {
        self.0.reserve(other.0.len());

        for (var, ty) in &other.0 {
            assert!(self.0.insert(*var, ty.clone()).is_none());
        }
    }
}

/// Implements the rewrite pass for `apply`: replaces [`InferenceVariable`]
/// leaves with their substituted types.
impl TypeRewriter for &Substitution {
    fn rewrite_inference_variable(
        &mut self,
        variable: InferenceVariable,
        _ctx: RewriteContext,
    ) -> Option<Interned<Type>> {
        self.0.get(&variable).cloned()
    }
}
