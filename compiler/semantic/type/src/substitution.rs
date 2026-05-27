use pernixc_hash::FxHashMap;
use pernixc_qbice::{Interner, TrackedEngine};
use pernixc_symbol::GlobalSymbolID;
use qbice::{Identifiable, StableHash, storage::intern::Interned};

use crate::{
    generic_parameters::{GenericParameterID, get_generic_parameters},
    r#type::{
        Type,
        constructor::rewrite::{RewriteContext, TypeRewriter},
        inference::InferenceVariable,
        rewrite::rewrite_type,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Variable {
    Inference(InferenceVariable),
    Generic(GenericParameterID),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Substitution(FxHashMap<Variable, Interned<Type>>);

impl Substitution {
    #[must_use]
    pub fn new() -> Self { Self::default() }

    #[must_use]
    pub fn singleton(variable: InferenceVariable, ty: Interned<Type>) -> Self {
        let mut map = FxHashMap::default();
        map.insert(Variable::Inference(variable), ty);
        Self(map)
    }

    pub fn insert_generic(
        &mut self,
        id: GenericParameterID,
        ty: Interned<Type>,
    ) {
        assert!(self.0.insert(Variable::Generic(id), ty).is_none());
    }

    pub fn merge(&mut self, other: &Self) {
        self.0.reserve(other.0.len());

        for (var, ty) in &other.0 {
            assert!(self.0.insert(*var, ty.clone()).is_none());
        }
    }

    pub(crate) async fn append_generic_arguments(
        &mut self,
        symbol_id: GlobalSymbolID,
        generic_arguments: &[Interned<Type>],
        engine: &TrackedEngine,
    ) {
        let generic_params = engine.get_generic_parameters(symbol_id).await;

        assert!(generic_params.len() == generic_arguments.len());

        for ((id, _), gen_arg) in
            generic_params.iter().zip(generic_arguments.iter())
        {
            self.insert_generic(
                GenericParameterID::new(symbol_id, id),
                gen_arg.clone(),
            );
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
        self.0.get(&Variable::Inference(variable)).cloned()
    }

    fn rewrite_generic_parameter(
        &mut self,
        id: GenericParameterID,
        _ctx: RewriteContext,
    ) -> Option<Interned<Type>> {
        self.0.get(&Variable::Generic(id)).cloned()
    }
}

pub trait Substitutable {
    #[must_use]
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl Interner,
    ) -> Option<Self>
    where
        Self: Sized;

    #[must_use]
    fn apply_or_clone(
        &self,
        subst: &Substitution,
        interner: &impl Interner,
    ) -> Self
    where
        Self: Sized + Clone,
    {
        self.apply(subst, interner).unwrap_or_else(|| self.clone())
    }

    #[must_use]
    fn apply_or_self(
        self,
        subst: &Substitution,
        interner: &impl Interner,
    ) -> Self
    where
        Self: Sized,
    {
        self.apply(subst, interner).unwrap_or(self)
    }
}

impl Substitutable for Interned<Type> {
    fn apply(
        &self,
        mut subst: &Substitution,
        interner: &impl Interner,
    ) -> Option<Self>
    where
        Self: Sized,
    {
        rewrite_type(self, &mut subst, interner)
    }
}

impl<
    T: Substitutable + StableHash + Send + Sync + 'static + Identifiable + Clone,
> Substitutable for Interned<[T]>
{
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl Interner,
    ) -> Option<Self>
    where
        Self: Sized,
    {
        for (i, item) in self.iter().enumerate() {
            if let Some(new_item) = item.apply(subst, interner) {
                let mut new_vec = Vec::with_capacity(self.len());
                new_vec.extend_from_slice(&self[..i]);
                new_vec.push(new_item);
                new_vec.extend(
                    self[(i + 1)..]
                        .iter()
                        .map(|item| item.apply_or_clone(subst, interner)),
                );
                return Some(interner.intern_unsized(new_vec));
            }
        }

        None
    }
}

pub trait InPlaceSubstitutable {
    fn apply_in_place(
        &mut self,
        subst: &Substitution,
        interner: &impl Interner,
    );
}
