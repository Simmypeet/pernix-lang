use pernixc_hash::FxHashMap;
use pernixc_qbice::{Interner, TrackedEngine};
use pernixc_symbol::GlobalSymbolID;
use qbice::{Identifiable, StableHash, storage::intern::Interned};

use crate::{
    generic_parameters::{GenericParameterID, get_generic_parameters2},
    r#type::{
        Type2,
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
pub struct Substitution(FxHashMap<Variable, Interned<Type2>>);

impl Substitution {
    #[must_use]
    pub fn new() -> Self { Self::default() }

    #[must_use]
    pub fn singleton(variable: InferenceVariable, ty: Interned<Type2>) -> Self {
        let mut map = FxHashMap::default();
        map.insert(Variable::Inference(variable), ty);
        Self(map)
    }

    pub fn insert_generic(
        &mut self,
        id: GenericParameterID,
        ty: Interned<Type2>,
    ) {
        assert!(self.0.insert(Variable::Generic(id), ty).is_none());
    }

    #[must_use]
    pub fn get_generic(
        &self,
        id: GenericParameterID,
    ) -> Option<&Interned<Type2>> {
        self.0.get(&Variable::Generic(id))
    }

    /// Composes `sub2` into `self` such that `self(sub2(x)) = composedSelf(x)`.
    pub fn compose(&mut self, mut sub2: Self, interner: &impl Interner) {
        self.0.reserve(sub2.0.len());

        for ty in sub2.0.values_mut() {
            *ty = ty.apply_or_clone(self, interner);
        }

        for (var, ty) in sub2.0 {
            // sub2 bias: if sub1 and sub2 have the same variable, the one from
            // sub2 will be used.
            self.0.insert(var, ty);
        }
    }

    pub fn merge(&mut self, other: &Self) {
        self.0.reserve(other.0.len());

        for (var, ty) in &other.0 {
            assert!(self.0.insert(*var, ty.clone()).is_none());
        }
    }

    pub fn retain(
        &mut self,
        mut f: impl FnMut(Variable, &Interned<Type2>) -> bool,
    ) {
        self.0.retain(|variable, ty| f(*variable, ty));
    }

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (Variable, &Interned<Type2>)> + '_ {
        self.0.iter().map(|(variable, ty)| (*variable, ty))
    }

    pub(crate) async fn append_generic_arguments(
        &mut self,
        symbol_id: GlobalSymbolID,
        generic_arguments: &[Interned<Type2>],
        engine: &TrackedEngine,
    ) {
        let generic_params = engine.get_generic_parameters2(symbol_id).await;

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
    ) -> Option<Interned<Type2>> {
        self.0.get(&Variable::Inference(variable)).cloned()
    }

    fn rewrite_generic_parameter(
        &mut self,
        id: GenericParameterID,
        _ctx: RewriteContext,
    ) -> Option<Interned<Type2>> {
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

impl Substitutable for Interned<Type2> {
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

#[cfg(test)]
mod test {
    use pernixc_qbice::create_minimal_engine as create_engine;

    use super::*;
    use crate::r#type::{
        constructor::Primitive, kind::TyKind, universe::UniverseIndex,
    };

    // input: {X: ?signature | ?tail} with both variables substituted
    // premise: ?signature -> bool, ?tail -> {}
    // output: {X: bool | {}}
    #[tokio::test]
    async fn substitution_recurses_through_both_effect_row_arguments() {
        let engine = create_engine().await;
        let signature_variable = InferenceVariable::new(
            0,
            TyKind::EffectSignature,
            UniverseIndex::root(),
        );
        let tail_variable =
            InferenceVariable::new(1, TyKind::EffectRow, UniverseIndex::root());
        let label: Interned<str> = engine.intern_unsized("X".to_owned());
        let row = Type2::new_effect_row_extend(
            label.clone(),
            Type2::new_inference_variable(signature_variable, &engine),
            Type2::new_inference_variable(tail_variable, &engine),
            &engine,
        );
        let replacement_signature =
            Type2::new_primitive(Primitive::Bool, &engine);
        let replacement_tail = Type2::new_effect_row_empty(&engine);
        let mut substitution = Substitution::singleton(
            signature_variable,
            replacement_signature.clone(),
        );
        substitution.merge(&Substitution::singleton(
            tail_variable,
            replacement_tail.clone(),
        ));

        let substituted = row.apply_or_clone(&substitution, &engine);

        assert_eq!(
            substituted,
            Type2::new_effect_row_extend(
                label,
                replacement_signature,
                replacement_tail,
                &engine,
            )
        );
    }
}
