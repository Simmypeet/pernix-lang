use pernixc_qbice::Interner;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::r#type::{
    Type,
    kind::TyKind,
    rewrite::{
        RewriteContext, TypeRewriter, rewrite_type, rewrite_type_or_clone,
    },
};

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Binder {
    bound_vars: Interned<[TyKind]>,
}

impl Binder {
    /// Creates a new binder with the given bound variable kinds.
    #[must_use]
    pub const fn new(bound_vars: Interned<[TyKind]>) -> Self {
        Self { bound_vars }
    }

    /// Returns the number of variables bound by this binder.
    #[must_use]
    pub fn len(&self) -> usize { self.bound_vars.len() }

    /// Returns whether this binder binds no variables.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.bound_vars.is_empty() }

    /// Returns the kind of the bound variable at the given index.
    #[must_use]
    pub fn kind_at(&self, index: usize) -> Option<TyKind> {
        self.bound_vars.get(index).copied()
    }

    /// Returns an iterator over the kinds of variables bound by this binder.
    #[must_use]
    pub fn kinds(&self) -> impl ExactSizeIterator<Item = TyKind> + '_ {
        self.bound_vars.iter().copied()
    }

    /// Instantiates the variables bound by this binder in `value`.
    ///
    /// The replacement at index `i` instantiates bound variable `(depth = 0,
    /// index = i)` at the root of `value`. Under nested binders, the rewritten
    /// depth is adjusted automatically so variables bound by nested binders are
    /// left untouched.
    ///
    /// This is intended for operations such as skolemizing this binder or
    /// instantiating it with freshly-created inference variables.
    #[must_use]
    pub fn instantiate<T: Instantiate + Clone>(
        &self,
        value: &T,
        replacements: &[Interned<Type>],
        interner: &impl Interner,
    ) -> T {
        assert_eq!(self.bound_vars.len(), replacements.len());

        value.instantiate(replacements, interner)
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct BoundVariable {
    depth: usize,
    index: usize,
}

impl BoundVariable {
    /// Creates a new bound variable reference.
    #[must_use]
    pub const fn new(depth: usize, index: usize) -> Self {
        Self { depth, index }
    }

    /// Returns the number of binders between this variable occurrence and the
    /// binder that introduces it.
    #[must_use]
    pub const fn depth(&self) -> usize { self.depth }

    /// Returns the variable index inside its binder.
    #[must_use]
    pub const fn index(&self) -> usize { self.index }
}

struct BoundVariableRewriter<'a, I> {
    replacements: &'a [Interned<Type>],
    interner: &'a I,
}

impl<I: Interner> TypeRewriter for BoundVariableRewriter<'_, I> {
    fn rewrite_bound_variable(
        &mut self,
        variable: BoundVariable,
        ctx: RewriteContext,
    ) -> Option<Interned<Type>> {
        if variable.depth != ctx.binder_depth() {
            return None;
        }

        let replacement = self.replacements.get(variable.index)?.clone();

        Some(shift_free_bound_variables(
            &replacement,
            ctx.binder_depth(),
            self.interner,
        ))
    }
}

struct ShiftFreeBoundVariableRewriter<'a, I> {
    amount: usize,
    interner: &'a I,
}

impl<I: Interner> TypeRewriter for ShiftFreeBoundVariableRewriter<'_, I> {
    fn rewrite_bound_variable(
        &mut self,
        variable: BoundVariable,
        ctx: RewriteContext,
    ) -> Option<Interned<Type>> {
        (variable.depth >= ctx.binder_depth()).then(|| {
            self.interner.intern(Type::BoundVariable(BoundVariable::new(
                variable.depth + self.amount,
                variable.index,
            )))
        })
    }
}

fn shift_free_bound_variables(
    ty: &Interned<Type>,
    amount: usize,
    interner: &impl Interner,
) -> Interned<Type> {
    if amount == 0 {
        return ty.clone();
    }

    rewrite_type_or_clone(
        ty,
        &mut ShiftFreeBoundVariableRewriter { amount, interner },
        interner,
    )
}

/// Instantiates values containing bound type variables.
///
/// A bound variable is instantiated when its depth points to the binder
/// immediately enclosing the instantiated value from its occurrence site. For
/// example, at the root of a type, this rewrites depth `0`; under one nested
/// binder, it rewrites depth `1`.
pub trait Instantiate {
    /// Attempts to instantiate bound variables in `self`.
    ///
    /// Returns `None` if no bound variables were replaced.
    #[must_use]
    fn try_instantiate(
        &self,
        replacements: &[Interned<Type>],
        interner: &impl Interner,
    ) -> Option<Self>
    where
        Self: Sized;

    /// Instantiates bound variables in `self`, or clones `self` if no bound
    /// variables were replaced.
    #[must_use]
    fn instantiate(
        &self,
        replacements: &[Interned<Type>],
        interner: &impl Interner,
    ) -> Self
    where
        Self: Sized + Clone,
    {
        self.try_instantiate(replacements, interner)
            .unwrap_or_else(|| self.clone())
    }
}

impl Instantiate for Interned<Type> {
    fn try_instantiate(
        &self,
        replacements: &[Interned<Type>],
        interner: &impl Interner,
    ) -> Option<Self>
    where
        Self: Sized,
    {
        rewrite_type(
            self,
            &mut BoundVariableRewriter { replacements, interner },
            interner,
        )
    }
}

impl<T: Instantiate + StableHash + Send + Sync + 'static + Identifiable + Clone>
    Instantiate for Interned<[T]>
{
    fn try_instantiate(
        &self,
        replacements: &[Interned<Type>],
        interner: &impl Interner,
    ) -> Option<Self>
    where
        Self: Sized,
    {
        for (index, item) in self.iter().enumerate() {
            if let Some(new_item) = item.try_instantiate(replacements, interner)
            {
                let mut new_vec = Vec::with_capacity(self.len());
                new_vec.extend_from_slice(&self[..index]);
                new_vec.push(new_item);
                new_vec.extend(
                    self[(index + 1)..]
                        .iter()
                        .map(|item| item.instantiate(replacements, interner)),
                );
                return Some(interner.intern_unsized(new_vec));
            }
        }

        None
    }
}
