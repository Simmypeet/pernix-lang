use pernixc_qbice::Interner;
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::r#type::{
    Type,
    kind::TyKind,
    rewrite::{RewriteContext, TypeRewriter, rewrite_type_or_clone},
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

    /// Rewrites the variables bound by this binder in `ty`.
    ///
    /// The replacement at index `i` rewrites bound variable `(depth = 0, index
    /// = i)` at the root of `ty`. Under nested binders, the rewritten depth is
    /// adjusted automatically so variables bound by nested binders are left
    /// untouched.
    ///
    /// This is intended for operations such as skolemizing this binder or
    /// instantiating it with freshly-created inference variables.
    #[must_use]
    pub fn rewrite_bound_variables(
        &self,
        ty: &Interned<Type>,
        replacements: &[Interned<Type>],
        interner: &impl Interner,
    ) -> Interned<Type> {
        assert_eq!(self.bound_vars.len(), replacements.len());

        rewrite_bound_variables(ty, replacements, interner)
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

/// Rewrites bound variables for the binder immediately enclosing `ty`.
///
/// A bound variable is rewritten when its depth points to that enclosing
/// binder from its occurrence site. For example, at the root of `ty`, this
/// rewrites depth `0`; under one nested binder, it rewrites depth `1`.
#[must_use]
pub fn rewrite_bound_variables(
    ty: &Interned<Type>,
    replacements: &[Interned<Type>],
    interner: &impl Interner,
) -> Interned<Type> {
    rewrite_type_or_clone(
        ty,
        &mut BoundVariableRewriter { replacements, interner },
        interner,
    )
}
