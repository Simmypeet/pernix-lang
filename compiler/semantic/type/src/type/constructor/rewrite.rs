use pernixc_qbice::Interner;
use qbice::storage::intern::Interned;

use super::{Application, Constructor};
use crate::{
    generic_parameters::GenericParameterID,
    r#type::{
        Type, bound::BoundVariable, inference::InferenceVariable,
        skolem::SkolemizedVariable,
    },
};

/// Context for a type rewrite operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RewriteContext {
    binder_depth: usize,
}

impl RewriteContext {
    /// Returns the number of binders surrounding the current type node.
    #[must_use]
    pub const fn binder_depth(&self) -> usize { self.binder_depth }

    const fn enter_binder(self) -> Self {
        Self { binder_depth: self.binder_depth + 1 }
    }
}

/// Rewrites selected nodes in a type tree.
///
/// A returned replacement is used as-is and is not recursively rewritten.
#[allow(unused_variables)]
pub trait TypeRewriter {
    /// Rewrites an application type.
    fn rewrite_application(
        &mut self,
        application: &Application,
        ctx: RewriteContext,
    ) -> Option<Interned<Type>> {
        None
    }

    /// Rewrites a generic parameter type.
    fn rewrite_generic_parameter(
        &mut self,
        id: GenericParameterID,
        ctx: RewriteContext,
    ) -> Option<Interned<Type>> {
        None
    }

    /// Rewrites an inference variable type.
    fn rewrite_inference_variable(
        &mut self,
        variable: InferenceVariable,
        ctx: RewriteContext,
    ) -> Option<Interned<Type>> {
        None
    }

    /// Rewrites a bound variable type.
    fn rewrite_bound_variable(
        &mut self,
        variable: BoundVariable,
        ctx: RewriteContext,
    ) -> Option<Interned<Type>> {
        None
    }

    /// Rewrites a skolemized variable type.
    fn rewrite_skolemized_variable(
        &mut self,
        variable: SkolemizedVariable,
        ctx: RewriteContext,
    ) -> Option<Interned<Type>> {
        None
    }
}

/// Rewrites a type using lazy clone-on-write traversal.
pub fn rewrite_type_or_clone<R: TypeRewriter>(
    ty: &Interned<Type>,
    rewriter: &mut R,
    interner: &impl Interner,
) -> Interned<Type> {
    rewrite_type_internal(ty, rewriter, interner, RewriteContext {
        binder_depth: 0,
    })
    .unwrap_or_else(|| ty.clone())
}

/// Rewrites an instance associated type application into the associated type of
/// the instance's trait, if possible.
pub fn rewrite_type<R: TypeRewriter>(
    ty: &Interned<Type>,
    rewriter: &mut R,
    interner: &impl Interner,
) -> Option<Interned<Type>> {
    rewrite_type_internal(ty, rewriter, interner, RewriteContext {
        binder_depth: 0,
    })
}

fn rewrite_type_internal<R: TypeRewriter>(
    ty: &Interned<Type>,
    rewriter: &mut R,
    interner: &impl Interner,
    ctx: RewriteContext,
) -> Option<Interned<Type>> {
    match &**ty {
        Type::GenericParameter(id) => {
            rewriter.rewrite_generic_parameter(*id, ctx)
        }

        Type::InferenceVariable(variable) => {
            rewriter.rewrite_inference_variable(*variable, ctx)
        }

        Type::BoundVariable(variable) => {
            rewriter.rewrite_bound_variable(*variable, ctx)
        }

        Type::SkolemizedVariable(variable) => {
            rewriter.rewrite_skolemized_variable(*variable, ctx)
        }

        Type::Application(application) => rewrite_application_with_rewriter(
            application,
            rewriter,
            interner,
            ctx,
        ),
    }
}

/// Rewrites the direct arguments of an application using a failable
/// asynchronous callback.
///
/// Returns `Ok(None)` when none of the arguments changed.
pub async fn rewrite_application<E>(
    application: &Application,
    mut rewrite_argument: impl AsyncFnMut(
        &Interned<Type>,
    ) -> Result<Option<Interned<Type>>, E>,
) -> Result<Option<Application>, E> {
    let mut new_arguments = None::<Vec<_>>;

    for (index, argument) in application.arguments.iter().enumerate() {
        let rewritten_argument = rewrite_argument(argument).await?;

        collect_rewritten_argument(
            &mut new_arguments,
            &application.arguments,
            index,
            argument,
            rewritten_argument,
        );
    }

    Ok(rewritten_application(application, new_arguments))
}

fn rewrite_application_with_rewriter<R: TypeRewriter>(
    application: &Application,
    rewriter: &mut R,
    interner: &impl Interner,
    ctx: RewriteContext,
) -> Option<Interned<Type>> {
    let argument_ctx = argument_context(application, ctx);
    let mut new_arguments = None::<Vec<_>>;

    for (index, argument) in application.arguments.iter().enumerate() {
        let rewritten_argument =
            rewrite_type_internal(argument, rewriter, interner, argument_ctx);

        collect_rewritten_argument(
            &mut new_arguments,
            &application.arguments,
            index,
            argument,
            rewritten_argument,
        );
    }

    let rewritten_application =
        rewritten_application(application, new_arguments);

    rewriter
        .rewrite_application(
            rewritten_application.as_ref().unwrap_or(application),
            ctx,
        )
        .or_else(|| {
            rewritten_application.map(|application| {
                interner.intern(Type::Application(application))
            })
        })
}

const fn argument_context(
    application: &Application,
    ctx: RewriteContext,
) -> RewriteContext {
    match application.constructor {
        Constructor::FunctionPointer(_) => ctx.enter_binder(),
        Constructor::Primitive(_)
        | Constructor::Lifetime(_)
        | Constructor::Reference(_)
        | Constructor::Symbolic(_)
        | Constructor::Tuple(_)
        | Constructor::AnonymousTraitInstance(_)
        | Constructor::InstanceAssociated(_) => ctx,
    }
}

fn collect_rewritten_argument(
    new_arguments: &mut Option<Vec<Interned<Type>>>,
    current_arguments: &[Interned<Type>],
    index: usize,
    argument: &Interned<Type>,
    rewritten_argument: Option<Interned<Type>>,
) {
    match (rewritten_argument, new_arguments.as_mut()) {
        (None, None) => {}

        (None, Some(new_arguments)) => {
            new_arguments.push(argument.clone());
        }

        (Some(rewritten_argument), None) => {
            let mut new_arguments_vec =
                Vec::with_capacity(current_arguments.len());

            new_arguments_vec
                .extend(current_arguments[..index].iter().cloned());
            new_arguments_vec.push(rewritten_argument);

            *new_arguments = Some(new_arguments_vec);
        }

        (Some(rewritten_argument), Some(new_arguments)) => {
            new_arguments.push(rewritten_argument);
        }
    }
}

fn rewritten_application(
    application: &Application,
    new_arguments: Option<Vec<Interned<Type>>>,
) -> Option<Application> {
    new_arguments.map(|arguments| Application {
        constructor: application.constructor.clone(),
        arguments: Interned::new_duplicating_unsized(arguments),
    })
}

#[cfg(test)]
mod test;
