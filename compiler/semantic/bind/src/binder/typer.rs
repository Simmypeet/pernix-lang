//! Implements a [`Typer`] interface for the binder.

use std::ops::Deref;

use pernixc_handler::Handler;
use pernixc_ir::{address::Address, typer::Typer, value::TypeOf};
use pernixc_stable_hash::StableHash;
use pernixc_type_system::UnrecoverableError;

use crate::binder::{inference_context::InferenceContext, Binder};

/// The struct that implements the [`Typer`] interface for the binder.
pub struct BinderTyper<'x> {
    ty_environment:
        pernixc_type_system::environment::Environment<'x, InferenceContext>,
    handler: &'x dyn Handler<crate::diagnostic::Diagnostic>,
}

impl std::fmt::Debug for BinderTyper<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BinderTyper").finish_non_exhaustive()
    }
}

/// An owned wrapper around a type that implements `Deref` to a type.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, StableHash,
)]
pub struct DerefWrapper<T>(T);

impl<T> Deref for DerefWrapper<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target { &self.0 }
}

impl Typer<Address> for BinderTyper<'_> {
    type Type<'s>
        = DerefWrapper<pernixc_term::r#type::Type>
    where
        Self: 's;

    type Error = UnrecoverableError;

    async fn type_of<E: pernixc_ir::typer::Environment>(
        &self,
        value: &Address,
        env: &E,
    ) -> Result<DerefWrapper<pernixc_term::r#type::Type>, UnrecoverableError>
    {
        let environment = pernixc_ir::value::Environment::builder()
            .maybe_captures(env.captures())
            .current_site(env.current_site())
            .type_environment(&self.ty_environment)
            .build();

        match env.values().type_of(value, &environment).await {
            Ok(ty) => Ok(DerefWrapper(ty.result)),
            Err(err) => Err(err.report_as_type_calculating_overflow(
                env.values()
                    .span_of_memory(value.get_root_memory(), &environment)
                    .await?
                    .unwrap(),
                &self.handler,
            )),
        }
    }
}

/// Implements the [`pernixc_ir::typer::Environment`] interface for the binder.
#[derive(Debug, Clone, Copy)]
pub struct Environment<'s> {
    captures: Option<&'s pernixc_ir::capture::Captures>,
    closure_parameters:
        Option<&'s pernixc_ir::closure_parameters::ClosureParameters>,
    tracked_engine: &'s pernixc_query::TrackedEngine,
    current_site: pernixc_target::Global<pernixc_symbol::ID>,
    scope_tree: &'s pernixc_ir::scope::Tree,
    values: &'s pernixc_ir::Values,
}

impl pernixc_ir::typer::Environment for Environment<'_> {
    fn captures(&self) -> Option<&pernixc_ir::capture::Captures> {
        self.captures
    }

    fn closure_parameters(
        &self,
    ) -> Option<&pernixc_ir::closure_parameters::ClosureParameters> {
        self.closure_parameters
    }

    fn values(&self) -> &pernixc_ir::Values { self.values }

    fn tracked_engine(&self) -> &pernixc_query::TrackedEngine {
        self.tracked_engine
    }

    fn current_site(&self) -> pernixc_target::Global<pernixc_symbol::ID> {
        self.current_site
    }

    fn scope_tree(&self) -> &pernixc_ir::scope::Tree { self.scope_tree }
}

impl Binder<'_> {
    /// Creates a typer for the binder.
    #[must_use]
    pub fn typer<'s>(
        &'s self,
        handler: &'s dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> BinderTyper<'s> {
        BinderTyper { ty_environment: self.create_environment(), handler }
    }

    /// Creates a typer environment for the binder.
    #[must_use]
    pub fn typer_environment(&self) -> Environment<'_> {
        Environment {
            captures: self.captures(),
            tracked_engine: self.engine(),
            current_site: self.current_site(),
            scope_tree: self.scope_tree(),
            values: self.values(),
            closure_parameters: self.closure_parameters(),
        }
    }
}
