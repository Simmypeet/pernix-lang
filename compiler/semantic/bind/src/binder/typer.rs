//! Implements a [`Typer`] interface for the binder.

use std::ops::Deref;

use pernixc_handler::Handler;
use pernixc_ir::{address::Address, typer::Typer, value::TypeOf};
use pernixc_semantic_element::parameter::get_parameters;
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
    type Type = DerefWrapper<pernixc_term::r#type::Type>;

    type Error = UnrecoverableError;

    async fn type_of<E: pernixc_ir::typer::Environment>(
        &self,
        value: &Address,
        env: &E,
    ) -> Result<Self::Type, Self::Error> {
        match env
            .values()
            .type_of(
                value,
                &pernixc_ir::value::Environment::builder()
                    .maybe_captures(env.captures())
                    .current_site(env.current_site())
                    .type_environment(&self.ty_environment)
                    .build(),
            )
            .await
        {
            Ok(ty) => Ok(DerefWrapper(ty.result)),
            Err(err) => {
                let span = match value.get_root_memory() {
                    pernixc_ir::address::Memory::Parameter(id) => {
                        let parameters = env
                            .tracked_engine()
                            .get_parameters(env.current_site())
                            .await?;

                        parameters.parameters[*id].span.unwrap()
                    }
                    pernixc_ir::address::Memory::Alloca(id) => {
                        env.values().allocas[*id].span.unwrap()
                    }
                    pernixc_ir::address::Memory::Capture(id) => {
                        env.captures().unwrap()[*id].span.unwrap()
                    }
                };

                Err(err
                    .report_as_type_calculating_overflow(span, &self.handler))
            }
        }
    }
}

/// Implements the [`pernixc_ir::typer::Environment`] interface for the binder.
#[derive(Debug, Clone, Copy)]
pub struct Environment<'s> {
    captures: Option<&'s pernixc_ir::capture::Captures>,
    tracked_engine: &'s pernixc_query::TrackedEngine,
    current_site: pernixc_target::Global<pernixc_symbol::ID>,
    scope_tree: &'s pernixc_ir::scope::Tree,
    values: &'s pernixc_ir::Values,
}

impl pernixc_ir::typer::Environment for Environment<'_> {
    fn captures(&self) -> Option<&pernixc_ir::capture::Captures> {
        self.captures
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
        }
    }
}
