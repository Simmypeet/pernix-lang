use std::ops::Deref;

use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_target::Global;

use crate::{capture::Captures, scope, Values};

pub trait Environment {
    fn captures(&self) -> Option<&Captures>;

    fn values(&self) -> &Values;

    fn tracked_engine(&self) -> &TrackedEngine;

    fn current_site(&self) -> Global<pernixc_symbol::ID>;

    fn scope_tree(&self) -> &scope::Tree;
}

pub trait Typer<V>: Environment {
    type Type: Deref<Target = pernixc_term::r#type::Type>;
    type Error: From<CyclicError>;

    fn type_of<'s, 'v>(
        &'s self,
        value: &'v V,
    ) -> impl std::future::Future<Output = Result<Self::Type, Self::Error>>
           + use<'s, 'v, Self, V>;
}
