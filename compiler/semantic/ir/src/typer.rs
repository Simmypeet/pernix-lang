//! An interface for retrieving types of values.

use std::ops::Deref;

use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_target::Global;

use crate::{
    capture::Captures, closure_parameters::ClosureParameters, scope, Values,
};

/// Represents the environment in which the analysis takes place.
pub trait Environment {
    /// Retrieves the captures associated with the environment (the analysis is
    /// taking place in a closure).
    fn captures(&self) -> Option<&Captures>;

    /// Retrieves the closure parameters associated with the environment (the
    /// analysis is taking place in a closure).
    fn closure_parameters(&self) -> Option<&ClosureParameters>;

    /// Retrieves the [`Values`]. The central repository for retrieving values.
    fn values(&self) -> &Values;

    /// Retrieves the [`TrackedEngine`] used for query additional information.
    fn tracked_engine(&self) -> &TrackedEngine;

    /// The site where the analysis is taking place.
    fn current_site(&self) -> Global<pernixc_symbol::ID>;

    /// Retrieves the scope tree.
    fn scope_tree(&self) -> &scope::Tree;
}

/// An interface for retrieving types of values.
///
/// This serves as a bridge between the analyzer on the IR and the type
/// provider.
pub trait Typer<V> {
    /// The object representing the [`pernixc_term::r#type::Type`] of a value.
    ///
    /// This type could possibly be a simple reference to the type or a complex
    /// structure that encapsulates additional information about the type.
    type Type<'s>: Deref<Target = pernixc_term::r#type::Type>
    where
        Self: 's;

    /// The error type that can be returned by the `type_of` method.
    ///
    /// This error type is used to handle unexpected errors that may occur
    /// during calculation of the type of a value. Most likely some form of
    /// overflow calculation or cyclic query detection.
    type Error: From<CyclicError>;

    /// Retrieve the type of a value.
    fn type_of<'s, 'v, 'e, E: Environment>(
        &'s self,
        value: &'v V,
        env: &'e E,
    ) -> impl std::future::Future<Output = Result<Self::Type<'s>, Self::Error>>
           + use<'s, 'v, 'e, Self, V, E>;
}
