//! An interface for retrieving types of values.

use std::ops::Deref;

use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::parameter::Parameters;
use pernixc_target::Global;
use qbice::storage::intern::Interned;

use crate::{
    Values, capture::Captures, handling_scope::OperationHandlerID, scope,
};

/// Represents the environment in which the analysis takes place.
pub trait Environment: Send + Sync {
    /// Retrieves the captures associated with the environment (the analysis is
    /// taking place in a closure).
    fn captures(&self) -> Option<&Captures>;

    /// Retrieves the [`Values`]. The central repository for retrieving values.
    fn values(&self) -> &Values;

    /// Retrieves the [`TrackedEngine`] used for query additional information.
    fn tracked_engine(&self) -> &TrackedEngine;

    /// The site where the analysis is taking place.
    fn current_site(&self) -> Global<pernixc_symbol::SymbolID>;

    /// Retrieves the current operation handler ID, if it exists (the analysis
    /// is taking place in a handling scope).
    fn current_operation_handler_id(&self) -> Option<&OperationHandlerID>;

    /// Retrieves the handling scopes associated with the environment.
    fn handling_scopes(&self) -> &crate::handling_scope::HandlingScopes;

    /// Retrieves the scope tree.
    fn scope_tree(&self) -> &scope::Tree;

    fn current_operation_parameters(
        &self,
    ) -> impl Future<Output = Interned<Parameters>> + Send + '_ {
        async move {
            let operation_handler_id =
                self.current_operation_handler_id().unwrap();

            self.handling_scopes()
                .get_operation_handler_parameters(
                    *operation_handler_id,
                    self.tracked_engine(),
                )
                .await
        }
    }
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
    /// overflow calculation.
    type Error;

    /// Retrieve the type of a value.
    fn type_of<'s, 'v, 'e, E: Environment>(
        &'s self,
        value: &'v V,
        env: &'e E,
    ) -> impl std::future::Future<Output = Result<Self::Type<'s>, Self::Error>>
    + use<'s, 'v, 'e, Self, V, E>;
}
