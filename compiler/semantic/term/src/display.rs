//! Contains the definition of [`Display`], a custom alternative of the
//! [`std::fmt::Display`].

use flexstr::SharedStr;
use pernixc_hash::HashMap;
use pernixc_query::TrackedEngine;

use crate::{
    constant::Constant,
    inference,
    lifetime::{Forall, Lifetime},
    r#type::Type,
};

/// Specifies how to write the inference variable during formatting.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InferenceRendering<T> {
    /// Attempt to recursively write the inference variable using the given
    /// term.
    Recurse(T),

    /// Write the inference variable as the given string.
    Rendered(SharedStr),
}

/// Maps inference variables to their rendering strategy.
pub type InferenceRenderingMap<T> =
    HashMap<inference::Variable<T>, InferenceRendering<T>>;

/// Stores the internal buffer for formatting.
pub struct Formatter<'x, 'y> {
    buffer: &'x mut (dyn std::fmt::Write + Send),
    forall_lifetime_names: HashMap<Forall, usize>,

    pub(crate) lifetime_inference_map:
        Option<&'y InferenceRenderingMap<Lifetime>>,
    pub(crate) type_inference_map: Option<&'y InferenceRenderingMap<Type>>,
    pub(crate) constant_inference_map:
        Option<&'y InferenceRenderingMap<Constant>>,
}

impl Formatter<'_, '_> {
    /// Obtains the forall lifetime number in the current formatting session.
    pub fn forall_lifetime_names(&mut self, forall_lifetime: &Forall) -> usize {
        let len = self.forall_lifetime_names.len();

        *self
            .forall_lifetime_names
            .entry(*forall_lifetime)
            .or_insert_with(|| len)
    }
}

impl std::fmt::Debug for Formatter<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Formatter").finish_non_exhaustive()
    }
}

impl std::fmt::Write for Formatter<'_, '_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.buffer.write_str(s)
    }
}

/// A specialized alternative to [`std::fmt::Display`] supporting async and
/// allows access to the [`TrackedEngine`].
pub trait Display: Send + Sync {
    /// Formats the value using the given [`TrackedEngine`] and [`Formatter`].
    fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut Formatter,
    ) -> impl std::future::Future<Output = std::fmt::Result>;

    /// Writes the value asynchronously into the given buffer.
    fn write_async<W: std::fmt::Write + Send>(
        &self,
        engine: &TrackedEngine,
        buffer: &mut W,
    ) -> impl std::future::Future<Output = std::fmt::Result> {
        async move {
            let mut formatter = Formatter {
                buffer,
                forall_lifetime_names: HashMap::default(),
                lifetime_inference_map: None,
                type_inference_map: None,
                constant_inference_map: None,
            };

            self.fmt(engine, &mut formatter).await
        }
    }

    /// Writes the value asynchronously into the given buffer, using the given
    /// inference variable mappings.
    fn write_async_with_mapping<W: std::fmt::Write + Send>(
        &self,
        engine: &TrackedEngine,
        buffer: &mut W,
        lifetime_inference_map: Option<&InferenceRenderingMap<Lifetime>>,
        type_inference_map: Option<&InferenceRenderingMap<Type>>,
        constant_inference_map: Option<&InferenceRenderingMap<Constant>>,
    ) -> impl std::future::Future<Output = std::fmt::Result> {
        async move {
            let mut formatter = Formatter {
                buffer,
                forall_lifetime_names: HashMap::default(),
                lifetime_inference_map,
                type_inference_map,
                constant_inference_map,
            };

            self.fmt(engine, &mut formatter).await
        }
    }
}
