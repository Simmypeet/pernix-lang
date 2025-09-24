//! Contains the definition of [`Display`], a custom alternative of the
//! [`std::fmt::Display`].

use bon::Builder;
use flexstr::SharedStr;
use getset::{CopyGetters, Getters};
use pernixc_hash::HashMap;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    constant::Constant,
    generic_arguments::GenericArguments,
    inference,
    lifetime::{ElidedLifetimeID, Forall, Lifetime},
    r#type::Type,
};

/// Specifies how to write the inference variable during formatting.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
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

/// Configuration object for formatting terms.
#[derive(Debug, Builder, CopyGetters)]
pub struct Configuration<'y> {
    /// Mapping for lifetime inference variables.
    #[get_copy = "pub"]
    lifetime_inferences: Option<&'y InferenceRenderingMap<Lifetime>>,
    /// Mapping for type inference variables.
    #[get_copy = "pub"]
    type_inferences: Option<&'y InferenceRenderingMap<Type>>,
    /// Mapping for constant inference variables.
    #[get_copy = "pub"]
    constant_inferences: Option<&'y InferenceRenderingMap<Constant>>,
    /// Mapping for elided lifetime IDs to their names.
    #[get_copy = "pub"]
    edlided_lifetimes: Option<&'y HashMap<ElidedLifetimeID, SharedStr>>,
}

impl Configuration<'_> {
    /// Checks whether the given lifetime will be displayed when formatting.
    #[must_use]
    pub fn lifetime_will_be_displayed(&self, lifetime: &Lifetime) -> bool {
        match lifetime {
            Lifetime::Inference(variable) => self
                .lifetime_inferences
                .is_some_and(|x| x.contains_key(variable)),

            Lifetime::Elided(member_id) => self
                .edlided_lifetimes
                .is_some_and(|x| x.contains_key(member_id)),

            Lifetime::Parameter(_) | Lifetime::Forall(_) | Lifetime::Static => {
                true
            }

            Lifetime::Erased | Lifetime::Error(_) => false,
        }
    }

    /// Determines whether the generic arguments will be displayed using
    /// [`crate::display::Display`].
    #[must_use]
    pub fn generic_arguments_will_be_displayed(
        &self,
        generic_arguments: &GenericArguments,
    ) -> bool {
        generic_arguments
            .lifetimes
            .iter()
            .any(|x| self.lifetime_will_be_displayed(x))
            || !generic_arguments.types.is_empty()
            || !generic_arguments.constants.is_empty()
    }
}

/// Stores the internal buffer for formatting.
#[derive(Builder, Getters)]
pub struct Formatter<'x, 'y> {
    buffer: &'x mut (dyn std::fmt::Write + Send),

    forall_lifetime_names: HashMap<Forall, usize>,

    /// Configuration for formatting.
    #[get = "pub"]
    configuration: &'y Configuration<'y>,
}

impl Formatter<'_, '_> {
    /// Obtains the forall lifetime number in the current formatting session.
    pub fn forall_lifetime_names(&mut self, forall_lifetime: &Forall) -> usize {
        let len = self.forall_lifetime_names.len();

        *self
            .forall_lifetime_names
            .entry(forall_lifetime.clone())
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
                configuration: &Configuration::builder().build(),
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
            let configuration = Configuration::builder()
                .maybe_lifetime_inferences(lifetime_inference_map)
                .maybe_type_inferences(type_inference_map)
                .maybe_constant_inferences(constant_inference_map)
                .build();

            let mut formatter = Formatter {
                buffer,
                forall_lifetime_names: HashMap::default(),
                configuration: &configuration,
            };

            self.fmt(engine, &mut formatter).await
        }
    }

    /// Writes the value into a string
    fn write_to_string(
        &self,
        engine: &TrackedEngine,
    ) -> impl std::future::Future<Output = Result<String, std::fmt::Error>>
    {
        async move {
            let mut string = String::new();
            self.write_async(engine, &mut string).await?;

            Result::<_, std::fmt::Error>::Ok(string)
        }
    }

    /// Writes the value into a string, using the given formatting
    /// configuration.c
    fn write_to_string_with_configuration(
        &self,
        engine: &TrackedEngine,
        configuration: &Configuration,
    ) -> impl std::future::Future<Output = Result<String, std::fmt::Error>>
    {
        async move {
            let mut string = String::new();
            let mut formatter = Formatter {
                buffer: &mut string,
                forall_lifetime_names: HashMap::default(),
                configuration,
            };

            self.fmt(engine, &mut formatter).await?;

            Result::<_, std::fmt::Error>::Ok(string)
        }
    }
}
