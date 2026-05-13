//! Contains the definition of [`Display`], a custom alternative to
//! [`std::fmt::Display`].

use std::fmt::Write as _;

use bon::Builder;
use getset::{CopyGetters, Getters};
use pernixc_hash::FxHashMap;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::name::{get_name, get_qualified_name};
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::{
    Term,
    constant::Constant,
    generic_arguments::GenericArguments,
    inference,
    instance::Instance,
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
    Encode,
    Decode,
)]
pub enum InferenceRendering<T> {
    /// Attempt to recursively write the inference variable using the given
    /// term.
    Recurse(T),

    /// Write the inference variable as the given string.
    Rendered(Interned<str>),
}

/// Maps inference variables to their rendering strategy.
pub type InferenceRenderingMap<T> =
    FxHashMap<inference::Variable<T>, InferenceRendering<T>>;

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

    /// Mapping for instance inference variables.
    #[get_copy = "pub"]
    instance_inferences: Option<&'y InferenceRenderingMap<Instance>>,

    /// Mapping for elided lifetime IDs to their names.
    #[get_copy = "pub"]
    elided_lifetimes: Option<&'y FxHashMap<ElidedLifetimeID, Interned<str>>>,

    /// Whether to only display the last segment of qualified identifiers.
    #[get_copy = "pub"]
    #[builder(default = false)]
    short_qualified_identifiers: bool,
}

impl Configuration<'_> {
    /// Checks whether the given lifetime will be displayed when formatting.
    #[must_use]
    pub fn lifetime_will_be_displayed(&self, lifetime: &Lifetime) -> bool {
        match lifetime {
            Lifetime::Inference(variable) => self
                .lifetime_inferences
                .is_some_and(|m| m.contains_key(variable)),

            Lifetime::Elided(member_id) => {
                self.elided_lifetimes.is_some_and(|m| m.contains_key(member_id))
            }

            Lifetime::Parameter(_) | Lifetime::Forall(_) | Lifetime::Static => {
                true
            }

            Lifetime::Erased | Lifetime::Error(_) => false,
        }
    }

    /// Determines whether the generic arguments will be displayed.
    #[must_use]
    pub fn generic_arguments_will_be_displayed(
        &self,
        generic_arguments: &GenericArguments,
    ) -> bool {
        generic_arguments
            .lifetimes()
            .iter()
            .any(|x| self.lifetime_will_be_displayed(x.as_ref()))
            || !generic_arguments.types().is_empty()
            || !generic_arguments.constants().is_empty()
            || !generic_arguments.instances().is_empty()
    }
}

/// Stores the internal buffer for formatting.
#[derive(Builder, Getters)]
pub struct Formatter<'x, 'y> {
    buffer: &'x mut (dyn std::fmt::Write + Send),

    forall_lifetime_names: FxHashMap<Forall, usize>,

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
/// allowing access to the [`TrackedEngine`].
pub trait Display: Send + Sync {
    /// Formats the value using the given [`TrackedEngine`] and [`Formatter`].
    fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut Formatter<'_, '_>,
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
                forall_lifetime_names: FxHashMap::default(),
                configuration: &Configuration::builder().build(),
            };

            self.fmt(engine, &mut formatter).await
        }
    }

    /// Writes the value asynchronously into the given buffer with the given
    /// formatting configuration.
    fn write_async_with_configuration(
        &self,
        engine: &TrackedEngine,
        buffer: &mut (dyn std::fmt::Write + Send),
        configuration: &Configuration<'_>,
    ) -> impl std::future::Future<Output = std::fmt::Result> {
        async move {
            let mut formatter = Formatter {
                buffer,
                forall_lifetime_names: FxHashMap::default(),
                configuration,
            };

            self.fmt(engine, &mut formatter).await
        }
    }

    /// Writes the value into a string.
    fn write_to_string(
        &self,
        engine: &TrackedEngine,
    ) -> impl std::future::Future<Output = Result<String, std::fmt::Error>>
    {
        async move {
            let mut string = String::new();
            self.write_async(engine, &mut string).await?;

            Ok(string)
        }
    }

    /// Writes the value into a string with the given formatting configuration.
    fn write_to_string_with_configuration(
        &self,
        engine: &TrackedEngine,
        configuration: &Configuration<'_>,
    ) -> impl std::future::Future<Output = Result<String, std::fmt::Error>>
    {
        async move {
            let mut string = String::new();
            let mut formatter = Formatter {
                buffer: &mut string,
                forall_lifetime_names: FxHashMap::default(),
                configuration,
            };

            self.fmt(engine, &mut formatter).await?;
            Ok(string)
        }
    }
}

impl<T> Display for Interned<T>
where
    T: Display + Send + Sync,
{
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut Formatter<'_, '_>,
    ) -> std::fmt::Result {
        self.as_ref().fmt(engine, formatter).await
    }
}

impl Display for Term {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut Formatter<'_, '_>,
    ) -> std::fmt::Result {
        match self {
            Self::Constant(constant) => constant.fmt(engine, formatter).await,
            Self::Lifetime(lifetime) => lifetime.fmt(engine, formatter).await,
            Self::Type(r#type) => r#type.fmt(engine, formatter).await,
            Self::Instance(instance) => instance.fmt(engine, formatter).await,
        }
    }
}

/// Displays a symbol name together with the supplied generic arguments.
#[derive(Debug)]
pub(crate) struct DisplaySymbolWithGenericArguments<'a> {
    global_id: pernixc_target::Global<pernixc_symbol::SymbolID>,
    generic_arguments: &'a GenericArguments,
}

impl<'a> DisplaySymbolWithGenericArguments<'a> {
    #[must_use]
    pub(crate) const fn new(
        global_id: pernixc_target::Global<pernixc_symbol::SymbolID>,
        generic_arguments: &'a GenericArguments,
    ) -> Self {
        Self { global_id, generic_arguments }
    }
}

impl Display for DisplaySymbolWithGenericArguments<'_> {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut Formatter<'_, '_>,
    ) -> std::fmt::Result {
        if formatter.configuration().short_qualified_identifiers() {
            let name = engine.get_name(self.global_id).await;
            write!(formatter, "{}", &*name)?;
        } else {
            let qualified_name =
                engine.get_qualified_name(self.global_id).await;
            write!(formatter, "{qualified_name}")?;
        }

        self.generic_arguments.fmt(engine, formatter).await
    }
}
