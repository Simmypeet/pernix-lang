//! Contains the logic for resolving syntax tree into some form of semantic
//! term/information.

use std::{collections::hash_map::Entry, pin::Pin};

use bon::Builder;
use pernixc_handler::Handler;
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_syntax::item::HigherRankedLifetimes;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_parameters::InstanceParameterID,
    instance::Instance,
    lifetime::{Forall, Lifetime, NamedForall},
    r#type::Type,
};
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::{
    diagnostic::{Diagnostic, ForallLifetimeRedefinition},
    qualified_identifier::Resolution,
};

pub mod diagnostic;
pub mod generic_parameter_namespace;
pub mod qualified_identifier;
pub mod term;

/// A trait for providing elided terms.
pub trait ElidedTermProvider<T>: Send {
    /// Creates a new instance of the term to supply the required missing term.
    fn create(&mut self) -> T;
}

/// The extra namespace that is used to resolve the symbols prior to the
/// resolution process.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    StableHash,
    Encode,
    Decode,
    Identifiable,
    Builder,
)]
#[allow(missing_docs)]
pub struct ExtraNamespace {
    #[builder(default = HashMap::default(), into)]
    lifetimes: HashMap<Interned<str>, Lifetime>,

    #[builder(default = HashMap::default(), into)]
    types: HashMap<Interned<str>, Type>,

    #[builder(default = HashMap::default(), into)]
    constants: HashMap<Interned<str>, Constant>,

    #[builder(default = HashMap::default(), into)]
    instances: HashMap<Interned<str>, Instance>,
}

impl ExtraNamespace {
    /// Inserts a lifetime into the extra namespace with the given name.
    pub fn insert_lifetime(&mut self, name: Interned<str>, lifetime: Lifetime) {
        self.lifetimes.insert(name, lifetime);
    }

    /// Inserts a type into the extra namespace with the given name.
    pub fn insert_type(&mut self, name: Interned<str>, ty: Type) {
        self.types.insert(name, ty);
    }

    /// Inserts a constant into the extra namespace with the given name.
    pub fn insert_constant(&mut self, name: Interned<str>, constant: Constant) {
        self.constants.insert(name, constant);
    }

    /// Inserts an instance into the extra namespace with the given name.
    pub fn insert_instance(&mut self, name: Interned<str>, instance: Instance) {
        self.instances.insert(name, instance);
    }

    /// Returns the lifetime with the given name from the extra namespace, if
    /// exists.
    #[must_use]
    pub fn get_lifetime(&self, name: &str) -> Option<&Lifetime> {
        self.lifetimes.get(name)
    }
}

/// An RAII wrapper for the extra namespace including the forall lifetimes
/// created from the syntax tree, which ensures the created forall lifetimes are
/// removed from the extra namespace when dropped.
#[derive(Debug)]
pub struct ExtraNamespaceWithForallLifetimes<'x> {
    extra_namespace: &'x mut ExtraNamespace,
    created_forall_lifetimes: Vec<Interned<str>>,
}

impl<'x> ExtraNamespaceWithForallLifetimes<'x> {
    /// Creates a new instance of the extra namespace with the forall lifetimes
    /// from the given syntax tree and inserts them into the extra namespace.
    pub fn new(
        extra_namespace: &'x mut ExtraNamespace,
        forall_lifetimes: Option<&HigherRankedLifetimes>,
        handler: &dyn Handler<ForallLifetimeRedefinition>,
    ) -> Self {
        let mut created_lifetimes = Vec::new();

        if let Some(forall_lifetimes) = forall_lifetimes
            .and_then(pernixc_syntax::item::HigherRankedLifetimes::lifetimes)
        {
            for forall_lifetime in
                forall_lifetimes.lifetimes().filter_map(|x| x.identifier())
            {
                match extra_namespace
                    .lifetimes
                    .entry(forall_lifetime.kind.0.clone())
                {
                    Entry::Vacant(entry) => {
                        let lifetime =
                            Lifetime::Forall(Forall::Named(NamedForall::new(
                                forall_lifetime.span,
                                forall_lifetime.kind.0.clone(),
                            )));

                        entry.insert(lifetime.clone());

                        created_lifetimes.push(forall_lifetime.kind.0.clone());
                    }

                    Entry::Occupied(_) => {
                        handler.receive(
                            ForallLifetimeRedefinition::builder()
                                .redefinition_span(forall_lifetime.span)
                                .build(),
                        );
                    }
                }
            }
        }

        Self { extra_namespace, created_forall_lifetimes: created_lifetimes }
    }

    /// Creates a new instance of the extra namespace with the forall lifetimes
    /// from the given syntax tree and inserts them into the extra namespace.
    #[must_use]
    pub fn nest<'a>(
        &'a mut self,
        forall_lifetimes: Option<&HigherRankedLifetimes>,
        handler: &dyn Handler<ForallLifetimeRedefinition>,
    ) -> ExtraNamespaceWithForallLifetimes<'a> {
        ExtraNamespaceWithForallLifetimes::new(
            self.extra_namespace,
            forall_lifetimes,
            handler,
        )
    }

    /// Returns the [`ExtraNamespace`] with the created forall lifetimes.
    #[must_use]
    pub const fn extra_namespace(&self) -> &ExtraNamespace {
        self.extra_namespace
    }
}

impl Drop for ExtraNamespaceWithForallLifetimes<'_> {
    fn drop(&mut self) {
        for lifetime in &self.created_forall_lifetimes {
            self.extra_namespace.lifetimes.remove(lifetime);
        }
    }
}

/// A trait for observing the resolution process.
///
/// The trait will be notified when a type, lifetime, or constant is resolved
/// during the resolution process.
///
/// This is useful for collecting the resolved terms for further well-formedness
/// checks such as checking whether the where clause is satisfied; those checks
/// are not performed during the resolution process.
pub trait Observer: Send {
    /// Notifies the observer when a resolution is resolved.
    fn on_resolution_resolved(
        &mut self,
        tracked_engine: &TrackedEngine,
        referring_site: Global<pernixc_symbol::ID>,
        resolution: &Resolution,
        span: &RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    );

    /// Notifies the observer when a type is resolved.
    fn on_type_resolved(
        &mut self,
        tracked_engine: &TrackedEngine,
        referring_site: Global<pernixc_symbol::ID>,
        ty: &Type,
        syntax_tree: &pernixc_syntax::r#type::Type,
        handler: &dyn Handler<Diagnostic>,
    );

    /// Notifies the observer when a lifetime is resolved.
    fn on_lifetime_resolved(
        &mut self,
        tracked_engine: &TrackedEngine,
        referring_site: Global<pernixc_symbol::ID>,
        lifetime: &Lifetime,
        syntax_tree: &pernixc_syntax::Lifetime,
        handler: &dyn Handler<Diagnostic>,
    );

    /// Notifies the observer when a constant is resolved.
    fn on_constant_arguments_resolved(
        &mut self,
        tracked_engine: &TrackedEngine,
        referring_site: Global<pernixc_symbol::ID>,
        constant: &Constant,
        syntax_tree: &pernixc_syntax::expression::Expression,
        handler: &dyn Handler<Diagnostic>,
    );

    /// Notifies the observer when a type is resolved as an unpacked element
    /// in a tuple type.
    fn on_unpacked_type_resolved(
        &mut self,
        tracked_engine: &TrackedEngine,
        referring_site: Global<pernixc_symbol::ID>,
        ty: &Type,
        syntax_tree: &pernixc_syntax::r#type::Unpackable,
        handler: &dyn Handler<Diagnostic>,
    );

    /// Notifies the observer when a constant is resolved as an unpacked element
    /// in a tuple constant.
    fn on_unpacked_constant_resolved(
        &mut self,
        tracked_engine: &TrackedEngine,
        referring_site: Global<pernixc_symbol::ID>,
        constant: &Constant,
        syntax_tree: &pernixc_syntax::expression::Expression,
        handler: &dyn Handler<Diagnostic>,
    );
}

/// An ad-hoc trait for resolving the instance's trait ref.
///
/// This is primarily used to avoid cyclic query when building "generic
/// parameter" item and when resolving from the trait ref of the instance during
/// the resolution, which requires the information of the "generic parameter"
/// item as well.
pub trait ResolveInstanceParameterTraitRef: Send + Sync {
    /// Resolves the instance parameter's trait ref and returns the trait ID.
    fn resolve_instance_parameter_trait_ref<'a>(
        &'a self,
        instance_parameter: &'a InstanceParameterID,
    ) -> Pin<
        Box<
            dyn Future<Output = Option<Global<pernixc_symbol::ID>>> + Send + 'a,
        >,
    >;
}

/// The configuration struct specifying the behaviour of the resolution process.
#[derive(Default, Builder)]
#[allow(missing_debug_implementations)]
pub struct Config<'lp, 'tp, 'cp, 'ob, 'ex> {
    /* These lifetimes are such a bad idea */
    /// If specified, when the lifetime argument is elided, the provider will
    /// be used to supply the missing required lifetimes.
    pub elided_lifetime_provider:
        Option<&'lp mut dyn ElidedTermProvider<Lifetime>>,

    /// If specified, when the type argument is elided, the provider will be
    /// used to supply the missing required types.
    pub elided_type_provider: Option<&'tp mut dyn ElidedTermProvider<Type>>,

    /// If specified, when the constant argument is elided, the provider will
    /// be used to supply the missing required constants.
    pub elided_constant_provider:
        Option<&'cp mut dyn ElidedTermProvider<Constant>>,

    /// If specified, when the instance argument is elided, the provider will
    /// be used to supply the missing required instances.
    pub elided_instance_provider:
        Option<&'cp mut dyn ElidedTermProvider<Instance>>,

    /// If specified, during the resolution process, the observer will be
    /// notified each time a type, lifetime, or constant is resolved.
    pub observer: Option<&'ob mut dyn Observer>,

    /// If specified, the extra namespace will be used to resolve the symbols
    /// prior to the resolution process.
    ///
    /// This is useful for including the symbols that are not directly defined
    /// in the table but are acessible such as higher-ranked lifetimes
    /// `for['x]`.
    pub extra_namespace: Option<&'ex ExtraNamespace>,

    /// If specified, when an instance is resolved and requires to resolve its
    /// trait ref, the resolver will call this to resolve the trait ref of the
    /// instance parameter instead of querying the information from the global
    /// engine.
    pub resolve_instance_parameter_trait_ref:
        Option<&'ex dyn ResolveInstanceParameterTraitRef>,

    /// If `true`, the resolution will search for symbols inside the ADT's
    /// implements
    ///
    /// This flag is primarily used when resolving a qualified identifier for
    /// the `implements` but cyclic query might happen when the resolution
    /// considers the ADT implementations as well.
    #[builder(default = true)]
    pub consider_adt_implements: bool,

    /// Represents the site where the resolution occurred.
    pub referring_site: Global<pernixc_symbol::ID>,
}

impl Config<'_, '_, '_, '_, '_> {
    /// Creates a new instance of the config.
    #[allow(clippy::option_if_let_else)]
    pub fn reborrow(&mut self) -> Config<'_, '_, '_, '_, '_> {
        Config {
            elided_lifetime_provider: match &mut self.elided_lifetime_provider {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            elided_type_provider: match &mut self.elided_type_provider {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            elided_constant_provider: match &mut self.elided_constant_provider {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            elided_instance_provider: match &mut self.elided_instance_provider {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            observer: match &mut self.observer {
                Some(observer) => Some(&mut **observer),
                None => None,
            },
            extra_namespace: self.extra_namespace,
            consider_adt_implements: self.consider_adt_implements,
            referring_site: self.referring_site,
            resolve_instance_parameter_trait_ref: self
                .resolve_instance_parameter_trait_ref,
        }
    }
}

/// The error type occurred during the resolution.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
#[allow(missing_docs)]
pub enum Error {
    /// Encounters a fatal error during the resolution.
    Abort,
}
