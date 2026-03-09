//! Contains the [`Config`] struct and related types for configuring the
//! resolution process.

use std::pin::Pin;

use bon::Builder;
use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant, generic_parameters::InstanceParameterID,
    instance::Instance, lifetime::Lifetime, r#type::Type,
};

use crate::{
    ExtraNamespace, Observer, ResolveInstanceParameterTraitRef,
    diagnostic::Diagnostic, qualified_identifier::Resolution,
};

/// A trait for providing elided terms.
pub trait ElidedTermProvider<T>: Send {
    /// Creates a new instance of the term to supply the required missing term.
    fn create(&mut self) -> T;
}

/// The configuration struct specifying the behaviour of the resolution process.
#[derive(Builder)]
#[allow(missing_debug_implementations)]
pub struct Resolver<'i, 'm> {
    /// The tracked engine used for querying.
    tracked_engine: &'i TrackedEngine,

    /// The handler used for reporting diagnostics.
    handler: &'i dyn Handler<Diagnostic>,

    /// If specified, when the lifetime argument is elided, the provider will
    /// be used to supply the missing required lifetimes.
    elided_lifetime_provider: Option<&'m mut dyn ElidedTermProvider<Lifetime>>,

    /// If specified, when the type argument is elided, the provider will be
    /// used to supply the missing required types.
    elided_type_provider: Option<&'m mut dyn ElidedTermProvider<Type>>,

    /// If specified, when the constant argument is elided, the provider will
    /// be used to supply the missing required constants.
    elided_constant_provider: Option<&'m mut dyn ElidedTermProvider<Constant>>,

    /// If specified, when the instance argument is elided, the provider will
    /// be used to supply the missing required instances.
    elided_instance_provider: Option<&'m mut dyn ElidedTermProvider<Instance>>,

    /// If specified, during the resolution process, the observer will be
    /// notified each time a type, lifetime, or constant is resolved.
    observer: Option<&'m mut dyn Observer>,

    /// If specified, the extra namespace will be used to resolve the symbols
    /// prior to the resolution process.
    ///
    /// This is useful for including the symbols that are not directly defined
    /// in the table but are acessible such as higher-ranked lifetimes
    /// `for['x]`.
    extra_namespace: Option<&'i ExtraNamespace>,

    /// If specified, when an instance is resolved and requires to resolve its
    /// trait ref, the resolver will call this to resolve the trait ref of the
    /// instance parameter instead of querying the information from the global
    /// engine.
    resolve_instance_parameter_trait_ref:
        Option<&'i dyn ResolveInstanceParameterTraitRef>,

    /// If `true`, the resolution will search for symbols inside the ADT's
    /// implements
    ///
    /// This flag is primarily used when resolving a qualified identifier for
    /// the `implements` but cyclic query might happen when the resolution
    /// considers the ADT implementations as well.
    #[builder(default = true)]
    consider_adt_implements: bool,

    /// Represents the site where the resolution occurred.
    referring_site: Global<pernixc_symbol::ID>,
}

impl<'i, 'm> Resolver<'i, 'm> {
    // =========================================================================
    // Getters for basic fields
    // =========================================================================

    /// Returns a reference to the tracked engine.
    #[must_use]
    pub const fn tracked_engine(&self) -> &'i TrackedEngine {
        self.tracked_engine
    }

    /// Returns a reference to the handler.
    #[must_use]
    pub const fn handler(&self) -> &'i dyn Handler<Diagnostic> { self.handler }

    /// Returns the referring site where the resolution occurred.
    #[must_use]
    pub const fn referring_site(&self) -> Global<pernixc_symbol::ID> {
        self.referring_site
    }

    /// Returns whether to consider ADT implements during resolution.
    #[must_use]
    pub const fn consider_adt_implements(&self) -> bool {
        self.consider_adt_implements
    }

    // =========================================================================
    // Diagnostic delegation
    // =========================================================================

    /// Sends a diagnostic to the handler.
    pub fn receive_diagnostic(&self, diagnostic: Diagnostic) {
        self.handler.receive(diagnostic);
    }

    // =========================================================================
    // Observer delegation methods
    // =========================================================================

    /// Notifies the observer when a resolution is resolved.
    pub fn notify_resolution_resolved(
        &mut self,
        resolution: &Resolution,
        span: &RelativeSpan,
    ) {
        if let Some(observer) = self.observer.as_mut() {
            observer.on_resolution_resolved(
                self.tracked_engine,
                self.referring_site,
                resolution,
                span,
                self.handler,
            );
        }
    }

    /// Notifies the observer when a type is resolved.
    pub fn notify_type_resolved(
        &mut self,
        ty: &Type,
        syntax_tree: &pernixc_syntax::r#type::Type,
    ) {
        if let Some(observer) = self.observer.as_mut() {
            observer.on_type_resolved(
                self.tracked_engine,
                self.referring_site,
                ty,
                syntax_tree,
                self.handler,
            );
        }
    }

    /// Notifies the observer when a lifetime is resolved.
    pub fn notify_lifetime_resolved(
        &mut self,
        lifetime: &Lifetime,
        syntax_tree: &pernixc_syntax::Lifetime,
    ) {
        if let Some(observer) = self.observer.as_mut() {
            observer.on_lifetime_resolved(
                self.tracked_engine,
                self.referring_site,
                lifetime,
                syntax_tree,
                self.handler,
            );
        }
    }

    /// Notifies the observer when a constant is resolved as an unpacked
    /// element.
    pub fn notify_unpacked_type_resolved(
        &mut self,
        ty: &Type,
        syntax_tree: &pernixc_syntax::r#type::Unpackable,
    ) {
        if let Some(observer) = self.observer.as_mut() {
            observer.on_unpacked_type_resolved(
                self.tracked_engine,
                self.referring_site,
                ty,
                syntax_tree,
                self.handler,
            );
        }
    }

    // =========================================================================
    // Elided term provider delegation methods
    // =========================================================================

    /// Creates an elided lifetime term using the provider, if available.
    pub fn create_elided_lifetime(&mut self) -> Option<Lifetime> {
        self.elided_lifetime_provider.as_mut().map(|provider| provider.create())
    }

    /// Creates an elided type term using the provider, if available.
    pub fn create_elided_type(&mut self) -> Option<Type> {
        self.elided_type_provider.as_mut().map(|provider| provider.create())
    }

    /// Creates an elided constant term using the provider, if available.
    pub fn create_elided_constant(&mut self) -> Option<Constant> {
        self.elided_constant_provider.as_mut().map(|provider| provider.create())
    }

    /// Creates an elided instance term using the provider, if available.
    pub fn create_elided_instance(&mut self) -> Option<Instance> {
        self.elided_instance_provider.as_mut().map(|provider| provider.create())
    }

    /// Returns a mutable reference to the elided type provider, if available.
    pub fn elided_type_provider_mut(
        &mut self,
    ) -> Option<&mut (dyn ElidedTermProvider<Type> + 'm)> {
        self.elided_type_provider.as_deref_mut()
    }

    /// Returns a mutable reference to the elided constant provider, if
    /// available.
    pub fn elided_constant_provider_mut(
        &mut self,
    ) -> Option<&mut (dyn ElidedTermProvider<Constant> + 'm)> {
        self.elided_constant_provider.as_deref_mut()
    }

    /// Returns a mutable reference to the elided instance provider, if
    /// available.
    pub fn elided_instance_provider_mut(
        &mut self,
    ) -> Option<&mut (dyn ElidedTermProvider<Instance> + 'm)> {
        self.elided_instance_provider.as_deref_mut()
    }

    /// Returns whether an elided lifetime provider is available.
    #[must_use]
    pub fn has_elided_lifetime_provider(&self) -> bool {
        self.elided_lifetime_provider.is_some()
    }

    /// Returns whether an elided type provider is available.
    #[must_use]
    pub fn has_elided_type_provider(&self) -> bool {
        self.elided_type_provider.is_some()
    }

    /// Returns whether an elided constant provider is available.
    #[must_use]
    pub fn has_elided_constant_provider(&self) -> bool {
        self.elided_constant_provider.is_some()
    }

    /// Returns whether an elided instance provider is available.
    #[must_use]
    pub fn has_elided_instance_provider(&self) -> bool {
        self.elided_instance_provider.is_some()
    }

    // =========================================================================
    // Extra namespace delegation methods
    // =========================================================================

    /// Looks up a lifetime in the extra namespace.
    #[must_use]
    pub fn lookup_extra_lifetime(&self, name: &str) -> Option<Lifetime> {
        self.extra_namespace.and_then(|ns| ns.get_lifetime(name).cloned())
    }

    /// Looks up a type in the extra namespace.
    #[must_use]
    pub fn lookup_extra_type(&self, name: &str) -> Option<Type> {
        self.extra_namespace.and_then(|ns| ns.get_type(name).cloned())
    }

    /// Looks up a constant in the extra namespace.
    #[must_use]
    pub fn lookup_extra_constant(&self, name: &str) -> Option<Constant> {
        self.extra_namespace.and_then(|ns| ns.get_constant(name).cloned())
    }

    /// Looks up an instance in the extra namespace.
    #[must_use]
    pub fn lookup_extra_instance(&self, name: &str) -> Option<Instance> {
        self.extra_namespace.and_then(|ns| ns.get_instance(name).cloned())
    }

    // =========================================================================
    // Instance parameter trait ref resolution
    // =========================================================================

    /// Resolves the trait ref for an instance parameter.
    ///
    /// Returns `None` if no resolver is configured, otherwise returns the
    /// result of calling the resolver.
    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn resolve_instance_parameter_trait_ref<'a>(
        &'a self,
        instance_parameter: &'a InstanceParameterID,
    ) -> Option<
        Pin<
            Box<
                dyn Future<Output = Option<Global<pernixc_symbol::ID>>>
                    + Send
                    + 'a,
            >,
        >,
    > {
        self.resolve_instance_parameter_trait_ref.map(|resolver| {
            resolver.resolve_instance_parameter_trait_ref(instance_parameter)
        })
    }

    /// Returns whether a custom instance parameter trait ref resolver is
    /// configured.
    #[must_use]
    pub fn has_instance_parameter_trait_ref_resolver(&self) -> bool {
        self.resolve_instance_parameter_trait_ref.is_some()
    }
}

use std::future::Future;
