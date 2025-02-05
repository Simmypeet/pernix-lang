//! Implements the symbol resolution algorithm.

use std::collections::{hash_map::Entry, HashMap};

use pernixc_handler::Handler;
use pernixc_source_file::Span;
use pernixc_syntax::syntax_tree::{
    self, GenericIdentifier, QualifiedIdentifier, QualifiedIdentifierRoot,
};
use pernixc_table::{
    component::SymbolKind, diagnostic::Diagnostic,
    query::CyclicDependencyError, GlobalID, Table,
};
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    generic_parameter::{
        ConstantParameterID, GenericParameters, LifetimeParameterID,
        TypeParameterID,
    },
    lifetime::Lifetime,
    r#type::Type,
    Model,
};
use qualified_identifier::Resolution;

pub mod diagnostic;
pub mod qualified_identifier;
pub mod term;

/// A trait for providing elided terms.
pub trait ElidedTermProvider<T> {
    /// Creates a new instance of the term to supply the required missing term.
    fn create(&mut self) -> T;
}

/// The extra namespace that is used to resolve the symbols prior to the
/// resolution process.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct ExtraNamespace<M: Model> {
    pub lifetimes: HashMap<String, Lifetime<M>>,
    pub types: HashMap<String, Type<M>>,
    pub constants: HashMap<String, Constant<M>>,
}

/// An extension for the [`Table`] to get the generic parameters namespace.
pub trait GetGenericParameterNamespaceExt {
    /// Creates the [`ExtraNamespace`] that includes the generic parameters.
    ///
    /// Includes this [`ExtraNamespace`] to the [`Config`] to enable the
    /// resolution of generic parameters.
    fn get_generic_parameter_namepsace<M: Model>(
        &self,
        global_id: GlobalID,
    ) -> ExtraNamespace<M>;
}

impl GetGenericParameterNamespaceExt for Table {
    fn get_generic_parameter_namepsace<M: Model>(
        &self,
        global_id: GlobalID,
    ) -> ExtraNamespace<M> {
        let mut extra_namespace = ExtraNamespace::default();

        for scope in self.scope_walker(global_id) {
            let scope = GlobalID::new(global_id.target_id, scope);
            let symbol_kind = *self.get::<SymbolKind>(scope);

            if !symbol_kind.has_generic_parameters() {
                continue;
            }

            let Ok(generic_parameter) = self.query::<GenericParameters>(scope)
            else {
                continue;
            };

            for (name, lt) in generic_parameter.lifetime_parameter_ids_by_name()
            {
                if let Entry::Vacant(entry) =
                    extra_namespace.lifetimes.entry(name.clone())
                {
                    entry.insert(Lifetime::Parameter(LifetimeParameterID {
                        parent: scope,
                        id: *lt,
                    }));
                }
            }

            for (name, ty) in generic_parameter.type_parameter_ids_by_name() {
                if let Entry::Vacant(entry) =
                    extra_namespace.types.entry(name.clone())
                {
                    entry.insert(Type::Parameter(TypeParameterID {
                        parent: scope,
                        id: *ty,
                    }));
                }
            }

            for (name, constant) in
                generic_parameter.constant_parameter_ids_by_name()
            {
                if let Entry::Vacant(entry) =
                    extra_namespace.constants.entry(name.clone())
                {
                    entry.insert(Constant::Parameter(ConstantParameterID {
                        parent: scope,
                        id: *constant,
                    }));
                }
            }
        }

        extra_namespace
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
pub trait Observer<M: Model> {
    /// Notifies the observer when a resolution is resolved.
    fn on_resolution_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        resolution: &Resolution<M>,
        span: &Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    );

    /// Notifies the observer when a type is resolved.
    fn on_type_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        ty: &Type<M>,
        syntax_tree: &syntax_tree::r#type::Type,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    );

    /// Notifies the observer when a lifetime is resolved.
    fn on_lifetime_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        lifetime: &Lifetime<M>,
        syntax_tree: &syntax_tree::Lifetime,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    );

    /// Notifies the observer when a constant is resolved.
    fn on_constant_arguments_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        constant: &Constant<M>,
        syntax_tree: &syntax_tree::Constant,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    );

    /// Notifies the observer when a type is resolved as an unpacked element
    /// in a tuple type.
    fn on_unpacked_type_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        ty: &Type<M>,
        syntax_tree: &syntax_tree::r#type::Type,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    );

    /// Notifies the observer when a constant is resolved as an unpacked element
    /// in a tuple constant.
    fn on_unpacked_constant_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        constant: &Constant<M>,
        syntax_tree: &syntax_tree::expression::Expression,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    );
}

/// The configuration struct specifying the behaviour of the resolution process.
#[derive(Default)]
#[allow(missing_debug_implementations)]
pub struct Config<'lp, 'tp, 'cp, 'ob, 'ex, M: Model> {
    /// If specified, when the lifetime argument is elided, the provider will
    /// be used to supply the missing required lifetimes.
    pub elided_lifetime_provider:
        Option<&'lp mut dyn ElidedTermProvider<Lifetime<M>>>,

    /// If specified, when the type argument is elided, the provider will be
    /// used to supply the missing required types.
    pub elided_type_provider: Option<&'tp mut dyn ElidedTermProvider<Type<M>>>,

    /// If specified, when the constant argument is elided, the provider will
    /// be used to supply the missing required constants.
    pub elided_constant_provider:
        Option<&'cp mut dyn ElidedTermProvider<Constant<M>>>,

    /// If specified, during the resolution process, the observer will be
    /// notified each time a type, lifetime, or constant is resolved.
    pub observer: Option<&'ob mut dyn Observer<M>>,

    /// If specified, the extra namespace will be used to resolve the symbols
    /// prior to the resolution process.
    ///
    /// This is useful for including the symbols that are not directly defined
    /// in the table but are acessible such as higher-ranked lifetimes
    /// `for['x]`.
    pub extra_namespace: Option<&'ex ExtraNamespace<M>>,
}

impl<'lp, 'tp, 'cp, 'ob, 'ex, M: Model> Config<'lp, 'tp, 'cp, 'ob, 'ex, M> {
    /// Creates a new instance of the config.
    #[allow(clippy::option_if_let_else)]
    pub fn reborrow(&mut self) -> Config<M> {
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
            observer: match &mut self.observer {
                Some(observer) => Some(&mut **observer),
                None => None,
            },
            extra_namespace: self.extra_namespace,
        }
    }
}

/// An extension trait on [`Table`] to resolve various syntax trees.
pub trait Ext {
    /// Resolves for [`Resolution`] based on the given [`QualifiedIdentifier`]
    /// syntax tree.
    ///
    /// # Errors
    ///
    /// See [`qualified_identifier::Error`] for more information.
    fn resolve_qualified_identifier<M: Model>(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        referring_site: GlobalID,
        config: Config<M>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Resolution<M>, qualified_identifier::Error>;

    /// Resolves for [`Resolution`] based on the given
    /// [`QualifiedIdentifierRoot`] syntax tree.
    ///
    /// # Errors
    ///
    /// See [`qualified_identifier::Error`] for more information.
    fn resolve_qualified_identifier_root<M: Model>(
        &self,
        root: &QualifiedIdentifierRoot,
        referring_site: GlobalID,
        config: Config<M>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Resolution<M>, qualified_identifier::Error>;

    /// Resolves the generic arguments for the given symbol.
    ///
    /// # Errors
    ///
    /// See [`CyclicDependencyError`] for more information.
    fn resolve_generic_arguments_for<M: Model>(
        &self,
        symbol_id: GlobalID,
        generic_identifier: &GenericIdentifier,
        referring_site: GlobalID,
        config: Config<M>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<GenericArguments<M>, CyclicDependencyError>;

    /// Resolves a [`GenericArguments`] from the given generic arguments syntax
    /// tree.
    fn resolve_generic_arguments<M: Model>(
        &self,
        generic_arguments: &syntax_tree::GenericArguments,
        referring_site: GlobalID,
        config: Config<M>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> GenericArguments<M>;

    /// Verifies that the given `generic_arguments` have the right amount of
    /// arguments by supplying the default arguments if necessary.
    ///
    /// # Errors
    ///
    /// See [`CyclicDependencyError`] for more information.
    fn verify_generic_arguments_for<M: Model>(
        &self,
        generic_arguments: GenericArguments<M>,
        generic_id: GlobalID,
        generic_identifier_span: Span,
        config: Config<M>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<GenericArguments<M>, CyclicDependencyError>;

    /// Resolves for a [`Lifetime`] based on the given [`syntax_tree::Lifetime`]
    fn resolve_lifetime<M: Model>(
        &self,
        lifetime_argument: &syntax_tree::Lifetime,
        referring_site: GlobalID,
        config: Config<M>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Lifetime<M>;

    /// Resolves for a [`Type`] based on the given [`syntax_tree::r#type::Type`]
    fn resolve_type<M: Model>(
        &self,
        type_argument: &syntax_tree::r#type::Type,
        referring_site: GlobalID,
        config: Config<M>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Type<M>;
}

impl Ext for Table {
    fn resolve_qualified_identifier<M: Model>(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        referring_site: GlobalID,
        config: Config<M>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Resolution<M>, qualified_identifier::Error> {
        qualified_identifier::resolve(
            self,
            qualified_identifier,
            referring_site,
            config,
            handler,
        )
    }

    fn resolve_qualified_identifier_root<M: Model>(
        &self,
        root: &QualifiedIdentifierRoot,
        referring_site: GlobalID,
        config: Config<M>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Resolution<M>, qualified_identifier::Error> {
        qualified_identifier::resolve_root(
            self,
            root,
            referring_site,
            config,
            handler,
        )
    }

    fn resolve_generic_arguments_for<M: Model>(
        &self,
        symbol_id: GlobalID,
        generic_identifier: &GenericIdentifier,
        referring_site: GlobalID,
        config: Config<M>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<GenericArguments<M>, CyclicDependencyError> {
        term::resolve_generic_arguments_for(
            self,
            symbol_id,
            generic_identifier,
            referring_site,
            config,
            handler,
        )
    }

    fn resolve_generic_arguments<M: Model>(
        &self,
        generic_arguments: &syntax_tree::GenericArguments,
        referring_site: GlobalID,
        config: Config<M>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> GenericArguments<M> {
        term::resolve_generic_arguments(
            self,
            generic_arguments,
            referring_site,
            config,
            handler,
        )
    }

    fn verify_generic_arguments_for<M: Model>(
        &self,
        generic_arguments: GenericArguments<M>,
        generic_id: GlobalID,
        generic_identifier_span: Span,
        config: Config<M>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<GenericArguments<M>, CyclicDependencyError> {
        term::verify_generic_arguments_for(
            self,
            generic_arguments,
            generic_id,
            generic_identifier_span,
            config,
            handler,
        )
    }

    fn resolve_lifetime<M: Model>(
        &self,
        lifetime_argument: &syntax_tree::Lifetime,
        referring_site: GlobalID,
        config: Config<M>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Lifetime<M> {
        term::resolve_lifetime(
            self,
            lifetime_argument,
            referring_site,
            config,
            handler,
        )
    }

    fn resolve_type<M: Model>(
        &self,
        type_argument: &syntax_tree::r#type::Type,
        referring_site: GlobalID,
        config: Config<M>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Type<M> {
        term::resolve_type(self, type_argument, referring_site, config, handler)
    }
}
