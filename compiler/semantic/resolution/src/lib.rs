//! Contains the logic for resolving syntax tree into some form of semantic
//! term/information.

use bon::Builder;
use flexstr::SharedStr;
use pernixc_handler::Handler;
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::{constant::Constant, lifetime::Lifetime, r#type::Type};

use crate::{diagnostic::Diagnostic, qualified_identifier::Resolution};

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
    Debug, Clone, PartialEq, Eq, Default, StableHash, Serialize, Deserialize,
)]
#[allow(missing_docs)]
pub struct ExtraNamespace {
    pub lifetimes: HashMap<SharedStr, Lifetime>,
    pub types: HashMap<SharedStr, Type>,
    pub constants: HashMap<SharedStr, Constant>,
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
            observer: match &mut self.observer {
                Some(observer) => Some(&mut **observer),
                None => None,
            },
            extra_namespace: self.extra_namespace,
            consider_adt_implements: self.consider_adt_implements,
            referring_site: self.referring_site,
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
    Cyclic(executor::CyclicError),
}
