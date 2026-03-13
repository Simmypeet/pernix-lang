//! Contains the logic for resolving syntax tree into some form of semantic
//! term/information.

use std::pin::Pin;

use bon::Builder;
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant, generic_parameters::InstanceParameterID,
    instance::Instance, lifetime::Lifetime, r#type::Type,
};
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::qualified_identifier::Resolution;

pub mod diagnostic;
pub mod generic_parameter_namespace;
pub mod qualified_identifier;
pub mod resolver;
pub mod term;

// Re-export config types for convenience
pub use resolver::{ElidedTermProvider, Resolver};

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

    /// Returns the type with the given name from the extra namespace, if
    /// exists.
    #[must_use]
    pub fn get_type(&self, name: &str) -> Option<&Type> { self.types.get(name) }

    /// Returns the constant with the given name from the extra namespace, if
    /// exists.
    #[must_use]
    pub fn get_constant(&self, name: &str) -> Option<&Constant> {
        self.constants.get(name)
    }

    /// Returns the instance with the given name from the extra namespace, if
    /// exists.
    #[must_use]
    pub fn get_instance(&self, name: &str) -> Option<&Instance> {
        self.instances.get(name)
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
        resolution: &Resolution,
        span: &RelativeSpan,
    );

    /// Notifies the observer when a type is resolved.
    fn on_type_resolved(
        &mut self,
        ty: &Type,
        syntax_tree: &pernixc_syntax::r#type::Type,
    );

    /// Notifies the observer when a lifetime is resolved.
    fn on_lifetime_resolved(
        &mut self,
        lifetime: &Lifetime,
        syntax_tree: &pernixc_syntax::Lifetime,
    );

    /// Notifies the observer when a constant is resolved.
    fn on_constant_arguments_resolved(
        &mut self,
        constant: &Constant,
        syntax_tree: &pernixc_syntax::expression::Expression,
    );

    /// Notifies the observer when a type is resolved as an unpacked element
    /// in a tuple type.
    fn on_unpacked_type_resolved(
        &mut self,
        ty: &Type,
        syntax_tree: &pernixc_syntax::r#type::Unpackable,
    );

    /// Notifies the observer when a constant is resolved as an unpacked element
    /// in a tuple constant.
    fn on_unpacked_constant_resolved(
        &mut self,
        constant: &Constant,
        syntax_tree: &pernixc_syntax::expression::Expression,
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

/// The error type occurred during the resolution.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
#[allow(missing_docs)]
pub enum Error {
    /// Encounters a fatal error during the resolution.
    Abort,
}
