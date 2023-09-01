use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    sync::atomic::AtomicUsize,
};

use pernixc_source::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{self, target::Target};
use pernixc_system::{
    arena::{self, Arena},
    diagnostic::Handler,
};

use crate::{
    constant::{self, Constant},
    error::Error,
    pattern::Irrefutable,
    ty::{self, TupleBoundable},
};

mod core;
mod drafting;
pub mod resolution;

/// Represents an automatic generated lifetime used in the place where the lifetime is elided.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElidedLifetime {
    /// The reference to the generic item that generates this elided lifetime.
    pub generic_item_ref: GenericItemRef,
}

/// Represents a reference to the associated item declared in various items.
#[derive(Debug)]
pub struct AssociatedItemRef<Parent, Kind> {
    pub parent: arena::ID<Parent>,
    pub associated_item: arena::ID<Kind>,
}

impl<Parent, Kind> Clone for AssociatedItemRef<Parent, Kind> {
    fn clone(&self) -> Self {
        Self {
            parent: self.parent,
            associated_item: self.associated_item,
        }
    }
}

impl<Parent, Kind> Copy for AssociatedItemRef<Parent, Kind> {}

impl<Parent, Kind> PartialEq for AssociatedItemRef<Parent, Kind> {
    fn eq(&self, other: &Self) -> bool {
        self.parent == other.parent && self.associated_item == other.associated_item
    }
}

impl<Parent, Kind> Eq for AssociatedItemRef<Parent, Kind> {}

impl<Parent, Kind> PartialOrd for AssociatedItemRef<Parent, Kind> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.parent
            .partial_cmp(&other.parent)
            .and_then(|ordering| match ordering {
                std::cmp::Ordering::Equal => {
                    self.associated_item.partial_cmp(&other.associated_item)
                }
                _ => Some(ordering),
            })
    }
}

impl<Parent, Kind> Ord for AssociatedItemRef<Parent, Kind> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.parent
            .cmp(&other.parent)
            .then_with(|| self.associated_item.cmp(&other.associated_item))
    }
}

impl<Parent, Kind> Hash for AssociatedItemRef<Parent, Kind> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.parent.hash(state);
        self.associated_item.hash(state);
    }
}

/// Represents a trait associated type declaration entry in the [`Trait`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitType {}

/// Represents a trait associated const declaration entry in the [`Trait`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitConstant {}

/// Represents a trait associated function declaration entry in the [`Trait`].
#[derive(Debug, Clone)]
pub struct TraitFunction {
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub name: String,
    pub parameters: Arena<Parameter>,
    pub return_type: ty::Type,
}

/// Represents a reference to the trait associated function declared in the trait.
pub type TraitFunctionRef = AssociatedItemRef<Trait, TraitFunction>;

/// Represents a reference to the trait associated const declared in the trait.
pub type TraitConstantRef = AssociatedItemRef<Trait, TraitConstant>;

/// Represents a reference to the trait associated type declared in the trait.
pub type TraitTypeRef = AssociatedItemRef<Trait, TraitType>;

/// Represents a reference to the generic parameter declared in the item.
#[derive(Debug)]
pub struct GenericParameterRef<Kind> {
    /// The index of the generic parameter in the declared list of the item.
    pub id: arena::ID<Kind>,
    /// The reference to the generic item that generates this generic parameter.
    pub generic_item_ref: GenericItemRef,
}

impl<Kind> Clone for GenericParameterRef<Kind> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            generic_item_ref: self.generic_item_ref,
        }
    }
}

impl<Kind> Copy for GenericParameterRef<Kind> {}

impl<Kind> PartialEq for GenericParameterRef<Kind> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.generic_item_ref == other.generic_item_ref
    }
}

impl<Kind> Eq for GenericParameterRef<Kind> {}

impl<Kind> PartialOrd for GenericParameterRef<Kind> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id
            .partial_cmp(&other.id)
            .and_then(|ordering| match ordering {
                std::cmp::Ordering::Equal => {
                    self.generic_item_ref.partial_cmp(&other.generic_item_ref)
                }
                _ => Some(ordering),
            })
    }
}

impl<Kind> Ord for GenericParameterRef<Kind> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id
            .cmp(&other.id)
            .then_with(|| self.generic_item_ref.cmp(&other.generic_item_ref))
    }
}

impl<Kind> Hash for GenericParameterRef<Kind> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.generic_item_ref.hash(state);
    }
}

/// Represents a declaration of a generic lifetime parameter.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct LifetimeParameter {
    pub name: String,
}

/// Represents an identifier/reference to a lifetime parameter declared in the item.
pub type LifetimeParameterRef = GenericParameterRef<LifetimeParameter>;

/// Represents a declaration of a generic type parameter.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct TypeParameter {
    pub name: String,
}

/// Represents an identifier/reference to a type parameter declared in the item.
pub type TypeParameterRef = GenericParameterRef<TypeParameter>;

/// Represents a declaration of a generic constant parameter.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ConstantParameter {
    pub name: String,
    pub ty: ty::Type,
}

/// Represents an identifier/reference to a constant parameter declared in the item.
pub type ConstantParameterRef = GenericParameterRef<ConstantParameter>;

pub trait Symbol {
    /// The name of the symbol.
    ///
    /// This name doesn't need to accurately represent the symbol's name in the source code.
    /// It is only meant for readability purposes.
    fn name(&self) -> &str;

    /// The accessibility of the symbol.
    fn accessibility(&self) -> Accessibility;

    /// Span of the symbol in the source code.
    fn span(&self) -> Option<Span>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lifetime {
    Static,
    Named(GenericParameterRef<LifetimeParameter>),
    ElidedLifetime(ElidedLifetime),
}

/// Represents an accessibility of an item.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Accessibility {
    Public,
    Private,
    Internal,
}

/// Represents a field declaration entry in the [`Struct`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub accessibility: Accessibility,
    pub name: String,
    pub ty: ty::Type,
}

/// Represents a struct declaration entry in the symbol table.
#[derive(Debug, Clone)]
pub struct Struct {
    pub accessibility: Accessibility,
    pub name: String,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub fields: Arena<Field>,
    pub syntax_tree: Option<syntax_tree::item::Struct>,
}

impl Symbol for arena::Symbol<Struct> {
    fn name(&self) -> &str { &self.name }

    fn accessibility(&self) -> Accessibility { self.accessibility }

    fn span(&self) -> Option<Span> {
        self.syntax_tree.as_ref().map(|syntax_tree| {
            syntax_tree
                .signature()
                .struct_keyword()
                .span
                .join(&syntax_tree.signature().identifier().span)
                .unwrap()
        })
    }
}

/// Represents a variant declaration entry in the [`Enum`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    pub name: String,
    pub association_type: Option<ty::Type>,
}

/// Represents an enum declaration entry in the symbol table.
#[derive(Debug, Clone)]
pub struct Enum {
    pub accessibility: Accessibility,
    pub name: String,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub variants: Arena<Variant>,
    pub syntax_tree: Option<syntax_tree::item::Enum>,
}

impl Symbol for arena::Symbol<Enum> {
    fn name(&self) -> &str { &self.name }

    fn accessibility(&self) -> Accessibility { self.accessibility }

    fn span(&self) -> Option<Span> {
        self.syntax_tree.as_ref().map(|syntax_tree| {
            syntax_tree
                .signature()
                .enum_keyword()
                .span
                .join(&syntax_tree.signature().identifier().span)
                .unwrap()
        })
    }
}

/// Represents an index of a trait associated type or trait associated const.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TraitAssociatedID {
    Type(arena::ID<TraitType>),
    Constant(arena::ID<TraitConstant>),
    Function(arena::ID<TraitFunction>),
}

/// Represents a higher ranked lifetime defined in the trait bounds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HigherRankedLifetime(usize);

impl HigherRankedLifetime {
    /// Generates a new unique higher ranked lifetime parameter.
    ///
    /// The newly generated higher ranked lifetime parameter is guaranteed to be unique to the any
    /// other generated higher ranked lifetime parameter.
    #[must_use]
    pub fn generate() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }
}

/// Represents a lifetime that can either be a normal lifetime or a higher ranked lifetime.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HigherRankedableLifetime {
    Normal(Lifetime),
    HigherRanked(HigherRankedLifetime),
}

#[derive(Debug, Clone)]
pub struct ImplementsSignature {
    pub generic_parameters: GenericParameters,
    pub trait_substitution: LocalSubstitution,
}

#[derive(Debug, Clone)]
pub struct Implements {
    pub signature: ImplementsSignature,
    pub is_const_implements: bool,
    pub where_clause: WhereClause,
}

/// Represents a trait implements declaration entry associated with the [`Trait`].
#[derive(Debug, Clone)]
pub enum ImplementsKind {
    /// If the generic parameters substitution matches to the negative signature, then the trait
    /// resolution will fail.
    Negative(ImplementsSignature),

    Positive(Implements),
}

/// Represents a trait declaration entry in the symbol table.
#[derive(Debug, Clone)]
pub struct Trait {
    pub accessibility: Accessibility,
    pub name: String,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub trait_associated_ids_by_name: HashMap<String, TraitAssociatedID>,
    pub trait_constants: Arena<TraitConstant>,
    pub trait_types: Arena<TraitType>,
    pub trait_functions: Arena<TraitFunction>,
    pub trait_implements: Arena<ImplementsKind>,
    pub syntax_tree: Option<syntax_tree::item::Trait>,
}

impl Symbol for arena::Symbol<Trait> {
    fn name(&self) -> &str { &self.name }

    fn accessibility(&self) -> Accessibility { self.accessibility }

    fn span(&self) -> Option<Span> {
        self.syntax_tree.as_ref().map(|syntax_tree| {
            syntax_tree
                .signature()
                .trait_keyword()
                .span
                .join(&syntax_tree.signature().identifier().span)
                .unwrap()
        })
    }
}

/// Represents a trait bound entry in the [`WhereClause`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitBound {
    /// The id of the trait that are bounded.
    pub trait_id: arena::ID<Trait>,
    /// The substitution of trait's generic type parameters.
    pub types: Vec<ty::Type>,
    /// The substitution of trait's generic lifetime parameters.
    pub lifetimes: Vec<HigherRankedableLifetime>,
    /// The substitution of trait's generic constant parameters.
    pub constants: Vec<Constant>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LifetimeBoundOperand {
    Lifetime(Lifetime),
    Type(ty::Type),
}

/// Represents a where clause declaration in various item.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct WhereClause {
    /// Set of type parameters that needs to be a tuple type.
    pub tuple_bounds: HashSet<TupleBoundable>,

    /// Set of required trait bounds and whether the trait bounds are const or not.
    ///
    /// The key represents the trait bound and the value represents whether the trait bound is
    /// const or not.
    pub trait_bounds: HashMap<TraitBound, bool>,

    /// Set of lifetime bounds.
    ///
    /// The key is the operand of the lifetime bound and the value is the set of lifetimes that
    /// the operand must outlive.
    pub lifetime_bounds: HashMap<LifetimeBoundOperand, HashSet<Lifetime>>,

    /// Set of trait associated constant bounds.
    ///
    /// The key is the trait associated type and the value is the constant that the trait
    /// associated constant must be.
    pub trait_associated_constant_bounds: HashMap<constant::TraitAssociated, Constant>,

    /// Set of trait associated type bounds.
    ///
    /// The key is the trait associated type and the value is the type that the trait associated
    /// type must be.
    pub trait_associated_type_bounds: HashMap<ty::TraitAssociated, ty::Type>,
}

/// Represents a generic parameter declaration in various items signatures.
#[derive(Debug, Clone, Default)]
pub struct GenericParameters {
    pub lifetimes: arena::NamedMap<LifetimeParameter>,
    pub types: arena::NamedMap<TypeParameter>,
    pub constants: arena::NamedMap<ConstantParameter>,
}

/// Represents a generic argument substitution for a single generic item.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalSubstitution {
    pub lifetimes: Vec<Lifetime>,
    pub types: Vec<ty::Type>,
    pub constants: Vec<Constant>,
    pub elided_lifetime: Lifetime,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Subsitution {
    /// The substitution of multiple generic items.
    ///
    /// The key indicates which generic parameter is being substituted and the value indicates
    /// the substitution.
    pub local_substitutions_by_generic_item_ref: HashMap<GenericItemRef, LocalSubstitution>,
}

/// Represents a module declaration entry in the symbol table.
#[derive(Debug, Clone)]
pub struct Module {
    pub accessibility: Accessibility,
    pub name: String,
    pub parent_module_id: Option<arena::ID<Module>>,
    pub children_ids_by_name: HashMap<String, ID>,
    pub usings: HashMap<arena::ID<Module>, syntax_tree::item::Using>,
    pub syntax_tree: Option<syntax_tree::item::ModuleSignature>,
}

impl Symbol for arena::Symbol<Module> {
    fn name(&self) -> &str { &self.name }

    fn accessibility(&self) -> Accessibility { self.accessibility }

    fn span(&self) -> Option<Span> { self.syntax_tree.as_ref().map(SourceElement::span) }
}

/// Represents a type declaration entry in the symbol table.
#[derive(Debug, Clone)]
pub struct Type {
    pub accessibility: Accessibility,
    pub name: String,
    pub generic_parameters: GenericParameters,
    pub alias: ty::Type,
    pub syntax_tree: Option<syntax_tree::item::Type>,
}

impl Symbol for arena::Symbol<Type> {
    fn name(&self) -> &str { &self.name }

    fn accessibility(&self) -> Accessibility { self.accessibility }

    fn span(&self) -> Option<Span> {
        self.syntax_tree.as_ref().map(|syntax_tree| {
            syntax_tree
                .signature()
                .type_keyword()
                .span
                .join(&syntax_tree.signature().identifier().span)
                .unwrap()
        })
    }
}

/// Represents a parameter declaration entry in the [`Function`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub pattern: Irrefutable,
    pub ty: ty::Type,
}

/// Represents a function declaration entry in the symbol table.
#[derive(Debug, Clone)]
pub struct Function {
    pub accessibility: Accessibility,
    pub name: String,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub parameters: Vec<Parameter>,
    pub return_type: ty::Type,
    pub syntax_tree: Option<syntax_tree::item::Function>,
}

impl Symbol for arena::Symbol<Function> {
    fn name(&self) -> &str { &self.name }

    fn accessibility(&self) -> Accessibility { self.accessibility }

    fn span(&self) -> Option<Span> {
        self.syntax_tree
            .as_ref()
            .map(|signature| {
                signature
                    .signature()
                    .function_keyword()
                    .span
                    .join(&signature.signature().identifier().span)
            })
            .unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ID {
    Module(arena::ID<Module>),
    Struct(arena::ID<Struct>),
    Enum(arena::ID<Enum>),
    Trait(arena::ID<Trait>),
    Type(arena::ID<Type>),
    Function(arena::ID<Function>),
}

/// Represents a reference to the item in the [`Table`] that can have generic parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GenericItemRef {
    Struct(arena::ID<Struct>),
    Enum(arena::ID<Enum>),
    Trait(arena::ID<Trait>),
    TraitType(TraitFunctionRef),
    TraitConstant(TraitConstantRef),
    TraitFunction(TraitFunctionRef),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[error("target name duplication found")]
pub struct TargetNameDuplicationError {
    pub name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[error("a target named '@core' is found, which is reserved for the core library")]
pub struct TargetNamedCoreError;

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error, derive_more::From,
)]
pub enum BuildError {
    #[error("{0}")]
    TargetNameDuplication(TargetNameDuplicationError),
    #[error("{0}")]
    TargetNamedCore(TargetNamedCoreError),
}

#[derive(Debug, Clone)]
pub struct Table {
    modules: Arena<Module>,
    structs: Arena<Struct>,
    enums: Arena<Enum>,
    traits: Arena<Trait>,
    types: Arena<Type>,
    functions: Arena<Function>,

    target_root_module_ids_by_name: HashMap<String, arena::ID<Module>>,
}

impl Table {
    /// Builds the symbol table by analysing the given target syntax trees.
    ///
    /// # Errors
    /// - [`BuildError::TargetNameDuplication`] is returned if there are multiple targets with the
    /// same name.
    /// - [`BuildError::TargetNamedCore`] is returned if there is a target named `@core`.
    pub fn build(
        targets: impl Iterator<Item = Target>,
        handler: &impl Handler<Error>,
    ) -> Result<Self, BuildError> {
        // default table
        let mut table = Self {
            modules: Arena::new(),
            structs: Arena::new(),
            enums: Arena::new(),
            traits: Arena::new(),
            types: Arena::new(),
            functions: Arena::new(),
            target_root_module_ids_by_name: HashMap::new(),
        };

        table.create_core_module();

        for target in targets {
            table.draft_target(target, handler)?;
        }

        Ok(table)
    }

    /// Gets the [`Symbol`] stored in the table.
    #[must_use]
    pub fn get_symbol(&self, id: ID) -> Option<&dyn Symbol> {
        match id {
            ID::Module(id) => self.modules.get(id).map(|x| x as _),
            ID::Struct(id) => self.structs.get(id).map(|x| x as _),
            ID::Enum(id) => self.enums.get(id).map(|x| x as _),
            ID::Trait(id) => self.traits.get(id).map(|x| x as _),
            ID::Type(id) => self.types.get(id).map(|x| x as _),
            ID::Function(id) => self.functions.get(id).map(|x| x as _),
        }
    }
}

// TODO: Create a module heirarchy
// TODO: Draft the symbols
// TODO: Finalize the symbols

#[cfg(test)]
mod tests;
