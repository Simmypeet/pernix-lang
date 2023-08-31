use std::{
    borrow::Borrow,
    collections::{hash_map::Entry, HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    sync::atomic::AtomicUsize,
};

use pernixc_syntax::syntax_tree::target::Target;
use pernixc_system::arena::{self, Arena};

use crate::{
    constant::{self, Constant},
    pattern::Irrefutable,
    ty::{self, TupleBoundableType},
};

mod core;
mod module;
pub mod resolution;

/// Represents an automatic generated lifetime used in the place where the lifetime is elided.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElidedLifetime {
    /// The reference to the generic item that generates this elided lifetime.
    pub generic_item_ref: GenericItemRef,
}

/// Represents a reference to the associated item declared in various items.
#[derive(Clone)]
pub struct AssociatedItemRef<Parent, Kind> {
    pub parent: arena::ID<Parent>,
    pub index: usize,
    _phantom: std::marker::PhantomData<Kind>,
}

impl<Parent: Debug, Kind> Debug for AssociatedItemRef<Parent, Kind> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(
            format!(
                "AssociatedItemRef<{}, {}>",
                std::any::type_name::<Parent>(),
                std::any::type_name::<Kind>()
            )
            .as_str(),
        )
        .field("parent", &self.parent)
        .field("index", &self.index)
        .finish()
    }
}

impl<Parent: Clone, Kind: Clone> Copy for AssociatedItemRef<Parent, Kind> {}

impl<Parent, Kind> PartialEq for AssociatedItemRef<Parent, Kind> {
    fn eq(&self, other: &Self) -> bool { self.parent == other.parent && self.index == other.index }
}

impl<Parent, Kind> Eq for AssociatedItemRef<Parent, Kind> {}

impl<Parent, Kind> PartialOrd for AssociatedItemRef<Parent, Kind> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.parent.cmp(&other.parent) {
            std::cmp::Ordering::Equal => self.index.partial_cmp(&other.index),
            ordering => Some(ordering),
        }
    }
}

impl<Parent, Kind> Ord for AssociatedItemRef<Parent, Kind> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.parent.cmp(&other.parent) {
            std::cmp::Ordering::Equal => self.index.cmp(&other.index),
            ordering => ordering,
        }
    }
}

impl<Parent, Kind> Hash for AssociatedItemRef<Parent, Kind> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.parent.hash(state);
        self.index.hash(state);
    }
}

/// Represents a trait associated type declaration entry in the [`Trait`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitAssociatedType {}

/// Represents a trait associated const declaration entry in the [`Trait`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitAssociatedConstant {}

/// Represents a reference to the trait associated type declared in the trait.
pub type TraitAssociatedTypeRef = AssociatedItemRef<Trait, TraitAssociatedType>;

/// Represents a reference to the trait associated const declared in the trait.
pub type TraitAssociatedConstantRef = AssociatedItemRef<Trait, TraitAssociatedConstant>;

/// Represents a reference to the generic parameter declared in the item.
#[derive(Clone)]
pub struct GenericParameterRef<Kind> {
    /// The index of the generic parameter in the declared list of the item.
    pub index: usize,
    /// The reference to the generic item that generates this generic parameter.
    pub generic_item_ref: GenericItemRef,

    _phantom: std::marker::PhantomData<Kind>,
}

impl<T> Debug for GenericParameterRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(
            format!("GenericParameterarena::ID<{}>", std::any::type_name::<T>()).as_str(),
        )
        .field("index", &self.index)
        .finish()
    }
}

impl<T: Clone> Copy for GenericParameterRef<T> {}

impl<Kind> GenericParameterRef<Kind> {
    #[must_use]
    pub fn new(index: usize, generic_item_ref: GenericItemRef) -> Self {
        Self {
            index,
            generic_item_ref,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<Kind> PartialEq for GenericParameterRef<Kind> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index && self.generic_item_ref == other.generic_item_ref
    }
}

impl<Kind> Eq for GenericParameterRef<Kind> {}

impl<Kind> PartialOrd for GenericParameterRef<Kind> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.generic_item_ref.cmp(&other.generic_item_ref) {
            std::cmp::Ordering::Equal => self.index.partial_cmp(&other.index),
            ordering => Some(ordering),
        }
    }
}

impl<Kind> Ord for GenericParameterRef<Kind> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.generic_item_ref.cmp(&other.generic_item_ref) {
            std::cmp::Ordering::Equal => self.index.cmp(&other.index),
            ordering => ordering,
        }
    }
}

impl<Kind> Hash for GenericParameterRef<Kind> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
        self.generic_item_ref.hash(state);
    }
}

/// Represents a declaration of a generic lifetime parameter.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeParameter {
    pub name: String,
}

/// Represents an identifier/reference to a lifetime parameter declared in the item.
pub type LifetimeParameterRef = GenericParameterRef<LifetimeParameter>;

/// Represents a declaration of a generic type parameter.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeParameter {
    pub name: String,
}

/// Represents an identifier/reference to a type parameter declared in the item.
pub type TypeParameterRef = GenericParameterRef<TypeParameter>;

/// Represents a declaration of a generic constant parameter.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
    pub accessibility: Accessibility,
    pub name: String,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub fields: VecMap<Field>,
}

/// Represents a variant declaration entry in the [`Enum`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    pub name: String,
    pub association_type: Option<ty::Type>,
}

/// Represents an enum declaration entry in the symbol table.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    pub accessibility: Accessibility,
    pub name: String,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub variants: VecMap<Variant>,
}

/// Represents an index of a trait associated type or trait associated const.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TraitAssociatedIndex {
    Type(usize),
    Constant(usize),
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

/// Represents a trait declaration entry in the symbol table.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trait {
    pub accessibility: Accessibility,
    pub name: String,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub trait_associated_indices_by_name: HashMap<String, TraitAssociatedIndex>,
    pub trait_associated_constants: VecMap<TraitAssociatedConstant>,
    pub trait_associated_types: VecMap<TraitAssociatedType>,
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhereClause {
    /// Set of type parameters that needs to be a tuple type.
    pub tuple_bounds: HashSet<TupleBoundableType>,

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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericParameters {
    pub lifetimes: VecMap<LifetimeParameter>,
    pub types: VecMap<TypeParameter>,
    pub constants: VecMap<ConstantParameter>,
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub accessibility: Accessibility,
    pub name: String,
    pub parent_module_id: Option<arena::ID<Module>>,
    pub children_ids_by_name: HashMap<String, ID>,
}

/// Represents a type declaration entry in the symbol table.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub accessibility: Accessibility,
    pub name: String,
    pub generic_parameters: GenericParameters,
    pub alias: ty::Type,
}

/// Represents a parameter declaration entry in the [`Function`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Paramter {
    pub pattern: Irrefutable,
    pub ty: ty::Type,
}

/// Represents a function declaration entry in the symbol table.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub accessibility: Accessibility,
    pub name: String,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub parameters: Vec<Paramter>,
    pub return_type: ty::Type,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GenericItemRef {
    Struct(arena::ID<Struct>),
    Enum(arena::ID<Enum>),
    TraitAssociatedType(TraitAssociatedTypeRef),
    TraitAssociatedConstant(TraitAssociatedConstantRef),
}

/// A container class for storing multiple symbols.
///
/// The container offers two ways to access the symbols either by name or by index.
///
/// When accessing the symbols by name, the container will perform a hash table lookup to find the
/// symbol. On the other hand, when accessing the symbols by index, the container will simply index
/// into the underlying vector, which offers a faster access time.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct VecMap<T> {
    vec: Vec<T>,
    map: HashMap<String, usize>,
}

impl<T> VecMap<T> {
    //// Creates a new empty [`VecMap`].
    #[must_use]
    pub fn new() -> Self {
        Self {
            vec: Vec::new(),
            map: HashMap::new(),
        }
    }

    #[must_use]
    pub fn len(&self) -> usize { self.vec.len() }

    #[must_use]
    pub fn is_empty(&self) -> bool { self.vec.is_empty() }

    /// Inserts a new value into the [`VecMap`].
    ///
    /// # Arguments
    ///
    /// - `name`: The name of the value.
    /// - `value`: The value to be inserted.
    ///
    /// # Errors
    ///
    /// Returns `Ok` with the index of the inserted value if the name is not already in the map.
    /// Otherwise, returns `Err` with the index of the existing value.
    pub fn insert(&mut self, name: String, value: T) -> Result<usize, usize> {
        match self.map.entry(name) {
            Entry::Occupied(entry) => Err(*entry.get()),
            Entry::Vacant(entry) => {
                let index = self.vec.len();
                entry.insert(index);
                self.vec.push(value);
                Ok(index)
            }
        }
    }

    #[must_use]
    pub fn get_by_name<Q>(&self, name: &Q) -> Option<&T>
    where
        String: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.get(name).map(|index| &self.vec[*index])
    }

    #[must_use]
    pub fn get_by_name_mut<Q>(&mut self, name: &Q) -> Option<&mut T>
    where
        String: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.get(name).map(|index| &mut self.vec[*index])
    }

    #[must_use]
    pub fn get_by_index(&self, index: usize) -> Option<&T> { self.vec.get(index) }

    #[must_use]
    pub fn get_by_index_mut(&mut self, index: usize) -> Option<&mut T> { self.vec.get_mut(index) }

    #[must_use]
    pub fn map_name_to_index<Q>(&self, name: &Q) -> Option<usize>
    where
        String: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.get(name).copied()
    }
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

/// The symbol table of the compiler.
///
/// The table virtually contains all the information about the program. The structure of the table
/// is flattened and is not a heirarchy. The table is also not a tree, but rather a graph.
#[derive(Debug, Clone)]
pub struct Table {
    modules: Arena<Module>,
    structs: Arena<Struct>,
    enums: Arena<Enum>,
    traits: Arena<Trait>,
    types: Arena<Type>,
    functions: Arena<Function>,

    root_target_module_ids_by_name: HashMap<String, arena::ID<Module>>,
}

impl Table {
    /// Builds the symbol table by analysing the given target syntax trees.
    ///
    /// # Errors
    /// - [`BuildError::TargetNameDuplication`] is returned if there are multiple targets with the
    /// same name.
    /// - [`BuildError::TargetNamedCore`] is returned if there is a target named `@core`.
    pub fn build(target: impl Iterator<Item = Target>) -> Result<Self, BuildError> {
        // default table
        let mut table = Self {
            modules: Arena::new(),
            structs: Arena::new(),
            enums: Arena::new(),
            traits: Arena::new(),
            types: Arena::new(),
            functions: Arena::new(),
            root_target_module_ids_by_name: HashMap::new(),
        };

        table.create_core_module();

        Ok(table)
    }
}

// TODO: Create a module heirarchy
// TODO: Draft the symbols
// TODO: Finalize the symbols
