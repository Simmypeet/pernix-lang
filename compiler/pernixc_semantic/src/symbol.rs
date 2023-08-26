use std::{
    borrow::Borrow,
    collections::{hash_map::Entry, BTreeSet, HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    sync::atomic::AtomicUsize,
};

use pernixc_system::arena::ID;

use crate::{constant::Constant, ty};

/// Represents an automatic generated lifetime used in the place where the lifetime is elided.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElidedLifetime {
    /// The reference to the generic item that generates this elided lifetime.
    pub generic_item_ref: GenericItemRef,
}

/// Represents a reference to the generic parameter declared in the item.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitAssociationRef<Kind> {
    /// The id of the trait that the associated item is declared in.
    pub trait_id: ID<Trait>,

    /// The index of the associated item in the trait.
    pub index: usize,

    _phantom: std::marker::PhantomData<Kind>,
}

impl<T> Debug for TraitAssociationRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(format!("TraitAssociationRef<{}>", std::any::type_name::<T>()).as_str())
            .field("trait_id", &self.trait_id)
            .field("index", &self.index)
            .finish()
    }
}

impl<T: Clone> Copy for TraitAssociationRef<T> {}

/// Represents a trait associated type declaration entry in the [`Trat`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitAssociatedType {}

/// Represents a trait associated const declaration entry in the [`Trat`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitAssociatedConst {}

/// Represents an identifier/reference to a trait associated type declared in the trait.
pub type TraitAssociatedTypeRef = TraitAssociationRef<TraitAssociatedType>;

/// Represents an identifier/reference to a trait associated type declared in the trait.
pub type TraitAssociatedConstRef = TraitAssociationRef<TraitAssociatedConst>;

/// Represents a reference to the generic parameter declared in the item.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericParameterRef<Kind> {
    /// The index of the generic parameter in the declared list of the item.
    pub index: usize,
    /// The reference to the generic item that generates this generic parameter.
    pub generic_item_ref: GenericItemRef,

    _phantom: std::marker::PhantomData<Kind>,
}

impl<T> Debug for GenericParameterRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(format!("GenericParameterID<{}>", std::any::type_name::<T>()).as_str())
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

/// Represents a type parameter that has been bounded to be a tuple type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TupleBoundedType {
    TypeParameter(TypeParameterRef),
    SubTuple(Box<SubTupleType>),
}

/// Represents a subtype of a particular tuple type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SubTupleType {
    pub super_tuple_type: TupleBoundedType,
}

/// Represents a type that can either be a [`ty::Type`] or a [`SubTupleType`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SubTupleableType {
    Normal(ty::Type),
    SubTuple(SubTupleType),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trait {
    pub name: String,
}

/// Represents a trait bound entry in the [`WhereClause`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitBound<Ty> {
    /// The id of the trait that are bounded.
    pub trait_id: ID<Trait>,
    /// The substitution of trait's generic type parameters.
    pub types: Vec<Ty>,
    /// The substitution of trait's generic lifetime parameters.
    pub lifetimes: Vec<HigherRankedableLifetime>,
    /// The substitution of trait's generic constant parameters.
    pub constants: Vec<Constant>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LifetimeBoundOperand<Ty> {
    Lifetime(Lifetime),
    Type(Ty),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForTupleClause {
    pub tuple_bounds: HashSet<SubTupleType>,
    pub trait_bounds: HashMap<TraitBound<SubTupleableType>, bool>,
    pub lifetime_bounds: HashMap<LifetimeBoundOperand<SubTupleType>, HashSet<Lifetime>>,
    pub for_tuple_clauses: HashMap<BTreeSet<SubTupleableType>, ForTupleClause>,
}

/// Represents a where clause declaration in various item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhereClause {
    /// Set of type parameters that needs to be a tuple type.
    pub tuple_bounds: HashSet<TypeParameter>,

    /// Set of required trait bounds and whether the trait bounds are const or not.
    ///
    /// The key represents the trait bound and the value represents whether the trait bound is
    /// const or not.
    pub trait_bounds: HashMap<TraitBound<ty::Type>, bool>,

    /// Set of lifetime bounds.
    ///
    /// The key is the operand of the lifetime bound and the value is the set of lifetimes that
    /// the operand must outlive.
    pub lifetime_bounds: HashMap<LifetimeBoundOperand<ty::Type>, HashSet<Lifetime>>,

    /// Set of trait associated type bounds.
    ///
    /// The key is the trait associated type reference and the value is the type that the trait
    /// associated type must be.
    pub trait_associated_type_bounds: HashMap<TraitAssociatedTypeRef, ty::Type>,

    /// Set of trait associated const bounds.
    ///
    /// The key is the trait associated const reference and the value is the constant that the
    /// trait associated const must be.
    pub trait_associated_const_bounds: HashMap<TraitAssociatedConstRef, Constant>,

    /// Set of for tuple clauses.
    ///
    /// The key is the set of type parameters that needs to be a tuple type and the value is the
    /// for tuple clause.
    pub for_tuple_clauses: HashMap<BTreeSet<TypeParameter>, ForTupleClause>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GenericItemRef {
    Struct(ID<Struct>),
    Enum(ID<Enum>),
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
