//! Contains all the definition of symbols used in the semantic analysis.

use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::Hash,
};

use enum_as_inner::EnumAsInner;
use pernixc_source::Span;

use crate::{
    constant, pattern,
    ty::{self, Lifetime},
};

/// Alias for the `usize` but with a more significant name.
///
/// Since indices will be used a lot in the semantic analysis phase, it is better to use a more
/// descriptive name for the type.
///
/// This is used to make a clear distinction between the `usize` that is used as an index and the
/// `usize` that is used as a count.
pub type Index = usize;

/// A direct translation of [`pernixc_syntax::syntax_tree::AccessModifier`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[allow(missing_docs)]
pub enum Accessibility {
    #[default]
    Public,
    Private,
    Internal,
}

/// A data structure providing both index and name based access to its elements.
///
/// This is commonly used to store members/associated items of a symbol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VecNameMap<T> {
    vec: Vec<T>,
    map: HashMap<String, Index>,
}

impl<T> Default for VecNameMap<T> {
    fn default() -> Self {
        Self {
            vec: Vec::default(),
            map: HashMap::default(),
        }
    }
}

impl<T> VecNameMap<T> {
    /// Creates a new empty [`VecNameMap`].
    #[must_use]
    pub fn new() -> Self {
        Self {
            vec: Vec::new(),
            map: HashMap::new(),
        }
    }

    /// Inserts a new value into the map.
    ///
    /// # Errors
    ///
    /// Returns [`Err`] with the index of the existing value if the name already exists.
    pub fn insert(&mut self, name: String, value: T) -> Result<Index, Index> {
        match self.map.entry(name) {
            Entry::Occupied(occupied) => Err(*occupied.get()),
            Entry::Vacant(occupied) => {
                let index = self.vec.len();
                occupied.insert(index);
                self.vec.push(value);
                Ok(index)
            }
        }
    }

    /// Returns the length of the collection.
    #[must_use]
    pub fn len(&self) -> usize { self.vec.len() }

    /// Returns `true` if the collection is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool { self.vec.is_empty() }

    /// Returns the index of the value with the given name.
    pub fn get_index_by_name<Q: ?Sized + Eq + Hash>(&self, name: &Q) -> Option<Index>
    where
        String: std::borrow::Borrow<Q>,
    {
        self.map.get(name).copied()
    }

    /// Returns an immutable reference to the value with the given index.
    #[must_use]
    pub fn get(&self, index: Index) -> Option<&T> { self.vec.get(index) }

    /// Returns a mutable reference to the value with the given index.
    pub fn get_mut(&mut self, index: Index) -> Option<&mut T> { self.vec.get_mut(index) }

    /// Returns an immutable reference to the value with the given name.
    #[must_use]
    pub fn get_by_name<Q: ?Sized + Eq + Hash>(&self, name: &Q) -> Option<&T>
    where
        String: std::borrow::Borrow<Q>,
    {
        self.map.get(name).map(|index| &self.vec[*index])
    }

    /// Returns a mutable reference to the value with the given name.
    pub fn get_mut_by_name<Q: ?Sized + Eq + Hash>(&mut self, name: &Q) -> Option<&mut T>
    where
        String: std::borrow::Borrow<Q>,
    {
        self.map.get(name).map(|index| &mut self.vec[*index])
    }

    /// Returns an iterator over the values of the map.
    pub fn iter(&self) -> impl Iterator<Item = &T> { self.vec.iter() }

    /// Returns a mutable iterator over the values of the map.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> { self.vec.iter_mut() }
}

impl<T> IntoIterator for VecNameMap<T> {
    type IntoIter = std::vec::IntoIter<T>;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter { self.vec.into_iter() }
}

impl<'a, T> IntoIterator for &'a VecNameMap<T> {
    type IntoIter = std::slice::Iter<'a, T>;
    type Item = &'a T;

    fn into_iter(self) -> Self::IntoIter { self.vec.iter() }
}

impl<'a, T> IntoIterator for &'a mut VecNameMap<T> {
    type IntoIter = std::slice::IterMut<'a, T>;
    type Item = &'a mut T;

    fn into_iter(self) -> Self::IntoIter { self.vec.iter_mut() }
}

impl<T> std::ops::Index<Index> for VecNameMap<T> {
    type Output = T;

    fn index(&self, index: Index) -> &Self::Output { &self.vec[index] }
}

impl<T> std::ops::IndexMut<Index> for VecNameMap<T> {
    fn index_mut(&mut self, index: Index) -> &mut Self::Output { &mut self.vec[index] }
}

impl<T, Q: ?Sized + Eq + Hash> std::ops::Index<&Q> for VecNameMap<T>
where
    String: std::borrow::Borrow<Q>,
{
    type Output = T;

    fn index(&self, index: &Q) -> &Self::Output { &self.vec[self.map[index]] }
}

impl<T, Q: ?Sized + Eq + Hash> std::ops::IndexMut<&Q> for VecNameMap<T>
where
    String: std::borrow::Borrow<Q>,
{
    fn index_mut(&mut self, index: &Q) -> &mut Self::Output { &mut self.vec[self.map[index]] }
}

/// A data structure used to refers to a member defined in a particular symbol.
///
/// Since there are some symbols that are defined in other symbols and usually requires multiple
/// references to refer to them, this data structure is used to store those references conveniently.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssociatedItemRef<ParentRef, AssociatedRef> {
    /// Reference to the parent symbol.
    pub parent_ref: ParentRef,

    /// Reference to the associated item that is stored in the parent symbol referenced by the
    /// [`Self::parent_ref`].
    pub associated_item_ref: AssociatedRef,
}

/// Represents a reference to the symbols that can be defined in a [`Module`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum ModuleMemberRef {
    Struct(Index),
    Enum(Index),
    Type(Index),
    Function(Index),
    Trait(Index),
    Module(Index),
    Constant(Index),
}

/// A reference used to refer to a [`GlobalItem`] symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum GlobalItemRef {
    Module(Index),
    Struct(Index),
    Enum(Index),
    Variant(AssociatedItemRef<Index, Index>),
    Type(Index),
    Constant(Index),
    Function(Index),
    Trait(Index),
    TraitAssociated(AssociatedItemRef<Index, TraitAssociatedRef>),
    Implements(AssociatedItemRef<Index, Index>),
    ImplementsAssociated(
        AssociatedItemRef<AssociatedItemRef<Index, Index>, ImplementsAssociatedRef>,
    ),
}

impl From<ModuleMemberRef> for GlobalItemRef {
    fn from(module_member_ref: ModuleMemberRef) -> Self {
        match module_member_ref {
            ModuleMemberRef::Module(index) => Self::Module(index),
            ModuleMemberRef::Struct(index) => Self::Struct(index),
            ModuleMemberRef::Enum(index) => Self::Enum(index),
            ModuleMemberRef::Type(index) => Self::Type(index),
            ModuleMemberRef::Function(index) => Self::Function(index),
            ModuleMemberRef::Trait(index) => Self::Trait(index),
            ModuleMemberRef::Constant(index) => Self::Constant(index),
        }
    }
}

/// Represents a kind of symbol that has a clear hierarchy/name and can be referenced globally by a
/// qualified name.
pub trait GlobalItem {
    /// The name of the symbol.
    fn name(&self) -> &str;

    /// The [`GlobalItemRef`] of this symbol.
    fn global_item_ref(&self) -> GlobalItemRef;

    /// Gets the [`GlobalItemRef`] that refers to the symbol that contains this symbol.
    fn parent(&self) -> Option<GlobalItemRef>;

    /// Searches the symbol with the given name defined in this symbol.
    fn get_member(&self, name: &str) -> Option<GlobalItemRef>;

    /// Gets the span of this symbol (if any).
    fn span(&self) -> Option<Span>;
}

impl GlobalItem for Module {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef { GlobalItemRef::Module(self.index) }

    fn parent(&self) -> Option<GlobalItemRef> {
        self.parent_module_index.map(GlobalItemRef::Module)
    }

    fn get_member(&self, name: &str) -> Option<GlobalItemRef> {
        self.module_member_refs_by_name
            .get(name)
            .copied()
            .map(Into::into)
    }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for Struct {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef { GlobalItemRef::Struct(self.index) }

    fn parent(&self) -> Option<GlobalItemRef> {
        Some(GlobalItemRef::Module(self.parent_module_index))
    }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for Enum {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef { GlobalItemRef::Enum(self.index) }

    fn parent(&self) -> Option<GlobalItemRef> {
        Some(GlobalItemRef::Module(self.parent_module_index))
    }

    fn get_member(&self, name: &str) -> Option<GlobalItemRef> {
        self.variants.get_index_by_name(name).map(|x| {
            GlobalItemRef::Variant(AssociatedItemRef {
                parent_ref: self.index,
                associated_item_ref: x,
            })
        })
    }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for Variant {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::Variant(AssociatedItemRef {
            parent_ref: self.parent_enum_index,
            associated_item_ref: self.index,
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> { Some(GlobalItemRef::Enum(self.parent_enum_index)) }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for Type {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef { GlobalItemRef::Type(self.index) }

    fn parent(&self) -> Option<GlobalItemRef> {
        Some(GlobalItemRef::Module(self.parent_module_index))
    }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for Constant {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef { GlobalItemRef::Constant(self.index) }

    fn parent(&self) -> Option<GlobalItemRef> {
        Some(GlobalItemRef::Module(self.parent_module_index))
    }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for Function {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef { GlobalItemRef::Function(self.index) }

    fn parent(&self) -> Option<GlobalItemRef> {
        Some(GlobalItemRef::Module(self.parent_module_index))
    }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for Trait {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef { GlobalItemRef::Trait(self.index) }

    fn parent(&self) -> Option<GlobalItemRef> {
        Some(GlobalItemRef::Module(self.parent_module_index))
    }

    fn get_member(&self, name: &str) -> Option<GlobalItemRef> {
        self.associated_ids_by_name.get(name).copied().map(|x| {
            GlobalItemRef::TraitAssociated(AssociatedItemRef {
                parent_ref: self.index,
                associated_item_ref: x,
            })
        })
    }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for TraitFunction {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::TraitAssociated(AssociatedItemRef {
            parent_ref: self.parent_trait_index,
            associated_item_ref: TraitAssociatedRef::Function(self.index),
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> {
        Some(GlobalItemRef::Trait(self.parent_trait_index))
    }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for TraitType {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::TraitAssociated(AssociatedItemRef {
            parent_ref: self.parent_trait_index,
            associated_item_ref: TraitAssociatedRef::Type(self.index),
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> {
        Some(GlobalItemRef::Trait(self.parent_trait_index))
    }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for TraitConstant {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::TraitAssociated(AssociatedItemRef {
            parent_ref: self.parent_trait_index,
            associated_item_ref: TraitAssociatedRef::Constant(self.index),
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> {
        Some(GlobalItemRef::Trait(self.parent_trait_index))
    }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { None }
}

impl GlobalItem for Implements {
    fn name(&self) -> &str { &self.trait_name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::Implements(AssociatedItemRef {
            parent_ref: self.parent_trait_index,
            associated_item_ref: self.index,
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> {
        Some(GlobalItemRef::Trait(self.parent_trait_index))
    }

    fn get_member(&self, name: &str) -> Option<GlobalItemRef> {
        self.associated_refs_by_name.get(name).copied().map(|x| {
            GlobalItemRef::ImplementsAssociated(AssociatedItemRef {
                parent_ref: AssociatedItemRef {
                    parent_ref: self.parent_trait_index,
                    associated_item_ref: self.index,
                },
                associated_item_ref: x,
            })
        })
    }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for ImplementsFunction {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::ImplementsAssociated(AssociatedItemRef {
            parent_ref: self.implements_ref,
            associated_item_ref: ImplementsAssociatedRef::Function(self.index),
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> {
        Some(GlobalItemRef::Implements(self.implements_ref))
    }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for ImplementsType {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::ImplementsAssociated(AssociatedItemRef {
            parent_ref: self.implements_ref,
            associated_item_ref: ImplementsAssociatedRef::Type(self.index),
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> {
        Some(GlobalItemRef::Implements(self.implements_ref))
    }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for ImplementsConstant {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::ImplementsAssociated(AssociatedItemRef {
            parent_ref: self.implements_ref,
            associated_item_ref: ImplementsAssociatedRef::Constant(self.index),
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> {
        Some(GlobalItemRef::Implements(self.implements_ref))
    }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { None }
}

/// A reference used to refer to a [`GenericItemRef`] symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum GenericItemRef {
    Enum(Index),
    Struct(Index),
    Type(Index),
    Function(Index),
    Trait(Index),
    TraitType(AssociatedItemRef<Index, Index>),
    TraitFunction(AssociatedItemRef<Index, Index>),
    Implements(AssociatedItemRef<Index, Index>),
    ImplementsType(AssociatedItemRef<AssociatedItemRef<Index, Index>, Index>),
    ImplementsFunction(AssociatedItemRef<AssociatedItemRef<Index, Index>, Index>),
}

impl TryFrom<GlobalItemRef> for GenericItemRef {
    type Error = GlobalItemRef;

    fn try_from(value: GlobalItemRef) -> Result<Self, Self::Error> {
        match value {
            GlobalItemRef::Module(_) => todo!(),
            GlobalItemRef::Struct(id) => Ok(Self::Struct(id)),
            GlobalItemRef::Enum(id) => Ok(Self::Enum(id)),
            GlobalItemRef::Variant(_) => todo!(),
            GlobalItemRef::Type(id) => Ok(Self::Type(id)),
            GlobalItemRef::Constant(_) => todo!(),
            GlobalItemRef::Function(id) => Ok(Self::Function(id)),
            GlobalItemRef::Trait(id) => Ok(Self::Trait(id)),
            GlobalItemRef::TraitAssociated(AssociatedItemRef {
                parent_ref,
                associated_item_ref: TraitAssociatedRef::Function(inner),
            }) => Ok(Self::TraitFunction(AssociatedItemRef {
                parent_ref,
                associated_item_ref: inner,
            })),
            GlobalItemRef::TraitAssociated(AssociatedItemRef {
                parent_ref,
                associated_item_ref: TraitAssociatedRef::Type(inner),
            }) => Ok(Self::TraitType(AssociatedItemRef {
                parent_ref,
                associated_item_ref: inner,
            })),
            GlobalItemRef::Implements(id) => Ok(Self::Implements(id)),
            GlobalItemRef::ImplementsAssociated(AssociatedItemRef {
                parent_ref,
                associated_item_ref: ImplementsAssociatedRef::Function(inner),
            }) => Ok(Self::ImplementsFunction(AssociatedItemRef {
                parent_ref,
                associated_item_ref: inner,
            })),
            GlobalItemRef::ImplementsAssociated(AssociatedItemRef {
                parent_ref,
                associated_item_ref: ImplementsAssociatedRef::Type(inner),
            }) => Ok(Self::ImplementsType(AssociatedItemRef {
                parent_ref,
                associated_item_ref: inner,
            })),
            found => Err(found),
        }
    }
}

impl From<GenericItemRef> for GlobalItemRef {
    fn from(value: GenericItemRef) -> Self {
        match value {
            GenericItemRef::Enum(id) => Self::Enum(id),
            GenericItemRef::Struct(id) => Self::Struct(id),
            GenericItemRef::Type(id) => Self::Type(id),
            GenericItemRef::Function(id) => Self::Function(id),
            GenericItemRef::Trait(id) => Self::Trait(id),
            GenericItemRef::TraitType(id) => Self::TraitAssociated(AssociatedItemRef {
                parent_ref: id.parent_ref,
                associated_item_ref: TraitAssociatedRef::Type(id.associated_item_ref),
            }),
            GenericItemRef::TraitFunction(id) => Self::TraitAssociated(AssociatedItemRef {
                parent_ref: id.parent_ref,
                associated_item_ref: TraitAssociatedRef::Function(id.associated_item_ref),
            }),
            GenericItemRef::Implements(id) => Self::Implements(id),
            GenericItemRef::ImplementsType(id) => Self::ImplementsAssociated(AssociatedItemRef {
                parent_ref: id.parent_ref,
                associated_item_ref: ImplementsAssociatedRef::Type(id.associated_item_ref),
            }),
            GenericItemRef::ImplementsFunction(id) => {
                Self::ImplementsAssociated(AssociatedItemRef {
                    parent_ref: id.parent_ref,
                    associated_item_ref: ImplementsAssociatedRef::Function(id.associated_item_ref),
                })
            }
        }
    }
}

/// Represents a kind of symbol that can define generic parameters and where clauses.
pub trait GenericItem {
    /// Returns an immutable reference to the generic parameters of this symbol.
    fn generic_parameters(&self) -> &GenericParameters;

    /// Returns an immutable reference to the where clause of this symbol.
    fn where_clause(&self) -> &WhereClause;

    /// Returns a mutable reference to the generic parameters of this symbol.
    fn generic_parameters_mut(&mut self) -> &mut GenericParameters;

    /// Returns a mutable reference to the where clause of this symbol.
    fn where_clause_mut(&mut self) -> &mut WhereClause;
}

impl GenericItem for Enum {
    fn generic_parameters(&self) -> &GenericParameters { &self.generic_parameters }

    fn where_clause(&self) -> &WhereClause { &self.where_clause }

    fn generic_parameters_mut(&mut self) -> &mut GenericParameters { &mut self.generic_parameters }

    fn where_clause_mut(&mut self) -> &mut WhereClause { &mut self.where_clause }
}

impl GenericItem for Struct {
    fn generic_parameters(&self) -> &GenericParameters { &self.generic_parameters }

    fn where_clause(&self) -> &WhereClause { &self.where_clause }

    fn generic_parameters_mut(&mut self) -> &mut GenericParameters { &mut self.generic_parameters }

    fn where_clause_mut(&mut self) -> &mut WhereClause { &mut self.where_clause }
}

impl GenericItem for Type {
    fn generic_parameters(&self) -> &GenericParameters { &self.generic_parameters }

    fn where_clause(&self) -> &WhereClause { &self.where_clause }

    fn generic_parameters_mut(&mut self) -> &mut GenericParameters { &mut self.generic_parameters }

    fn where_clause_mut(&mut self) -> &mut WhereClause { &mut self.where_clause }
}

impl GenericItem for Function {
    fn generic_parameters(&self) -> &GenericParameters { &self.generic_parameters }

    fn where_clause(&self) -> &WhereClause { &self.where_clause }

    fn generic_parameters_mut(&mut self) -> &mut GenericParameters { &mut self.generic_parameters }

    fn where_clause_mut(&mut self) -> &mut WhereClause { &mut self.where_clause }
}

impl GenericItem for Trait {
    fn generic_parameters(&self) -> &GenericParameters { &self.generic_parameters }

    fn where_clause(&self) -> &WhereClause { &self.where_clause }

    fn generic_parameters_mut(&mut self) -> &mut GenericParameters { &mut self.generic_parameters }

    fn where_clause_mut(&mut self) -> &mut WhereClause { &mut self.where_clause }
}

impl GenericItem for TraitType {
    fn generic_parameters(&self) -> &GenericParameters { &self.generic_parameters }

    fn where_clause(&self) -> &WhereClause { &self.where_clause }

    fn generic_parameters_mut(&mut self) -> &mut GenericParameters { &mut self.generic_parameters }

    fn where_clause_mut(&mut self) -> &mut WhereClause { &mut self.where_clause }
}

impl GenericItem for TraitFunction {
    fn generic_parameters(&self) -> &GenericParameters { &self.generic_parameters }

    fn where_clause(&self) -> &WhereClause { &self.where_clause }

    fn generic_parameters_mut(&mut self) -> &mut GenericParameters { &mut self.generic_parameters }

    fn where_clause_mut(&mut self) -> &mut WhereClause { &mut self.where_clause }
}

impl GenericItem for Implements {
    fn generic_parameters(&self) -> &GenericParameters { &self.signature.generic_parameters }

    fn where_clause(&self) -> &WhereClause { &self.where_clause }

    fn generic_parameters_mut(&mut self) -> &mut GenericParameters {
        &mut self.signature.generic_parameters
    }

    fn where_clause_mut(&mut self) -> &mut WhereClause { &mut self.where_clause }
}

impl GenericItem for ImplementsType {
    fn generic_parameters(&self) -> &GenericParameters { &self.generic_parameters }

    fn where_clause(&self) -> &WhereClause { &self.where_clause }

    fn generic_parameters_mut(&mut self) -> &mut GenericParameters { &mut self.generic_parameters }

    fn where_clause_mut(&mut self) -> &mut WhereClause { &mut self.where_clause }
}

impl GenericItem for ImplementsFunction {
    fn generic_parameters(&self) -> &GenericParameters { &self.generic_parameters }

    fn where_clause(&self) -> &WhereClause { &self.where_clause }

    fn generic_parameters_mut(&mut self) -> &mut GenericParameters { &mut self.generic_parameters }

    fn where_clause_mut(&mut self) -> &mut WhereClause { &mut self.where_clause }
}

/// Contains all the generic parameters that can be defined in a symbol.
#[derive(Debug, Clone, Default)]
#[allow(missing_docs)]
pub struct GenericParameters {
    pub lifetimes: VecNameMap<LifetimeParameter>,
    pub types: VecNameMap<TypeParameter>,
    pub constants: VecNameMap<ConstantParameter>,
}

/// An operand that will be tested against other lifetimes whether or not the operand outlives the
/// other lifetimes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeBoundOperand {
    LifetimeParameter(GenericParameterRef),
    Type(ty::Type),
}

/// Represents a higher-ranked lifetime, denoted by `for<'a>`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HigherRankedLifetime {
    /// The unique [`usize`] value that distinguishes this lifetime from other lifetimes.
    pub unique_id: usize,
}

/// A lifetime that may be higher-ranked. Used in [`TraitBound`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum HigherRankedableLifetime {
    Regular(Lifetime),
    HigherRanked(HigherRankedLifetime),
}

/// Represents a trait bound in the where clause.
///
/// This bound states that there must be an `implements` for the trait with the given index and
/// generic arguments.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TraitBound {
    pub trait_index: Index,
    pub type_substituions: Vec<ty::Type>,
    pub lifetime_substituions: Vec<HigherRankedableLifetime>,
    pub constant_substituions: Vec<constant::Constant>,
}

/// Represents a where clause that can be defined in a symbol.
#[derive(Debug, Clone, Default)]
pub struct WhereClause {
    /// Represents lifetime bounds, denoted by `operand: 'a + 'static` syntax.
    ///
    /// The key represents the operand, and the value represents the lifetimes that the operand
    /// must outlive to satisfy the bound.
    pub lifetime_bounds: HashMap<LifetimeBoundOperand, HashSet<Lifetime>>,

    /// Represents type bounds, denoted by `trait<args>::associated<args> = type` syntax.
    ///
    /// The key represents the associated type, and the value represents the type that when the
    /// associated type is resolved, the type must be equal to the value.
    pub associated_type_bounds: HashMap<ty::TraitAssociated, ty::Type>,

    /// Represents constant bounds, denoted by `trait<args>::associated<args> = {constant}` syntax.
    ///
    /// The key represents the associated constant, and the value represents the constant that when
    /// the associated constant is resolved, the constant must be equal to the value.
    pub associated_constant_bounds: HashMap<constant::TraitAssociated, constant::Constant>,

    /// Represents trait bounds, denoted by `trait<args>` syntax.
    ///
    /// The value represents whether or not the trait bound is constant.
    pub trait_bounds: HashMap<TraitBound, bool>,
}

/// A reference to a generic parameter of a particular [`GenericItem`] symbol.
///
/// **Depends on the context**, this reference can be used to refer to a lifetime parameter, a type
/// parameter, or a constant parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericParameterRef {
    /// The reference to the [`GenericItem`] that defines this parameter.
    pub generic_item_ref: GenericItemRef,

    /// The index of this parameter in the [`GenericParameters`].
    ///
    /// **Depends on the context**, this index can be used to refer to any kind of generic
    /// parameter.
    pub index: Index,
}

/// Represents a generic type parameter.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct TypeParameter {
    pub name: String,
    pub index: Index,
    pub parent_generic_item_ref: GenericItemRef,
    pub span: Option<Span>,
}

/// Represents a generic lifetime parameter.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct LifetimeParameter {
    pub name: String,
    pub index: Index,
    pub parent_generic_item_ref: GenericItemRef,
    pub span: Option<Span>,
}

/// Represents a generic constant parameter.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ConstantParameter {
    pub name: String,
    pub index: Index,
    pub ty: ty::Type,
    pub parent_generic_item_ref: GenericItemRef,
    pub span: Option<Span>,
}

/// Represents a generic arguments substituion to a particular [`GenericItem`] symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct LocalSubstitution {
    /// Represents the lifetime substitutions for each parameter defined in
    /// [`GenericParameters::lifetimes`].
    pub lifetime_substitutions: Vec<Lifetime>,

    /// Represents the type substitutions for each parameter defined in
    /// [`GenericParameters::types`].
    pub type_substitutions: Vec<ty::Type>,

    /// Represents the constant substitutions for each parameter defined in
    /// [`GenericParameters::constants`].
    pub constant_substitutions: Vec<constant::Constant>,
}

/// Represents a particular variant case of an enum.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Variant {
    pub name: String,
    pub index: Index,
    pub parent_enum_index: Index,
    pub ty: Option<ty::Type>,
    pub span: Option<Span>,
}

/// Represents an enum symbol.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Enum {
    pub name: String,
    pub index: Index,
    pub accessibility: Accessibility,
    pub parent_module_index: Index,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub variants: VecNameMap<Variant>,
    pub span: Option<Span>,
}

/// Represents a field of a struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Field {
    pub name: String,
    pub index: Index,
    pub accessibility: Accessibility,
    pub parent_struct_index: Index,
    pub ty: ty::Type,
}

/// Represents a struct symbol.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Struct {
    pub name: String,
    pub index: Index,
    pub accessibility: Accessibility,
    pub parent_module_index: Index,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub fields: VecNameMap<Field>,
    pub span: Option<Span>,
}

/// Represents a type alias symbol, denoted by `type name = type` syntax.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Type {
    pub name: String,
    pub index: Index,
    pub accessibility: Accessibility,
    pub parent_module_index: Index,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub ty: ty::Type,
    pub span: Option<Span>,
}

/// Represents a constant symbol.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Constant {
    pub name: String,
    pub index: Index,
    pub accessibility: Accessibility,
    pub parent_module_index: Index,
    pub ty: ty::Type,
    pub constant: constant::Constant,
    pub span: Option<Span>,
}

/// Represents a function parameter.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Parameter {
    pub index: Index,
    pub parent_function_index: Index,
    pub ty: ty::Type,
    pub pattern: pattern::Irrefutable,
}

/// Represents a function symbol.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Function {
    pub name: String,
    pub index: Index,
    pub accessibility: Accessibility,
    pub parent_module_index: Index,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub parameters: Vec<Parameter>,
    pub return_type: ty::Type,
    pub span: Option<Span>,
}

/// Represents a trait associated type.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct TraitType {
    pub name: String,
    pub index: Index,
    pub parent_trait_index: Index,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub span: Option<Span>,
}

/// Represents a trait associated function.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct TraitFunction {
    pub name: String,
    pub index: Index,
    pub parent_trait_index: Index,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub parameters: Vec<Parameter>,
    pub return_type: ty::Type,
    pub span: Option<Span>,
}

/// Represents a trait associated constant.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TraitConstant {
    pub name: String,
    pub index: Index,
    pub parent_trait_index: Index,
    pub ty: ty::Type,
}

/// Represents a reference to all three kinds of trait associated items.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum TraitAssociatedRef {
    Function(Index),
    Type(Index),
    Constant(Index),
}

/// Represents a signature of an `implements`.
#[derive(Debug, Clone)]
pub struct ImplementsSignature {
    /// Defined generic parameters of the `implements`, denoted by `implements<params>`.
    pub generic_parameters: GenericParameters,

    /// The generic arguments substituions to the implemented trait.
    pub trait_substitution: LocalSubstitution,
}

/// Represents a reference to all three kinds of `implements` associated items.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum ImplementsAssociatedRef {
    Type(Index),
    Function(Index),
    Constant(Index),
}

/// Represents a `implements` of a trait.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Implements {
    pub index: Index,
    pub signature: ImplementsSignature,
    pub is_const_implements: bool,
    pub where_clause: WhereClause,
    pub parent_trait_index: Index,
    pub span: Option<Span>,
    pub associated_refs_by_name: HashMap<String, ImplementsAssociatedRef>,
    pub types: Vec<ImplementsType>,
    pub functions: Vec<ImplementsFunction>,
    pub constants: Vec<ImplementsConstant>,
    pub declared_in_module_index: Index,
    pub trait_name: String,
}

/// Represents an implements associated constant.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ImplementsConstant {
    pub name: String,
    pub implements_ref: AssociatedItemRef<Index, Index>,
    pub span: Option<Span>,
    pub index: Index,
}

/// Represents an implements associated function.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ImplementsFunction {
    pub name: String,
    pub implements_ref: AssociatedItemRef<Index, Index>,
    pub span: Option<Span>,
    pub index: Index,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
}

/// Represents an implements associated type.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ImplementsType {
    pub name: String,
    pub implements_ref: AssociatedItemRef<Index, Index>,
    pub span: Option<Span>,
    pub index: Index,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
}

/// Represents a trait symbol.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Trait {
    pub name: String,
    pub index: Index,
    pub accessibility: Accessibility,
    pub parent_module_index: Index,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub functions: Vec<TraitFunction>,
    pub types: Vec<TraitType>,
    pub constants: Vec<TraitConstant>,
    pub associated_ids_by_name: HashMap<String, TraitAssociatedRef>,
    pub implements: Vec<Implements>,
    pub negative_implements: Vec<ImplementsSignature>,
    pub span: Option<Span>,
}

/// Represents a module declaration.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Module {
    pub name: String,
    pub index: Index,
    pub accessibility: Accessibility,
    pub parent_module_index: Option<Index>,
    pub module_member_refs_by_name: HashMap<String, ModuleMemberRef>,
    pub usings: HashMap<Index, Span>,
    pub span: Option<Span>,
}

/// Represents generic arguments substituions on multiple [`GenericItem`] boundaries.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Substitution {
    pub type_substitutions: HashMap<GenericParameterRef, ty::Type>,
    pub constant_substitutions: HashMap<GenericParameterRef, constant::Constant>,
    pub lifetime_substitutions: HashMap<GenericParameterRef, Lifetime>,
    pub associated_type_substitutions: HashMap<ty::TraitAssociated, ty::Type>,
    pub associated_constant_substitutions: HashMap<constant::TraitAssociated, constant::Constant>,
}
