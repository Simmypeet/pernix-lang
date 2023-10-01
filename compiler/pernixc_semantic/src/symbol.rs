//! Contains all the definition of symbols used in the semantic analysis.
//!
//! ## Implementation
//!
//! The symbol table is a graph-like data structure in its nature. For that reason, it might not
//! resonate well with the rust's borrow checker. To get around with that, the references to the
//! symbols are represented by indices instead of references (`&` or `Arc`) to the symbols.
//!
//! Indices are represented by `usize` type and are used to index for all kinds of symbols in the
//! table. Having one type (`usize`) for indexing all kinds of symbols in the table might be
//! confusing, therefore, there are various new-type wrappers around `usize` to represent different
//! kinds of indices to different kinds of symbols.
//!
//! The convention is to name the new-type wrappers  with `*Ref*` suffix, where `*` is the name of
//! the symbol that the index refers to. For example, an index used to refer to a `Module` symbol
//! is named `ModuleRef`.

use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::Hash,
};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;

use crate::{
    constant, pattern,
    ty::{self, Lifetime},
};

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
    map: HashMap<String, usize>,
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
    pub fn insert(&mut self, name: String, value: T) -> Result<usize, usize> {
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
    pub fn get_index_by_name<Q: ?Sized + Eq + Hash>(&self, name: &Q) -> Option<usize>
    where
        String: std::borrow::Borrow<Q>,
    {
        self.map.get(name).copied()
    }

    /// Returns an immutable reference to the value with the given index.
    #[must_use]
    pub fn get(&self, index: usize) -> Option<&T> { self.vec.get(index) }

    /// Returns a mutable reference to the value with the given index.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> { self.vec.get_mut(index) }

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

impl<T> std::ops::Index<usize> for VecNameMap<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output { &self.vec[index] }
}

impl<T> std::ops::IndexMut<usize> for VecNameMap<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output { &mut self.vec[index] }
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

macro_rules! declare_ref {
    ($struct_name:ident) => {
        paste::paste! {
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            #[allow(missing_docs)]
            pub struct [< $struct_name Ref >](pub usize);
        }
    };
}

declare_ref!(Struct);
declare_ref!(Enum);
declare_ref!(Type);
declare_ref!(Function);
declare_ref!(Trait);
declare_ref!(Module);
declare_ref!(Constant);
declare_ref!(LocalVariant);
declare_ref!(LocalTraitType);
declare_ref!(LocalTraitFunction);
declare_ref!(LocalTraitConstant);
declare_ref!(LocalImplements);
declare_ref!(LocalImplementsType);
declare_ref!(LocalImplementsFunction);
declare_ref!(LocalImplementsConstant);

/// An enumeration containig all kinds of local references to the trait associated symbols.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum LocalTraitAssociatedRef {
    Type(LocalTraitTypeRef),
    Function(LocalTraitFunctionRef),
    Constant(LocalTraitConstantRef),
}

/// An enumeration containig all kinds of local references to the implements associated symbols.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, From, EnumAsInner)]
#[allow(missing_docs)]
pub enum LocalImplementsAssociatedRef {
    Type(LocalImplementsTypeRef),
    Function(LocalImplementsFunctionRef),
    Constant(LocalImplementsConstantRef),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TraitAssociatedRef {
    pub trait_ref: TraitRef,
    pub local_ref: LocalTraitAssociatedRef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct ImplementsRef {
    pub trait_ref: TraitRef,
    pub local_ref: LocalImplementsRef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct ImplementsAssociatedRef {
    pub implements_ref: ImplementsRef,
    pub local_ref: LocalImplementsAssociatedRef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct VariantRef {
    pub enum_ref: EnumRef,
    pub local_ref: LocalVariantRef,
}

/// Represents a reference to the symbols that can be defined in a [`Module`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ModuleMemberRef {
    Struct(StructRef),
    Enum(EnumRef),
    Type(TypeRef),
    Function(FunctionRef),
    Trait(TraitRef),
    Module(ModuleRef),
    Constant(ConstantRef),
}

/// A reference used to refer to a [`GlobalItem`] symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum GlobalItemRef {
    Module(ModuleRef),
    Struct(StructRef),
    Enum(EnumRef),
    Variant(VariantRef),
    Type(TypeRef),
    Constant(ConstantRef),
    Function(FunctionRef),
    Trait(TraitRef),
    TraitAssociated(TraitAssociatedRef),
    Implements(ImplementsRef),
    ImplementsAssociated(ImplementsAssociatedRef),
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

    fn global_item_ref(&self) -> GlobalItemRef { self.module_ref.into() }

    fn parent(&self) -> Option<GlobalItemRef> { self.parent_module_ref.map(Into::into) }

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

    fn global_item_ref(&self) -> GlobalItemRef { self.struct_ref.into() }

    fn parent(&self) -> Option<GlobalItemRef> { Some(self.parent_module_ref.into()) }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for Enum {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef { self.enum_ref.into() }

    fn parent(&self) -> Option<GlobalItemRef> { Some(self.parent_module_ref.into()) }

    fn get_member(&self, name: &str) -> Option<GlobalItemRef> {
        self.variants.get_index_by_name(name).map(|x| {
            GlobalItemRef::Variant(VariantRef {
                enum_ref: self.enum_ref,
                local_ref: LocalVariantRef(x),
            })
        })
    }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for Variant {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::Variant(VariantRef {
            enum_ref: self.parent_enum_ref,
            local_ref: self.local_variant_ref,
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> { Some(self.parent_enum_ref.into()) }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for Type {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef { self.type_ref.into() }

    fn parent(&self) -> Option<GlobalItemRef> { Some(self.parent_module_ref.into()) }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for Constant {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef { self.constant_ref.into() }

    fn parent(&self) -> Option<GlobalItemRef> { Some(self.parent_module_ref.into()) }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for Function {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef { self.function_ref.into() }

    fn parent(&self) -> Option<GlobalItemRef> { Some(self.parent_module_ref.into()) }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for Trait {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef { self.trait_ref.into() }

    fn parent(&self) -> Option<GlobalItemRef> { Some(self.parent_module_ref.into()) }

    fn get_member(&self, name: &str) -> Option<GlobalItemRef> {
        self.associated_refs_by_name.get(name).copied().map(|x| {
            GlobalItemRef::TraitAssociated(TraitAssociatedRef {
                trait_ref: self.trait_ref,
                local_ref: x,
            })
        })
    }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for TraitFunction {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::TraitAssociated(TraitAssociatedRef {
            trait_ref: self.parent_trait_ref,
            local_ref: self.trait_function_ref.into(),
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> { Some(self.parent_trait_ref.into()) }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for TraitType {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::TraitAssociated(TraitAssociatedRef {
            trait_ref: self.parent_trait_ref,
            local_ref: self.trait_type_ref.into(),
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> { Some(self.parent_trait_ref.into()) }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for TraitConstant {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::TraitAssociated(TraitAssociatedRef {
            trait_ref: self.parent_trait_ref,
            local_ref: self.trait_constant_ref.into(),
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> { Some(self.parent_trait_ref.into()) }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { None }
}

impl GlobalItem for Implements {
    fn name(&self) -> &str { &self.trait_name }

    fn global_item_ref(&self) -> GlobalItemRef {
        ImplementsRef {
            trait_ref: self.parent_trait_ref,
            local_ref: self.implements_ref,
        }
        .into()
    }

    fn parent(&self) -> Option<GlobalItemRef> { Some(self.parent_trait_ref.into()) }

    fn get_member(&self, name: &str) -> Option<GlobalItemRef> {
        self.associated_refs_by_name.get(name).copied().map(|x| {
            GlobalItemRef::ImplementsAssociated(ImplementsAssociatedRef {
                implements_ref: ImplementsRef {
                    trait_ref: self.parent_trait_ref,
                    local_ref: self.implements_ref,
                },
                local_ref: x,
            })
        })
    }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for ImplementsFunction {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::ImplementsAssociated(ImplementsAssociatedRef {
            implements_ref: self.parent_implements_ref,
            local_ref: self.implements_function_ref.into(),
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> { Some(self.parent_implements_ref.into()) }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for ImplementsType {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::ImplementsAssociated(ImplementsAssociatedRef {
            implements_ref: self.parent_implements_ref,
            local_ref: self.implements_type_ref.into(),
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> { Some(self.parent_implements_ref.into()) }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for ImplementsConstant {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::ImplementsAssociated(ImplementsAssociatedRef {
            implements_ref: self.parent_implements_ref,
            local_ref: self.implements_constant_ref.into(),
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> { Some(self.parent_implements_ref.into()) }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { None }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TraitTypeRef {
    pub trait_ref: TraitRef,
    pub local_ref: LocalTraitTypeRef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TraitFunctionRef {
    pub trait_ref: TraitRef,
    pub local_ref: LocalTraitFunctionRef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TraitConstantRef {
    pub trait_ref: TraitRef,
    pub local_ref: LocalTraitConstantRef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct ImplementsTypeRef {
    pub implements_ref: ImplementsRef,
    pub local_ref: LocalImplementsTypeRef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct ImplementsFunctionRef {
    pub implements_ref: ImplementsRef,
    pub local_ref: LocalImplementsFunctionRef,
}

/// A reference used to refer to a [`GenericItemRef`] symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum GenericItemRef {
    Enum(EnumRef),
    Struct(StructRef),
    Type(TypeRef),
    Function(FunctionRef),
    Trait(TraitRef),
    TraitType(TraitTypeRef),
    TraitFunction(TraitFunctionRef),
    Implements(ImplementsRef),
    ImplementsType(ImplementsTypeRef),
    ImplementsFunction(ImplementsFunctionRef),
}

impl TryFrom<GlobalItemRef> for GenericItemRef {
    type Error = GlobalItemRef;

    fn try_from(value: GlobalItemRef) -> Result<Self, Self::Error> {
        match value {
            GlobalItemRef::Struct(id) => Ok(Self::Struct(id)),
            GlobalItemRef::Enum(id) => Ok(Self::Enum(id)),
            GlobalItemRef::Type(id) => Ok(Self::Type(id)),
            GlobalItemRef::Function(id) => Ok(Self::Function(id)),
            GlobalItemRef::Trait(id) => Ok(Self::Trait(id)),
            GlobalItemRef::TraitAssociated(TraitAssociatedRef {
                trait_ref,
                local_ref: LocalTraitAssociatedRef::Function(local_ref),
            }) => Ok(Self::TraitFunction(TraitFunctionRef {
                trait_ref,
                local_ref,
            })),
            GlobalItemRef::TraitAssociated(TraitAssociatedRef {
                trait_ref,
                local_ref: LocalTraitAssociatedRef::Type(local_ref),
            }) => Ok(Self::TraitType(TraitTypeRef {
                trait_ref,
                local_ref,
            })),
            GlobalItemRef::Implements(id) => Ok(Self::Implements(id)),
            GlobalItemRef::ImplementsAssociated(ImplementsAssociatedRef {
                implements_ref,
                local_ref: LocalImplementsAssociatedRef::Function(local_ref),
            }) => Ok(Self::ImplementsFunction(ImplementsFunctionRef {
                implements_ref,
                local_ref,
            })),
            GlobalItemRef::ImplementsAssociated(ImplementsAssociatedRef {
                implements_ref,
                local_ref: LocalImplementsAssociatedRef::Type(local_ref),
            }) => Ok(Self::ImplementsType(ImplementsTypeRef {
                implements_ref,
                local_ref,
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
            GenericItemRef::TraitType(id) => Self::TraitAssociated(TraitAssociatedRef {
                trait_ref: id.trait_ref,
                local_ref: LocalTraitAssociatedRef::Type(id.local_ref),
            }),
            GenericItemRef::TraitFunction(id) => Self::TraitAssociated(TraitAssociatedRef {
                trait_ref: id.trait_ref,
                local_ref: LocalTraitAssociatedRef::Function(id.local_ref),
            }),
            GenericItemRef::Implements(id) => Self::Implements(id),
            GenericItemRef::ImplementsType(id) => {
                Self::ImplementsAssociated(ImplementsAssociatedRef {
                    implements_ref: id.implements_ref,
                    local_ref: LocalImplementsAssociatedRef::Type(id.local_ref),
                })
            }
            GenericItemRef::ImplementsFunction(id) => {
                Self::ImplementsAssociated(ImplementsAssociatedRef {
                    implements_ref: id.implements_ref,
                    local_ref: LocalImplementsAssociatedRef::Function(id.local_ref),
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
    LifetimeParameter(LifetimeParameterRef),
    Type(ty::Type),
}

/// Represents a higher-ranked lifetime, denoted by `for<'a>`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HigherRankedLifetime {
    /// The unique [`usize`] value that distinguishes this lifetime from other lifetimes.
    pub unique_id: usize,
}

/// A lifetime that may be higher-ranked. Used in [`TraitBound`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
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
    pub trait_ref: TraitRef,
    pub type_substituions: Vec<ty::Type>,
    pub lifetime_substituions: Vec<HigherRankedableLifetime>,
    pub constant_substituions: Vec<constant::Constant>,
}

/// Contains both lifetime and trait bounds.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Bounds {
    /// Represents lifetime bounds, denoted by `operand: 'a + 'static` syntax.
    ///
    /// The key represents the operand, and the value represents the lifetimes that the operand
    /// must outlive to satisfy the bound.
    pub lifetime_bounds: HashMap<LifetimeBoundOperand, HashSet<Lifetime>>,

    /// Represents trait bounds, denoted by `trait<args>` syntax.
    ///
    /// The value represents whether or not the trait bound is constant.
    pub trait_bounds: HashMap<TraitBound, bool>,
}

impl TraitBound {
    /// Checks if `self` trait bound is a subset or equivalent trait bound of the `source` trait
    /// bound.
    fn is_subset_or_equal(&self, source: &Self) -> bool {
        if !(self.trait_ref == source.trait_ref
            && self.type_substituions == source.type_substituions
            && self.constant_substituions == source.constant_substituions)
        {
            return false;
        }

        assert_eq!(
            self.lifetime_substituions.len(),
            source.lifetime_substituions.len()
        );

        let mut source_higher_ranked_lifetime_map: HashMap<
            HigherRankedLifetime,
            HigherRankedableLifetime,
        > = HashMap::new();

        for (target_lt, source_lt) in self
            .lifetime_substituions
            .iter()
            .copied()
            .zip(source.lifetime_substituions.iter().copied())
        {
            match (target_lt, source_lt) {
                // arbitrary lifetime check
                (
                    HigherRankedableLifetime::Regular(target),
                    HigherRankedableLifetime::Regular(source),
                ) => {
                    if target != source {
                        return false;
                    }
                }
                // maps the higher ranked trait bound of source to target lt
                (target_lt, HigherRankedableLifetime::HigherRanked(source)) => {
                    match source_higher_ranked_lifetime_map.entry(source) {
                        Entry::Occupied(existing) => {
                            if *existing.get() != target_lt {
                                return false;
                            }
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(target_lt);
                        }
                    }
                }
                _ => return false,
            }
        }

        true
    }
}

impl Bounds {
    pub(super) fn try_add_trait_bound(
        trait_bounds: &mut HashMap<TraitBound, bool>,
        trait_bound: TraitBound,
        is_const: bool,
    ) {
        let mut is_subset_or_equal = false;
        for (existing_trait_bound, existing_is_const) in trait_bounds.iter_mut() {
            if trait_bound.is_subset_or_equal(existing_trait_bound)
                && (!is_const || *existing_is_const)
            {
                is_subset_or_equal = true;
                break;
            }
        }

        // there is already a superset trait bound, so we don't need to add this trait bound
        if is_subset_or_equal {
            return;
        }

        let mut removing_trait_bounds = Vec::new();

        for (existing_trait_bound, existing_is_const) in trait_bounds.iter_mut() {
            if existing_trait_bound.is_subset_or_equal(&trait_bound)
                && (!*existing_is_const || is_const)
            {
                removing_trait_bounds.push(existing_trait_bound.clone());
            }
        }

        for removing_trait_bound in removing_trait_bounds {
            assert!(trait_bounds.remove(&removing_trait_bound).is_some());
        }

        assert!(trait_bounds.insert(trait_bound, is_const).is_none());
    }

    /// Combines two [`Bounds`] into one.
    #[must_use]
    pub fn combine(first: &Self, second: &Self) -> Self {
        let mut result = first.clone();

        // combine lifetime bound
        for (operand, bound) in &second.lifetime_bounds {
            let bounds = result.lifetime_bounds.entry(operand.clone()).or_default();

            for bound in bound {
                bounds.insert(*bound);
            }
        }

        // combine trait bound
        for (trait_bound, is_const) in &second.trait_bounds {
            Self::try_add_trait_bound(&mut result.trait_bounds, trait_bound.clone(), *is_const);
        }

        result
    }
}

/// Represents a where clause that can be defined in a symbol.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct WhereClause {
    /// Contains both trait-associated type and constant bounds, denoted by `trait<args>::assoc =
    /// bound`
    pub associated_bounds: AssociatedBounds,

    /// Contains both lifetime and trait bounds.
    pub bounds: Bounds,
}

declare_ref!(LocalLifetimeParameter);
declare_ref!(LocalTypeParameter);
declare_ref!(LocalConstantParameter);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct LifetimeParameterRef {
    pub generic_item_ref: GenericItemRef,
    pub local_ref: LocalLifetimeParameterRef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TypeParameterRef {
    pub generic_item_ref: GenericItemRef,
    pub local_ref: LocalTypeParameterRef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct ConstantParameterRef {
    pub generic_item_ref: GenericItemRef,
    pub local_ref: LocalConstantParameterRef,
}

/// Represents a generic type parameter.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct TypeParameter {
    pub name: String,
    pub local_type_parameter_ref: LocalTypeParameterRef,
    pub parent_generic_item_ref: GenericItemRef,
    pub span: Option<Span>,
}

/// Represents a generic lifetime parameter.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct LifetimeParameter {
    pub name: String,
    pub local_lifetime_parameter_ref: LocalLifetimeParameterRef,
    pub parent_generic_item_ref: GenericItemRef,
    pub span: Option<Span>,
}

/// Represents a generic constant parameter.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ConstantParameter {
    pub name: String,
    pub local_constant_parameter_ref: LocalConstantParameterRef,
    pub ty: ty::Type,
    pub parent_generic_item_ref: GenericItemRef,
    pub span: Option<Span>,
}

/// Represents a generic arguments substituion to a particular [`GenericItem`] symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct LocalSubstitution {
    /// Represents the lifetime substitutions for each parameter defined in
    /// [`GenericParameters::lifetimes`].
    pub lifetimes: Vec<Lifetime>,

    /// Represents the type substitutions for each parameter defined in
    /// [`GenericParameters::types`].
    pub types: Vec<ty::Type>,

    /// Represents the constant substitutions for each parameter defined in
    /// [`GenericParameters::constants`].
    pub constants: Vec<constant::Constant>,
}

impl LocalSubstitution {
    /// Checks whether or not this substitution is concrete.
    ///
    /// A substitution is concrete if it does not contain any generic arguments (except for
    /// lifetimes)..
    #[must_use]
    pub fn is_concrete(&self) -> bool {
        self.types.iter().all(ty::Type::is_concrete)
            && self.constants.iter().all(constant::Constant::is_concrete)
    }
}

/// Represents a particular variant case of an enum.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Variant {
    pub name: String,
    pub local_variant_ref: LocalVariantRef,
    pub parent_enum_ref: EnumRef,
    pub ty: Option<ty::Type>,
    pub span: Option<Span>,
}

/// Represents an enum symbol.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Enum {
    pub name: String,
    pub enum_ref: EnumRef,
    pub accessibility: Accessibility,
    pub parent_module_ref: ModuleRef,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub variants: VecNameMap<Variant>,
    pub span: Option<Span>,
}

declare_ref!(LocalField);

/// Represents a field of a struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Field {
    pub name: String,
    pub field: LocalFieldRef,
    pub accessibility: Accessibility,
    pub parent_struct_ref: StructRef,
    pub ty: ty::Type,
}

/// Represents a struct symbol.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Struct {
    pub name: String,
    pub struct_ref: StructRef,
    pub accessibility: Accessibility,
    pub parent_module_ref: ModuleRef,
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
    pub type_ref: TypeRef,
    pub accessibility: Accessibility,
    pub parent_module_ref: ModuleRef,
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
    pub constant_ref: ConstantRef,
    pub accessibility: Accessibility,
    pub parent_module_ref: ModuleRef,
    pub ty: ty::Type,
    pub constant: constant::Constant,
    pub span: Option<Span>,
}

declare_ref!(LocalParameter);

/// Represents a function parameter.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Parameter<T> {
    pub parameter_ref: LocalParameterRef,
    pub parent_ref: T,
    pub ty: ty::Type,
    pub pattern: pattern::Irrefutable,
}

/// Represents a function symbol.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Function {
    pub name: String,
    pub function_ref: FunctionRef,
    pub accessibility: Accessibility,
    pub parent_module_ref: ModuleRef,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub parameters: Vec<Parameter<FunctionRef>>,
    pub return_type: ty::Type,
    pub span: Option<Span>,
}

/// Represents a trait associated type.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct TraitType {
    pub name: String,
    pub trait_type_ref: LocalTraitTypeRef,
    pub parent_trait_ref: TraitRef,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub span: Option<Span>,
}

/// Represents a trait associated function.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct TraitFunction {
    pub name: String,
    pub trait_function_ref: LocalTraitFunctionRef,
    pub parent_trait_ref: TraitRef,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub parameters: Vec<Parameter<TraitFunctionRef>>,
    pub return_type: ty::Type,
    pub span: Option<Span>,
}

/// Represents a trait associated constant.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TraitConstant {
    pub name: String,
    pub trait_constant_ref: LocalTraitConstantRef,
    pub parent_trait_ref: TraitRef,
    pub ty: ty::Type,
}

/// Represents a signature of an `implements`.
#[derive(Debug, Clone)]
pub struct ImplementsSignature {
    /// Defined generic parameters of the `implements`, denoted by `implements<params>`.
    pub generic_parameters: GenericParameters,

    /// The generic arguments substituions to the implemented trait.
    pub trait_substitution: LocalSubstitution,
}

/// Represents a `implements` of a trait.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Implements {
    pub implements_ref: LocalImplementsRef,
    pub signature: ImplementsSignature,
    pub is_const_implements: bool,
    pub where_clause: WhereClause,
    pub parent_trait_ref: TraitRef,
    pub span: Option<Span>,
    pub associated_refs_by_name: HashMap<String, LocalImplementsAssociatedRef>,
    pub types: Vec<ImplementsType>,
    pub functions: Vec<ImplementsFunction>,
    pub constants: Vec<ImplementsConstant>,
    pub declared_in_module_ref: ModuleRef,
    pub trait_name: String,
    pub implements_type_ref_by_trait_type_ref: HashMap<LocalTraitTypeRef, LocalImplementsTypeRef>,
    pub implements_function_ref_by_trait_function_ref:
        HashMap<LocalTraitFunctionRef, LocalImplementsFunctionRef>,
    pub implements_constant_ref_by_trait_constant_ref:
        HashMap<LocalTraitConstantRef, LocalImplementsConstantRef>,
}

/// Represents an implements associated constant.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ImplementsConstant {
    pub name: String,
    pub implements_constant_ref: LocalImplementsConstantRef,
    pub span: Option<Span>,
    pub parent_implements_ref: ImplementsRef,
    pub constant: constant::Constant,
}

/// Represents an implements associated function.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ImplementsFunction {
    pub name: String,
    pub parent_implements_ref: ImplementsRef,
    pub span: Option<Span>,
    pub implements_function_ref: LocalImplementsFunctionRef,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
}

/// Represents an implements associated type.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ImplementsType {
    pub name: String,
    pub parent_implements_ref: ImplementsRef,
    pub span: Option<Span>,
    pub implements_type_ref: LocalImplementsTypeRef,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub alias: ty::Type,
}

/// Represents a trait symbol.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Trait {
    pub name: String,
    pub trait_ref: TraitRef,
    pub accessibility: Accessibility,
    pub parent_module_ref: ModuleRef,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub functions: Vec<TraitFunction>,
    pub types: Vec<TraitType>,
    pub constants: Vec<TraitConstant>,
    pub associated_refs_by_name: HashMap<String, LocalTraitAssociatedRef>,
    pub implements: Vec<Implements>,
    pub negative_implements: Vec<ImplementsSignature>,
    pub span: Option<Span>,
}

/// Represents a module declaration.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Module {
    pub name: String,
    pub module_ref: ModuleRef,
    pub accessibility: Accessibility,
    pub parent_module_ref: Option<ModuleRef>,
    pub module_member_refs_by_name: HashMap<String, ModuleMemberRef>,
    pub usings: HashMap<ModuleRef, Span>,
    pub span: Option<Span>,
}

/// Contains all the associated bounds.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct AssociatedBounds {
    pub associated_type_bounds: HashMap<ty::TraitAssociated, ty::Type>,
    pub associated_constant_bounds: HashMap<constant::TraitAssociated, constant::Constant>,
}

/// Represents generic arguments substituions on multiple [`GenericItem`] boundaries.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Substitution {
    pub type_substitutions: HashMap<TypeParameterRef, ty::Type>,
    pub constant_substitutions: HashMap<ConstantParameterRef, constant::Constant>,
    pub lifetime_substitutions: HashMap<LifetimeParameterRef, Lifetime>,
}

impl Substitution {
    /// Combines two [`Substitution`]s into one.
    ///
    /// If any duplicate substitutions are found, this function will return `None`.
    #[must_use]
    pub fn combine(lhs: &Self, rhs: &Self) -> Option<Self> {
        let mut new = lhs.clone();

        for (key, value) in &rhs.type_substitutions {
            let None = new.type_substitutions.insert(*key, value.clone()) else {
                return None;
            };
        }

        for (key, value) in &rhs.constant_substitutions {
            let None = new.constant_substitutions.insert(*key, value.clone()) else {
                return None;
            };
        }

        for (key, value) in &rhs.lifetime_substitutions {
            let None = new.lifetime_substitutions.insert(*key, *value) else {
                return None;
            };
        }

        Some(new)
    }

    /// Transforms the [`LocalSubstitution`] targetted to a particular [`GenericItem`] into a global
    /// [`Substitution`].
    #[must_use]
    pub fn from_local(
        local_substituion: &LocalSubstitution,
        generic_item_ref: GenericItemRef,
    ) -> Self {
        let mut type_substitution = HashMap::new();
        let mut constant_substitution = HashMap::new();
        let mut lifetime_substitution = HashMap::new();

        for (index, substitution) in local_substituion.types.iter().enumerate() {
            type_substitution.insert(
                TypeParameterRef {
                    generic_item_ref,
                    local_ref: LocalTypeParameterRef(index),
                },
                substitution.clone(),
            );
        }
        for (index, substitution) in local_substituion.constants.iter().enumerate() {
            constant_substitution.insert(
                ConstantParameterRef {
                    generic_item_ref,
                    local_ref: LocalConstantParameterRef(index),
                },
                substitution.clone(),
            );
        }
        for (index, substitution) in local_substituion.lifetimes.iter().enumerate() {
            lifetime_substitution.insert(
                LifetimeParameterRef {
                    generic_item_ref,
                    local_ref: LocalLifetimeParameterRef(index),
                },
                *substitution,
            );
        }

        Self {
            type_substitutions: type_substitution,
            constant_substitutions: constant_substitution,
            lifetime_substitutions: lifetime_substitution,
        }
    }
}
