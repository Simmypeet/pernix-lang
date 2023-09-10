use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::Hash,
};

use pernixc_source::Span;

use crate::{
    constant, pattern,
    ty::{self, Lifetime},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum Accessibility {
    #[default]
    Public,
    Private,
    Internal,
}

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

    pub fn get_index_by_name<Q: ?Sized + Eq + Hash>(&self, name: &Q) -> Option<usize>
    where
        String: std::borrow::Borrow<Q>,
    {
        self.map.get(name).copied()
    }

    #[must_use]
    pub fn get(&self, index: usize) -> Option<&T> { self.vec.get(index) }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> { self.vec.get_mut(index) }

    #[must_use]
    pub fn get_by_name<Q: ?Sized + Eq + Hash>(&self, name: &Q) -> Option<&T>
    where
        String: std::borrow::Borrow<Q>,
    {
        self.map.get(name).map(|index| &self.vec[*index])
    }

    pub fn get_mut_by_name<Q: ?Sized + Eq + Hash>(&mut self, name: &Q) -> Option<&mut T>
    where
        String: std::borrow::Borrow<Q>,
    {
        self.map.get(name).map(|index| &mut self.vec[*index])
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> { self.vec.iter() }

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssociatedItemRef {
    pub parent_index: usize,
    pub associated_item_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModuleMemberRef {
    Struct(usize),
    Enum(usize),
    Type(usize),
    Function(usize),
    Trait(usize),
    Module(usize),
    Constant(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GlobalItemRef {
    Module(usize),
    Struct(usize),
    Enum(usize),
    Variant(AssociatedItemRef),
    Type(usize),
    Constant(usize),
    Function(usize),
    Trait(usize),
    TraitFunction(AssociatedItemRef),
    TraitType(AssociatedItemRef),
    TraitConstant(AssociatedItemRef),
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
                parent_index: self.index,
                associated_item_index: x,
            })
        })
    }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for Variant {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::Variant(AssociatedItemRef {
            parent_index: self.parent_enum_index,
            associated_item_index: self.index,
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
        self.associated_ids_by_name
            .get(name)
            .copied()
            .map(|x| match x {
                TraitAssociatedItemRef::Function(index) => {
                    GlobalItemRef::TraitFunction(AssociatedItemRef {
                        parent_index: self.index,
                        associated_item_index: index,
                    })
                }
                TraitAssociatedItemRef::Type(index) => {
                    GlobalItemRef::TraitType(AssociatedItemRef {
                        parent_index: self.index,
                        associated_item_index: index,
                    })
                }
                TraitAssociatedItemRef::Constant(index) => {
                    GlobalItemRef::TraitConstant(AssociatedItemRef {
                        parent_index: self.index,
                        associated_item_index: index,
                    })
                }
            })
    }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl GlobalItem for TraitFunction {
    fn name(&self) -> &str { &self.name }

    fn global_item_ref(&self) -> GlobalItemRef {
        GlobalItemRef::TraitFunction(AssociatedItemRef {
            parent_index: self.parent_trait_index,
            associated_item_index: self.index,
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
        GlobalItemRef::TraitType(AssociatedItemRef {
            parent_index: self.parent_trait_index,
            associated_item_index: self.index,
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
        GlobalItemRef::TraitConstant(AssociatedItemRef {
            parent_index: self.parent_trait_index,
            associated_item_index: self.index,
        })
    }

    fn parent(&self) -> Option<GlobalItemRef> {
        Some(GlobalItemRef::Trait(self.parent_trait_index))
    }

    fn get_member(&self, _: &str) -> Option<GlobalItemRef> { None }

    fn span(&self) -> Option<Span> { None }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GenericItemRef {
    Enum(usize),
    Struct(usize),
    Type(usize),
    Function(usize),
    Trait(usize),
}

#[derive(Debug, Clone, Default)]
pub struct GenericParameters {
    pub types: VecNameMap<TypeParameter>,
    pub lifetimes: VecNameMap<LifetimeParameter>,
    pub constants: VecNameMap<ConstantParameter>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LifetimeBoundOperand {
    LifetimeParameter(GenericParameterRef),
    Type(ty::Type),
}

#[derive(Debug, Clone, Default)]
pub struct WhereClause {
    pub lifetime_bounds: HashMap<LifetimeBoundOperand, HashSet<Lifetime>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericParameterRef {
    pub generic_item_ref: GenericItemRef,
    pub index: usize,
}

#[derive(Debug, Clone)]
pub struct TypeParameter {
    pub name: String,
    pub index: usize,
    pub parent_generic_item_ref: GenericItemRef,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct LifetimeParameter {
    pub name: String,
    pub index: usize,
    pub parent_generic_item_ref: GenericItemRef,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct ConstantParameter {
    pub name: String,
    pub index: usize,
    pub ty: ty::Type,
    pub parent_generic_item_ref: GenericItemRef,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalSubstitution {
    pub type_substitutions: Vec<ty::Type>,
    pub constant_substitutions: Vec<constant::Constant>,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: String,
    pub index: usize,
    pub accessibility: Accessibility,
    pub parent_enum_index: usize,
    pub ty: Option<ty::Type>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: String,
    pub index: usize,
    pub accessibility: Accessibility,
    pub parent_module_index: usize,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub variants: VecNameMap<Variant>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field {
    pub name: String,
    pub index: usize,
    pub accessibility: Accessibility,
    pub parent_struct_index: usize,
    pub ty: ty::Type,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub index: usize,
    pub accessibility: Accessibility,
    pub parent_module_index: usize,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub fields: VecNameMap<Field>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub name: String,
    pub index: usize,
    pub accessibility: Accessibility,
    pub parent_module_index: usize,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub ty: ty::Type,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct Constant {
    pub name: String,
    pub index: usize,
    pub accessibility: Accessibility,
    pub parent_module_index: usize,
    pub ty: ty::Type,
    pub constant: constant::Constant,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter {
    pub index: usize,
    pub parent_function_index: usize,
    pub ty: ty::Type,
    pub pattern: pattern::Irrefutable,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub index: usize,
    pub accessibility: Accessibility,
    pub parent_module_index: usize,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub parameters: Vec<Parameter>,
    pub return_type: ty::Type,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct TraitType {
    pub name: String,
    pub index: usize,
    pub parent_trait_index: usize,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct TraitFunction {
    pub name: String,
    pub index: usize,
    pub parent_trait_index: usize,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub parameters: Vec<Parameter>,
    pub return_type: ty::Type,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitConstant {
    pub name: String,
    pub index: usize,
    pub parent_trait_index: usize,
    pub ty: ty::Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TraitAssociatedItemRef {
    Function(usize),
    Type(usize),
    Constant(usize),
}

#[derive(Debug, Clone)]
pub struct ImplementsSignature {
    pub generic_parameters: GenericParameters,
    pub trait_substitution: LocalSubstitution,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImplementsAssociatedRef {
    Type(usize),
    Function(usize),
    Constant(usize),
}

#[derive(Debug, Clone)]
pub struct Implements {
    pub index: usize,
    pub signature: ImplementsSignature,
    pub is_const_implements: bool,
    pub where_clause: WhereClause,
    pub parent_trait_id: usize,
    pub span: Option<Span>,
    pub associated_refs_by_name: HashMap<String, ImplementsAssociatedRef>,
    pub types: Vec<ImplementsType>,
    pub functions: Vec<ImplementsFunction>,
    pub constants: Vec<ImplementsConstant>,
    pub declared_in_module_id: usize,
    pub trait_name: String,
}

#[derive(Debug, Clone)]
pub struct ImplementsConstant {
    pub name: String,
    pub implements_ref: AssociatedItemRef,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct ImplementsFunction {
    pub name: String,
    pub implements_ref: AssociatedItemRef,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct ImplementsType {
    pub name: String,
    pub implements_ref: AssociatedItemRef,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub name: String,
    pub index: usize,
    pub accessibility: Accessibility,
    pub parent_module_index: usize,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub functions: Vec<TraitFunction>,
    pub types: Vec<TraitType>,
    pub constants: Vec<TraitConstant>,
    pub associated_ids_by_name: HashMap<String, TraitAssociatedItemRef>,
    pub implements: Vec<Implements>,
    pub negative_implements: Vec<ImplementsSignature>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub index: usize,
    pub accessibility: Accessibility,
    pub parent_module_index: Option<usize>,
    pub module_member_refs_by_name: HashMap<String, ModuleMemberRef>,
    pub usings: HashMap<usize, Span>,
    pub span: Option<Span>,
}
