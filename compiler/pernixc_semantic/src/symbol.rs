use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    sync::atomic::AtomicUsize,
};

use enum_as_inner::EnumAsInner;
use pernixc_source::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{self, target::Target};
use pernixc_system::{
    arena::{self, Arena, NamedMap},
    diagnostic::Handler,
};

use crate::{
    constant, error,
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

impl From<ID> for GlobalRef {
    fn from(value: ID) -> Self {
        match value {
            ID::Module(id) => Self::Module(id),
            ID::Struct(id) => Self::Struct(id),
            ID::Enum(id) => Self::Enum(id),
            ID::Function(id) => Self::Function(id),
            ID::Type(id) => Self::Type(id),
            ID::Constant(id) => Self::Constant(id),
            ID::Trait(id) => Self::Trait(id),
        }
    }
}

/// Represents a trait associated type declaration entry in the [`Trait`].
#[derive(Debug, Clone)]
pub struct TraitType {
    pub name: String,
    pub generic_parameters: GenericParameters,
    pub parent_trait_id: arena::ID<Trait>,
    pub alias: ty::Type,
    pub syntax_tree: Option<syntax_tree::item::TraitType>,
}

/// Represents a trait associated const declaration entry in the [`Trait`].
#[derive(Debug, Clone)]
pub struct TraitConstant {
    pub name: String,
    pub ty: ty::Type,
    pub parent_trait_id: arena::ID<Trait>,
    pub syntax_tree: Option<syntax_tree::item::TraitConstant>,
    pub constant: constant::Constant,
}

/// Represents a trait associated function declaration entry in the [`Trait`].
#[derive(Debug, Clone)]
pub struct TraitFunction {
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub name: String,
    pub parameters: Arena<Parameter>,
    pub parent_trait_id: arena::ID<Trait>,
    pub syntax_tree: Option<syntax_tree::item::TraitFunction>,
    pub return_type: ty::Type,
}

/// Represents a reference to the variant eclared in the enum.
pub type VariantRef = AssociatedItemRef<Enum, Variant>;

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
#[derive(Debug, Clone, Default)]
pub struct LifetimeParameter {
    pub name: String,
    pub span: Option<Span>,
}

/// Represents an identifier/reference to a lifetime parameter declared in the item.
pub type LifetimeParameterRef = GenericParameterRef<LifetimeParameter>;

/// Represents a declaration of a generic type parameter.
#[derive(Debug, Clone, Default)]
pub struct TypeParameter {
    pub name: String,
    pub span: Option<Span>,
}

/// Represents an identifier/reference to a type parameter declared in the item.
pub type TypeParameterRef = GenericParameterRef<TypeParameter>;

/// Represents a declaration of a generic constant parameter.
#[derive(Debug, Clone, Default)]
pub struct ConstantParameter {
    pub name: String,
    pub ty: ty::Type,
    pub span: Option<Span>,
}

/// Represents an identifier/reference to a constant parameter declared in the item.
pub type ConstantParameterRef = GenericParameterRef<ConstantParameter>;

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
    pub parent_module_id: arena::ID<Module>,
}

/// Represents a variant declaration entry in the [`Enum`].
#[derive(Debug, Clone)]
pub struct Variant {
    pub name: String,
    pub parent_enum_id: arena::ID<Enum>,
    pub association_type: Option<ty::Type>,
    pub syntax_tree: Option<syntax_tree::item::Variant>,
}

/// Represents an enum declaration entry in the symbol table.
#[derive(Debug, Clone)]
pub struct Enum {
    pub accessibility: Accessibility,
    pub name: String,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub variants: NamedMap<Variant>,
    pub syntax_tree: Option<syntax_tree::item::EnumSignature>,
    pub parent_module_id: arena::ID<Module>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImplementsAssociatedID {
    Type(arena::ID<ImplementsType>),
    Function(arena::ID<ImplementsFunction>),
    Constant(arena::ID<ImplementsConstant>),
}

#[derive(Debug, Clone)]
pub struct Implements {
    pub signature: ImplementsSignature,
    pub is_const_implements: bool,
    pub where_clause: WhereClause,
    pub parent_trait_id: arena::ID<Trait>,
    pub syntax_tree: Option<syntax_tree::item::Implements>,
    pub associated_ids_by_name: HashMap<String, ImplementsAssociatedID>,
    pub types: Arena<ImplementsType>,
    pub functions: Arena<ImplementsFunction>,
    pub constants: Arena<ImplementsConstant>,
    pub declared_in_module_id: arena::ID<Module>,
    pub trait_name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementsRef {
    pub trait_id: arena::ID<Trait>,
    pub implements_id: arena::ID<Implements>,
}

#[derive(Debug)]
pub struct AssociatedImplementsItemRef<Item> {
    pub reference: ImplementsRef,
    pub associated_item: arena::ID<Item>,
}

impl<Item> Clone for AssociatedImplementsItemRef<Item> {
    fn clone(&self) -> Self {
        Self {
            reference: self.reference,
            associated_item: self.associated_item,
        }
    }
}

impl<Item> Copy for AssociatedImplementsItemRef<Item> {}

impl<Item> PartialEq for AssociatedImplementsItemRef<Item> {
    fn eq(&self, other: &Self) -> bool {
        self.reference == other.reference && self.associated_item == other.associated_item
    }
}

impl<Item> Eq for AssociatedImplementsItemRef<Item> {}

impl<Item> PartialOrd for AssociatedImplementsItemRef<Item> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.reference
            .partial_cmp(&other.reference)
            .and_then(|ordering| match ordering {
                std::cmp::Ordering::Equal => {
                    self.associated_item.partial_cmp(&other.associated_item)
                }
                _ => Some(ordering),
            })
    }
}

impl<Item> Ord for AssociatedImplementsItemRef<Item> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.reference
            .cmp(&other.reference)
            .then_with(|| self.associated_item.cmp(&other.associated_item))
    }
}

impl<Item> Hash for AssociatedImplementsItemRef<Item> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.reference.hash(state);
        self.associated_item.hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct ImplementsConstant {
    pub name: String,
    pub parent_reference: ImplementsRef,
    pub syntax_tree: Option<syntax_tree::item::ImplementsConstant>,
}

#[derive(Debug, Clone)]
pub struct ImplementsFunction {
    pub name: String,
    pub parent_reference: ImplementsRef,
    pub syntax_tree: Option<syntax_tree::item::ImplementsFunction>,
}

#[derive(Debug, Clone)]
pub struct ImplementsType {
    pub name: String,
    pub parent_reference: ImplementsRef,
    pub syntax_tree: Option<syntax_tree::item::ImplementsType>,
}

pub type ImplementsConstantRef = AssociatedImplementsItemRef<ImplementsConstant>;
pub type ImplementsFunctionRef = AssociatedImplementsItemRef<ImplementsFunction>;
pub type ImplementsTypeRef = AssociatedImplementsItemRef<ImplementsType>;

/// Represents a trait declaration entry in the symbol table.
#[derive(Debug, Clone)]
pub struct Trait {
    pub accessibility: Accessibility,
    pub name: String,
    pub generic_parameters: GenericParameters,
    pub where_clause: WhereClause,
    pub associated_ids_by_name: HashMap<String, TraitAssociatedID>,
    pub constants: Arena<TraitConstant>,
    pub types: Arena<TraitType>,
    pub functions: Arena<TraitFunction>,
    pub implements: Arena<Implements>,
    pub negative_implements: Arena<ImplementsSignature>,
    pub syntax_tree: Option<syntax_tree::item::TraitSignature>,
    pub parent_module_id: arena::ID<Module>,
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
    pub constants: Vec<constant::Constant>,
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
    pub trait_associated_constant_bounds: HashMap<constant::TraitAssociated, constant::Constant>,

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
    pub constants: Vec<constant::Constant>,
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

/// Represents a type declaration entry in the symbol table.
#[derive(Debug, Clone)]
pub struct Type {
    pub accessibility: Accessibility,
    pub name: String,
    pub generic_parameters: GenericParameters,
    pub alias: ty::Type,
    pub syntax_tree: Option<syntax_tree::item::Type>,
    pub parent_module_id: arena::ID<Module>,
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
    pub parameters: Arena<Parameter>,
    pub return_type: ty::Type,
    pub syntax_tree: Option<syntax_tree::item::Function>,
    pub parent_module_id: arena::ID<Module>,
}

/// Represents a constant declaration entry in the symbol table.
#[derive(Debug, Clone)]
pub struct Constant {
    pub name: String,
    pub accessibility: Accessibility,
    pub ty: ty::Type,
    pub constant: constant::Constant,
    pub syntax_tree: Option<syntax_tree::item::Constant>,
    pub parent_module_id: arena::ID<Module>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ID {
    Module(arena::ID<Module>),
    Struct(arena::ID<Struct>),
    Enum(arena::ID<Enum>),
    Trait(arena::ID<Trait>),
    Type(arena::ID<Type>),
    Function(arena::ID<Function>),
    Constant(arena::ID<Constant>),
}

/// Represents a reference to the item in the [`Table`] that can have generic parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GenericItemRef {
    Struct(arena::ID<Struct>),
    Enum(arena::ID<Enum>),
    Trait(arena::ID<Trait>),
    Type(arena::ID<Type>),
    Function(arena::ID<Function>),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum State {
    Drafting,
    Constructing,
}

#[derive(Debug, Clone, Copy)]
enum DraftingSymbolRef {
    Struct(arena::ID<Struct>),
    Enum(arena::ID<Enum>),
    Trait(arena::ID<Trait>),
    Type(arena::ID<Type>),
    Function(arena::ID<Function>),
    Constant(arena::ID<Constant>),
}

impl PartialEq for DraftingSymbolRef {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Struct(id), Self::Struct(other_id)) => id == other_id,
            (Self::Enum(id), Self::Enum(other_id)) => id == other_id,
            (Self::Trait(id), Self::Trait(other_id)) => id == other_id,
            (Self::Type(id), Self::Type(other_id)) => id == other_id,
            (Self::Function(id), Self::Function(other_id)) => id == other_id,
            (Self::Constant(id), Self::Constant(other_id)) => id == other_id,
            _ => false,
        }
    }
}

impl Eq for DraftingSymbolRef {}

impl std::hash::Hash for DraftingSymbolRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Struct(id) => id.hash(state),
            Self::Enum(id) => id.hash(state),
            Self::Trait(id) => id.hash(state),
            Self::Type(id) => id.hash(state),
            Self::Function(id) => id.hash(state),
            Self::Constant(id) => id.hash(state),
        }
    }
}

/// Represents a reference to the symbol in that can be referenced by using qualified name path.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From, EnumAsInner,
)]
pub enum GlobalRef {
    Module(arena::ID<Module>),
    Struct(arena::ID<Struct>),
    Enum(arena::ID<Enum>),
    Variant(VariantRef),
    Function(arena::ID<Function>),
    Type(arena::ID<Type>),
    Constant(arena::ID<Constant>),
    Trait(arena::ID<Trait>),
    TraitFunction(TraitFunctionRef),
    TraitConstant(TraitConstantRef),
    TraitType(TraitTypeRef),
    Implements(ImplementsRef),
    ImplementsFunction(ImplementsFunctionRef),
    ImplementsConstant(ImplementsConstantRef),
    ImplementsType(ImplementsTypeRef),
}

/// Represents a symbol that can be referenced by using qualified name path.
pub trait Global {
    /// Gets the reference to the symbol.
    fn global_ref(&self) -> GlobalRef;

    /// Gets the reference to the child symbol with the given name.
    fn get_child_symbol(&self, name: &str) -> Option<GlobalRef>;

    /// The non-qualified name of the symbol.
    fn name(&self) -> &str;

    /// The span of the symbol in the source code.
    fn span(&self) -> Option<Span>;

    /// Gets the reference to the parent symbol.
    fn parent(&self) -> Option<GlobalRef>;
}

impl Global for arena::Symbol<Module> {
    fn global_ref(&self) -> GlobalRef { GlobalRef::Module(self.id()) }

    fn get_child_symbol(&self, name: &str) -> Option<GlobalRef> {
        self.children_ids_by_name.get(name).map(|id| match id {
            ID::Module(id) => GlobalRef::Module(*id),
            ID::Struct(id) => GlobalRef::Struct(*id),
            ID::Enum(id) => GlobalRef::Enum(*id),
            ID::Type(id) => GlobalRef::Type(*id),
            ID::Function(id) => GlobalRef::Function(*id),
            ID::Constant(id) => GlobalRef::Constant(*id),
            ID::Trait(id) => GlobalRef::Trait(*id),
        })
    }

    fn parent(&self) -> Option<GlobalRef> { self.parent_module_id.map(GlobalRef::Module) }

    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<Span> {
        self.syntax_tree
            .as_ref()
            .map(|x| x.module_keyword().span.join(&x.identifier().span).unwrap())
    }
}

impl Global for arena::Symbol<Struct> {
    fn global_ref(&self) -> GlobalRef { GlobalRef::Struct(self.id()) }

    fn get_child_symbol(&self, _: &str) -> Option<GlobalRef> { None }

    fn parent(&self) -> Option<GlobalRef> { Some(GlobalRef::Module(self.parent_module_id)) }

    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<Span> {
        self.syntax_tree.as_ref().map(|x| {
            x.signature()
                .struct_keyword()
                .span
                .join(&x.signature().identifier().span)
                .unwrap()
        })
    }
}

impl Global for arena::Symbol<Enum> {
    fn global_ref(&self) -> GlobalRef { GlobalRef::Enum(self.id()) }

    fn get_child_symbol(&self, name: &str) -> Option<GlobalRef> {
        self.variants.get_by_name(name).map(|id| {
            GlobalRef::Variant(VariantRef {
                parent: self.id(),
                associated_item: id.id(),
            })
        })
    }

    fn parent(&self) -> Option<GlobalRef> { Some(GlobalRef::Module(self.parent_module_id)) }

    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<Span> {
        self.syntax_tree
            .as_ref()
            .map(|x| x.enum_keyword().span.join(&x.identifier().span).unwrap())
    }
}

impl Global for arena::Symbol<Variant> {
    fn global_ref(&self) -> GlobalRef {
        GlobalRef::Variant(VariantRef {
            parent: self.parent_enum_id,
            associated_item: self.id(),
        })
    }

    fn get_child_symbol(&self, _: &str) -> Option<GlobalRef> { None }

    fn parent(&self) -> Option<GlobalRef> { Some(GlobalRef::Enum(self.parent_enum_id)) }

    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<Span> { None }
}

impl Global for arena::Symbol<Function> {
    fn global_ref(&self) -> GlobalRef { GlobalRef::Function(self.id()) }

    fn get_child_symbol(&self, _: &str) -> Option<GlobalRef> { None }

    fn parent(&self) -> Option<GlobalRef> { Some(GlobalRef::Module(self.parent_module_id)) }

    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<Span> {
        self.syntax_tree.as_ref().map(|x| {
            x.signature()
                .function_keyword()
                .span
                .join(&x.signature().identifier().span)
                .unwrap()
        })
    }
}

impl Global for arena::Symbol<Type> {
    fn global_ref(&self) -> GlobalRef { GlobalRef::Type(self.id()) }

    fn get_child_symbol(&self, _: &str) -> Option<GlobalRef> { None }

    fn parent(&self) -> Option<GlobalRef> { Some(GlobalRef::Module(self.parent_module_id)) }

    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<Span> {
        self.syntax_tree.as_ref().map(|x| {
            x.signature()
                .type_keyword()
                .span
                .join(&x.signature().identifier().span)
                .unwrap()
        })
    }
}

impl Global for arena::Symbol<Constant> {
    fn global_ref(&self) -> GlobalRef { GlobalRef::Constant(self.id()) }

    fn get_child_symbol(&self, _: &str) -> Option<GlobalRef> { None }

    fn parent(&self) -> Option<GlobalRef> { Some(GlobalRef::Module(self.parent_module_id)) }

    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<Span> {
        self.syntax_tree.as_ref().map(|x| {
            x.signature()
                .const_keyword()
                .span
                .join(&x.signature().identifier().span)
                .unwrap()
        })
    }
}

impl Global for arena::Symbol<Trait> {
    fn global_ref(&self) -> GlobalRef { GlobalRef::Trait(self.id()) }

    fn get_child_symbol(&self, name: &str) -> Option<GlobalRef> {
        self.associated_ids_by_name.get(name).map(|id| match id {
            TraitAssociatedID::Type(id) => GlobalRef::TraitType(TraitTypeRef {
                parent: self.id(),
                associated_item: *id,
            }),
            TraitAssociatedID::Constant(id) => GlobalRef::TraitConstant(TraitConstantRef {
                parent: self.id(),
                associated_item: *id,
            }),
            TraitAssociatedID::Function(id) => GlobalRef::TraitFunction(TraitFunctionRef {
                parent: self.id(),
                associated_item: *id,
            }),
        })
    }

    fn parent(&self) -> Option<GlobalRef> { Some(GlobalRef::Module(self.parent_module_id)) }

    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<Span> {
        self.syntax_tree
            .as_ref()
            .map(|x| x.trait_keyword().span.join(&x.identifier().span).unwrap())
    }
}

impl Global for arena::Symbol<TraitFunction> {
    fn global_ref(&self) -> GlobalRef {
        GlobalRef::TraitFunction(TraitFunctionRef {
            parent: self.parent_trait_id,
            associated_item: self.id(),
        })
    }

    fn get_child_symbol(&self, _: &str) -> Option<GlobalRef> { None }

    fn parent(&self) -> Option<GlobalRef> { Some(GlobalRef::Trait(self.parent_trait_id)) }

    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<Span> {
        self.syntax_tree.as_ref().map(|x| {
            x.signature()
                .function_keyword()
                .span
                .join(&x.signature().identifier().span)
                .unwrap()
        })
    }
}

impl Global for arena::Symbol<TraitConstant> {
    fn global_ref(&self) -> GlobalRef {
        GlobalRef::TraitConstant(TraitConstantRef {
            parent: self.parent_trait_id,
            associated_item: self.id(),
        })
    }

    fn get_child_symbol(&self, _: &str) -> Option<GlobalRef> { None }

    fn parent(&self) -> Option<GlobalRef> { Some(GlobalRef::Trait(self.parent_trait_id)) }

    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<Span> {
        self.syntax_tree.as_ref().map(|x| {
            x.signature()
                .const_keyword()
                .span
                .join(&x.signature().identifier().span)
                .unwrap()
        })
    }
}

impl Global for arena::Symbol<TraitType> {
    fn global_ref(&self) -> GlobalRef {
        GlobalRef::TraitType(TraitTypeRef {
            parent: self.parent_trait_id,
            associated_item: self.id(),
        })
    }

    fn get_child_symbol(&self, _: &str) -> Option<GlobalRef> { None }

    fn parent(&self) -> Option<GlobalRef> { Some(GlobalRef::Trait(self.parent_trait_id)) }

    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<Span> {
        self.syntax_tree.as_ref().map(|x| {
            x.signature()
                .type_keyword()
                .span
                .join(&x.signature().identifier().span)
                .unwrap()
        })
    }
}

impl Global for arena::Symbol<Implements> {
    fn global_ref(&self) -> GlobalRef {
        GlobalRef::Implements(ImplementsRef {
            trait_id: self.parent_trait_id,
            implements_id: self.id(),
        })
    }

    fn get_child_symbol(&self, name: &str) -> Option<GlobalRef> {
        let implements_ref = ImplementsRef {
            trait_id: self.parent_trait_id,
            implements_id: self.id(),
        };

        self.associated_ids_by_name.get(name).map(|id| match id {
            ImplementsAssociatedID::Type(id) => GlobalRef::ImplementsType(ImplementsTypeRef {
                reference: implements_ref,
                associated_item: *id,
            }),
            ImplementsAssociatedID::Function(id) => {
                GlobalRef::ImplementsFunction(ImplementsFunctionRef {
                    reference: implements_ref,
                    associated_item: *id,
                })
            }
            ImplementsAssociatedID::Constant(id) => {
                GlobalRef::ImplementsConstant(ImplementsConstantRef {
                    reference: implements_ref,
                    associated_item: *id,
                })
            }
        })
    }

    fn parent(&self) -> Option<GlobalRef> { Some(GlobalRef::Module(self.declared_in_module_id)) }

    fn name(&self) -> &str { &self.trait_name }

    fn span(&self) -> Option<Span> {
        self.syntax_tree.as_ref().map(|x| {
            x.signature()
                .implements_keyword()
                .span
                .join(&x.signature().qualified_identifier().span())
                .unwrap()
        })
    }
}

impl Global for arena::Symbol<ImplementsFunction> {
    fn global_ref(&self) -> GlobalRef {
        GlobalRef::ImplementsFunction(ImplementsFunctionRef {
            reference: self.parent_reference,
            associated_item: self.id(),
        })
    }

    fn get_child_symbol(&self, _: &str) -> Option<GlobalRef> { None }

    fn parent(&self) -> Option<GlobalRef> { Some(GlobalRef::Implements(self.parent_reference)) }

    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<Span> { todo!() }
}

impl Global for arena::Symbol<ImplementsConstant> {
    fn global_ref(&self) -> GlobalRef {
        GlobalRef::ImplementsConstant(ImplementsConstantRef {
            reference: self.parent_reference,
            associated_item: self.id(),
        })
    }

    fn get_child_symbol(&self, _: &str) -> Option<GlobalRef> { None }

    fn parent(&self) -> Option<GlobalRef> { Some(GlobalRef::Implements(self.parent_reference)) }

    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<Span> {
        self.syntax_tree.as_ref().map(|x| {
            x.signature()
                .const_keyword()
                .span
                .join(&x.signature().identifier().span)
                .unwrap()
        })
    }
}

impl Global for arena::Symbol<ImplementsType> {
    fn global_ref(&self) -> GlobalRef {
        GlobalRef::ImplementsType(ImplementsTypeRef {
            reference: self.parent_reference,
            associated_item: self.id(),
        })
    }

    fn get_child_symbol(&self, _: &str) -> Option<GlobalRef> { None }

    fn parent(&self) -> Option<GlobalRef> { Some(GlobalRef::Implements(self.parent_reference)) }

    fn name(&self) -> &str { &self.name }

    fn span(&self) -> Option<Span> {
        self.syntax_tree.as_ref().map(|x| {
            x.signature()
                .type_keyword()
                .span
                .join(&x.signature().identifier().span)
                .unwrap()
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
pub enum OperationError {
    #[error("the reference input for the operation is invalid")]
    InvalidReference,
    #[error("fatal semantics error occurred, the analysis aborted")]
    SemanticError,
}

/// Represents a symbol table, containing all the symbol declarations and program information.
#[derive(Debug, Clone)]
pub struct Table {
    modules: Arena<Module>,
    structs: Arena<Struct>,
    enums: Arena<Enum>,
    traits: Arena<Trait>,
    types: Arena<Type>,
    functions: Arena<Function>,
    constants: Arena<Constant>,

    target_root_module_ids_by_name: HashMap<String, arena::ID<Module>>,

    states_by_drafting_symbol_refs: HashMap<DraftingSymbolRef, State>,
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
        handler: &impl Handler<error::Error>,
    ) -> Result<Self, BuildError> {
        // default table
        let mut table = Self {
            modules: Arena::new(),
            structs: Arena::new(),
            enums: Arena::new(),
            traits: Arena::new(),
            types: Arena::new(),
            functions: Arena::new(),
            constants: Arena::new(),
            target_root_module_ids_by_name: HashMap::new(),
            states_by_drafting_symbol_refs: HashMap::new(),
        };

        table.create_core_module();

        let mut implements_syntax_tree_vecs_by_module_id = HashMap::new();

        for target in targets {
            table.draft_target(
                target,
                &mut implements_syntax_tree_vecs_by_module_id,
                handler,
            )?;
        }

        Ok(table)
    }

    /// Gets the [`Global`] symbol stored in the table via the given [`GlobalRef`].
    #[must_use]
    pub fn get_global(&self, global_ref: GlobalRef) -> Option<&dyn Global> {
        match global_ref {
            GlobalRef::Module(id) => self.modules.get(id).map(|x| x as _),
            GlobalRef::Struct(id) => self.structs.get(id).map(|x| x as _),
            GlobalRef::Enum(id) => self.enums.get(id).map(|x| x as _),
            GlobalRef::Variant(id) => self
                .enums
                .get(id.parent)
                .and_then(|x| x.variants.get(id.associated_item))
                .map(|x| x as _),
            GlobalRef::Function(id) => self.functions.get(id).map(|x| x as _),
            GlobalRef::Type(id) => self.types.get(id).map(|x| x as _),
            GlobalRef::Constant(id) => self.constants.get(id).map(|x| x as _),
            GlobalRef::Trait(id) => self.traits.get(id).map(|x| x as _),
            GlobalRef::TraitFunction(id) => self
                .traits
                .get(id.parent)
                .and_then(|x| x.functions.get(id.associated_item))
                .map(|x| x as _),
            GlobalRef::TraitConstant(id) => self
                .traits
                .get(id.parent)
                .and_then(|x| x.constants.get(id.associated_item))
                .map(|x| x as _),
            GlobalRef::TraitType(id) => self
                .traits
                .get(id.parent)
                .and_then(|x| x.types.get(id.associated_item))
                .map(|x| x as _),
            GlobalRef::Implements(id) => self
                .traits
                .get(id.trait_id)
                .and_then(|x| x.implements.get(id.implements_id))
                .map(|x| x as _),
            GlobalRef::ImplementsFunction(id) => self
                .traits
                .get(id.reference.trait_id)
                .and_then(|x| x.implements.get(id.reference.implements_id))
                .and_then(|x| x.functions.get(id.associated_item))
                .map(|x| x as _),
            GlobalRef::ImplementsConstant(id) => self
                .traits
                .get(id.reference.trait_id)
                .and_then(|x| x.implements.get(id.reference.implements_id))
                .and_then(|x| x.constants.get(id.associated_item))
                .map(|x| x as _),
            GlobalRef::ImplementsType(id) => self
                .traits
                .get(id.reference.trait_id)
                .and_then(|x| x.implements.get(id.reference.implements_id))
                .and_then(|x| x.types.get(id.associated_item))
                .map(|x| x as _),
        }
    }

    /// Gets the closest [`Module`] that contains the given [`GlobalRef`] (including the module
    /// itself).
    ///
    /// # Returns
    /// - `None` if the given [`GlobalRef`] is invalid.
    /// - `Some(id)` if the given [`GlobalRef`] is valid and the closest [`Module`] is found.
    #[allow(clippy::too_many_lines)]
    #[must_use]
    pub fn get_closet_module_id(&self, global_ref: GlobalRef) -> Option<arena::ID<Module>> {
        match global_ref {
            GlobalRef::Module(id) => {
                if self.modules.get(id).is_none() {
                    None
                } else {
                    Some(id)
                }
            }
            GlobalRef::Struct(id) => self.structs.get(id).map(|x| x.parent_module_id),
            GlobalRef::Enum(id) => self.enums.get(id).map(|x| x.parent_module_id),
            GlobalRef::Variant(id) => {
                // check if the ref is valid
                if self
                    .enums
                    .get(id.parent)
                    .and_then(|x| x.variants.get(id.associated_item))
                    .is_none()
                {
                    None
                } else {
                    self.enums.get(id.parent).map(|x| x.parent_module_id)
                }
            }
            GlobalRef::Function(id) => self.functions.get(id).map(|x| x.parent_module_id),
            GlobalRef::Type(id) => self.types.get(id).map(|x| x.parent_module_id),
            GlobalRef::Constant(id) => self.constants.get(id).map(|x| x.parent_module_id),
            GlobalRef::Trait(id) => self.traits.get(id).map(|x| x.parent_module_id),
            GlobalRef::TraitFunction(id) => {
                if self
                    .traits
                    .get(id.parent)
                    .and_then(|x| x.functions.get(id.associated_item))
                    .is_none()
                {
                    None
                } else {
                    self.traits.get(id.parent).map(|x| x.parent_module_id)
                }
            }
            GlobalRef::TraitConstant(id) => {
                if self
                    .traits
                    .get(id.parent)
                    .and_then(|x| x.constants.get(id.associated_item))
                    .is_none()
                {
                    None
                } else {
                    self.traits.get(id.parent).map(|x| x.parent_module_id)
                }
            }
            GlobalRef::TraitType(id) => {
                if self
                    .traits
                    .get(id.parent)
                    .and_then(|x| x.types.get(id.associated_item))
                    .is_none()
                {
                    None
                } else {
                    self.traits.get(id.parent).map(|x| x.parent_module_id)
                }
            }
            GlobalRef::Implements(id) => {
                if self
                    .traits
                    .get(id.trait_id)
                    .and_then(|x| x.implements.get(id.implements_id))
                    .is_none()
                {
                    None
                } else {
                    self.traits
                        .get(id.trait_id)
                        .and_then(|x| x.implements.get(id.implements_id))
                        .map(|x| x.declared_in_module_id)
                }
            }
            GlobalRef::ImplementsFunction(id) => {
                if self
                    .traits
                    .get(id.reference.trait_id)
                    .and_then(|x| x.implements.get(id.reference.implements_id))
                    .and_then(|x| x.functions.get(id.associated_item))
                    .is_none()
                {
                    None
                } else {
                    self.traits
                        .get(id.reference.trait_id)
                        .and_then(|x| x.implements.get(id.reference.implements_id))
                        .map(|x| x.declared_in_module_id)
                }
            }
            GlobalRef::ImplementsConstant(id) => {
                if self
                    .traits
                    .get(id.reference.trait_id)
                    .and_then(|x| x.implements.get(id.reference.implements_id))
                    .and_then(|x| x.constants.get(id.associated_item))
                    .is_none()
                {
                    None
                } else {
                    self.traits
                        .get(id.reference.trait_id)
                        .and_then(|x| x.implements.get(id.reference.implements_id))
                        .map(|x| x.declared_in_module_id)
                }
            }
            GlobalRef::ImplementsType(id) => {
                if self
                    .traits
                    .get(id.reference.trait_id)
                    .and_then(|x| x.implements.get(id.reference.implements_id))
                    .and_then(|x| x.types.get(id.associated_item))
                    .is_none()
                {
                    None
                } else {
                    self.traits
                        .get(id.reference.trait_id)
                        .and_then(|x| x.implements.get(id.reference.implements_id))
                        .map(|x| x.declared_in_module_id)
                }
            }
        }
    }

    /// Gets the [`Accessibility`] of the given [`GlobalRef`].
    ///
    /// # Returns
    /// - `None` if the given [`GlobalRef`] is invalid.
    /// - `Some(accessibility)` if the given [`GlobalRef`] is valid.
    #[allow(clippy::too_many_lines)]
    #[must_use]
    pub fn get_accessibility(&self, global_ref: GlobalRef) -> Option<Accessibility> {
        match global_ref {
            GlobalRef::Module(id) => self.modules.get(id).map(|x| x.accessibility),
            GlobalRef::Struct(id) => self.structs.get(id).map(|x| x.accessibility),
            GlobalRef::Enum(id) => self.enums.get(id).map(|x| x.accessibility),
            GlobalRef::Variant(id) => {
                // check if the ref is valid
                if self
                    .enums
                    .get(id.parent)
                    .and_then(|x| x.variants.get(id.associated_item))
                    .is_none()
                {
                    None
                } else {
                    self.enums.get(id.parent).map(|x| x.accessibility)
                }
            }
            GlobalRef::Function(id) => self.functions.get(id).map(|x| x.accessibility),
            GlobalRef::Type(id) => self.types.get(id).map(|x| x.accessibility),
            GlobalRef::Constant(id) => self.constants.get(id).map(|x| x.accessibility),
            GlobalRef::Trait(id) => self.traits.get(id).map(|x| x.accessibility),
            GlobalRef::TraitFunction(id) => {
                if self
                    .traits
                    .get(id.parent)
                    .and_then(|x| x.functions.get(id.associated_item))
                    .is_none()
                {
                    None
                } else {
                    self.traits.get(id.parent).map(|x| x.accessibility)
                }
            }
            GlobalRef::TraitConstant(id) => {
                if self
                    .traits
                    .get(id.parent)
                    .and_then(|x| x.constants.get(id.associated_item))
                    .is_none()
                {
                    None
                } else {
                    self.traits.get(id.parent).map(|x| x.accessibility)
                }
            }
            GlobalRef::TraitType(id) => {
                if self
                    .traits
                    .get(id.parent)
                    .and_then(|x| x.types.get(id.associated_item))
                    .is_none()
                {
                    None
                } else {
                    self.traits.get(id.parent).map(|x| x.accessibility)
                }
            }
            GlobalRef::Implements(id) => {
                if self
                    .traits
                    .get(id.trait_id)
                    .and_then(|x| x.implements.get(id.implements_id))
                    .is_none()
                {
                    None
                } else {
                    self.traits.get(id.trait_id).map(|x| x.accessibility)
                }
            }
            GlobalRef::ImplementsFunction(id) => {
                if self
                    .traits
                    .get(id.reference.trait_id)
                    .and_then(|x| x.implements.get(id.reference.implements_id))
                    .and_then(|x| x.functions.get(id.associated_item))
                    .is_none()
                {
                    None
                } else {
                    self.traits
                        .get(id.reference.trait_id)
                        .map(|x| x.accessibility)
                }
            }
            GlobalRef::ImplementsConstant(id) => {
                if self
                    .traits
                    .get(id.reference.trait_id)
                    .and_then(|x| x.implements.get(id.reference.implements_id))
                    .and_then(|x| x.constants.get(id.associated_item))
                    .is_none()
                {
                    None
                } else {
                    self.traits
                        .get(id.reference.trait_id)
                        .map(|x| x.accessibility)
                }
            }
            GlobalRef::ImplementsType(id) => {
                if self
                    .traits
                    .get(id.reference.trait_id)
                    .and_then(|x| x.implements.get(id.reference.implements_id))
                    .and_then(|x| x.types.get(id.associated_item))
                    .is_none()
                {
                    None
                } else {
                    self.traits
                        .get(id.reference.trait_id)
                        .map(|x| x.accessibility)
                }
            }
        }
    }

    /// Checks if the given [`GlobalRef`] is accessible from the given [`GlobalRef`].
    ///
    /// # Returns
    /// - `None` if the given [`GlobalRef`]s are invalid.
    #[must_use]
    pub fn symbol_accessible(&self, referred: GlobalRef, referring: GlobalRef) -> Option<bool> {
        match self.get_accessibility(referred)? {
            Accessibility::Public => {
                // PEDANTIC: check if the referring is valid
                self.get_global(referring)?;

                Some(true)
            }
            Accessibility::Private => {
                let referred_module_id = self.get_closet_module_id(referred)?;
                let referring_module_id = self.get_closet_module_id(referring)?;

                // if same module, it is accessible
                if referred_module_id == referring_module_id {
                    return Some(true);
                }

                let mut current_referrer_parent_id = referring_module_id.into();

                while let Some(parent_id) = self
                    .get_global(current_referrer_parent_id)
                    .unwrap()
                    .parent()
                {
                    match parent_id {
                        GlobalRef::Module(module_id) if module_id == referred_module_id => {
                            return Some(true);
                        }
                        _ => {
                            current_referrer_parent_id = parent_id;
                        }
                    }
                }

                Some(false)
            }
            Accessibility::Internal => Some(
                self.get_target_root_module_id(referred)?
                    == self.get_target_root_module_id(referring)?,
            ),
        }
    }

    /// Gets the target's root [`Module`] ID containing the given [`GlobalRef`].
    ///
    /// # Returns
    /// - `None` if the given [`GlobalRef`] is invalid.
    /// - `Some(id)` if the given [`GlobalRef`] is valid.
    #[must_use]
    pub fn get_target_root_module_id(&self, mut id: GlobalRef) -> Option<arena::ID<Module>> {
        while let Some(parent_id) = self.get_global(id)?.parent() {
            id = parent_id;
        }

        Some(
            id.into_module()
                .expect("It should be a module at the root."),
        )
    }

    /// Gets the fully qualified name of the given [`GlobalRef`].
    ///
    /// The name does not include the generic parameters.
    ///
    /// # Returns
    ///
    /// - `None` if the given [`GlobalRef`] is invalid.
    /// - `Some(name)` if the given [`GlobalRef`] is valid.
    #[must_use]
    pub fn get_qualified_name(&self, mut global_ref: GlobalRef) -> Option<String> {
        let mut current_name = self.get_global(global_ref)?.name().to_owned();

        while let Some(parent_symbol_id) = self.get_global(global_ref)?.parent() {
            let parent_global_symbol_id = parent_symbol_id;

            current_name.insert_str(0, "::");
            current_name.insert_str(0, self.get_global(parent_global_symbol_id)?.name());

            global_ref = parent_global_symbol_id;
        }

        Some(current_name)
    }
}

// TODO: Create a module heirarchy
// TODO: Draft the symbols
// TODO: Finalize the symbols
