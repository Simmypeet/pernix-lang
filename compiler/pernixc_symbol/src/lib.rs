//! Contains definitions related to the symbol resolution pass of the compiler.

#![deny(
    missing_docs,
    missing_debug_implementations,
    missing_copy_implementations,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    convert::Into,
    fmt::Debug,
    hash::Hash,
    sync::Arc,
};

use derive_more::{Deref, DerefMut, Display, From};
use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::Identifier;
use pernixc_syntax::syntax_tree::{
    self,
    item::{
        EnumSignature as EnumSignatureSyntaxTree, ReturnType, StructField as StructFieldSyntaxTree,
        StructSignature as StructSignatureSyntaxTree, Type as TypeSyntaxTree,
    },
    AccessModifier,
};
use pernixc_system::arena;

#[cfg(test)]
mod input;

pub mod error;
pub mod table;
pub mod ty;

/// The accessibility of the symbol. Determines where the symbol can be accessed from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, Display)]
pub enum Accessibility {
    /// The symbol can only be accessed from the same module it is defined in.
    #[display(fmt = "private")]
    Private,

    /// The symbol can only be accessed from the same target it is defined in.
    #[display(fmt = "internal")]
    Internal,

    /// The symbol can be accessed from anywhere.
    #[display(fmt = "public")]
    Public,
}

impl Accessibility {
    /// Creates a new [`Accessibility`] from the given [`AccessModifier`].
    #[must_use]
    pub fn from_syntax_tree(syntax_tree: &AccessModifier) -> Self {
        match syntax_tree {
            AccessModifier::Public(..) => Self::Public,
            AccessModifier::Private(..) => Self::Private,
            AccessModifier::Internal(..) => Self::Internal,
        }
    }

    /// Returns the rank of the accessibility. The higher the rank, the more accessible the symbol
    /// is.
    #[must_use]
    pub fn rank(&self) -> u8 {
        match self {
            Self::Private => 0,
            Self::Internal => 1,
            Self::Public => 2,
        }
    }
}

/// Represents an trait-member associated type symbol.
#[derive(Debug, Clone)]
pub struct TraitType {
    /// The name of the associated type.
    pub name: String,

    /// The generics of the associated type.
    pub generic_parameters: GenericParameters,

    /// The ID of the trait that contais the associated type.
    pub parent_trait_id: arena::ID<Trait>,

    /// The syntax tree of the associated type.
    pub syntax_tree: Option<Arc<syntax_tree::item::TraitType>>,
}

impl Symbol for arena::Symbol<TraitType> {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_trait_id.into()) }
}

impl Global for arena::Symbol<TraitType> {
    fn name(&self) -> &str { &self.name }
}

/// Represents generic parameters substitution.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Substitution {
    /// Maps type parameters to their type arguments.
    pub type_arguments_by_parameter: BTreeMap<arena::ID<TypeParameter>, ty::Type>,

    /// Maps lifetime parameters to their lifetime arguments.
    pub lifetime_arguments_by_parameter: BTreeMap<arena::ID<LifetimeParameter>, LifetimeArgument>,
}

impl Substitution {
    /// Applies the substitution mappings to the given type.
    pub fn apply_type(&self, ty: &mut ty::Type) {
        match ty {
            ty::Type::Struct(struct_type) => {
                for type_argument in struct_type
                    .substitution
                    .type_arguments_by_parameter
                    .values_mut()
                {
                    self.apply_type(type_argument);
                }

                for lifetime_argument in struct_type
                    .substitution
                    .lifetime_arguments_by_parameter
                    .values_mut()
                {
                    let LifetimeArgument::Parameter(lifetime_parameter) = lifetime_argument else {
                        continue;
                    };

                    if let Some(sub) = self.lifetime_arguments_by_parameter.get(lifetime_parameter)
                    {
                        *lifetime_argument = *sub;
                    }
                }
            }
            ty::Type::Primitive(..) => {}
            ty::Type::Reference(reference_type) => self.apply_type(&mut reference_type.operand),
            ty::Type::Parameter(type_parameter) => {
                if let Some(sub) = self.type_arguments_by_parameter.get(type_parameter) {
                    *ty = sub.clone();
                }
            }
            ty::Type::TraitType(trait_type) => {
                for type_argument in trait_type
                    .substitution
                    .type_arguments_by_parameter
                    .values_mut()
                {
                    self.apply_type(type_argument);
                }

                for lifetime_argument in trait_type
                    .substitution
                    .lifetime_arguments_by_parameter
                    .values_mut()
                {
                    let LifetimeArgument::Parameter(lifetime_parameter) = lifetime_argument else {
                        continue;
                    };

                    if let Some(sub) = self.lifetime_arguments_by_parameter.get(lifetime_parameter)
                    {
                        *lifetime_argument = *sub;
                    }
                }
            }
        }
    }

    /// Checks if the type parameter substitution is empty.
    #[must_use]
    pub fn type_parameter_is_empty(&self) -> bool { self.type_arguments_by_parameter.is_empty() }

    /// Checks if the lifetime parameter substitution is empty.
    #[must_use]
    pub fn lifetime_parameter_is_empty(&self) -> bool {
        self.lifetime_arguments_by_parameter.is_empty()
    }

    /// Checks if both type and lifetime parameter substitutions are empty.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.type_parameter_is_empty() && self.lifetime_parameter_is_empty()
    }
}

/// Represents a trait bound constraint.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitBound {
    /// The ID of the trait that is bound.
    pub trait_id: arena::ID<Trait>,

    /// Contains the generic parameters substitution.
    pub substitution: Substitution,
}

/// Represents an implements block.
#[derive(Debug, Clone)]
pub struct Implements {
    /// The generics of the implements block.
    pub generics: Generics,

    /// The trait that is implemented.
    pub trait_id: arena::ID<Trait>,

    /// Contains the generic parameters substitution of the trait.
    pub substitution: Substitution,

    /// Maps associated type to their type implementation.
    pub implements_types_by_associated_type:
        HashMap<arena::ID<TraitType>, arena::ID<ImplementsType>>,

    /// Maps function to their function implementation.
    pub implements_functions_by_trait_function:
        HashMap<arena::ID<TraitFunction>, arena::ID<ImplementsFunction>>,
}

impl Symbol for arena::Symbol<Implements> {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { None }
}

/// Represents an implements-member type.
#[derive(Debug, Clone)]
pub struct ImplementsType {
    /// The generic parameters of the type.
    pub generics: Generics,

    /// The ID of the associated type that is implemented.
    pub associated_type_id: arena::ID<TraitType>,

    /// The type that implements the associated type.
    pub alias: ty::Type,

    /// The ID of the implements that contains this symbol.
    pub parent_implements_id: arena::ID<Implements>,
}

impl Symbol for arena::Symbol<ImplementsType> {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_implements_id.into()) }
}

impl Genericable for arena::Symbol<ImplementsType> {
    fn generic_parameters(&self) -> &GenericParameters { &self.generics.parameters }

    fn where_clause(&self) -> Option<&WhereClause> { Some(&self.generics.where_clause) }
}

/// Represents an implements-member function.
#[derive(Debug, Clone, Deref, DerefMut)]
pub struct ImplementsFunction {
    /// Contains the data of function signature.
    #[deref]
    #[deref_mut]
    pub function_signature: FunctionSignature<Self>,

    /// The ID of the implements that contains the function.
    pub parent_implements_id: arena::ID<Trait>,

    /// The ID of the trait function that is implemented.
    pub trait_function_id: arena::ID<TraitFunction>,
}

impl Symbol for arena::Symbol<ImplementsFunction> {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_implements_id.into()) }
}

impl Genericable for arena::Symbol<ImplementsFunction> {
    fn generic_parameters(&self) -> &GenericParameters { &self.generics.parameters }

    fn where_clause(&self) -> Option<&WhereClause> { Some(&self.generics.where_clause) }
}

/// Is an enumeration of symbol IDs that can be used as a trait member.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum TraitMemberID {
    TraitFunction(arena::ID<TraitFunction>),
    TraitType(arena::ID<TraitType>),
}

impl From<TraitMemberID> for GlobalID {
    fn from(id: TraitMemberID) -> Self {
        match id {
            TraitMemberID::TraitFunction(id) => id.into(),
            TraitMemberID::TraitType(id) => id.into(),
        }
    }
}

/// Represents a trait symbol.
#[derive(Debug, Clone)]
pub struct Trait {
    /// The name of the trait.
    pub name: String,

    /// The ID of the module that contains this trait.
    pub parent_module_id: arena::ID<Module>,

    /// The generics of the trait.
    pub generics: Generics,

    /// The list of implements of the trait.
    pub implements: Vec<arena::ID<Implements>>,

    /// The syntax tree of the trait.
    pub syntax_tree: Option<Arc<syntax_tree::item::TraitSignature>>,

    /// The accessibility of the trait.
    pub accessibility: Accessibility,

    /// Maps the name of the trait member to its ID.
    pub trait_member_ids_by_name: HashMap<String, TraitMemberID>,
}

impl Symbol for arena::Symbol<Trait> {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_module_id.into()) }
}

impl Global for arena::Symbol<Trait> {
    fn name(&self) -> &str { &self.name }
}

impl Scoped for arena::Symbol<Trait> {
    fn get_child_id_by_name(&self, name: &str) -> Option<GlobalID> {
        self.trait_member_ids_by_name
            .get(name)
            .copied()
            .map(Into::into)
    }
}

/// Represents a trait-member function symbol.
#[derive(Debug, Clone, Deref, DerefMut)]
pub struct TraitFunction {
    /// Contains the data of function signature.
    #[deref]
    #[deref_mut]
    pub function_signature: FunctionSignature<Self>,

    /// The ID of the module that contains the function.
    pub parent_trait_id: arena::ID<Trait>,
}

impl Symbol for arena::Symbol<TraitFunction> {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_trait_id.into()) }
}

impl Global for arena::Symbol<TraitFunction> {
    fn name(&self) -> &str { &self.name }
}

impl Genericable for arena::Symbol<TraitFunction> {
    fn generic_parameters(&self) -> &GenericParameters { &self.generics.parameters }

    fn where_clause(&self) -> Option<&WhereClause> { Some(&self.generics.where_clause) }
}

/// Is an enumeration of symbol IDs that accept generic parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum GenericableID {
    Struct(arena::ID<Struct>),
    Function(arena::ID<Function>),
    Implements(arena::ID<Implements>),
    Trait(arena::ID<Trait>),
    TraitType(arena::ID<TraitType>),
    TraitFunction(arena::ID<TraitFunction>),
    Type(arena::ID<Type>),
    ImplementsFunction(arena::ID<ImplementsFunction>),
    ImplementsType(arena::ID<ImplementsType>),
}

impl From<GenericableID> for ID {
    fn from(id: GenericableID) -> Self {
        match id {
            GenericableID::Struct(id) => id.into(),
            GenericableID::Function(id) => id.into(),
            GenericableID::Implements(id) => id.into(),
            GenericableID::Trait(id) => id.into(),
            GenericableID::TraitType(id) => id.into(),
            GenericableID::TraitFunction(id) => id.into(),
            GenericableID::Type(id) => id.into(),
            GenericableID::ImplementsFunction(id) => id.into(),
            GenericableID::ImplementsType(id) => id.into(),
        }
    }
}

impl TryFrom<GlobalID> for GenericableID {
    type Error = GlobalID;

    fn try_from(value: GlobalID) -> Result<Self, Self::Error> {
        match value {
            GlobalID::Struct(id) => Ok(Self::Struct(id)),
            GlobalID::Function(id) => Ok(Self::Function(id)),
            GlobalID::Trait(id) => Ok(Self::Trait(id)),
            GlobalID::TraitType(id) => Ok(Self::TraitType(id)),
            GlobalID::TraitFunction(id) => Ok(Self::TraitFunction(id)),
            GlobalID::Type(id) => Ok(Self::Type(id)),
            id => Err(id),
        }
    }
}

impl TryFrom<ID> for GenericableID {
    type Error = ID;

    fn try_from(value: ID) -> Result<Self, Self::Error> {
        match value {
            ID::Struct(id) => Ok(Self::Struct(id)),
            ID::Function(id) => Ok(Self::Function(id)),
            ID::Implements(id) => Ok(Self::Implements(id)),
            ID::Trait(id) => Ok(Self::Trait(id)),
            ID::TraitType(id) => Ok(Self::TraitType(id)),
            ID::TraitFunction(id) => Ok(Self::TraitFunction(id)),
            ID::Type(id) => Ok(Self::Type(id)),
            ID::ImplementsFunction(id) => Ok(Self::ImplementsFunction(id)),
            ID::ImplementsType(id) => Ok(Self::ImplementsType(id)),
            id => Err(id),
        }
    }
}

/// Represents a symbol with generics
pub trait Genericable: Symbol {
    /// Returns the generics of the symbol.
    fn generic_parameters(&self) -> &GenericParameters;

    /// Returns the where clause of the symbol.
    fn where_clause(&self) -> Option<&WhereClause>;
}

impl Genericable for arena::Symbol<Struct> {
    fn generic_parameters(&self) -> &GenericParameters { &self.generics.parameters }

    fn where_clause(&self) -> Option<&WhereClause> { Some(&self.generics.where_clause) }
}

impl Genericable for arena::Symbol<Function> {
    fn generic_parameters(&self) -> &GenericParameters { &self.generics.parameters }

    fn where_clause(&self) -> Option<&WhereClause> { Some(&self.generics.where_clause) }
}

impl Genericable for arena::Symbol<Implements> {
    fn generic_parameters(&self) -> &GenericParameters { &self.generics.parameters }

    fn where_clause(&self) -> Option<&WhereClause> { Some(&self.generics.where_clause) }
}

impl Genericable for arena::Symbol<Trait> {
    fn generic_parameters(&self) -> &GenericParameters { &self.generics.parameters }

    fn where_clause(&self) -> Option<&WhereClause> { Some(&self.generics.where_clause) }
}

impl Genericable for arena::Symbol<TraitType> {
    fn generic_parameters(&self) -> &GenericParameters { &self.generic_parameters }

    fn where_clause(&self) -> Option<&WhereClause> { None }
}

/// Is a trait for symbols that can be used as a generic parameter.
pub trait GenericParameter: Symbol {
    /// Returns the name of the generic parameter.
    fn name(&self) -> &str;

    /// Returns the ID of the parent genericable.
    fn parent_genericable_id(&self) -> GenericableID;
}

impl GenericParameter for arena::Symbol<LifetimeParameter> {
    fn name(&self) -> &str { &self.name }

    fn parent_genericable_id(&self) -> GenericableID { self.parent_genericable_id }
}

impl GenericParameter for arena::Symbol<TypeParameter> {
    fn name(&self) -> &str { &self.name }

    fn parent_genericable_id(&self) -> GenericableID { self.parent_genericable_id }
}

/// Is an enumeration of symbols ID that can be used as a generic parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum GenericParameterID {
    Type(arena::ID<TypeParameter>),
    Lifetime(arena::ID<LifetimeParameter>),
}

/// Represents a lifetime parameter symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeParameter {
    /// The name of the lifetime parameter.
    pub name: String,

    /// The ID of the parent genericable.
    pub parent_genericable_id: GenericableID,
}

impl Symbol for arena::Symbol<LifetimeParameter> {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_genericable_id.into()) }
}

/// Represents a type parameter symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeParameter {
    /// The name of the type parameter.
    pub name: String,

    /// The ID of the parent genericable.
    pub parent_genericable_id: GenericableID,
}

impl Symbol for arena::Symbol<TypeParameter> {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_genericable_id.into()) }
}

/// Is an enumeration of either a lifetime parameter or a static lifetime.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeArgument {
    Static,
    Parameter(arena::ID<LifetimeParameter>),
}

/// Contains the declaration of the generic parameters (both lifetimes and type parameters).
#[derive(Debug, Clone, Default)]
pub struct GenericParameters {
    /// The declaration order of the type parameters.
    pub type_parameter_order: Vec<arena::ID<TypeParameter>>,

    /// Maps the name of the type parameter to its ID.
    pub type_parameter_ids_by_name: HashMap<String, arena::ID<TypeParameter>>,

    /// The declaration order of the lifetime parameters.
    pub lifetime_parameter_order: Vec<arena::ID<LifetimeParameter>>,

    /// Maps the name of the lifetime parameter to its ID.
    pub lifetime_parameter_ids_by_name: HashMap<String, arena::ID<LifetimeParameter>>,
}

/// Contains all the constraint defined on the where clause of a genericable.
#[derive(Debug, Clone, Default)]
pub struct WhereClause {
    /// Maps the lifetime parameter to its lifetime bounds.
    pub lifetime_argument_sets_by_lifetime_parameter:
        HashMap<arena::ID<LifetimeParameter>, HashSet<LifetimeArgument>>,

    /// Maps the associated trait type to its lietime argument bounds.
    pub lifetime_argument_sets_by_trait_type: HashMap<ty::TraitType, HashSet<LifetimeArgument>>,

    /// Maps the associated trait type to its type bound.
    pub types_by_trait_type: HashMap<ty::TraitType, ty::Type>,

    /// Maps the type parameter to its lifetime bounds.
    pub lifetime_argument_sets_by_type_parameter:
        HashMap<arena::ID<TypeParameter>, HashSet<LifetimeArgument>>,
}

/// Contains all the information related to the generics.
#[derive(Debug, Clone, Default)]
pub struct Generics {
    /// The generic parameters of the symbol.
    pub parameters: GenericParameters,

    /// The where clause of the symbol.
    pub where_clause: WhereClause,
}

/// Is a trait that all symbols must implement.
pub trait Symbol {
    /// The ID of the symbol.
    fn id(&self) -> ID;

    /// The ID of the symbol in the higher level.
    fn parent_symbol(&self) -> Option<ID>;
}

/// A trait representing the symbol that can be referred in the global scope and has a clear
/// hierarchy.
pub trait Global: Symbol {
    /// Returns the name of the symbol.
    fn name(&self) -> &str;
}

/// A trait representing the symbol that introduces a new scope and can contain child symbols.
pub trait Scoped: Global {
    /// Returns the ID of the child symbol contained in this scope from the given name.
    fn get_child_id_by_name(&self, name: &str) -> Option<GlobalID>;
}

/// Contains the data of the module symbol.
#[derive(Debug, Clone)]
pub struct Module {
    /// The name of the module
    pub name: String,

    /// The accessibility of the module
    pub accessibility: Accessibility,

    /// The parent ID of the module. If `None` then the module is the root module.
    pub parent_module_id: Option<arena::ID<Module>>,

    /// Maps the name of the symbol defined in this module to its corresponding ID.
    pub module_child_ids_by_name: HashMap<String, ModuleChildID>,

    /// The IDs of modules that are used in the `using` statements.
    pub usings: HashSet<arena::ID<Module>>,
}

impl Symbol for arena::Symbol<Module> {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { self.parent_module_id.map(ID::Module) }
}

impl Global for arena::Symbol<Module> {
    fn name(&self) -> &str { &self.name }
}

impl Scoped for arena::Symbol<Module> {
    fn get_child_id_by_name(&self, name: &str) -> Option<GlobalID> {
        self.module_child_ids_by_name
            .get(name)
            .copied()
            .map(GlobalID::from)
    }
}

/// Contains the data of the field symbol.
#[derive(Debug, Clone)]
pub struct Field {
    /// The name of the field
    pub name: String,

    /// The accessibility of the field
    pub accessibility: Accessibility,

    /// The struct ID where the field is defined.
    pub parent_struct_id: arena::ID<Struct>,

    /// The syntax tree that was used to create the field.
    pub syntax_tree: Option<Arc<StructFieldSyntaxTree>>,

    /// The order in which the field was declared.
    pub declaration_order: usize,

    /// The type of the field.
    pub ty: ty::Type,
}

impl Symbol for arena::Symbol<Field> {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_struct_id.into()) }
}

/// Contains the data of the struct symbol.
#[derive(Debug, Clone)]
pub struct Struct {
    /// The name of the struct pub name: String,
    pub name: String,

    /// The accessibility of the struct
    pub accessibility: Accessibility,

    /// The parent ID of the struct.
    pub parent_module_id: arena::ID<Module>,

    /// The syntax tree that was used to create the struct.
    pub syntax_tree: Option<Arc<StructSignatureSyntaxTree>>,

    /// Maps the name of the field to its corresponding ID.
    pub field_ids_by_name: HashMap<String, arena::ID<Field>>,

    /// Maps the name of the field to its corresponding ID.
    pub generics: Generics,

    /// List of the fields in the order in which they were declared.
    pub field_order: Vec<arena::ID<Field>>,
}

impl Symbol for arena::Symbol<Struct> {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_module_id.into()) }
}

impl Global for arena::Symbol<Struct> {
    fn name(&self) -> &str { &self.name }
}

/// Contains the data of the enum symbol.
#[derive(Debug, Clone)]
pub struct EnumVariant {
    /// The nae of the enum variant
    pub name: String,

    /// The ID of the enum that contains the enum variant.
    pub parent_enum_id: arena::ID<Enum>,

    /// The order in which the enum variant was declared.
    pub declaration_order: usize,

    /// The syntax tree that was used to create the enum variant.
    pub syntax_tree: Option<Arc<Identifier>>,
}

impl Symbol for arena::Symbol<EnumVariant> {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_enum_id.into()) }
}

impl Global for arena::Symbol<EnumVariant> {
    fn name(&self) -> &str { &self.name }
}

/// Contains the data of the enum symbol.
#[derive(Debug, Clone)]
pub struct Enum {
    /// The name of the enum
    pub name: String,

    /// The accessibility of the enum
    pub accessibility: Accessibility,

    /// The ID of the module that contains the enum.
    pub parent_module_id: arena::ID<Module>,

    /// The syntax tree that was used to create the enum.
    pub syntax_tree: Option<Arc<EnumSignatureSyntaxTree>>,

    /// Maps the name of the enum variant to the ID of the enum variant.
    pub variant_ids_by_name: HashMap<String, arena::ID<EnumVariant>>,

    /// List of the enum variants in the order in which they were declared.
    pub variant_order: Vec<arena::ID<EnumVariant>>,
}

impl Symbol for arena::Symbol<Enum> {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_module_id.into()) }
}

impl Scoped for arena::Symbol<Enum> {
    fn get_child_id_by_name(&self, name: &str) -> Option<GlobalID> {
        self.variant_ids_by_name
            .get(name)
            .copied()
            .map(GlobalID::EnumVariant)
    }
}

impl Global for arena::Symbol<Enum> {
    fn name(&self) -> &str { &self.name }
}

/// Represents an overload signature syntax tree.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct FunctionSignatureSyntaxTree {
    pub identifier: Identifier,
    pub generic_parameters: Option<syntax_tree::item::GenericParameters>,
    pub return_type: Option<ReturnType>,
    pub where_clause: Option<syntax_tree::item::WhereClause>,
}

/// Is an enumeration of all symbols that are allowed to be a parent of a parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ParameterParentID {
    Function(arena::ID<Function>),
    TraitFunction(arena::ID<TraitFunction>),
}

impl From<ParameterParentID> for ID {
    fn from(id: ParameterParentID) -> Self {
        match id {
            ParameterParentID::Function(id) => id.into(),
            ParameterParentID::TraitFunction(id) => id.into(),
        }
    }
}

/// Contains the data of parameter symbol.
#[derive(Debug, Clone)]
pub struct Parameter<T> {
    /// The name of the parameter
    pub name: String,

    /// The id to the parent symbol of the parameter
    pub parameter_parent_id: arena::ID<T>,

    /// The order in which the parameter was declared.
    pub declaration_order: usize,

    /// The type of the parameter
    pub ty: ty::Type,

    /// The syntax tree of the parameter.
    pub syntax_tree: Option<Arc<syntax_tree::item::Parameter>>,

    /// Whether the parameter is mutable.
    pub is_mutable: bool,
}

impl<T> Symbol for arena::Symbol<Parameter<T>>
where
    arena::ID<T>: Into<ID>,
    arena::ID<Parameter<T>>: Into<ID>,
{
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parameter_parent_id.into()) }
}

/// Contains the data of the function signature symbol.
#[derive(Debug, Clone)]
pub struct FunctionSignature<T> {
    /// The name of the overload set.
    pub name: String,

    /// Maps the name of the parameter to its corresponding ID.
    pub parameter_ids_by_name: HashMap<String, arena::ID<Parameter<T>>>,

    /// List of the parameters in the order in which they were declared.
    pub parameter_order: Vec<arena::ID<Parameter<T>>>,

    /// The return type of the overload.
    pub return_type: ty::Type,

    /// The syntax tree of the function signature.
    pub syntax_tree: Option<Arc<FunctionSignatureSyntaxTree>>,

    /// The generics of the overload.
    pub generics: Generics,
}

impl Symbol for arena::Symbol<Function> {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_module_id.into()) }
}

impl Global for arena::Symbol<Function> {
    fn name(&self) -> &str { &self.name }
}

/// Contains the data of function symbol.
#[derive(Debug, Clone, Deref, DerefMut)]
pub struct Function {
    /// Contains the data of function signature.
    #[deref]
    #[deref_mut]
    pub function_signature: FunctionSignature<Self>,

    /// The ID of the module that contains the function.
    pub parent_module_id: arena::ID<Module>,

    /// The syntax tree of the function body.
    pub syntax_tree: Option<Arc<syntax_tree::item::FunctionBody>>,

    /// The accessibility of the function.
    pub accessibility: Accessibility,
}

/// Is an enumeration of symbols that can be used as a parent of an overload set.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ParentOverloadSetID {
    Module(arena::ID<Module>),
    Trait(arena::ID<Trait>),
}

/// Contains the data of type alias symbol.
#[derive(Debug, Clone)]
pub struct Type {
    /// The name of the type alias.
    pub name: String,

    /// The accessibility of the type alias.
    pub accessibility: Accessibility,

    /// The ID of the parent symbol that contains the type alias.
    pub parent_module_id: arena::ID<Module>,

    /// The type that the type alias represents.
    pub alias: ty::Type,

    /// The generic parameters of the type alias.
    pub generic_parameters: GenericParameters,

    /// The syntax tree that was used to create the type alias.
    pub syntax_tree: Option<Arc<TypeSyntaxTree>>,
}

impl Genericable for arena::Symbol<Type> {
    fn generic_parameters(&self) -> &GenericParameters { &self.generic_parameters }

    fn where_clause(&self) -> Option<&WhereClause> { None }
}

impl Symbol for arena::Symbol<Type> {
    fn id(&self) -> ID { self.id().into() }

    fn parent_symbol(&self) -> Option<ID> { Some(self.parent_module_id.into()) }
}

impl Global for arena::Symbol<Type> {
    fn name(&self) -> &str { &self.name }
}

/// Is an enumeration of ID of the symbols that can be used as a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum TypedID {
    Enum(arena::ID<Enum>),
    Struct(arena::ID<Struct>),
}

impl From<TypedID> for GlobalID {
    fn from(value: TypedID) -> Self {
        match value {
            TypedID::Enum(id) => Self::Enum(id),
            TypedID::Struct(id) => Self::Struct(id),
        }
    }
}

impl From<TypedID> for ID {
    fn from(value: TypedID) -> Self {
        match value {
            TypedID::Enum(id) => Self::Enum(id),
            TypedID::Struct(id) => Self::Struct(id),
        }
    }
}

impl TryFrom<GlobalID> for ScopedID {
    type Error = GlobalID;

    fn try_from(value: GlobalID) -> Result<Self, Self::Error> {
        match value {
            GlobalID::Module(s) => Ok(Self::Module(s)),
            GlobalID::Enum(s) => Ok(Self::Enum(s)),
            GlobalID::Trait(s) => Ok(Self::Trait(s)),
            value => Err(value),
        }
    }
}

/// Is an enumeration of ID of the symbols that introduce a new scope.
#[derive(Debug, Clone, Copy, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ScopedID {
    Module(arena::ID<Module>),
    Enum(arena::ID<Enum>),
    Trait(arena::ID<Trait>),
}

impl From<ScopedID> for GlobalID {
    fn from(value: ScopedID) -> Self {
        match value {
            ScopedID::Module(id) => Self::Module(id),
            ScopedID::Enum(id) => Self::Enum(id),
            ScopedID::Trait(id) => Self::Trait(id),
        }
    }
}

impl From<ScopedID> for ID {
    fn from(value: ScopedID) -> Self {
        match value {
            ScopedID::Module(id) => Self::Module(id),
            ScopedID::Enum(id) => Self::Enum(id),
            ScopedID::Trait(id) => Self::Trait(id),
        }
    }
}

impl TryFrom<ID> for ScopedID {
    type Error = ID;

    fn try_from(value: ID) -> Result<Self, Self::Error> {
        match value {
            ID::Module(s) => Ok(Self::Module(s)),
            ID::Enum(s) => Ok(Self::Enum(s)),
            ID::Trait(s) => Ok(Self::Trait(s)),
            value => Err(value),
        }
    }
}

/// Is an enumeration of ID of the symbols that can be defined in a module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ModuleChildID {
    Module(arena::ID<Module>),
    Struct(arena::ID<Struct>),
    Enum(arena::ID<Enum>),
    Function(arena::ID<Function>),
    Type(arena::ID<Type>),
    Trait(arena::ID<Trait>),
}

impl From<ModuleChildID> for ID {
    fn from(value: ModuleChildID) -> Self {
        match value {
            ModuleChildID::Module(id) => Self::Module(id),
            ModuleChildID::Struct(id) => Self::Struct(id),
            ModuleChildID::Enum(id) => Self::Enum(id),
            ModuleChildID::Function(id) => Self::Function(id),
            ModuleChildID::Type(id) => Self::Type(id),
            ModuleChildID::Trait(id) => Self::Trait(id),
        }
    }
}

impl TryFrom<ID> for ModuleChildID {
    type Error = ID;

    fn try_from(value: ID) -> Result<Self, Self::Error> {
        match value {
            ID::Module(s) => Ok(Self::Module(s)),
            ID::Struct(s) => Ok(Self::Struct(s)),
            ID::Enum(s) => Ok(Self::Enum(s)),
            ID::Function(s) => Ok(Self::Function(s)),
            ID::Type(s) => Ok(Self::Type(s)),
            ID::Trait(s) => Ok(Self::Trait(s)),
            value => Err(value),
        }
    }
}

/// Is an enumeration of ID of the symbols that can be referred in the global scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum GlobalID {
    Module(arena::ID<Module>),
    Struct(arena::ID<Struct>),
    Enum(arena::ID<Enum>),
    EnumVariant(arena::ID<EnumVariant>),
    Function(arena::ID<Function>),
    Type(arena::ID<Type>),
    Trait(arena::ID<Trait>),
    TraitFunction(arena::ID<TraitFunction>),
    TraitType(arena::ID<TraitType>),
}

impl From<ModuleChildID> for GlobalID {
    fn from(value: ModuleChildID) -> Self {
        match value {
            ModuleChildID::Module(id) => Self::Module(id),
            ModuleChildID::Struct(id) => Self::Struct(id),
            ModuleChildID::Enum(id) => Self::Enum(id),
            ModuleChildID::Function(id) => Self::Function(id),
            ModuleChildID::Type(id) => Self::Type(id),
            ModuleChildID::Trait(id) => Self::Trait(id),
        }
    }
}

impl From<GlobalID> for ID {
    fn from(value: GlobalID) -> Self {
        match value {
            GlobalID::Module(id) => Self::Module(id),
            GlobalID::Struct(id) => Self::Struct(id),
            GlobalID::Enum(id) => Self::Enum(id),
            GlobalID::Function(id) => Self::Function(id),
            GlobalID::Type(id) => Self::Type(id),
            GlobalID::EnumVariant(id) => Self::EnumVariant(id),
            GlobalID::Trait(id) => Self::Trait(id),
            GlobalID::TraitFunction(id) => Self::TraitFunction(id),
            GlobalID::TraitType(id) => Self::TraitType(id),
        }
    }
}

impl TryFrom<ID> for GlobalID {
    type Error = ID;

    fn try_from(value: ID) -> Result<Self, Self::Error> {
        match value {
            ID::Module(s) => Ok(Self::Module(s)),
            ID::Struct(s) => Ok(Self::Struct(s)),
            ID::Enum(s) => Ok(Self::Enum(s)),
            ID::Function(s) => Ok(Self::Function(s)),
            ID::Type(s) => Ok(Self::Type(s)),
            ID::EnumVariant(s) => Ok(Self::EnumVariant(s)),
            ID::Trait(s) => Ok(Self::Trait(s)),
            ID::TraitFunction(s) => Ok(Self::TraitFunction(s)),
            ID::TraitType(s) => Ok(Self::TraitType(s)),
            value => Err(value),
        }
    }
}

/// Is an enumeration of all kinds of symbol IDs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ID {
    Module(arena::ID<Module>),
    Struct(arena::ID<Struct>),
    Enum(arena::ID<Enum>),
    EnumVariant(arena::ID<EnumVariant>),
    Function(arena::ID<Function>),
    Type(arena::ID<Type>),
    Field(arena::ID<Field>),
    FunctionParameter(arena::ID<Parameter<Function>>),
    TraitFunctionParameter(arena::ID<Parameter<TraitFunction>>),
    ImplementsFunctionParameter(arena::ID<Parameter<ImplementsFunction>>),
    Trait(arena::ID<Trait>),
    TraitType(arena::ID<TraitType>),
    TypeParameter(arena::ID<TypeParameter>),
    LifetimeParameter(arena::ID<LifetimeParameter>),
    TraitFunction(arena::ID<TraitFunction>),
    Implements(arena::ID<Implements>),
    ImplementsFunction(arena::ID<ImplementsFunction>),
    ImplementsType(arena::ID<ImplementsType>),
}
