//! Contains the definition of all symbol kinds in the language.

use std::collections::{HashMap, HashSet};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;
use pernixc_syntax::syntax_tree::AccessModifier;

use self::semantic::Symbolic;
use crate::{
    arena::{Arena, Map, ID},
    semantic::{
        model::Model,
        pattern::Irrefutable,
        predicate,
        substitution::Substitution,
        term::{constant, lifetime, r#type, GenericArguments},
    },
};

pub mod semantic;

/// Represents an accessibility of a symbol.
///
/// ```
/// use pernixc_semantic::symbol::Accessibility;
///
/// let private = Accessibility::Private;
/// let internal = Accessibility::Internal;
/// let public = Accessibility::Public;
///
/// assert!(private < internal);
/// assert!(internal < public);
/// assert!(private < public);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Accessibility {
    /// The symbol is accessible only from the same module and its submodules.
    Private,

    /// The symbol is accessible only from the same target.
    Internal,

    /// The symbol is accessible from anywhere.
    Public,
}

impl Accessibility {
    /// Converts the [`AccessModifier`] to [`Accessibility`].
    #[must_use]
    pub fn from_syntax_tree(syntax_tree: &AccessModifier) -> Self {
        match syntax_tree {
            AccessModifier::Public(..) => Self::Public,
            AccessModifier::Private(..) => Self::Private,
            AccessModifier::Internal(..) => Self::Internal,
        }
    }

    /// Gets the rank of the accessibility.
    ///
    /// The higher the number, the more accessible the accessibility is.
    #[must_use]
    pub fn rank(&self) -> usize {
        match self {
            Self::Private => 0,
            Self::Internal => 1,
            Self::Public => 2,
        }
    }
}

/// Represents a predicate introduced by either a where clause or implication.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Predicate {
    /// The predicate itself.
    pub predicate: predicate::Predicate<Symbolic>,

    /// Location of where the predicate is declared.
    pub span: Option<Span>,

    /// Whether the predicate is explicitly declared or was implied by the compiler.
    pub explicit: bool,
}

/// An ID of all kinds of symbols that implements the [`Generic`] trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum GenericID {
    Struct(ID<Struct>),
    Trait(ID<Trait>),
    Enum(ID<Enum>),
    Type(ID<Type>),
    Function(ID<Function>),
    TraitType(ID<TraitType>),
    TraitFunction(ID<TraitFunction>),
    NegativeImplementation(ID<NegativeImplementation>),
    Implementation(ID<Implementation>),
    ImplementationFunction(ID<ImplementationFunction>),
    ImplementationType(ID<ImplementationType>),
}

macro_rules! from_ids {
    ($from:ident, $to:ident $(, ($from_kind:ident, $to_kind:ident))*) => {
        impl From<$from> for $to {
            fn from(value: $from) -> Self {
                match value {
                    $(
                        $from::$from_kind(id) => Self::$to_kind(id),
                    )*
                }
            }
        }
    }
}

from_ids!(
    GenericID,
    GlobalID,
    (Struct, Struct),
    (Trait, Trait),
    (Enum, Enum),
    (Type, Type),
    (Function, Function),
    (TraitType, TraitType),
    (TraitFunction, TraitFunction),
    (NegativeImplementation, NegativeImplementation),
    (Implementation, Implementation),
    (ImplementationFunction, ImplementationFunction),
    (ImplementationType, ImplementationType)
);

macro_rules! try_from_ids {
    ($from:ident, $to:ident $(, ($from_kind:ident, $to_kind:ident))*) => {
        impl TryFrom<$from> for $to {
            type Error = $from;

            #[allow(unreachable_patterns)]
            fn try_from(value: $from) -> Result<Self, Self::Error> {
                match value {
                    $(
                        $from::$from_kind(id) => Ok(Self::$to_kind(id)),
                    )*
                    _ => Err(value)
                }
            }
        }
    };
}

try_from_ids!(
    GlobalID,
    GenericID,
    (Struct, Struct),
    (Trait, Trait),
    (Enum, Enum),
    (Type, Type),
    (Function, Function),
    (Trait, Trait),
    (TraitType, TraitType),
    (TraitFunction, TraitFunction),
    (Implementation, Implementation),
    (NegativeImplementation, NegativeImplementation),
    (ImplementationType, ImplementationType),
    (ImplementationFunction, ImplementationFunction)
);

/// Represents a kind of symbol that accepts generic arguments.
pub trait Generic {
    /// The ID representing the symbol itself.
    fn generic_id(&self) -> GenericID;

    /// Gets the [`GenericParameters`] of the symbol.
    fn generic_declaration(&self) -> &GenericDeclaration;

    #[doc(hidden)]
    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration;
}

/// Represents a generic declaration containing generic parameters and predicates.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct GenericDeclaration {
    /// Generic parameters defined in the generic declaration.
    pub parameters: GenericParameters,

    /// Contains all the predicates required by the generic declaration.
    pub predicates: Vec<Predicate>,
}

/// An ID of all kinds of symbols that implements the [`Global`] trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum GlobalID {
    Module(ID<Module>),
    Struct(ID<Struct>),
    Trait(ID<Trait>),
    Enum(ID<Enum>),
    Type(ID<Type>),
    Constant(ID<Constant>),
    Function(ID<Function>),
    Variant(ID<Variant>),
    TraitType(ID<TraitType>),
    TraitFunction(ID<TraitFunction>),
    TraitConstant(ID<TraitConstant>),
    Implementation(ID<Implementation>),
    NegativeImplementation(ID<NegativeImplementation>),
    ImplementationFunction(ID<ImplementationFunction>),
    ImplementationType(ID<ImplementationType>),
    ImplementationConstant(ID<ImplementationConstant>),
}

/// Represents a kind of symbol that has a clear hierarchy/name and can be referenced globally by a
/// qualified name.
pub trait Global {
    /// The name of the symbol.
    fn name(&self) -> &str;

    /// The ID representing the symbol itself.
    fn global_id(&self) -> GlobalID;

    /// The ID of the parent symbol.
    fn parent_global_id(&self) -> Option<GlobalID>;

    /// Gets the ID of the global symbol defined as a member of this symbol with its name.
    fn get_member(&self, name: &str) -> Option<GlobalID>;

    /// Location of where the symbol is declared.
    fn span(&self) -> Option<Span>;
}

/// An ID to all kinds of symbols that can be defined in a module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum ModuleMemberID {
    Module(ID<Module>),
    Enum(ID<Enum>),
    Struct(ID<Struct>),
    Trait(ID<Trait>),
    Type(ID<Type>),
    Function(ID<Function>),
    Constant(ID<Constant>),
}

from_ids!(
    ModuleMemberID,
    GlobalID,
    (Module, Module),
    (Enum, Enum),
    (Struct, Struct),
    (Trait, Trait),
    (Type, Type),
    (Function, Function),
    (Constant, Constant)
);

/// Represents a module declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    /// The ID of the module declaration.
    pub id: ID<Module>,

    /// The name of the module declaration.
    pub name: String,

    /// The accessibility of the module.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    ///
    /// If this module is a root module, then this field is `None`.
    pub parent_module_id: Option<ID<Module>>,

    /// Maps the name of the module child to its ID.
    pub module_child_ids_by_name: HashMap<String, ModuleMemberID>,

    /// Location of where the module is declared.
    pub span: Option<Span>,

    /// The modules that are used by `using` statements.
    pub usings: HashSet<ID<Module>>,
}

impl Global for Module {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Module(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> { self.parent_module_id.map(GlobalID::Module) }

    fn get_member(&self, name: &str) -> Option<GlobalID> {
        self.module_child_ids_by_name
            .get(name)
            .copied()
            .map(Into::into)
    }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents a lifetime parameter, denoted by `'a` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeParameter {
    /// The name of the lifetime parameter (if none, then it is anonymous lifetime parameter )
    pub name: Option<String>,

    /// The ID where the lifetime parameter is declared.
    pub parent_generic_id: GenericID,

    /// Location of where the lifetime parameter is declared.
    pub span: Option<Span>,
}

/// An ID used to refer to a particular symbol defined in a particular **parent** symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberID<ChildID, ParentID> {
    /// Symbol ID of the parent symbol.
    pub parent: ParentID,

    /// Symbol ID of the child symbol.
    pub id: ChildID,
}

/// An ID to a type parameter.
pub type TypeParameterID = MemberID<ID<TypeParameter>, GenericID>;

/// An ID to a constant parameter.
pub type ConstantParameterID = MemberID<ID<ConstantParameter>, GenericID>;

/// An ID to a lifetime parameter.
pub type LifetimeParameterID = MemberID<ID<LifetimeParameter>, GenericID>;

/// Represents a type parameter, denoted by `T` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeParameter {
    /// The name of the type parameter.
    pub name: String,

    /// The ID where the type parameter is declared.
    pub parent_generic_id: GenericID,

    /// The kind of the type parameter.
    pub span: Option<Span>,
}

/// Represents a constant parameter, denoted by `const C: TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantParameter {
    /// The name of the constant parameter.
    pub name: String,

    /// The ID where the constant parameter is declared.
    pub parent_generic_id: GenericID,

    /// The type of the constant parameter.
    pub r#type: r#type::Type<Symbolic>,

    /// The type of the constant parameter.
    pub span: Option<Span>,
}

/// Represents a list of generic parameters.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct GenericParameters {
    /// List of defined lifetime parameters.
    pub lifetimes: Arena<LifetimeParameter>,

    /// List of defined type parameters.
    pub types: Arena<TypeParameter>,

    /// List of defined constant parameters.
    pub constants: Arena<ConstantParameter>,

    /// Maps the name of the lifetime parameter to its ID.
    pub lifetime_parameter_ids_by_name: HashMap<String, ID<LifetimeParameter>>,

    /// Maps the name of the type parameter to its ID.
    pub type_parameter_ids_by_name: HashMap<String, ID<TypeParameter>>,

    /// Maps the name of the constant parameter to its ID.
    pub constant_parameter_ids_by_name: HashMap<String, ID<ConstantParameter>>,

    /// List of default type parameters to be used when the generic parameters are not specified.
    pub default_type_parameters: Vec<r#type::Type<Symbolic>>,

    /// List of default constant parameters to be used when the generic parameters are not
    pub default_constant_parameters: Vec<constant::Constant<Symbolic>>,
}

impl GenericParameters {
    /// Creates a new [`GenericArguments`] that contains the generic parameters as its arguments.
    #[must_use]
    pub fn create_identity_generic_arguments<S: Model>(
        &self,
        parent_generic_id: GenericID,
    ) -> GenericArguments<S> {
        GenericArguments {
            types: (0..self.types.len())
                .map(|idx| {
                    r#type::Type::Parameter(TypeParameterID {
                        parent: parent_generic_id,
                        id: ID::new(idx),
                    })
                })
                .collect(),
            lifetimes: (0..self.lifetimes.len())
                .map(|idx| {
                    lifetime::Lifetime::Parameter(LifetimeParameterID {
                        parent: parent_generic_id,
                        id: ID::new(idx),
                    })
                })
                .collect(),
            constants: (0..self.constants.len())
                .map(|idx| {
                    constant::Constant::Parameter(ConstantParameterID {
                        parent: parent_generic_id,
                        id: ID::new(idx),
                    })
                })
                .collect(),
        }
    }

    /// Creates a new [`Substitution`] that maps all the generic parameters to itself.
    #[must_use]
    pub fn create_identity_substitution<S: Model>(
        &self,
        parent_generic_id: GenericID,
    ) -> Substitution<S> {
        Substitution {
            types: (0..self.types.len())
                .map(|idx| {
                    let type_parameter = r#type::Type::Parameter(TypeParameterID {
                        parent: parent_generic_id,
                        id: ID::new(idx),
                    });

                    (type_parameter.clone(), type_parameter)
                })
                .collect(),
            constants: (0..self.constants.len())
                .map(|idx| {
                    let constant_parameter = constant::Constant::Parameter(ConstantParameterID {
                        parent: parent_generic_id,
                        id: ID::new(idx),
                    });

                    (constant_parameter.clone(), constant_parameter)
                })
                .collect(),
            lifetimes: (0..self.lifetimes.len())
                .map(|idx| {
                    let lifetime_parameter = lifetime::Lifetime::Parameter(LifetimeParameterID {
                        parent: parent_generic_id,
                        id: ID::new(idx),
                    });

                    (lifetime_parameter.clone(), lifetime_parameter)
                })
                .collect(),
        }
    }
}

/// Represents a field declaration in the struct, denoted by `NAME: TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field {
    /// The accessibility of the field.
    pub accessibility: Accessibility,

    /// The name of the field.
    pub name: String,

    /// The type of the field.
    pub ty: r#type::Type<Symbolic>,

    /// Location of where the field is declared.
    pub span: Option<Span>,
}

/// Represents a struct declaration, denoted by `struct NAME { ... }` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
    /// The ID of the struct.
    pub id: ID<Struct>,

    /// The name of the struct.
    pub name: String,

    /// The accessibility of the struct.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    pub parent_module_id: ID<Module>,

    /// The generic declaration of the struct.
    pub generic_declaration: GenericDeclaration,

    /// Contains all the fields defined in the struct.
    pub fields: Map<Field>,

    /// Location of where the struct is declared.
    pub span: Option<Span>,
}

impl Generic for Struct {
    fn generic_id(&self) -> GenericID { GenericID::Struct(self.id) }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Struct {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Struct(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Module(self.parent_module_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents an enum variant declaration, denoted by `NAME(ASSOC_TYPE)` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    /// The ID of the variant.
    pub id: ID<Variant>,

    /// The name of the variant.
    pub name: String,

    /// The type of the associated value of the variant (if any).
    pub associated_type: Option<r#type::Type<Symbolic>>,

    /// The parent enum ID.
    pub parent_enum_id: ID<Enum>,

    /// The span where the variant is declared.
    pub span: Option<Span>,
}

impl Global for Variant {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Variant(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Enum(self.parent_enum_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents an enum declaration, denoted by `enum NAME { ... }` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    /// The ID of the enum.
    pub id: ID<Enum>,

    /// The name of the enum.
    pub name: String,

    /// The accessibility of the enum.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    pub parent_module_id: ID<Module>,

    /// The generic declaration of the enum.
    pub generic_declaration: GenericDeclaration,

    /// Maps the name of variants defined in the enum to its ID.
    pub variant_ids_by_name: HashMap<String, ID<Variant>>,

    /// Location of where the enum is declared.
    pub span: Option<Span>,
}

impl Generic for Enum {
    fn generic_id(&self) -> GenericID { GenericID::Enum(self.id) }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Enum {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Enum(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Module(self.parent_module_id)) }

    fn get_member(&self, name: &str) -> Option<GlobalID> {
        self.variant_ids_by_name
            .get(name)
            .copied()
            .map(GlobalID::Variant)
    }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents a type declaration, denoted by `type NAME = TYPE;` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    /// The ID of the type.
    pub id: ID<Type>,

    /// The generic declaration of the type.
    pub generic_declaration: GenericDeclaration,

    /// The type alias of this [`Type`].
    pub r#type: r#type::Type<Symbolic>,

    /// The span where the type is declared.
    pub span: Option<Span>,

    /// The name of the type.
    pub name: String,

    /// The accessibility of the type.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    pub parent_module_id: ID<Module>,
}

impl Global for Type {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Type(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Module(self.parent_module_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl Generic for Type {
    fn generic_id(&self) -> GenericID { GenericID::Type(self.id) }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

/// Represents a constant declaration, denoted by `const NAME: TYPE = VALUE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constant {
    /// The ID of the constant.
    pub id: ID<Constant>,

    /// The name of the constant.
    pub name: String,

    /// The type of the constant.
    pub r#type: r#type::Type<Symbolic>,

    /// The constant value of this declaration.
    pub constant: constant::Constant<Symbolic>,

    /// Location of where the constant is declared.
    pub span: Option<Span>,

    /// The accessibility of the constant.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    pub parent_module_id: ID<Module>,
}

impl Global for Constant {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Constant(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Module(self.parent_module_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents a function declaration as an implements member, denoted by
/// `function NAME(...) {...}` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    /// The ID of the function.
    pub id: ID<Function>,

    /// The parameters of the function.
    pub parameters: Map<Parameter<ID<ImplementationFunction>>>,

    /// The ID of the parent module.
    pub parent_module_id: ID<Module>,

    /// Location of where the function is declared.
    pub span: Option<Span>,

    /// The name of the function.
    pub name: String,

    /// The return type of the function.
    pub return_type: r#type::Type<Symbolic>,

    /// The accessibility of the function.
    pub accessibility: Accessibility,

    /// The generic declaration of the function.
    pub generic_declaration: GenericDeclaration,
}

impl Global for Function {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Function(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Module(self.parent_module_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl Generic for Function {
    fn generic_id(&self) -> GenericID { GenericID::Function(self.id) }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

/// Represents a signature of a trait implements
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplementationSignature {
    /// Generic arguments applied to the trait.
    pub arguments: GenericArguments<Symbolic>,

    /// The ID of the trait.
    pub trait_id: ID<Trait>,

    /// Location of where the implements is declared.
    pub span: Option<Span>,

    /// The generic declaration of this signature.
    pub generic_declaration: GenericDeclaration,

    /// The name of the trait that is being implemented.
    pub trait_name: String,

    /// The ID module where the implementation is declared.
    pub declared_in: ID<Module>,
}

/// Represents a negative trait implementation, denoted by
/// `implements<PARAM> TRAIT<PARAM> = delete;` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NegativeImplementation {
    /// The signature of the negative trait implementation.
    pub signature: ImplementationSignature,

    /// The ID of the negative trait implementation.
    pub id: ID<NegativeImplementation>,
}

impl Global for NegativeImplementation {
    fn name(&self) -> &str { &self.signature.trait_name }

    fn global_id(&self) -> GlobalID { GlobalID::NegativeImplementation(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(self.signature.declared_in.into()) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.signature.span.clone() }
}

impl Generic for NegativeImplementation {
    fn generic_id(&self) -> GenericID { GenericID::NegativeImplementation(self.id) }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.signature.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.signature.generic_declaration
    }
}

/// Represents a function declaration as an implements member, denoted by
/// `function NAME(...) {...}` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplementationFunction {
    /// The ID of the function.
    pub id: ID<ImplementationFunction>,

    /// The parameters of the function.
    pub parameters: Map<Parameter<ID<ImplementationFunction>>>,

    /// The ID of the parent implements.
    pub parent_implementation_id: ID<Implementation>,

    /// Location of where the function is declared.
    pub span: Option<Span>,

    /// The name of the function.
    pub name: String,

    /// The return type of the function.
    pub return_type: r#type::Type<Symbolic>,

    /// The generic declaration of the function.
    pub generic_declaration: GenericDeclaration,
}

impl Generic for ImplementationFunction {
    fn generic_id(&self) -> GenericID { GenericID::ImplementationFunction(self.id) }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for ImplementationFunction {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::ImplementationFunction(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Implementation(self.parent_implementation_id))
    }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents a type declaration as an implements member, denoted by `type NAME = TYPE;` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplementationType {
    /// The ID of the type.
    pub id: ID<ImplementationType>,

    /// The ID of the parent implements.
    pub parent_implementation_id: ID<Implementation>,

    /// The name of the type.
    pub name: String,

    /// The generic declaration of the type.
    pub generic_declaration: GenericDeclaration,

    /// The type of the type.
    pub r#type: r#type::Type<Symbolic>,

    /// Location of where the type is declared.
    pub span: Option<Span>,
}

impl Generic for ImplementationType {
    fn generic_id(&self) -> GenericID { GenericID::ImplementationType(self.id) }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for ImplementationType {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::ImplementationType(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Implementation(self.parent_implementation_id))
    }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents a constant declaration as an implements member, denoted by `const NAME: TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementationConstant {
    /// The ID of the constant.
    pub id: ID<ImplementationConstant>,

    /// The ID of the parent implements.
    pub parent_implementation_id: ID<Implementation>,

    /// The name of the constant.
    pub name: String,

    /// The type of the constant.
    pub r#type: r#type::Type<Symbolic>,

    /// The constant value of this declaration.
    pub constant: constant::Constant<Symbolic>,

    /// Location of where the constant is declared.
    pub span: Option<Span>,
}

impl Global for ImplementationConstant {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::ImplementationConstant(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Implementation(self.parent_implementation_id))
    }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// An enumeration containing all an ID to all kinds of symbols that can be defined in an
/// implements.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
#[allow(missing_docs)]
pub enum ImplementationMemberID {
    Type(ID<ImplementationType>),
    Function(ID<ImplementationFunction>),
    Constant(ID<ImplementationConstant>),
}

from_ids!(
    ImplementationMemberID,
    GlobalID,
    (Type, ImplementationType),
    (Function, ImplementationFunction),
    (Constant, ImplementationConstant)
);

/// Represents a trait implementation, denoted by `implements<PARAM> TRAIT<PARAM> { ... }` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Implementation {
    /// The ID of the trait implementation.
    pub id: ID<Implementation>,

    /// The signature of the trait implementation.
    pub signature: ImplementationSignature,

    /// Indicates whether the trait implementation is a constant implementation.
    pub is_const: bool,

    /// Maps the name of the trait member to its ID.
    pub implementation_member_ids_by_name: HashMap<String, ImplementationMemberID>,

    /// Maps the ID of the trait type to the ID of the implementation type.
    pub implementation_type_ids_by_trait_type_id: HashMap<ID<TraitType>, ID<ImplementationType>>,

    /// Maps the ID of the trait function to the ID of the implementation function.
    pub implementation_function_ids_by_trait_function_id:
        HashMap<ID<TraitFunction>, ID<ImplementationFunction>>,

    /// Maps the ID of the trait constant to the ID of the implementation constant.
    pub implementation_constant_ids_by_trait_constant_id:
        HashMap<ID<TraitConstant>, ID<ImplementationConstant>>,
}

impl Generic for Implementation {
    fn generic_id(&self) -> GenericID { GenericID::Implementation(self.id) }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.signature.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.signature.generic_declaration
    }
}

impl Global for Implementation {
    fn name(&self) -> &str { &self.signature.trait_name }

    fn global_id(&self) -> GlobalID { GlobalID::Implementation(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Trait(self.signature.trait_id))
    }

    fn get_member(&self, name: &str) -> Option<GlobalID> {
        self.implementation_member_ids_by_name
            .get(name)
            .copied()
            .map(|x| match x {
                ImplementationMemberID::Type(x) => GlobalID::ImplementationType(x),
                ImplementationMemberID::Function(x) => GlobalID::ImplementationFunction(x),
                ImplementationMemberID::Constant(x) => GlobalID::ImplementationConstant(x),
            })
    }

    fn span(&self) -> Option<Span> { self.signature.span.clone() }
}

/// Represents a function parameter in the function signature, denoted by `NAME: TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter<ParentID> {
    /// The pattern binding of the parameter.
    pub pattern: Irrefutable,

    /// The type of the parameter.
    pub r#type: r#type::Type<Symbolic>,

    /// The ID of the parent function.
    pub parent_id: ParentID,

    /// Location of where the parameter is declared.
    pub span: Option<Span>,
}

/// Represents a type declaration as a trait member, denoted by `type NAME;` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitType {
    /// The ID of the type.
    pub id: ID<TraitType>,

    /// The trait ID where the type is declared.
    pub parent_trait_id: ID<Trait>,

    /// The name of the type.
    pub name: String,

    /// The generic declaration of the type.
    pub generic_declaration: GenericDeclaration,

    /// The span where the type is declared.
    pub span: Option<Span>,
}

impl Generic for TraitType {
    fn generic_id(&self) -> GenericID { GenericID::TraitType(self.id) }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for TraitType {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::TraitType(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Trait(self.parent_trait_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents a function declaration as a trait member, denoted by `function NAME(...) {...}`
/// syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitFunction {
    /// The ID of the function.
    pub id: ID<TraitFunction>,

    /// The trait ID where the function is declared.
    pub parent_trait_id: ID<Trait>,

    /// The name of the function.
    pub name: String,

    /// The generic declaration of the function.
    pub generic_declaration: GenericDeclaration,

    /// The parameters of the function.
    pub parameters: Map<Parameter<ID<TraitFunction>>>,

    /// The return type of the function.
    pub return_type: r#type::Type<Symbolic>,

    /// The span where the function is declared.
    pub span: Option<Span>,
}

impl Generic for TraitFunction {
    fn generic_id(&self) -> GenericID { GenericID::TraitFunction(self.id) }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for TraitFunction {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::TraitFunction(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Trait(self.parent_trait_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents a constant declaration as a trait member, denoted by `const NAME: TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitConstant {
    /// The ID of the constant.xc
    pub id: ID<TraitConstant>,

    /// The trait ID where the constant is declared.
    pub parent_trait_id: ID<Trait>,

    /// The name of the constant.
    pub name: String,

    /// The type of the constant.
    pub r#type: r#type::Type<Symbolic>,

    /// The span where the constant is declared.
    pub span: Option<Span>,
}

impl Global for TraitConstant {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::TraitConstant(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Trait(self.parent_trait_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// An enumeration containing all an ID to all kinds of symbols that can be defined in a trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
#[allow(missing_docs)]
pub enum TraitMemberID {
    Type(ID<TraitType>),
    Function(ID<TraitFunction>),
    Constant(ID<TraitConstant>),
}

from_ids!(
    TraitMemberID,
    GlobalID,
    (Type, TraitType),
    (Function, TraitFunction),
    (Constant, TraitConstant)
);

/// Represents a trait declaration, denoted by `trait NAME { ... }` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trait {
    /// The ID of the trait.
    pub id: ID<Trait>,

    /// The name of the trait.
    pub name: String,

    /// The accessibility of the trait.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    pub parent_module_id: ID<Module>,

    /// The generic declaration of the trait.
    pub generic_declaration: GenericDeclaration,

    /// Contains all the negative trait implementation defined in the trait.
    pub negative_implementations: Vec<ID<NegativeImplementation>>,

    /// Contains all the trait implementation defined in the trait.
    pub implementations: Vec<ID<Implementation>>,

    /// Contains all the types defined in the trait.
    pub span: Option<Span>,

    /// Map the name of the trait member to its ID.
    pub trait_member_ids_by_name: HashMap<String, TraitMemberID>,
}

impl Generic for Trait {
    fn generic_id(&self) -> GenericID { GenericID::Trait(self.id) }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Trait {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Trait(self.id) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Module(self.parent_module_id)) }

    fn get_member(&self, name: &str) -> Option<GlobalID> {
        self.trait_member_ids_by_name
            .get(name)
            .copied()
            .map(|x| match x {
                TraitMemberID::Type(x) => GlobalID::TraitType(x),
                TraitMemberID::Function(x) => GlobalID::TraitFunction(x),
                TraitMemberID::Constant(x) => GlobalID::TraitConstant(x),
            })
    }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Enumeration of all kinds of generic parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LocalGenericParameterID {
    Lifetime(ID<LifetimeParameter>),
    Type(ID<TypeParameter>),
    Constant(ID<ConstantParameter>),
}

/// Enumeration of either positive or negative implementation.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From,
)]
#[allow(missing_docs)]
pub enum ImplementationKindID {
    Positive(ID<Implementation>),
    Negative(ID<NegativeImplementation>),
}

from_ids!(
    ImplementationKindID,
    GlobalID,
    (Positive, Implementation),
    (Negative, NegativeImplementation)
);

from_ids!(
    ImplementationKindID,
    GenericID,
    (Positive, Implementation),
    (Negative, NegativeImplementation)
);

/// Enumeration of all kinds of generic parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum GenericKind {
    Type,
    Lifetime,
    Constant,
}
