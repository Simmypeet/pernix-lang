//! Contains the definition of all symbol kinds in the language.

use std::{collections::HashMap};

use derive_more::{From, Index, IndexMut};
use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;

use crate::{
    arena::{Arena, Item, Map, ID},
    entity::{constant, pattern::Irrefutable, predicate, r#type, GenericArguments, Model, Never},
};

/// Represents an accessibility of a symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Accessibility {
    /// The symbol is accessible from anywhere.
    Public,

    /// The symbol is accessible only from the same module and its submodules.
    Private,

    /// The symbol is accessible only from the same target.
    Internal,
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
    TraitType(TraitTypeID),
    TraitFunction(TraitFunctionID),
    Implementation(ImplementationID),
    ImplementationFunction(ImplementationFunctionID),
    ImplementationType(ImplementationTypeID),
}

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
#[derive(Debug, Clone, PartialEq, Eq)]
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
    Variant(VariantID),
    TraitType(TraitTypeID),
    TraitFunction(TraitFunctionID),
    TraitConstant(TraitConstantID),
    Implements(ImplementationID),
    ImplementsFunction(ImplementationFunctionID),
    ImplementsType(ImplementationTypeID),
    ImplementsConstant(ImplementsConstantID),
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum ModuleMemberID {
    Module(ID<Module>),
    Enum(ID<Enum>),
    Struct(ID<Struct>),
    Trait(ID<Trait>),
    Type(ID<Type>),
}

impl From<ModuleMemberID> for GlobalID {
    fn from(id: ModuleMemberID) -> Self {
        match id {
            ModuleMemberID::Module(id) => Self::Module(id),
            ModuleMemberID::Enum(id) => Self::Enum(id),
            ModuleMemberID::Struct(id) => Self::Struct(id),
            ModuleMemberID::Trait(id) => Self::Trait(id),
            ModuleMemberID::Type(id) => Self::Type(id),
        }
    }
}

/// Represents a module declaration.
#[derive(Debug, Clone, PartialEq, Eq, Index, IndexMut)]
pub struct Module {
    /// The name of the module declaration.
    pub name: String,

    /// The accessibility of the module.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    ///
    /// If this module is a root module, then this field is `None`.
    pub parent_module_id: Option<ID<Module>>,

    /// Maps the name of the module child to its ID.
    #[index]
    #[index_mut]
    pub module_child_ids_by_name: HashMap<String, ModuleMemberID>,

    /// Location of where the module is declared.
    pub span: Option<Span>,
}

impl Global for Item<Module> {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Module(self.id()) }

    fn parent_global_id(&self) -> Option<GlobalID> { self.parent_module_id.map(GlobalID::Module) }

    fn get_member(&self, name: &str) -> Option<GlobalID> {
        self.module_child_ids_by_name
            .get(name)
            .copied()
            .map(Into::into)
    }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Describes when a lifetime parameter is bounded.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BoundKind {
    /// The lifetime can be later bounded at the use site.
    Late,

    /// The lifetime must be known immediately at resolution.   
    Early,
}

/// Represents a lifetime parameter, denoted by `'a` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeParameter {
    /// The name of the lifetime parameter.
    pub name: String,

    /// The kind of the lifetime parameter.
    pub bound_kind: BoundKind,

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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericParameters {
    /// List of defined lifetime parameters.
    pub lifetimes: Map<LifetimeParameter>,

    /// List of defined type parameters.
    pub types: Map<TypeParameter>,

    /// List of defined constant parameters.
    pub constants: Map<ConstantParameter>,
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

impl Generic for Item<Struct> {
    fn generic_id(&self) -> GenericID { GenericID::Struct(self.id()) }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Item<Struct> {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Struct(self.id()) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Module(self.parent_module_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents an enum variant declaration, denoted by `NAME(ASSOC_TYPE)` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    /// The name of the variant.
    pub name: String,

    /// The type of the associated value of the variant (if any).
    pub associated_type: Option<r#type::Type<Symbolic>>,

    /// The parent enum ID.
    pub parent_enum_id: ID<Enum>,

    /// The span where the variant is declared.
    pub span: Option<Span>,
}

/// An ID to an enum variant.
pub type VariantID = MemberID<ID<Variant>, ID<Enum>>;

impl Global for Item<Variant> {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID {
        GlobalID::Variant(VariantID {
            parent: self.parent_enum_id,
            id: self.id(),
        })
    }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Enum(self.parent_enum_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents an enum declaration, denoted by `enum NAME { ... }` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    /// The name of the enum.
    pub name: String,

    /// The accessibility of the enum.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    pub parent_module_id: ID<Module>,

    /// The generic declaration of the enum.
    pub generic_declaration: GenericDeclaration,

    /// Contains all the variants defined in the enum.
    pub variants: Map<Variant>,

    /// Location of where the enum is declared.
    pub span: Option<Span>,
}

impl Generic for Item<Enum> {
    fn generic_id(&self) -> GenericID { GenericID::Enum(self.id()) }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Item<Enum> {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Enum(self.id()) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Module(self.parent_module_id)) }

    fn get_member(&self, name: &str) -> Option<GlobalID> {
        self.variants.get_id(name).map(|x| {
            GlobalID::Variant(VariantID {
                parent: self.id(),
                id: x,
            })
        })
    }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents a type declaration, denoted by `type NAME = TYPE;` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
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

impl Global for Item<Type> {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Type(self.id()) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Module(self.parent_module_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

impl Generic for Item<Type> {
    fn generic_id(&self) -> GenericID { GenericID::Type(self.id()) }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

/// Represents a constant declaration, denoted by `const NAME: TYPE = VALUE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constant {
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

impl Global for Item<Constant> {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Constant(self.id()) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Module(self.parent_module_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents a function declaration as an implements member, denoted by
/// `function NAME(...) {...}` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
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

    /// The generic declaration of the function.
    pub generic_declaration: GenericDeclaration,
}

impl Global for Item<Function> {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Function(self.id()) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Module(self.parent_module_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
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
}

/// Represents a negative trait implementation, denoted by
/// `implements<PARAM> TRAIT<PARAM> = delete;` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NegativeImplementation {
    /// The signature of the negative trait implementation.
    pub signature: ImplementationSignature,
}

/// Represents a function declaration as an implements member, denoted by
/// `function NAME(...) {...}` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplementationFunction {
    /// The parameters of the function.
    pub parameters: Map<Parameter<ID<ImplementationFunction>>>,

    /// The ID of the parent implements.
    pub parent_implements_id: ImplementationID,

    /// Location of where the function is declared.
    pub span: Option<Span>,

    /// The name of the function.
    pub name: String,

    /// The return type of the function.
    pub return_type: r#type::Type<Symbolic>,

    /// The generic declaration of the function.
    pub generic_declaration: GenericDeclaration,
}

/// An ID to an implements function.
pub type ImplementationFunctionID = MemberID<ID<ImplementationFunction>, ImplementationID>;

impl Generic for Item<ImplementationFunction> {
    fn generic_id(&self) -> GenericID {
        GenericID::ImplementationFunction(ImplementationFunctionID {
            parent: self.parent_implements_id,
            id: self.id(),
        })
    }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Item<ImplementationFunction> {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID {
        GlobalID::ImplementsFunction(ImplementationFunctionID {
            parent: self.parent_implements_id,
            id: self.id(),
        })
    }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Implements(self.parent_implements_id))
    }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents a type declaration as an implements member, denoted by `type NAME = TYPE;` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplementationType {
    /// The ID of the parent implements.
    pub parent_implements_id: ImplementationID,

    /// The name of the type.
    pub name: String,

    /// The generic declaration of the type.
    pub generic_declaration: GenericDeclaration,

    /// The type of the type.
    pub r#type: r#type::Type<Symbolic>,

    /// Location of where the type is declared.
    pub span: Option<Span>,
}

/// An ID to an implements type.
pub type ImplementationTypeID = MemberID<ID<ImplementationType>, ImplementationID>;

impl Generic for Item<ImplementationType> {
    fn generic_id(&self) -> GenericID {
        GenericID::ImplementationType(ImplementationTypeID {
            parent: self.parent_implements_id,
            id: self.id(),
        })
    }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Item<ImplementationType> {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID {
        GlobalID::ImplementsType(ImplementationTypeID {
            parent: self.parent_implements_id,
            id: self.id(),
        })
    }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Implements(self.parent_implements_id))
    }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents a constant declaration as an implements member, denoted by `const NAME: TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementationConstant {
    /// The ID of the parent implements.
    pub parent_implements_id: ImplementationID,

    /// The name of the constant.
    pub name: String,

    /// The type of the constant.
    pub r#type: r#type::Type<Symbolic>,

    /// The constant value of this declaration.
    pub constant: constant::Constant<Symbolic>,

    /// Location of where the constant is declared.
    pub span: Option<Span>,
}

/// An ID to an implements constant.
type ImplementsConstantID = MemberID<ID<ImplementationConstant>, ImplementationID>;

impl Global for Item<ImplementationConstant> {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID {
        GlobalID::ImplementsConstant(ImplementsConstantID {
            parent: self.parent_implements_id,
            id: self.id(),
        })
    }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Implements(self.parent_implements_id))
    }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// An enumeration containing all an ID to all kinds of symbols that can be defined in an
/// implements.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LocalImplementationID {
    Type(ID<ImplementationType>),
    Function(ID<ImplementationFunction>),
    Constant(ID<ImplementationConstant>),
}

/// Represents a trait implementation, denoted by `implements<PARAM> TRAIT<PARAM> { ... }` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Implementation {
    /// The signature of the trait implementation.
    pub signature: ImplementationSignature,

    /// Maps the name of the trait member to its ID.
    pub local_implements_ids_by_name: HashMap<String, LocalImplementationID>,

    /// Contains all the types declared in the implements.
    ///
    /// The key is the ID of the trait type that is being implemented by this implements.
    pub types: Map<ImplementationType, ID<TraitType>>,

    /// Contains all the functions declared in the implements.
    ///
    /// The key is the ID of the trait function that is being implemented by this implements.
    pub functions: Map<ImplementationFunction, ID<TraitFunction>>,

    /// Contains all the constants declared in the implements.
    ///
    /// The key is the ID of the trait constant that is being implemented by this implements.
    pub constants: Map<ImplementationConstant, ID<TraitConstant>>,
}

/// An ID to a trait implementation.
pub type ImplementationID = MemberID<ID<Implementation>, ID<Trait>>;

impl Generic for Item<Implementation> {
    fn generic_id(&self) -> GenericID {
        GenericID::Implementation(ImplementationID {
            parent: self.signature.trait_id,
            id: self.id(),
        })
    }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.signature.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.signature.generic_declaration
    }
}

impl Global for Item<Implementation> {
    fn name(&self) -> &str { &self.signature.trait_name }

    fn global_id(&self) -> GlobalID {
        GlobalID::Implements(ImplementationID {
            parent: self.signature.trait_id,
            id: self.id(),
        })
    }

    fn parent_global_id(&self) -> Option<GlobalID> {
        Some(GlobalID::Trait(self.signature.trait_id))
    }

    fn get_member(&self, name: &str) -> Option<GlobalID> {
        let implements_id = ImplementationID {
            parent: self.signature.trait_id,
            id: self.id(),
        };

        self.local_implements_ids_by_name
            .get(name)
            .copied()
            .map(|x| match x {
                LocalImplementationID::Type(x) => GlobalID::ImplementsType(ImplementationTypeID {
                    parent: implements_id,
                    id: x,
                }),
                LocalImplementationID::Function(x) => {
                    GlobalID::ImplementsFunction(ImplementationFunctionID {
                        parent: implements_id,
                        id: x,
                    })
                }
                LocalImplementationID::Constant(x) => {
                    GlobalID::ImplementsConstant(ImplementsConstantID {
                        parent: implements_id,
                        id: x,
                    })
                }
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
    /// The trait ID where the type is declared.
    pub parent_trait_id: ID<Trait>,

    /// The name of the type.
    pub name: String,

    /// The generic declaration of the type.
    pub generic_declaration: GenericDeclaration,

    /// The span where the type is declared.
    pub span: Option<Span>,
}

/// An ID to a trait type.
pub type TraitTypeID = MemberID<ID<TraitType>, ID<Trait>>;

impl Generic for Item<TraitType> {
    fn generic_id(&self) -> GenericID {
        GenericID::TraitType(TraitTypeID {
            parent: self.parent_trait_id,
            id: self.id(),
        })
    }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Item<TraitType> {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID {
        GlobalID::TraitType(TraitTypeID {
            parent: self.parent_trait_id,
            id: self.id(),
        })
    }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Trait(self.parent_trait_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents a function declaration as a trait member, denoted by `function NAME(...) {...}`
/// syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitFunction {
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

/// An ID to a trait function.
pub type TraitFunctionID = MemberID<ID<TraitFunction>, ID<Trait>>;

impl Generic for Item<TraitFunction> {
    fn generic_id(&self) -> GenericID {
        GenericID::TraitFunction(TraitFunctionID {
            parent: self.parent_trait_id,
            id: self.id(),
        })
    }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Item<TraitFunction> {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID {
        GlobalID::TraitFunction(TraitFunctionID {
            parent: self.parent_trait_id,
            id: self.id(),
        })
    }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Trait(self.parent_trait_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// Represents a constant declaration as a trait member, denoted by `const NAME: TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitConstant {
    /// The trait ID where the constant is declared.
    pub parent_trait_id: ID<Trait>,

    /// The name of the constant.
    pub name: String,

    /// The type of the constant.
    pub r#type: r#type::Type<Symbolic>,

    /// The span where the constant is declared.
    pub span: Option<Span>,
}

/// An ID to a trait constant.
pub type TraitConstantID = MemberID<ID<TraitConstant>, ID<Trait>>;

impl Global for Item<TraitConstant> {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID {
        GlobalID::TraitConstant(TraitConstantID {
            parent: self.parent_trait_id,
            id: self.id(),
        })
    }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Trait(self.parent_trait_id)) }

    fn get_member(&self, _: &str) -> Option<GlobalID> { None }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// An enumeration containing all an ID to all kinds of symbols that can be defined in a trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LocalTraitMemberID {
    Type(ID<TraitType>),
    Function(ID<TraitFunction>),
    Constant(ID<TraitConstant>),
}

/// Represents a trait declaration, denoted by `trait NAME { ... }` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trait {
    /// The name of the trait.
    pub name: String,

    /// The accessibility of the trait.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    pub parent_module_id: ID<Module>,

    /// The generic declaration of the trait.
    pub generic_declaration: GenericDeclaration,

    /// Contains all the negative trait implementation defined in the trait.
    pub negative_implements: Arena<NegativeImplementation>,

    /// Contains all the trait implementation defined in the trait.
    pub implementations: Arena<Implementation>,

    /// Contains all the types defined in the trait.
    pub span: Option<Span>,

    /// Map the name of the trait member to its ID.
    pub local_trait_member_ids_by_name: HashMap<String, LocalTraitMemberID>,

    /// Contains all the functions defined in the trait.
    pub functions: Arena<TraitFunction>,

    /// Contains all the constants defined in the trait.
    pub constants: Arena<TraitConstant>,

    /// Contains all the types defined in the trait.
    pub types: Arena<TraitType>,
}

impl Generic for Item<Trait> {
    fn generic_id(&self) -> GenericID { GenericID::Trait(self.id()) }

    fn generic_declaration(&self) -> &GenericDeclaration { &self.generic_declaration }

    fn generic_declaration_mut(&mut self) -> &mut GenericDeclaration {
        &mut self.generic_declaration
    }
}

impl Global for Item<Trait> {
    fn name(&self) -> &str { &self.name }

    fn global_id(&self) -> GlobalID { GlobalID::Trait(self.id()) }

    fn parent_global_id(&self) -> Option<GlobalID> { Some(GlobalID::Module(self.parent_module_id)) }

    fn get_member(&self, name: &str) -> Option<GlobalID> {
        self.local_trait_member_ids_by_name
            .get(name)
            .copied()
            .map(|x| match x {
                LocalTraitMemberID::Type(x) => GlobalID::TraitType(TraitTypeID {
                    parent: self.id(),
                    id: x,
                }),
                LocalTraitMemberID::Function(x) => GlobalID::TraitFunction(TraitFunctionID {
                    parent: self.id(),
                    id: x,
                }),
                LocalTraitMemberID::Constant(x) => GlobalID::TraitConstant(TraitConstantID {
                    parent: self.id(),
                    id: x,
                }),
            })
    }

    fn span(&self) -> Option<Span> { self.span.clone() }
}

/// A struct that implements [`System`] which describes the system of the symbolic model.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Symbolic;

impl Model for Symbolic {
    type ConstantInference = Never;
    // no extra context for region only 'static and lifetime parameter.
    type RegionContext = Never;
    type TypeInference = Never;
}
