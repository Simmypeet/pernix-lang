//! Contains the definition of the high-level intermediate representation (HIR) of the compiler.

use std::{collections::HashMap, fmt::Debug, hash::Hash, sync::Arc};

use derive_more::{Deref, From};
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_lexical::token::Identifier as IdentifierToken;
use pernixc_source::Span;
use pernixc_system::{
    arena::{Arena, InvalidIDError},
    create_symbol,
};
use thiserror::Error;

use self::{
    binder::{Binder, IntermediateTypeID},
    error::Error as HirError,
    instruction::Backend,
    value::binding::Binding,
};
use crate::{
    cfg::ControlFlowGraph,
    infer::{InferableType, InferenceContext},
    symbol::{
        error::Error as SymbolError, table::Table, ty::Type, Global, OverloadID, OverloadSetID,
        ScopedID,
    },
};

pub mod binder;
pub mod error;
pub mod instruction;
pub mod value;

/// Represents a high-level intermediate representation (HIR) of the compiler.
///
/// HIR is layout in the way that comforms to the *Static Single Assignment* (SSA) form, if you've
/// worked with LLVM IR before, you'll find it familiar. This allows the semantic analyzer to
/// perform complex data flow analysis on the program such as dead code detection, checking return
/// paths, etc.
#[derive(Debug, Deref)]
pub struct Hir {
    container: Container<Type>,
}

/// Is an error occured during the binding process in [`Hir::bind()`].
#[derive(Debug, EnumAsInner, From, Error)]
#[allow(missing_docs)]
pub enum BindError {
    #[error("Invalid `overload_id` given")]
    InvalidIDError(InvalidIDError),

    #[error(
        "Encountered a semantic error during the binding process and the program doesn't fully \
         conform to the language specification"
    )]
    Suboptimal(Box<Suboptimal>),

    #[error("Encountered a fatal semantic error that completely halts the binding process")]
    FataSemanticError,
}

/// Represents a result returned from the [`Target::bind()`] function where the binding process
/// encounters a semantic error and the IR might not fully represent the program.
#[derive(Debug, Getters)]
pub struct FaultTarget {
    /// The symbol table that was used in the binding process.
    #[get = "pub"]
    table: Arc<Table>,

    /// Maps the overload ID to its binding result.
    #[get = "pub"]
    results_by_overload_id: HashMap<OverloadID, Result<Hir, BindError>>,
}

/// Represents a collection of [`Hir`]s of all the overloads defined in the program.
///
/// This is the final output of intermediate representation (IR) of the compiler and will be used
/// in the code generation phase.
#[derive(Debug, Getters)]
pub struct Target {
    /// The symbol table that was used in the binding process.
    #[get = "pub"]
    table: Arc<Table>,

    /// Maps the overload ID to the [`Hir`] of the overloads.
    #[get = "pub"]
    hirs_by_overload_id: HashMap<OverloadID, Hir>,
}

impl Target {
    /// Binds all the overloads in the given table into a [`Target`].
    ///
    /// # Errors
    /// Returns [`FaultTarget`] if the binding process encounters any semantic errors.
    pub fn bind(table: Arc<Table>, handler: &impl ErrorHandler) -> Result<Self, FaultTarget> {
        let results = std::thread::scope(|scope| {
            let mut results = Vec::new();

            // bind each overload
            for overload_id in table.overloads().map(pernixc_system::arena::Symbol::id) {
                let table = table.clone();

                results.push(
                    scope.spawn(move || (overload_id, Hir::bind(&table, overload_id, handler))),
                );
            }

            results
                .into_iter()
                .map(|x| x.join().unwrap())
                .collect::<Vec<_>>()
        });

        if results.iter().any(|x| x.1.is_err()) {
            Err(FaultTarget {
                table,
                results_by_overload_id: results.into_iter().collect(),
            })
        } else {
            Ok(Self {
                table,
                hirs_by_overload_id: results.into_iter().map(|(k, v)| (k, v.unwrap())).collect(),
            })
        }
    }
}

impl Hir {
    /// Binds the given overload into the [`Hir`].
    ///
    /// # Parameters
    /// - `table`: The symbol table to bind the overload from.
    /// - `overload_id`: The overload to bind.
    /// - `handler`: The error handler to handle any semantic errors that may occur during the
    ///   binding process.
    ///
    /// # Errors
    /// - [`BindError::InvalidIDError`]: If the given overload ID is invalid.
    /// - [`BindError::Suboptimal`]: If the program has a semantic error but still able to be
    ///   recovered hence producing a suboptimal program.
    /// - [`BindError::FatalSemanticError`]: If the program has a semantic error that completely
    ///   halts the binding process.
    pub fn bind(
        table: &Arc<Table>,
        overload_id: OverloadID,
        handler: &impl ErrorHandler,
    ) -> Result<Self, BindError> {
        let mut binder = Binder::new(table.clone(), overload_id)?;

        for statement in table
            .get_overload(overload_id)
            .unwrap()
            .syntax_tree()
            .block_without_label
            .statements()
        {
            binder
                .bind_statement(statement, handler)
                .map_err(|_| BindError::FataSemanticError)?;
        }

        Ok(binder.build(handler)?)
    }
}

/// Represents an [`Hir`] that contains suboptimal semantic information.
#[derive(Debug, Getters)]
pub struct Suboptimal {
    /// The container that holds the contents of the [`Hir`].
    #[get = "pub"]
    container: Container<IntermediateTypeID>,

    /// The inference context that holds the type information of the [`Hir`].
    #[get = "pub"]
    inference_context: InferenceContext,
}

/// The container that holds the contents of the [`Hir`].
///
/// This container is as well used in the [`binder::Binder`] to construct the [`Hir`].
#[derive(Debug, Getters, CopyGetters)]
pub struct Container<T: TypeSystem> {
    /// The control flow graph that represents the flow of the function.
    #[get = "pub"]
    control_flow_graph: ControlFlowGraph<Backend<T>>,

    /// The list of all the registers that are used in the function.
    #[get = "pub"]
    registers: Arena<Register<T>>,

    /// The list of all the allocas that are used in the function.
    #[get = "pub"]
    allocas: Arena<Alloca<T>>,

    /// The table that is used in lookup and resolution of the symbols.
    #[get = "pub"]
    table: Arc<Table>,

    /// The overload that this container represents.
    #[get_copy = "pub"]
    overload_id: OverloadID,

    /// The parent overload set that the [`Self::overload_id`] is defined in.
    #[get_copy = "pub"]
    parent_overload_set_id: OverloadSetID,

    /// The parent module that the [`Self::parent_overload_set_id`] is defined in.
    #[get_copy = "pub"]
    parent_scoped_id: ScopedID,

    /// The scope tree that represents how the scopes are nested in the function.
    #[get = "pub"]
    scope_tree: ScopeTree,
}

impl<T: TypeSystem> Container<T> {
    /// Constructs a new empty container with ID validation.
    ///
    /// # Errors
    /// If the given [`OverloadID`] is invalid for the given [`Table`].
    pub fn new(table: Arc<Table>, overload_id: OverloadID) -> Result<Self, InvalidIDError> {
        let overload = table
            .get_overload(overload_id)
            .map_err(|_| InvalidIDError)?;
        let overload_set = table
            .get_overload_set(overload.parent_overload_set_id())
            .map_err(|_| InvalidIDError)?;

        Ok(Self {
            control_flow_graph: ControlFlowGraph::new(),
            overload_id,
            parent_overload_set_id: overload_set.id(),
            parent_scoped_id: overload_set.parent_scoped_id().unwrap(),
            table,
            registers: Arena::new(),
            allocas: Arena::new(),
            scope_tree: ScopeTree::new(),
        })
    }
}

/// Is a trait implemented by [`Container`] that allows to inspect various properties of the
/// values.
pub trait ValueInspect<T: TypeSystem, V> {
    /// Returns the type of the given value.
    ///
    /// # Errors
    /// - If the given value wasn't created by the same [`Container`] as this one.
    fn get_type(&self, value: &V) -> Result<T, InvalidValueError>;

    /// Returns the span of the given value.
    ///
    /// # Errors
    /// - If the given value wasn't created by the same [`Container`] as this one.
    fn get_span(&self, value: &V) -> Result<Span, InvalidValueError>;
}

/// Is an enumeration of semantic errors that can occur during HIR construction.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum AllHirError {
    HirError(HirError),
    SymbolError(SymbolError),
}

/// Is a derived [`pernixc_system::error_handler::ErrorHandler`] trait that can handle both
/// [`HirError`] and [`SymbolError`] (the two types of errors that can occur while building the
/// HIR)
pub trait ErrorHandler:
    pernixc_system::error_handler::ErrorHandler<HirError>
    + pernixc_system::error_handler::ErrorHandler<SymbolError>
{
}

impl<
        T: pernixc_system::error_handler::ErrorHandler<HirError>
            + pernixc_system::error_handler::ErrorHandler<SymbolError>,
    > ErrorHandler for T
{
}

/// Represents that is used for type checking.
pub trait TypeSystem: Debug + Clone + Copy + PartialEq + Eq + PartialOrd + Ord + Hash {
    /// Creates an instance of [`Self`] from a concrete [`Type`].
    fn from_type(ty: Type) -> Self;
}

impl TypeSystem for Type {
    fn from_type(ty: Type) -> Self { ty }
}

impl TypeSystem for InferableType {
    fn from_type(ty: Type) -> Self { Self::Type(ty) }
}

/// Occurs when a value is used in a context where it wasn't created from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("The value is used in a context where it wasn't created from.")]
pub struct InvalidValueError;

create_symbol! {
    /// Represents a single distinct register assignment in the SSA form.
    #[derive(Debug, Clone, PartialEq, Eq, Getters)]
    pub struct Register<T: TypeSystem> {
        /// The binding value of the register.
        #[get = "pub"]
        binding: Binding<T>
    }
}

impl<T: TypeSystem> ValueInspect<T, RegisterID> for Container<T> {
    fn get_type(&self, value: &RegisterID) -> Result<T, InvalidValueError> {
        let register = self.registers.get(*value).map_err(|_| InvalidValueError)?;
        self.get_type(&register.binding)
    }

    fn get_span(&self, value: &RegisterID) -> Result<Span, InvalidValueError> {
        let register = self.registers.get(*value).map_err(|_| InvalidValueError)?;
        self.get_span(&register.binding)
    }
}

create_symbol! {
    /// Is a stack allocated memory that is used for variable declaration statements.
    #[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
    pub struct Alloca<T: TypeSystem> {
        /// The identifier token of the variable.
        #[get = "pub"]
        identifier_token: IdentifierToken,

        /// Specifies whether the variable was declared as mutable.
        #[get_copy = "pub"]
        is_mutable: bool,

        /// The type of the variable.
        #[get_copy = "pub"]
        ty: T,

        /// The scope in which the variable was declared.
        #[get_copy = "pub"]
        scope_id: ScopeID,

        /// The order in which the variable was declared (in the scope, starting from 0).
        #[get_copy = "pub"]
        declaration_order: usize,
    }
}

/// Is an enumeration of either a [`ScopeID`] or [`BranchID`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ScopeChildID {
    ScopeID(ScopeID),
    BranchID(BranchID),
}

create_symbol! {
    /// Represents a scope
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters, CopyGetters)]
    pub struct Scope {
        /// The ID of the parent scope.
        #[get_copy = "pub"]
        parent_scope: Option<ScopeID>,

        /// The ID of the branch that this scope participates in (if any).
        #[get_copy = "pub"]
        branch: Option<BranchID>,

        /// The list of child scopes/branches in this scope. (The order of the children is the same
        /// order appearing in the source code.)
        #[get = "pub"]
        children: Vec<ScopeChildID>,

        /// The scope depth of this scope. The depth of the root scope is 0.
        #[get_copy = "pub"]
        depth: usize,
    }
}

create_symbol! {
    /// Represents a branch that was created by conditional expressions (if, match, etc.)
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters, CopyGetters)]
    pub struct Branch {
        /// The ID of the scope where this branch was created.
        #[get_copy = "pub"]
        parent_scope: ScopeID,

        /// The list of scopes that are in this branch.
        #[get = "pub"]
        scopes: Vec<ScopeID>,
    }
}

/// Represents a scope tree.
#[derive(Debug, CopyGetters, Getters)]
pub struct ScopeTree {
    /// Contains all the scopes in the tree.
    #[get = "pub"]
    scopes: Arena<Scope>,

    /// Contains all the branches in the tree.
    #[get = "pub"]
    branches: Arena<Branch>,

    /// The ID of the root scope.
    #[get_copy = "pub"]
    root_scope: ScopeID,
}

impl ScopeTree {
    /// Creates a new scope tree with a root scope.
    #[must_use]
    pub fn new() -> Self {
        let mut scopes = Arena::new();
        let root_scope = scopes.insert(Scope {
            parent_scope: None,
            branch: None,
            children: Vec::new(),
            depth: 0,
        });

        Self {
            scopes,
            branches: Arena::new(),
            root_scope,
        }
    }
}

impl Default for ScopeTree {
    fn default() -> Self { Self::new() }
}
