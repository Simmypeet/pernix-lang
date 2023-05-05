//! Contains the definition of the high-level intermediate representation (HIR) of the compiler.

use std::{fmt::Debug, hash::Hash, sync::Arc};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_lexical::token::Identifier as IdentifierToken;
use pernixc_source::Span;
use pernixc_system::{
    arena::{Arena, InvalidIDError},
    create_symbol,
};
use thiserror::Error;

use self::{error::Error as HirError, instruction::Backend, value::binding::Binding};
use crate::{
    cfg::ControlFlowGraph,
    infer::InferableType,
    symbol::{
        error::Error as SymbolError, table::Table, ty::Type, Global, OverloadID, OverloadSetID,
        ScopedID,
    },
};

pub mod builder;
pub mod error;
pub mod instruction;
pub mod value;

/// Represents a high-level intermediate representation (HIR) of the compiler.
///
/// HIR is layout in the way that comforms to the *Static Single Assignment* (SSA) form, if you've
/// worked with LLVM IR before, you'll find it familiar. This allows the semantic analyzer to
/// perform complex data flow analysis on the program such as dead code detection, checking return
/// paths, etc.
#[derive(Debug, CopyGetters)]
pub struct Hir {
    #[allow(dead_code)]
    container: Container<Type>,

    /// Returns true if the HIR is in a suboptimal state.
    ///
    /// This is used to prevent the compiler from generating code for a program that is not
    /// semantically correct.
    #[get_copy = "pub"]
    is_suboptimal: bool,
}

/// The container that holds the contents of the [`Hir`].
///
/// This container is as well used in the [`builder::Builder`] to construct the [`Hir`].
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
    }
}
