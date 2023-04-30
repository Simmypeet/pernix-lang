//! Contains the definition of the high-level intermediate representation (HIR) of the compiler.

use std::{fmt::Debug, hash::Hash, sync::Arc};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_lexical::token::Identifier as IdentifierToken;
use pernixc_system::{
    arena::{Arena, InvalidIDError},
    create_symbol,
    error_handler::ErrorHandler,
};

use self::{builder::Builder, error::BindingError, instruction::Backend, value::binding::Binding};
use crate::{
    cfg::ControlFlowGraph,
    symbol::{error::SymbolError, table::Table, ty::Type, OverloadID},
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
    control_flow_graph: ControlFlowGraph<Backend<Type>>,
    registers: Arena<Register<Type>>,
    allocas: Arena<Alloca<Type>>,
    table: Arc<Table>,
    overload_id: OverloadID,

    /// Returns true if the HIR is in a suboptimal state.
    ///
    /// This is used to prevent the compiler from generating code for a program that is not
    /// semantically correct.
    #[get_copy = "pub"]
    is_suboptimal: bool,
}

impl Hir {
    /// Binds the given overload to the HIR.
    ///
    /// # Errors
    /// - If the overload ID is invalid.
    pub fn bind(
        table: Arc<Table>,
        overload_id: OverloadID,
        error_handler: &impl BindingErrorHandler,
    ) -> Result<Self, InvalidIDError> {
        let mut builder = Builder::new(table, overload_id)?;

        todo!("Binds all the statements in the overload into instructions and registers.");

        Ok(builder.build(error_handler))
    }
}

/// Is a derived [`ErrorHandler`] trait that can handle both [`BindingError`] and [`SymbolError`]
/// (the two types of errors that can occur while building the HIR)
pub trait BindingErrorHandler: ErrorHandler<BindingError> + ErrorHandler<SymbolError> {}

impl<T: ErrorHandler<BindingError> + ErrorHandler<SymbolError>> BindingErrorHandler for T {}

/// Represents that is used for type checking.
pub trait TypeSystem: Debug + Clone + Copy + PartialEq + Eq + PartialOrd + Ord + Hash {
    /// Creates an instance of [`Self`] from a concrete [`Type`].
    fn from_type(ty: Type) -> Self;
}

impl TypeSystem for Type {
    fn from_type(ty: Type) -> Self { ty }
}

create_symbol! {
    /// Represents a single distinct register assignment in the SSA form.
    #[derive(Debug, Clone, PartialEq, Eq, Getters)]
    pub struct Register<T: TypeSystem> {
        /// The binding value of the register.
        #[get = "pub"]
        binding: Binding<T>,
    }
}

/// Is a stack allocated memory that is used for variable declaration statements.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct UserAlloca<T: TypeSystem> {
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

/// Is a stack allocated memory defined implicitly by the compiler for storing intermediate values.
#[derive(Debug, Clone, PartialEq, Eq, Hash, CopyGetters)]
pub struct ImplicitAlloca<T: TypeSystem> {
    /// The type of the variable.
    #[get_copy = "pub"]
    ty: T,
}

create_symbol! {
    /// Represents a single distinct stack allocated memory in the SSA form.
    #[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
    #[allow(missing_docs)]
    pub enum Alloca<T: TypeSystem> {
        UserAlloca(UserAlloca<T>),
        ImplicitAlloca(ImplicitAlloca<T>),
    }
}
