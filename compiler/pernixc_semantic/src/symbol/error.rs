//! Contains the errors that can occur during the symbol resolution/construction phase of the
//! compiler.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_common::{printing::LogSeverity, source_file::Span};
use pernixc_system::arena::InvalidIDError;

use super::{
    table::Table, Accessibility, EnumVariantID, FieldID, GlobalID, OverloadID, ParameterID, ID,
};

/// Is an error that can occur during the symbol resolution/construction phase of the compiler.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs, clippy::module_name_repetitions)]
pub enum SymbolError {
    SymbolNotFound(SymbolNotFound),
    SymbolNotAccessible(SymbolNotAccessible),
    PrivateSymbolLeak(PrivateSymbolLeak),
    ParameterRedefinition(ParameterRedefinition),
    StructMemberMoreAccessibleThanStruct(StructMemberMoreAccessibleThanStruct),
    FieldRedefinition(FieldRedefinition),
    EnumVariantRedefinition(EnumVariantRedefinition),
    SymbolRedifinition(SymbolRedifinition),
    CircularDependency(CircularDependency),
    TypeExpected(TypeExpected),
    OverloadRedefinition(OverloadRedefinition),
}

/// Expected a symbol that can be used as a type, but found a symbol that cannot be used as a type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct TypeExpected {
    /// Specifies the location where the resolution failed.
    pub(super) span: Span,

    /// The ID of the found symbol that cannot be used as a type.
    pub(super) found: GlobalID,
}

impl TypeExpected {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        pernixc_common::printing::log(
            LogSeverity::Error,
            format!(
                "the symbol `{}` is not a type",
                table.get_full_name_of(self.found)?
            )
            .as_str(),
        );
        pernixc_common::printing::print_source_code(&self.span, None);
        println!();

        Ok(())
    }
}

/// The symbol of the given name was not found in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct SymbolNotFound {
    /// Specifies the location where the resolution failed.
    #[get = "pub"]
    pub(super) span: Span,

    /// Specifies the scope where the resolution was attempted.
    ///
    /// If this is `None`, then the resolution was attempted in the global scope.
    #[get = "pub"]
    pub(super) in_scope_id: Option<GlobalID>,
}

impl SymbolNotFound {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        let symbol_name = self.span.str();

        #[allow(clippy::option_if_let_else)]
        if let Some(symbol_id) = self.in_scope_id {
            pernixc_common::printing::log(
                LogSeverity::Error,
                format!(
                    "`{symbol_name}` was not found in `{parent_symbol}`",
                    symbol_name = symbol_name,
                    parent_symbol = table.get_full_name_of(symbol_id)?
                )
                .as_str(),
            );
        } else {
            pernixc_common::printing::log(
                LogSeverity::Error,
                format!("no target found for `{symbol_name}`").as_str(),
            );
        }

        pernixc_common::printing::print_source_code(&self.span, None);
        println!();

        Ok(())
    }
}

/// The symbol was not accessible from the given scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct SymbolNotAccessible {
    /// Specifies the location where the resolution failed.
    #[get = "pub"]
    pub(super) span: Span,

    /// Specifies the scope where the resolution was attempted.
    ///
    /// If this is `None`, then the resolution was attempted in the global scope.
    #[get = "pub"]
    pub(super) global_id: GlobalID,
}

impl SymbolNotAccessible {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        let symbol_name = self.span.str();

        pernixc_common::printing::log(
            LogSeverity::Error,
            format!(
                "`{symbol_name}` is not accessible from `{symbol}`",
                symbol_name = symbol_name,
                symbol = table.get_full_name_of(self.global_id)?
            )
            .as_str(),
        );

        pernixc_common::printing::print_source_code(&self.span, None);
        println!();

        Ok(())
    }
}

/// Uses of private symbols are leaked to the less restrictive scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct PrivateSymbolLeak {
    /// Specifies the location where the leak occurred.
    #[get = "pub"]
    pub(super) span: Span,

    /// The access modifier of the symbol that was leaked.
    #[get = "pub"]
    pub(super) accessibility: Accessibility,

    /// The access modifier of the scope that leaked the symbol.
    #[get = "pub"]
    pub(super) parent_accessibility: Accessibility,
}

impl PrivateSymbolLeak {
    /// Prints the error message to the console.
    pub fn print(&self) {
        pernixc_common::printing::log(
            LogSeverity::Error,
            "private symbols cannot be leaked to less restrictive scopes",
        );

        pernixc_common::printing::print_source_code(&self.span, None);
        println!();
    }
}

/// Parameters cannot be redefined.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct ParameterRedefinition {
    /// Specifies the location where the redefinition occurred.
    #[get = "pub"]
    pub(super) span: Span,

    /// The ID of the parameter that was redefined.
    #[get = "pub"]
    pub(super) available_parameter_id: ParameterID,
}

impl ParameterRedefinition {
    /// Prints the error message to the console.
    pub fn print(&self) {
        pernixc_common::printing::log(
            LogSeverity::Error,
            format!("parameter `{}` was redefined", self.span.str(),).as_str(),
        );

        pernixc_common::printing::print_source_code(&self.span, None);
        println!();
    }
}

/// Function overloads cannot be redefined
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct OverloadRedefinition {
    /// Specifies the location where the redefinition occurred.
    #[get = "pub"]
    pub(super) span: Span,

    /// The ID of the overload that was redefined.
    #[get = "pub"]
    pub(super) available_overload_id: OverloadID,
}

impl OverloadRedefinition {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        pernixc_common::printing::log(
            LogSeverity::Error,
            format!(
                "function overload `{}` has a redefinition",
                table.get_full_name_of(
                    table
                        .get_overload(self.available_overload_id)
                        .ok_or(InvalidIDError)?
                        .parent_overload_set_id
                )?
            )
            .as_str(),
        );

        pernixc_common::printing::print_source_code(&self.span, None);
        pernixc_common::printing::print_source_code(
            &table
                .get_overload(self.available_overload_id)
                .ok_or(InvalidIDError)?
                .syntax_tree()
                .identifier
                .span,
            Some("previous definition here"),
        );
        println!();

        Ok(())
    }
}

/// Struct members cannot be more accessible than the struct itself.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct StructMemberMoreAccessibleThanStruct {
    /// Specifies the location of the field group access modifier that is more accessible than the
    /// struct access modifier.
    #[get = "pub"]
    pub(super) span: Span,

    /// The access modifier of the struct.
    #[get = "pub"]
    pub(super) struct_accessibility: Accessibility,

    /// The access modifier of the struct member.
    #[get = "pub"]
    pub(super) member_accessibility: Accessibility,
}

impl StructMemberMoreAccessibleThanStruct {
    /// Prints the error message to the console.
    pub fn print(&self) {
        pernixc_common::printing::log(
            LogSeverity::Error,
            "this field group is more accessible than the struct itself, which is not allowed",
        );

        pernixc_common::printing::print_source_code(&self.span, None);
        println!();
    }
}

/// Struct members cannot be redefined.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct FieldRedefinition {
    /// Specifies the location where the redefinition occurred.
    #[get = "pub"]
    pub(super) span: Span,

    /// The ID of the field that was redefined.
    #[get = "pub"]
    pub(super) available_field_id: FieldID,
}

impl FieldRedefinition {
    /// Prints the error message to the console.
    pub fn print(&self) {
        pernixc_common::printing::log(
            LogSeverity::Error,
            format!("struct member `{}` was redefined", self.span.str(),).as_str(),
        );

        pernixc_common::printing::print_source_code(&self.span, None);
        println!();
    }
}

/// Enum variants cannot be redefined.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct EnumVariantRedefinition {
    /// Specifies the location where the redefinition occurred.
    #[get = "pub"]
    pub(super) span: Span,

    /// The ID of the enum variant that was redefined.
    #[get = "pub"]
    pub(super) available_enum_variant_id: EnumVariantID,
}

impl EnumVariantRedefinition {
    /// Prints the error message to the console.
    pub fn print(&self) {
        pernixc_common::printing::log(
            LogSeverity::Error,
            format!("enum variant `{}` was redefined", self.span.str(),).as_str(),
        );

        pernixc_common::printing::print_source_code(&self.span, None);
        println!();
    }
}

/// Symbols cannot be redefined.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct SymbolRedifinition {
    /// Specifies the location where the redefinition occurred.
    #[get = "pub"]
    pub(super) span: Span,

    /// The symbol that was redefined.
    #[get = "pub"]
    pub(super) available_global_id: GlobalID,
}

impl SymbolRedifinition {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        #[allow(clippy::option_if_let_else)]
        if let Some(parent_id) = table
            .get_global(self.available_global_id)
            .ok_or(InvalidIDError)?
            .parent_scoped_id()
        {
            pernixc_common::printing::log(
                LogSeverity::Error,
                &format!(
                    "symbol `{}` was already defined in `{}`",
                    self.span.str(),
                    table.get_full_name_of(parent_id)?
                ),
            );
        } else {
            pernixc_common::printing::log(
                LogSeverity::Error,
                &format!("symbol `{}` was already defined", self.span.str(),),
            );
        }

        pernixc_common::printing::print_source_code(&self.span, Some("redefinition"));
        println!();

        Ok(())
    }
}

/// A circular dependency was detected.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct CircularDependency {
    /// The list of symbols that form the circular dependency.
    #[get = "pub"]
    pub(super) symbol_ids: Vec<ID>,
}

impl CircularDependency {
    /// Prints the error message to the console.
    pub fn print(&self) {
        pernixc_common::printing::log(
            LogSeverity::Error,
            "found circular dependency between these symbols.",
        );
    }
}

impl SymbolError {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    #[allow(clippy::unit_arg)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        Ok(match self {
            Self::TypeExpected(e) => e.print(table)?,
            Self::SymbolNotFound(e) => e.print(table)?,
            Self::SymbolNotAccessible(e) => e.print(table)?,
            Self::PrivateSymbolLeak(e) => e.print(),
            Self::ParameterRedefinition(e) => e.print(),
            Self::StructMemberMoreAccessibleThanStruct(e) => e.print(),
            Self::FieldRedefinition(e) => e.print(),
            Self::EnumVariantRedefinition(e) => e.print(),
            Self::SymbolRedifinition(e) => e.print(table)?,
            Self::CircularDependency(e) => e.print(),
            Self::OverloadRedefinition(e) => e.print(table)?,
        })
    }
}
