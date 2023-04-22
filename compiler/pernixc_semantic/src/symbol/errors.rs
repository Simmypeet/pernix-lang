//! Contains the errors that can occur during the symbol resolution/construction phase of the
//! compiler.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_common::{printing::LogSeverity, source_file::Span};

use super::item::{AccessModifier, Table, ID};

/// Is an error that can occur during the symbol resolution/construction phase of the compiler.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum SymbolError {
    SymbolNotFound(SymbolNotFound),
    SymbolNotAccessible(SymbolNotAccessible),
    PrivateSymbolLeak(PrivateSymbolLeak),
    ParameterRedefinition(ParameterRedefinition),
    StructMemberMoreAccessibleThanStruct(FieldGroupMoreAccessibleThanStruct),
    FieldRedefinition(FieldRedefinition),
    EnumVariantRedefinition(EnumVariantRedefinition),
    SymbolRedifinition(SymbolRedifinition),
}

/// The symbol of the given name was not found in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct SymbolNotFound {
    /// Specifies the location where the resolution failed.
    #[get = "pub"]
    pub(super) span: Span,

    /// Specifies the scope where the resolution was attempted.
    ///
    /// If this is `None`, then the resolution was attempted in the root scope.
    #[get = "pub"]
    pub(super) symbol_id: Option<ID>,
}

impl SymbolNotFound {
    /// Prints the error message to the console.
    pub fn print(&self, table: &Table) {
        // let symbol_name = self.span.str();
        // self.symbol_id.map_or_else(
        //     || {
        //         pernixc_common::printing::log(
        //             LogSeverity::Error,
        //             format!("no target found for `{symbol_name}`").as_str(),
        //         );
        //     },
        //     |symbol_id| {
        //         let parent_symbol = &table[symbol_id];

        //         pernixc_common::printing::log(
        //             LogSeverity::Error,
        //             format!(
        //                 "`{symbol_name}` was not found in `{parent_symbol}`",
        //                 symbol_name = symbol_name,
        //                 parent_symbol = parent_symbol.name(),
        //             )
        //             .as_str(),
        //         );
        //     },
        // );

        // pernixc_common::printing::print_source_code(&self.span, None);
        // println!();
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
    /// If this is `None`, then the resolution was attempted in the root scope.
    #[get = "pub"]
    pub(super) symbol_id: ID,
}

impl SymbolNotAccessible {
    /// Prints the error message to the console.
    pub fn print(&self, table: &Table) {
        // let symbol_name = self.span.str();
        // let symbol = &table[self.symbol_id];

        // pernixc_common::printing::log(
        //     LogSeverity::Error,
        //     format!(
        //         "`{symbol_name}` is not accessible from `{symbol}`",
        //         symbol_name = symbol_name,
        //         symbol = symbol.name(),
        //     )
        //     .as_str(),
        // );

        // pernixc_common::printing::print_source_code(&self.span, None);
        // println!();
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
    pub(super) access_modifier: AccessModifier,

    /// The access modifier of the scope that leaked the symbol.
    #[get = "pub"]
    pub(super) parent_access_modifier: AccessModifier,

    /// The less restrictive scope that leaked the symbol.
    #[get = "pub"]
    pub(super) parent_symbol_id: ID,
}

impl PrivateSymbolLeak {
    /// Prints the error message to the console.
    pub fn print(&self, table: &Table) {
        // let symbol_name = self.span.str();
        // let parent_symbol = &table[self.parent_symbol_id];

        // pernixc_common::printing::log(
        //     LogSeverity::Error,
        //     format!(
        //         "`{symbol_name}` is more restrictive and cannot be leaked to `{parent_symbol}`, \
        //          which is less restrictive",
        //         symbol_name = symbol_name,
        //         parent_symbol = parent_symbol.name(),
        //     )
        //     .as_str(),
        // );

        // pernixc_common::printing::print_source_code(&self.span, None);
        // println!();
    }
}

/// Parameters cannot be redefined.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct ParameterRedefinition {
    /// Specifies the location where the redefinition occurred.
    #[get = "pub"]
    pub(super) span: Span,
}

impl ParameterRedefinition {
    /// Prints the error message to the console.
    pub fn print(&self) {
        // pernixc_common::printing::log(
        //     LogSeverity::Error,
        //     format!("parameter `{}` was redefined", self.span.str(),).as_str(),
        // );

        // pernixc_common::printing::print_source_code(&self.span, None);
        // println!();
    }
}

/// Struct members cannot be more accessible than the struct itself.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct FieldGroupMoreAccessibleThanStruct {
    /// Specifies the location of the struct member that is more accessible than the struct itself
    /// (identifier).
    #[get = "pub"]
    pub(super) span: Span,

    /// The access modifier of the struct.
    #[get = "pub"]
    pub(super) struct_access_modifier: AccessModifier,

    /// The access modifier of the struct member.
    #[get = "pub"]
    pub(super) group_access_modifier: AccessModifier,
}

impl FieldGroupMoreAccessibleThanStruct {
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
    pub(super) available_symbol_id: ID,
}

impl SymbolRedifinition {
    /// Prints the error message to the console.
    pub fn print(&self, table: &Table) {
        // let symbol_name = self.span.str();
        // let available_symbol = &table[self.available_symbol_id];

        // pernixc_common::printing::log(
        //     LogSeverity::Error,
        //     format!(
        //         "`{symbol_name}` was redefined, but it is already defined in `{available_symbol}`",
        //         symbol_name = symbol_name,
        //         available_symbol = available_symbol.name(),
        //     )
        //     .as_str(),
        // );

        // pernixc_common::printing::print_source_code(&self.span, Some("redefinition"));
        // println!();
    }
}

impl SymbolError {
    /// Prints the error message to the console.
    pub fn print(&self, table: &Table) {
        match self {
            Self::SymbolNotFound(e) => e.print(table),
            Self::SymbolNotAccessible(e) => e.print(table),
            Self::PrivateSymbolLeak(e) => e.print(table),
            Self::ParameterRedefinition(e) => e.print(),
            Self::StructMemberMoreAccessibleThanStruct(e) => e.print(),
            Self::FieldRedefinition(e) => e.print(),
            Self::EnumVariantRedefinition(e) => e.print(),
            Self::SymbolRedifinition(e) => e.print(table),
        }
    }
}
