use std::collections::HashMap;

use pernixc_syntactic_analysis::abstract_syntax_tree::{
    declaration::{AccessModifier, QualifiedTypeAnnotationAST, TypeAnnotationAST},
    PositionWrapper, TypeUnitAST,
};

use crate::{error::SemanticError, scope::ScopeInfo};

pub mod builder;

/// Is an enumeration of all the primitive types that are supported by the language.
pub type PrimitiveTypeUnit = pernixc_syntactic_analysis::abstract_syntax_tree::PrimitiveTypeUnit;

/// Is a symbol ID that is used to uniquely identify a symbol in the symbol table.
/// It is used for fast lookup of symbols in the symbol table and is cheaper than
/// using the full qualified name of the symbol.
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct SymbolID {
    id: usize,
}

/// Is a qualified type symbol that is used to give a type annotation to an lvalue
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct QualifiedTypeAnnotationSymbol {
    pub type_annotation_symbol: TypeAnnotationSymbol,
    pub is_mutable: bool,
}

/// Is a type annotation symbol that builds on top of a type unit symbol by adding
/// additional annotations to it such as reference, array, etc.
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum TypeAnnotationSymbol {
    TypeUnitSymbol(TypeUnitSymbol),
}

/// Is a type unit symbol that can be either a primitive type unit or a user
/// defined type unit.
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum TypeUnitSymbol {
    PrimitiveTypeUnit(PrimitiveTypeUnit),
    UserDefinedTypeUnit(SymbolID),
}

/// Represent a class declaration symbol. It contains the name of the class and
/// the class layout.
pub struct ClassSymbol {
    pub name: String,
    pub fields: HashMap<String, ClassFieldSymbol>,
}

/// Represent a class field declaration symbol. It contains the name of the field,
/// the type annotation and the access modifier.
pub struct ClassFieldSymbol {
    pub name: String,
    pub type_annotation_symbol: TypeAnnotationSymbol,
    pub access_modifier: AccessModifier,
}

/// Represent a function declaration symbol. It contains the name of the function,
/// the return type, the parameters and the access modifier.
pub struct FunctionSymbol {
    pub name: String,
    pub return_type: TypeAnnotationSymbol,
    pub parameters: Vec<QualifiedTypeAnnotationSymbol>,
    pub access_modifier: AccessModifier,
}

/// Is a symbol that contains a collection of functions that share the same name but
/// have different signatures (parameters).
pub struct OverloadSetSymbol {
    pub name: String,
    pub functions: Vec<FunctionSymbol>,
}

/// Is an enumeration of all the symbols that can be defined in the language.
pub enum Symbol {
    Namespace,
    ClassSymbol(ClassSymbol),
    OverloadSetSymbol(OverloadSetSymbol),
}

impl Symbol {
    /// Returns `true` if the symbol is [`Namespace`].
    ///
    /// [`Namespace`]: Symbol::Namespace
    #[must_use]
    pub fn is_namespace(&self) -> bool {
        matches!(self, Self::Namespace)
    }

    /// Returns `true` if the symbol is [`ClassSymbol`].
    ///
    /// [`ClassSymbol`]: Symbol::ClassSymbol
    #[must_use]
    pub fn is_class_symbol(&self) -> bool {
        matches!(self, Self::ClassSymbol(..))
    }

    /// Returns `true` if the symbol is [`OverloadSetSymbol`].
    ///
    /// [`OverloadSetSymbol`]: Symbol::OverloadSetSymbol
    #[must_use]
    pub fn is_overload_set_symbol(&self) -> bool {
        matches!(self, Self::OverloadSetSymbol(..))
    }
}

/// Is a symbol table that contains all the symbols that are defined in the program.
/// It allows fast lookup of symbols by name.
pub struct SymbolTable {
    symbols: Vec<Symbol>,
    qualified_name_map: HashMap<String, usize>,
}

impl SymbolTable {
    /// Insert a new symbol into the symbol table. The function returns `Ok` with
    /// the symbol ID of the newly inserted symbol if the symbol was successfully.
    /// Otherwise, it returns `Err` with the symbol ID of the symbol that already
    /// exists in the symbol table.
    pub fn insert(&mut self, qualified_name: String, symbol: Symbol) -> Result<SymbolID, SymbolID> {
        // check if the symbol already exists
        if let Some(id) = self.qualified_name_map.get(&qualified_name) {
            return Err(SymbolID { id: *id });
        }

        // insert the symbol
        let id = self.symbols.len();
        self.symbols.push(symbol);
        self.qualified_name_map.insert(qualified_name, id);

        Ok(SymbolID { id })
    }

    /// Get a reference to a symbol by its symbol ID.
    pub fn get_by_id(&self, id: SymbolID) -> Option<&Symbol> {
        self.symbols.get(id.id)
    }

    /// Get a mutable reference to a symbol by its symbol ID.
    pub fn get_mut_by_id(&mut self, id: SymbolID) -> Option<&mut Symbol> {
        self.symbols.get_mut(id.id)
    }

    /// Get a reference to a symbol by its full qualified name.
    pub fn get_by_full_qualified_name(&self, qualified_name: &str) -> Option<&Symbol> {
        if let Some(id) = self.qualified_name_map.get(qualified_name) {
            self.symbols.get(*id)
        } else {
            None
        }
    }

    /// Get a mutable reference to a symbol by its full qualified name.
    pub fn get_mut_by_full_qualified_name(&mut self, qualified_name: &str) -> Option<&mut Symbol> {
        if let Some(id) = self.qualified_name_map.get(qualified_name) {
            self.symbols.get_mut(*id)
        } else {
            None
        }
    }

    /// Lookup a symbol by its referred name and the scope information of the referring site. The
    /// function will return the [`SymbolID`] of the symbol if it was found. Otherwise, it will
    /// return [`None`].
    pub fn lookup(&self, referred_name: &str, scope_info: &ScopeInfo) -> Option<SymbolID> {
        // first, we will try to lookup the entry based on the given usings.

        // but we will iterate the usings list in reverse order. this is because
        // the usings list is a stack. the last usings are the ones that are
        // activated first.
        for using in scope_info.active_using_directives.iter().rev() {
            let full_qualified_name = if using.is_empty() {
                referred_name.to_string()
            } else {
                format!("{}::{}", using, referred_name)
            };

            if let Some(entry) = self.qualified_name_map.get(&full_qualified_name) {
                return Some(SymbolID { id: *entry });
            }
        }

        // next, we will try to lookup the entry based on the reference scope,
        // but every time it fails, we will remove the last part of the scope
        // and try again.
        let mut current_scope = scope_info.scope_name.clone();
        loop {
            let full_qualified_name = if current_scope.is_empty() {
                referred_name.to_string()
            } else {
                format!("{}::{}", current_scope, referred_name)
            };

            if let Some(entry) = self.qualified_name_map.get(&full_qualified_name) {
                return Some(SymbolID { id: *entry });
            }

            // if the current scope is empty, we will stop the loop.
            if current_scope.is_empty() {
                return None;
            }

            // remove the last part of the scope.
            current_scope = current_scope
                .rsplit_once("::")
                .map(|(namespace, _)| namespace)
                .unwrap_or("")
                .to_string();
        }
    }

    /// Is a helper function that will create a new [`TypeAnnotationSymbol`] from the given
    /// [`TypeAnnotationAST`] and the scope information of the referring site.
    pub fn get_type_annotation_symbol<'ast, 'src>(
        &self,
        type_annotation_ast: &'ast PositionWrapper<TypeAnnotationAST<'src>>,
        scope_info: &ScopeInfo<'src>,
    ) -> Result<TypeAnnotationSymbol, SemanticError<'ast, 'src>> {
        match &type_annotation_ast.value {
            // type unit type annotation
            TypeAnnotationAST::TypeUnit(type_unit) => match type_unit {
                // primitive type
                TypeUnitAST::PrimitiveTypeUnit(primitive_type_unit) => {
                    Ok(TypeAnnotationSymbol::TypeUnitSymbol(
                        TypeUnitSymbol::PrimitiveTypeUnit(*primitive_type_unit),
                    ))
                }

                // qualified name
                TypeUnitAST::QualifiedName(qualified_name) => {
                    match self.lookup(&qualified_name, scope_info) {
                        Some(sym_id) => {
                            // check if the symbol belongs to type
                            if !matches!(self.get_by_id(sym_id).unwrap(), Symbol::ClassSymbol(_)) {
                                return Err(SemanticError::TypeExpected {
                                    type_annotation_ast,
                                });
                            } else {
                                Ok(TypeAnnotationSymbol::TypeUnitSymbol(
                                    TypeUnitSymbol::UserDefinedTypeUnit(sym_id),
                                ))
                            }
                        }
                        None => {
                            return Err(SemanticError::TypeNotFound {
                                type_annotation_ast,
                            });
                        }
                    }
                }
            },
        }
    }

    /// Is a helper function that will create a new [`QualifiedTypeAnnotationSymbol`] from the given
    /// [`QualifiedTypeAnnotationAST`] and the scope information of the referring site.
    pub fn get_qualified_type_annotation_symbol<'ast, 'src>(
        &self,
        qualified_type_annotation_ast: &'ast PositionWrapper<QualifiedTypeAnnotationAST<'src>>,
        scope_info: &ScopeInfo<'src>,
    ) -> Result<QualifiedTypeAnnotationSymbol, SemanticError<'ast, 'src>> {
        let type_annotation_symbol = self.get_type_annotation_symbol(
            &qualified_type_annotation_ast.value.type_annotation,
            scope_info,
        )?;

        Ok(QualifiedTypeAnnotationSymbol {
            type_annotation_symbol,
            is_mutable: qualified_type_annotation_ast.value.is_mutable,
        })
    }

    /// Get the [`SymbolID`] of a symbol by its full qualified name. If the symbol was not found,
    /// the function will return [`None`].
    pub fn get_symbol_id_by_full_qualified_name(&self, qualified_name: &str) -> Option<SymbolID> {
        self.qualified_name_map
            .get(qualified_name)
            .map(|id| SymbolID { id: *id })
    }
}

#[cfg(test)]
mod test;
