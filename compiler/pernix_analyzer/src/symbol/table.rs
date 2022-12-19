use std::collections::{
    hash_map::{Entry, Values},
    HashMap,
};

use pernix_parser::{
    abstract_syntax_tree::{
        declaration::{Declaration, TypeAnnotation},
        PositionWrapper,
    },
    File,
};

use crate::{
    error::Error,
    scope::{ScopeInfo, ScopeTraverser},
};

use super::{FunctionSymbol, PrimitiveType, TypeSymbol};

/// Represent an entry in the [`SymbolTable`]. The entry contains the
/// namespace, name and value of the entry. The value is generic and can be
/// anything.
pub struct SymbolTableEntry<T> {
    pub namespace: String,
    pub name: String,
    pub value: T,
}

/// Represent a symbol table that manages the declarations of the program.
pub struct SymbolTable<T> {
    // A HashMap containing a string representing the full qualified name of
    // the entry and the entry itself.
    entries: HashMap<String, SymbolTableEntry<T>>,
}

impl<T> SymbolTable<T> {
    /// Create a new instance of the [`SymbolTable`] struct with an empty
    /// entries.
    fn new_internal() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    /// Insert a new entry to the [`SymbolTable`] by providing the
    /// namespace, name and value of the entry. If the entry with the same full
    /// qualified name already exists, the function will return `false` and the
    /// entry will not be inserted.
    fn insert(&mut self, namespace: &str, name: &str, value: T) -> bool {
        let full_qualified_name = if namespace.is_empty() {
            name.to_string()
        } else {
            format!("{}.{}", namespace, name)
        };
        let new_entry = SymbolTableEntry {
            namespace: namespace.to_string(),
            name: name.to_string(),
            value,
        };

        // insert a new entry to the HashMap and check if the entry already
        // exists. if the entry already exists, the function will return `None`
        // and don't update the HashMap.
        match self.entries.entry(full_qualified_name) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert(new_entry);
                true
            }
        }
    }

    /// Get an entry from the [`SymbolTable`] by providing the full
    /// qualified name of the entry. If the entry doesn't exist, the function
    /// will return `None`.
    pub fn get(
        &self,
        full_qualified_name: &str,
    ) -> Option<&SymbolTableEntry<T>> {
        self.entries.get(full_qualified_name)
    }

    /// Lookup an entry from the [`SymbolTable`]. The function will search
    /// for the entry from the given scope info. If the entry doesn't exist,
    /// the function will return `None`.
    ///
    /// ## Parameters
    /// - `reference_scope`: is the name of the scope where the reference is
    /// made.
    /// - `name`: is the name of the entry to lookup.
    /// - `usings`: is a list of namespaces activated in the current scope.
    pub fn lookup(
        &self,
        scope_info: &ScopeInfo,
        name: &str,
    ) -> Option<&SymbolTableEntry<T>> {
        // first, we will try to lookup the entry based on the given usings.

        // but we will iterate the usings list in reverse order. this is because
        // the usings list is a stack. the last usings are the ones that are
        // activated first.
        for using in scope_info.active_using_directives.iter().rev() {
            let full_qualified_name = if using.is_empty() {
                name.to_string()
            } else {
                format!("{}.{}", using, name)
            };

            if let Some(entry) = self.get(&full_qualified_name) {
                return Some(entry);
            }
        }

        // next, we will try to lookup the entry based on the reference scope,
        // but every time it fails, we will remove the last part of the scope
        // and try again.
        let mut current_scope = scope_info.current_namespace_scope.clone();
        loop {
            let full_qualified_name = if current_scope.is_empty() {
                name.to_string()
            } else {
                format!("{}.{}", current_scope, name)
            };

            if let Some(entry) = self.get(&full_qualified_name) {
                return Some(entry);
            }

            // if the current scope is empty, we will stop the loop.
            if current_scope.is_empty() {
                return None;
            }

            // remove the last part of the scope.
            current_scope = current_scope
                .rsplit_once('.')
                .map(|(namespace, _)| namespace)
                .unwrap_or("")
                .to_string();
        }
    }

    /// Get the full hash map containing the entries.
    pub fn get_entries(&self) -> &HashMap<String, SymbolTableEntry<T>> {
        &self.entries
    }

    /// Get the iterator of the values of the entries.
    pub fn values(&self) -> Values<'_, String, SymbolTableEntry<T>> {
        self.entries.values()
    }
}

/// Represent a symbol table that manages the type symbols.
pub type TypeSymbolTable = SymbolTable<TypeSymbol>;

/// Represent a symbol table that manages the function symbols.
pub type FunctionSymbolTable<'table, 'ast> =
    SymbolTable<FunctionSymbol<'table, 'ast>>;

impl TypeSymbolTable {
    /// Create a new instance of the [`TypeSymbolTable`] struct with the
    /// primitive types.
    pub fn new<'parser, 'ast>() -> Self {
        let mut table = Self::new_internal();

        // insert the primitive types to the table.
        table.insert(
            "",
            "int8",
            TypeSymbol::PrimitiveType(PrimitiveType::Int8),
        );
        table.insert(
            "",
            "int16",
            TypeSymbol::PrimitiveType(PrimitiveType::Int16),
        );
        table.insert(
            "",
            "int32",
            TypeSymbol::PrimitiveType(PrimitiveType::Int32),
        );
        table.insert(
            "",
            "int64",
            TypeSymbol::PrimitiveType(PrimitiveType::Int64),
        );
        table.insert(
            "",
            "uint8",
            TypeSymbol::PrimitiveType(PrimitiveType::Uint8),
        );
        table.insert(
            "",
            "uint16",
            TypeSymbol::PrimitiveType(PrimitiveType::Uint16),
        );
        table.insert(
            "",
            "uint32",
            TypeSymbol::PrimitiveType(PrimitiveType::Uint32),
        );
        table.insert(
            "",
            "uint64",
            TypeSymbol::PrimitiveType(PrimitiveType::Uint64),
        );
        table.insert(
            "",
            "float32",
            TypeSymbol::PrimitiveType(PrimitiveType::Float32),
        );
        table.insert(
            "",
            "float64",
            TypeSymbol::PrimitiveType(PrimitiveType::Float64),
        );
        table.insert(
            "",
            "bool",
            TypeSymbol::PrimitiveType(PrimitiveType::Bool),
        );
        table.insert(
            "",
            "void",
            TypeSymbol::PrimitiveType(PrimitiveType::Void),
        );

        table
    }

    /// Get the primitive type from the given [`PrimitiveType`].
    pub fn get_primitive_type(
        &self,
        primitive_type: PrimitiveType,
    ) -> &SymbolTableEntry<TypeSymbol> {
        match primitive_type {
            PrimitiveType::Void => self.entries.get("void").unwrap(),
            PrimitiveType::Bool => self.entries.get("bool").unwrap(),
            PrimitiveType::Int8 => self.entries.get("int8").unwrap(),
            PrimitiveType::Int16 => self.entries.get("int16").unwrap(),
            PrimitiveType::Int32 => self.entries.get("int32").unwrap(),
            PrimitiveType::Int64 => self.entries.get("int64").unwrap(),
            PrimitiveType::Uint8 => self.entries.get("uint8").unwrap(),
            PrimitiveType::Uint16 => self.entries.get("uint16").unwrap(),
            PrimitiveType::Uint32 => self.entries.get("uint32").unwrap(),
            PrimitiveType::Uint64 => self.entries.get("uint64").unwrap(),
            PrimitiveType::Float32 => self.entries.get("float32").unwrap(),
            PrimitiveType::Float64 => self.entries.get("float64").unwrap(),
        }
    }

    /// Lookup a type from the [`TypeSymbolTable`]. The function will
    /// search for the type from the given scope info. If the type doesn't
    /// exist, the function will return `None`.
    pub fn lookup_from_type_annotation<'parser, 'ast>(
        &self,
        scope_info: &ScopeInfo,
        type_annotation: &'parser PositionWrapper<TypeAnnotation<'ast>>,
    ) -> Result<&SymbolTableEntry<TypeSymbol>, Error<'_, 'ast>> {
        match type_annotation.value {
            TypeAnnotation::QualifiedName(qualified_name) => {
                let entry = self
                    .lookup(scope_info, &qualified_name)
                    .ok_or_else(|| Error::TypeNotFound {
                        type_name: PositionWrapper {
                            position: type_annotation.position.clone(),
                            value: qualified_name,
                        },
                    })?;

                Ok(&entry)
            }
        }
    }
}

impl<'table, 'ast: 'table> FunctionSymbolTable<'table, 'ast> {
    /// Creates a new empty instance of the [`FunctionSymbolTable`] struct.
    pub fn new() -> Self {
        Self::new_internal()
    }

    /// Populate the [`FunctionSymbolTable`] with the functions from the
    /// given AST. The function will look for the function declarations in the
    /// given AST and will insert them to the table.
    ///
    /// ## Parameters
    /// - `ast`: is the AST that contains the function declarations.
    /// - `type_table`: is the type table that function declarations will use to
    ///  lookup the types.
    ///
    /// ## Returns
    /// The function will return `Ok(())` if the function declarations are all
    /// valid. Otherwise, the function will return `Err(errors)` where `errors`
    /// is a list of errors that occurred during the binding process.
    pub fn populate(
        &mut self,
        ast: &'ast File<'ast>,
        type_table: &'table TypeSymbolTable,
    ) -> Result<(), Vec<Error<'_, 'ast>>> {
        let mut scope_traverser = ScopeTraverser::new(ast);
        let mut errors = Vec::<Error<'_, 'ast>>::new();

        scope_traverser.traverse(&mut |scope_info, declaration| {
            match &declaration.value {
                Declaration::FunctionDeclaration(function) => {
                    match FunctionSymbol::bind(
                        scope_info,
                        PositionWrapper {
                            position: declaration.position.clone(),
                            value: function,
                        },
                        type_table,
                    ) {
                        Ok(bound_func) => {
                            if !self.insert(
                                &scope_info.current_namespace_scope,
                                bound_func.ast.value.function_name.value,
                                bound_func,
                            ) {
                                let full_qualified_name = format!(
                                    "{}.{}",
                                    scope_info.current_namespace_scope,
                                    function.function_name.value
                                );

                                let previous_declaration =
                                    self.get(&full_qualified_name).unwrap();

                                errors.push(Error::FunctionRedeclaration {
                                    previous_declaration: previous_declaration
                                        .value
                                        .ast
                                        .clone(),
                                    redeclaration: PositionWrapper {
                                        position: declaration.position.clone(),
                                        value: function,
                                    },
                                });
                            }
                        }
                        Err(mut bound_errors) => {
                            errors.append(&mut bound_errors);
                        }
                    }
                }
                _ => {}
            }
        });

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

#[cfg(test)]
mod test;
