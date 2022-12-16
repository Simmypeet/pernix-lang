use std::collections::HashMap;

use pernix_parser::abstract_syntax_tree::{
    declaration::{FunctionDeclaration, QualifiedType},
    PositionWrapper,
};

use crate::{error::Error, scope::ScopeInfo};

use self::table::TypeSymbolTable;

pub mod table;

/// Represent a bound version of the [`FunctionDeclaration`] AST.
#[derive(Debug, Clone)]
pub struct FunctionSymbol<'table, 'parser, 'ast> {
    ast: PositionWrapper<&'parser FunctionDeclaration<'ast>>,
    return_type: &'table TypeSymbol,
    parameter_types: Vec<VariableSymbol<'table, 'ast>>,
    full_qualified_name: String,
}

impl<'table, 'parser, 'ast: 'table> FunctionSymbol<'table, 'parser, 'ast> {
    /// Bind the [`FunctionDeclaration`] AST to a [`FunctionSymbol`]
    /// struct. The function will check if the function parameters have
    /// redeclaration errors and if the return type is valid.
    ///
    /// ## Parameters
    /// - `scope_info`: the scope information that will be used to lookup
    /// - `ast`: the AST that will be bound.
    /// - `table`: the type table that will be used to lookup the types.
    pub fn bind(
        scope_info: ScopeInfo,
        ast: PositionWrapper<&'parser FunctionDeclaration<'ast>>,
        table: &'table TypeSymbolTable,
    ) -> Result<Self, Vec<Error<'table, 'parser, 'ast>>> {
        let mut errors = Vec::new();

        // check if the function parameters have redeclaration
        // errors
        {
            let mut parameter_names = HashMap::<
                &'ast str,
                &'parser PositionWrapper<(QualifiedType<'ast>, &'ast str)>,
            >::new();

            let mut errors = Vec::new();
            for parameter in &ast.value.parameters {
                match parameter_names.get(parameter.value.1) {
                    Some(prev) => {
                        errors.push(Error::ParameterRedeclaration {
                            function_declaration: ast.clone(),
                            previous_declaration: prev,
                            redeclaration: parameter,
                        });
                    }
                    None => {
                        parameter_names.insert(parameter.value.1, parameter);
                    }
                }
            }

            if !errors.is_empty() {
                return Err(errors);
            }
        }

        // The return type of the function
        let return_type = match &ast.value.return_type {
            Some(val) => {
                match table.lookup_from_type_annotation(scope_info, &val) {
                    Ok(val) => Some(val),
                    Err(err) => {
                        errors.push(err);
                        None
                    }
                }
            }
            None => Some(&table.get("void").unwrap().value),
        };

        let parameter_types = {
            let mut parameters = Vec::new();

            for parameter in &ast.value.parameters {
                let parameter_type = match table.lookup_from_type_annotation(
                    scope_info,
                    &parameter.value.0.type_annotation,
                ) {
                    Ok(val) => val,
                    Err(err) => {
                        errors.push(err);
                        continue;
                    }
                };

                parameters.push(VariableSymbol {
                    variable_type: parameter_type,
                    name: parameter.value.1,
                    is_mutable: parameter.value.0.is_mutable,
                });
            }

            parameters
        };

        let full_qualified_name =
            if scope_info.current_namespace_scope.is_empty() {
                ast.value.function_name.value.to_string()
            } else {
                format!(
                    "{}.{}",
                    scope_info.current_namespace_scope,
                    ast.value.function_name.value
                )
            };

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(Self {
                ast,
                return_type: return_type.unwrap(),
                parameter_types,
                full_qualified_name: full_qualified_name,
            })
        }
    }

    /// Return a reference to the ast of this [`FunctionSymbol`].
    pub fn ast(&self) -> &PositionWrapper<&'parser FunctionDeclaration<'ast>> {
        &self.ast
    }

    /// Return a reference to the return type of this [`FunctionSymbol`].
    pub fn return_type(&self) -> &'table TypeSymbol {
        self.return_type
    }

    /// Return a reference to the parameter types of this [`FunctionSymbol`].
    pub fn parameter_types(&self) -> &[VariableSymbol<'table, 'table>] {
        self.parameter_types.as_ref()
    }

    /// Returns a reference to the full qualified name of this [`FunctionSymbol`].
    pub fn full_qualified_name(&self) -> &str {
        self.full_qualified_name.as_ref()
    }
}

/// Represent a type of a variable.
#[derive(Debug, Clone, Copy)]
pub struct VariableSymbol<'table, 'ast> {
    pub variable_type: &'table TypeSymbol,
    pub name: &'ast str,
    pub is_mutable: bool,
}

/// Represent an enumeration containing all the primitive types supported by Pernix.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Void,
    Bool,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
}

/// Represent an enumeration containing all the types supported by Pernix.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeSymbol {
    PrimitiveType(PrimitiveType),
}
