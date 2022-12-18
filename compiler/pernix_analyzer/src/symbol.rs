use std::collections::HashMap;
use std::hash::Hash;

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
    parameters: Vec<VariableSymbol<'table, 'ast>>,
    scope_info: ScopeInfo,
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
        scope_info: &ScopeInfo,
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
                    Ok(val) => Some(&val.value),
                    Err(err) => {
                        errors.push(err);
                        None
                    }
                }
            }
            None => Some(&table.get_primitive_type(PrimitiveType::Void).value),
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

                // no void type!
                if parameter_type.value.is_void() {
                    errors.push(Error::DefinedVoidVariable {
                        position: parameter.position.clone(),
                    });
                } else {
                    parameters.push(VariableSymbol {
                        variable_type: &parameter_type.value,
                        name: parameter.value.1,
                        is_mutable: parameter.value.0.is_mutable,
                    });
                }
            }

            parameters
        };

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(Self {
                ast,
                return_type: return_type.unwrap(),
                parameters: parameter_types,
                scope_info: scope_info.clone(),
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
    pub fn parameters(&self) -> &[VariableSymbol<'table, 'table>] {
        self.parameters.as_ref()
    }

    /// Return a reference to the full qualified name of this [`FunctionSymbol`].
    pub fn full_qualified_name(&self) -> String {
        if self.scope_info.current_namespace_scope.is_empty() {
            self.ast.value.function_name.value.to_string()
        } else {
            format!(
                "{}.{}",
                self.scope_info.current_namespace_scope,
                self.ast.value.function_name.value
            )
        }
    }

    /// Return a reference to the scope info of this [`FunctionSymbol`].
    pub fn scope_info(&self) -> &ScopeInfo {
        &self.scope_info
    }
}

/// Represent a type of a variable.
#[derive(Debug, Clone, Copy)]
pub struct VariableSymbol<'table, 'ast> {
    pub variable_type: &'table TypeSymbol,
    pub name: &'ast str,
    pub is_mutable: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone)]
pub enum TypeSymbol {
    PrimitiveType(PrimitiveType),
}

impl TypeSymbol {
    /// Check whether the type is a primitive `bool` type.
    pub fn is_bool(&self) -> bool {
        match self {
            TypeSymbol::PrimitiveType(PrimitiveType::Bool) => true,
            _ => false,
        }
    }

    /// Check whether the type is a primitive numeric type.
    pub fn is_numeric(&self) -> bool {
        match self {
            TypeSymbol::PrimitiveType(PrimitiveType::Int8)
            | TypeSymbol::PrimitiveType(PrimitiveType::Int16)
            | TypeSymbol::PrimitiveType(PrimitiveType::Int32)
            | TypeSymbol::PrimitiveType(PrimitiveType::Int64)
            | TypeSymbol::PrimitiveType(PrimitiveType::Uint8)
            | TypeSymbol::PrimitiveType(PrimitiveType::Uint16)
            | TypeSymbol::PrimitiveType(PrimitiveType::Uint32)
            | TypeSymbol::PrimitiveType(PrimitiveType::Uint64)
            | TypeSymbol::PrimitiveType(PrimitiveType::Float32)
            | TypeSymbol::PrimitiveType(PrimitiveType::Float64) => true,
            _ => false,
        }
    }

    /// Check whether the type is a primitive signed numeric type.
    pub fn is_signed_numeric(&self) -> bool {
        match self {
            TypeSymbol::PrimitiveType(PrimitiveType::Int8)
            | TypeSymbol::PrimitiveType(PrimitiveType::Int16)
            | TypeSymbol::PrimitiveType(PrimitiveType::Int32)
            | TypeSymbol::PrimitiveType(PrimitiveType::Int64)
            | TypeSymbol::PrimitiveType(PrimitiveType::Float32)
            | TypeSymbol::PrimitiveType(PrimitiveType::Float64) => true,
            _ => false,
        }
    }

    /// Check whether the type is a primitive unsigned numeric type.
    pub fn is_unsigned_numeric(&self) -> bool {
        match self {
            TypeSymbol::PrimitiveType(PrimitiveType::Uint8)
            | TypeSymbol::PrimitiveType(PrimitiveType::Uint16)
            | TypeSymbol::PrimitiveType(PrimitiveType::Uint32)
            | TypeSymbol::PrimitiveType(PrimitiveType::Uint64) => true,
            _ => false,
        }
    }

    /// Check whether the type is a primitive type.
    pub fn is_primitive_type(&self) -> bool {
        match self {
            TypeSymbol::PrimitiveType(_) => true,
        }
    }

    /// Check whether the type is a primitive `void` type.
    pub fn is_void(&self) -> bool {
        match self {
            TypeSymbol::PrimitiveType(PrimitiveType::Void) => true,
            _ => false,
        }
    }

    /// Check whether the type is a primitive integer type.
    pub fn is_integer(&self) -> bool {
        match self {
            TypeSymbol::PrimitiveType(PrimitiveType::Int8)
            | TypeSymbol::PrimitiveType(PrimitiveType::Int16)
            | TypeSymbol::PrimitiveType(PrimitiveType::Int32)
            | TypeSymbol::PrimitiveType(PrimitiveType::Int64)
            | TypeSymbol::PrimitiveType(PrimitiveType::Uint8)
            | TypeSymbol::PrimitiveType(PrimitiveType::Uint16)
            | TypeSymbol::PrimitiveType(PrimitiveType::Uint32)
            | TypeSymbol::PrimitiveType(PrimitiveType::Uint64) => true,
            _ => false,
        }
    }

    /// Check whether the type is a primitive floating point type.
    pub fn is_floating_point(&self) -> bool {
        match self {
            TypeSymbol::PrimitiveType(PrimitiveType::Float32)
            | TypeSymbol::PrimitiveType(PrimitiveType::Float64) => true,
            _ => false,
        }
    }

    /// Check whether the type is a primitive type
    pub fn is_primitive_type_of(&self, primitive_type: PrimitiveType) -> bool {
        match self {
            TypeSymbol::PrimitiveType(val) => val == &primitive_type,
        }
    }
}
