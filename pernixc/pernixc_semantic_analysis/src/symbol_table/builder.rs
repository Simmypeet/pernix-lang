use std::collections::{hash_map::Entry, HashMap, HashSet};

use pernixc_syntactic_analysis::abstract_syntax_tree::{
    declaration::{ClassMemberDeclarationAST, NamespaceLevelDeclarationAST},
    FileAST, PositionWrapper,
};

use crate::{error::SemanticError, scope::ScopeInfo};

use super::{
    ClassFieldSymbol, ClassSymbol, FunctionSymbol, OverloadSetSymbol, Symbol, SymbolTable,
};

/// Is a builder struct that is used to build a symbol table. It records a list of
/// declarations AST nodes and their corresponding scope information. Then, it
/// uses those to build the symbol table.
pub struct SymbolTableBuilder<'ast, 'src> {
    pub namespaces: HashSet<String>,
    pub declarations: Vec<(
        &'ast PositionWrapper<NamespaceLevelDeclarationAST<'src>>,
        ScopeInfo<'src>,
        String,
    )>,
}

impl<'ast, 'src> SymbolTableBuilder<'ast, 'src> {
    /// Construct a new instance of the [`SymbolTableBuilder`].
    pub fn new() -> Self {
        Self {
            namespaces: HashSet::new(),
            declarations: Vec::new(),
        }
    }

    /// Add namespaces defined in the given namespace string to the hash set.
    fn add_namespace_from_namespace_string(&mut self, namespace_string: &str) {
        // early exit
        if namespace_string.is_empty() {
            return;
        }

        // split the scope and add one by one
        let split = namespace_string.split("::");
        let mut namespace_string = String::new();

        // loop through the split and add the namespace
        for scope in split {
            if !namespace_string.is_empty() {
                namespace_string.push_str("::");
            }

            namespace_string.push_str(scope);
            self.namespaces.insert(namespace_string.clone());
        }
    }

    /// Record all the declaration ASTs defined in the given [`FileAST`].
    pub fn add_from_file_ast(&mut self, file_ast: &'ast FileAST<'src>)
    where
        'ast: 'src,
    {
        ScopeInfo::traverse(file_ast, &mut |scope_info, declaration| {
            let full_qualified_name = if scope_info.scope_name.is_empty() {
                declaration.value.get_name().value.to_string()
            } else {
                format!(
                    "{}::{}",
                    scope_info.scope_name,
                    declaration.value.get_name().value
                )
            };

            if let NamespaceLevelDeclarationAST::NamespaceDeclaration(_) = &declaration.value {
                // full-qualified name of the namespace
                self.add_namespace_from_namespace_string(&full_qualified_name);
            } else {
                // add to declaration list
                self.declarations
                    .push((declaration, scope_info.clone(), full_qualified_name));
            }
        });
    }

    /// Finalize the builder and build the symbol table.
    pub fn build(self) -> (SymbolTable, Vec<SemanticError<'ast, 'src>>) {
        let mut errors = Vec::new();
        let mut symbol_table = SymbolTable {
            symbols: Vec::new(),
            qualified_name_map: HashMap::new(),
        };

        // insert the namespace into the symbol table first
        for namespace in self.namespaces {
            let _ = symbol_table.insert(namespace, Symbol::Namespace);
        }

        // pass 1: loop through the declarations and add them to the symbol table
        for (declaration, _, full_qualified_name) in &self.declarations {
            // the class symbol to insert
            let class_symbol = Symbol::ClassSymbol(ClassSymbol {
                name: declaration.value.get_name().value.to_string(),
                fields: HashMap::new(),
            });

            // insert the symbol
            if let Err(sym_id) = symbol_table.insert(full_qualified_name.clone(), class_symbol) {
                if let Symbol::Namespace = symbol_table.get_by_id(sym_id).unwrap() {
                    errors.push(SemanticError::DeclarationNameConflictWithNamespace {
                        name_conflict: declaration.value.get_name(),
                    })
                } else {
                    errors.push(SemanticError::SymbolRedeclaration {
                        redeclaration_name: declaration.value.get_name(),
                    });
                }
            }
        }

        // pass 2: loop through the class declarations and expand the class method symbol
        for (declaration, scope_info, full_qualified_name) in &self.declarations {
            if let NamespaceLevelDeclarationAST::ClassDeclaration(class_declaration) =
                &declaration.value
            {
                for class_member in &class_declaration.members {
                    if let ClassMemberDeclarationAST::ClassMethodDeclaration(class_method) =
                        &class_member.value
                    {
                        // full method name
                        let method_name =
                            format!("{}::{}", full_qualified_name, class_method.identifier.value);

                        // the overload set symbol to insert
                        let overload_set = Symbol::OverloadSetSymbol(OverloadSetSymbol {
                            name: class_method.identifier.value.to_string(),
                            functions: Vec::new(),
                        });

                        // construct the function symbol
                        let function_symbol = {
                            // get the return type
                            let return_type = symbol_table.get_type_annotation_symbol(
                                &class_method.return_type_annotation,
                                scope_info,
                            );

                            // list of parameter
                            let mut parameters = Vec::new();

                            for parameter in &class_method.parameters {
                                parameters.push(symbol_table.get_qualified_type_annotation_symbol(
                                    &parameter.value.qualified_type_annotation,
                                    scope_info,
                                ));
                            }

                            // unwrap the Result
                            if return_type.is_err() || parameters.iter().any(|x| x.is_err()) {
                                // unwrap the error of return type
                                if let Err(error) = return_type {
                                    errors.push(error);
                                }

                                // unwrap the error of parameters
                                for parameter in parameters {
                                    if let Err(error) = parameter {
                                        errors.push(error);
                                    }
                                }

                                continue;
                            } else {
                                // everything is ok, unwrap the type
                                let return_type = return_type.ok().unwrap();
                                let mut unwrapped_parameters = Vec::new();

                                for parameter in parameters {
                                    // push to the list
                                    unwrapped_parameters.push(parameter.ok().unwrap());
                                }

                                FunctionSymbol {
                                    name: class_method.identifier.value.to_string(),
                                    return_type,
                                    parameters: unwrapped_parameters,
                                    access_modifier: class_method.access_modifier.value,
                                }
                            }
                        };

                        // try to insert new overload set symbol
                        let overload_set = match symbol_table.insert(method_name, overload_set) {
                            Ok(sym_id) => {
                                if let Symbol::OverloadSetSymbol(overload_set) =
                                    symbol_table.get_mut_by_id(sym_id).unwrap()
                                {
                                    overload_set
                                } else {
                                    unreachable!();
                                }
                            }
                            Err(sym_id) => {
                                if let Symbol::OverloadSetSymbol(overload_set) =
                                    symbol_table.get_mut_by_id(sym_id).unwrap()
                                {
                                    overload_set
                                } else {
                                    errors.push(SemanticError::SymbolRedeclaration {
                                        redeclaration_name: &class_method.identifier,
                                    });
                                    continue;
                                }
                            }
                        };

                        // check for function redeclaration
                        if {
                            let mut is_redeclaration = false;

                            for function in &overload_set.functions {
                                if function.parameters == function_symbol.parameters {
                                    is_redeclaration = true;
                                    break;
                                }
                            }

                            is_redeclaration
                        } {
                            errors.push(SemanticError::FunctionRedeclaration {
                                redeclaration_name: &class_method.identifier,
                            });
                        } else {
                            // insert the function symbol
                            overload_set.functions.push(function_symbol);
                        }
                    }
                }
            }
        }

        // pass 3: construct class layout
        for (declaration, scope_info, full_qualified_name) in &self.declarations {
            // class declaration
            let class_declaration = match &declaration.value {
                NamespaceLevelDeclarationAST::ClassDeclaration(class_declaration) => {
                    class_declaration
                }
                _ => continue,
            };

            let mut class_field_map = HashMap::new();

            // construct the class layout
            for member in &class_declaration.members {
                // class field
                let class_field = match &member.value {
                    ClassMemberDeclarationAST::ClassFieldDeclaration(class_field) => class_field,
                    _ => continue,
                };

                // get the type annotation symbol
                match symbol_table
                    .get_type_annotation_symbol(&class_field.type_annotation, scope_info)
                {
                    Ok(type_annotation_symbol) => {
                        // check if the clas field already exists
                        match class_field_map.entry(class_field.identifier.value.to_string()) {
                            Entry::Occupied(_) => {
                                errors.push(SemanticError::ClassFieldRedeclaration {
                                    redeclaration_name: &class_field.identifier,
                                });
                            }
                            Entry::Vacant(entry) => {
                                entry.insert(ClassFieldSymbol {
                                    name: class_field.identifier.value.to_string(),
                                    type_annotation_symbol,
                                    access_modifier: class_field.access_modifier.value,
                                });
                            }
                        }
                    }
                    Err(error) => {
                        errors.push(error);
                        continue;
                    }
                }
            }

            // get the class symbol
            let class_symbol = match symbol_table
                .get_mut_by_full_qualified_name(full_qualified_name)
                .unwrap()
            {
                Symbol::ClassSymbol(class_sym) => class_sym,
                _ => unreachable!("should be a class symbol"),
            };

            // assign the class field map
            class_symbol.fields = class_field_map;
        }

        // pass 4: check for recursive type in class layout
        for symbol in &symbol_table.symbols {
            let class_symbol = match symbol {
                Symbol::ClassSymbol(class_symbol) => class_symbol,
                _ => continue,
            };
        }

        (symbol_table, errors)
    }
}
