use std::collections::{hash_map::Entry, HashMap, HashSet};

use pernixc_syntactic_analysis::abstract_syntax_tree::{
    declaration::{ClassMemberDeclarationAST, NamespaceLevelDeclarationAST},
    FileAST, PositionWrapper,
};

use crate::{error::SemanticError, scope::ScopeInfo};

use super::{
    ClassFieldSymbol, ClassSymbol, FunctionSymbol, NamespaceSymbol, OverloadSetSymbol, Symbol,
    SymbolAttribute, SymbolID, SymbolTable, TypeAnnotationSymbol, TypeUnitSymbol, VariableSymbol,
    VariableSymbolID,
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
    pub fn build(self) -> (SymbolTable<'ast, 'src>, Vec<SemanticError<'ast, 'src>>)
    where
        'ast: 'src,
    {
        let mut errors = Vec::new();
        let mut symbol_table = SymbolTable {
            symbols: Vec::new(),
            qualified_name_map: HashMap::new(),
        };

        // insert the namespace into the symbol table first
        for namespace in self.namespaces {
            let _ = symbol_table.insert(
                namespace.clone(),
                Symbol::Namespace(NamespaceSymbol {
                    full_qualified_name: namespace,
                }),
            );
        }

        // pass 1: loop through the declarations and add them to the symbol table
        for (declaration, scope_info, full_qualified_name) in self.declarations {
            // the class symbol to insert
            let symbol = {
                match &declaration.value {
                    NamespaceLevelDeclarationAST::ClassDeclaration(class_decl) => {
                        Symbol::ClassSymbol(ClassSymbol {
                            full_qualified_name: full_qualified_name.clone(),
                            fields: HashMap::new(),
                            symbol_attribute: SymbolAttribute {
                            abstract_syntax_tree: PositionWrapper {
                                    value: class_decl,
                                    position: declaration.position.clone(),
                                },
                                scope_info
                            },
                        })
                    }
                    NamespaceLevelDeclarationAST::NamespaceDeclaration(_) => unreachable!("Namespace declaration should have been added to the namespace set in the pass 1."),
                }
            };

            // insert the symbol
            if let Err(sym_id) = symbol_table.insert(full_qualified_name, symbol) {
                if let Symbol::Namespace(..) = symbol_table.get_by_id(sym_id).unwrap() {
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
        for i in 0..symbol_table.symbols.len() {
            // get the class symbol
            let (class_declaration_ast, full_qualified_name, scope_info) =
                match &symbol_table.symbols[i] {
                    Symbol::ClassSymbol(class_symbol) => (
                        class_symbol.symbol_attribute.abstract_syntax_tree.value,
                        class_symbol.full_qualified_name.clone(),
                        class_symbol.symbol_attribute.scope_info.clone(),
                    ),
                    _ => continue,
                };

            for class_member in &class_declaration_ast.members {
                if let ClassMemberDeclarationAST::ClassMethodDeclaration(class_method) =
                    &class_member.value
                {
                    // full method name
                    let full_qualified_name =
                        format!("{}::{}", full_qualified_name, class_method.identifier.value);

                    // the overload set symbol to insert
                    let overload_set = Symbol::OverloadSetSymbol(OverloadSetSymbol {
                        full_qualified_name: full_qualified_name.clone(),
                        functions: Vec::new(),
                    });

                    let scope_info = ScopeInfo {
                        scope_name: full_qualified_name.clone(),
                        active_using_directives: scope_info.active_using_directives.clone(),
                        source_file: scope_info.source_file,
                    };

                    // construct the function symbol
                    let function_symbol = {
                        // get the return type
                        let return_type = symbol_table.get_type_annotation_symbol(
                            &class_method.return_type_annotation,
                            &scope_info,
                        );

                        // list of parameter
                        let mut parameters = Vec::new();

                        for parameter in &class_method.parameters {
                            parameters.push(symbol_table.get_qualified_type_annotation_symbol(
                                &parameter.value.qualified_type_annotation,
                                &scope_info,
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
                            let mut unwrapped_parameters = HashMap::new();

                            {
                                let mut idx = 0;
                                for parameter in parameters {
                                    // push to the list
                                    match unwrapped_parameters.entry(
                                        class_method.parameters[i]
                                            .value
                                            .identifier
                                            .value
                                            .to_string(),
                                    ) {
                                        Entry::Occupied(entry) => {
                                            errors.push(SemanticError::SymbolRedeclaration {
                                                redeclaration_name: &class_method.parameters[i]
                                                    .value
                                                    .identifier,
                                            });
                                        }
                                        Entry::Vacant(entry) => {
                                            entry.insert(VariableSymbol {
                                                id: VariableSymbolID { id: idx },
                                                qualified_type_annotation_symbol: parameter
                                                    .ok()
                                                    .unwrap(),
                                            });
                                        }
                                    }
                                    idx += 1;
                                }
                            }

                            FunctionSymbol {
                                full_qualified_name: full_qualified_name.clone(),
                                return_type,
                                parameters: unwrapped_parameters,
                                access_modifier: class_method.access_modifier.value,
                                symbol_attribute: SymbolAttribute {
                                    abstract_syntax_tree: PositionWrapper {
                                        value: class_method,
                                        position: class_member.position.clone(),
                                    },
                                    scope_info,
                                },
                            }
                        }
                    };

                    // try to insert new overload set symbol
                    let overload_set = match symbol_table.insert(full_qualified_name, overload_set)
                    {
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

        // pass 3: construct class layout
        for i in 0..symbol_table.symbols.len() {
            // get the class symbol
            let (class_ast, scope_info, full_qualified_name) = match &symbol_table.symbols[i] {
                Symbol::ClassSymbol(class_symbol) => (
                    class_symbol.symbol_attribute.abstract_syntax_tree.clone(),
                    class_symbol.symbol_attribute.scope_info.clone(),
                    class_symbol.full_qualified_name.clone(),
                ),
                _ => continue,
            };

            // construct the class layout
            let mut class_field_map = HashMap::new();

            // construct the class layout
            for member in &class_ast.value.members {
                // class field
                let class_field = match &member.value {
                    ClassMemberDeclarationAST::ClassFieldDeclaration(class_field) => class_field,
                    _ => continue,
                };

                // get the scope name

                // get the type annotation symbol
                match symbol_table
                    .get_type_annotation_symbol(&class_field.type_annotation, &scope_info)
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
                                let scope_info = ScopeInfo {
                                    scope_name: full_qualified_name.clone(),
                                    active_using_directives: scope_info
                                        .active_using_directives
                                        .clone(),
                                    source_file: scope_info.source_file,
                                };

                                entry.insert(ClassFieldSymbol {
                                    name: class_field.identifier.value.to_string(),
                                    type_annotation_symbol,
                                    access_modifier: class_field.access_modifier.value,
                                    symbol_attribute: SymbolAttribute {
                                        abstract_syntax_tree: PositionWrapper {
                                            value: class_field,
                                            position: member.position.clone(),
                                        },
                                        scope_info,
                                    },
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

            // assign the class field map
            match symbol_table.symbols.get_mut(i).unwrap() {
                Symbol::ClassSymbol(class_symbol) => {
                    class_symbol.fields = class_field_map;
                }
                _ => unreachable!(),
            }
        }

        // pass 4: check for recursive type in class layout
        for (idx, symbol) in symbol_table.symbols.iter().enumerate() {
            if !matches!(symbol, Symbol::ClassSymbol(_)) {
                continue;
            }

            Self::is_recursive(
                &symbol_table,
                &mut Vec::new(),
                &mut errors,
                SymbolID { id: idx },
            );
        }

        (symbol_table, errors)
    }

    fn is_recursive(
        symbol_table: &SymbolTable,
        visited_symbols: &mut Vec<SymbolID>,
        errors: &mut Vec<SemanticError<'src, 'ast>>,
        symbol_id: SymbolID,
    ) where
        'src: 'ast,
    {
        // check if the symbol is already visited
        if visited_symbols.contains(&symbol_id) {
            // error with these recursive symbols might have already been reported
            // we must check if the error has already been reported with these symbols
            let has_been_reported = {
                let mut has_been_reported = false;

                for error in errors.iter() {
                    if let SemanticError::RecursiveType {
                        recursive_class_symbol_ids,
                    } = error
                    {
                        if recursive_class_symbol_ids.len() != visited_symbols.len() {
                            continue;
                        }

                        for idx in visited_symbols.iter() {
                            if !recursive_class_symbol_ids.contains(idx) {
                                continue;
                            }
                        }

                        has_been_reported = true;
                        break;
                    }
                }

                has_been_reported
            };

            if !has_been_reported {
                errors.push(SemanticError::RecursiveType {
                    recursive_class_symbol_ids: visited_symbols.clone(),
                });
            }

            return;
        }

        // get the class symbol
        if let Symbol::ClassSymbol(class_symbol) = symbol_table.get_by_id(symbol_id).unwrap() {
            // append the symbol id to the visited symbols
            visited_symbols.push(symbol_id);

            // check for recursive type in class layout
            for class_field in class_symbol.fields.values() {
                // get the type of the field
                let type_annotation_symbol = &class_field.type_annotation_symbol;

                match type_annotation_symbol {
                    TypeAnnotationSymbol::TypeUnitSymbol(type_unit) => match type_unit {
                        TypeUnitSymbol::UserDefinedTypeUnit(type_sym) => {
                            Self::is_recursive(symbol_table, visited_symbols, errors, *type_sym);
                        }
                        _ => (),
                    },
                }
            }

            // pop the symbol id from the visited symbols
            visited_symbols.pop();
        }
    }
}
