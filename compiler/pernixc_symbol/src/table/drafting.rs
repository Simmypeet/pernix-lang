use std::{collections::HashMap, sync::Arc};

use pernixc_lexical::token::Identifier;
use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::{
    item::{self, Item},
    target::{File, Target},
    ConnectedList,
};
use pernixc_system::{arena, diagnostic::Handler};

use super::Table;
use crate::{
    error::{Error, LifetimeParameterShadowing, SymbolRedefinition, TypeParameterShadowing},
    ty::{self, PrimitiveType},
    Accessibility, Enum, EnumVariant, Field, Function, FunctionSignature,
    FunctionSignatureSyntaxTree, GenericParameters, GenericableID, Generics, LifetimeParameter,
    Module, ModuleChildID, Parameter, Struct, Trait, TraitFunction, TraitType, Type, TypeParameter,
    ID,
};

#[cfg(test)]
mod input;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum SymbolState {
    Drafted,
    Constructing { constructing_order: usize },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct States {
    pub(super) symbol_states_by_id: HashMap<ID, SymbolState>,
    pub(super) current_constructing_order: usize,
}

#[derive(Debug, Clone)]
pub(super) struct ImplementsSyntaxTreeWithModuleID {
    /// The module where the implements item is defined.
    pub(super) module_id: arena::ID<Module>,

    /// The syntax tree of the implements item.
    pub(super) implements: item::Implements,
}

impl Table {
    pub(super) fn draft_symbols(
        &mut self,
        targets: Vec<Target>,
        handler: &impl Handler<Error>,
    ) -> (States, Vec<ImplementsSyntaxTreeWithModuleID>) {
        let mut symbol_states_by_id = HashMap::new();
        let mut implements_syntax_tree_with_module_ids = Vec::new();

        // iterate through each target
        for target in targets {
            let (file, target_name) = target.dissolve();

            let module_id = self.target_root_module_ids_by_name[&target_name];

            self.draft_symbols_in_file(
                file,
                module_id,
                &mut implements_syntax_tree_with_module_ids,
                &mut symbol_states_by_id,
                handler,
            );
        }

        (
            States {
                symbol_states_by_id,
                current_constructing_order: 0,
            },
            implements_syntax_tree_with_module_ids,
        )
    }

    fn draft_symbols_in_file(
        &mut self,
        current_file: File,
        module_id: arena::ID<Module>,
        implements_syntax_tree_with_module_ids: &mut Vec<ImplementsSyntaxTreeWithModuleID>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl Handler<Error>,
    ) {
        for submodule in current_file.submodules {
            let module = &self.modules[module_id];

            let submodule_id = module.module_child_ids_by_name
                [submodule.module.identifier.span.str()]
            .into_module()
            .unwrap();

            self.draft_symbols_in_file(
                submodule.file,
                submodule_id,
                implements_syntax_tree_with_module_ids,
                symbol_states_by_id,
                handler,
            );
        }

        for item in current_file.items {
            // extract implements out
            let item = match item.into_implements() {
                Ok(implements) => {
                    implements_syntax_tree_with_module_ids.push(ImplementsSyntaxTreeWithModuleID {
                        module_id,
                        implements,
                    });

                    continue;
                }
                Err(item) => item,
            };

            // the name of the symbol
            let name = {
                let identifier = match &item {
                    Item::Trait(i) => &i.signature.identifier,
                    Item::Function(i) => &i.signature.identifier,
                    Item::Type(i) => &i.signature.identifier,
                    Item::Struct(i) => &i.signature.identifier,
                    Item::Enum(i) => &i.signature.identifier,

                    Item::Implements(_) => unreachable!(),
                };

                let module = &self.modules[module_id];

                // redefinition check
                if let Err(error) =
                    Self::redefinition_check(&module.module_child_ids_by_name, identifier)
                {
                    handler.recieve(Error::SymbolRedefinition(error));
                    continue;
                }

                identifier.span.str().to_string()
            };

            let module_child_id = match item {
                Item::Trait(i) => self.draft_trait(i, module_id, handler).into(),
                Item::Function(i) => self.draft_function(i, module_id, handler).into(),
                Item::Type(i) => self.draft_type(i, module_id, handler).into(),
                Item::Struct(i) => self.draft_struct(i, module_id, handler).into(),
                Item::Enum(i) => self.draft_enum(i, module_id, handler).into(),

                Item::Implements(..) => unreachable!(),
            };

            // add the symbol as a child of the module
            assert!(
                self.modules[module_id]
                    .module_child_ids_by_name
                    .insert(name, module_child_id)
                    .is_none(),
                "redefinition should be handled already"
            );

            match module_child_id {
                ModuleChildID::Struct(i) => assert!(symbol_states_by_id
                    .insert(i.into(), SymbolState::Drafted)
                    .is_none()),
                ModuleChildID::Function(i) => assert!(symbol_states_by_id
                    .insert(i.into(), SymbolState::Drafted)
                    .is_none()),
                ModuleChildID::Type(i) => assert!(symbol_states_by_id
                    .insert(i.into(), SymbolState::Drafted)
                    .is_none()),
                ModuleChildID::Trait(i) => assert!(symbol_states_by_id
                    .insert(i.into(), SymbolState::Drafted)
                    .is_none()),
                ModuleChildID::Enum(..) => (),
                ModuleChildID::Module(..) => {
                    unreachable!()
                }
            };
        }
    }

    fn check_type_parameter_shadowing(
        &self,
        mut parent: ID,
        identifier: &Identifier,
    ) -> Result<(), Error> {
        loop {
            if let Ok(genericable_id) = parent.try_into() {
                if let Some(shadowed_type_parameter_id) = self
                    .get_genericable(genericable_id)
                    .unwrap()
                    .generic_parameters()
                    .type_parameter_ids_by_name
                    .get(identifier.span.str())
                    .copied()
                {
                    break Err(Error::TypeParameterShadowing(TypeParameterShadowing {
                        span: identifier.span.clone(),
                        shadowed_type_parameter_id,
                    }));
                }
            }
            match self.get_symbol(parent).unwrap().parent_symbol() {
                Some(new_parent_id) => parent = new_parent_id,
                None => break Ok(()),
            }
        }
    }

    fn check_lifetime_parameter_shadowing(
        &self,
        mut parent: ID,
        identifier: &Identifier,
    ) -> Result<(), Error> {
        loop {
            if let Ok(genericable_id) = parent.try_into() {
                if let Some(shadowed_lifetime_parameter_id) = self
                    .get_genericable(genericable_id)
                    .unwrap()
                    .generic_parameters()
                    .lifetime_parameter_ids_by_name
                    .get(identifier.span.str())
                    .copied()
                {
                    break Err(Error::LifetimeParameterShadowing(
                        LifetimeParameterShadowing {
                            span: identifier.span.clone(),
                            shadowed_lifetime_parameter_id,
                        },
                    ));
                }
            }
            match self.get_symbol(parent).unwrap().parent_symbol() {
                Some(new_parent_id) => parent = new_parent_id,
                None => break Ok(()),
            }
        }
    }

    pub(super) fn create_generic_parameters(
        &mut self,
        parent_genericable_id: GenericableID,
        generic_parameters_syntax_tree: &item::GenericParameters,
        handler: &impl Handler<Error>,
    ) -> GenericParameters {
        let mut generic_parameters = GenericParameters::default();

        for generic_parameter in generic_parameters_syntax_tree.parameter_list.elements() {
            match generic_parameter {
                item::GenericParameter::Lifetime(lt) => {
                    // lifetime must be declared prior to type parameters
                    if !generic_parameters.type_parameter_ids_by_name.is_empty() {
                        handler.recieve(
                            Error::LifetimeParameterMustBeDeclaredPriotToTypeParameter(
                                crate::error::LifetimeParameterMustBeDeclaredPriotToTypeParameter {
                                    lifetime_parameter_span: lt.span().unwrap(),
                                },
                            ),
                        );
                    }

                    // redefinition check
                    if let Err(error) = Self::redefinition_check(
                        &generic_parameters.lifetime_parameter_ids_by_name,
                        &lt.identifier,
                    ) {
                        handler.recieve(Error::LifetimeParameterRedefinition(error));
                        continue;
                    }

                    if let Err(error) = self.check_lifetime_parameter_shadowing(
                        self.get_genericable(parent_genericable_id)
                            .unwrap()
                            .parent_symbol()
                            .unwrap(),
                        &lt.identifier,
                    ) {
                        handler.recieve(error);
                        continue;
                    }

                    // add lifetime parameter
                    let lifetime_parameter_id = self.lifetime_parameters.push(LifetimeParameter {
                        name: lt.identifier.span.str().to_string(),
                        parent_genericable_id,
                    });

                    generic_parameters
                        .lifetime_parameter_ids_by_name
                        .insert(lt.identifier.span.str().to_string(), lifetime_parameter_id);

                    generic_parameters
                        .lifetime_parameter_order
                        .push(lifetime_parameter_id);
                }
                item::GenericParameter::Type(ty) => {
                    // redefinition check
                    if let Err(error) = Self::redefinition_check(
                        &generic_parameters.type_parameter_ids_by_name,
                        &ty.identifier,
                    ) {
                        handler.recieve(Error::TypeParameterRedefinition(error));
                        continue;
                    }

                    if let Err(error) = self.check_type_parameter_shadowing(
                        self.get_genericable(parent_genericable_id)
                            .unwrap()
                            .parent_symbol()
                            .unwrap(),
                        &ty.identifier,
                    ) {
                        handler.recieve(error);
                        continue;
                    }

                    let type_parameter_id = self.type_parameters.push(TypeParameter {
                        name: ty.identifier.span.str().to_string(),
                        parent_genericable_id,
                    });

                    generic_parameters
                        .type_parameter_ids_by_name
                        .insert(ty.identifier.span.str().to_string(), type_parameter_id);

                    generic_parameters
                        .type_parameter_order
                        .push(type_parameter_id);
                }
            }
        }

        generic_parameters
    }

    fn draft_type(
        &mut self,
        type_syntax_tree: item::Type,
        parent_module_id: arena::ID<Module>,
        handler: &impl Handler<Error>,
    ) -> arena::ID<Type> {
        let type_syntax_tree = Arc::new(type_syntax_tree);

        let type_id = self.types.push(Type {
            name: type_syntax_tree.signature.identifier.span.str().to_string(),
            accessibility: Accessibility::from_syntax_tree(&type_syntax_tree.access_modifier),
            parent_module_id,
            alias: ty::Type::PrimitiveType(PrimitiveType::Void), // to be filled later
            generic_parameters: GenericParameters::default(),    // to be filled later
            syntax_tree: Some(type_syntax_tree.clone()),
        });

        // update generic parameters
        if let Some(generic_parameters) = type_syntax_tree.signature.generic_parameters.as_ref() {
            let generic_parameters =
                self.create_generic_parameters(type_id.into(), generic_parameters, handler);

            self.types[type_id].generic_parameters = generic_parameters;
        }

        type_id
    }

    fn draft_trait_function(
        &mut self,
        function_signature_syntax_tree: item::FunctionSignature,
        parent_trait_id: arena::ID<Trait>,
        handler: &impl Handler<Error>,
    ) -> arena::ID<TraitFunction> {
        let function_signature_syntax_tree_without_parameters =
            Arc::new(FunctionSignatureSyntaxTree {
                identifier: function_signature_syntax_tree.identifier,
                generic_parameters: function_signature_syntax_tree.generic_parameters,
                return_type: function_signature_syntax_tree.return_type,
                where_clause: function_signature_syntax_tree.where_clause,
            });

        let trait_function_id = self.trait_functions.push(TraitFunction {
            function_signature: FunctionSignature {
                name: function_signature_syntax_tree_without_parameters
                    .identifier
                    .span
                    .str()
                    .to_string(),
                parameter_ids_by_name: HashMap::new(), // to be filled later
                parameter_order: Vec::new(),           // to be filled later
                return_type: ty::Type::PrimitiveType(PrimitiveType::Void), // to be filled later
                syntax_tree: Some(function_signature_syntax_tree_without_parameters.clone()),
                generics: Generics::default(), // to be filled later
            },
            parent_trait_id,
        });

        // Update trait function with generics
        if let Some(generic_parameters) = function_signature_syntax_tree_without_parameters
            .generic_parameters
            .as_ref()
        {
            let generic_parameters = self.create_generic_parameters(
                trait_function_id.into(),
                generic_parameters,
                handler,
            );

            self.trait_functions[trait_function_id]
                .function_signature
                .generics
                .parameters = generic_parameters;
        }

        for parameter in function_signature_syntax_tree
            .parameters
            .parameter_list
            .into_iter()
            .flat_map(ConnectedList::into_elements)
        {
            let trait_function_symbol = &mut self.trait_functions[trait_function_id];

            // check for redefinition
            if let Err(error) = Self::redefinition_check(
                &trait_function_symbol
                    .function_signature
                    .parameter_ids_by_name,
                &parameter.identifier,
            ) {
                handler.recieve(Error::ParameterRedefinition(error));
                continue;
            }

            let parameter_name = parameter.identifier.span.str().to_string();

            let parameter_id = self.parameters.push(Parameter {
                name: parameter_name.clone(),
                parameter_parent_id: trait_function_id.into(),
                declaration_order: trait_function_symbol
                    .function_signature
                    .parameter_order
                    .len(),
                ty: ty::Type::PrimitiveType(PrimitiveType::Void),
                is_mutable: parameter.mutable_keyword.is_some(),
                syntax_tree: Some(Arc::new(parameter)),
            });

            // add parameter to the function signature
            trait_function_symbol
                .function_signature
                .parameter_ids_by_name
                .insert(parameter_name, parameter_id);
            trait_function_symbol
                .function_signature
                .parameter_order
                .push(parameter_id);
        }

        trait_function_id
    }

    fn draft_trait_type(
        &mut self,
        trait_type_syntax_tree: item::TraitType,
        parent_trait_id: arena::ID<Trait>,
        handler: &impl Handler<Error>,
    ) -> arena::ID<TraitType> {
        let trait_type_syntax_tree = Arc::new(trait_type_syntax_tree);
        let trait_type_id = self.trait_types.push(TraitType {
            name: trait_type_syntax_tree
                .type_signature
                .identifier
                .span
                .str()
                .to_string(),
            generic_parameters: GenericParameters::default(), // to be filled later
            parent_trait_id,
            syntax_tree: Some(trait_type_syntax_tree.clone()),
        });

        if let Some(generic_parameters) = trait_type_syntax_tree
            .type_signature
            .generic_parameters
            .as_ref()
        {
            let generic_parameters =
                self.create_generic_parameters(trait_type_id.into(), generic_parameters, handler);

            self.trait_types[trait_type_id].generic_parameters = generic_parameters;
        }

        trait_type_id
    }

    fn draft_trait(
        &mut self,
        trait_syntax_tree: item::Trait,
        parent_module_id: arena::ID<Module>,
        handler: &impl Handler<Error>,
    ) -> arena::ID<Trait> {
        let trait_signature = Arc::new(trait_syntax_tree.signature);
        let trait_id = self.traits.push(crate::Trait {
            name: trait_signature.identifier.span.str().to_string(),
            parent_module_id,
            generics: Generics::default(), // to be filled later
            implements: Vec::new(),        // to be filled later
            syntax_tree: Some(trait_signature.clone()),
            accessibility: Accessibility::from_syntax_tree(&trait_syntax_tree.access_modifier),
            trait_member_ids_by_name: HashMap::new(), // to be filled later
        });

        // update generic parameters
        if let Some(generic_parameters) = trait_signature.generic_parameters.as_ref() {
            let generic_parameters =
                self.create_generic_parameters(trait_id.into(), generic_parameters, handler);

            self.traits[trait_id].generics.parameters = generic_parameters;
        }

        for trait_member in trait_syntax_tree.body.members {
            let trait_symbol = &mut self.traits[trait_id];
            let identifier = match &trait_member {
                item::TraitMember::Function(f) => &f.function_signature.identifier,
                item::TraitMember::Type(t) => &t.type_signature.identifier,
            };
            let trait_member_name = identifier.span.str().to_string();

            // redefinition check
            if let Err(error) =
                Self::redefinition_check(&trait_symbol.trait_member_ids_by_name, identifier)
            {
                handler.recieve(Error::TraitMemberRedefinition(error));
                continue;
            }

            let trait_member_id = match trait_member {
                item::TraitMember::Function(f) => self
                    .draft_trait_function(f.function_signature, trait_id, handler)
                    .into(),
                item::TraitMember::Type(t) => self.draft_trait_type(t, trait_id, handler).into(),
            };

            self.traits[trait_id]
                .trait_member_ids_by_name
                .insert(trait_member_name, trait_member_id);
        }

        trait_id
    }

    fn draft_function(
        &mut self,
        function_syntax_tree: item::Function,
        parent_module_id: arena::ID<Module>,
        handler: &impl Handler<Error>,
    ) -> arena::ID<Function> {
        // function signature syntax tree (without parameters)
        let function_signature_syntax_tree = Arc::new(FunctionSignatureSyntaxTree {
            identifier: function_syntax_tree.signature.identifier,
            generic_parameters: function_syntax_tree.signature.generic_parameters,
            return_type: function_syntax_tree.signature.return_type,
            where_clause: function_syntax_tree.signature.where_clause,
        });

        let function_id = self.functions.push(crate::Function {
            function_signature: crate::FunctionSignature {
                name: function_signature_syntax_tree
                    .identifier
                    .span
                    .str()
                    .to_string(),
                parameter_ids_by_name: HashMap::new(), // to be filled later
                parameter_order: Vec::new(),           // to be filled later
                return_type: ty::Type::PrimitiveType(PrimitiveType::Void),
                syntax_tree: Some(function_signature_syntax_tree.clone()),
                generics: Generics::default(), // to be filled later
            },
            parent_module_id,
            syntax_tree: Some(Arc::new(function_syntax_tree.body)),
            accessibility: Accessibility::from_syntax_tree(&function_syntax_tree.access_modifier),
        });

        // create function generics
        if let Some(generic_parameters) = function_signature_syntax_tree.generic_parameters.as_ref()
        {
            let generic_parameters =
                self.create_generic_parameters(function_id.into(), generic_parameters, handler);

            self.functions[function_id].generics.parameters = generic_parameters;
        }

        for parameter in function_syntax_tree
            .signature
            .parameters
            .parameter_list
            .into_iter()
            .flat_map(ConnectedList::into_elements)
        {
            let function = &mut self.functions[function_id];

            // redefinition check
            if let Err(error) =
                Self::redefinition_check(&function.parameter_ids_by_name, &parameter.identifier)
            {
                handler.recieve(Error::ParameterRedefinition(error));
                continue;
            }

            let parameter_name = parameter.identifier.span.str().to_string();

            let parameter_id = self.parameters.push(Parameter {
                name: parameter_name.clone(),
                parameter_parent_id: function_id.into(),
                declaration_order: function.parameter_order.len(),
                ty: ty::Type::PrimitiveType(PrimitiveType::Void), // to be replaced
                is_mutable: parameter.mutable_keyword.is_some(),
                syntax_tree: Some(Arc::new(parameter)),
            });

            function
                .parameter_ids_by_name
                .insert(parameter_name, parameter_id);
            function.parameter_order.push(parameter_id);
        }

        function_id
    }

    fn draft_struct(
        &mut self,
        struct_syntax_tree: item::Struct,
        parent_module_id: arena::ID<Module>,
        handler: &impl Handler<Error>,
    ) -> arena::ID<Struct> {
        let struct_signature = Arc::new(struct_syntax_tree.signature);
        let struct_id = self.structs.push(Struct {
            name: struct_signature.identifier.span.str().to_string(),
            accessibility: Accessibility::from_syntax_tree(&struct_syntax_tree.access_modifier),
            parent_module_id,
            field_ids_by_name: HashMap::new(), // will be filled later.
            field_order: Vec::new(),           // will be filled later.
            generics: Generics::default(),     // type parameters will be filled later.
            syntax_tree: Some(struct_signature.clone()),
        });

        // update the generic parameters
        if let Some(generic_parameters) = struct_signature.generic_parameters.as_ref() {
            let generic_parameters =
                self.create_generic_parameters(struct_id.into(), generic_parameters, handler);

            self.structs[struct_id].generics.parameters = generic_parameters;
        }

        for member in struct_syntax_tree.body.members {
            match member {
                item::StructMember::Field(field) => {
                    let struct_symbol = &mut self.structs[struct_id];

                    // redefinition check
                    if let Err(err) = Self::redefinition_check(
                        &struct_symbol.field_ids_by_name,
                        &field.identifier,
                    ) {
                        handler.recieve(Error::FieldRedefinition(err));
                        continue;
                    }

                    let field_name = field.identifier.span.str().to_string();
                    let field_accessibility =
                        Accessibility::from_syntax_tree(&field.access_modifier);

                    // add the field to the struct
                    let field_id = self.fields.push(Field {
                        name: field_name.clone(),
                        accessibility: field_accessibility,
                        parent_struct_id: struct_id,
                        syntax_tree: Some(Arc::new(field)),
                        declaration_order: struct_symbol.field_order.len(),
                        ty: ty::Type::PrimitiveType(PrimitiveType::Void), // will be replaced later
                    });

                    // add the field to the struct
                    struct_symbol.field_ids_by_name.insert(field_name, field_id);
                    struct_symbol.field_order.push(field_id);

                    // accessibility check
                    if struct_symbol.accessibility.rank() < field_accessibility.rank() {
                        handler.recieve(Error::FieldMoreAccessibleThanStruct(
                            crate::error::FieldMoreAccessibleThanStruct {
                                field_id,
                                struct_id,
                            },
                        ));
                    }
                }
            }
        }

        struct_id
    }

    fn redefinition_check<T: Clone + Into<U>, U>(
        map: &HashMap<String, T>,
        name: &Identifier,
    ) -> Result<(), SymbolRedefinition<U>> {
        map.get(name.span.str())
            .cloned()
            .map_or(Ok(()), |previous_definition_id| {
                Err(SymbolRedefinition {
                    previous_definition_id: previous_definition_id.into(),
                    redefinition_span: name.span.clone(),
                })
            })
    }

    fn draft_enum(
        &mut self,
        enum_syntax_tree: item::Enum,
        parent_module_id: arena::ID<Module>,
        handler: &impl Handler<Error>,
    ) -> arena::ID<Enum> {
        let enum_id = self.enums.push(Enum {
            name: enum_syntax_tree.signature.identifier.span.str().to_string(),
            accessibility: Accessibility::from_syntax_tree(&enum_syntax_tree.access_modifier),
            parent_module_id,
            syntax_tree: Some(Arc::new(enum_syntax_tree.signature)),
            variant_ids_by_name: HashMap::new(), // will be filled later
            variant_order: Vec::new(),           // will be filled later,
        });

        for variant in enum_syntax_tree
            .body
            .variant_list
            .into_iter()
            .flat_map(ConnectedList::into_elements)
        {
            let enum_symbol = &mut self.enums[enum_id];

            if let Err(error) = Self::redefinition_check(&enum_symbol.variant_ids_by_name, &variant)
            {
                handler.recieve(Error::SymbolRedefinition(error));
                continue;
            };

            let name = variant.span.str().to_string();

            // adds variant to enum
            let variant_id = self.enum_variants.push(EnumVariant {
                name: name.clone(),
                parent_enum_id: enum_id,
                declaration_order: enum_symbol.variant_order.len(),
                syntax_tree: Some(Arc::new(variant)),
            });

            enum_symbol.variant_ids_by_name.insert(name, variant_id);
            enum_symbol.variant_order.push(variant_id);
        }

        enum_id
    }
}

#[cfg(test)]
mod tests;
