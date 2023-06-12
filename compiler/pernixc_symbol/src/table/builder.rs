use std::{collections::HashMap, sync::Arc};

use pernixc_lexical::token::Identifier;
use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::{
    item::{self, Item},
    target::{File, Target},
    ConnectedList,
};
use pernixc_system::diagnostic::Handler;

use super::Table;
use crate::{
    error::{Error, SymbolRedefinition},
    ty::{self, PrimitiveType},
    Accessibility, Enum, EnumID, EnumVariant, Field, FunctionID, FunctionSignature,
    FunctionSignatureSyntaxTree, GenericParameters, GenericableID, Generics, GlobalID,
    LifetimeParameter, ModuleID, Parameter, Struct, StructID, TraitFunction, TraitFunctionID,
    TraitID, TraitType, TraitTypeID, Type, TypeID, TypeParameter, ID,
};

pub(super) enum SymbolState {
    Drafted,
    Constructing,
}

pub(super) struct Drafting {
    symbol_states_by_id: HashMap<ID, SymbolState>,
    implements_vecs_by_module_id: HashMap<ModuleID, Vec<item::Implements>>,
}

impl Table {
    pub(super) fn draft_symbols(
        &mut self,
        targets: Vec<Target>,
        handler: &impl Handler<Error>,
    ) -> Drafting {
        let mut symbol_states_by_id = HashMap::new();
        // implements item will be handled separately
        let mut implements_vecs_by_module_id = HashMap::new();

        // iterate through each target
        for target in targets {
            let (file, target_name) = target.dissolve();

            let module_id = self.root_module_ids_by_target_name[&target_name];

            self.draft_symbols_in_file(
                file,
                module_id,
                &mut implements_vecs_by_module_id,
                &mut symbol_states_by_id,
                handler,
            );
        }

        Drafting {
            symbol_states_by_id,
            implements_vecs_by_module_id,
        }
    }

    fn draft_symbols_in_file(
        &mut self,
        current_file: File,
        module_id: ModuleID,
        implements_vecs_by_module_id: &mut HashMap<ModuleID, Vec<item::Implements>>,
        symbol_states_by_id: &mut HashMap<ID, SymbolState>,
        handler: &impl Handler<Error>,
    ) {
        for submodule in current_file.submodules {
            let module = &self.modules[module_id];

            let submodule_id = module.child_ids_by_name[submodule.module.identifier.span.str()]
                .into_module()
                .unwrap();

            self.draft_symbols_in_file(
                submodule.file,
                submodule_id,
                implements_vecs_by_module_id,
                symbol_states_by_id,
                handler,
            );
        }

        for item in current_file.items {
            // extract implements out
            let item = match item.into_implements() {
                Ok(implements) => {
                    implements_vecs_by_module_id
                        .entry(module_id)
                        .or_default()
                        .push(implements);

                    continue;
                }
                Err(item) => item,
            };

            // the name of the symbol
            let name = {
                let identifier = match &item {
                    Item::Trait(i) => &i.trait_signature.identifier,
                    Item::Function(i) => &i.signature.identifier,
                    Item::Type(i) => &i.type_signature.identifier,
                    Item::Struct(i) => &i.struct_signature.identifier,
                    Item::Enum(i) => &i.enum_signature.identifier,

                    Item::Implements(_) => unreachable!(),
                };

                let module = &self.modules[module_id];

                // redefinition check
                if let Err(error) = Self::redefinition_check(&module.child_ids_by_name, identifier)
                {
                    handler.recieve(Error::SymbolRedefinition(error));
                    continue;
                }

                identifier.span.str().to_string()
            };

            let global_id = match item {
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
                    .child_ids_by_name
                    .insert(name, global_id)
                    .is_none(),
                "redefinition should be handled already"
            );

            match global_id {
                GlobalID::Struct(i) => assert!(symbol_states_by_id
                    .insert(i.into(), SymbolState::Drafted)
                    .is_none()),
                GlobalID::Function(i) => assert!(symbol_states_by_id
                    .insert(i.into(), SymbolState::Drafted)
                    .is_none()),
                GlobalID::Type(i) => assert!(symbol_states_by_id
                    .insert(i.into(), SymbolState::Drafted)
                    .is_none()),
                GlobalID::Trait(i) => assert!(symbol_states_by_id
                    .insert(i.into(), SymbolState::Drafted)
                    .is_none()),
                GlobalID::Enum(..) => (),

                GlobalID::EnumVariant(..)
                | GlobalID::TraitFunction(..)
                | GlobalID::TraitType(..)
                | GlobalID::Module(..) => {
                    unreachable!()
                }
            };
        }
    }

    fn create_generic_parameters(
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
                        &generic_parameters.lifetime_parameter_id_by_name,
                        &lt.identifier,
                    ) {
                        handler.recieve(Error::LifetimeParameterRedefinition(error));
                        continue;
                    }

                    // add lifetime parameter
                    let lifetime_parameter_id =
                        self.lifetime_parameters.insert(LifetimeParameter {
                            name: lt.identifier.span.str().to_string(),
                            parent_genericable_id,
                        });

                    generic_parameters
                        .lifetime_parameter_id_by_name
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

                    let type_parameter_id = self.type_parameters.insert(TypeParameter {
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
        parent_module_id: ModuleID,
        handler: &impl Handler<Error>,
    ) -> TypeID {
        let type_syntax_tree = Arc::new(type_syntax_tree);

        let type_id = self.types.insert(Type {
            name: type_syntax_tree
                .type_signature
                .identifier
                .span
                .str()
                .to_string(),
            accessibility: Accessibility::from_syntax_tree(&type_syntax_tree.access_modifier),
            parent_module_id,
            alias: ty::Type::PrimitiveType(PrimitiveType::Void), // to be filled later
            generic_parameters: GenericParameters::default(),    // to be filled later
            syntax_tree: type_syntax_tree.clone(),
        });

        // update generic parameters
        if let Some(generic_parameters) =
            type_syntax_tree.type_signature.generic_parameters.as_ref()
        {
            let generic_parameters =
                self.create_generic_parameters(type_id.into(), generic_parameters, handler);

            self.types[type_id].generic_parameters = generic_parameters;
        }

        type_id
    }

    fn draft_trait_function(
        &mut self,
        function_signature_syntax_tree: item::FunctionSignature,
        parent_trait_id: TraitID,
        handler: &impl Handler<Error>,
    ) -> TraitFunctionID {
        let function_signature_syntax_tree_without_parameters =
            Arc::new(FunctionSignatureSyntaxTree {
                identifier: function_signature_syntax_tree.identifier,
                generic_parameters: function_signature_syntax_tree.generic_parameters,
                return_type: function_signature_syntax_tree.return_type,
                where_clause: function_signature_syntax_tree.where_clause,
            });

        let trait_function_id = self.trait_functions.insert(TraitFunction {
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
                .generic_parameters = generic_parameters;
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

            let parameter_id = self.parameters.insert(Parameter {
                name: parameter_name.clone(),
                parent_parameter_id: trait_function_id.into(),
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
        parent_trait_id: TraitID,
        handler: &impl Handler<Error>,
    ) -> TraitTypeID {
        let trait_type_syntax_tree = Arc::new(trait_type_syntax_tree);
        let trait_type_id = self.trait_types.insert(TraitType {
            name: trait_type_syntax_tree
                .type_signature
                .identifier
                .span
                .str()
                .to_string(),
            generic_parameters: GenericParameters::default(), // to be filled later
            parent_trait_id,
            syntax_tree: trait_type_syntax_tree.clone(),
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
        parent_module_id: ModuleID,
        handler: &impl Handler<Error>,
    ) -> TraitID {
        let trait_signature = Arc::new(trait_syntax_tree.trait_signature);
        let trait_id = self.traits.insert(crate::Trait {
            name: trait_signature.identifier.span.str().to_string(),
            parent_module_id,
            generics: Generics::default(), // to be filled later
            implements: Vec::new(),        // to be filled later
            syntax_tree: Some(trait_signature.clone()),
            trait_member_ids_by_name: HashMap::new(), // to be filled later
        });

        // update generic parameters
        if let Some(generic_parameters) = trait_signature.generic_parameters.as_ref() {
            let generic_parameters =
                self.create_generic_parameters(trait_id.into(), generic_parameters, handler);

            self.traits[trait_id].generics.generic_parameters = generic_parameters;
        }

        for trait_member in trait_syntax_tree.trait_body.trait_members {
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
        parent_module_id: ModuleID,
        handler: &impl Handler<Error>,
    ) -> FunctionID {
        // function signature syntax tree (without parameters)
        let function_signature_syntax_tree = Arc::new(FunctionSignatureSyntaxTree {
            identifier: function_syntax_tree.signature.identifier,
            generic_parameters: function_syntax_tree.signature.generic_parameters,
            return_type: function_syntax_tree.signature.return_type,
            where_clause: function_syntax_tree.signature.where_clause,
        });

        let function_id = self.functions.insert(crate::Function {
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
            syntax_tree: Arc::new(function_syntax_tree.body),
            accessibility: Accessibility::from_syntax_tree(&function_syntax_tree.access_modifier),
        });

        // create function generics
        if let Some(generic_parameters) = function_signature_syntax_tree.generic_parameters.as_ref()
        {
            let generic_parameters =
                self.create_generic_parameters(function_id.into(), generic_parameters, handler);

            self.functions[function_id].generics.generic_parameters = generic_parameters;
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

            let parameter_id = self.parameters.insert(Parameter {
                name: parameter_name.clone(),
                parent_parameter_id: function_id.into(),
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
        parent_module_id: ModuleID,
        handler: &impl Handler<Error>,
    ) -> StructID {
        let struct_signature = Arc::new(struct_syntax_tree.struct_signature);
        let struct_id = self.structs.insert(Struct {
            name: struct_signature.identifier.span.str().to_string(),
            accessibility: Accessibility::from_syntax_tree(&struct_syntax_tree.access_modifier),
            parent_module_id,
            field_ids_by_name: HashMap::new(), // will be filled later.
            field_order: Vec::new(),           // will be filled later.
            generics: Generics::default(),     // type parameters will be filled later.
            syntax_tree: struct_signature.clone(),
        });

        // update the generic parameters
        if let Some(generic_parameters) = struct_signature.generic_parameters.as_ref() {
            let generic_parameters =
                self.create_generic_parameters(struct_id.into(), generic_parameters, handler);

            self.structs[struct_id].generics.generic_parameters = generic_parameters;
        }

        for member in struct_syntax_tree.struct_body.struct_members {
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
                    let field_id = self.fields.insert(Field {
                        name: field_name.clone(),
                        accessibility: field_accessibility,
                        parent_struct_id: struct_id,
                        syntax_tree: Arc::new(field),
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
        parent_module_id: ModuleID,
        handler: &impl Handler<Error>,
    ) -> EnumID {
        let enum_id = self.enums.insert(Enum {
            name: enum_syntax_tree
                .enum_signature
                .identifier
                .span
                .str()
                .to_string(),
            accessibility: Accessibility::from_syntax_tree(&enum_syntax_tree.access_modifier),
            parent_module_id,
            syntax_tree: enum_syntax_tree.enum_signature,
            variant_ids_by_name: HashMap::new(), // will be filled later
            variant_order: Vec::new(),           // will be filled later,
        });

        for variant in enum_syntax_tree
            .enum_body
            .enum_variant_list
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
            let variant_id = self.enum_variants.insert(EnumVariant {
                name: name.clone(),
                parent_enum_id: enum_id,
                declaration_order: enum_symbol.variant_order.len(),
                syntax_tree: Arc::new(variant),
            });

            enum_symbol.variant_ids_by_name.insert(name, variant_id);
        }

        enum_id
    }
}
