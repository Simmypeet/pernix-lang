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
    Accessibility, Enum, EnumID, EnumVariant, Field, GenericParameters, GenericableID, Generics,
    GlobalID, LifetimeParameter, ModuleID, Struct, StructID, TypeParameter, ID,
};

enum SymbolState {
    Drafted,
    Constructing,
    Constructed,
}

impl Table {
    fn draft_symbols(
        &mut self,
        targets: Vec<Target>,
        handler: &impl Handler<Error>,
    ) -> HashMap<ID, SymbolState> {
        let mut symbol_states_by_id = HashMap::new();
        // implements item will be handled separately
        let mut implements_vecs_by_module_id = HashMap::new();

        // iterate through each target
        for target in targets {
            let (file, target_name) = target.dissolve();

            let module_id = self
                .root_module_ids_by_target_name
                .get(&target_name)
                .copied()
                .unwrap();

            self.draft_symbols_in_file(
                file,
                module_id,
                &mut implements_vecs_by_module_id,
                &mut symbol_states_by_id,
                handler,
            );
        }

        symbol_states_by_id
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
            let module = self.modules.get(module_id).unwrap();

            let submodule_id = module
                .child_ids_by_name
                .get(submodule.module.identifier.span.str())
                .unwrap()
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
                    Item::Function(i) => &i.function_signature.identifier,
                    Item::Type(i) => &i.type_signature.identifier,
                    Item::Struct(i) => &i.struct_signature.identifier,
                    Item::Enum(i) => &i.enum_signature.identifier,

                    Item::Implements(_) => unreachable!(),
                };

                let module = self.modules.get(module_id).unwrap();

                // redefinition check
                if let Err(error) = Self::redefinition_check(&module.child_ids_by_name, identifier)
                {
                    handler.recieve(Error::SymbolRedefinition(error));
                    continue;
                }

                identifier.span.str().to_string()
            };

            let global_id: Option<GlobalID> = match item {
                Item::Trait(_) => todo!(),
                Item::Function(_) => todo!(),
                Item::Type(_) => todo!(),
                Item::Struct(i) => Some(self.draft_struct(i, module_id, handler).into()),
                Item::Enum(i) => Some(self.draft_enum(i, module_id, handler).into()),

                Item::Implements(..) => unreachable!(),
            };

            let Some(global_id) = global_id else {
                continue;
            };

            // add the symbol as a child of the module
            let module = self.modules.get_mut(module_id).unwrap();
            assert!(
                module.child_ids_by_name.insert(name, global_id).is_none(),
                "redefinition should be handled already"
            );

            match global_id {
                GlobalID::Struct(i) => symbol_states_by_id.insert(i.into(), SymbolState::Drafted),
                GlobalID::Enum(i) => symbol_states_by_id.insert(i.into(), SymbolState::Constructed),
                GlobalID::Function(i) => symbol_states_by_id.insert(i.into(), SymbolState::Drafted),
                GlobalID::Type(i) => symbol_states_by_id.insert(i.into(), SymbolState::Drafted),
                GlobalID::Trait(i) => symbol_states_by_id.insert(i.into(), SymbolState::Drafted),

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

        for generic_parameter in generic_parameters_syntax_tree
            .generic_parameter_list
            .elements()
        {
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

            self.structs
                .get_mut(struct_id)
                .unwrap()
                .generics
                .generic_parameters = generic_parameters;
        }

        for member in struct_syntax_tree.struct_body.struct_members {
            match member {
                item::StructMember::Field(field) => {
                    // redefinition check
                    if let Err(err) = Self::redefinition_check(
                        &self.structs.get(struct_id).unwrap().field_ids_by_name,
                        &field.identifier,
                    ) {
                        handler.recieve(Error::FieldRedefinition(err));
                        continue;
                    }

                    let struct_symbol = self.structs.get_mut(struct_id).unwrap();
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
            let enum_symbol = self.enums.get_mut(enum_id).unwrap();

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
