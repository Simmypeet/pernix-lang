use std::collections::{hash_map::Entry, HashMap};

use parking_lot::RwLock;
use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::{
    self,
    item::{GenericParameter, Item, ModulePath},
    target::{ModuleTree, Target},
    AccessModifier, ConnectedList,
};
use pernixc_system::{
    arena::{self, Arena, NamedMap},
    diagnostic::Handler,
};

use super::{
    Accessibility, BuildError, ConstantParameter, Function, GenericParameters, Implements,
    ImplementsSignature, LifetimeParameter, Struct, Table, TargetNamedCoreError, WhereClause,
};
use crate::{
    constant,
    error::{
        ConstantParameterDuplication, Error, LifetimeParameterDeclaredAfterTypeOrConstantParameter,
        LifetimeParameterDuplication, ModuleExpected, ModuleNotFound, SymbolDuplication,
        TraitMemberDuplication, TypeParameterDeclaredAfterConstantParameter,
        TypeParameterDuplication, UsingDuplication, VariantDuplication,
    },
    symbol::{
        DraftingSymbolRef, Global, Module, TargetNameDuplicationError, Trait, TraitAssociatedID,
        TraitFunction, Type, ID,
    },
    ty,
};

impl Table {
    pub(super) fn draft_targets(
        &self,
        targets: impl Iterator<Item = Target>,
        handler: &impl Handler<Error>,
    ) -> Result<(), BuildError> {
        let implements_syntax_tree_vecs_by_module_id = RwLock::new(HashMap::new());

        for target in targets {
            self.draft_target(target, &implements_syntax_tree_vecs_by_module_id, handler)?;
        }

        let implements_syntax_tree_vecs_by_module_id =
            implements_syntax_tree_vecs_by_module_id.into_inner();

        for (module_id, implements_syntax_tree) in implements_syntax_tree_vecs_by_module_id
            .into_iter()
            .flat_map(|x| x.1.into_iter().map(move |syn| (x.0, syn)))
        {
            let Ok(trait_id) = self.resolve_trait_path(
                implements_syntax_tree.signature().qualified_identifier(),
                module_id,
                handler,
            ) else {
                continue;
            };

            let mut container = self.container.write();
            let trait_name = container.traits[trait_id].name.clone();
            container.traits[trait_id].implements.push(Implements {
                signature: ImplementsSignature::default(),
                is_const_implements: false,
                where_clause: WhereClause::default(),
                parent_trait_id: trait_id,
                syntax_tree: Some(implements_syntax_tree),
                associated_ids_by_name: HashMap::new(),
                types: Arena::new(),
                functions: Arena::new(),
                constants: Arena::new(),
                declared_in_module_id: module_id,
                trait_name,
            });
        }

        Ok(())
    }

    fn draft_target(
        &self,
        target: Target,
        implements_syntax_tree_vecs_by_module_id: &RwLock<
            HashMap<arena::ID<Module>, Vec<syntax_tree::item::Implements>>,
        >,
        handler: &impl Handler<Error>,
    ) -> Result<(), BuildError> {
        // target name can't be "@core"
        if target.target_name() == "@core" {
            return Err(BuildError::TargetNamedCore(TargetNamedCoreError));
        }

        // target name can't be duplicated
        if self
            .target_root_module_ids_by_name
            .read()
            .contains_key(target.target_name())
        {
            return Err(BuildError::TargetNameDuplication(
                TargetNameDuplicationError {
                    name: target.target_name().to_string(),
                },
            ));
        }

        let (module_tree, target_name) = target.dissolve();

        self.draft_module_tree(
            None,
            target_name,
            module_tree,
            implements_syntax_tree_vecs_by_module_id,
            handler,
        );

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    fn draft_generic_parameters(
        generic_parameters: &mut GenericParameters,
        syntax_tree: &syntax_tree::item::GenericParameters,
        handler: &impl Handler<Error>,
    ) {
        for generic_parameter in syntax_tree.parameter_list().elements() {
            match generic_parameter {
                GenericParameter::Lifetime(param) => {
                    if !(generic_parameters.types.is_empty()
                        || generic_parameters.constants.is_empty())
                    {
                        handler.receive(
                            Error::LifetimeParameterDeclaredAfterTypeOrConstantParameter(
                                LifetimeParameterDeclaredAfterTypeOrConstantParameter {
                                    lifetime_parameter_span: param.span(),
                                },
                            ),
                        );
                        continue;
                    }

                    if let Some(symbol) = generic_parameters
                        .lifetimes
                        .get_by_name(param.identifier().span.str())
                    {
                        // lifetime parameter duplication
                        handler.receive(Error::LifetimeParameterDuplication(
                            LifetimeParameterDuplication {
                                duplicate_span: param.span(),
                                existing_lifetime_parameter_span: symbol.span.clone().unwrap(),
                            },
                        ));
                        continue;
                    }

                    generic_parameters
                        .lifetimes
                        .insert(
                            param.identifier().span.str().to_string(),
                            LifetimeParameter {
                                name: param.identifier().span.str().to_string(),
                                span: Some(param.span()),
                            },
                        )
                        .expect("should not be duplicated");
                }
                GenericParameter::Type(param) => {
                    if !generic_parameters.constants.is_empty() {
                        handler.receive(Error::TypeParameterDeclaredAfterConstantParameter(
                            TypeParameterDeclaredAfterConstantParameter {
                                type_parameter_span: generic_parameter.span(),
                            },
                        ));
                        continue;
                    }

                    if let Some(symbol) = generic_parameters
                        .types
                        .get_by_name(param.identifier().span.str())
                    {
                        // type parameter duplication
                        handler.receive(Error::TypeParameterDuplication(
                            TypeParameterDuplication {
                                duplicate_span: generic_parameter.span(),
                                existing_type_parameter: symbol.span.clone().unwrap(),
                            },
                        ));
                        continue;
                    }

                    generic_parameters
                        .types
                        .insert(
                            param.identifier().span.str().to_string(),
                            super::TypeParameter {
                                name: param.identifier().span.str().to_string(),
                                span: Some(param.span()),
                                default: None,
                            },
                        )
                        .expect("should not be duplicated");
                }
                GenericParameter::Constant(param) => {
                    if let Some(symbol) = generic_parameters
                        .constants
                        .get_by_name(param.identifier().span.str())
                    {
                        // constant parameter duplication
                        handler.receive(Error::ConstantParameterDuplication(
                            ConstantParameterDuplication {
                                duplicate_span: param.span(),
                                existing_constant_parameter: symbol.span.clone().unwrap(),
                            },
                        ));
                        continue;
                    }

                    generic_parameters
                        .constants
                        .insert(
                            param.identifier().span.str().to_string(),
                            ConstantParameter {
                                name: param.identifier().span.str().to_string(),
                                ty: ty::Type::default(),
                                span: Some(param.span()),
                                default: None,
                            },
                        )
                        .expect("should not be duplicated");
                }
            }
        }
    }

    #[allow(clippy::significant_drop_tightening)]
    fn draft_function(
        &self,
        parent_module_id: arena::ID<Module>,
        syntax_tree: syntax_tree::item::Function,
        handler: &impl Handler<Error>,
    ) {
        let mut container = self.container.write();
        let function_name = syntax_tree.signature().identifier().span.str().to_string();

        let function = container.functions.push(Function {
            accessibility: match syntax_tree.access_modifier() {
                AccessModifier::Public(_) => Accessibility::Public,
                AccessModifier::Private(_) => Accessibility::Private,
                AccessModifier::Internal(_) => Accessibility::Internal,
            },
            name: function_name.clone(),
            generic_parameters: GenericParameters::default(),
            where_clause: WhereClause::default(),
            parameters: Arena::default(),
            return_type: ty::Type::default(),
            syntax_tree: Some(syntax_tree),
            parent_module_id,
        });

        let function_sym = &mut container.functions[function];

        if let Some(syntax_tree) = function_sym
            .syntax_tree
            .as_ref()
            .unwrap()
            .signature()
            .generic_parameters()
            .as_ref()
        {
            let mut generic_parameters = GenericParameters::default();
            Self::draft_generic_parameters(&mut generic_parameters, syntax_tree, handler);
            function_sym.generic_parameters = generic_parameters;
        }

        assert!(
            container.modules[parent_module_id]
                .children_ids_by_name
                .insert(function_name, ID::Function(function))
                .is_none(),
            "Duplication detected, but it should've already been checked."
        );

        self.add_drafted_symbol(DraftingSymbolRef::Function(function));
    }

    #[allow(clippy::too_many_lines, clippy::significant_drop_in_scrutinee)]
    fn draft_module_tree(
        &self,
        parent_module_id: Option<arena::ID<Module>>,
        module_name: String,
        module_tree: ModuleTree,
        implements_syntax_tree_vecs_by_module_id: &RwLock<
            HashMap<arena::ID<Module>, Vec<syntax_tree::item::Implements>>,
        >,
        handler: &impl Handler<Error>,
    ) {
        let current_module_id = self.container.write().modules.push(Module {
            accessibility: module_tree
                .signature()
                .as_ref()
                .map_or(Accessibility::Public, |x| match &x.access_modifier {
                    AccessModifier::Public(_) => Accessibility::Public,
                    AccessModifier::Private(_) => Accessibility::Private,
                    AccessModifier::Internal(_) => Accessibility::Internal,
                }),
            name: module_name.clone(),
            parent_module_id,
            children_ids_by_name: HashMap::new(),
            usings: HashMap::new(),
            syntax_tree: None,
        });

        // add to the parent module
        if let Some(parent_module_id) = parent_module_id {
            assert!(
                self.container.write().modules[parent_module_id]
                    .children_ids_by_name
                    .insert(module_name, ID::Module(current_module_id))
                    .is_none(),
                "Duplication detected, but it should've already been checked."
            );
        } else {
            assert!(
                self.target_root_module_ids_by_name
                    .write()
                    .insert(module_name, current_module_id)
                    .is_none(),
                "Duplication detected, but it should've already been checked."
            );
        }

        let (.., content, submodules) = module_tree.dissolve();
        let (content_using, content_module) = content.dissolve();

        // create all submodules first
        {
            std::thread::scope(move |scope| {
                for (name, submodule) in submodules {
                    scope.spawn(move || {
                        self.draft_module_tree(
                            Some(current_module_id),
                            name,
                            submodule,
                            implements_syntax_tree_vecs_by_module_id,
                            handler,
                        );
                    });
                }
            });
        }

        // then create the using statements
        for using in content_using {
            let Some(module_id) = self.resolve_module_path(using.module_path(), handler) else {
                continue;
            };

            let mut container = self.container.write();
            match container.modules[current_module_id].usings.entry(module_id) {
                Entry::Occupied(entry) => {
                    handler.receive(Error::UsingDuplication(UsingDuplication {
                        duplicate_span: using.span(),
                        existing_using_span: entry.get().span(),
                    }));
                }
                Entry::Vacant(entry) => {
                    entry.insert(using);
                }
            }
        }

        // then create content for the module
        for item in content_module {
            let identifier_span = match item {
                Item::Trait(ref syn) => syn.signature().identifier().span(),
                Item::Function(ref syn) => syn.signature().identifier().span(),
                Item::Type(ref syn) => syn.signature().identifier().span(),
                Item::Struct(ref syn) => syn.signature().identifier().span(),
                Item::Implements(syn) => {
                    implements_syntax_tree_vecs_by_module_id
                        .write()
                        .entry(current_module_id)
                        .or_default()
                        .push(syn);

                    continue;
                }
                Item::Enum(ref syn) => syn.signature().identifier().span(),
                Item::Constant(ref syn) => syn.signature().identifier().span(),
                Item::Module(_) => unreachable!("submodules should've been extracted out already"),
            };

            if let Some(existing_symbol) = self.container.write().modules[current_module_id]
                .children_ids_by_name
                .get(identifier_span.str())
            {
                handler.receive(Error::SymbolDuplication(SymbolDuplication {
                    duplicate_span: identifier_span,
                    existing_symbol_span: self
                        .get_global((*existing_symbol).into())
                        .unwrap()
                        .span()
                        .unwrap(),
                }));
                continue;
            }

            match item {
                Item::Trait(syn) => self.draft_trait(current_module_id, syn, handler),
                Item::Function(syn) => self.draft_function(current_module_id, syn, handler),
                Item::Type(syn) => self.draft_type(current_module_id, syn, handler),
                Item::Struct(syn) => self.draft_struct(current_module_id, syn, handler),
                Item::Enum(syn) => self.draft_enum(current_module_id, syn, handler),
                Item::Constant(syn) => self.draft_constant(current_module_id, syn),
                Item::Implements(..) | Item::Module(..) => {
                    unreachable!("implements should've been extracted out already")
                }
            }
        }
    }

    #[allow(clippy::significant_drop_tightening)]
    fn draft_constant(
        &self,
        parent_module_id: arena::ID<Module>,
        syntax_tree: syntax_tree::item::Constant,
    ) {
        let mut container = self.container.write();
        let constant_name = syntax_tree.signature().identifier().span.str().to_string();

        let constant_id = container.constants.push(super::Constant {
            accessibility: match syntax_tree.access_modifier() {
                AccessModifier::Public(_) => Accessibility::Public,
                AccessModifier::Private(_) => Accessibility::Private,
                AccessModifier::Internal(_) => Accessibility::Internal,
            },
            name: constant_name.clone(),
            syntax_tree: Some(syntax_tree),
            ty: ty::Type::default(),
            constant: constant::Constant::Tuple(constant::Tuple(Vec::new())),
            parent_module_id,
        });

        assert!(
            container.modules[parent_module_id]
                .children_ids_by_name
                .insert(constant_name, ID::Constant(constant_id))
                .is_none(),
            "Duplication detected, but it should've already been checked."
        );

        self.add_drafted_symbol(DraftingSymbolRef::Constant(constant_id));
    }

    #[allow(clippy::significant_drop_tightening)]
    fn draft_enum(
        &self,
        parent_module_id: arena::ID<Module>,
        syntax_tree: syntax_tree::item::Enum,
        handler: &impl Handler<Error>,
    ) {
        let mut container = self.container.write();

        let (accessibility, signature_syntax, body_syntax) = syntax_tree.dissolve();

        let enum_name = signature_syntax.identifier().span.str().to_string();

        let enum_id = container.enums.push(super::Enum {
            accessibility: match accessibility {
                AccessModifier::Public(_) => Accessibility::Public,
                AccessModifier::Private(_) => Accessibility::Private,
                AccessModifier::Internal(_) => Accessibility::Internal,
            },
            name: enum_name.clone(),
            generic_parameters: GenericParameters::default(),
            where_clause: WhereClause::default(),
            syntax_tree: Some(signature_syntax),
            variants: NamedMap::default(),
            parent_module_id,
        });

        let enum_sym = &mut container.enums[enum_id];

        if let Some(syntax_tree) = enum_sym
            .syntax_tree
            .as_ref()
            .unwrap()
            .generic_parameters()
            .as_ref()
        {
            let mut generic_parameters = GenericParameters::default();
            Self::draft_generic_parameters(&mut generic_parameters, syntax_tree, handler);
            enum_sym.generic_parameters = generic_parameters;
        }

        for variant_syn in body_syntax
            .dissolve()
            .1
            .into_iter()
            .flat_map(ConnectedList::into_elements)
        {
            let variant_name = variant_syn.identifier().span.str();

            if let Some(variant) = enum_sym.variants.get_by_name(variant_name) {
                handler.receive(Error::VariantDuplication(VariantDuplication {
                    duplication_span: variant_syn.identifier().span.clone(),
                    existing_variant_span: variant.span().unwrap(),
                }));
            }

            enum_sym
                .variants
                .insert(variant_name.to_string(), super::Variant {
                    name: variant_name.to_string(),
                    parent_enum_id: enum_id,
                    association_type: None,
                    syntax_tree: Some(variant_syn),
                })
                .expect("should've not duplicated");
        }

        assert!(
            container.modules[parent_module_id]
                .children_ids_by_name
                .insert(enum_name, ID::Enum(enum_id))
                .is_none(),
            "Duplication detected, but it should've already been checked."
        );

        self.add_drafted_symbol(DraftingSymbolRef::Enum(enum_id));
    }

    #[allow(clippy::significant_drop_tightening)]
    fn draft_struct(
        &self,
        parent_module_id: arena::ID<Module>,
        syntax_tree: syntax_tree::item::Struct,
        handler: &impl Handler<Error>,
    ) {
        let struct_id = {
            let mut container = self.container.write();
            let struct_name = syntax_tree.signature().identifier().span.str().to_string();

            let struct_id = container.structs.push(Struct {
                accessibility: match syntax_tree.access_modifier() {
                    AccessModifier::Public(_) => Accessibility::Public,
                    AccessModifier::Private(_) => Accessibility::Private,
                    AccessModifier::Internal(_) => Accessibility::Internal,
                },
                name: struct_name.clone(),
                generic_parameters: GenericParameters::default(),
                syntax_tree: Some(syntax_tree),
                where_clause: WhereClause::default(),
                fields: Arena::default(),
                parent_module_id,
            });

            let struct_sym = &mut container.structs[struct_id];

            if let Some(syntax_tree) = struct_sym
                .syntax_tree
                .as_ref()
                .unwrap()
                .signature()
                .generic_parameters()
                .as_ref()
            {
                let mut generic_parameters = GenericParameters::default();
                Self::draft_generic_parameters(&mut generic_parameters, syntax_tree, handler);
                struct_sym.generic_parameters = generic_parameters;
            }

            assert!(
                container.modules[parent_module_id]
                    .children_ids_by_name
                    .insert(struct_name, ID::Struct(struct_id))
                    .is_none(),
                "Duplication detected, but it should've already been checked."
            );

            struct_id
        };

        self.add_drafted_symbol(DraftingSymbolRef::Struct(struct_id));
    }

    #[allow(clippy::significant_drop_tightening)]
    fn draft_type(
        &self,
        parent_module_id: arena::ID<Module>,
        syntax_tree: syntax_tree::item::Type,
        handler: &impl Handler<Error>,
    ) {
        let mut container = self.container.write();
        let type_name = syntax_tree.signature().identifier().span.str().to_string();

        let type_id = container.types.push(Type {
            accessibility: match syntax_tree.access_modifier() {
                AccessModifier::Public(_) => Accessibility::Public,
                AccessModifier::Private(_) => Accessibility::Private,
                AccessModifier::Internal(_) => Accessibility::Internal,
            },
            name: type_name.clone(),
            generic_parameters: GenericParameters::default(),
            syntax_tree: Some(syntax_tree),
            alias: ty::Type::default(),
            parent_module_id,
        });

        let type_sym = &mut container.types[type_id];

        if let Some(syntax_tree) = type_sym
            .syntax_tree
            .as_ref()
            .unwrap()
            .signature()
            .generic_parameters()
            .as_ref()
        {
            let mut generic_parameters = GenericParameters::default();
            Self::draft_generic_parameters(&mut generic_parameters, syntax_tree, handler);
            type_sym.generic_parameters = generic_parameters;
        }

        assert!(
            container.modules[parent_module_id]
                .children_ids_by_name
                .insert(type_name, ID::Type(type_id))
                .is_none(),
            "Duplication detected, but it should've already been checked."
        );

        self.add_drafted_symbol(DraftingSymbolRef::Type(type_id));
    }

    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    fn draft_trait(
        &self,
        parent_module_id: arena::ID<Module>,
        syntax_tree: syntax_tree::item::Trait,
        handler: &impl Handler<Error>,
    ) {
        let mut container = self.container.write();

        let (accessibility, signature_syntax, body_syntax) = syntax_tree.dissolve();

        let trait_name = signature_syntax.identifier().span.str().to_string();

        let trait_id = container.traits.push(Trait {
            accessibility: match accessibility {
                AccessModifier::Public(_) => Accessibility::Public,
                AccessModifier::Private(_) => Accessibility::Private,
                AccessModifier::Internal(_) => Accessibility::Internal,
            },
            name: trait_name.clone(),
            generic_parameters: GenericParameters::default(),
            where_clause: WhereClause::default(),
            associated_ids_by_name: HashMap::new(),
            constants: Arena::default(),
            types: Arena::default(),
            functions: Arena::default(),
            implements: Arena::default(),
            syntax_tree: Some(signature_syntax),
            negative_implements: Arena::default(),
            parent_module_id,
        });

        let trait_sym = &mut container.traits[trait_id];

        if let Some(syntax_tree) = trait_sym
            .syntax_tree
            .as_ref()
            .unwrap()
            .generic_parameters()
            .as_ref()
        {
            let mut generic_parameters = GenericParameters::default();
            Self::draft_generic_parameters(&mut generic_parameters, syntax_tree, handler);
            trait_sym.generic_parameters = generic_parameters;
        }

        for member_syn in body_syntax.dissolve().1 {
            let identifier_span = match &member_syn {
                syntax_tree::item::TraitMember::Function(syn) => {
                    syn.signature().identifier().span.clone()
                }
                syntax_tree::item::TraitMember::Type(syn) => {
                    syn.signature().identifier().span.clone()
                }
                syntax_tree::item::TraitMember::Constant(syn) => {
                    syn.signature().identifier().span.clone()
                }
            };

            if let Some(existing_symbol_id) =
                trait_sym.associated_ids_by_name.get(identifier_span.str())
            {
                handler.receive(Error::TraitMemberDuplication(TraitMemberDuplication {
                    duplication_span: identifier_span,
                    existing_trait_member_span: match existing_symbol_id {
                        TraitAssociatedID::Type(id) => trait_sym.types[*id].span().unwrap().clone(),
                        TraitAssociatedID::Constant(id) => {
                            trait_sym.constants[*id].span().unwrap().clone()
                        }
                        TraitAssociatedID::Function(id) => {
                            trait_sym.functions[*id].span().unwrap().clone()
                        }
                    },
                }));
                continue;
            }

            let trait_associated_id = match member_syn {
                syntax_tree::item::TraitMember::Function(func) => {
                    TraitAssociatedID::Function(trait_sym.functions.push(TraitFunction {
                        generic_parameters: {
                            let mut generic_parameters = GenericParameters::default();

                            if let Some(generic_parameters_syn) =
                                func.signature().generic_parameters().as_ref()
                            {
                                Self::draft_generic_parameters(
                                    &mut generic_parameters,
                                    generic_parameters_syn,
                                    handler,
                                );
                            }

                            generic_parameters
                        },
                        where_clause: WhereClause::default(),
                        name: identifier_span.str().to_string(),
                        parameters: Arena::new(),
                        parent_trait_id: trait_id,
                        syntax_tree: Some(func),
                        return_type: ty::Type::default(),
                    }))
                }
                syntax_tree::item::TraitMember::Type(type_syn) => {
                    TraitAssociatedID::Type(trait_sym.types.push(super::TraitType {
                        generic_parameters: {
                            let mut generic_parameters = GenericParameters::default();

                            if let Some(generic_parameters_syn) =
                                type_syn.signature().generic_parameters().as_ref()
                            {
                                Self::draft_generic_parameters(
                                    &mut generic_parameters,
                                    generic_parameters_syn,
                                    handler,
                                );
                            }

                            generic_parameters
                        },
                        name: identifier_span.str().to_string(),
                        parent_trait_id: trait_id,
                        syntax_tree: Some(type_syn),
                        alias: ty::Type::default(),
                    }))
                }
                syntax_tree::item::TraitMember::Constant(const_syn) => {
                    TraitAssociatedID::Constant(trait_sym.constants.push(super::TraitConstant {
                        name: identifier_span.str().to_string(),
                        parent_trait_id: trait_id,
                        syntax_tree: Some(const_syn),
                        ty: ty::Type::default(),
                        constant: constant::Constant::Tuple(constant::Tuple(Vec::new())),
                    }))
                }
            };

            trait_sym
                .associated_ids_by_name
                .insert(identifier_span.str().to_string(), trait_associated_id)
                .expect("should've not duplicated");
        }

        assert!(
            container.modules[parent_module_id]
                .children_ids_by_name
                .insert(trait_name, ID::Trait(trait_id))
                .is_none(),
            "Duplication detected, but it should've already been checked."
        );

        self.add_drafted_symbol(DraftingSymbolRef::Trait(trait_id));
    }

    fn resolve_module_path(
        &self,
        module_path: &ModulePath,
        handler: &impl Handler<Error>,
    ) -> Option<arena::ID<Module>> {
        let mut current_module_id = None;

        for path in module_path.paths() {
            let next = match current_module_id {
                // continue searching from current module
                Some(current_module_id) => 'a: {
                    // search from current module id
                    let Some(id) = self.container.read().modules[current_module_id]
                        .children_ids_by_name
                        .get(path.span.str())
                        .copied()
                    else {
                        break 'a None;
                    };

                    // must be a module
                    let ID::Module(module_id) = id else {
                        handler.receive(Error::ModuleExpected(ModuleExpected {
                            module_path_span: path.span.clone(),
                        }));
                        return None;
                    };

                    Some(module_id)
                }

                // search from root
                None => self
                    .target_root_module_ids_by_name
                    .read()
                    .get(path.span.str())
                    .copied(),
            };

            let Some(next) = next else {
                handler.receive(Error::ModuleNotFound(ModuleNotFound {
                    module_path_span: path.span.clone(),
                    serached_module_id: current_module_id,
                }));
                return None;
            };

            current_module_id = Some(next);
        }

        current_module_id
    }
}

#[cfg(test)]
mod tests;
