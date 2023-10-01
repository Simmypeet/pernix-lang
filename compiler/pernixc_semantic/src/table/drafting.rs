use std::collections::{hash_map::Entry, HashMap};

use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{
    self,
    target::{ModuleTree, Target},
};

use super::{
    state::{DraftedSymbolRef, DraftedSymbolSyntax},
    BuildError, Table,
};
use crate::{
    constant,
    error::{self, ModuleUsingItself, UsingDuplication},
    symbol::{
        Accessibility, Constant, ConstantRef, EnumRef, Function, FunctionRef, GenericParameters,
        Module, ModuleMemberRef, ModuleRef, StructRef, Trait, TraitRef, Type, TypeRef, VecNameMap,
        WhereClause,
    },
    ty,
};

impl Table {
    pub(super) fn draft_targets(
        &mut self,
        targets: impl Iterator<Item = Target>,
        handler: &impl Handler<error::Error>,
    ) -> Result<(), BuildError> {
        let mut implements_syntax_tree_vecs_by_module_index = HashMap::new();

        for target in targets {
            self.draft_target(
                target,
                &mut implements_syntax_tree_vecs_by_module_index,
                handler,
            )?;
        }

        for (module_index, implements_syntax_trees) in implements_syntax_tree_vecs_by_module_index {
            for implement_syntax_tree in implements_syntax_trees {
                let Ok(trait_index) = self.resolve_trait_path(
                    implement_syntax_tree.signature().qualified_identifier(),
                    module_index,
                    handler,
                ) else {
                    eprintln!("FAILED TO RESOLVE");
                    continue;
                };

                // add to the implements syntax tree list
                self.state_mananger
                    .get_item_syntax_mut(DraftedSymbolRef::Trait(trait_index))
                    .expect("should exist")
                    .as_trait_mut()
                    .unwrap()
                    .1
                    .push((module_index, implement_syntax_tree));
            }
        }

        Ok(())
    }

    pub(super) fn draft_target(
        &mut self,
        target: Target,
        implements_syntax_tree_vecs_by_module_index: &mut HashMap<
            ModuleRef,
            Vec<syntax_tree::item::Implements>,
        >,
        handler: &impl Handler<error::Error>,
    ) -> Result<(), BuildError> {
        // target name must not be core
        if target.name() == "@core" {
            return Err(BuildError::TargetNamedCore);
        }

        // target name duplication
        if self
            .target_root_module_indices_by_name
            .contains_key(target.name())
        {
            return Err(BuildError::TargetNameDuplication(target.name().to_string()));
        }

        let (module_tree, target_name) = target.dissolve();

        self.draft_module_tree(
            None,
            target_name,
            module_tree,
            implements_syntax_tree_vecs_by_module_index,
            handler,
        );

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    fn draft_module_tree(
        &mut self,
        parent_module_ref: Option<ModuleRef>,
        module_name: String,
        module_tree: ModuleTree,
        implements_syntax_tree_vecs_by_module_index: &mut HashMap<
            ModuleRef,
            Vec<syntax_tree::item::Implements>,
        >,
        handler: &impl Handler<error::Error>,
    ) {
        // create a new module
        let current_module_ref = ModuleRef(self.modules.len());
        self.modules.push(Module {
            name: module_name.clone(),
            module_ref: current_module_ref,
            accessibility: module_tree
                .signature()
                .as_ref()
                .map_or(Accessibility::Public, |x| match &x.access_modifier {
                    syntax_tree::AccessModifier::Public(_) => Accessibility::Public,
                    syntax_tree::AccessModifier::Private(_) => Accessibility::Private,
                    syntax_tree::AccessModifier::Internal(_) => Accessibility::Internal,
                }),
            parent_module_ref,
            module_member_refs_by_name: HashMap::new(),
            usings: HashMap::new(),
            span: module_tree.signature().as_ref().map(|x| {
                x.signature
                    .module_keyword()
                    .span
                    .join(&x.signature.identifier().span)
                    .unwrap()
            }),
        });

        // add the module to the parent module
        if let Some(parent_module_ref) = parent_module_ref {
            assert!(
                self.modules[parent_module_ref.0]
                    .module_member_refs_by_name
                    .insert(
                        module_name,
                        crate::symbol::ModuleMemberRef::Module(current_module_ref)
                    )
                    .is_none(),
                "Module duplication detected, but it should've been detected in the parser \
                 already."
            );
        } else {
            // add the module to the target root module indices
            assert!(
                self.target_root_module_indices_by_name
                    .insert(module_name, current_module_ref)
                    .is_none(),
                "Module duplication detected, but it should've been detected in the earlier \
                 function already."
            );
        }

        let (.., content, submodules) = module_tree.dissolve();
        let (content_using, content_module) = content.dissolve();

        // create all submodules
        for (name, submodule) in submodules {
            self.draft_module_tree(
                Some(current_module_ref),
                name,
                submodule,
                implements_syntax_tree_vecs_by_module_index,
                handler,
            );
        }

        // create the using statements
        for using in content_using {
            // using module index
            let Some(module_index) = self.resolve_module_path(using.module_path(), handler) else {
                continue;
            };

            // check: module using duplication, module using self
            if module_index == current_module_ref {
                handler.receive(error::Error::ModuleUsingItself(ModuleUsingItself {
                    using_span: using.span(),
                }));
                continue;
            }

            match self.modules[current_module_ref.0]
                .usings
                .entry(module_index)
            {
                Entry::Occupied(entry) => {
                    handler.receive(error::Error::UsingDuplication(UsingDuplication {
                        duplicate_span: using.span(),
                        existing_using_span: entry.get().clone(),
                    }));
                }
                Entry::Vacant(entry) => {
                    entry.insert(using.span());
                }
            }
        }

        for item in content_module {
            let identifier_span = match item {
                syntax_tree::item::Item::Trait(ref syn) => {
                    syn.signature().identifier().span.clone()
                }
                syntax_tree::item::Item::Function(ref syn) => {
                    syn.signature().identifier().span.clone()
                }
                syntax_tree::item::Item::Type(ref syn) => syn.signature().identifier().span.clone(),
                syntax_tree::item::Item::Struct(ref syn) => {
                    syn.signature().identifier().span.clone()
                }
                syntax_tree::item::Item::Implements(syn) => {
                    implements_syntax_tree_vecs_by_module_index
                        .entry(current_module_ref)
                        .or_default()
                        .push(syn);
                    continue;
                }
                syntax_tree::item::Item::Enum(ref syn) => syn.signature().identifier().span.clone(),
                syntax_tree::item::Item::Constant(ref syn) => {
                    syn.signature().identifier().span.clone()
                }
                syntax_tree::item::Item::Module(_) => {
                    unreachable!("submodules should've been extracted out already")
                }
            };

            // redefinition check
            if let Some(existing_module_member_ref) = self.modules[current_module_ref.0]
                .module_member_refs_by_name
                .get(identifier_span.str())
                .copied()
            {
                handler.receive(error::Error::SymbolDuplication(error::SymbolDuplication {
                    duplicate_span: identifier_span,
                    existing_module_member_ref,
                }));
                continue;
            }

            let module_member_ref = match item {
                syntax_tree::item::Item::Trait(syntax_tree) => {
                    ModuleMemberRef::Trait(self.draft_trait(current_module_ref, syntax_tree))
                }
                syntax_tree::item::Item::Function(syntax_tree) => {
                    ModuleMemberRef::Function(self.draft_function(current_module_ref, syntax_tree))
                }
                syntax_tree::item::Item::Type(syntax_tree) => {
                    ModuleMemberRef::Type(self.draft_type(current_module_ref, syntax_tree))
                }
                syntax_tree::item::Item::Struct(syntax_tree) => {
                    ModuleMemberRef::Struct(self.draft_struct(current_module_ref, syntax_tree))
                }
                syntax_tree::item::Item::Enum(syntax_tree) => {
                    ModuleMemberRef::Enum(self.draft_enum(current_module_ref, syntax_tree))
                }
                syntax_tree::item::Item::Constant(syntax_tree) => {
                    ModuleMemberRef::Constant(self.draft_constant(current_module_ref, syntax_tree))
                }
                syntax_tree::item::Item::Module(_) | syntax_tree::item::Item::Implements(_) => {
                    unreachable!("it should've been filtered out already")
                }
            };

            self.modules[current_module_ref.0]
                .module_member_refs_by_name
                .insert(identifier_span.str().to_string(), module_member_ref);
        }
    }

    fn draft_constant(
        &mut self,
        parent_module_ref: ModuleRef,
        syntax_tree: syntax_tree::item::Constant,
    ) -> ConstantRef {
        let constant_ref = ConstantRef(self.constants.len());
        self.constants.push(Constant {
            name: syntax_tree.signature().identifier().span.str().to_string(),
            constant_ref,
            accessibility: match syntax_tree.access_modifier() {
                syntax_tree::AccessModifier::Public(_) => Accessibility::Public,
                syntax_tree::AccessModifier::Private(_) => Accessibility::Private,
                syntax_tree::AccessModifier::Internal(_) => Accessibility::Internal,
            },
            parent_module_ref,
            ty: ty::Type::Tuple(ty::Tuple {
                elements: Vec::new(),
            }),
            constant: constant::Constant::Primitive(constant::Primitive::Bool(true)),
            span: Some(
                syntax_tree
                    .signature()
                    .const_keyword()
                    .span
                    .join(&syntax_tree.signature().identifier().span)
                    .unwrap(),
            ),
        });

        self.state_mananger.add_drafted_symbol(
            DraftedSymbolRef::Constant(constant_ref),
            DraftedSymbolSyntax::Constant {
                constant_syntax: syntax_tree,
            },
        );

        constant_ref
    }

    fn draft_enum(
        &mut self,
        parent_module_ref: ModuleRef,
        syntax_tree: syntax_tree::item::Enum,
    ) -> EnumRef {
        let enum_ref = EnumRef(self.enums.len());
        self.enums.push(crate::symbol::Enum {
            name: syntax_tree.signature().identifier().span.str().to_string(),
            enum_ref,
            accessibility: match syntax_tree.access_modifier() {
                syntax_tree::AccessModifier::Public(_) => Accessibility::Public,
                syntax_tree::AccessModifier::Private(_) => Accessibility::Private,
                syntax_tree::AccessModifier::Internal(_) => Accessibility::Internal,
            },
            parent_module_ref,
            generic_parameters: GenericParameters::default(),
            where_clause: WhereClause::default(),
            variants: VecNameMap::new(),
            span: Some(
                syntax_tree
                    .signature()
                    .enum_keyword()
                    .span
                    .join(&syntax_tree.signature().identifier().span)
                    .unwrap(),
            ),
        });

        self.state_mananger.add_drafted_symbol(
            DraftedSymbolRef::Enum(enum_ref),
            DraftedSymbolSyntax::Enum {
                enum_syntax: syntax_tree,
            },
        );

        enum_ref
    }

    fn draft_struct(
        &mut self,
        parent_module_ref: ModuleRef,
        syntax_tree: syntax_tree::item::Struct,
    ) -> StructRef {
        let struct_ref = StructRef(self.structs.len());
        self.structs.push(crate::symbol::Struct {
            name: syntax_tree.signature().identifier().span.str().to_string(),
            struct_ref,
            accessibility: match syntax_tree.access_modifier() {
                syntax_tree::AccessModifier::Public(_) => Accessibility::Public,
                syntax_tree::AccessModifier::Private(_) => Accessibility::Private,
                syntax_tree::AccessModifier::Internal(_) => Accessibility::Internal,
            },
            parent_module_ref,
            generic_parameters: GenericParameters::default(),
            where_clause: WhereClause::default(),
            fields: VecNameMap::new(),
            span: Some(
                syntax_tree
                    .signature()
                    .struct_keyword()
                    .span
                    .join(&syntax_tree.signature().identifier().span)
                    .unwrap(),
            ),
        });

        self.state_mananger.add_drafted_symbol(
            DraftedSymbolRef::Struct(struct_ref),
            DraftedSymbolSyntax::Struct {
                struct_syntax: syntax_tree,
            },
        );

        struct_ref
    }

    fn draft_type(
        &mut self,
        parent_module_ref: ModuleRef,
        syntax_tree: syntax_tree::item::Type,
    ) -> TypeRef {
        let type_ref = TypeRef(self.types.len());
        self.types.push(Type {
            name: syntax_tree.signature().identifier().span.str().to_string(),
            type_ref,
            accessibility: match syntax_tree.access_modifier() {
                syntax_tree::AccessModifier::Public(_) => Accessibility::Public,
                syntax_tree::AccessModifier::Private(_) => Accessibility::Private,
                syntax_tree::AccessModifier::Internal(_) => Accessibility::Internal,
            },
            parent_module_ref,
            generic_parameters: GenericParameters::default(),
            where_clause: WhereClause::default(),
            ty: ty::Type::Tuple(ty::Tuple {
                elements: Vec::new(),
            }),
            span: Some(
                syntax_tree
                    .signature()
                    .type_keyword()
                    .span
                    .join(&syntax_tree.signature().identifier().span)
                    .unwrap(),
            ),
        });

        self.state_mananger.add_drafted_symbol(
            DraftedSymbolRef::Type(type_ref),
            DraftedSymbolSyntax::Type {
                type_syntax: syntax_tree,
            },
        );

        type_ref
    }

    fn draft_function(
        &mut self,
        parent_module_ref: ModuleRef,
        syntax_tree: syntax_tree::item::Function,
    ) -> FunctionRef {
        let function_ref = FunctionRef(self.functions.len());
        self.functions.push(Function {
            name: syntax_tree.signature().identifier().span.str().to_string(),
            function_ref,
            accessibility: match syntax_tree.access_modifier() {
                syntax_tree::AccessModifier::Public(_) => Accessibility::Public,
                syntax_tree::AccessModifier::Private(_) => Accessibility::Private,
                syntax_tree::AccessModifier::Internal(_) => Accessibility::Internal,
            },
            generic_parameters: GenericParameters::default(),
            where_clause: WhereClause::default(),
            return_type: ty::Type::Tuple(ty::Tuple {
                elements: Vec::new(),
            }),
            parameters: Vec::new(),
            span: Some(
                syntax_tree
                    .signature()
                    .function_keyword()
                    .span
                    .join(&syntax_tree.signature().identifier().span)
                    .unwrap(),
            ),
            parent_module_ref,
        });

        self.state_mananger.add_drafted_symbol(
            DraftedSymbolRef::Function(function_ref),
            DraftedSymbolSyntax::Function {
                function_syntax: syntax_tree,
            },
        );

        function_ref
    }

    fn draft_trait(
        &mut self,
        parent_module_ref: ModuleRef,
        syntax_tree: syntax_tree::item::Trait,
    ) -> TraitRef {
        let trait_ref = TraitRef(self.traits.len());
        self.traits.push(Trait {
            name: syntax_tree.signature().identifier().span.str().to_string(),
            trait_ref,
            accessibility: match syntax_tree.access_modifier() {
                syntax_tree::AccessModifier::Public(_) => Accessibility::Public,
                syntax_tree::AccessModifier::Private(_) => Accessibility::Private,
                syntax_tree::AccessModifier::Internal(_) => Accessibility::Internal,
            },
            parent_module_ref,
            generic_parameters: GenericParameters::default(),
            where_clause: WhereClause::default(),
            functions: Vec::new(),
            types: Vec::new(),
            constants: Vec::new(),
            associated_refs_by_name: HashMap::new(),
            span: Some(
                syntax_tree
                    .signature()
                    .trait_keyword()
                    .span
                    .join(&syntax_tree.signature().identifier().span)
                    .unwrap(),
            ),
            implements: Vec::new(),
            negative_implements: Vec::new(),
        });

        self.state_mananger.add_drafted_symbol(
            DraftedSymbolRef::Trait(trait_ref),
            DraftedSymbolSyntax::Trait {
                trait_syntax: Some(syntax_tree),
                implements_syntax: Vec::new(),
            },
        );

        trait_ref
    }
}
