use std::collections::{hash_map::Entry, HashMap};

use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::{
    self,
    target::{ModuleTree, Target},
};
use pernixc_system::diagnostic::Handler;

use super::{
    state::{DraftedSymbolRef, GlobalItemSyntax},
    BuildError, Table,
};
use crate::{
    constant,
    error::{self, ModuleUsingItself, UsingDuplication},
    symbol::{
        Accessibility, Constant, Function, GenericParameters, Module, Trait, Type, VecNameMap,
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

        Ok(())
    }

    pub(super) fn draft_target(
        &mut self,
        target: Target,
        implements_syntax_tree_vecs_by_module_index: &mut HashMap<
            usize,
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
        parent_module_index: Option<usize>,
        module_name: String,
        module_tree: ModuleTree,
        implements_syntax_tree_vecs_by_module_index: &mut HashMap<
            usize,
            Vec<syntax_tree::item::Implements>,
        >,
        handler: &impl Handler<error::Error>,
    ) {
        // create a new module
        let current_module_index = self.modules.len();
        self.modules.push(Module {
            name: module_name.clone(),
            index: current_module_index,
            accessibility: module_tree
                .signature()
                .as_ref()
                .map_or(Accessibility::Public, |x| match &x.access_modifier {
                    syntax_tree::AccessModifier::Public(_) => Accessibility::Public,
                    syntax_tree::AccessModifier::Private(_) => Accessibility::Private,
                    syntax_tree::AccessModifier::Internal(_) => Accessibility::Internal,
                }),
            parent_module_index,
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
        if let Some(parent_module_index) = parent_module_index {
            assert!(
                self.modules[parent_module_index]
                    .module_member_refs_by_name
                    .insert(
                        module_name,
                        crate::symbol::ModuleMemberRef::Module(current_module_index)
                    )
                    .is_none(),
                "Module duplication detected, but it should've been detected in the parser \
                 already."
            );
        } else {
            // add the module to the target root module indices
            assert!(
                self.target_root_module_indices_by_name
                    .insert(module_name, current_module_index)
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
                Some(current_module_index),
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
            if module_index == current_module_index {
                handler.receive(error::Error::ModuleUsingItself(ModuleUsingItself {
                    using_span: using.span(),
                }));
                continue;
            }

            match self.modules[current_module_index]
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
                        .entry(current_module_index)
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
            if let Some(existing_symbol) = self.modules[current_module_index]
                .module_member_refs_by_name
                .get(identifier_span.str())
                .copied()
            {
                handler.receive(error::Error::SymbolDuplication(error::SymbolDuplication {
                    duplicate_span: identifier_span,
                    existing_symbol_span: self
                        .get_global_item(existing_symbol.into())
                        .unwrap()
                        .span()
                        .unwrap(),
                }));
                continue;
            }

            match item {
                syntax_tree::item::Item::Trait(syntax_tree) => {
                    self.draft_trait(current_module_index, syntax_tree);
                }
                syntax_tree::item::Item::Function(syntax_tree) => {
                    self.draft_function(current_module_index, syntax_tree);
                }
                syntax_tree::item::Item::Type(syntax_tree) => {
                    self.draft_type(current_module_index, syntax_tree);
                }
                syntax_tree::item::Item::Struct(syntax_tree) => {
                    self.draft_struct(current_module_index, syntax_tree);
                }
                syntax_tree::item::Item::Enum(syntax_tree) => {
                    self.draft_enum(current_module_index, syntax_tree);
                }
                syntax_tree::item::Item::Constant(syntax_tree) => {
                    self.draft_constant(current_module_index, syntax_tree);
                }
                syntax_tree::item::Item::Module(_) | syntax_tree::item::Item::Implements(_) => {
                    unreachable!("it should've been filtered out already")
                }
            }
        }
    }

    fn draft_constant(
        &mut self,
        parent_module_index: usize,
        syntax_tree: syntax_tree::item::Constant,
    ) {
        let constant_index = self.constants.len();
        self.constants.push(Constant {
            name: syntax_tree.signature().identifier().span.str().to_string(),
            index: constant_index,
            accessibility: match syntax_tree.access_modifier() {
                syntax_tree::AccessModifier::Public(_) => Accessibility::Public,
                syntax_tree::AccessModifier::Private(_) => Accessibility::Private,
                syntax_tree::AccessModifier::Internal(_) => Accessibility::Internal,
            },
            parent_module_index,
            ty: ty::Type::Tuple(ty::Tuple {
                elements: Vec::new(),
            }),
            constant: constant::Constant::Bool(true),
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
            DraftedSymbolRef::Constant(constant_index),
            GlobalItemSyntax::Constant {
                constant_syntax: syntax_tree,
            },
        );
    }

    fn draft_enum(&mut self, parent_module_index: usize, syntax_tree: syntax_tree::item::Enum) {
        let enum_index = self.enums.len();
        self.enums.push(crate::symbol::Enum {
            name: syntax_tree.signature().identifier().span.str().to_string(),
            index: enum_index,
            accessibility: match syntax_tree.access_modifier() {
                syntax_tree::AccessModifier::Public(_) => Accessibility::Public,
                syntax_tree::AccessModifier::Private(_) => Accessibility::Private,
                syntax_tree::AccessModifier::Internal(_) => Accessibility::Internal,
            },
            parent_module_index,
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
            DraftedSymbolRef::Enum(enum_index),
            GlobalItemSyntax::Enum {
                enum_syntax: syntax_tree,
            },
        );
    }

    fn draft_struct(&mut self, parent_module_index: usize, syntax_tree: syntax_tree::item::Struct) {
        let struct_index = self.structs.len();
        self.structs.push(crate::symbol::Struct {
            name: syntax_tree.signature().identifier().span.str().to_string(),
            index: struct_index,
            accessibility: match syntax_tree.access_modifier() {
                syntax_tree::AccessModifier::Public(_) => Accessibility::Public,
                syntax_tree::AccessModifier::Private(_) => Accessibility::Private,
                syntax_tree::AccessModifier::Internal(_) => Accessibility::Internal,
            },
            parent_module_index,
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
            DraftedSymbolRef::Struct(struct_index),
            GlobalItemSyntax::Struct {
                struct_syntax: syntax_tree,
            },
        );
    }

    fn draft_type(&mut self, parent_module_index: usize, syntax_tree: syntax_tree::item::Type) {
        let type_index = self.types.len();
        self.types.push(Type {
            name: syntax_tree.signature().identifier().span.str().to_string(),
            index: type_index,
            accessibility: match syntax_tree.access_modifier() {
                syntax_tree::AccessModifier::Public(_) => Accessibility::Public,
                syntax_tree::AccessModifier::Private(_) => Accessibility::Private,
                syntax_tree::AccessModifier::Internal(_) => Accessibility::Internal,
            },
            parent_module_index,
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
            DraftedSymbolRef::Type(type_index),
            GlobalItemSyntax::Type {
                type_syntax: syntax_tree,
            },
        );
    }

    fn draft_function(
        &mut self,
        parent_module_index: usize,
        syntax_tree: syntax_tree::item::Function,
    ) {
        let function_index = self.functions.len();
        self.functions.push(Function {
            name: syntax_tree.signature().identifier().span.str().to_string(),
            index: function_index,
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
            parent_module_index,
        });

        self.state_mananger.add_drafted_symbol(
            DraftedSymbolRef::Function(function_index),
            GlobalItemSyntax::Function {
                function_syntax: syntax_tree,
            },
        );
    }

    fn draft_trait(&mut self, parent_module_index: usize, syntax_tree: syntax_tree::item::Trait) {
        let trait_index = self.traits.len();
        self.traits.push(Trait {
            name: syntax_tree.signature().identifier().span.str().to_string(),
            index: trait_index,
            accessibility: match syntax_tree.access_modifier() {
                syntax_tree::AccessModifier::Public(_) => Accessibility::Public,
                syntax_tree::AccessModifier::Private(_) => Accessibility::Private,
                syntax_tree::AccessModifier::Internal(_) => Accessibility::Internal,
            },
            parent_module_index,
            generic_parameters: GenericParameters::default(),
            where_clause: WhereClause::default(),
            functions: Vec::new(),
            types: Vec::new(),
            constants: Vec::new(),
            associated_ids_by_name: HashMap::new(),
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
            DraftedSymbolRef::Trait(trait_index),
            GlobalItemSyntax::Trait {
                trait_syntax: syntax_tree,
                implements_syntax: Vec::new(),
            },
        );
    }
}
