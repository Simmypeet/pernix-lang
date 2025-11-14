use std::sync::Arc;

use flexstr::FlexStr;
use pernixc_handler::Handler;
use pernixc_symbol::{
    accessibility::Accessibility, get_target_root_module_id, kind::Kind,
};
use pernixc_syntax::item::module::Member as ModuleMemberSyn;
use pernixc_target::get_invocation_arguments;
use pernixc_tokio::scoped;
use tokio::task::JoinHandle;

use crate::{
    diagnostic::{Diagnostic, ItemRedefinition, RecursiveFileRequest},
    table::{
        Entry, ExternalSubmodule, MemberBuilder, ModuleKind, Naming,
        TableContext,
    },
};

impl TableContext {
    #[allow(clippy::manual_async_fn)]
    fn handle_module_member<'x>(
        self: &'x Arc<Self>,
        module_syntax: &'x pernixc_syntax::item::module::Module,
        member_builder: &'x mut MemberBuilder,
    ) -> impl std::future::Future<Output = Option<JoinHandle<()>>> + Send + 'x
    {
        async move {
            self.handle_module_member_internal(module_syntax, member_builder)
                .await
        }
    }

    #[allow(clippy::too_many_lines)]
    async fn handle_module_member_internal(
        self: &Arc<Self>,
        module_syntax: &pernixc_syntax::item::module::Module,
        member_builder: &mut MemberBuilder,
    ) -> Option<JoinHandle<()>> {
        let signature = module_syntax.signature()?;
        let identifier = signature.identifier()?;

        let access_modifier = module_syntax.access_modifier();
        let next_submodule_qualified_name = member_builder
            .symbol_qualified_name
            .iter()
            .cloned()
            .chain(std::iter::once(identifier.kind.0.clone()))
            .collect::<Arc<[_]>>();

        let accessibility = match access_modifier {
            Some(pernixc_syntax::AccessModifier::Private(_)) => {
                Accessibility::Scoped(member_builder.symbol_id)
            }
            Some(pernixc_syntax::AccessModifier::Internal(_)) => {
                Accessibility::Scoped(
                    self.engine.get_target_root_module_id(self.target_id).await,
                )
            }
            Some(pernixc_syntax::AccessModifier::Public(_)) | None => {
                Accessibility::Public
            }
        };

        let span = identifier.span;

        if let Some(member) = module_syntax.inline_body() {
            let next_submodule_id =
                member_builder.add_member(identifier, &self.engine).await;

            Some(
                self.create_module(member.content(), ModuleKind::Submodule {
                    submodule_id: next_submodule_id,
                    submodule_qualified_name: next_submodule_qualified_name,
                    accessibility,
                    span,
                })
                .await,
            )
        } else {
            let invocation_arguments =
                self.engine.get_invocation_arguments(self.target_id).await;

            let mut load_path = invocation_arguments
                .command
                .input()
                .file
                .parent()
                .map(ToOwned::to_owned)
                .unwrap_or_default();

            load_path.extend(
                next_submodule_qualified_name
                    .iter()
                    .skip(1)
                    .map(FlexStr::as_str),
            );

            load_path.set_extension("pnx");

            if self.is_root
                && invocation_arguments
                    .command
                    .input()
                    .file
                    .file_stem()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .as_ref()
                    == identifier.kind.0.as_str()
            {
                // this is the recursive file loading, so we skip it
                self.storage.receive(Diagnostic::RecursiveFileRequest(
                    RecursiveFileRequest {
                        submodule_span: identifier.span,
                        path: load_path,
                    },
                ));
                return None;
            }

            let existing_member_id = member_builder
                .member_ids_by_name
                .get(&identifier.kind.0)
                .copied();

            if let Some(existing_member_id) = existing_member_id {
                member_builder.redefinition_errors.insert(ItemRedefinition {
                    existing_id: self.target_id.make_global(existing_member_id),
                    redefinition_span: identifier.span,
                    in_id: self.target_id.make_global(member_builder.symbol_id),
                });
            } else {
                let id =
                    member_builder.add_member(identifier, &self.engine).await;

                self.external_submodules.insert(
                    id,
                    Arc::new(ExternalSubmodule {
                        path: Arc::from(load_path),
                        submodule_id: id,
                        submodule_qualified_name: next_submodule_qualified_name,
                        accessibility,
                        span,
                    }),
                );
            }

            None
        }
    }

    #[allow(clippy::needless_pass_by_value, clippy::too_many_lines)]
    pub(super) async fn handle_module_content(
        self: &Arc<Self>,
        module_content: pernixc_syntax::item::module::Content,
        module_member_builder: &mut MemberBuilder,
        imports: &mut Vec<pernixc_syntax::item::module::Import>,
    ) {
        // will be joined later
        scoped!(|tasks| async move {
            for item in
                module_content.members().filter_map(|x| x.into_line().ok())
            {
                let entry = match item {
                    ModuleMemberSyn::Module(module) => {
                        // custom handling for the module member
                        if let Some(handle) = self
                            .handle_module_member(
                                &module,
                                module_member_builder,
                            )
                            .await
                        {
                            tasks.push(handle);
                        }

                        continue;
                    }

                    ModuleMemberSyn::Trait(trait_syntax) => {
                        // custom handling for the trait member
                        if let Some(handle) = self
                            .handle_trait_member(
                                &trait_syntax,
                                module_member_builder,
                            )
                            .await
                        {
                            tasks.push(handle);
                        }

                        continue;
                    }

                    ModuleMemberSyn::Function(function_syntax) => {
                        let Some(identifier) = function_syntax
                            .signature()
                            .and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::Function)
                            .naming(Naming::Identifier(identifier.clone()))
                            .accessibility(function_syntax.access_modifier())
                            .generic_parameters_syntax(
                                function_syntax
                                    .signature()
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                function_syntax
                                    .body()
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .function_signature_syntax((
                                function_syntax
                                    .signature()
                                    .and_then(|x| x.parameters()),
                                function_syntax
                                    .signature()
                                    .and_then(|x| x.return_type()),
                            ))
                            .function_body_syntax(
                                function_syntax
                                    .body()
                                    .and_then(|x| x.members()),
                            )
                            .function_effect_annotation_syntax(
                                function_syntax
                                    .signature()
                                    .and_then(|x| x.effect_annotation()),
                            )
                            .function_unsafe_keyword(
                                function_syntax
                                    .signature()
                                    .and_then(|x| x.unsafe_keyword()),
                            )
                            .build()
                    }

                    ModuleMemberSyn::Type(type_syntax) => {
                        let Some(identifier) = type_syntax
                            .signature()
                            .and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::Type)
                            .naming(Naming::Identifier(identifier.clone()))
                            .accessibility(type_syntax.access_modifier())
                            .generic_parameters_syntax(
                                type_syntax
                                    .signature()
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                type_syntax
                                    .body()
                                    .and_then(|x| x.trailing_where_clause())
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .type_alias_syntax(
                                type_syntax.body().and_then(|x| x.r#type()),
                            )
                            .build()
                    }

                    ModuleMemberSyn::Constant(constant_syntax) => {
                        let Some(identifier) = constant_syntax
                            .signature()
                            .and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::Constant)
                            .naming(Naming::Identifier(identifier.clone()))
                            .accessibility(constant_syntax.access_modifier())
                            .generic_parameters_syntax(
                                constant_syntax
                                    .signature()
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                constant_syntax
                                    .body()
                                    .and_then(|x| x.trailing_where_clause())
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .constant_type_annotation_syntax(
                                constant_syntax
                                    .signature()
                                    .and_then(|x| x.r#type()),
                            )
                            .constant_expression_syntax(
                                constant_syntax
                                    .body()
                                    .and_then(|x| x.expression()),
                            )
                            .build()
                    }

                    ModuleMemberSyn::Struct(struct_syntax) => {
                        let Some(identifier) = struct_syntax
                            .signature()
                            .and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::Struct)
                            .naming(Naming::Identifier(identifier.clone()))
                            .accessibility(struct_syntax.access_modifier())
                            .generic_parameters_syntax(
                                struct_syntax
                                    .signature()
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                struct_syntax
                                    .body()
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .fields_syntax(struct_syntax.body())
                            .build()
                    }

                    ModuleMemberSyn::Enum(enum_syntax) => {
                        // custom handling for the enum member
                        if let Some(handle) = self
                            .handle_enum_member(
                                &enum_syntax,
                                module_member_builder,
                            )
                            .await
                        {
                            tasks.push(handle);
                        }

                        continue;
                    }

                    ModuleMemberSyn::Marker(marker_syntax) => {
                        let Some(identifier) = marker_syntax
                            .signature()
                            .and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::Marker)
                            .naming(Naming::Identifier(identifier.clone()))
                            .accessibility(marker_syntax.access_modifier())
                            .generic_parameters_syntax(
                                marker_syntax
                                    .signature()
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                marker_syntax
                                    .trailing_where_clause()
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .build()
                    }

                    ModuleMemberSyn::Import(import) => {
                        // collect the import syntax for later processing
                        imports.push(import);
                        continue;
                    }

                    ModuleMemberSyn::Implements(impls) => {
                        // custom handling for the implements member
                        self.handle_implements(impls, module_member_builder)
                            .await;

                        continue;
                    }

                    ModuleMemberSyn::Extern(ext) => {
                        // custom handling for the extern member
                        self.handle_extern(&ext, module_member_builder).await;

                        continue;
                    }

                    ModuleMemberSyn::Effect(eff) => {
                        if let Some(handle) = self
                            .handle_effect_member(&eff, module_member_builder)
                            .await
                        {
                            tasks.push(handle);
                        }

                        continue;
                    }
                };

                let member_id = module_member_builder
                    .add_member(
                        entry.naming.as_identifier().unwrap().clone(),
                        &self.engine,
                    )
                    .await;

                self.add_symbol_entry(
                    member_id,
                    module_member_builder.symbol_id,
                    entry,
                )
                .await;
            }
        });
    }
}
