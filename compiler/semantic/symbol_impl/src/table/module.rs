use std::sync::Arc;

use flexstr::FlexStr;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    accessibility::Accessibility, get_target_root_module_id, kind::Kind,
};
use pernixc_syntax::item::module::Member as ModuleMemberSyn;
use pernixc_target::get_invocation_arguments;
use pernixc_tokio::scoped;
use tokio::task::JoinHandle;

use crate::{
    diagnostic::{Diagnostic, RecursiveFileRequest},
    table::{
        ExternalSubmodule, ModuleKind,
        builder::{Builder, MemberBuilder},
    },
};

impl Builder {
    #[allow(clippy::manual_async_fn)]
    fn handle_module_members<'x>(
        self: &'x Arc<Self>,
        module_syntax: &'x pernixc_syntax::item::module::Module,
        member_builder: &'x mut MemberBuilder,
    ) -> impl std::future::Future<Output = Option<JoinHandle<()>>> + Send + 'x
    {
        async move {
            self.handle_module_members_internal(module_syntax, member_builder)
                .await
        }
    }

    #[allow(clippy::too_many_lines)]
    async fn handle_module_members_internal(
        self: &Arc<Self>,
        module_syntax: &pernixc_syntax::item::module::Module,
        member_builder: &mut MemberBuilder,
    ) -> Option<JoinHandle<()>> {
        let signature = module_syntax.signature()?;
        let identifier = signature.identifier()?;

        let access_modifier = module_syntax.access_modifier();
        let next_submodule_qualified_name = member_builder
            .extend_qualified_name_sequence(identifier.kind.0.clone());

        let accessibility = self
            .create_accessibility(
                member_builder.current_symbol_id(),
                access_modifier.as_ref(),
            )
            .await;
        let span = identifier.span;

        if let Some(member) = module_syntax.inline_body() {
            let next_submodule_id =
                member_builder.add_member(identifier, self.engine()).await;

            Some(
                self.create_module(
                    member.content(),
                    Some(module_syntax.span()),
                    ModuleKind::Submodule {
                        submodule_id: next_submodule_id,
                        submodule_qualified_name: next_submodule_qualified_name,
                        accessibility,
                        span,
                    },
                )
                .await
                .0,
            )
        } else {
            let invocation_arguments =
                self.engine().get_invocation_arguments(self.target_id()).await;

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

            if self.is_root()
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
                self.add_diagnostic(Diagnostic::RecursiveFileRequest(
                    RecursiveFileRequest {
                        submodule_span: identifier.span,
                        path: load_path,
                    },
                ));
                return None;
            }

            if let Some(id) = member_builder
                .add_member_no_unnamed(identifier, self.engine())
                .await
            {
                self.insert_external_submodule(
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
    async fn handle_module_content(
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
                let Some(handle) = self
                    .handle_module_member(item, module_member_builder, imports)
                    .await
                else {
                    continue;
                };

                tasks.push(handle);
            }
        });
    }

    #[allow(clippy::too_many_lines)]
    async fn handle_module_member(
        self: &Arc<Self>,
        member: pernixc_syntax::item::module::Member,
        module_member_builder: &mut MemberBuilder,
        imports: &mut Vec<pernixc_syntax::item::module::Import>,
    ) -> Option<JoinHandle<()>> {
        match member {
            ModuleMemberSyn::Module(module) => {
                self.handle_module_members(&module, module_member_builder).await
            }

            ModuleMemberSyn::Trait(trait_syntax) => {
                self.handle_trait_member(&trait_syntax, module_member_builder)
                    .await
            }

            ModuleMemberSyn::Function(function_syntax) => {
                let identifier =
                    function_syntax.signature().and_then(|x| x.identifier())?;

                let id = module_member_builder
                    .add_member(identifier.clone(), self.engine())
                    .await;

                self.insert_kind(id, Kind::Function);
                self.insert_scope_span(id, function_syntax.span());
                self.insert_name_identifier(id, &identifier);
                self.insert_accessibility_by_access_modifier(
                    id,
                    module_member_builder.current_symbol_id(),
                    function_syntax.access_modifier().as_ref(),
                )
                .await;
                self.insert_generic_parameters_syntax(
                    id,
                    function_syntax
                        .signature()
                        .and_then(|x| x.generic_parameters()),
                );
                self.insert_where_clause_syntax(
                    id,
                    function_syntax
                        .body()
                        .and_then(|x| x.where_clause())
                        .and_then(|x| x.predicates()),
                );
                self.insert_function_signature_syntax(
                    id,
                    function_syntax.signature().and_then(|x| x.parameters()),
                    function_syntax.signature().and_then(|x| x.return_type()),
                );
                self.insert_function_body_syntax(
                    id,
                    function_syntax.body().and_then(|x| x.members()),
                );
                self.insert_function_effect_annotation_syntax(
                    id,
                    function_syntax
                        .signature()
                        .and_then(|x| x.effect_annotation()),
                );
                self.insert_function_unsafe_keyword(
                    id,
                    function_syntax
                        .signature()
                        .and_then(|x| x.unsafe_keyword()),
                );

                None
            }

            ModuleMemberSyn::Type(type_syntax) => {
                let identifier =
                    type_syntax.signature().and_then(|x| x.identifier())?;

                let id = module_member_builder
                    .add_member(identifier.clone(), self.engine())
                    .await;

                self.insert_kind(id, Kind::Type);
                self.insert_scope_span(id, type_syntax.span());
                self.insert_name_identifier(id, &identifier);
                self.insert_accessibility_by_access_modifier(
                    id,
                    module_member_builder.current_symbol_id(),
                    type_syntax.access_modifier().as_ref(),
                )
                .await;
                self.insert_generic_parameters_syntax(
                    id,
                    type_syntax
                        .signature()
                        .and_then(|x| x.generic_parameters()),
                );
                self.insert_where_clause_syntax(
                    id,
                    type_syntax
                        .body()
                        .and_then(|x| x.trailing_where_clause())
                        .and_then(|x| x.where_clause())
                        .and_then(|x| x.predicates()),
                );
                self.insert_type_alias_syntax(
                    id,
                    type_syntax.body().and_then(|x| x.r#type()),
                );

                None
            }

            ModuleMemberSyn::Constant(constant_syntax) => {
                let identifier =
                    constant_syntax.signature().and_then(|x| x.identifier())?;

                let id = module_member_builder
                    .add_member(identifier.clone(), self.engine())
                    .await;

                self.insert_kind(id, Kind::Constant);
                self.insert_scope_span(id, constant_syntax.span());
                self.insert_name_identifier(id, &identifier);
                self.insert_accessibility_by_access_modifier(
                    id,
                    module_member_builder.current_symbol_id(),
                    constant_syntax.access_modifier().as_ref(),
                )
                .await;
                self.insert_generic_parameters_syntax(
                    id,
                    constant_syntax
                        .signature()
                        .and_then(|x| x.generic_parameters()),
                );
                self.insert_where_clause_syntax(
                    id,
                    constant_syntax
                        .body()
                        .and_then(|x| x.trailing_where_clause())
                        .and_then(|x| x.where_clause())
                        .and_then(|x| x.predicates()),
                );
                self.insert_constant_type_annotation_syntax(
                    id,
                    constant_syntax.signature().and_then(|x| x.r#type()),
                );
                self.insert_constant_expression_syntax(
                    id,
                    constant_syntax.body().and_then(|x| x.expression()),
                );

                None
            }

            ModuleMemberSyn::Struct(struct_syntax) => {
                let identifier =
                    struct_syntax.signature().and_then(|x| x.identifier())?;

                let id = module_member_builder
                    .add_member(identifier.clone(), self.engine())
                    .await;

                self.insert_kind(id, Kind::Struct);
                self.insert_scope_span(id, struct_syntax.span());
                self.insert_name_identifier(id, &identifier);
                self.insert_accessibility_by_access_modifier(
                    id,
                    module_member_builder.current_symbol_id(),
                    struct_syntax.access_modifier().as_ref(),
                )
                .await;
                self.insert_generic_parameters_syntax(
                    id,
                    struct_syntax
                        .signature()
                        .and_then(|x| x.generic_parameters()),
                );
                self.insert_where_clause_syntax(
                    id,
                    struct_syntax
                        .body()
                        .and_then(|x| x.where_clause())
                        .and_then(|x| x.predicates()),
                );
                self.insert_struct_field_syntax(id, struct_syntax.body());

                None
            }

            ModuleMemberSyn::Enum(enum_syntax) => {
                self.handle_enum_member(&enum_syntax, module_member_builder)
                    .await
            }

            ModuleMemberSyn::Marker(marker_syntax) => {
                let identifier =
                    marker_syntax.signature().and_then(|x| x.identifier())?;

                let id = module_member_builder
                    .add_member(identifier.clone(), self.engine())
                    .await;

                self.insert_kind(id, Kind::Marker);
                self.insert_scope_span(id, marker_syntax.span());
                self.insert_name_identifier(id, &identifier);
                self.insert_accessibility_by_access_modifier(
                    id,
                    module_member_builder.current_symbol_id(),
                    marker_syntax.access_modifier().as_ref(),
                )
                .await;
                self.insert_generic_parameters_syntax(
                    id,
                    marker_syntax
                        .signature()
                        .and_then(|x| x.generic_parameters()),
                );
                self.insert_where_clause_syntax(
                    id,
                    marker_syntax
                        .trailing_where_clause()
                        .and_then(|x| x.where_clause())
                        .and_then(|x| x.predicates()),
                );

                None
            }

            ModuleMemberSyn::Import(import) => {
                // collect the import syntax for later processing
                imports.push(import);

                None
            }

            ModuleMemberSyn::Implements(impls) => {
                // custom handling for the implements member
                self.handle_implements(impls, module_member_builder).await
            }

            ModuleMemberSyn::Extern(ext) => {
                // custom handling for the extern member
                self.handle_extern(&ext, module_member_builder).await;

                None
            }

            ModuleMemberSyn::Effect(eff) => {
                self.handle_effect_member(&eff, module_member_builder).await
            }
        }
    }

    /// Creates a new module symbol and populates the symbol table with its
    /// members.
    pub async fn create_module(
        self: &Arc<Self>,
        module_content: Option<pernixc_syntax::item::module::Content>,
        scope_span: Option<RelativeSpan>,
        module_kind: ModuleKind,
    ) -> (JoinHandle<()>, pernixc_symbol::ID) {
        // extract the information about the module
        let (accessibility, current_module_id, module_qualified_name, span) =
            match module_kind {
                ModuleKind::Root => {
                    let invocation_arguments = self
                        .engine()
                        .get_invocation_arguments(self.target_id())
                        .await;

                    let target_name =
                        invocation_arguments.command.input().target_name();

                    let current_module_id = self
                        .engine()
                        .get_target_root_module_id(self.target_id())
                        .await;

                    let module_qualified_name = Arc::from([target_name]);

                    (
                        Accessibility::Public,
                        current_module_id,
                        module_qualified_name,
                        None,
                    )
                }
                ModuleKind::Submodule {
                    submodule_id,
                    submodule_qualified_name,
                    accessibility,
                    span,
                } => (
                    accessibility,
                    submodule_id,
                    submodule_qualified_name,
                    Some(span),
                ),
            };

        let builder = self.clone();

        (
            tokio::spawn(async move {
                let mut member_builder = MemberBuilder::new(
                    current_module_id,
                    module_qualified_name,
                    builder.target_id(),
                );
                let mut imports = Vec::new();

                if let Some(module_content) = module_content {
                    builder
                        .handle_module_content(
                            module_content,
                            &mut member_builder,
                            &mut imports,
                        )
                        .await;
                }

                builder.insert_imports(current_module_id, imports.into());
                builder.insert_span(current_module_id, span);
                builder.insert_maybe_scope_span(current_module_id, scope_span);
                builder.insert_accessibility(current_module_id, accessibility);
                builder.insert_name(
                    current_module_id,
                    member_builder.last_name().clone(),
                );
                builder.insert_kind(current_module_id, Kind::Module);
                builder.insert_member_from_builder(
                    current_module_id,
                    member_builder,
                );
            }),
            current_module_id,
        )
    }
}
