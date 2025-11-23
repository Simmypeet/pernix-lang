use std::sync::Arc;

use flexstr::SharedStr;
use pernixc_arena::ID;
use pernixc_handler::{Handler, Storage};
use pernixc_hash::{HashMap, HashSet};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::runtime::executor::{self, CyclicError};
use pernixc_resolution::{
    Config,
    generic_parameter_namespace::get_generic_parameter_namespace,
    qualified_identifier::{
        MemberGeneric, Resolution, resolve_qualified_identifier,
    },
    term::{ResolutionToTypeError, resolution_to_type},
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::SourceElement;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    accessibility::symbol_accessible,
    final_implements::is_implements_final,
    kind::{Kind, get_kind},
    member::get_members,
    name::get_name,
    syntax::{
        get_implements_member_access_modifier,
        get_implements_qualified_identifier,
    },
};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::{
        ConstantParameter, LifetimeParameter, TypeParameter,
        get_generic_parameters,
    },
    lifetime::Lifetime,
    r#type::Type,
    visitor::{self, Element},
};

use crate::{
    build::{self, Output},
    implements_qualified_identifier::diagnostic::{
        AdtImplementationCannotBeFinal, AdtImplementationCannotBeNegative,
        InvalidSymbolForImplements, MarkerImplementsNotFinal,
    },
    occurrences,
};

pub mod diagnostic;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    pernixc_query::Key,
)]
#[value(Option<Arc<Resolution>>)]
pub struct Key(pub Global<pernixc_symbol::ID>);

build::register_build!(Key);

impl crate::build::Build for Key {
    type Diagnostic = diagnostic::Diagnostic;

    async fn execute(
        engine: &pernixc_query::TrackedEngine,
        key: &Self,
    ) -> Result<Output<Self>, CyclicError> {
        let qualified_identifier =
            engine.get_implements_qualified_identifier(key.0).await;

        let mut occurrences = occurrences::Occurrences::default();
        let storage = Storage::<diagnostic::Diagnostic>::new();
        let generic_parameter_namespace =
            engine.get_generic_parameter_namespace(key.0).await?;

        let resolution = match engine
            .resolve_qualified_identifier(
                &qualified_identifier,
                Config::builder()
                    .consider_adt_implements(false)
                    .observer(&mut occurrences)
                    .referring_site(key.0)
                    .extra_namespace(&generic_parameter_namespace)
                    .build(),
                &storage,
            )
            .await
        {
            Ok(resolution) => resolution,

            // couldn't resolve, but we still want to return diagnostics
            Err(pernixc_resolution::Error::Abort) => {
                return Ok(Output {
                    item: None,
                    diagnostics: storage.into_vec().into(),
                    occurrences: Arc::new(occurrences),
                });
            }

            Err(pernixc_resolution::Error::Cyclic(cyclic)) => {
                return Err(cyclic);
            }
        };

        check_valid_resolution(
            engine,
            resolution.clone(),
            qualified_identifier.span(),
            &storage,
        )
        .await?;

        // performs extra necessary check
        if let Resolution::Generic(generic) = &resolution {
            let kind = engine.get_kind(generic.id).await;

            #[allow(clippy::match_same_arms)]
            match kind {
                Kind::Marker => {
                    check_marker(
                        engine,
                        key.0,
                        qualified_identifier.span(),
                        &storage,
                    )
                    .await?;
                }

                Kind::Trait => {
                    check_trait(engine, key.0, generic.id, &storage).await?;
                }

                Kind::Struct | Kind::Enum => {
                    check_adt(engine, key.0, &storage).await?;
                }

                _ => {}
            }

            // Check for unused generic parameters
            check_unused_generic_parameters(
                engine,
                key.0,
                &generic.generic_arguments,
                &storage,
            )
            .await?;
        }

        Ok(Output {
            item: Some(Arc::new(resolution)),
            diagnostics: storage.into_vec().into(),
            occurrences: Arc::new(occurrences),
        })
    }
}

async fn check_valid_resolution(
    tracked_engine: &pernixc_query::TrackedEngine,
    resolution: Resolution,
    qualified_identifier: RelativeSpan,
    storage: &Storage<diagnostic::Diagnostic>,
) -> Result<(), executor::CyclicError> {
    match resolution {
        Resolution::Module(global) => {
            storage.receive(
                diagnostic::Diagnostic::InvalidSymbolForImplements(
                    InvalidSymbolForImplements {
                        qualified_identifier_span: qualified_identifier,
                        symbol_id: global,
                    },
                ),
            );

            Ok(())
        }

        Resolution::Variant(variant) => {
            storage.receive(
                diagnostic::Diagnostic::InvalidSymbolForImplements(
                    InvalidSymbolForImplements {
                        qualified_identifier_span: qualified_identifier,
                        symbol_id: variant.variant_id,
                    },
                ),
            );

            Ok(())
        }

        Resolution::Generic(generic) => {
            let kind = tracked_engine.get_kind(generic.id).await;

            match kind {
                Kind::Trait | Kind::Marker | Kind::Struct | Kind::Enum => {
                    // valid implements symbol
                }

                Kind::Type => {
                    // if it's a type, it must be a struct/enum
                    let ty = match tracked_engine
                        .resolution_to_type(Resolution::Generic(generic))
                        .await
                    {
                        Ok(ty) => ty,
                        Err(ResolutionToTypeError::Failed(_)) => unreachable!(),
                        Err(ResolutionToTypeError::Cyclic(cyclic)) => {
                            return Err(cyclic);
                        }
                    };

                    if !matches!(
                        &ty,
                        Type::Symbol(sym)
                            if is_adt_type(tracked_engine, sym).await
                    ) {
                        storage.receive(
                            diagnostic::Diagnostic::InvalidTypeForImplements(
                                diagnostic::InvalidTypeForImplements {
                                    qualified_identifier_span:
                                        qualified_identifier,
                                    r#type: ty,
                                },
                            ),
                        );
                    }
                }

                _ => {
                    storage.receive(
                        diagnostic::Diagnostic::InvalidSymbolForImplements(
                            InvalidSymbolForImplements {
                                qualified_identifier_span: qualified_identifier,
                                symbol_id: generic.id,
                            },
                        ),
                    );
                }
            }

            Ok(())
        }

        Resolution::MemberGeneric(MemberGeneric { id: global_id, .. }) => {
            storage.receive(
                diagnostic::Diagnostic::InvalidSymbolForImplements(
                    InvalidSymbolForImplements {
                        qualified_identifier_span: qualified_identifier,
                        symbol_id: global_id,
                    },
                ),
            );

            Ok(())
        }
    }
}

async fn is_adt_type(
    engine: &pernixc_query::TrackedEngine,
    sym_ty: &Symbol,
) -> bool {
    let kind = engine.get_kind(sym_ty.id).await;
    matches!(kind, Kind::Struct | Kind::Enum)
}

async fn check_marker(
    engine: &pernixc_query::TrackedEngine,
    implements: Global<pernixc_symbol::ID>,
    qualified_identifier: RelativeSpan,
    storage: &Storage<diagnostic::Diagnostic>,
) -> Result<(), executor::CyclicError> {
    let is_final = engine.is_implements_final(implements).await;

    if !is_final {
        storage.receive(diagnostic::Diagnostic::MarkerImplementsNotFinal(
            MarkerImplementsNotFinal {
                qualified_identifier_span: qualified_identifier,
            },
        ));
    }

    // we'll check if he marker implementation has any members (it shouldn't).
    if engine.get_kind(implements).await != Kind::PositiveImplementation {
        return Ok(());
    }

    let members = engine.get_members(implements).await;

    for member_id in members
        .member_ids_by_name
        .values()
        .copied()
        .chain(members.unnameds.iter().copied())
    {
        storage.receive(
            diagnostic::Diagnostic::MemberInMarkerImplementationIsNotAllowed(
                diagnostic::MemberInMarkerImplementationIsNotAllowed {
                    implements_member_id: implements
                        .target_id
                        .make_global(member_id),
                },
            ),
        );
    }

    Ok(())
}

#[allow(clippy::too_many_lines)]
async fn check_trait(
    engine: &pernixc_query::TrackedEngine,
    implements: Global<pernixc_symbol::ID>,
    trait_id: Global<pernixc_symbol::ID>,
    storage: &Storage<diagnostic::Diagnostic>,
) -> Result<(), executor::CyclicError> {
    // no need to check the member
    if engine.get_kind(implements).await != Kind::PositiveImplementation {
        return Ok(());
    }

    let implements_members = engine.get_members(implements).await;
    let trait_members = engine.get_members(trait_id).await;

    // Maps member name to (implementation_member_id, trait_member_id)
    let mut implemented_member_by_name: HashMap<
        SharedStr,
        (Global<pernixc_symbol::ID>, Option<Global<pernixc_symbol::ID>>),
    > = HashMap::default();

    // Check each implementation member
    for implements_member_id in implements_members
        .member_ids_by_name
        .values()
        .copied()
        .map(|x| implements.target_id.make_global(x))
    {
        let member_name = engine.get_name(implements_member_id).await;
        let trait_equivalent_id = trait_members
            .member_ids_by_name
            .get(&member_name)
            .map(|&id| trait_id.target_id.make_global(id));

        // Check if trait implementation members have access modifiers (they
        // shouldn't)
        let access_modifier = engine
            .get_implements_member_access_modifier(implements_member_id)
            .await;
        if access_modifier.is_some() {
            storage.receive(
                diagnostic::Diagnostic::TraitMemberCannotHaveAccessModifier(
                    diagnostic::TraitMemberCannotHaveAccessModifier {
                        implementation_member_id: implements_member_id,
                    },
                ),
            );
        }

        if let Some(trait_member_id) = trait_equivalent_id {
            // Check if the kinds match
            let impl_kind = engine.get_kind(implements_member_id).await;
            let trait_kind = engine.get_kind(trait_member_id).await;

            let kinds_match = matches!(
                (trait_kind, impl_kind),
                (Kind::TraitFunction, Kind::ImplementationFunction)
                    | (Kind::TraitType, Kind::ImplementationType)
                    | (Kind::TraitConstant, Kind::ImplementationConstant)
            );

            if !kinds_match {
                storage.receive(
                    diagnostic::Diagnostic::TraitMemberKindMismatch(
                        diagnostic::TraitMemberKindMismatch {
                            trait_member_id,
                            implementation_member_id: implements_member_id,
                        },
                    ),
                );
            }

            // Check if the trait member is accessible from the implementation
            // site
            let is_accessible =
                engine.symbol_accessible(implements, trait_member_id).await;

            if !is_accessible {
                storage.receive(
                    diagnostic::Diagnostic::InaccessibleTraitMember(
                        diagnostic::InaccessibleTraitMember {
                            trait_member_id,
                            implementation_member_id: implements_member_id,
                        },
                    ),
                );
            }

            assert!(
                implemented_member_by_name
                    .insert(
                        member_name.clone(),
                        (implements_member_id, Some(trait_member_id)),
                    )
                    .is_none(),
                "should've no duplication"
            );
        } else {
            // Implementation member doesn't correspond to any trait member
            storage.receive(
                diagnostic::Diagnostic::ExtraneousImplementationMember(
                    diagnostic::ExtraneousImplementationMember {
                        implementation_member_id: implements_member_id,
                    },
                ),
            );

            implemented_member_by_name
                .insert(member_name.clone(), (implements_member_id, None));
        }
    }

    // Check for missing trait members
    let mut unimplemented_trait_members = Vec::new();

    for (trait_member_name, &trait_member_id) in
        &trait_members.member_ids_by_name
    {
        let trait_member_global_id =
            trait_id.target_id.make_global(trait_member_id);

        if !implemented_member_by_name.contains_key(trait_member_name) {
            unimplemented_trait_members.push(trait_member_global_id);
        }
    }

    // Emit a single diagnostic for all unimplemented members if any
    if !unimplemented_trait_members.is_empty() {
        storage.receive(diagnostic::Diagnostic::TraitMemberNotImplemented(
            diagnostic::TraitMemberNotImplemented {
                unimplemented_trait_member_ids: unimplemented_trait_members,
                implementation_id: implements,
            },
        ));
    }

    Ok(())
}

async fn check_adt(
    engine: &pernixc_query::TrackedEngine,
    implements: Global<pernixc_symbol::ID>,
    storage: &Storage<diagnostic::Diagnostic>,
) -> Result<(), executor::CyclicError> {
    let kind = engine.get_kind(implements).await;

    // Check if it's a negative implementation
    if kind == Kind::NegativeImplementation {
        storage.receive(
            diagnostic::Diagnostic::AdtImplementationCannotBeNegative(
                AdtImplementationCannotBeNegative {
                    implementation_id: implements,
                },
            ),
        );
    }

    // Check if it's a final implementation
    let is_final = engine.is_implements_final(implements).await;
    if is_final {
        storage.receive(
            diagnostic::Diagnostic::AdtImplementationCannotBeFinal(
                AdtImplementationCannotBeFinal {
                    implementation_id: implements,
                },
            ),
        );
    }

    // Check access modifiers for ADT implementation members (only for positive
    // implementations)
    if kind == Kind::PositiveImplementation {
        let implements_members = engine.get_members(implements).await;

        // Check each implementation member
        for implements_member_id in implements_members
            .member_ids_by_name
            .values()
            .copied()
            .map(|x| implements.target_id.make_global(x))
        {
            // Check if ADT implementation members have access modifiers (they
            // should)
            let access_modifier = engine
                .get_implements_member_access_modifier(implements_member_id)
                .await;
            if access_modifier.is_none() {
                storage.receive(
                    diagnostic::Diagnostic::AdtMemberMissingAccessModifier(
                        diagnostic::AdtMemberMissingAccessModifier {
                            implementation_member_id: implements_member_id,
                        },
                    ),
                );
            }
        }
    }

    Ok(())
}

/// A visitor that collects all used generic parameter IDs from generic
/// arguments. It skips parameters that appear under trait or trait associated
/// types.
#[derive(Debug, Default)]
struct UsedParameterCollector {
    used_lifetime_ids: HashSet<ID<LifetimeParameter>>,
    used_type_ids: HashSet<ID<TypeParameter>>,
    used_constant_ids: HashSet<ID<ConstantParameter>>,
    current_symbol_id: Global<pernixc_symbol::ID>,
}

impl UsedParameterCollector {
    fn handle_lifetime(&mut self, lifetime: &Lifetime) {
        if let Lifetime::Parameter(param) = lifetime
            && param.parent_id == self.current_symbol_id
        {
            self.used_lifetime_ids.insert(param.id);
        }
    }

    fn handle_type(&mut self, ty: &Type) {
        // if it's a trait associated type, we don't go deeper
        if ty.is_trait_member() {
            return;
        }

        // found a type parameter, try to collect it
        if let Some(ty) = ty.as_parameter() {
            if ty.parent_id == self.current_symbol_id {
                self.used_type_ids.insert(ty.id);
            }

            return;
        }

        // go deeper
        let _ = ty.accept_one_level(self);
    }

    fn handle_constant(&mut self, constant: &Constant) {
        // found a constant parameter, try to collect it
        if let Some(constant) = constant.as_parameter() {
            if constant.parent_id == self.current_symbol_id {
                self.used_constant_ids.insert(constant.id);
            }

            return;
        }

        // go deeper
        let _ = constant.accept_one_level(self);
    }
}

impl visitor::Visitor<'_, Lifetime> for UsedParameterCollector {
    fn visit(
        &mut self,
        term: &'_ Lifetime,
        _: <Lifetime as visitor::Element>::Location,
    ) -> bool {
        self.handle_lifetime(term);
        true
    }
}

impl visitor::Visitor<'_, Type> for UsedParameterCollector {
    fn visit(
        &mut self,
        term: &'_ Type,
        _: <Type as visitor::Element>::Location,
    ) -> bool {
        self.handle_type(term);
        true
    }
}

impl visitor::Visitor<'_, Constant> for UsedParameterCollector {
    fn visit(
        &mut self,
        term: &'_ Constant,
        _: <Constant as visitor::Element>::Location,
    ) -> bool {
        self.handle_constant(term);
        true
    }
}

/// Check for unused generic parameters in the implements declaration.
async fn check_unused_generic_parameters(
    engine: &pernixc_query::TrackedEngine,
    implements_id: Global<pernixc_symbol::ID>,
    generic_arguments: &GenericArguments,
    storage: &Storage<diagnostic::Diagnostic>,
) -> Result<(), executor::CyclicError> {
    // Get the generic parameters defined on this implements
    let generic_parameters =
        engine.get_generic_parameters(implements_id).await?;

    // Collect used parameters from generic arguments
    let mut collector = UsedParameterCollector {
        current_symbol_id: implements_id,
        ..Default::default()
    };

    // Visit all lifetimes
    for lifetime in &generic_arguments.lifetimes {
        collector.handle_lifetime(lifetime);
    }

    // Visit all types
    for ty in &generic_arguments.types {
        collector.handle_type(ty);
    }

    // Visit all constants
    for constant in &generic_arguments.constants {
        collector.handle_constant(constant);
    }

    // Check for unused lifetime parameters
    for (param_id, param) in generic_parameters.lifetime_parameters_as_order() {
        if !collector.used_lifetime_ids.contains(&param_id)
            && let Some(span) = param.span
        {
            storage.receive(diagnostic::Diagnostic::UnusedGenericParameter(
                diagnostic::UnusedGenericParameter {
                    implementation_id: implements_id,
                    unused_parameter_span: span,
                    parameter_name: param.name.clone(),
                },
            ));
        }
    }

    // Check for unused type parameters
    for (param_id, param) in generic_parameters.type_parameters_as_order() {
        if !collector.used_type_ids.contains(&param_id)
            && let Some(span) = param.span
        {
            storage.receive(diagnostic::Diagnostic::UnusedGenericParameter(
                diagnostic::UnusedGenericParameter {
                    implementation_id: implements_id,
                    unused_parameter_span: span,
                    parameter_name: param.name.clone(),
                },
            ));
        }
    }

    // Check for unused constant parameters
    for (param_id, param) in generic_parameters.constant_parameters_as_order() {
        if !collector.used_constant_ids.contains(&param_id)
            && let Some(span) = param.span
        {
            storage.receive(diagnostic::Diagnostic::UnusedGenericParameter(
                diagnostic::UnusedGenericParameter {
                    implementation_id: implements_id,
                    unused_parameter_span: span,
                    parameter_name: param.name.clone(),
                },
            ));
        }
    }

    Ok(())
}

#[derive(Debug, Default)]
pub struct ExtractImplementsID;

impl executor::Executor<pernixc_semantic_element::implements::Key>
    for ExtractImplementsID
{
    async fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &pernixc_semantic_element::implements::Key,
    ) -> Result<Option<Global<pernixc_symbol::ID>>, executor::CyclicError> {
        let resolution = engine.query(&Key(key.0)).await?;
        Ok(resolution.as_ref().map(|x| x.global_id()))
    }
}

pernixc_register::register!(
    pernixc_semantic_element::implements::Key,
    ExtractImplementsID
);

#[derive(Debug, Default)]
pub struct ExtractGenericArguments;

impl executor::Executor<pernixc_semantic_element::implements_arguments::Key>
    for ExtractGenericArguments
{
    async fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &pernixc_semantic_element::implements_arguments::Key,
    ) -> Result<Option<Arc<GenericArguments>>, executor::CyclicError> {
        let resolution = engine.query(&Key(key.0)).await?;
        Ok(resolution
            .as_ref()
            .and_then(|x| x.as_generic())
            .map(|x| Arc::new(x.generic_arguments.clone())))
    }
}

pernixc_register::register!(
    pernixc_semantic_element::implements_arguments::Key,
    ExtractGenericArguments
);
