//! Defines all kinds of diagnostics occurred during resolution.

use bon::Builder;
use derive_more::From;
use pernixc_diagnostic::{Highlight, Rendered, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::import::get_import_map;
use pernixc_source_file::ByteIndex;
use pernixc_symbol::{
    kind::{Kind, get_kind},
    member::try_get_members,
    name::get_qualified_name,
    source_map::to_absolute_span,
};
use pernixc_target::{Global, get_target_map};
use pernixc_term::{
    display::Display,
    generic_parameters::{
        ConstantParameterID, GenericKind, GenericParameter,
        InstanceParameterID, TypeParameterID, get_generic_parameters,
    },
    r#type::Type,
};
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::qualified_identifier::Resolution;

/// Enumeration of all kinds of diagnostic generated during resolution.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Diagnostic {
    MoreThanOneUnpackedInTupleType(MoreThanOneUnpackedInTupleType),
    MisorderedGenericArgument(MisorderedGenericArgument),
    ThisNotFound(ThisNotFound),
    SymbolNotFound(SymbolNotFound),
    SymbolIsNotAccessible(SymbolIsNotAccessible),
    LifetimeParameterNotFound(LifetimeParameterNotFound),
    UnexpectedInference(UnexpectedInference),
    MismatchedGenericArgumentCount(MismatchedGenericArgumentCount),
    NoGenericArgumentsRequired(NoGenericArgumentsRequired),
    ExpectType(ExpectType),
    ExpectModule(ExpectModule),
    ExpectInstance(ExpectInstance),
    ExpectTrait(ExpectTrait),
    ExpectEffect(ExpectEffect),
    NoMemberInType(NoMemberInType),
    NoMemberInFunction(NoMemberInFunction),
    MismatchedKindInArgument(MismatchedKindInArgument),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        match self {
            Self::MoreThanOneUnpackedInTupleType(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::MisorderedGenericArgument(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::ThisNotFound(diagnostic) => diagnostic.report(engine).await,
            Self::SymbolNotFound(diagnostic) => diagnostic.report(engine).await,
            Self::SymbolIsNotAccessible(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::LifetimeParameterNotFound(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::UnexpectedInference(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::MismatchedGenericArgumentCount(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::NoGenericArgumentsRequired(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::ExpectType(diagnostic) => diagnostic.report(engine).await,
            Self::ExpectModule(diagnostic) => diagnostic.report(engine).await,
            Self::NoMemberInType(diagnostic) => diagnostic.report(engine).await,
            Self::NoMemberInFunction(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::MismatchedKindInArgument(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::ExpectInstance(diagnostic) => diagnostic.report(engine).await,
            Self::ExpectTrait(diagnostic) => diagnostic.report(engine).await,
            Self::ExpectEffect(diagnostic) => diagnostic.report(engine).await,
        }
    }
}

/// The `this` keyword was used in the wrong context.
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
    Encode,
    Decode,
)]
pub struct ThisNotFound {
    /// The span where the `this` keyword was used.
    pub this_span: RelativeSpan,
}

impl Report for ThisNotFound {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::new(
                engine.to_absolute_span(&self.this_span).await,
                Some(
                    "`this` keyword is only available in `implements`, \
                     `struct`, `enum`, or `trait`"
                        .to_string(),
                ),
            ))
            .message("the `this` keyword is not allowed here")
            .build()
    }
}

/// The lifetime parameter was not found in the given scope.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct LifetimeParameterNotFound {
    /// The span where the lifetime parameter was referred from.
    pub referred_span: RelativeSpan,

    /// The name of the lifetime that was searched.
    pub name: Interned<str>,

    /// The site where the search occurred.
    pub referring_site: Global<pernixc_symbol::ID>,
}

impl Report for LifetimeParameterNotFound {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let referring_site_qualified_name =
            engine.get_qualified_name(self.referring_site).await;

        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::new(
                engine.to_absolute_span(&self.referred_span).await,
                Some(format!(
                    "the lifetime parameter `{}` was not found in \
                     `{referring_site_qualified_name}`",
                    &*self.name
                )),
            ))
            .message("cannot find the lifetime parameter")
            .build()
    }
}

/// The inference term isn't allowed in the given context.
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
    Encode,
    Decode,
)]
pub struct UnexpectedInference {
    /// Span where the inference term was found.
    pub unexpected_span: RelativeSpan,

    /// The kind of the inference term.
    pub generic_kind: GenericKind,
}

impl Report for UnexpectedInference {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::new(
                engine.to_absolute_span(&self.unexpected_span).await,
                Some(format!(
                    "expected an explicit {} to be inserted here",
                    match self.generic_kind {
                        GenericKind::Type => "type",
                        GenericKind::Lifetime => "lifetime",
                        GenericKind::Constant => "constant",
                        GenericKind::Instance => "instance",
                    }
                )),
            ))
            .message(format!("{} inference is not allowed here", match self
                .generic_kind
            {
                GenericKind::Type => "type",
                GenericKind::Lifetime => "lifetime",
                GenericKind::Constant => "constant",
                GenericKind::Instance => "instance",
            }))
            .build()
    }
}

/// The type was expected but the non-type symbol was found.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Builder,
)]
pub struct ExpectType {
    /// The span where the non-type symbol was found.
    non_type_symbol_span: RelativeSpan,

    /// The resolved symbol ID where the non-type symbol was found.
    resolved_resolution: Resolution,
}

impl Report for ExpectType {
    async fn report(
        &self,
        table: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let found_string = self.resolved_resolution.found_string(table).await;

        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::new(
                table.to_absolute_span(&self.non_type_symbol_span).await,
                Some(format!(
                    "the type was expected but found `{found_string}`",
                )),
            ))
            .message("type expected")
            .build()
    }
}

/// The instance was expected but the non-instance symbol was found.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Builder,
)]
pub struct ExpectInstance {
    /// The span where the non-instance symbol was found.
    non_instance_symbol_span: RelativeSpan,

    /// The resolved symbol ID where the non-instance symbol was found.
    resolved_resolution: Resolution,
}

impl Report for ExpectInstance {
    async fn report(
        &self,
        table: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let found =
            if let Some(global_id) = self.resolved_resolution.global_id() {
                let qualified_name = table.get_qualified_name(global_id).await;
                let kind = table.get_kind(global_id).await;

                format!("`{} {qualified_name}`", kind.kind_str())
            } else {
                match &self.resolved_resolution {
                    Resolution::Module(_)
                    | Resolution::Variant(_)
                    | Resolution::Generic(_)
                    | Resolution::MemberGeneric(_)
                    | Resolution::InstanceAssociatedFunction(_) => {
                        unreachable!("should've gotten a global_id()")
                    }

                    Resolution::Type(ty) => {
                        let mut string = "`type ".to_string();
                        ty.write_async(table, &mut string).await.unwrap();
                        string.push('`');

                        string
                    }

                    Resolution::Instance(_) => {
                        unreachable!(
                            "this is already an instance, should've not \
                             caused this error"
                        )
                    }
                }
            };

        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::new(
                table.to_absolute_span(&self.non_instance_symbol_span).await,
                Some(format!("the instance was expected but found `{found}`",)),
            ))
            .message("instance expected")
            .build()
    }
}

/// The trait was expected but the non-trait symbol was found.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Builder,
)]
pub struct ExpectTrait {
    /// The span where the non-trait symbol was found.
    non_trait_symbol_span: RelativeSpan,

    /// The resolved symbol ID where the non-trait symbol was found.
    resolved_resolution: Resolution,
}

impl Report for ExpectTrait {
    async fn report(
        &self,
        table: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let found =
            if let Some(global_id) = self.resolved_resolution.global_id() {
                let qualified_name = table.get_qualified_name(global_id).await;
                let kind = table.get_kind(global_id).await;

                format!("`{} {qualified_name}`", kind.kind_str())
            } else {
                match &self.resolved_resolution {
                    Resolution::Module(_)
                    | Resolution::Variant(_)
                    | Resolution::Generic(_)
                    | Resolution::MemberGeneric(_)
                    | Resolution::InstanceAssociatedFunction(_) => {
                        unreachable!("should've gotten a global_id()")
                    }

                    Resolution::Type(ty) => {
                        let mut string = "`type ".to_string();
                        ty.write_async(table, &mut string).await.unwrap();
                        string.push('`');

                        string
                    }

                    Resolution::Instance(instance) => {
                        let mut string = "`instance ".to_string();
                        instance.write_async(table, &mut string).await.unwrap();
                        string.push('`');

                        string
                    }
                }
            };

        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::new(
                table.to_absolute_span(&self.non_trait_symbol_span).await,
                Some(format!("the trait was expected but found `{found}`",)),
            ))
            .message("trait expected")
            .build()
    }
}

/// The tuple type contains more than one unpacked type.
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
    Encode,
    Decode,
)]
pub struct MoreThanOneUnpackedInTupleType {
    /// The span where the illegal tuple type was found.
    pub illegal_tuple_type_span: RelativeSpan,
}

impl Report for MoreThanOneUnpackedInTupleType {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::new(
                engine.to_absolute_span(&self.illegal_tuple_type_span).await,
                None,
            ))
            .message(
                "the tuple type cannot contain more than one unpacked type",
            )
            .build()
    }
}

/// The generic arguments were supplied in the wrong order.
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
    Encode,
    Decode,
)]
pub struct MisorderedGenericArgument {
    /// The kind of the mis-ordered generic argument.
    pub generic_kind: GenericKind,

    /// The span of the generic argument.
    pub generic_argument: RelativeSpan,
}

impl Report for MisorderedGenericArgument {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::new(
                engine.to_absolute_span(&self.generic_argument).await,
                match self.generic_kind {
                    GenericKind::Type => Some(
                        "can't be supplied after constant or instance \
                         arguments"
                            .to_string(),
                    ),
                    GenericKind::Lifetime => Some(
                        "can't be supplied after type, constant, or instance \
                         arguments"
                            .to_string(),
                    ),
                    GenericKind::Constant => Some(
                        "can't be supplied after type or instance arguments"
                            .to_string(),
                    ),
                    GenericKind::Instance => None,
                },
            ))
            .message("the generic argument was supplied in the wrong order")
            .build()
    }
}

/// Generic arguments count mismatch.
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
    Encode,
    Decode,
)]
pub struct MismatchedGenericArgumentCount {
    /// The kind of the generic parameter.
    pub generic_kind: GenericKind,

    /// Span where mismatch occurred.
    pub generic_identifier_span: RelativeSpan,

    /// Expected count of generic arguments.
    pub expected_count: usize,

    /// Supplied count of generic arguments.
    pub supplied_count: usize,
}

impl Report for MismatchedGenericArgumentCount {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::new(
                engine.to_absolute_span(&self.generic_identifier_span).await,
                Some(format!(
                    "expected {} {} arguments, but {} were supplied",
                    self.expected_count,
                    match self.generic_kind {
                        GenericKind::Type => "type",
                        GenericKind::Lifetime => "lifetime",
                        GenericKind::Constant => "constant",
                        GenericKind::Instance => "instance",
                    },
                    self.supplied_count,
                )),
            ))
            .message("mismatched generic argument count")
            .build()
    }
}

/// The symbol doesn't require any generic arguments but some were supplied.
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
    Encode,
    Decode,
)]
pub struct NoGenericArgumentsRequired {
    /// The symbol that  was supplied with generic arguments.
    pub global_id: Global<pernixc_symbol::ID>,

    /// The span where the generic arguments were supplied.
    pub generic_argument_span: RelativeSpan,
}

impl Report for NoGenericArgumentsRequired {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let qualified_name = engine.get_qualified_name(self.global_id).await;

        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::new(
                engine.to_absolute_span(&self.generic_argument_span).await,
                Some(format!(
                    "the symbol `{qualified_name}` doesn't require any \
                     generic arguments"
                )),
            ))
            .message(
                "generic arguments was supplied to the symbol that doesn't \
                 have a generic parameter",
            )
            .build()
    }
}

/// The symbol was not found in the given scope.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct SymbolNotFound {
    /// The [`GlobalID`] where the symbol was searched in. If `None`, the root
    /// module was searched.
    pub searched_item_id: Option<Global<pernixc_symbol::ID>>,

    /// The span where the symbol was searched from.
    pub resolution_span: RelativeSpan,

    /// The name that failed to resolved.
    pub name: Interned<str>,
}

#[allow(clippy::cast_precision_loss)]
fn suggest<'a>(
    not_found_name: &str,
    available_names: impl IntoIterator<Item = &'a str>,
) -> Option<&'a str> {
    // Calculate appropriate maximum distance based on input length
    let max_distance = match not_found_name.len() {
        0..=3 => 1,   // Very short: allow only 1 char difference
        4..=6 => 2,   // Short: allow 2 char difference
        7..=10 => 3,  // Medium: allow 3 char difference
        11..=15 => 4, // Long: allow 4 char difference
        _ => not_found_name.len() / 4, // Very long: allow 25% difference
    };

    let mut best_candidate = None;
    let mut best_score = f64::NEG_INFINITY;

    for candidate in available_names {
        let distance = strsim::levenshtein(not_found_name, candidate);

        // Skip if distance is too large
        if distance > max_distance {
            continue;
        }

        // Skip exact matches (shouldn't happen in real usage)
        if distance == 0 {
            continue;
        }

        // Calculate confidence score
        let max_len = not_found_name.len().max(candidate.len()) as f64;
        let base_score = 1.0 - (distance as f64 / max_len);

        // Apply bonus for common typo patterns

        if base_score > best_score {
            best_score = base_score;
            best_candidate = Some(candidate);
        }
    }

    // Only return suggestion if confidence is reasonable
    if best_score > 0.4 { best_candidate } else { None }
}

impl Report for SymbolNotFound {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let searched_item_id_qualified_name = match self.searched_item_id {
            Some(id) => Some(engine.get_qualified_name(id).await),
            None => None,
        };

        let did_you_mean = 'dym: {
            if let Some(item) = self.searched_item_id {
                let Some(members) = engine.try_get_members(item).await else {
                    break 'dym None;
                };

                let kind = engine.get_kind(item).await;

                match kind {
                    Kind::Module => {
                        let imports = engine.get_import_map(item).await;

                        suggest(
                            &self.name,
                            members
                                .member_ids_by_name
                                .keys()
                                .map(|s| &**s)
                                .chain(imports.keys().map(|s| &**s)),
                        )
                        .map(ToString::to_string)
                    }

                    _ => suggest(
                        &self.name,
                        members.member_ids_by_name.keys().map(|s| &**s),
                    )
                    .map(ToString::to_string),
                }
            } else {
                let target_map = engine.get_target_map().await;
                suggest(&self.name, target_map.keys().map(|s| &**s))
                    .map(ToString::to_string)
            }
        };

        let span_message = searched_item_id_qualified_name.map_or_else(
            || format!("the target named `{}` is not found", &*self.name),
            |x| {
                format!(
                    "the symbol named `{}` does not exist in `{x}`",
                    &*self.name,
                )
            },
        );

        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::new(
                engine.to_absolute_span(&self.resolution_span).await,
                Some(span_message),
            ))
            .message("the symbol could not be found")
            .maybe_help_message(
                did_you_mean
                    .as_ref()
                    .map(|suggestion| format!("did you mean `{suggestion}`?")),
            )
            .build()
    }
}

/// The symbol is not accessible from the referring site.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct SymbolIsNotAccessible {
    /// [`Global`] ID where the [`Self::referred`] is referred.
    pub referring_site: Global<pernixc_symbol::ID>,

    /// The symbol that was referred and is not accessible.
    pub referred: Global<pernixc_symbol::ID>,

    /// The span where the [`Self::referred`] is referred from.
    pub referred_span: RelativeSpan,
}

impl Report for SymbolIsNotAccessible {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let referring_site_qualified_name =
            engine.get_qualified_name(self.referring_site).await;
        let referred_qualified_name =
            engine.get_qualified_name(self.referred).await;

        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::new(
                engine.to_absolute_span(&self.referred_span).await,
                Some(format!(
                    "the symbol `{referred_qualified_name}` is not accessible \
                     from `{referring_site_qualified_name}`",
                )),
            ))
            .message("the symbol is not accessible")
            .build()
    }
}

/// Expected a module in the module path, but found other kind of symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct ExpectModule {
    /// The module path that was expected to be a module.
    pub module_path: RelativeSpan,

    /// The ID of the symbol that was found instead of a module.
    pub found_id: Global<pernixc_symbol::ID>,
}

impl Report for ExpectModule {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let found_symbol_qualified_name =
            engine.get_qualified_name(self.found_id).await;

        let kind = engine.get_kind(self.found_id).await;

        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::new(
                engine.to_absolute_span(&self.module_path).await,
                None,
            ))
            .message(format!(
                "expected a module in the module path, but found `{} {}`",
                kind.kind_str(),
                found_symbol_qualified_name
            ))
            .build()
    }
}

/// The higher-ranked lifetime with the same name already exists in the given
/// scope.
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
    Encode,
    Decode,
    Builder,
)]
pub struct ForallLifetimeRedefinition {
    /// The span of the redefinition.
    redefinition_span: RelativeSpan,
}

impl Report for ForallLifetimeRedefinition {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::new(
                engine.to_absolute_span(&self.redefinition_span).await,
                Some(
                    "lifetime with the same name is already defined"
                        .to_string(),
                ),
            ))
            .message("forall lifetime redefinition")
            .build()
    }
}

/// The type doesn't have a member to be accessed.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Builder,
)]
pub struct NoMemberInType {
    resolution_span: RelativeSpan,
    r#type: Type,
}

impl Report for NoMemberInType {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        Rendered::builder()
            .message({
                let mut message =
                    "cannot access to the member of type `".to_string();

                self.r#type.write_async(parameter, &mut message).await.unwrap();

                message.push('`');

                message
            })
            .primary_highlight(
                Highlight::builder()
                    .span(
                        parameter.to_absolute_span(&self.resolution_span).await,
                    )
                    .build(),
            )
            .build()
    }
}

/// The function doesn't have a member to be accessed.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Builder,
)]
pub struct NoMemberInFunction {
    resolution_span: RelativeSpan,
    function_id: Global<pernixc_symbol::ID>,
}

impl Report for NoMemberInFunction {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let function_qualified_name =
            parameter.get_qualified_name(self.function_id).await;

        Rendered::builder()
            .message(format!(
                "cannot access to the member of function \
                 `{function_qualified_name}`"
            ))
            .primary_highlight(
                Highlight::builder()
                    .span(
                        parameter.to_absolute_span(&self.resolution_span).await,
                    )
                    .build(),
            )
            .build()
    }
}

/// The kind of parameter was expected.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    From,
)]
#[allow(missing_docs)]
pub enum ExpectedParameter {
    Type(TypeParameterID),
    Constant(ConstantParameterID),
    Instance(InstanceParameterID),
}

/// Expected type argument at the given position
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Builder,
)]
pub struct MismatchedKindInArgument {
    argument_span: RelativeSpan,
    found_kind: GenericKind,
    found_parameter: ExpectedParameter,
}

impl Report for MismatchedKindInArgument {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        Rendered::builder()
            .message(format!(
                "expected {} argument, but found {} argument",
                match self.found_parameter {
                    ExpectedParameter::Type(_) => "a type",
                    ExpectedParameter::Constant(_) => "a constant",
                    ExpectedParameter::Instance(_) => "an instance",
                },
                match self.found_kind {
                    GenericKind::Type => "a type",
                    GenericKind::Lifetime => "a lifetime",
                    GenericKind::Constant => "a constant",
                    GenericKind::Instance => "an instance",
                }
            ))
            .primary_highlight(
                Highlight::builder()
                    .span(parameter.to_absolute_span(&self.argument_span).await)
                    .message({
                        let generic_parameters = parameter
                            .get_generic_parameters(
                                match self.found_parameter {
                                    ExpectedParameter::Type(member_id) => {
                                        member_id.parent_id()
                                    }
                                    ExpectedParameter::Constant(member_id) => {
                                        member_id.parent_id()
                                    }
                                    ExpectedParameter::Instance(member_id) => {
                                        member_id.parent_id()
                                    }
                                },
                            )
                            .await;

                        let parameter_name = match self.found_parameter {
                            ExpectedParameter::Type(member_id) => {
                                generic_parameters[member_id.id()].name()
                            }
                            ExpectedParameter::Constant(member_id) => {
                                generic_parameters[member_id.id()].name()
                            }
                            ExpectedParameter::Instance(member_id) => {
                                generic_parameters[member_id.id()].name()
                            }
                        };

                        format!(
                            "required {} for the generic parameter `{}`",
                            match self.found_parameter {
                                ExpectedParameter::Type(_) => "a type",
                                ExpectedParameter::Constant(_) => "a constant",
                                ExpectedParameter::Instance(_) => "an instance",
                            },
                            parameter_name.as_ref()
                        )
                    })
                    .build(),
            )
            .build()
    }
}

/// The effect was expected but the non-effect symbol was found.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Builder,
)]
pub struct ExpectEffect {
    /// The span where the non-effect symbol was found.
    non_effect_symbol_span: RelativeSpan,

    /// The resolved symbol ID where the non-effect symbol was found.
    resolved_resolution: Resolution,
}

impl Report for ExpectEffect {
    async fn report(
        &self,
        table: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let found = self.resolved_resolution.found_string(table).await;

        pernixc_diagnostic::Rendered::builder()
            .primary_highlight(Highlight::new(
                table.to_absolute_span(&self.non_effect_symbol_span).await,
                Some(format!("the effect was expected but found `{found}`",)),
            ))
            .message("effect expected")
            .build()
    }
}
