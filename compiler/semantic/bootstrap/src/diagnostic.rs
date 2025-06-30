//! Contains all the diagnostics related creating a new compilation target.

use std::{any::Any, fmt::Debug};

use flexstr::SharedStr;
use pernixc_diagnostic::{Report, Severity};
use pernixc_lexical::tree::{RelativeLocation, RelativeSpan};
use pernixc_query::Engine;
use pernixc_target::Global;

use crate::{name::Ext as _, span::Ext as _, symbol};

/// Implemented all the diagnostic objects and provides type erasure.
pub trait Diagnostic:
    for<'a> Report<&'a Engine, Location = RelativeLocation>
    + Debug
    + Send
    + Sync
    + 'static
{
    #[allow(missing_docs)]
    fn as_any(&self) -> &dyn Any;

    #[allow(missing_docs)]
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl<
        U: for<'a> Report<&'a Engine, Location = RelativeLocation>
            + Debug
            + Any
            + Send
            + Sync
            + 'static,
    > Diagnostic for U
{
    fn as_any(&self) -> &dyn Any { self }

    fn as_any_mut(&mut self) -> &mut dyn Any { self }
}

impl<
        U: for<'a> Report<&'a Engine, Location = RelativeLocation>
            + Debug
            + Any
            + Send
            + Sync
            + 'static,
    > From<U> for Box<dyn Diagnostic>
{
    fn from(value: U) -> Self { Box::new(value) }
}

/// The item symbol with the same name already exists in the given scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemRedifinition {
    /// The ID of the existing symbol.
    pub existing_id: Global<symbol::ID>,

    /// The ID of the new symbol.
    pub new_id: Global<symbol::ID>,

    /// The scope in which the duplication occurred.
    pub in_id: Global<symbol::ID>,
}

impl Report<&Engine> for ItemRedifinition {
    type Location = RelativeLocation;

    fn report(
        &self,
        engine: &Engine,
    ) -> pernixc_diagnostic::Diagnostic<RelativeLocation> {
        let existing_symbol_span = engine.get_span(self.existing_id);
        let new_symbol_span = engine.get_span(self.new_id);
        let existing_symbol_name = engine.get_name(self.existing_id);
        let in_name = engine.get_qualified_name(self.in_id);

        pernixc_diagnostic::Diagnostic {
            span: new_symbol_span
                .map(|x| (x, Some("redefinition here".to_string()))),

            message: format!(
                "symbol `{}` is already defined in the scope `{}`",
                *existing_symbol_name, in_name
            ),
            severity: pernixc_diagnostic::Severity::Error,
            help_message: None,
            related: existing_symbol_span
                .map(|span| pernixc_diagnostic::Related {
                    span,
                    message: format!(
                        "symbol `{}` is already defined here",
                        *existing_symbol_name
                    ),
                })
                .into_iter()
                .collect(),
        }
    }
}

/// The symbol was not found in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolNotFound {
    /// The [`GlobalID`] where the symbol was searched in. If `None`, the root
    /// module was searched.
    pub searched_item_id: Option<Global<symbol::ID>>,

    /// The span where the symbol was searched from.
    pub resolution_span: RelativeSpan,

    /// The name that failed to resolved.
    pub name: SharedStr,
}

impl Report<&Engine> for SymbolNotFound {
    type Location = RelativeLocation;

    fn report(
        &self,
        table: &Engine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let searched_item_id_qualified_name =
            self.searched_item_id.map(|x| table.get_qualified_name(x));

        let span_message = searched_item_id_qualified_name.map_or_else(
            || {
                format!(
                    "the target named `{}` is not found",
                    self.name.as_str()
                )
            },
            |x| {
                format!(
                    "the symbol named `{}` does not exist in `{x}`",
                    self.name.as_str(),
                )
            },
        );

        pernixc_diagnostic::Diagnostic {
            span: Some((self.resolution_span, Some(span_message))),
            message: "the symbol could not be found".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The symbol is not accessible from the referring site.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolIsNotAccessible {
    /// [`Global`] ID where the [`Self::referred`] is referred.
    pub referring_site: Global<symbol::ID>,

    /// The symbol that was referred and is not accessible.
    pub referred: Global<symbol::ID>,

    /// The span where the [`Self::referred`] is referred from.
    pub referred_span: RelativeSpan,
}

impl Report<&Engine> for SymbolIsNotAccessible {
    type Location = RelativeLocation;

    fn report(
        &self,
        table: &Engine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let referring_site_qualified_name =
            table.get_qualified_name(self.referring_site);
        let referred_qualified_name = table.get_qualified_name(self.referred);

        pernixc_diagnostic::Diagnostic {
            span: Some((
                self.referred_span,
                Some(format!(
                    "the symbol `{referred_qualified_name}` is not accessible \
                     from `{referring_site_qualified_name}`",
                )),
            )),
            message: "the symbol is not accessible".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}
