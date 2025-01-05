//! Contains the resolution logic for the table.

use diagnostic::{SymbolIsNotAccessible, SymbolNotFound};
use pernixc_base::handler::Handler;
use pernixc_lexical::token::Identifier;
use pernixc_syntax::syntax_tree::{SimplePath, SimplePathRoot};

use super::{GlobalID, Representation, ID};
use crate::{
    component::{Import, Member},
    diagnostic::Diagnostic,
};

pub mod diagnostic;

/// An error returned by various `resolve_*` methods.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum ResolveQualifiedIdentifierError {
    #[error("the given `referring_site` id does not exist in the table")]
    InvalidReferringSiteID,

    #[error("encountered a fatal semantic error that aborts the process")]
    SemanticError,

    #[error(
        "the resolution observer requested to abort the resolution process"
    )]
    Abort,
}

/// An error returned by [`Representation::resolve_sequence`].
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum ResolveSequenceError {
    #[error("the given 'simple_path' iterator was empty")]
    EmptyIterator,

    #[error(transparent)]
    ResolutionError(#[from] ResolveQualifiedIdentifierError),
}
impl Representation {
    /// Resolves a [`SimplePath`] as a [`ItemID`].
    pub fn resolve_simple_path(
        &self,
        simple_path: &SimplePath,
        referring_site: GlobalID,
        start_from_root: bool,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<GlobalID, ResolveQualifiedIdentifierError> {
        use ResolveQualifiedIdentifierError::*;

        let root: GlobalID = match simple_path.root() {
            SimplePathRoot::Target(_) => {
                GlobalID::new(referring_site.target_id, ID::ROOT_MODULE)
            }
            SimplePathRoot::Identifier(ident) => {
                if start_from_root {
                    let Some(id) =
                        self.targets_by_name.get(ident.span.str()).copied()
                    else {
                        handler.receive(Box::new(SymbolNotFound {
                            searched_item_id: None,
                            resolution_span: ident.span.clone(),
                        }));

                        return Err(SemanticError);
                    };

                    GlobalID::new(id, ID::ROOT_MODULE)
                } else {
                    let closet_module_id = self
                        .get_closet_module_id(referring_site)
                        .ok_or(InvalidReferringSiteID)?;

                    let global_closest_module_id = GlobalID::new(
                        referring_site.target_id,
                        closet_module_id,
                    );

                    let Some(id) = self
                        .get::<Member>(global_closest_module_id)
                        .unwrap()
                        .get(ident.span.str())
                        .map(|x| {
                            GlobalID::new(referring_site.target_id, (*x).into())
                        })
                        .or_else(|| {
                            self.get::<Import>(
                                global_closest_module_id,
                            )
                            .unwrap()
                            .get(ident.span.str())
                            .map(|x| x.id)
                        })
                    else {
                        handler.receive(Box::new(SymbolNotFound {
                            searched_item_id: Some(global_closest_module_id),
                            resolution_span: ident.span.clone(),
                        }));

                        return Err(SemanticError);
                    };

                    id
                }
            }
        };

        match self.resolve_sequence(
            simple_path.rest().iter().map(|x| &x.1),
            referring_site,
            root,
            handler,
        ) {
            Ok(id) => Ok(id),
            Err(ResolveSequenceError::EmptyIterator) => Ok(root),
            Err(ResolveSequenceError::ResolutionError(e)) => Err(e),
        }
    }

    /// Resolves a sequence of identifier starting of from the given `root`.
    pub fn resolve_sequence<'a>(
        &self,
        simple_path: impl Iterator<Item = &'a Identifier>,
        referring_site: GlobalID,
        root: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<GlobalID, ResolveSequenceError> {
        use ResolveSequenceError::*;

        let mut lastest_resolution = root;
        for identifier in simple_path {
            let new_id = self
                .get_member_of(lastest_resolution, identifier.span.str())
                .map_err(|_| {
                    handler.receive(Box::new(SymbolNotFound {
                        searched_item_id: Some(lastest_resolution),
                        resolution_span: identifier.span.clone(),
                    }));

                    ResolutionError(
                        ResolveQualifiedIdentifierError::SemanticError,
                    )
                })?;

            // non-fatal error, no need to return early
            if !self.symbol_accessible(referring_site, new_id).ok_or(
                ResolutionError(
                    ResolveQualifiedIdentifierError::InvalidReferringSiteID,
                ),
            )? {
                handler.receive(Box::new(SymbolIsNotAccessible {
                    referring_site,
                    referred: new_id,
                    referred_span: identifier.span.clone(),
                }));
            }

            lastest_resolution = new_id;
        }

        Ok(lastest_resolution)
    }
}
