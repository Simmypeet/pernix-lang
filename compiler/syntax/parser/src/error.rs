//! Contains the definition of [`Error`] struct

use pernixc_arena::ID;
use pernixc_diagnostic::{ByteIndex, Highlight, Rendered, Report};
use pernixc_hash::HashSet;
use pernixc_lexical::{
    kind,
    tree::{
        Branch, DelimiterKind, FragmentKind, OffsetMode, ROOT_BRANCH_ID,
        RelativeLocation, RelativeSpan,
    },
};
use pernixc_qbice::TrackedEngine;
use pernixc_source_file::{
    AbsoluteSpan, GlobalSourceID, Span, get_source_file_path,
};
use qbice::{Decode, Encode, Identifiable, StableHash};

use crate::{
    expect::{self, Expected},
    state::Cursor,
};

/// Represents an error of encountering an unexpected token at a certain
/// possition at its possible expected tokens.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Encode,
    Decode,
    StableHash,
    Identifiable,
)]
pub struct Error {
    /// The tokens that are expected at the cursor position.
    pub expecteds: HashSet<Expected>,

    /// The cursor position where the error occurred.
    pub at: Cursor,

    /// The source ID of the token tree where the error occurred.
    pub source_id: GlobalSourceID,
}

fn found_node_string(
    node: &pernixc_lexical::tree::Node,
    token_tree: &pernixc_lexical::tree::Tree,
    source_id: GlobalSourceID,
) -> (String, AbsoluteSpan) {
    match node {
        pernixc_lexical::tree::Node::Leaf(token) => {
            let string = match &token.kind {
                kind::Kind::Keyword(keyword) => {
                    format!("`{keyword}` keyword")
                }
                kind::Kind::NewLine(_) => "new line".to_string(),
                kind::Kind::Character(_) => "character literal".to_string(),
                kind::Kind::String(_) => "string literal".to_string(),
                kind::Kind::Identifier(identifier) => {
                    format!("`{identifier}` identifier")
                }
                kind::Kind::Punctuation(punctuation) => {
                    format!("`{punctuation}` punctuation")
                }
                kind::Kind::Numeric(_) => "numeric literal".to_string(),
            };

            (string, token_tree.absolute_span_of(&token.span))
        }

        pernixc_lexical::tree::Node::Branch(id) => {
            let fragment = token_tree[*id].kind.as_fragment().unwrap();

            let string = match &fragment.fragment_kind {
                FragmentKind::Delimiter(delimiter) => {
                    match delimiter.delimiter {
                        DelimiterKind::Parenthesis => {
                            "`( ... )` block".to_string()
                        }
                        DelimiterKind::Brace => "`{ ... }` block".to_string(),
                        DelimiterKind::Bracket => "`[ ... ]` block".to_string(),
                    }
                }
                FragmentKind::Indentation(_) => "indentation block".to_string(),
            };

            (
                string,
                token_tree.absolute_span_of(&Span {
                    start: RelativeLocation {
                        offset: 0,
                        mode: OffsetMode::Start,
                        relative_to: *id,
                    },
                    end: RelativeLocation {
                        offset: 0,
                        mode: OffsetMode::End,
                        relative_to: *id,
                    },
                    source_id,
                }),
            )
        }
    }
}

impl Cursor {
    /// Retrieves the relative span of the cursor in the given token tree.
    #[must_use]
    pub fn to_relative_span(
        &self,
        token_tree: &pernixc_lexical::tree::Tree,
    ) -> Span<RelativeLocation> {
        let branch = &token_tree[self.branch_id];
        let is_at_end = self.node_index == branch.nodes.len();
        let is_at_root = self.branch_id == ROOT_BRANCH_ID;

        match (is_at_end, is_at_root) {
            (false, _) => match &branch.nodes[self.node_index] {
                pernixc_lexical::tree::Node::Leaf(token) => token.span,
                pernixc_lexical::tree::Node::Branch(id) => RelativeSpan {
                    start: RelativeLocation {
                        offset: 0,
                        mode: OffsetMode::Start,
                        relative_to: *id,
                    },
                    end: RelativeLocation {
                        offset: 0,
                        mode: OffsetMode::End,
                        relative_to: *id,
                    },
                    source_id: token_tree.source_id(),
                },
            },

            (true, false) => {
                match &branch.kind.as_fragment().unwrap().fragment_kind {
                    FragmentKind::Delimiter(delimiter) => delimiter.close.span,
                    FragmentKind::Indentation(_) => {
                        let mut branch_id: ID<Branch> = self.branch_id;
                        let mut branch: &Branch = branch;
                        let mut parent_branch: &Branch;

                        loop {
                            let Some(parent_branch_id) = branch.parent() else {
                                break Span {
                                    start: RelativeLocation {
                                        offset: 0,
                                        mode: OffsetMode::End,
                                        relative_to: ROOT_BRANCH_ID,
                                    },
                                    end: RelativeLocation {
                                        offset: 0,
                                        mode: OffsetMode::End,
                                        relative_to: ROOT_BRANCH_ID,
                                    },
                                    source_id: token_tree.source_id(),
                                };
                            };

                            parent_branch = &token_tree[parent_branch_id];

                            let node_index = parent_branch
                                .nodes
                                .iter()
                                .position(|x| {
                                    x.as_branch()
                                        .is_some_and(|x| *x == branch_id)
                                })
                                .unwrap();

                            let node = branch.nodes.get(node_index);

                            if let Some(node) = node {
                                break match node {
                                    pernixc_lexical::tree::Node::Leaf(
                                        token,
                                    ) => token.span,
                                    pernixc_lexical::tree::Node::Branch(id) => {
                                        RelativeSpan {
                                            start: RelativeLocation {
                                                offset: 0,
                                                mode: OffsetMode::Start,
                                                relative_to: *id,
                                            },
                                            end: RelativeLocation {
                                                offset: 0,
                                                mode: OffsetMode::End,
                                                relative_to: *id,
                                            },
                                            source_id: token_tree.source_id(),
                                        }
                                    }
                                };
                            }

                            branch_id = parent_branch_id;
                            branch = parent_branch;
                        }
                    }
                }
            }

            (true, true) => Span {
                start: RelativeLocation {
                    offset: 0,
                    mode: OffsetMode::End,
                    relative_to: ROOT_BRANCH_ID,
                },
                end: RelativeLocation {
                    offset: 0,
                    mode: OffsetMode::End,
                    relative_to: ROOT_BRANCH_ID,
                },
                source_id: token_tree.source_id(),
            },
        }
    }
}

#[allow(clippy::trivially_copy_pass_by_ref)]
fn expected_string(expected: &Expected) -> String {
    match expected {
        Expected::Identifier(_) => "identifier".to_string(),
        Expected::IdentifierValue(identifier_value) => {
            format!("`{} identifier`, ", identifier_value.expected_string())
        }
        Expected::String(_) => "string literal".to_string(),
        Expected::Character(_) => "character literal".to_string(),
        Expected::Numeric(_) => "numeric literal".to_string(),
        Expected::Punctuation(a) => {
            format!("`{a}` punctuation")
        }
        Expected::NewLine(_) => "new line".to_string(),
        Expected::Keyword(keyword) => format!("`{keyword}` keyword"),
        Expected::Fragment(fragment) => match fragment {
            expect::Fragment::Indentation => "indentation block".to_string(),
            expect::Fragment::Delimited(delimiter_kind) => match delimiter_kind
            {
                DelimiterKind::Parenthesis => "`( ... )` block".to_string(),
                DelimiterKind::Brace => "`{ ... }` block".to_string(),
                DelimiterKind::Bracket => "`[ ... ]` block".to_string(),
            },
        },
    }
}

async fn found_string(
    engine: &TrackedEngine,
    source_id: GlobalSourceID,
    at: &Cursor,
) -> (String, AbsoluteSpan) {
    let path = engine.get_source_file_path(source_id).await;

    let (token_tree, _) = engine
        .query(&pernixc_lexical::Key { path, target_id: source_id.target_id })
        .await
        .expect("should have loaded the source file since the parser did");

    let branch = &token_tree[at.branch_id];
    let is_at_end = at.node_index == branch.nodes.len();
    let is_at_root = at.branch_id == ROOT_BRANCH_ID;

    match (is_at_end, is_at_root) {
        (true, true) => (
            "End of file".to_string(),
            token_tree.absolute_span_of(&Span {
                start: RelativeLocation {
                    offset: 0,
                    mode: OffsetMode::End,
                    relative_to: ROOT_BRANCH_ID,
                },
                end: RelativeLocation {
                    offset: 0,
                    mode: OffsetMode::End,
                    relative_to: ROOT_BRANCH_ID,
                },
                source_id,
            }),
        ),

        (true, false) => {
            match &branch.kind.as_fragment().unwrap().fragment_kind {
                FragmentKind::Delimiter(delimiter) => {
                    let string = format!("`{}` punctuation", match delimiter
                        .delimiter
                    {
                        DelimiterKind::Parenthesis => ')',
                        DelimiterKind::Brace => '}',
                        DelimiterKind::Bracket => ']',
                    });

                    (string, token_tree.absolute_span_of(&delimiter.close.span))
                }
                FragmentKind::Indentation(_) => {
                    let mut branch_id: ID<Branch> = at.branch_id;
                    let mut branch: &Branch = branch;
                    let mut parent_branch: &Branch;

                    loop {
                        let Some(parent_branch_id) = branch.parent() else {
                            break (
                                "End of file".to_string(),
                                token_tree.absolute_span_of(&Span {
                                    start: RelativeLocation {
                                        offset: 0,
                                        mode: OffsetMode::End,
                                        relative_to: ROOT_BRANCH_ID,
                                    },
                                    end: RelativeLocation {
                                        offset: 0,
                                        mode: OffsetMode::End,
                                        relative_to: ROOT_BRANCH_ID,
                                    },
                                    source_id,
                                }),
                            );
                        };

                        parent_branch = &token_tree[parent_branch_id];

                        let node_index = parent_branch
                            .nodes
                            .iter()
                            .position(|x| {
                                x.as_branch().is_some_and(|x| *x == branch_id)
                            })
                            .unwrap();

                        let node = branch.nodes.get(node_index);

                        if let Some(node) = node {
                            break found_node_string(
                                node,
                                &token_tree,
                                source_id,
                            );
                        }

                        branch_id = parent_branch_id;
                        branch = parent_branch;
                    }
                }
            }
        }

        (false, _) => found_node_string(
            &branch.nodes[at.node_index],
            &token_tree,
            source_id,
        ),
    }
}

impl Report for Error {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Rendered<ByteIndex> {
        let expected_string = self
            .expecteds
            .iter()
            .map(expected_string)
            .collect::<Vec<_>>()
            .join(", ");

        let (found_string, found_span) =
            found_string(engine, self.source_id, &self.at).await;

        let message =
            format!("unexpected {found_string}, expected {expected_string}");

        Rendered {
            primary_highlight: Some(Highlight::new(found_span, None)),
            message,
            severity: pernixc_diagnostic::Severity::Error,
            help_message: None,
            related: Vec::default(),
        }
    }
}
