//! Contains the definition of the [`State`].

use getset::CopyGetters;
use pernixc_arena::ID;
use pernixc_lexical::tree::DelimiterKind;

use crate::concrete_tree::AstInfo;

/// Represents the state machine of the parser. The parser will use this state
/// machine to scan the token tree and produce a syntax tree.
#[derive(Debug, Clone, CopyGetters)]
pub struct State<'a> {
    /// The token tree that will be used to scan the source code.
    #[get_copy = "pub"]
    tree: &'a pernixc_lexical::tree::Tree,

    /// The current cursor position in the token tree.
    cursor: Cursor,

    /// List of events that will be used to build the syntax tree at the end of
    /// parsing.
    events: Vec<Event>,

    /// Determines whether the new line character is considered a significant
    /// token.
    new_line_significant: bool,
}

/// Used for representing the position where the parser is in the token tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cursor {
    /// In which branch of the token tree the parser is currently in.
    pub branch_id: ID<pernixc_lexical::tree::Branch>,

    /// The node index in the branch that the [`Cursor::branch_id`] points to.
    pub node_index: usize,
}

/// An enumeration of what kind of fragment that the state machine can step
/// into.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum FragmentKind {
    Indentation,
    Delimited(DelimiterKind),
}

impl<'a> State<'a> {
    /// Creates a new parser state machine that starts at the root of the
    /// token tree and the first node.
    #[must_use]
    pub const fn new(tree: &'a pernixc_lexical::tree::Tree) -> Self {
        Self {
            tree,
            cursor: Cursor {
                branch_id: pernixc_lexical::tree::ROOT_BRANCH_ID,
                node_index: 0,
            },
            events: Vec::new(),
            new_line_significant: true,
        }
    }
}

/// The result of parsing operations. Instead of parsing the tree structure
/// directly in each parsing operation, the state machine will keep track of
/// the events that will be used to build the syntax tree at the end of
/// parsing operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Event {
    /// Start a new node with the given [`AstInfo`] and push it onto the
    /// stack. If [`AstInfo::step_into_fragment`] present, the parser shall
    /// step into the fragment as well.
    NewNode(AstInfo),

    /// Take token to the top-most node in the stack.
    Take(usize),

    /// Pop the top-most node in the stack and creates a new node with the
    /// popped [`AstInfo`]. If the [`AstInfo::step_into_fragment`] present, the
    /// parser shall step into the fragment as well and, if there still tokens
    /// left in the stepping out fragment, the leftover tokens will be grouped
    /// into a new error node and is attached to the popped node.
    FinishNode,
}
