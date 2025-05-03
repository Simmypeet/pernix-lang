use std::{any::TypeId, collections::HashMap};

use getset::CopyGetters;
use pernixc_arena::ID;
use pernixc_lexical::tree::DelimiterKind;

/// Represents the state machine of the parser. The parser will use this state
/// machine to scan the token tree and produce a syntax tree.
#[derive(Debug, Clone, CopyGetters)]
pub struct State<'a> {
    /// The token tree that will be used to scan the source code.
    #[get_copy = "pub"]
    tree: &'a pernixc_lexical::tree::Tree,

    cursor: Cursor,
    events: Vec<Event>,

    /// Determines whether the new line character is considered a significant
    /// token.
    new_line_significant: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FragmentKind {
    Indentation,
    Delimited(DelimiterKind),
}

impl<'a> State<'a> {
    /// Creates a new parser state machine that starts at the root of the
    /// token tree and the first node.
    #[must_use]
    pub fn new(tree: &'a pernixc_lexical::tree::Tree) -> Self {
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

    pub fn start_node(
        &mut self,
        ast_node_id: Option<TypeId>,
        inner: impl FnOnce(&mut Self),
    ) {
        self.events.push(Event::NewNode(ast_node_id));
        inner(self);
        self.events.push(Event::FinishNode);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cursor {
    pub branch_id: ID<pernixc_lexical::tree::Branch>,
    pub node_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Event {
    NewNode(Option<TypeId>),
    Take(usize),
    FinishNode,
    StepIntoFragment,
    StepOutFragment,
}
