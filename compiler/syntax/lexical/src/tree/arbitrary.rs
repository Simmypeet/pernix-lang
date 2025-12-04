#![allow(missing_docs)]

use std::fmt::{Display, Formatter};

use derive_more::{Deref, DerefMut};
use enum_as_inner::EnumAsInner;
use pernixc_arena::ID;
use pernixc_source_file::SourceMap;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy, TestCaseError},
    prop_assert, prop_assert_eq, prop_oneof,
};

use super::{DelimiterKind, Tree};
use crate::{
    kind,
    token::{self, arbitrary::PriorInsignificant},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Indentation {
    pub size: usize,
    pub block: IndentationBlock,
    pub colon_insignificant: Option<PriorInsignificant>,
}

impl Input<ID<super::Branch>, (usize, &SourceMap, &Tree)> for &Indentation {
    fn assert(
        self,
        branch_id: ID<super::Branch>,
        (prev_size, source_map, tree): (usize, &SourceMap, &Tree),
    ) -> proptest::test_runner::TestCaseResult {
        let branch = &tree[branch_id];
        prop_assert!(
            branch
                .kind
                .as_fragment()
                .is_some_and(|x| x.fragment_kind.is_indentation())
        );

        let mut i = 0;

        for (line_no, token) in self.block.lines.iter().enumerate() {
            match token {
                IndentationLine::Normal(token_stream, end_with_indentation) => {
                    if self.size > 0 {
                        let current = &branch.nodes[i];
                        let whitespace_span = match current {
                            super::Node::Leaf(leaf) => {
                                leaf.prior_insignificant.as_ref()
                            }
                            super::Node::Branch(fragment) => tree
                                .first_fragment_token_insignificant_span(
                                    *fragment,
                                ),
                        };

                        let indent_size = whitespace_span.map_or(0, |x| {
                            let source_file =
                                &source_map.get(x.source_id).unwrap();
                            let abs_span =
                                x.to_absolute_span(source_file, tree);

                            let source =
                                &source_file.content()[abs_span.range()];

                            source
                                .chars()
                                .map(|x| if x == '\t' { self.size } else { 1 })
                                .sum()
                        });

                        prop_assert_eq!(indent_size, self.size + prev_size);
                    }

                    for token in token_stream.iter() {
                        let current = &branch.nodes[i];
                        token.assert(current, (source_map, tree))?;

                        i += 1;
                    }

                    if let Some(indentation) = end_with_indentation {
                        let current = &branch.nodes[i];
                        let fragment =
                            current.as_branch().ok_or(TestCaseError::fail(
                                "expected fragment for \
                                 `IndentationLine::Normal`",
                            ))?;

                        indentation.assert(
                            *fragment,
                            (prev_size + self.size, source_map, tree),
                        )?;

                        i += 1;
                    }
                }

                IndentationLine::WhiteSpaces(whitespace_len) => {
                    let current = &branch.nodes[i];
                    let whitespace_span = match current {
                        super::Node::Leaf(leaf) => {
                            leaf.prior_insignificant.as_ref()
                        }
                        super::Node::Branch(fragment) => tree
                            .first_fragment_token_insignificant_span(*fragment),
                    };

                    let indent_size = whitespace_span.map_or(0, |x| {
                        let source_file = &source_map.get(x.source_id).unwrap();
                        let abs_span = x.to_absolute_span(source_file, tree);

                        let source = &source_file.content()[abs_span.range()];

                        source
                            .chars()
                            .map(|x| if x == '\t' { self.size } else { 1 })
                            .sum()
                    });

                    prop_assert_eq!(indent_size, *whitespace_len);
                }
            }

            if line_no != self.block.lines.len() - 1 {
                prop_assert!(
                    &branch.nodes[i]
                        .as_leaf()
                        .and_then(|x| x.kind.as_new_line())
                        .is_some()
                );

                i += 1;
            }
        }

        prop_assert_eq!(i, branch.nodes.len());

        Ok(())
    }
}

impl Arbitrary for Indentation {
    type Parameters = Option<BoxedStrategy<Nodes>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let indentation_leaf = Just(Self {
            size: 4,
            block: IndentationBlock {
                lines: vec![IndentationLine::Normal(
                    Nodes(vec![Node::Leaf(token::arbitrary::Token {
                        kind: kind::arbitrary::Kind::Keyword(
                            kind::arbitrary::Keyword::Pass,
                        ),
                        prior_insignificant: None,
                    })]),
                    None,
                )],
            },
            colon_insignificant: None,
        });

        indentation_leaf
            .prop_recursive(6, 120, 20, move |inner| {
                (
                    2usize..=16usize,
                    IndentationBlock::arbitrary_with((
                        args.clone(),
                        Some(inner),
                    )),
                    proptest::option::of(PriorInsignificant::arbitrary()),
                )
                    .prop_map(|(x, y, z)| Self {
                        size: x,
                        block: y,
                        colon_insignificant: z,
                    })
            })
            .boxed()
    }
}

impl Indentation {
    fn fmt_internal(
        &self,
        f: &mut Formatter,
        prev_indentation_level: usize,
    ) -> std::fmt::Result {
        if let Some(c) = self.colon_insignificant.as_ref() {
            write!(f, "{c}")?;
        }
        writeln!(f, ":")?;
        self.block.fmt_internal(f, prev_indentation_level + self.size)
    }
}

impl IndentationBlock {
    fn fmt_internal(
        &self,
        f: &mut Formatter,
        indentation_size: usize,
    ) -> std::fmt::Result {
        for (line_no, line) in self.lines.iter().enumerate() {
            match line {
                IndentationLine::Normal(nodes, end_with_indentation) => {
                    nodes.fmt_internal(f, Some(indentation_size))?;

                    if let Some(indentation) = end_with_indentation {
                        indentation.fmt_internal(f, indentation_size)?;
                    }
                }

                IndentationLine::WhiteSpaces(size) => {
                    for _ in 0..*size {
                        write!(f, " ")?;
                    }
                }
            }

            if line_no != self.lines.len() - 1 {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}

impl Display for IndentationBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.fmt_internal(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IndentationBlock {
    pub lines: Vec<IndentationLine>,
}

impl Arbitrary for IndentationBlock {
    type Parameters =
        (Option<BoxedStrategy<Nodes>>, Option<BoxedStrategy<Indentation>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(IndentationLine::arbitrary_with(args), 1..20)
            .prop_filter("must at least have a token", |x| {
                x.iter().any(|x| {
                    x.as_normal().is_some_and(|x| {
                        x.0.iter().any(|x| {
                            !x.as_leaf().is_some_and(|x| x.kind.is_new_line())
                        })
                    })
                })
            })
            .prop_filter("last line should not be empty", |x| {
                x.last().is_some_and(|x| {
                    !x.is_white_spaces()
                        && x.as_normal()
                            .is_some_and(|x| !x.0.is_empty() || x.1.is_some())
                })
            })
            .prop_map(|x| Self { lines: x })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum IndentationLine {
    Normal(Nodes, Option<Indentation>),
    WhiteSpaces(usize),
}

impl Arbitrary for IndentationLine {
    type Parameters =
        (Option<BoxedStrategy<Nodes>>, Option<BoxedStrategy<Indentation>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(
        (token_stream, indentation): Self::Parameters,
    ) -> Self::Strategy {
        let indentation = indentation.unwrap_or_else(|| {
            Indentation::arbitrary_with(token_stream.clone())
        });

        let token_stream = token_stream.unwrap_or_else(Nodes::arbitrary);

        prop_oneof![
            6 => (token_stream
                    .prop_map(|mut x| {
                        x.sanitize(true);
                        x
                    }),
                    proptest::option::weighted(0.5, indentation)
                )
                .prop_filter("filter out empty line", |(x, y)| !x.0.is_empty() || !y.is_none())
                .prop_filter("remove possible scope sperator after colon",|(x, y)| {
                    !(x.0
                        .last()
                        .is_some_and(|x|
                            x.as_leaf()
                            .is_some_and(|x|
                                x.kind
                                    .as_punctuation()
                                    .is_some_and(|x| **x == ':'
                            )))
                    && y.is_some())
                })
                .prop_map(|(x, y)| Self::Normal(x, y)),
            1 => (1usize..=10usize).prop_map(Self::WhiteSpaces),
        ]
        .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Node {
    Leaf(token::arbitrary::Token<kind::arbitrary::Kind>),
    Fragment(Fragment),
}

impl Input<&super::Node, (&SourceMap, &Tree)> for &Node {
    fn assert(
        self,
        output: &super::Node,
        (source_map, tree): (&SourceMap, &Tree),
    ) -> proptest::test_runner::TestCaseResult {
        match (self, output) {
            (Node::Leaf(a), super::Node::Leaf(b)) => {
                a.assert(b, (source_map, tree))
            }
            (Node::Fragment(a), super::Node::Branch(b)) => {
                a.assert(*b, (source_map, tree))
            }
            (a, b) => Err(TestCaseError::fail(format!("{a:?} != {b:?}"))),
        }
    }
}

impl Arbitrary for Node {
    type Parameters = Option<BoxedStrategy<Nodes>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            4 => token::arbitrary::Token::arbitrary().prop_map(Node::Leaf),
            1 => Fragment::arbitrary_with(args).prop_map(Node::Fragment),
        ]
        .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Fragment {
    Delimited(Delimited),
    Indentation(Indentation),
}

impl Input<ID<super::Branch>, (&SourceMap, &Tree)> for &Fragment {
    fn assert(
        self,
        output: ID<super::Branch>,
        (source_map, tree): (&SourceMap, &Tree),
    ) -> proptest::test_runner::TestCaseResult {
        match self {
            Fragment::Delimited(a) => a.assert(output, (source_map, tree)),
            Fragment::Indentation(a) => a.assert(output, (0, source_map, tree)),
        }
    }
}

impl Arbitrary for Fragment {
    type Parameters = Option<BoxedStrategy<Nodes>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Delimited::arbitrary_with(args.clone())
                .prop_map(Fragment::Delimited),
            Indentation::arbitrary_with(args).prop_map(Fragment::Indentation),
        ]
        .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Delimited {
    pub delimiter: DelimiterKind,
    pub open_insignificant: Option<PriorInsignificant>,
    pub close_insignificant: Option<PriorInsignificant>,
    pub nodes: Nodes,
}

impl Input<ID<super::Branch>, (&SourceMap, &Tree)> for &Delimited {
    fn assert(
        self,
        output: ID<super::Branch>,
        (source_map, tree): (&SourceMap, &Tree),
    ) -> proptest::test_runner::TestCaseResult {
        let branch = &tree[output];
        let delimiter = branch
            .kind
            .as_fragment()
            .and_then(|x| x.fragment_kind.as_delimiter())
            .ok_or(TestCaseError::fail(
                "expected `Delimited` for `Fragment`",
            ))?;

        self.open_insignificant.as_ref().assert(
            delimiter.open.prior_insignificant.as_ref(),
            (source_map, tree),
        )?;

        self.close_insignificant.as_ref().assert(
            delimiter.close.prior_insignificant.as_ref(),
            (source_map, tree),
        )?;

        prop_assert_eq!(self.delimiter, delimiter.delimiter);

        self.nodes.assert(&branch.nodes, (source_map, tree))
    }
}

impl Arbitrary for Delimited {
    type Parameters = Option<BoxedStrategy<Nodes>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let delimiter_kind = prop_oneof![
            Just(DelimiterKind::Parenthesis),
            Just(DelimiterKind::Brace),
            Just(DelimiterKind::Bracket),
        ];

        (
            delimiter_kind,
            proptest::option::of(PriorInsignificant::arbitrary()),
            proptest::option::of(PriorInsignificant::arbitrary()),
            args.unwrap_or_else(Nodes::arbitrary).prop_map(|mut x| {
                x.sanitize(false);
                x
            }),
        )
            .prop_map(
                |(
                    delimiter,
                    open_insignificant,
                    close_insignificant,
                    nodes,
                )| Self {
                    delimiter,
                    open_insignificant,
                    close_insignificant,
                    nodes,
                },
            )
            .boxed()
    }
}

impl Display for Delimited {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let (open, close) = match self.delimiter {
            DelimiterKind::Parenthesis => ('(', ')'),
            DelimiterKind::Brace => ('{', '}'),
            DelimiterKind::Bracket => ('[', ']'),
        };

        if let Some(o) = self.open_insignificant.as_ref() {
            write!(f, "{o}")?;
        }
        write!(f, "{open}")?;
        self.nodes.fmt_internal(f, None)?;
        if let Some(o) = self.close_insignificant.as_ref() {
            write!(f, "{o}")?;
        }
        write!(f, "{close}")?;

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IsInIndentation(pub bool);

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, DerefMut,
)]
pub struct Nodes(pub Vec<Node>);

impl Nodes {
    fn fmt_internal(
        &self,
        f: &mut Formatter,
        indentation_level: Option<usize>,
    ) -> std::fmt::Result {
        for token in &**self {
            match token {
                Node::Leaf(token) => write!(f, "{token}")?,

                Node::Fragment(delimited) => match delimited {
                    Fragment::Delimited(delimited) => {
                        Display::fmt(delimited, f)?;
                    }
                    Fragment::Indentation(indentation) => {
                        indentation
                            .fmt_internal(f, indentation_level.unwrap_or(0))?;
                    }
                },
            }
        }

        Ok(())
    }
}

impl Display for Nodes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.fmt_internal(f, None)
    }
}

impl Arbitrary for Nodes {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = proptest::collection::vec(
            token::arbitrary::Token::arbitrary().prop_map(Node::Leaf),
            0..10,
        )
        .prop_map(Self);

        leaf.prop_recursive(6, 120, 20, |inner| {
            proptest::collection::vec(Node::arbitrary_with(Some(inner)), 0..=16)
                .prop_map(Self)
        })
        .boxed()
    }
}

fn get_indentation_level_from_prior_insignificant(
    insignificant: Option<&PriorInsignificant>,
) -> usize {
    insignificant.map_or(0, |x| match x {
        PriorInsignificant::Tab(tab) => *tab * 4,
        PriorInsignificant::Whitespace(count) => *count,
    })
}

fn get_indentation_level(tokens: &[Node]) -> (usize, Option<usize>) {
    for (idx, token) in tokens.iter().enumerate() {
        match token {
            Node::Leaf(token) => {
                if token.kind.is_new_line() {
                    continue;
                }

                return (
                    idx,
                    Some(get_indentation_level_from_prior_insignificant(
                        token.prior_insignificant.as_ref(),
                    )),
                );
            }

            Node::Fragment(Fragment::Delimited(a)) => {
                return (
                    idx,
                    Some(get_indentation_level_from_prior_insignificant(
                        a.open_insignificant.as_ref(),
                    )),
                );
            }

            Node::Fragment(Fragment::Indentation(a)) => {
                return (
                    idx,
                    Some(get_indentation_level_from_prior_insignificant(
                        a.colon_insignificant.as_ref(),
                    )),
                );
            }
        }
    }

    (tokens.len(), None)
}

impl Nodes {
    pub fn sanitize(&mut self, is_in_indent: bool) {
        let mut changed = true;
        while changed {
            changed = false;

            changed |= self.remove_delimiter_punctuations();
            changed |= self.remove_possible_indent(is_in_indent);
            if is_in_indent {
                changed |= self.sanitize_in_indent_block();
            }
            changed |= self.remove_consecutive_identifiers();
            if !is_in_indent {
                changed |= self.add_new_line_after_indentation();
            }
        }
    }

    pub fn add_new_line_after_indentation(&mut self) -> bool {
        let mut i = 0;
        let mut changed = false;

        while i < self.len() {
            if !matches!(&self[i], Node::Fragment(Fragment::Indentation(_))) {
                i += 1;
                continue;
            }

            let Some(next) = self.get(i + 1) else {
                break;
            };

            if next.as_leaf().is_some_and(|x| x.kind.is_new_line()) {
                i += 1;
            } else {
                changed = true;
                self.insert(
                    i + 1,
                    Node::Leaf(token::arbitrary::Token {
                        kind: kind::arbitrary::Kind::NewLine(
                            kind::arbitrary::NewLine::LF,
                        ),
                        prior_insignificant: None,
                    }),
                );
                i += 2;
            }
        }

        changed
    }

    pub fn remove_consecutive_identifiers(&mut self) -> bool {
        let mut changed = false;
        let mut i = 0;

        while i < self.len() {
            let current = &self[i];
            let Some(next) = self.get(i + 1) else {
                break;
            };

            if current.as_leaf().is_some_and(|x| {
                x.kind.is_keyword()
                    || x.kind.is_identifier()
                    || x.kind.is_numeric()
            }) && next.as_leaf().is_some_and(|x| {
                (x.kind.is_keyword()
                    || x.kind.is_identifier()
                    || x.kind.is_numeric())
                    && x.prior_insignificant.is_none()
            }) {
                changed = true;
                self.remove(i + 1);
            } else {
                i += 1;
            }
        }

        changed
    }

    pub fn remove_delimiter_punctuations(&mut self) -> bool {
        let starting_len = self.len();
        self.retain(|x| {
            !x.as_leaf().is_some_and(|x| {
                x.kind.as_punctuation().is_some_and(|x| {
                    ['{', '}', '[', ']', '(', ')'].contains(&x.0)
                })
            })
        });

        starting_len != self.len()
    }

    #[allow(clippy::too_many_lines)]
    pub fn remove_possible_indent(&mut self, is_in_indent: bool) -> bool {
        enum NewLineSearch {
            FoundSignificant,
            EndOfTokenStream,
            Found(usize),
        }

        let mut changed = false;

        // remove colon followed by new line
        let mut i = 0;

        while i < self.len() {
            let current = &self[i];

            if !matches!(
                current,
                Node::Leaf(token::arbitrary::Token {
                    kind: kind::arbitrary::Kind::Punctuation(
                        kind::arbitrary::Punctuation(':')
                    ),
                    ..
                })
            ) {
                i += 1;
                continue;
            }

            let found_new_line =
                self.get(i + 1).map_or(NewLineSearch::EndOfTokenStream, |x| {
                    match x {
                        Node::Leaf(token) => match &token.kind {
                            kind::arbitrary::Kind::Identifier(_)
                            | kind::arbitrary::Kind::Keyword(_)
                            | kind::arbitrary::Kind::Numeric(_)
                            | kind::arbitrary::Kind::Punctuation(_)
                            | kind::arbitrary::Kind::Character(_)
                            | kind::arbitrary::Kind::String(_) => {
                                NewLineSearch::FoundSignificant
                            }

                            kind::arbitrary::Kind::NewLine(_) => {
                                NewLineSearch::Found(i + 1)
                            }
                        },
                        Node::Fragment(_) => NewLineSearch::FoundSignificant,
                    }
                });

            match (found_new_line, is_in_indent) {
                (NewLineSearch::EndOfTokenStream, false)
                | (NewLineSearch::FoundSignificant, _) => {
                    i += 1;
                }

                (NewLineSearch::EndOfTokenStream, true) => {
                    self.splice(i.., std::iter::empty());
                    changed = true;
                    break;
                }
                (NewLineSearch::Found(new_line_index), _) => {
                    self.splice((i + 1)..=new_line_index, std::iter::empty());
                    changed = true;
                }
            }
        }

        let mut i = 0;
        while i < self.len() {
            let Node::Fragment(Fragment::Indentation(current)) = &self[i]
            else {
                i += 1;
                continue;
            };

            let (offset, Some(level)) = get_indentation_level(&self[(i + 1)..])
            else {
                i += 1;
                continue;
            };

            if level >= current.size {
                changed = true;
                self.splice((i + 1)..=(i + 1 + offset), std::iter::empty());
            } else {
                i += 1;
            }
        }

        changed
    }

    fn sanitize_in_indent_block(&mut self) -> bool {
        let mut changed = false;
        // remove new line and comment indentation block
        let starting_len = self.len();
        self.retain(|x| {
            !x.as_leaf().is_some_and(|x| x.kind.is_new_line())
                && !x.as_fragment().is_some_and(Fragment::is_indentation)
        });

        changed |= starting_len != self.len();

        // remove leading whitespaces
        if let Some(Node::Leaf(first)) = self.first_mut() {
            first.prior_insignificant = None;
        }

        match self.first_mut() {
            Some(Node::Leaf(first)) => first.prior_insignificant = None,
            Some(Node::Fragment(Fragment::Indentation(first))) => {
                first.colon_insignificant = None;
            }
            Some(Node::Fragment(Fragment::Delimited(first))) => {
                first.open_insignificant = None;
            }
            None => {}
        }

        changed
    }

    fn fix_indentation(&mut self) {
        for token in &mut **self {
            match token {
                Node::Fragment(Fragment::Delimited(delimited)) => {
                    delimited.nodes.fix_indentation();
                }
                Node::Fragment(Fragment::Indentation(indentation)) => {
                    indentation.fix_indentation(indentation.size);
                }
                _ => {}
            }
        }
    }
}

impl Indentation {
    #[allow(unused)]
    pub(super) fn fix_indentation(&mut self, indentation_level: usize) {
        for line in &mut self.block.lines {
            match line {
                IndentationLine::Normal(token_stream, indentation) => {
                    match token_stream.first_mut() {
                        Some(Node::Leaf(leaf)) => {
                            leaf.prior_insignificant =
                                Some(PriorInsignificant::Whitespace(
                                    indentation_level,
                                ));
                        }
                        Some(Node::Fragment(Fragment::Delimited(
                            delimited,
                        ))) => {
                            delimited.open_insignificant =
                                Some(PriorInsignificant::Whitespace(
                                    indentation_level,
                                ));
                        }
                        Some(Node::Fragment(Fragment::Indentation(
                            indentation,
                        ))) => {
                            indentation.colon_insignificant =
                                Some(PriorInsignificant::Whitespace(
                                    indentation_level,
                                ));
                        }

                        None => {
                            if let Some(indentation) = indentation {
                                indentation.colon_insignificant =
                                    Some(PriorInsignificant::Whitespace(
                                        indentation_level,
                                    ));
                            }
                        }
                    }

                    for token in token_stream.iter_mut() {
                        match token {
                            Node::Fragment(Fragment::Delimited(delimited)) => {
                                delimited.nodes.fix_indentation();
                            }

                            Node::Fragment(Fragment::Indentation(
                                indentation,
                            )) => {
                                indentation.fix_indentation(
                                    indentation_level + indentation.size,
                                );
                            }

                            _ => {}
                        }
                    }

                    if let Some(indentation) = indentation {
                        indentation.fix_indentation(
                            indentation_level + indentation.size,
                        );
                    }
                }
                IndentationLine::WhiteSpaces(_) => {}
            }
        }
    }
}

pub fn arbitrary_indentation() -> impl Strategy<Value = Indentation> {
    Indentation::arbitrary().prop_map(|mut x| {
        x.fix_indentation(x.size);
        x
    })
}

pub fn arbitrary_nodes() -> impl Strategy<Value = Nodes> {
    Nodes::arbitrary().prop_map(|mut x| {
        x.sanitize(true);
        x.fix_indentation();
        x
    })
}
