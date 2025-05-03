use crate::{expect, state::State};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unexpected;

pub trait Parser {
    fn parse(&self, state: &mut State) -> Result<(), Unexpected>;
}

impl Parser for expect::Identifier {
    fn parse(&self, state: &mut State) -> Result<(), Unexpected> { Ok(()) }
}

impl<'a, F: Fn(&mut State) -> Result<(), Unexpected>> Parser for F {
    fn parse(&self, state_machine: &mut State) -> Result<(), Unexpected> {
        self(state_machine)
    }
}
