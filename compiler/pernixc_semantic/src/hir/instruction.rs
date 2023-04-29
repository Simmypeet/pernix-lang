use super::{SsaValue, SsaVariableID};
use crate::cfg::{
    BasicBlockID, BasicInstruction, ConditionalJumpInstruction, InstructionBackend,
    JumpInstruction, ReturnInstruction,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Basic {
    SsaVariableAssignment(SsaVariableID),
}

impl BasicInstruction for Basic {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConditionalJump {
    pub condition: SsaValue,
    pub true_jump_target: BasicBlockID,
    pub false_jump_target: BasicBlockID,
}

impl ConditionalJumpInstruction for ConditionalJump {
    type Value = SsaValue;

    fn condition_value(&self) -> &Self::Value { &self.condition }

    fn true_jump_target(&self) -> BasicBlockID { self.true_jump_target }

    fn false_jump_target(&self) -> BasicBlockID { self.false_jump_target }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Jump {
    pub jump_target: BasicBlockID,
}

impl JumpInstruction for Jump {
    fn jump_target(&self) -> BasicBlockID { self.jump_target }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Return {
    pub return_value: Option<SsaValue>,
}

impl ReturnInstruction for Return {
    type Value = SsaValue;

    fn return_value(&self) -> Option<&Self::Value> { self.return_value.as_ref() }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Backend;

impl InstructionBackend for Backend {
    type Basic = Basic;
    type ConditionalJump = ConditionalJump;
    type Jump = Jump;
    type Return = Return;
    type Value = SsaValue;
}
