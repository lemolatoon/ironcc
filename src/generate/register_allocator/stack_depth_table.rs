use crate::{error::CompileError, unimplemented_err};

use super::data_location::{DataLocation, RegId, StackRegIdx};

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct StackDepthTable {
    reg_ids: Vec<Option<RegId>>,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug)]
pub struct StackDepthTableEntry<'a> {
    stack_reg_idx: StackRegIdx,
    stack: &'a mut Vec<Option<RegId>>,
}

impl<'a> StackDepthTableEntry<'a> {
    pub fn new(
        stack_reg_idx: StackRegIdx,
        reg_ids: &'a mut Vec<Option<RegId>>,
    ) -> Result<Self, CompileError> {
        if reg_ids.get(stack_reg_idx.as_idx()).is_none() {
            return Err(unimplemented_err!(
                "INTERNAL COMPILER ERROR: try to create StackDepthTableEntry, but failed"
            ));
        }
        Ok(Self {
            stack_reg_idx,
            stack: reg_ids,
        })
    }

    pub fn as_location(&self) -> DataLocation {
        DataLocation::Stack(self.stack_reg_idx)
    }

    pub fn insert(&mut self, id: RegId) {
        *(self.stack.get_mut(self.stack_reg_idx.as_idx()).unwrap()) = Some(id);
        // TODO: StackDepthTableEntry について再考
    }

    pub fn deallocate(&mut self) {
        *(self.stack.get_mut(self.stack_reg_idx.as_idx()).unwrap()) = None;
    }
}

impl StackDepthTable {
    pub fn new() -> Self {
        Self {
            reg_ids: Vec::new(),
        }
    }

    pub fn allocate_entry(&mut self) -> Result<StackDepthTableEntry, CompileError> {
        let stack_reg_idx = self.reg_ids.len();
        self.reg_ids.push(None);
        StackDepthTableEntry::new(StackRegIdx::from_idx(stack_reg_idx), &mut self.reg_ids)
    }

    pub fn allocate_entry_of_specific_depth(
        &mut self,
        stack_reg_idx: StackRegIdx,
    ) -> Result<StackDepthTableEntry, CompileError> {
        self.ensures_accessable_idx(stack_reg_idx.as_idx());
        if let Some(maybe_stack_reg_idx) = self.reg_ids.get(stack_reg_idx.as_idx()) {
            if maybe_stack_reg_idx.is_some() {
                return Err(unimplemented_err!(format!("INTERNAL COMPILER ERROR: try to allocate specific depth in stack area, the specified stack_reg_idx: {:?} was already used", stack_reg_idx)));
            }
        } else {
            unreachable!();
        }
        StackDepthTableEntry::new(stack_reg_idx, &mut self.reg_ids)
    }

    pub fn ensures_accessable_idx(&mut self, idx: usize) {
        for idx in 0..=idx {
            if self.reg_ids.get(idx).is_none() {
                self.reg_ids.push(None);
            }
        }
    }

    pub fn get(&self, idx: StackRegIdx) -> Option<&Option<RegId>> {
        self.reg_ids.get(idx.as_idx())
    }

    pub fn get_vec(&self) -> &Vec<Option<RegId>> {
        &self.reg_ids
    }

    pub fn get_vec_mut(&mut self) -> &mut Vec<Option<RegId>> {
        &mut self.reg_ids
    }

    pub fn deallocate_all(&mut self) {
        self.reg_ids.clear();
    }
}
