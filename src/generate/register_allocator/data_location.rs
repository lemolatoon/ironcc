use crate::{
    error::CompileError,
    generate::registers::{RegKind, NUM_REGISTER},
    unimplemented_err,
};

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub struct RegId(usize);

impl From<usize> for RegId {
    fn from(value: usize) -> Self {
        RegId(value)
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub struct StackRegIdx(usize);

impl StackRegIdx {
    pub fn as_depth(&self) -> usize {
        self.0 * 8
    }

    pub fn as_idx(&self) -> usize {
        self.0
    }

    pub fn from_idx(idx: usize) -> Self {
        Self(idx)
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub enum DataLocation {
    Register(RegKind),
    Stack(StackRegIdx),
}

impl From<RegKind> for DataLocation {
    fn from(reg: RegKind) -> Self {
        DataLocation::Register(reg)
    }
}

impl DataLocation {
    pub fn new_from_depth(depth: usize) -> Result<Self, CompileError> {
        if depth % 8 != 0 {
            return Err(unimplemented_err!(format!(
                "try to new DataLocation from depth, but the provided depth was not multiple of 8."
            )));
        }

        Ok(DataLocation::Stack(StackRegIdx(depth / 8)))
    }
}
