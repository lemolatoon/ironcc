use super::{
    allocated_register_table::AllocatedRegisterTableEntry,
    data_location::{DataLocation, RegId},
    stack_depth_table::StackDepthTableEntry,
};

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug)]
pub enum RegIdEntry<'a> {
    Reg(AllocatedRegisterTableEntry<'a>),
    Stack(StackDepthTableEntry<'a>),
}
impl<'a> From<AllocatedRegisterTableEntry<'a>> for RegIdEntry<'a> {
    fn from(entry: AllocatedRegisterTableEntry<'a>) -> Self {
        RegIdEntry::Reg(entry)
    }
}

impl<'a> From<StackDepthTableEntry<'a>> for RegIdEntry<'a> {
    fn from(entry: StackDepthTableEntry<'a>) -> Self {
        RegIdEntry::Stack(entry)
    }
}

impl<'a> RegIdEntry<'a> {
    pub fn insert(&mut self, id: RegId) {
        match self {
            RegIdEntry::Reg(entry) => entry.insert(id),
            RegIdEntry::Stack(entry) => entry.insert(id),
        }
    }

    pub fn as_location(&self) -> DataLocation {
        match self {
            RegIdEntry::Reg(entry) => entry.as_location(),
            RegIdEntry::Stack(entry) => entry.as_location(),
        }
    }

    pub fn deallocate(&mut self) {
        match self {
            RegIdEntry::Reg(entry) => entry.deallocate(),
            RegIdEntry::Stack(entry) => entry.deallocate(),
        }
    }
}
