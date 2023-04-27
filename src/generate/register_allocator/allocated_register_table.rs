use crate::{
    error::CompileError,
    generate::registers::{RegKind, NUM_REGISTER},
    unimplemented_err,
};

use super::data_location::{DataLocation, RegId};

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct AllocatedRegisterTable([Option<RegId>; NUM_REGISTER]);

impl AllocatedRegisterTable {
    pub fn new() -> Self {
        Self([None; NUM_REGISTER])
    }

    pub fn iter(&self) -> impl Iterator<Item = &Option<RegId>> {
        self.0.iter()
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug)]
pub struct AllocatedRegisterTableEntry<'a> {
    pub(super) kind: RegKind,
    pub(super) entry: &'a mut Option<RegId>,
}

impl<'a> AllocatedRegisterTableEntry<'a> {
    pub fn new(kind: RegKind, muttable_ref: &'a mut Option<RegId>) -> Self {
        Self {
            kind,
            entry: muttable_ref,
        }
    }

    pub fn kind(&self) -> RegKind {
        self.kind
    }

    pub fn as_location(&self) -> DataLocation {
        DataLocation::Register(self.kind)
    }

    pub fn insert(&mut self, id: RegId) {
        *(self.entry) = Some(id);
    }

    pub fn deallocate(&mut self) {
        *(self.entry) = None;
    }
}

impl AllocatedRegisterTable {
    pub fn record_as_allocated(&mut self, reg: RegKind, id: RegId) -> Result<(), CompileError> {
        if self.0[reg as usize].is_some() {
            return Err(unimplemented_err!(format!(
                "INTERNAL COMPILER ERROR: try to allocate register {}, but it was already used.",
                reg.qword()
            )));
        } else {
            self.0[reg as usize] = Some(id);
        }

        Ok(())
    }

    pub fn deallocate(&mut self, reg: RegKind) -> Result<RegId, CompileError> {
        match self.0[reg as usize] {
            Some(prev_id) => {
                self.0[reg as usize] = None;
                Ok(prev_id)
            },
            None => Err(unimplemented_err!(format!(
                "INTERNAL COMPILER ERROR: try to deallocate register {}, but it was already deallocated.",
                reg.qword()
            ))),
        }
    }

    pub fn allocate_any_entry(
        &mut self,
    ) -> Result<Option<AllocatedRegisterTableEntry>, CompileError> {
        let idx = self
            .0
            .iter()
            .enumerate()
            .filter_map(|(idx, &allocated)| if allocated.is_some() { None } else { Some(idx) })
            .next();
        idx.map(|idx| {
            Ok(AllocatedRegisterTableEntry::new(
                RegKind::try_from(idx)?,
                &mut self.0[idx],
            ))
        })
        .transpose()
    }

    pub fn allocate_any(&mut self, id: RegId) -> Result<Option<RegKind>, CompileError> {
        Ok(if let Some(mut entry) = self.allocate_any_entry()? {
            entry.insert(id);
            Some(entry.kind())
        } else {
            None
        })
    }

    /// try to allocate given register return Entry if successfully allocated.
    pub fn allocate_entry(&mut self, reg: RegKind) -> Option<AllocatedRegisterTableEntry> {
        if self.0[reg as usize].is_none() {
            Some(AllocatedRegisterTableEntry::new(
                reg,
                &mut self.0[reg as usize],
            ))
        } else {
            None
        }
    }

    pub fn get(&self, reg: RegKind) -> &Option<RegId> {
        &self.0[reg as usize]
    }

    pub fn get_mut(&mut self, reg: RegKind) -> &mut Option<RegId> {
        &mut self.0[reg as usize]
    }
}
