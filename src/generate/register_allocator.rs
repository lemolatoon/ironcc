use std::collections::BTreeMap;

use crate::{error::CompileError, unimplemented_err};

use super::registers::RegKind;

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
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub enum DataLocation {
    Register(RegKind),
    Stack(StackRegIdx),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct AllocatedRegisterTable([Option<RegId>; 16]);

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug)]
pub struct AllocatedRegisterTableEntry<'a> {
    kind: RegKind,
    entry: &'a mut Option<RegId>,
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

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct LocationMap(BTreeMap<RegId, DataLocation>);

impl LocationMap {
    /// Call this function under the consumption that the location is not occupied. Check it by looking at AllocatedRegisterTable
    pub fn allocate(&mut self, location: DataLocation) -> (RegId, DataLocation) {
        for id in (0..).map(RegId::from) {
            match self.0.entry(id) {
                std::collections::btree_map::Entry::Occupied(_) => continue,
                std::collections::btree_map::Entry::Vacant(entry) => {
                    entry.insert(location.clone());
                    return (id, location);
                }
            }
        }
        unreachable!()
    }

    pub fn relocate(&mut self, id: RegId, location: DataLocation) -> Result<(), CompileError> {
        if self.0.insert(id, location).is_none() {
            return Err(unimplemented_err!(format!("INTERNAL COMPILER ERROR: try to relocate data, but the id: {:?} was empty in location map.", id)));
        }
        Ok(())
    }

    pub fn delete(&mut self, id: RegId) -> Result<(), CompileError> {
        if self.0.remove(&id).is_none() {
            return Err(unimplemented_err!(format!("INTERNAL COMPILER ERROR: try to delete data from location map, but the id: {:?} was empty in location map.", id)));
        }
        Ok(())
    }

    pub fn get(&self, id: &RegId) -> Option<&DataLocation> {
        self.0.get(id)
    }

    pub fn replace(&mut self, id1: RegId, id2: RegId) -> Result<(), CompileError> {
        let entry1 = match self.0.entry(id1) {
            std::collections::btree_map::Entry::Vacant(_) => {
                return Err(unimplemented_err!(format!(
                    "INTERNAL COMPILER ERROR: try to replace {:?}, {:?} but {:?} was empty",
                    id1, id2, id1
                )))
            }
            std::collections::btree_map::Entry::Occupied(entry) => entry,
        };
        let id1_location = entry1.get().clone();
        let location2 = match self.0.insert(id2, id1_location) {
            Some(location2) => location2,
            None => {
                return Err(unimplemented_err!(format!(
                    "INTERNAL COMPILER ERROR: try to replace {:?}, {:?} but {:?} was empty",
                    id1, id2, id1
                )))
            }
        };
        self.0.insert(id2, location2);

        Ok(())
    }
}

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
        if reg_ids.get(stack_reg_idx.0).is_none() {
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
        *(self.stack.get_mut(self.stack_reg_idx.0).unwrap()) = Some(id);
        // TODO: StackDepthTableEntry について再考
    }

    pub fn deallocate(&mut self) {
        *(self.stack.get_mut(self.stack_reg_idx.0).unwrap()) = None;
    }
}

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

impl StackDepthTable {
    pub fn allocate_entry(&mut self) -> Result<StackDepthTableEntry, CompileError> {
        let stack_reg_idx = self.reg_ids.len();
        StackDepthTableEntry::new(StackRegIdx(stack_reg_idx), &mut self.reg_ids)
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
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct RegisterAllocator {
    location_map: LocationMap,
    allocated: AllocatedRegisterTable,
    depth: StackDepthTable,
}

impl RegisterAllocator {
    /// High Layer abstraction of relocate data to specific register
    pub fn assign(
        &mut self,
        id: RegId,
        assign_to_location: DataLocation,
    ) -> Result<(), CompileError> {
        match self.location_map.get(&id) {
            Some(&prev_allocated_location) if prev_allocated_location == assign_to_location => {
                // already assigned
                Ok(())
            }
            Some(prev_location) => {
                if let Some(using_assign_to_location_id) =
                    self.look_up_reg_id_from_location(assign_to_location)
                {
                    // prev state                               |   next state
                    // id -> prev_location                      |   id -> assign_to
                    // using_assign_to_location_id -> assign_to |   using_assign_to_location_id -> prev_location
                    self.replace(
                        (id, *prev_location),
                        (*using_assign_to_location_id, assign_to_location),
                    )?;
                } else {
                    // prev state            |   next state
                    // id -> prev_location   |   id -> assign_to
                    // None -> assign_to     |   None -> prev_location (deallocated)
                    // TODO: entry のデータ構造を変えてもっと抽象化する
                    let mut prev_location_entry = self.from_location_to_entry(*prev_location)?;
                    prev_location_entry.deallocate();
                    let mut assign_to_entry = self.from_location_to_entry(assign_to_location)?;
                    assign_to_entry.insert(id);
                    self.location_map.relocate(id, assign_to_location)?;
                }
                Ok(())
            }
            None => Err(CompileError::new_register_assign_error(
                id,
                assign_to_location,
                "given RegId is not yet allocated.",
            )),
        }
    }

    fn look_up_reg_id_from_location(&self, location: DataLocation) -> Option<&RegId> {
        match location {
            DataLocation::Register(reg) => self.allocated.get(reg).as_ref(),
            DataLocation::Stack(stack_reg_idx) => self
                .depth
                .get(stack_reg_idx)
                .and_then(|maybe_reg_id| maybe_reg_id.as_ref()),
        }
    }

    fn from_location_to_entry(
        &mut self,
        location: DataLocation,
    ) -> Result<RegIdEntry, CompileError> {
        match location {
            DataLocation::Register(reg) => RegIdEntry::Reg(AllocatedRegisterTableEntry {
                kind: reg,
                entry: self.allocated.get_mut(reg),
            }),
            DataLocation::Stack(stack_reg_idx) => RegIdEntry::Stack(StackDepthTableEntry::new(
                stack_reg_idx,
                self.depth.get_vec_mut(),
            )?),
        };
        todo!()
    }

    fn replace(
        &mut self,
        (id1, location1): (RegId, DataLocation),
        (id2, location2): (RegId, DataLocation),
    ) -> Result<(), CompileError> {
        let mut entry_at_location1 = self.from_location_to_entry(location1)?;
        entry_at_location1.insert(id2);
        let mut entry_at_location2 = self.from_location_to_entry(location2)?;
        entry_at_location2.insert(id1);
        self.location_map.replace(id1, id2)?;
        Ok(())
    }

    fn allocate_any_entry(&mut self) -> Result<RegIdEntry, CompileError> {
        if let Some(entry) = self.allocated.allocate_any_entry()? {
            Ok(entry.into())
        } else {
            Ok(self.depth.allocate_entry()?.into())
        }
    }

    /// High Layer abstraction of allocate to specific register
    pub fn allocate_if_possible(&mut self, reg: RegKind) -> Option<(RegId, DataLocation)> {
        if let Some(mut entry) = self.allocated.allocate_entry(reg) {
            let (id, location) = self.location_map.allocate(DataLocation::Register(reg));
            entry.insert(id);
            Some((id, location))
        } else {
            None
        }
    }

    /// High Layer abstraction of allocate to some location
    pub fn allocate(
        &mut self,
        prioritized_reg: Option<RegKind>,
    ) -> Result<(RegId, DataLocation), CompileError> {
        if let Some(prioritized_reg) = prioritized_reg {
            if let Some((id, location)) = self.allocate_if_possible(prioritized_reg) {
                return Ok((id, location));
            }
        }
        /* allocate any entry inlined */
        let mut entry: RegIdEntry = if let Some(entry) = self.allocated.allocate_any_entry()? {
            entry.into()
        } else {
            self.depth.allocate_entry()?.into()
        };
        /* allocate any entry end */
        let location = entry.as_location();
        let (id, location) = self.location_map.allocate(location);
        entry.insert(id);
        Ok((id, location))
    }
}
