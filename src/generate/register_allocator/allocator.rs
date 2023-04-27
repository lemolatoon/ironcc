use crate::{error::CompileError, generate::registers::RegKind, unimplemented_err};

use super::{
    allocated_register_table::{AllocatedRegisterTable, AllocatedRegisterTableEntry},
    data_location::{DataLocation, RegId},
    location_map::LocationMap,
    regid_entry::RegIdEntry,
    stack_depth_table::{StackDepthTable, StackDepthTableEntry},
};

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct RegisterAllocator {
    location_map: LocationMap,
    allocated: AllocatedRegisterTable,
    depth: StackDepthTable,
}

impl RegisterAllocator {
    pub fn new() -> Self {
        Self {
            location_map: LocationMap::new(),
            allocated: AllocatedRegisterTable::new(),
            depth: StackDepthTable::new(),
        }
    }

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

    /// free all allocated stacks
    pub fn deallocate_stacks(&mut self) {
        self.depth.deallocate_all();
    }

    /// subscribe unmanaged place for this RegisterAllocator
    pub fn publish_reg_id(&mut self, location: DataLocation) -> Result<RegId, CompileError> {
        let (id, location) = self.location_map.allocate(location);
        match location {
            DataLocation::Register(reg) => {
                if let Some(mut entry) = self.allocated.allocate_entry(reg) {
                    entry.insert(id);
                } else {
                    return Err(unimplemented_err!(format!("INTERNAL COMPILER ERROR: try to subscribe specified location: {:?}, but the location was already used", location)));
                }
            }
            DataLocation::Stack(stack_reg_idx) => {
                let mut entry = self.depth.allocate_entry_of_specific_depth(stack_reg_idx)?;
                entry.insert(id);
            }
        }
        Ok(id)
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

    pub fn look_up_location_from_reg_id(&self, reg_id: &RegId) -> Option<&DataLocation> {
        self.location_map.get(reg_id)
    }

    fn from_location_to_entry(
        &mut self,
        location: DataLocation,
    ) -> Result<RegIdEntry, CompileError> {
        Ok(match location {
            DataLocation::Register(reg) => RegIdEntry::Reg(AllocatedRegisterTableEntry {
                kind: reg,
                entry: self.allocated.get_mut(reg),
            }),
            DataLocation::Stack(stack_reg_idx) => {
                self.depth.ensures_accessable_idx(stack_reg_idx.as_idx());
                RegIdEntry::Stack(StackDepthTableEntry::new(
                    stack_reg_idx,
                    self.depth.get_vec_mut(),
                )?)
            }
        })
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

#[cfg(test)]
mod register_allocator_test {
    use crate::generate::{
        register_allocator::data_location::StackRegIdx, registers::NUM_REGISTER,
    };

    use super::*;
    #[test]
    pub fn allocate_register_test() {
        let mut allocator = RegisterAllocator::new();
        let (reg_id, location) = allocator.allocate(Some(RegKind::Rax)).unwrap();
        assert_eq!(location, DataLocation::Register(RegKind::Rax));
        assert_eq!(
            *allocator.look_up_reg_id_from_location(location).unwrap(),
            reg_id
        );
        assert_eq!(
            *allocator.look_up_location_from_reg_id(&reg_id).unwrap(),
            location
        );
    }

    #[test]
    pub fn allocate_same_place_test() {
        let mut allocator = RegisterAllocator::new();
        let (reg_id1, location1) = allocator.allocate(Some(RegKind::Rax)).unwrap();
        assert_eq!(location1, DataLocation::Register(RegKind::Rax));
        assert_eq!(
            *allocator.look_up_reg_id_from_location(location1).unwrap(),
            reg_id1
        );
        assert_eq!(
            *allocator.look_up_location_from_reg_id(&reg_id1).unwrap(),
            location1
        );
        let (reg_id2, location2) = allocator.allocate(Some(RegKind::Rax)).unwrap();
        assert_eq!(location1, DataLocation::Register(RegKind::Rax));
        assert_ne!(location2, DataLocation::Register(RegKind::Rax));
        assert_eq!(
            *allocator.look_up_reg_id_from_location(location2).unwrap(),
            reg_id2
        );
        assert_eq!(
            *allocator.look_up_location_from_reg_id(&reg_id2).unwrap(),
            location2
        );
    }

    #[test]
    pub fn assign_test() {
        let mut allocator = RegisterAllocator::new();
        let (reg_id1, location1) = allocator.allocate(Some(RegKind::Rax)).unwrap();
        assert_eq!(location1, DataLocation::Register(RegKind::Rax));
        let (reg_id2, location2) = allocator.allocate(Some(RegKind::Rax)).unwrap();
        allocator
            .assign(reg_id2, DataLocation::Register(RegKind::Rax))
            .unwrap();
        assert_eq!(
            *allocator.look_up_location_from_reg_id(&reg_id2).unwrap(),
            DataLocation::Register(RegKind::Rax)
        );
        assert_eq!(
            *allocator.look_up_location_from_reg_id(&reg_id1).unwrap(),
            location2
        );
    }

    #[test]
    pub fn assign_to_not_allocated_location() {
        let mut allocator = RegisterAllocator::new();
        let (reg_id1, _) = allocator.allocate(Some(RegKind::Rax)).unwrap();
        allocator
            .assign(reg_id1, DataLocation::Register(RegKind::Rdi))
            .unwrap();
        assert_eq!(
            *allocator.look_up_location_from_reg_id(&reg_id1).unwrap(),
            DataLocation::Register(RegKind::Rdi)
        );
        assert_eq!(
            allocator.look_up_reg_id_from_location(DataLocation::Register(RegKind::Rax)),
            None
        );
    }

    #[test]
    pub fn stack_allocate_test() {
        let mut allocator = RegisterAllocator::new();
        let (rax_id, rax_location) = allocator.allocate(Some(RegKind::Rax)).unwrap();
        assert_eq!(rax_location, DataLocation::Register(RegKind::Rax));
        for _ in 0..(NUM_REGISTER - 1) {
            allocator.allocate(None).unwrap();
        }
        // all register might be allocated
        for maybe_reg_id in allocator.allocated.iter() {
            eprintln!("{:?}", maybe_reg_id);
            assert!(maybe_reg_id.is_some());
        }

        // allocate stack
        let (maybe_in_stack_reg_id, maybe_stack_location) = allocator.allocate(None).unwrap();
        assert_eq!(
            maybe_stack_location,
            DataLocation::Stack(StackRegIdx::from_idx(0))
        );
        assert_eq!(
            *allocator
                .look_up_reg_id_from_location(maybe_stack_location)
                .unwrap(),
            maybe_in_stack_reg_id
        );
        assert_eq!(
            *allocator
                .look_up_location_from_reg_id(&maybe_in_stack_reg_id)
                .unwrap(),
            maybe_stack_location
        );

        // assign value on stack to specific register
        allocator
            .assign(maybe_in_stack_reg_id, DataLocation::Register(RegKind::Rax))
            .unwrap();
        assert_eq!(
            *allocator
                .look_up_reg_id_from_location(maybe_stack_location)
                .unwrap(),
            rax_id
        );
        assert_eq!(
            *allocator.look_up_location_from_reg_id(&rax_id).unwrap(),
            maybe_stack_location
        );
        assert_eq!(
            *allocator
                .look_up_reg_id_from_location(DataLocation::Register(RegKind::Rax))
                .unwrap(),
            maybe_in_stack_reg_id
        );
        assert_eq!(
            *allocator
                .look_up_location_from_reg_id(&maybe_in_stack_reg_id)
                .unwrap(),
            DataLocation::Register(RegKind::Rax)
        );
    }

    #[test]
    pub fn publish_reg_id_test() {
        let mut allocator = RegisterAllocator::new();
        let (rax_id, _) = allocator.allocate(Some(RegKind::Rax)).unwrap();
        let rdi_id = allocator.publish_reg_id(RegKind::Rdi.into()).unwrap();
        assert_eq!(rdi_id, RegId::from(1));
        assert_eq!(
            *allocator.look_up_location_from_reg_id(&rdi_id).unwrap(),
            DataLocation::Register(RegKind::Rdi)
        );
        assert!(allocator.publish_reg_id(RegKind::Rax.into()).is_err());
        allocator
            .assign(rax_id, DataLocation::new_from_depth(0).unwrap())
            .unwrap();
        let stack_16_id = allocator
            .publish_reg_id(DataLocation::new_from_depth(16).unwrap())
            .unwrap();
        assert_eq!(
            *allocator
                .look_up_location_from_reg_id(&stack_16_id)
                .unwrap(),
            DataLocation::new_from_depth(16).unwrap()
        );
        assert!(allocator
            .publish_reg_id(DataLocation::new_from_depth(0).unwrap())
            .is_err());
        assert!(allocator
            .publish_reg_id(DataLocation::new_from_depth(8).unwrap())
            .is_ok());
        assert!(allocator
            .publish_reg_id(DataLocation::new_from_depth(16).unwrap())
            .is_err());
    }
}

#[test]
pub fn deallocate_stacks_test() {
    let mut allocator = RegisterAllocator::new();
    let id1 = allocator
        .publish_reg_id(DataLocation::new_from_depth(0).unwrap())
        .unwrap();
    assert_eq!(
        allocator.look_up_reg_id_from_location(DataLocation::new_from_depth(0).unwrap()),
        Some(&id1)
    );
    let (id2, _) = allocator.allocate_if_possible(RegKind::Rax.into()).unwrap();
    let _id3 = allocator
        .publish_reg_id(DataLocation::new_from_depth(8).unwrap())
        .unwrap();
    allocator.deallocate_stacks();
    assert_eq!(
        allocator.look_up_reg_id_from_location(RegKind::Rax.into()),
        Some(&id2)
    );

    assert_eq!(
        allocator.look_up_reg_id_from_location(DataLocation::new_from_depth(0).unwrap()),
        None
    );

    assert_eq!(
        allocator.look_up_reg_id_from_location(DataLocation::new_from_depth(8).unwrap()),
        None
    );
}
