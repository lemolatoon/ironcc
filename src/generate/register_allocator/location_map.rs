use std::collections::BTreeMap;

use crate::{error::CompileError, unimplemented_err};

use super::data_location::{DataLocation, RegId};

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct LocationMap(BTreeMap<RegId, DataLocation>);

impl LocationMap {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

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
        self.0.insert(id1, location2);

        Ok(())
    }
}
