use std::collections::BTreeMap;

use crate::{
    analyze::types::Type,
    error::{CompileError, VariableKind},
    tokenize::debug_infos::DebugInfo,
};

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Scope {
    typedef_names: Vec<BTreeMap<String, Type>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            typedef_names: vec![BTreeMap::new()], // global scope
        }
    }

    pub fn scope_push(&mut self) {
        self.typedef_names.push(BTreeMap::new());
    }

    pub fn scope_pop(&mut self) {
        // global scope has not to be popped
        assert!(self.typedef_names.len() > 1);
        self.typedef_names.pop();
    }

    pub fn look_up_typedef_name(&self, name: &str) -> Option<Type> {
        for scope in self.typedef_names.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    pub fn register_typedef_name(
        &mut self,
        name: String,
        ty: Type,
        debug_info: DebugInfo,
    ) -> Result<(), CompileError> {
        if self.typedef_names.last().unwrap().contains_key(&name) {
            return Err(CompileError::new_redefined_variable(
                name,
                debug_info,
                VariableKind::Typedef,
            ));
        }
        self.typedef_names.last_mut().unwrap().insert(name, ty);
        Ok(())
    }
}
