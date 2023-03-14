use std::collections::BTreeMap;

use crate::{
    error::{CompileError, VariableKind},
    tokenize::debug_infos::DebugInfo,
};

use super::{
    analyze::{
        ConstInitializer, Enum, EnumTagKind, EnumVariant, GVar, LVar, StructTagKind, Taged, Var,
    },
    types::{InCompleteKind, Type},
    util::aligned_offset,
};

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Scope {
    global: BTreeMap<String, GVar>,
    pub scopes: Vec<BTreeMap<String, LVar>>,
    pub tag_scope: Vec<BTreeMap<String, Taged>>,
    max_stack_size: usize,
    diff: Vec<usize>,
    index_for_anonymous_enum_variants: usize,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            global: BTreeMap::new(),
            scopes: Vec::new(),
            tag_scope: Vec::new(),
            max_stack_size: 0,
            diff: Vec::new(),
            index_for_anonymous_enum_variants: 0,
        }
    }

    pub fn register_tag(&mut self, name: String, taged: Taged) {
        self.tag_scope
            .last_mut()
            .expect("INTERNAL COMPILER ERROR.")
            .insert(name, taged);
    }

    pub fn register_anonymous_enum_tag(&mut self, enum_ident_map: BTreeMap<String, usize>) {
        let name = format!(
            ".__anonymous_enum_{}",
            self.index_for_anonymous_enum_variants
        );
        self.index_for_anonymous_enum_variants += 1;
        self.tag_scope
            .last_mut()
            .expect("INTERNAL COMPILER ERROR.")
            .insert(name.clone(), Taged::new_enum_tag(name, enum_ident_map));
    }

    pub fn push_tag_scope(&mut self) {
        self.tag_scope.push(BTreeMap::new());
    }

    pub fn pop_tag_scope(&mut self) {
        self.tag_scope.pop();
    }

    pub fn push_scope(&mut self) {
        self.diff.push(0);
        self.scopes.push(BTreeMap::new());
        self.tag_scope.push(BTreeMap::new());
    }

    pub fn pop_scope(&mut self, offset: &mut usize) {
        assert!(!self.scopes.is_empty());
        assert!(!self.tag_scope.is_empty());
        self.scopes.pop();
        self.tag_scope.pop();
        *offset -= self.diff.pop().expect("diff has to exist.");
    }

    pub fn look_up_struct_tag(&self, name: &str) -> Option<&Taged> {
        for map in self.tag_scope.iter().rev() {
            if let Some(taged) = map.get(name) {
                return Some(taged);
            }
        }
        None
    }

    pub fn resolve_tag_name(&self, tag_name: &String) -> Option<&Taged> {
        for map in self.tag_scope.iter().rev() {
            if let Some(map) = map.get(tag_name) {
                return Some(map);
            }
        }
        None
    }

    pub fn resolve_tag_name_and_get_ty(&self, tag_name: &String) -> Option<Type> {
        if let Some(Taged::Struct(StructTagKind::Struct(struct_struct))) =
            self.resolve_tag_name(tag_name)
        {
            Some(Type::Struct(struct_struct.clone()))
        } else {
            None
        }
    }

    pub const fn get_stack_size(&self) -> usize {
        self.max_stack_size
    }

    pub fn reset_stack(&mut self) {
        assert!(self.scopes.is_empty());
        self.max_stack_size = 0;
    }

    pub fn look_up_lvar(&self, name: &String) -> Option<Var> {
        for map in self.scopes.iter().rev() {
            if let Some(lvar) = map.get(name) {
                return Some(Var::LVar(lvar.clone()));
            }
        }
        None
    }

    pub fn look_up_gvar(&self, name: &String) -> Option<Var> {
        if let Some(gvar) = self.global.get(name).map(|gvar| Var::GVar(gvar.clone())) {
            return Some(gvar);
        }
        None
    }

    pub fn look_up_enum_variant(&self, name: &String) -> Option<Var> {
        if let Some(enum_variant) = self.resolve_enum_variant_name(name) {
            return Some(Var::EnumVariant(enum_variant));
        }

        None
    }

    pub fn look_up(&self, name: &String) -> Option<Var> {
        for map in self.scopes.iter().rev() {
            if let Some(lvar) = map.get(name) {
                return Some(Var::LVar(lvar.clone()));
            }
        }
        if let Some(gvar) = self.global.get(name).map(|gvar| Var::GVar(gvar.clone())) {
            return Some(gvar);
        }

        if let Some(enum_variant) = self.resolve_enum_variant_name(name) {
            return Some(Var::EnumVariant(enum_variant));
        }

        None
    }

    pub fn resolve_enum_variant_name(&self, name: &String) -> Option<EnumVariant> {
        for map in self.tag_scope.iter().rev() {
            let map_iter = map.values();
            for taged in map_iter {
                if let Taged::Enum(EnumTagKind::Enum(Enum {
                    tag: _,
                    members: map,
                })) = taged
                {
                    if let Some(value) = map.get(name) {
                        return Some(EnumVariant {
                            name: name.clone(),
                            value: *value,
                        });
                    }
                }
            }
        }
        None
    }

    fn insert_lvar_to_current_scope(&mut self, name: String, lvar: LVar) {
        self.scopes.last_mut().map_or_else(
            || {
                unreachable!(
                    "Unreachable. scope stacks have to have local scope when you register lvar."
                );
            },
            |map| {
                map.insert(name, lvar);
            },
        );
    }

    fn insert_global_var_to_global_scope(&mut self, gvar: GVar) {
        self.global.insert(gvar.name.clone(), gvar);
    }

    pub fn register_gvar(
        &mut self,
        debug_info: DebugInfo,
        name: &str,
        ty: Type,
        is_extern: bool,
        init: Option<ConstInitializer>,
    ) -> Result<GVar, CompileError> {
        for scope in &self.scopes {
            if scope.contains_key(name) {
                return Err(CompileError::new_redefined_variable(
                    name.to_string(),
                    debug_info,
                    VariableKind::Local,
                ));
            }
        }
        if let Some(gvar) = self.global.get(name) {
            if !(gvar.is_extern || is_extern) {
                return Err(CompileError::new_redefined_variable(
                    name.to_string(),
                    debug_info,
                    VariableKind::Global,
                ));
            }
        }
        let gvar = GVar {
            name: name.to_string(),
            ty,
            init,
            is_extern,
        };
        self.insert_global_var_to_global_scope(gvar.clone());
        Ok(gvar)
    }

    pub fn register_lvar(
        &mut self,
        debug_info: DebugInfo,
        new_offset: &mut usize,
        name: &str,
        ty: Type,
    ) -> Result<LVar, CompileError> {
        for scope in &self.scopes {
            if scope.contains_key(name) {
                return Err(CompileError::new_redefined_variable(
                    name.to_string(),
                    debug_info,
                    VariableKind::Local,
                ));
            }
        }
        // if self.global.contains_key(name) {
        //     return Err(CompileError::new_redefined_variable(
        //         name.to_string(),
        //         debug_info,
        //         VariableKind::Global,
        //     ));
        // }
        let mut ty = ty;
        if let Type::InComplete(InCompleteKind::Struct(tag)) = &ty {
            let taged = self.look_up_struct_tag(tag);
            if let Some(Taged::Struct(StructTagKind::Struct(struct_struct))) = taged {
                ty = Type::Struct(struct_struct.clone());
            }
        };
        *self.diff.last_mut().expect("this has to exist.") +=
            aligned_offset(*new_offset, &ty) - *new_offset;
        *new_offset = aligned_offset(*new_offset, &ty);
        let lvar = LVar::new_raw(*new_offset, ty);
        self.insert_lvar_to_current_scope(name.to_string(), lvar.clone());
        self.max_stack_size = usize::max(self.max_stack_size, *new_offset);
        Ok(lvar)
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}
