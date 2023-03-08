use std::collections::BTreeMap;

use crate::{error::CompileError, unimplemented_err};

#[derive(PartialOrd, Ord, PartialEq, Eq, Copy, Clone, Debug)]
pub enum ParserContextKind {
    TopFuncArgs,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct ParserContext(BTreeMap<ParserContextKind, Vec<bool>>);

impl ParserContext {
    pub fn new() -> Self {
        let mut map = BTreeMap::new();
        // insert all kinds
        map.insert(ParserContextKind::TopFuncArgs, Vec::new());
        Self(map)
    }

    pub fn push_ctx(&mut self, context: ParserContextKind, flag: bool) -> Result<(), CompileError> {
        self.0
            .get_mut(&context)
            .ok_or_else(|| {
                unimplemented_err!(format!(
                    "INTERNAL COMPILER ERROR: ParserContextKind: {:?} is not initialized.",
                    &context
                ))
            })?
            .push(flag);
        Ok(())
    }

    pub fn pop(&mut self, context: ParserContextKind) -> Result<(), CompileError> {
        let vec = self.0.get_mut(&context).ok_or_else(|| {
            unimplemented_err!(format!(
                "INTERNAL COMPILER ERROR: ParserContextKind: {:?} is not initialized.",
                &context
            ))
        })?;
        if vec.is_empty() {
            return Err(Self::error());
        }
        vec.pop();
        Ok(())
    }

    pub fn in_top_func_args(&self, context: ParserContextKind) -> Result<bool, CompileError> {
        let vec = self.0.get(&context).ok_or_else(|| {
            unimplemented_err!(format!(
                "INTERNAL COMPILER ERROR: ParserContextKind: {:?} is not initialized.",
                &context
            ))
        })?;
        if let Some(stack_top) = vec.last() {
            Ok(*stack_top)
        } else {
            Err(Self::error())
        }
    }

    fn error() -> CompileError {
        unimplemented_err!("INTERNAL COMPILER ERROR: ParserContext's stack state is broken")
    }
}
