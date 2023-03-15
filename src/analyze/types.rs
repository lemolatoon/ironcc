use crate::generate::registers::RegSize;

use super::analyze::{Func, Struct};

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Void,
    /// Tag unresolved struct or union
    InComplete(InCompleteKind),
    Base(BaseType),
    Ptr(Box<Type>),
    Func {
        ret_ty: Box<Type>,
        args: Vec<Type>,
        is_flexible: bool,
    },
    Array(Box<Type>, usize),
    // Tag resolved struct
    Struct(Struct),
    // Enum(Enum),
}

impl Type {
    pub fn ty_eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Type::Void, Type::Void) => true,
            (Type::InComplete(tag_name_lhs), Type::InComplete(tag_name_rhs)) => {
                tag_name_lhs == tag_name_rhs
            }
            (Type::Base(base_ty_lhs), Type::Base(base_ty_rhs)) => base_ty_lhs == base_ty_rhs,
            (Type::Ptr(ptr_to_lhs), Type::Ptr(ptr_to_rhs)) => ptr_to_lhs.ty_eq(ptr_to_rhs),
            (
                Type::Func {
                    ret_ty: return_ty_lhs,
                    args: arg_ty_lhs,
                    is_flexible: is_flexible_lhs,
                },
                Type::Func {
                    ret_ty: return_ty_rhs,
                    args: arg_ty_rhs,
                    is_flexible: is_flexible_rhs,
                },
            ) => {
                return_ty_lhs.ty_eq(return_ty_rhs)
                    && is_flexible_lhs == is_flexible_rhs
                    && arg_ty_lhs
                        .iter()
                        .zip(arg_ty_rhs.iter())
                        .all(|(lhs_ty, rhs_ty)| lhs_ty.ty_eq(rhs_ty))
            }
            (
                Type::Array(array_base_ty_lhs, length_lhs),
                Type::Array(array_base_ty_rhs, length_rhs),
            ) => array_base_ty_lhs == array_base_ty_rhs && length_lhs == length_rhs,
            (Type::Struct(struct_lhs), Type::Struct(struct_rhs)) => struct_lhs == struct_rhs,
            (Type::Struct(struct_lhs), Type::InComplete(InCompleteKind::Struct(tag_name_rhs))) => {
                struct_lhs.tag.as_ref() == Some(tag_name_rhs)
            }
            (Type::InComplete(InCompleteKind::Struct(tag_name_lhs)), Type::Struct(struct_rhs)) => {
                Some(tag_name_lhs) == struct_rhs.tag.as_ref()
            }
            _ => false,
        }
    }

    pub const fn is_ptr(&self) -> bool {
        matches!(self, Type::Ptr(_))
    }

    /// if `self` is array, return ptr-converted self
    pub fn into_ptr(self) -> Self {
        if let Type::Array(base_ty, _) = self {
            Type::Ptr(base_ty)
        } else {
            self
        }
    }

    pub fn get_ptr_to_recursively(&self) -> Option<&Self> {
        let mut ptr_to = self.get_ptr_to()?;
        while let Some(next_ptr_to) = ptr_to.get_ptr_to() {
            ptr_to = next_ptr_to;
        }
        return Some(ptr_to);
    }

    pub const fn is_func(&self) -> bool {
        matches!(self, Type::Func { .. })
    }
}

impl From<Func> for Type {
    fn from(func: Func) -> Self {
        Type::Func {
            ret_ty: Box::new(func.ret),
            args: func.args.args,
            is_flexible: func.args.is_flexible_length,
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum InCompleteKind {
    Struct(String),
    Enum(String),
}

impl Type {
    pub fn ptr(ty: Type) -> Self {
        Type::Ptr(Box::new(ty))
    }

    pub const fn is_void(&self) -> bool {
        matches!(self, Type::Void)
    }

    pub const fn is_void_ptr(&self) -> bool {
        if let Type::Ptr(ptr_to) = self {
            ptr_to.is_void()
        } else {
            false
        }
    }

    pub const fn get_ptr_to(&self) -> Option<&Type> {
        match self {
            Type::Base(_)
            | Type::Func {
                ret_ty: _,
                args: _,
                is_flexible: _,
            }
            | Type::Array(_, _)
            | Type::Struct(_)
            | Type::Void => None,
            Type::Ptr(ptr_to) => Some(ptr_to),
            Type::InComplete(_) => todo!(),
        }
    }

    /// Get the base type of array recursively, if the type is not an array just return its reference.
    pub const fn get_array_base_recursively(&self) -> &Type {
        match self {
            Type::Array(base_ty, _) => base_ty.get_array_base_recursively(),
            _ => self,
        }
    }

    pub const fn get_array_base(&self) -> Option<&Type> {
        match self {
            Type::Base(_)
            | Type::Func {
                ret_ty: _,
                args: _,
                is_flexible: _,
            }
            | Type::Struct(_)
            | Type::Void
            | Type::Ptr(_) => None,
            Type::Array(base, _) => Some(base),
            Type::InComplete(_) => todo!(),
        }
    }
    pub fn size_of(&self) -> usize {
        match self {
            Type::Base(BaseType::Int) => 4,
            Type::Base(BaseType::Char)
            | Type::Func {
                ret_ty: _,
                args: _,
                is_flexible: _,
            }
            | Type::Void => 1,
            Type::Ptr(_) => 8,
            Type::Array(ty, size) => ty.size_of() * *size,
            Type::Struct(struct_struct) => struct_struct.size_of(),
            Type::InComplete(InCompleteKind::Enum(_)) => Type::Base(BaseType::Int).size_of(),
            Type::InComplete(_) => todo!("{:?}", self),
        }
    }

    pub fn align_of(&self) -> usize {
        match self {
            Type::Void => todo!("return appropriate error"),
            Type::Base(BaseType::Int) => 4,
            Type::Base(BaseType::Char) => 1,
            Type::Ptr(_) => 8,
            Type::Func {
                ret_ty: _,
                args: _,
                is_flexible: _,
            }
            | Type::InComplete(_) => todo!(),
            Type::Array(base_ty, _) => base_ty.align_of(),
            Type::Struct(struct_struct) => struct_struct.align_of(),
        }
    }

    pub const fn base_type(&self) -> &Type {
        match self {
            ty @ Type::Base(_) => ty,
            Type::Ptr(base)
            | Type::Func {
                ret_ty: base,
                args: _,
                is_flexible: _,
            }
            | Type::Array(base, _) => base,
            Type::Struct(_) | Type::Void | Type::InComplete(_) => todo!(),
        }
    }

    pub const fn get_base(&self) -> Option<&BaseType> {
        match self {
            Type::Base(base) => Some(base),
            Type::Ptr(_)
            | Type::Func {
                ret_ty: _,
                args: _,
                is_flexible: _,
            }
            | Type::Array(_, _) => None,
            Type::Struct(_) | Type::Void | Type::InComplete(_) => todo!(),
        }
    }

    pub const fn is_base(&self) -> bool {
        match self {
            Type::Base(_) => true,
            Type::Void
            | Type::Ptr(_)
            | Type::Func {
                ret_ty: _,
                args: _,
                is_flexible: _,
            }
            | Type::Array(_, _)
            | Type::Struct(_) => false,
            Type::InComplete(_) => todo!(),
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub enum BaseType {
    Int,
    Char,
}

impl BaseType {
    pub const fn bytes(&self) -> usize {
        match self {
            BaseType::Int => 4,
            BaseType::Char => 1,
        }
    }
}

impl From<&BaseType> for RegSize {
    fn from(base: &BaseType) -> Self {
        match base {
            BaseType::Char => RegSize::Byte,
            BaseType::Int => RegSize::Dword,
        }
    }
}
