use crate::{
    analyze::expr::ConvExpr,
    error::{CompileError, UnexpectedTypeSizeStatus},
    unimplemented_err,
};

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub enum RegOrLit {
    Reg(RegKind),
    Lit(isize),
}

impl RegOrLit {
    pub fn size_to_reg_name(&self, size: RegSize) -> String {
        match size {
            RegSize::Byte => self.byte(),
            RegSize::Dword => self.dword(),
            RegSize::Qword => self.qword(),
        }
    }

    pub fn byte(&self) -> String {
        match self {
            RegOrLit::Reg(reg) => reg.byte().to_string(),
            RegOrLit::Lit(num) => num.to_string(),
        }
    }

    pub fn dword(&self) -> String {
        match self {
            RegOrLit::Reg(reg) => reg.dword().to_string(),
            RegOrLit::Lit(num) => num.to_string(),
        }
    }

    pub fn qword(&self) -> String {
        match self {
            RegOrLit::Reg(reg) => reg.qword().to_string(),
            RegOrLit::Lit(num) => num.to_string(),
        }
    }
}

impl ToString for RegOrLit {
    fn to_string(&self) -> String {
        match self {
            RegOrLit::Reg(reg) => reg.to_string(),
            RegOrLit::Lit(num) => num.to_string(),
        }
    }
}

pub const NUM_REGISTER: usize = 15;
#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub enum RegKind {
    Rax = 0,
    Rdi = 1,
    Rsi = 2,
    Rdx = 3,
    Rcx = 4,
    Rbp = 5,
    Rsp = 6,
    R8 = 7,
    R9 = 8,
    R10 = 9,
    R11 = 10,
    R12 = 11,
    R13 = 12,
    R14 = 13,
    R15 = 14,
}

impl TryFrom<usize> for RegKind {
    type Error = CompileError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        let u8_value = value as u8;
        if u8_value > NUM_REGISTER as u8 {
            return Err(unimplemented_err!(format!(
                "Try to convert usize to RegKind, but it exceeds its number: {}",
                value
            )));
        }
        Ok(unsafe { std::mem::transmute(u8_value) })
    }
}

impl ToString for RegKind {
    fn to_string(&self) -> String {
        match self {
            RegKind::Rax => "rax",
            RegKind::Rdi => "rdi",
            RegKind::Rsi => "rsi",
            RegKind::Rdx => "rdx",
            RegKind::Rcx => "rcx",
            RegKind::Rbp => "rbp",
            RegKind::Rsp => "rsp",
            RegKind::R8 => "r8",
            RegKind::R9 => "r9",
            RegKind::R10 => "r10",
            RegKind::R11 => "r11",
            RegKind::R12 => "r12",
            RegKind::R13 => "r13",
            RegKind::R14 => "r14",
            RegKind::R15 => "r15",
        }
        .to_string()
    }
}

impl TryFrom<&str> for RegKind {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "rax" => RegKind::Rax,
            "rdi" => RegKind::Rdi,
            "rsi" => RegKind::Rsi,
            "rdx" => RegKind::Rdx,
            "rcx" => RegKind::Rcx,
            "r8" => RegKind::R8,
            "r9" => RegKind::R9,
            _ => return Err(()),
        })
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub enum RegSize {
    Byte,
    Dword,
    Qword,
}

impl RegSize {
    pub const fn try_new(size: usize) -> Option<Self> {
        Some(match size {
            1 => Self::Byte,
            4 => Self::Dword,
            8 => Self::Qword,
            _ => return None,
        })
    }

    // clippy complains about this, but it's wrong
    #[allow(clippy::missing_const_for_fn)]
    pub fn try_new_with_error(size: usize, expr: ConvExpr) -> Result<Self, CompileError> {
        Ok(match size {
            1 => Self::Byte,
            4 => Self::Dword,
            8 => Self::Qword,
            _ => {
                return Err(CompileError::new_type_size_error(
                    UnexpectedTypeSizeStatus::Expr(expr),
                ));
            }
        })
    }
}

impl AsRef<str> for RegSize {
    fn as_ref(&self) -> &str {
        match self {
            RegSize::Byte => "BYTE",
            RegSize::Dword => "DWORD",
            RegSize::Qword => "QWORD",
        }
    }
}

impl RegSize {
    pub const fn size_to_name(size: usize) -> Option<RegSize> {
        match size {
            1 => Some(Self::Byte),
            4 => Some(Self::Dword),
            8 => Some(Self::Qword),
            _ => None,
        }
    }
}

impl RegKind {
    pub const fn size_to_reg_name(&self, size: RegSize) -> &str {
        match size {
            RegSize::Byte => self.byte(),
            RegSize::Dword => self.dword(),
            RegSize::Qword => self.qword(),
        }
    }

    pub const fn byte(&self) -> &str {
        match self {
            RegKind::Rax => "al",
            RegKind::Rdi => "dil",
            RegKind::Rsi => "sil",
            RegKind::Rdx => "dl",
            RegKind::Rcx => "cl",
            RegKind::Rbp => "bpl",
            RegKind::Rsp => "spl",
            RegKind::R8 => "r8b",
            RegKind::R9 => "r9b",
            RegKind::R10 => "r10b",
            RegKind::R11 => "r11b",
            RegKind::R12 => "r12b",
            RegKind::R13 => "r13b",
            RegKind::R14 => "r14b",
            RegKind::R15 => "r15b",
        }
    }

    pub const fn dword(&self) -> &str {
        match self {
            RegKind::Rax => "eax",
            RegKind::Rdi => "edi",
            RegKind::Rsi => "esi",
            RegKind::Rdx => "edx",
            RegKind::Rcx => "ecx",
            RegKind::Rbp => "ebp",
            RegKind::Rsp => "esp",
            RegKind::R8 => "r8d",
            RegKind::R9 => "r9d",
            RegKind::R10 => "r10d",
            RegKind::R11 => "r11d",
            RegKind::R12 => "r12d",
            RegKind::R13 => "r13d",
            RegKind::R14 => "r14d",
            RegKind::R15 => "r15d",
        }
    }

    pub const fn qword(&self) -> &str {
        match self {
            RegKind::Rax => "rax",
            RegKind::Rdi => "rdi",
            RegKind::Rsi => "rsi",
            RegKind::Rdx => "rdx",
            RegKind::Rcx => "rcx",
            RegKind::Rbp => "rbp",
            RegKind::Rsp => "rsp",
            RegKind::R8 => "r8",
            RegKind::R9 => "r9",
            RegKind::R10 => "r10",
            RegKind::R11 => "r11",
            RegKind::R12 => "r12",
            RegKind::R13 => "r13",
            RegKind::R14 => "r14",
            RegKind::R15 => "r15",
        }
    }
}
