use super::types::Type;

/// calculate aligned next offset
pub fn aligned_offset(current_offset: usize, ty: &Type) -> usize {
    if (current_offset + ty.size_of()) % ty.align_of() == 0 {
        return current_offset + ty.size_of();
    }
    current_offset + ty.size_of() + ty.align_of() - (current_offset + ty.size_of()) % ty.align_of()
}

pub const fn align_to(current_offset: usize, alignment: usize) -> usize {
    if current_offset % alignment == 0 {
        current_offset
    } else {
        current_offset + alignment - current_offset % alignment
    }
}
