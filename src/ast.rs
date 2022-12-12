use std::fmt;

#[derive(Clone, Copy, PartialEq)]
pub enum NumberKind {
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
}

impl fmt::Display for NumberKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                NumberKind::I8 => "i8",
                NumberKind::I16 => "i16",
                NumberKind::I32 => "i32",
                NumberKind::I64 => "i64",
                NumberKind::I128 => "i128",
                NumberKind::U8 => "u8",
                NumberKind::U16 => "u16",
                NumberKind::U32 => "u32",
                NumberKind::U64 => "u64",
                NumberKind::U128 => "u128",
                NumberKind::F32 => "f32",
                NumberKind::F64 => "f64",
            }
        )
    }
}

impl NumberKind {
    pub fn bytesize(&self) -> usize {
        match self {
            NumberKind::I8 => 8,
            NumberKind::I16 => 16,
            NumberKind::I32 => 32,
            NumberKind::I64 => 64,
            NumberKind::I128 => 128,
            NumberKind::U8 => 8,
            NumberKind::U16 => 16,
            NumberKind::U32 => 32,
            NumberKind::U64 => 64,
            NumberKind::U128 => 128,
            NumberKind::F32 => 32,
            NumberKind::F64 => 64,
        }
    }

    pub fn is_signed_int(&self) -> bool {
        match self {
            NumberKind::I8
            | NumberKind::I16
            | NumberKind::I32
            | NumberKind::I64
            | NumberKind::I128 => true,
            _ => false,
        }
    }

    pub fn is_unsigned_int(&self) -> bool {
        match self {
            NumberKind::U8
            | NumberKind::U16
            | NumberKind::U32
            | NumberKind::U64
            | NumberKind::U128 => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            NumberKind::F32 | NumberKind::F64 => true,
            _ => false,
        }
    }
}

impl From<i8> for NumberKind {
    fn from(_: i8) -> Self {
        NumberKind::I8
    }
}

impl From<i16> for NumberKind {
    fn from(_: i16) -> Self {
        NumberKind::I16
    }
}

impl From<i32> for NumberKind {
    fn from(_: i32) -> Self {
        NumberKind::I32
    }
}

impl From<i64> for NumberKind {
    fn from(_: i64) -> Self {
        NumberKind::I64
    }
}

impl From<i128> for NumberKind {
    fn from(_: i128) -> Self {
        NumberKind::I128
    }
}

impl From<u8> for NumberKind {
    fn from(_: u8) -> Self {
        NumberKind::U8
    }
}

impl From<u16> for NumberKind {
    fn from(_: u16) -> Self {
        NumberKind::U16
    }
}

impl From<u32> for NumberKind {
    fn from(_: u32) -> Self {
        NumberKind::U32
    }
}

impl From<u64> for NumberKind {
    fn from(_: u64) -> Self {
        NumberKind::U64
    }
}

impl From<u128> for NumberKind {
    fn from(_: u128) -> Self {
        NumberKind::U128
    }
}

impl From<f32> for NumberKind {
    fn from(_: f32) -> Self {
        NumberKind::F32
    }
}

impl From<f64> for NumberKind {
    fn from(_: f64) -> Self {
        NumberKind::F64
    }
}
