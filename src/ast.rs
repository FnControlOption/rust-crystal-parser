use crate::location::Location;
use std::{fmt, rc::Rc};

pub trait ASTNode<'a> {
    fn location(&self) -> Option<Rc<Location<'a>>>;
    fn set_location(&mut self, location: Option<Rc<Location<'a>>>);

    fn end_location(&self) -> Option<Rc<Location<'a>>>;
    fn set_end_location(&mut self, end_location: Option<Rc<Location<'a>>>);

    fn at(&mut self, location: Location<'a>) {
        self.set_location(Some(Rc::new(location)));
    }

    fn copy_location(&mut self, node: Box<dyn ASTNode<'a>>) {
        self.set_location(node.location());
        self.set_end_location(node.end_location());
    }

    fn at_end(&mut self, end_location: Location<'a>) {
        self.set_end_location(Some(Rc::new(end_location)));
    }

    fn copy_end_location(&mut self, node: Box<dyn ASTNode<'a>>) {
        self.set_end_location(node.end_location());
    }
}

macro_rules! ASTNode {
    ($name:ident) => {
        ASTNode!($name;);
    };

    ($name:ident; $(pub $field:ident: $typ:ty,)*) => {
        pub struct $name<'a> {
            location: Option<Rc<Location<'a>>>,
            end_location: Option<Rc<Location<'a>>>,
            $(pub $field: $typ),*
        }

        impl<'a> ASTNode<'a> for $name<'a> {
            fn location(&self) -> Option<Rc<Location<'a>>> {
                self.location.clone()
            }

            fn set_location(&mut self, location: Option<Rc<Location<'a>>>) {
                self.location = location;
            }

            fn end_location(&self) -> Option<Rc<Location<'a>>> {
                self.end_location.clone()
            }

            fn set_end_location(&mut self, end_location: Option<Rc<Location<'a>>>) {
                self.end_location = end_location;
            }
        }

        impl<'a> $name<'a> {
            pub fn new($($field: $typ),*) -> Box<Self> {
                Box::new(Self {
                    location: None,
                    end_location: None,
                    $($field),*
                })
            }
        }
    };
}

ASTNode!(Nop);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ExpressionsKeyword {
    None,
    Paren,
    Begin,
}

ASTNode!(
    Expressions;
    pub expressions: Vec<Box<dyn ASTNode<'a>>>,
    pub keyword: ExpressionsKeyword,
);

impl<'a> Expressions<'a> {
    pub fn default() -> Box<Self> {
        Self::new(vec![], ExpressionsKeyword::None)
    }

    pub fn from<T>(obj: T) -> Box<dyn ASTNode<'a>>
    where
        T: IntoExpressions<'a>,
    {
        obj.into()
    }
}

pub trait IntoExpressions<'a> {
    fn into(self) -> Box<dyn ASTNode<'a>>;
}

// impl<'a, T> IntoExpressions<'a> for Option<T>
// where
//     T: IntoExpressions<'a>,
// {
//     fn into(self) -> Box<dyn ASTNode<'a>> {
//         if let Some(obj) = self {
//             obj.into()
//         } else {
//             Nop::new()
//         }
//     }
// }

// impl<'a> IntoExpressions<'a> for Vec<Box<dyn ASTNode<'a>>> {
//     fn into(self) -> Box<dyn ASTNode<'a>> {
//         // Expressions::new(self, ExpressionsKeyword::None)
//         // Nop::<'a>::new()
//         // match self.len() {
//         //     0 => Nop::new(),
//         //     1 => self[0],
//         //     _ => Expressions::new(self, ExpressionsKeyword::None),
//         // }
//     }
// }

impl<'a> IntoExpressions<'a> for Box<dyn ASTNode<'a>> {
    fn into(self) -> Box<dyn ASTNode<'a>> {
        self
    }
}

ASTNode!(
    RangeLiteral;
    pub from: Box<dyn ASTNode<'a>>,
    pub to: Box<dyn ASTNode<'a>>,
    pub exclusive: bool,
);

#[derive(Clone, Copy, Debug, PartialEq)]
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Visibility {
    Public,
    Protected,
    Private,
}

#[test]
fn it_works() {
    RangeLiteral::new(Nop::new(), Nop::new(), false);
}
