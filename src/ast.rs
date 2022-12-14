use crate::location::Location;
use std::{fmt, rc::Rc};

pub type AstNodeBox<'a> = Box<dyn AstNode<'a> + 'a>;

pub trait AstNode<'a> {
    fn location(&self) -> Option<Rc<Location<'a>>>;
    fn set_location(&mut self, location: Option<Rc<Location<'a>>>);

    fn end_location(&self) -> Option<Rc<Location<'a>>>;
    fn set_end_location(&mut self, end_location: Option<Rc<Location<'a>>>);

    fn at(&mut self, location: Location<'a>) {
        self.set_location(Some(Rc::new(location)));
    }

    fn copy_location(&mut self, node: AstNodeBox<'a>) {
        self.set_location(node.location());
        self.set_end_location(node.end_location());
    }

    fn at_end(&mut self, end_location: Location<'a>) {
        self.set_end_location(Some(Rc::new(end_location)));
    }

    fn copy_end_location(&mut self, node: AstNodeBox<'a>) {
        self.set_end_location(node.end_location());
    }
}

macro_rules! Node {
    ($name:ident) => {
        Node!($name;);
    };

    ($name:ident; $(pub $field:ident: $typ:ty,)*) => {
        pub struct $name<'a> {
            location: Option<Rc<Location<'a>>>,
            end_location: Option<Rc<Location<'a>>>,
            $(pub $field: $typ),*
        }

        impl<'a> AstNode<'a> for $name<'a> {
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

Node!(Nop);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ExpressionsKeyword {
    None,
    Paren,
    Begin,
}

Node!(
    Expressions;
    pub expressions: Vec<AstNodeBox<'a>>,
    pub keyword: ExpressionsKeyword,
);

impl<'a> Expressions<'a> {
    pub fn default() -> Box<Self> {
        Self::new(vec![], ExpressionsKeyword::None)
    }

    pub fn from<T>(obj: T) -> AstNodeBox<'a>
    where
        T: IntoExpressions<'a>,
    {
        obj.into()
    }
}

pub trait IntoExpressions<'a> {
    fn into(self) -> AstNodeBox<'a>;
}

impl<'a, T> IntoExpressions<'a> for Option<T>
where
    T: IntoExpressions<'a>,
{
    fn into(self) -> AstNodeBox<'a> {
        if let Some(obj) = self {
            obj.into()
        } else {
            Nop::new()
        }
    }
}

impl<'a> IntoExpressions<'a> for Vec<AstNodeBox<'a>> {
    fn into(mut self) -> AstNodeBox<'a> {
        match self.len() {
            0 => Nop::new(),
            1 => self.swap_remove(0), // TODO: why doesn't self[0] work?
            _ => Expressions::new(self, ExpressionsKeyword::None),
        }
    }
}

impl<'a> IntoExpressions<'a> for AstNodeBox<'a> {
    fn into(self) -> AstNodeBox<'a> {
        self
    }
}

Node!(NilLiteral);

Node!(
    BoolLiteral;
    pub value: bool,
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

Node!(
    NumberLiteral;
    pub value: Vec<char>,
    pub kind: NumberKind,
);

Node!(
    CharLiteral;
    pub value: char,
);

Node!(
    StringLiteral;
    pub value: Vec<char>,
);

Node!(
    StringInterpolation;
    pub expressions: Vec<AstNodeBox<'a>>,
    pub heredoc_indent: usize,
);

Node!(
    SymbolLiteral;
    pub value: Vec<char>,
);

Node!(
    ArrayLiteral;
    pub elements: Vec<AstNodeBox<'a>>,
    pub of: Option<AstNodeBox<'a>>,
    pub name: Option<AstNodeBox<'a>>,
);

Node!(
    HashLiteral;
    pub elements: Vec<HashLiteralEntry<'a>>,
    pub of: Option<HashLiteralEntry<'a>>,
    pub name: Option<AstNodeBox<'a>>,
);

pub struct HashLiteralEntry<'a> {
    pub key: AstNodeBox<'a>,
    pub value: AstNodeBox<'a>,
}

Node!(
    NamedTupleLiteral;
    pub entries: Vec<NamedTupleLiteralEntry<'a>>,
);

pub struct NamedTupleLiteralEntry<'a> {
    pub key: Vec<char>,
    pub value: AstNodeBox<'a>,
}

Node!(
    RangeLiteral;
    pub from: AstNodeBox<'a>,
    pub to: AstNodeBox<'a>,
    pub exclusive: bool,
);

Node!(
    RegexLiteral;
    pub value: AstNodeBox<'a>,
    // options
);

Node!(
    TupleLiteral;
    pub elements: Vec<AstNodeBox<'a>>,
);

Node!(
    Var;
    pub name: Vec<char>,
);

Node!(
    Block;
    pub args: Vec<Box<Var<'a>>>,
    pub body: AstNodeBox<'a>,
    pub call: Option<Box<Call<'a>>>,
    pub splat_index: Option<usize>,
);

Node!(
    Call;
    pub obj: Option<AstNodeBox<'a>>,
    pub name: Vec<char>,
    pub args: Vec<AstNodeBox<'a>>,
    pub block: Option<Box<Block<'a>>>,
    pub block_arg: Option<AstNodeBox<'a>>,
    pub named_args: Option<Vec<Box<NamedArgument<'a>>>>,
    pub name_location: Option<Location<'a>>,
    // name_size
    pub doc: Option<Vec<char>>,
    pub visibility: Visibility,
    pub global: bool,
    pub expansion: bool,
    pub has_parentheses: bool,
);

Node!(
    NamedArgument;
    pub name: Vec<char>,
    pub value: AstNodeBox<'a>,
);

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
