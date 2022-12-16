use crate::{location::Location, token::TokenKind};
use std::{fmt, rc::Rc};

pub type AstNodeBox<'f> = Box<dyn AstNode<'f> + 'f>;

pub trait AstNode<'f>: fmt::Debug {
    fn location(&self) -> Option<Rc<Location<'f>>>;
    fn set_location(&mut self, location: Option<Rc<Location<'f>>>);

    fn end_location(&self) -> Option<Rc<Location<'f>>>;
    fn set_end_location(&mut self, end_location: Option<Rc<Location<'f>>>);

    fn at(&mut self, location: Location<'f>) {
        self.set_location(Some(Rc::new(location)));
    }

    fn at_node(&'_ mut self, node: &'_ dyn AstNode<'f>) {
        self.set_location(node.location());
        self.set_end_location(node.end_location());
    }

    fn at_end(&mut self, end_location: Location<'f>) {
        self.set_end_location(Some(Rc::new(end_location)));
    }

    fn at_node_end(&'_ mut self, node: &'_ dyn AstNode<'f>) {
        self.set_end_location(node.end_location());
    }

    fn tag(&self) -> AstNodeTag;

    fn is_control_expression(&self) -> bool {
        CONTROL_EXPRESSIONS.contains(&self.tag())
    }

    fn to_box_enum(self: Box<Self>) -> AstNodeBoxEnum<'f>;
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AstNodeTag {
    Nop,
    Expressions,
    NilLiteral,
    BoolLiteral,
    NumberLiteral,
    CharLiteral,
    StringLiteral,
    StringInterpolation,
    SymbolLiteral,
    ArrayLiteral,
    HashLiteral,
    NamedTupleLiteral,
    RangeLiteral,
    RegexLiteral,
    TupleLiteral,
    Var,
    Block,
    Call,
    NamedArgument,
    If,
    Unless,
    Assign,
    OpAssign,
    MultiAssign,
    InstanceVar,
    ReadInstanceVar,
    ClassVar,
    Global,
    And,
    Or,
    Arg,
    ProcNotation,
    Def,
    Macro,
    Not,
    PointerOf,
    SizeOf,
    InstanceSizeOf,
    Out,
    OffsetOf,
    VisibilityModifier,
    IsA,
    RespondsTo,
    Require,
    When,
    Case,
    Select,
    ImplicitObj,
    Path,
    ClassDef,
    ModuleDef,
    AnnotationDef,
    While,
    Until,
    Generic,
    TypeDeclaration,
    UninitializedVar,
    Rescue,
    ExceptionHandler,
    ProcLiteral,
    ProcPointer,
    Union,
    Self_,
    Return,
    Break,
    Next,
    Yield,
    Include,
    Extend,
    LibDef,
    FunDef,
    TypeDef,
    CStructOrUnionDef,
    EnumDef,
    ExternalVar,
    Alias,
    Metaclass,
    Cast,
    NilableCast,
    TypeOf,
    Annotation,
    MacroExpression,
    MacroLiteral,
    MacroVerbatim,
    MacroIf,
    MacroFor,
    MacroVar,
    Underscore,
    Splat,
    DoubleSplat,
    MagicConstant,
    Asm,
    AsmOperand,
}

#[derive(Debug)]
pub enum AstNodeBoxEnum<'f> {
    Nop(Box<Nop<'f>>),
    Expressions(Box<Expressions<'f>>),
    NilLiteral(Box<NilLiteral<'f>>),
    BoolLiteral(Box<BoolLiteral<'f>>),
    NumberLiteral(Box<NumberLiteral<'f>>),
    CharLiteral(Box<CharLiteral<'f>>),
    StringLiteral(Box<StringLiteral<'f>>),
    StringInterpolation(Box<StringInterpolation<'f>>),
    SymbolLiteral(Box<SymbolLiteral<'f>>),
    ArrayLiteral(Box<ArrayLiteral<'f>>),
    HashLiteral(Box<HashLiteral<'f>>),
    NamedTupleLiteral(Box<NamedTupleLiteral<'f>>),
    RangeLiteral(Box<RangeLiteral<'f>>),
    RegexLiteral(Box<RegexLiteral<'f>>),
    TupleLiteral(Box<TupleLiteral<'f>>),
    Var(Box<Var<'f>>),
    Block(Box<Block<'f>>),
    Call(Box<Call<'f>>),
    NamedArgument(Box<NamedArgument<'f>>),
    If(Box<If<'f>>),
    Unless(Box<Unless<'f>>),
    Assign(Box<Assign<'f>>),
    OpAssign(Box<OpAssign<'f>>),
    MultiAssign(Box<MultiAssign<'f>>),
    InstanceVar(Box<InstanceVar<'f>>),
    ReadInstanceVar(Box<ReadInstanceVar<'f>>),
    ClassVar(Box<ClassVar<'f>>),
    Global(Box<Global<'f>>),
    And(Box<And<'f>>),
    Or(Box<Or<'f>>),
    Arg(Box<Arg<'f>>),
    ProcNotation(Box<ProcNotation<'f>>),
    Def(Box<Def<'f>>),
    Macro(Box<Macro<'f>>),
    Not(Box<Not<'f>>),
    PointerOf(Box<PointerOf<'f>>),
    SizeOf(Box<SizeOf<'f>>),
    InstanceSizeOf(Box<InstanceSizeOf<'f>>),
    Out(Box<Out<'f>>),
    OffsetOf(Box<OffsetOf<'f>>),
    VisibilityModifier(Box<VisibilityModifier<'f>>),
    IsA(Box<IsA<'f>>),
    RespondsTo(Box<RespondsTo<'f>>),
    Require(Box<Require<'f>>),
    When(Box<When<'f>>),
    Case(Box<Case<'f>>),
    Select(Box<Select<'f>>),
    ImplicitObj(Box<ImplicitObj<'f>>),
    Path(Box<Path<'f>>),
    ClassDef(Box<ClassDef<'f>>),
    ModuleDef(Box<ModuleDef<'f>>),
    AnnotationDef(Box<AnnotationDef<'f>>),
    While(Box<While<'f>>),
    Until(Box<Until<'f>>),
    Generic(Box<Generic<'f>>),
    TypeDeclaration(Box<TypeDeclaration<'f>>),
    UninitializedVar(Box<UninitializedVar<'f>>),
    Rescue(Box<Rescue<'f>>),
    ExceptionHandler(Box<ExceptionHandler<'f>>),
    ProcLiteral(Box<ProcLiteral<'f>>),
    ProcPointer(Box<ProcPointer<'f>>),
    Union(Box<Union<'f>>),
    Self_(Box<Self_<'f>>),
    Return(Box<Return<'f>>),
    Break(Box<Break<'f>>),
    Next(Box<Next<'f>>),
    Yield(Box<Yield<'f>>),
    Include(Box<Include<'f>>),
    Extend(Box<Extend<'f>>),
    LibDef(Box<LibDef<'f>>),
    FunDef(Box<FunDef<'f>>),
    TypeDef(Box<TypeDef<'f>>),
    CStructOrUnionDef(Box<CStructOrUnionDef<'f>>),
    EnumDef(Box<EnumDef<'f>>),
    ExternalVar(Box<ExternalVar<'f>>),
    Alias(Box<Alias<'f>>),
    Metaclass(Box<Metaclass<'f>>),
    Cast(Box<Cast<'f>>),
    NilableCast(Box<NilableCast<'f>>),
    TypeOf(Box<TypeOf<'f>>),
    Annotation(Box<Annotation<'f>>),
    MacroExpression(Box<MacroExpression<'f>>),
    MacroLiteral(Box<MacroLiteral<'f>>),
    MacroVerbatim(Box<MacroVerbatim<'f>>),
    MacroIf(Box<MacroIf<'f>>),
    MacroFor(Box<MacroFor<'f>>),
    MacroVar(Box<MacroVar<'f>>),
    Underscore(Box<Underscore<'f>>),
    Splat(Box<Splat<'f>>),
    DoubleSplat(Box<DoubleSplat<'f>>),
    MagicConstant(Box<MagicConstant<'f>>),
    Asm(Box<Asm<'f>>),
    AsmOperand(Box<AsmOperand<'f>>),
}

macro_rules! Node {
    ($name:ident) => {
        Node!($name;);
    };

    ($name:ident; $(pub $field:ident: $typ:ty,)*) => {
        #[derive(Debug)]
        pub struct $name<'f> {
            location: Option<Rc<Location<'f>>>,
            end_location: Option<Rc<Location<'f>>>,
            $(pub $field: $typ),*
        }

        impl<'f> AstNode<'f> for $name<'f> {
            fn location(&self) -> Option<Rc<Location<'f>>> {
                self.location.clone()
            }

            fn set_location(&mut self, location: Option<Rc<Location<'f>>>) {
                self.location = location;
            }

            fn end_location(&self) -> Option<Rc<Location<'f>>> {
                self.end_location.clone()
            }

            fn set_end_location(&mut self, end_location: Option<Rc<Location<'f>>>) {
                self.end_location = end_location;
            }

            fn tag(&self) -> AstNodeTag {
                AstNodeTag::$name
            }

            fn to_box_enum(self: Box<Self>) -> AstNodeBoxEnum<'f> {
                AstNodeBoxEnum::$name(self)
            }
        }

        // impl<'f> $name<'f> {
        //     pub fn new($($field: $typ),*) -> Box<Self> {
        //         Box::new(Self {
        //             location: None,
        //             end_location: None,
        //             $($field),*
        //         })
        //     }
        // }
    };
}

macro_rules! new_node {
    ($($field:ident: $value:expr,)*) => {
        Box::new(Self {
            location: None,
            end_location: None,
            $($field: $value),*
        })
    };
}

Node!(Nop);

impl<'f> Nop<'f> {
    pub fn new() -> Box<Self> {
        new_node!()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ExpressionsKeyword {
    None,
    Paren,
    Begin,
}

Node!(
    Expressions;
    pub expressions: Vec<AstNodeBox<'f>>,
    pub keyword: ExpressionsKeyword,
);

impl<'f> Expressions<'f> {
    pub fn new(expressions: Vec<AstNodeBox<'f>>) -> Box<Self> {
        new_node! {
            expressions: expressions,
            keyword: ExpressionsKeyword::None,
        }
    }

    pub fn from<T>(obj: T) -> AstNodeBox<'f>
    where
        T: IntoExpressions<'f>,
    {
        obj.into()
    }
}

pub trait IntoExpressions<'f> {
    fn into(self) -> AstNodeBox<'f>;
}

impl<'f, T> IntoExpressions<'f> for Option<T>
where
    T: IntoExpressions<'f>,
{
    fn into(self) -> AstNodeBox<'f> {
        if let Some(obj) = self {
            obj.into()
        } else {
            Nop::new()
        }
    }
}

impl<'f> IntoExpressions<'f> for Vec<AstNodeBox<'f>> {
    fn into(mut self) -> AstNodeBox<'f> {
        match self.len() {
            0 => Nop::new(),
            1 => self.swap_remove(0), // TODO: why doesn't self[0] work?
            _ => Expressions::new(self),
        }
    }
}

impl<'f> IntoExpressions<'f> for AstNodeBox<'f> {
    fn into(self) -> AstNodeBox<'f> {
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
    pub expressions: Vec<AstNodeBox<'f>>,
    pub heredoc_indent: usize,
);

Node!(
    SymbolLiteral;
    pub value: Vec<char>,
);

Node!(
    ArrayLiteral;
    pub elements: Vec<AstNodeBox<'f>>,
    pub of: Option<AstNodeBox<'f>>,
    pub name: Option<AstNodeBox<'f>>,
);

Node!(
    HashLiteral;
    pub elements: Vec<HashLiteralEntry<'f>>,
    pub of: Option<HashLiteralEntry<'f>>,
    pub name: Option<AstNodeBox<'f>>,
);

#[derive(Debug)]
pub struct HashLiteralEntry<'f> {
    pub key: AstNodeBox<'f>,
    pub value: AstNodeBox<'f>,
}

Node!(
    NamedTupleLiteral;
    pub entries: Vec<NamedTupleLiteralEntry<'f>>,
);

#[derive(Debug)]
pub struct NamedTupleLiteralEntry<'f> {
    pub key: Vec<char>,
    pub value: AstNodeBox<'f>,
}

Node!(
    RangeLiteral;
    pub from: AstNodeBox<'f>,
    pub to: AstNodeBox<'f>,
    pub exclusive: bool,
);

Node!(
    RegexLiteral;
    pub value: AstNodeBox<'f>,
    // options
);

Node!(
    TupleLiteral;
    pub elements: Vec<AstNodeBox<'f>>,
);

Node!(
    Var;
    pub name: Vec<char>,
);

Node!(
    Block;
    pub args: Vec<Box<Var<'f>>>,
    pub body: AstNodeBox<'f>,
    pub call: Option<Box<Call<'f>>>,
    pub splat_index: Option<usize>,
);

Node!(
    Call;
    pub obj: Option<AstNodeBox<'f>>,
    pub name: Vec<char>,
    pub args: Vec<AstNodeBox<'f>>,
    pub block: Option<Box<Block<'f>>>,
    pub block_arg: Option<AstNodeBox<'f>>,
    pub named_args: Option<Vec<Box<NamedArgument<'f>>>>,
    pub name_location: Option<Location<'f>>,
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
    pub value: AstNodeBox<'f>,
);

Node!(
    If;
    pub cond: AstNodeBox<'f>,
    pub then: AstNodeBox<'f>,
    pub else_: AstNodeBox<'f>,
    pub ternary: bool,
    pub else_location: Option<Location<'f>>,
);

Node!(
    Unless;
    pub cond: AstNodeBox<'f>,
    pub then: AstNodeBox<'f>,
    pub else_: AstNodeBox<'f>,
);

Node!(
    Assign;
    pub target: AstNodeBox<'f>,
    pub value: AstNodeBox<'f>,
    pub doc: Option<Vec<char>>,
);

Node!(
    OpAssign;
    pub target: AstNodeBox<'f>,
    pub op: Vec<char>,
    pub value: AstNodeBox<'f>,
    pub name_location: Option<Location<'f>>,
);

Node!(
    MultiAssign;
    pub targets: Vec<AstNodeBox<'f>>,
    pub values: Vec<AstNodeBox<'f>>,
);

Node!(
    InstanceVar;
    pub name: Vec<char>,
);

Node!(
    ReadInstanceVar;
    pub obj: AstNodeBox<'f>,
    pub name: Vec<char>,
);

Node!(
    ClassVar;
    pub name: Vec<char>,
);

Node!(
    Global;
    pub name: Vec<char>,
);

macro_rules! BinaryOp {
    ($name:ident) => {
        Node!(
            $name;
            pub left: AstNodeBox<'f>,
            pub right: AstNodeBox<'f>,
        );
    };
}

BinaryOp!(And);

BinaryOp!(Or);

Node!(
    Arg;
    pub name: Vec<char>,
    pub external_name: Vec<char>,
    pub default_value: Option<AstNodeBox<'f>>,
    pub restriction: Option<AstNodeBox<'f>>,
    pub doc: Option<Vec<char>>,
    pub parsed_annotations: Option<Vec<Annotation<'f>>>,
);

Node!(
    ProcNotation;
    pub inputs: Option<Vec<AstNodeBox<'f>>>,
    pub output: Option<AstNodeBox<'f>>,
);

Node!(
    Def;
    pub free_vars: Option<Vec<Vec<char>>>,
    pub receiver: Option<AstNodeBox<'f>>,
    pub name: Vec<char>,
    pub args: Vec<Box<Arg<'f>>>,
    pub double_splat: Option<Box<Arg<'f>>>,
    pub body: AstNodeBox<'f>,
    pub block_arg: Option<Box<Arg<'f>>>,
    pub return_type: Option<AstNodeBox<'f>>,
    pub block_arity: Option<usize>,
    pub name_location: Option<Location<'f>>,
    pub splat_index: Option<usize>,
    pub doc: Option<Vec<char>>,
    pub visibility: Visibility,

    pub macro_def: bool,
    pub calls_super: bool,
    pub calls_initialize: bool,
    pub calls_previous_def: bool,
    pub uses_block_arg: bool,
    pub assigns_special_var: bool,
    pub abstract_: bool,
);

Node!(
    Macro;
    pub name: Vec<char>,
    pub args: Vec<Box<Arg<'f>>>,
    pub body: AstNodeBox<'f>,
    pub double_splat: Option<Box<Arg<'f>>>,
    pub block_arg: Option<Box<Arg<'f>>>,
    pub name_location: Option<Location<'f>>,
    pub splat_index: Option<usize>,
    pub doc: Option<Vec<char>>,
    pub visibility: Visibility,
);

macro_rules! UnaryExpression {
    ($name:ident) => {
        Node!(
            $name;
            pub exp: AstNodeBox<'f>,
        );
    };
}

UnaryExpression!(Not);

UnaryExpression!(PointerOf);

UnaryExpression!(SizeOf);

UnaryExpression!(InstanceSizeOf);

UnaryExpression!(Out);

Node!(
    OffsetOf;
    pub offsetof_type: AstNodeBox<'f>,
    pub offset: AstNodeBox<'f>,
);

Node!(
    VisibilityModifier;
    pub modifier: Visibility,
    pub exp: AstNodeBox<'f>,
    pub doc: Option<Vec<char>>,
);

Node!(
    IsA;
    pub obj: AstNodeBox<'f>,
    pub const_: AstNodeBox<'f>,
    pub nil_check: bool,
);

Node!(
    RespondsTo;
    pub obj: AstNodeBox<'f>,
    pub name: Vec<char>,
);

Node!(
    Require;
    pub string: Vec<char>,
);

Node!(
    When;
    pub conds: Vec<AstNodeBox<'f>>,
    pub body: AstNodeBox<'f>,
    pub exhaustive: bool,
);

Node!(
    Case;
    pub cond: Option<AstNodeBox<'f>>,
    pub whens: Vec<Box<When<'f>>>,
    pub else_: Option<AstNodeBox<'f>>,
    pub exhaustive: bool,
);

#[derive(Debug)]
pub struct SelectWhen<'f> {
    pub condition: AstNodeBox<'f>,
    pub body: AstNodeBox<'f>,
}

impl<'f> SelectWhen<'f> {
    pub fn new(condition: AstNodeBox<'f>, body: AstNodeBox<'f>) -> Self {
        Self { condition, body }
    }
}

Node!(
    Select;
    pub whens: Vec<SelectWhen<'f>>,
    pub else_: Option<AstNodeBox<'f>>,
);

Node!(ImplicitObj);

Node!(
    Path;
    pub names: Vec<Vec<char>>,
    pub global: bool,
    pub visibility: Visibility,
);

Node!(
    ClassDef;
    pub name: Box<Path<'f>>,
    pub body: AstNodeBox<'f>,
    pub superclass: Option<AstNodeBox<'f>>,
    pub type_vars: Option<Vec<Vec<char>>>,
    pub name_location: Option<Location<'f>>,
    pub doc: Option<Vec<char>>,
    pub splat_index: Option<usize>,
    pub abstract_: bool,
    pub struct_: bool,
    pub visibility: Visibility,
);

Node!(
    ModuleDef;
    pub name: Box<Path<'f>>,
    pub body: AstNodeBox<'f>,
    pub type_vars: Option<Vec<Vec<char>>>,
    pub splat_index: Option<usize>,
    pub name_location: Option<Location<'f>>,
    pub doc: Option<Vec<char>>,
    pub visibility: Visibility,
);

Node!(
    AnnotationDef;
    pub name: Box<Path<'f>>,
    pub doc: Option<Vec<char>>,
    pub name_location: Option<Location<'f>>,
);

Node!(
    While;
    pub cond: AstNodeBox<'f>,
    pub body: AstNodeBox<'f>,
);

Node!(
    Until;
    pub cond: AstNodeBox<'f>,
    pub body: AstNodeBox<'f>,
);

Node!(
    Generic;
    pub name: AstNodeBox<'f>,
    pub type_vars: Vec<AstNodeBox<'f>>,
    pub named_args: Option<Vec<Box<NamedArgument<'f>>>>,
    pub suffix: GenericSuffix,
);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum GenericSuffix {
    None,
    Question,
    Asterisk,
    Bracket,
}

Node!(
    TypeDeclaration;
    pub var: AstNodeBox<'f>,
    pub declared_type: AstNodeBox<'f>,
    pub value: Option<AstNodeBox<'f>>,
);

Node!(
    UninitializedVar;
    pub var: AstNodeBox<'f>,
    pub declared_type: AstNodeBox<'f>,
);

Node!(
    Rescue;
    pub body: AstNodeBox<'f>,
    pub types: Option<Vec<AstNodeBox<'f>>>,
    pub name: Option<Vec<char>>,
);

Node!(
    ExceptionHandler;
    pub body: AstNodeBox<'f>,
    pub rescues: Option<Vec<Box<Rescue<'f>>>>,
    pub else_: Option<AstNodeBox<'f>>,
    pub ensure: Option<AstNodeBox<'f>>,
    pub implicit: bool,
    pub suffix: bool,
    pub else_location: Option<Location<'f>>,
    pub ensure_location: Option<Location<'f>>,
);

Node!(
    ProcLiteral;
    pub def: Box<Def<'f>>,
);

Node!(
    ProcPointer;
    pub obj: Option<AstNodeBox<'f>>,
    pub name: Vec<char>,
    pub args: Vec<AstNodeBox<'f>>,
    pub global: bool,
);

Node!(
    Union;
    pub types: Vec<AstNodeBox<'f>>,
);

Node!(
    Self_;
);

macro_rules! ControlExpressions {
    ($($name:ident;)+) => {
        $(Node!(
            $name;
            pub exp: Option<AstNodeBox<'f>>,
        );)+

        const CONTROL_EXPRESSIONS: &[AstNodeTag] = &[$(AstNodeTag::$name),+];
    };
}

ControlExpressions!(
    Return;
    Break;
    Next;
);

Node!(
    Yield;
    pub exps: Vec<AstNodeBox<'f>>,
    pub scope: Option<AstNodeBox<'f>>,
    pub has_parentheses: bool,
);

Node!(
    Include;
    pub name: AstNodeBox<'f>,
);

Node!(
    Extend;
    pub name: AstNodeBox<'f>,
);

Node!(
    LibDef;
    pub name: Vec<char>,
    pub body: AstNodeBox<'f>,
    pub name_location: Option<Location<'f>>,
    pub visibility: Visibility,
);

Node!(
    FunDef;
    pub name: Vec<char>,
    pub args: Vec<Box<Arg<'f>>>,
    pub return_type: Option<AstNodeBox<'f>>,
    pub body: Option<AstNodeBox<'f>>,
    pub real_name: Vec<char>,
    pub doc: Option<Vec<char>>,
    pub varargs: bool,
);

Node!(
    TypeDef;
    pub name: Vec<char>,
    pub type_spec: AstNodeBox<'f>,
    pub name_location: Option<Location<'f>>,
);

Node!(
    CStructOrUnionDef;
    pub name: Vec<char>,
    pub body: AstNodeBox<'f>,
    pub union: bool,
);

Node!(
    EnumDef;
    pub name: Box<Path<'f>>,
    pub members: Vec<AstNodeBox<'f>>,
    pub base_type: Option<AstNodeBox<'f>>,
    pub doc: Option<Vec<char>>,
    pub visibility: Visibility,
);

Node!(
    ExternalVar;
    pub name: Vec<char>,
    pub type_spec: AstNodeBox<'f>,
    pub real_name: Option<Vec<char>>,
);

Node!(
    Alias;
    pub name: Box<Path<'f>>,
    pub value: AstNodeBox<'f>,
    pub doc: Option<Vec<char>>,
    pub visibility: Visibility,
);

Node!(
    Metaclass;
    pub name: AstNodeBox<'f>,
);

Node!(
    Cast;
    pub obj: AstNodeBox<'f>,
    pub to: AstNodeBox<'f>,
);

Node!(
    NilableCast;
    // TODO
);

Node!(
    TypeOf;
    pub expressions: Vec<AstNodeBox<'f>>,
);

Node!(
    Annotation;
    pub path: Box<Path<'f>>,
    pub args: Vec<AstNodeBox<'f>>,
    pub named_args: Option<Vec<Box<NamedArgument<'f>>>>,
    pub doc: Option<Vec<char>>,
);

Node!(
    MacroExpression;
    pub exp: AstNodeBox<'f>,
    pub output: bool,
);

Node!(
    MacroLiteral;
    pub value: Vec<char>,
);

UnaryExpression!(MacroVerbatim);

Node!(
    MacroIf;
    pub cond: AstNodeBox<'f>,
    pub then: AstNodeBox<'f>,
    pub else_: AstNodeBox<'f>,
);

Node!(
    MacroFor;
    pub vars: Vec<Box<Var<'f>>>,
    pub exp: AstNodeBox<'f>,
    pub body: AstNodeBox<'f>,
);

Node!(
    MacroVar;
    pub name: Vec<char>,
    pub exps: Option<Vec<AstNodeBox<'f>>>,
);

Node!(
    Underscore;
);

UnaryExpression!(Splat);

UnaryExpression!(DoubleSplat);

Node!(
    MagicConstant;
    pub name: TokenKind,
);

Node!(
    Asm;
    pub text: Vec<char>,
    pub outputs: Option<Vec<Box<AsmOperand<'f>>>>,
    pub inputs: Option<Vec<Box<AsmOperand<'f>>>>,
    pub clobbers: Option<Vec<Vec<char>>>,
    pub volatile: bool,
    pub alignstack: bool,
    pub intel: bool,
    pub can_throw: bool,
);

Node!(
    AsmOperand;
    pub constraint: Vec<char>,
    pub exp: AstNodeBox<'f>,
);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Visibility {
    Public,
    Protected,
    Private,
}

#[test]
fn it_works() {
    let mut node = Nop::new();
    node.at(Location::new("foo", 12, 34));
    node.at_node_end(Nop::new().as_ref());

    let node: AstNodeBox = Nop::new();
    assert_eq!(node.tag(), AstNodeTag::Nop);
    let node: Box<Nop> = Nop::new();
    assert_eq!(node.tag(), AstNodeTag::Nop);

    // let node: AstNodeBox = Return::new(None);
    // assert!(node.is_control_expression());
}
