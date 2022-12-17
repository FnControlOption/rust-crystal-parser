use crate::{location::Location, token::TokenKind};
use std::{fmt, rc::Rc};

pub type AstNodeBox<'f> = Box<dyn AstNode<'f> + 'f>;

pub type AstNodeRef<'a, 'f> = &'a dyn AstNode<'f>;

pub trait AstNode<'f>: fmt::Debug {
    fn location(&self) -> Option<Rc<Location<'f>>>;
    fn set_location(&mut self, location: Option<Rc<Location<'f>>>);

    fn end_location(&self) -> Option<Rc<Location<'f>>>;
    fn set_end_location(&mut self, end_location: Option<Rc<Location<'f>>>);

    fn tag(&self) -> AstTag;

    fn is_control_expression(&self) -> bool {
        CONTROL_EXPRESSIONS.contains(&self.tag())
    }

    fn to_box(self: Box<Self>) -> AstBox<'f>;

    fn to_ref<'a>(&'a self) -> AstRef<'a, 'f>;
}

pub trait At<'f, L> {
    fn at(&mut self, location: L);
    fn at_end(&mut self, location: L);
}

impl<'f, T> At<'f, Location<'f>> for T
where
    T: AstNode<'f>,
{
    fn at(&mut self, location: Location<'f>) {
        self.set_location(Some(Rc::new(location)));
    }

    fn at_end(&mut self, location: Location<'f>) {
        self.set_location(Some(Rc::new(location)));
    }
}

// impl<'f, T> At<'f, AstNodeRef<'_, 'f>> for T
// where
//     T: AstNode<'f>,
// {
//     fn at(&mut self, node: AstNodeRef<'_, 'f>) {
//         self.set_location(node.location());
//         self.set_end_location(node.end_location());
//     }

//     fn at_end(&mut self, node: AstNodeRef<'_, 'f>) {
//         self.set_end_location(node.end_location());
//     }
// }

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AstTag {
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
pub enum AstBox<'f> {
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

#[derive(Debug)]
pub enum AstRef<'a, 'f> {
    Nop(&'a Nop<'f>),
    Expressions(&'a Expressions<'f>),
    NilLiteral(&'a NilLiteral<'f>),
    BoolLiteral(&'a BoolLiteral<'f>),
    NumberLiteral(&'a NumberLiteral<'f>),
    CharLiteral(&'a CharLiteral<'f>),
    StringLiteral(&'a StringLiteral<'f>),
    StringInterpolation(&'a StringInterpolation<'f>),
    SymbolLiteral(&'a SymbolLiteral<'f>),
    ArrayLiteral(&'a ArrayLiteral<'f>),
    HashLiteral(&'a HashLiteral<'f>),
    NamedTupleLiteral(&'a NamedTupleLiteral<'f>),
    RangeLiteral(&'a RangeLiteral<'f>),
    RegexLiteral(&'a RegexLiteral<'f>),
    TupleLiteral(&'a TupleLiteral<'f>),
    Var(&'a Var<'f>),
    Block(&'a Block<'f>),
    Call(&'a Call<'f>),
    NamedArgument(&'a NamedArgument<'f>),
    If(&'a If<'f>),
    Unless(&'a Unless<'f>),
    Assign(&'a Assign<'f>),
    OpAssign(&'a OpAssign<'f>),
    MultiAssign(&'a MultiAssign<'f>),
    InstanceVar(&'a InstanceVar<'f>),
    ReadInstanceVar(&'a ReadInstanceVar<'f>),
    ClassVar(&'a ClassVar<'f>),
    Global(&'a Global<'f>),
    And(&'a And<'f>),
    Or(&'a Or<'f>),
    Arg(&'a Arg<'f>),
    ProcNotation(&'a ProcNotation<'f>),
    Def(&'a Def<'f>),
    Macro(&'a Macro<'f>),
    Not(&'a Not<'f>),
    PointerOf(&'a PointerOf<'f>),
    SizeOf(&'a SizeOf<'f>),
    InstanceSizeOf(&'a InstanceSizeOf<'f>),
    Out(&'a Out<'f>),
    OffsetOf(&'a OffsetOf<'f>),
    VisibilityModifier(&'a VisibilityModifier<'f>),
    IsA(&'a IsA<'f>),
    RespondsTo(&'a RespondsTo<'f>),
    Require(&'a Require<'f>),
    When(&'a When<'f>),
    Case(&'a Case<'f>),
    Select(&'a Select<'f>),
    ImplicitObj(&'a ImplicitObj<'f>),
    Path(&'a Path<'f>),
    ClassDef(&'a ClassDef<'f>),
    ModuleDef(&'a ModuleDef<'f>),
    AnnotationDef(&'a AnnotationDef<'f>),
    While(&'a While<'f>),
    Until(&'a Until<'f>),
    Generic(&'a Generic<'f>),
    TypeDeclaration(&'a TypeDeclaration<'f>),
    UninitializedVar(&'a UninitializedVar<'f>),
    Rescue(&'a Rescue<'f>),
    ExceptionHandler(&'a ExceptionHandler<'f>),
    ProcLiteral(&'a ProcLiteral<'f>),
    ProcPointer(&'a ProcPointer<'f>),
    Union(&'a Union<'f>),
    Self_(&'a Self_<'f>),
    Return(&'a Return<'f>),
    Break(&'a Break<'f>),
    Next(&'a Next<'f>),
    Yield(&'a Yield<'f>),
    Include(&'a Include<'f>),
    Extend(&'a Extend<'f>),
    LibDef(&'a LibDef<'f>),
    FunDef(&'a FunDef<'f>),
    TypeDef(&'a TypeDef<'f>),
    CStructOrUnionDef(&'a CStructOrUnionDef<'f>),
    EnumDef(&'a EnumDef<'f>),
    ExternalVar(&'a ExternalVar<'f>),
    Alias(&'a Alias<'f>),
    Metaclass(&'a Metaclass<'f>),
    Cast(&'a Cast<'f>),
    NilableCast(&'a NilableCast<'f>),
    TypeOf(&'a TypeOf<'f>),
    Annotation(&'a Annotation<'f>),
    MacroExpression(&'a MacroExpression<'f>),
    MacroLiteral(&'a MacroLiteral<'f>),
    MacroVerbatim(&'a MacroVerbatim<'f>),
    MacroIf(&'a MacroIf<'f>),
    MacroFor(&'a MacroFor<'f>),
    MacroVar(&'a MacroVar<'f>),
    Underscore(&'a Underscore<'f>),
    Splat(&'a Splat<'f>),
    DoubleSplat(&'a DoubleSplat<'f>),
    MagicConstant(&'a MagicConstant<'f>),
    Asm(&'a Asm<'f>),
    AsmOperand(&'a AsmOperand<'f>),
}

impl<'f> From<AstBox<'f>> for AstNodeBox<'f> {
    fn from(value: AstBox<'f>) -> Self {
        match value {
            AstBox::Nop(node) => node,
            AstBox::Expressions(node) => node,
            AstBox::NilLiteral(node) => node,
            AstBox::BoolLiteral(node) => node,
            AstBox::NumberLiteral(node) => node,
            AstBox::CharLiteral(node) => node,
            AstBox::StringLiteral(node) => node,
            AstBox::StringInterpolation(node) => node,
            AstBox::SymbolLiteral(node) => node,
            AstBox::ArrayLiteral(node) => node,
            AstBox::HashLiteral(node) => node,
            AstBox::NamedTupleLiteral(node) => node,
            AstBox::RangeLiteral(node) => node,
            AstBox::RegexLiteral(node) => node,
            AstBox::TupleLiteral(node) => node,
            AstBox::Var(node) => node,
            AstBox::Block(node) => node,
            AstBox::Call(node) => node,
            AstBox::NamedArgument(node) => node,
            AstBox::If(node) => node,
            AstBox::Unless(node) => node,
            AstBox::Assign(node) => node,
            AstBox::OpAssign(node) => node,
            AstBox::MultiAssign(node) => node,
            AstBox::InstanceVar(node) => node,
            AstBox::ReadInstanceVar(node) => node,
            AstBox::ClassVar(node) => node,
            AstBox::Global(node) => node,
            AstBox::And(node) => node,
            AstBox::Or(node) => node,
            AstBox::Arg(node) => node,
            AstBox::ProcNotation(node) => node,
            AstBox::Def(node) => node,
            AstBox::Macro(node) => node,
            AstBox::Not(node) => node,
            AstBox::PointerOf(node) => node,
            AstBox::SizeOf(node) => node,
            AstBox::InstanceSizeOf(node) => node,
            AstBox::Out(node) => node,
            AstBox::OffsetOf(node) => node,
            AstBox::VisibilityModifier(node) => node,
            AstBox::IsA(node) => node,
            AstBox::RespondsTo(node) => node,
            AstBox::Require(node) => node,
            AstBox::When(node) => node,
            AstBox::Case(node) => node,
            AstBox::Select(node) => node,
            AstBox::ImplicitObj(node) => node,
            AstBox::Path(node) => node,
            AstBox::ClassDef(node) => node,
            AstBox::ModuleDef(node) => node,
            AstBox::AnnotationDef(node) => node,
            AstBox::While(node) => node,
            AstBox::Until(node) => node,
            AstBox::Generic(node) => node,
            AstBox::TypeDeclaration(node) => node,
            AstBox::UninitializedVar(node) => node,
            AstBox::Rescue(node) => node,
            AstBox::ExceptionHandler(node) => node,
            AstBox::ProcLiteral(node) => node,
            AstBox::ProcPointer(node) => node,
            AstBox::Union(node) => node,
            AstBox::Self_(node) => node,
            AstBox::Return(node) => node,
            AstBox::Break(node) => node,
            AstBox::Next(node) => node,
            AstBox::Yield(node) => node,
            AstBox::Include(node) => node,
            AstBox::Extend(node) => node,
            AstBox::LibDef(node) => node,
            AstBox::FunDef(node) => node,
            AstBox::TypeDef(node) => node,
            AstBox::CStructOrUnionDef(node) => node,
            AstBox::EnumDef(node) => node,
            AstBox::ExternalVar(node) => node,
            AstBox::Alias(node) => node,
            AstBox::Metaclass(node) => node,
            AstBox::Cast(node) => node,
            AstBox::NilableCast(node) => node,
            AstBox::TypeOf(node) => node,
            AstBox::Annotation(node) => node,
            AstBox::MacroExpression(node) => node,
            AstBox::MacroLiteral(node) => node,
            AstBox::MacroVerbatim(node) => node,
            AstBox::MacroIf(node) => node,
            AstBox::MacroFor(node) => node,
            AstBox::MacroVar(node) => node,
            AstBox::Underscore(node) => node,
            AstBox::Splat(node) => node,
            AstBox::DoubleSplat(node) => node,
            AstBox::MagicConstant(node) => node,
            AstBox::Asm(node) => node,
            AstBox::AsmOperand(node) => node,
        }
    }
}

impl<'a, 'f> From<AstRef<'a, 'f>> for AstNodeRef<'a, 'f> {
    fn from(value: AstRef<'a, 'f>) -> Self {
        match value {
            AstRef::Nop(node) => node,
            AstRef::Expressions(node) => node,
            AstRef::NilLiteral(node) => node,
            AstRef::BoolLiteral(node) => node,
            AstRef::NumberLiteral(node) => node,
            AstRef::CharLiteral(node) => node,
            AstRef::StringLiteral(node) => node,
            AstRef::StringInterpolation(node) => node,
            AstRef::SymbolLiteral(node) => node,
            AstRef::ArrayLiteral(node) => node,
            AstRef::HashLiteral(node) => node,
            AstRef::NamedTupleLiteral(node) => node,
            AstRef::RangeLiteral(node) => node,
            AstRef::RegexLiteral(node) => node,
            AstRef::TupleLiteral(node) => node,
            AstRef::Var(node) => node,
            AstRef::Block(node) => node,
            AstRef::Call(node) => node,
            AstRef::NamedArgument(node) => node,
            AstRef::If(node) => node,
            AstRef::Unless(node) => node,
            AstRef::Assign(node) => node,
            AstRef::OpAssign(node) => node,
            AstRef::MultiAssign(node) => node,
            AstRef::InstanceVar(node) => node,
            AstRef::ReadInstanceVar(node) => node,
            AstRef::ClassVar(node) => node,
            AstRef::Global(node) => node,
            AstRef::And(node) => node,
            AstRef::Or(node) => node,
            AstRef::Arg(node) => node,
            AstRef::ProcNotation(node) => node,
            AstRef::Def(node) => node,
            AstRef::Macro(node) => node,
            AstRef::Not(node) => node,
            AstRef::PointerOf(node) => node,
            AstRef::SizeOf(node) => node,
            AstRef::InstanceSizeOf(node) => node,
            AstRef::Out(node) => node,
            AstRef::OffsetOf(node) => node,
            AstRef::VisibilityModifier(node) => node,
            AstRef::IsA(node) => node,
            AstRef::RespondsTo(node) => node,
            AstRef::Require(node) => node,
            AstRef::When(node) => node,
            AstRef::Case(node) => node,
            AstRef::Select(node) => node,
            AstRef::ImplicitObj(node) => node,
            AstRef::Path(node) => node,
            AstRef::ClassDef(node) => node,
            AstRef::ModuleDef(node) => node,
            AstRef::AnnotationDef(node) => node,
            AstRef::While(node) => node,
            AstRef::Until(node) => node,
            AstRef::Generic(node) => node,
            AstRef::TypeDeclaration(node) => node,
            AstRef::UninitializedVar(node) => node,
            AstRef::Rescue(node) => node,
            AstRef::ExceptionHandler(node) => node,
            AstRef::ProcLiteral(node) => node,
            AstRef::ProcPointer(node) => node,
            AstRef::Union(node) => node,
            AstRef::Self_(node) => node,
            AstRef::Return(node) => node,
            AstRef::Break(node) => node,
            AstRef::Next(node) => node,
            AstRef::Yield(node) => node,
            AstRef::Include(node) => node,
            AstRef::Extend(node) => node,
            AstRef::LibDef(node) => node,
            AstRef::FunDef(node) => node,
            AstRef::TypeDef(node) => node,
            AstRef::CStructOrUnionDef(node) => node,
            AstRef::EnumDef(node) => node,
            AstRef::ExternalVar(node) => node,
            AstRef::Alias(node) => node,
            AstRef::Metaclass(node) => node,
            AstRef::Cast(node) => node,
            AstRef::NilableCast(node) => node,
            AstRef::TypeOf(node) => node,
            AstRef::Annotation(node) => node,
            AstRef::MacroExpression(node) => node,
            AstRef::MacroLiteral(node) => node,
            AstRef::MacroVerbatim(node) => node,
            AstRef::MacroIf(node) => node,
            AstRef::MacroFor(node) => node,
            AstRef::MacroVar(node) => node,
            AstRef::Underscore(node) => node,
            AstRef::Splat(node) => node,
            AstRef::DoubleSplat(node) => node,
            AstRef::MagicConstant(node) => node,
            AstRef::Asm(node) => node,
            AstRef::AsmOperand(node) => node,
        }
    }
}

macro_rules! Node {
    ($name:ident) => {
        Node!($name;);
    };

    ($name:ident; $($vis:vis $field:ident: $type:ty,)*) => {
        #[derive(Debug)]
        pub struct $name<'f> {
            location: Option<Rc<Location<'f>>>,
            end_location: Option<Rc<Location<'f>>>,
            $($vis $field: $type),*
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

            fn tag(&self) -> AstTag {
                AstTag::$name
            }

            fn to_box(self: Box<Self>) -> AstBox<'f> {
                AstBox::$name(self)
            }

            fn to_ref<'a>(&'a self) -> AstRef<'a, 'f> {
                AstRef::$name(self)
            }
        }
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

impl<'f> NilLiteral<'f> {
    pub fn new() -> Box<Self> {
        new_node!()
    }
}

Node!(
    BoolLiteral;
    pub value: bool,
);

impl<'f> BoolLiteral<'f> {
    pub fn new(value: bool) -> Box<Self> {
        new_node! {
            value: value,
        }
    }
}

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

impl<'f> NumberLiteral<'f> {
    pub fn new(value: Vec<char>, kind: NumberKind) -> Box<Self> {
        new_node! {
            value: value,
            kind: kind,
        }
    }
}

Node!(
    CharLiteral;
    pub value: char,
);

impl<'f> CharLiteral<'f> {
    pub fn new(value: char) -> Box<Self> {
        new_node! {
            value: value,
        }
    }
}

Node!(
    StringLiteral;
    pub value: Vec<char>,
);

impl<'f> StringLiteral<'f> {
    pub fn new(value: Vec<char>) -> Box<Self> {
        new_node! {
            value: value,
        }
    }
}

Node!(
    StringInterpolation;
    pub expressions: Vec<AstNodeBox<'f>>,
    pub heredoc_indent: usize,
);

impl<'f> StringInterpolation<'f> {
    pub fn new(expressions: Vec<AstNodeBox<'f>>, heredoc_indent: usize) -> Box<Self> {
        new_node! {
            expressions: expressions,
            heredoc_indent: heredoc_indent,
        }
    }
}

Node!(
    SymbolLiteral;
    pub value: Vec<char>,
);

impl<'f> SymbolLiteral<'f> {
    pub fn new(value: Vec<char>) -> Box<Self> {
        new_node! {
            value: value,
        }
    }
}

Node!(
    ArrayLiteral;
    pub elements: Vec<AstNodeBox<'f>>,
    pub of: Option<AstNodeBox<'f>>,
    pub name: Option<AstNodeBox<'f>>,
);

impl<'f> ArrayLiteral<'f> {
    pub fn new(
        elements: Vec<AstNodeBox<'f>>,
        of: Option<AstNodeBox<'f>>,
        name: Option<AstNodeBox<'f>>,
    ) -> Box<Self> {
        new_node! {
            elements: elements,
            of: of,
            name: name,
        }
    }
}

Node!(
    HashLiteral;
    pub entries: Vec<HashLiteralEntry<'f>>,
    pub of: Option<HashLiteralEntry<'f>>,
    pub name: Option<AstNodeBox<'f>>,
);

impl<'f> HashLiteral<'f> {
    pub fn new(
        entries: Vec<HashLiteralEntry<'f>>,
        of: Option<HashLiteralEntry<'f>>,
        name: Option<AstNodeBox<'f>>,
    ) -> Box<Self> {
        new_node! {
            entries: entries,
            of: of,
            name: name,
        }
    }
}

#[derive(Debug)]
pub struct HashLiteralEntry<'f> {
    pub key: AstNodeBox<'f>,
    pub value: AstNodeBox<'f>,
}

impl<'f> HashLiteralEntry<'f> {
    pub fn new(key: AstNodeBox<'f>, value: AstNodeBox<'f>) -> Self {
        Self { key, value }
    }
}

Node!(
    NamedTupleLiteral;
    pub entries: Vec<NamedTupleEntry<'f>>,
);

impl<'f> NamedTupleLiteral<'f> {
    pub fn new(entries: Vec<NamedTupleEntry<'f>>) -> Box<Self> {
        new_node! {
            entries: entries,
        }
    }
}

#[derive(Debug)]
pub struct NamedTupleEntry<'f> {
    pub key: Vec<char>,
    pub value: AstNodeBox<'f>,
}

impl<'f> NamedTupleEntry<'f> {
    pub fn new(key: Vec<char>, value: AstNodeBox<'f>) -> Self {
        Self { key, value }
    }
}

Node!(
    RangeLiteral;
    pub from: AstNodeBox<'f>,
    pub to: AstNodeBox<'f>,
    pub exclusive: bool,
);

impl<'f> RangeLiteral<'f> {
    pub fn new(from: AstNodeBox<'f>, to: AstNodeBox<'f>, exclusive: bool) -> Box<Self> {
        new_node! {
            from: from,
            to: to,
            exclusive: exclusive,
        }
    }
}

Node!(
    RegexLiteral;
    pub value: AstNodeBox<'f>,
    // pub options: ?,
);

impl<'f> RegexLiteral<'f> {
    pub fn new(
        value: AstNodeBox<'f>,
        // options: ?,
    ) -> Box<Self> {
        new_node! {
            value: value,
            // options: options,
        }
    }
}

Node!(
    TupleLiteral;
    pub elements: Vec<AstNodeBox<'f>>,
);

impl<'f> TupleLiteral<'f> {
    pub fn new(elements: Vec<AstNodeBox<'f>>) -> Box<Self> {
        new_node! {
            elements: elements,
        }
    }
}

Node!(
    Var;
    pub name: Vec<char>,
);

impl<'f> Var<'f> {
    pub fn new(name: Vec<char>) -> Box<Self> {
        new_node! {
            name: name,
        }
    }
}

Node!(
    Block;
    pub args: Vec<Box<Var<'f>>>,
    pub body: AstNodeBox<'f>,
    pub call: Option<Box<Call<'f>>>,
    pub splat_index: Option<usize>,
);

impl<'f> Block<'f> {
    pub fn new(
        args: Vec<Box<Var<'f>>>,
        body: AstNodeBox<'f>,
        call: Option<Box<Call<'f>>>,
        splat_index: Option<usize>,
    ) -> Box<Self> {
        new_node! {
            args: args,
            body: body,
            call: call,
            splat_index: splat_index,
        }
    }
}

Node!(
    Call;
    pub obj: Option<AstNodeBox<'f>>,
    pub name: Vec<char>,
    pub args: Vec<AstNodeBox<'f>>,
    pub block: Option<Box<Block<'f>>>,
    pub block_arg: Option<AstNodeBox<'f>>,
    pub named_args: Option<Vec<Box<NamedArgument<'f>>>>,
    pub name_location: Option<Location<'f>>,
    name_size: Option<usize>,
    pub doc: Option<Vec<char>>,
    pub visibility: Visibility,
    pub global: bool,
    pub expansion: bool,
    pub has_parentheses: bool,
);

impl<'f> Call<'f> {
    pub fn new(
        obj: Option<AstNodeBox<'f>>,
        name: Vec<char>,
        args: Vec<AstNodeBox<'f>>,
        block: Option<Box<Block<'f>>>,
        block_arg: Option<AstNodeBox<'f>>,
        named_args: Option<Vec<Box<NamedArgument<'f>>>>,
        name_location: Option<Location<'f>>,
        name_size: Option<usize>,
        doc: Option<Vec<char>>,
        // visibility: Visibility,
        global: bool,
        // expansion: bool,
        // has_parentheses: bool,
    ) -> Box<Self> {
        new_node! {
            obj: obj,
            name: name,
            args: args,
            block: block,
            block_arg: block_arg,
            named_args: named_args,
            name_location: name_location,
            name_size: name_size,
            doc: doc,
            visibility: Visibility::Public,
            global: global,
            expansion: false,
            has_parentheses: false,
        }
    }

    pub fn name_size(&mut self) -> usize {
        if self.name_size.is_none() {
            self.name_size = Some(self.calculate_name_size());
        }
        self.name_size.unwrap()
    }

    fn calculate_name_size(&self) -> usize {
        if let Some(c) = self.name.last() {
            if *c == '=' || *c == '@' {
                return self.name.len() - 1;
            }
        }
        self.name.len()
    }

    pub fn set_name_size(&mut self, name_size: usize) {
        self.name_size = Some(name_size);
    }
}

Node!(
    NamedArgument;
    pub name: Vec<char>,
    pub value: AstNodeBox<'f>,
);

impl<'f> NamedArgument<'f> {
    pub fn new(name: Vec<char>, value: AstNodeBox<'f>) -> Box<Self> {
        new_node! {
            name: name,
            value: value,
        }
    }
}

Node!(
    If;
    pub cond: AstNodeBox<'f>,
    pub then: AstNodeBox<'f>,
    pub else_: AstNodeBox<'f>,
    pub ternary: bool,
    pub else_location: Option<Location<'f>>,
);

impl<'f> If<'f> {
    pub fn new(
        cond: AstNodeBox<'f>,
        then: AstNodeBox<'f>,
        else_: AstNodeBox<'f>,
        ternary: bool,
        else_location: Option<Location<'f>>,
    ) -> Box<Self> {
        new_node! {
            cond: cond,
            then: then,
            else_: else_,
            ternary: ternary,
            else_location: else_location,
        }
    }
}

Node!(
    Unless;
    pub cond: AstNodeBox<'f>,
    pub then: AstNodeBox<'f>,
    pub else_: AstNodeBox<'f>,
    pub else_location: Option<Location<'f>>,
);

impl<'f> Unless<'f> {
    pub fn new(
        cond: AstNodeBox<'f>,
        then: AstNodeBox<'f>,
        else_: AstNodeBox<'f>,
        else_location: Option<Location<'f>>,
    ) -> Box<Self> {
        new_node! {
            cond: cond,
            then: then,
            else_: else_,
            else_location: else_location,
        }
    }
}

Node!(
    Assign;
    pub target: AstNodeBox<'f>,
    pub value: AstNodeBox<'f>,
    pub doc: Option<Vec<char>>,
);

impl<'f> Assign<'f> {
    pub fn new(target: AstNodeBox<'f>, value: AstNodeBox<'f>, doc: Option<Vec<char>>) -> Box<Self> {
        new_node! {
            target: target,
            value: value,
            doc: doc,
        }
    }
}

Node!(
    OpAssign;
    pub target: AstNodeBox<'f>,
    pub op: Vec<char>,
    pub value: AstNodeBox<'f>,
    pub name_location: Option<Location<'f>>,
);

impl<'f> OpAssign<'f> {
    pub fn new(
        target: AstNodeBox<'f>,
        op: Vec<char>,
        value: AstNodeBox<'f>,
        name_location: Option<Location<'f>>,
    ) -> Box<Self> {
        new_node! {
            target: target,
            op: op,
            value: value,
            name_location: name_location,
        }
    }
}

Node!(
    MultiAssign;
    pub targets: Vec<AstNodeBox<'f>>,
    pub values: Vec<AstNodeBox<'f>>,
);

impl<'f> MultiAssign<'f> {
    pub fn new(targets: Vec<AstNodeBox<'f>>, values: Vec<AstNodeBox<'f>>) -> Box<Self> {
        new_node! {
            targets: targets,
            values: values,
        }
    }
}

Node!(
    InstanceVar;
    pub name: Vec<char>,
);

impl<'f> InstanceVar<'f> {
    pub fn new(name: Vec<char>) -> Box<Self> {
        new_node! {
            name: name,
        }
    }
}

Node!(
    ReadInstanceVar;
    pub obj: AstNodeBox<'f>,
    pub name: Vec<char>,
);

impl<'f> ReadInstanceVar<'f> {
    pub fn new(obj: AstNodeBox<'f>, name: Vec<char>) -> Box<Self> {
        new_node! {
            obj: obj,
            name: name,
        }
    }
}

Node!(
    ClassVar;
    pub name: Vec<char>,
);

impl<'f> ClassVar<'f> {
    pub fn new(name: Vec<char>) -> Box<Self> {
        new_node! {
            name: name,
        }
    }
}

Node!(
    Global;
    pub name: Vec<char>,
);

impl<'f> Global<'f> {
    pub fn new(name: Vec<char>) -> Box<Self> {
        new_node! {
            name: name,
        }
    }
}

macro_rules! BinaryOp {
    ($name:ident) => {
        Node!(
            $name;
            pub left: AstNodeBox<'f>,
            pub right: AstNodeBox<'f>,
        );

        impl<'f> $name<'f> {
            pub fn new(left: AstNodeBox<'f>, right: AstNodeBox<'f>) -> Box<Self> {
                new_node! {
                    left: left,
                    right: right,
                }
            }
        }
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
    pub parsed_annotations: Option<Vec<Box<Annotation<'f>>>>,
);

impl<'f> Arg<'f> {
    pub fn new(
        name: Vec<char>,
        external_name: Vec<char>,
        default_value: Option<AstNodeBox<'f>>,
        restriction: Option<AstNodeBox<'f>>,
        doc: Option<Vec<char>>,
        parsed_annotations: Option<Vec<Box<Annotation<'f>>>>,
    ) -> Box<Self> {
        new_node! {
            name: name,
            external_name: external_name,
            default_value: default_value,
            restriction: restriction,
            doc: doc,
            parsed_annotations: parsed_annotations,
        }
    }
}

Node!(
    ProcNotation;
    pub inputs: Option<Vec<AstNodeBox<'f>>>,
    pub output: Option<AstNodeBox<'f>>,
);

impl<'f> ProcNotation<'f> {
    pub fn new(inputs: Option<Vec<AstNodeBox<'f>>>, output: Option<AstNodeBox<'f>>) -> Box<Self> {
        new_node! {
            inputs: inputs,
            output: output,
        }
    }
}

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

impl<'f> Def<'f> {
    pub fn new(
        free_vars: Option<Vec<Vec<char>>>,
        receiver: Option<AstNodeBox<'f>>,
        name: Vec<char>,
        args: Vec<Box<Arg<'f>>>,
        double_splat: Option<Box<Arg<'f>>>,
        body: AstNodeBox<'f>,
        block_arg: Option<Box<Arg<'f>>>,
        return_type: Option<AstNodeBox<'f>>,
        block_arity: Option<usize>,
        name_location: Option<Location<'f>>,
        splat_index: Option<usize>,
        doc: Option<Vec<char>>,
        // visibility: Visibility,
        macro_def: bool,
        // calls_super: bool,
        // calls_initialize: bool,
        // calls_previous_def: bool,
        // uses_block_arg: bool,
        // assigns_special_var: bool,
        abstract_: bool,
    ) -> Box<Self> {
        new_node! {
            free_vars: free_vars,
            receiver: receiver,
            name: name,
            args: args,
            double_splat: double_splat,
            body: body,
            block_arg: block_arg,
            return_type: return_type,
            block_arity: block_arity,
            name_location: name_location,
            splat_index: splat_index,
            doc: doc,
            visibility: Visibility::Public,
            macro_def: macro_def,
            calls_super: false,
            calls_initialize: false,
            calls_previous_def: false,
            uses_block_arg: false,
            assigns_special_var: false,
            abstract_: abstract_,
        }
    }
}

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

impl<'f> Macro<'f> {
    pub fn new(
        name: Vec<char>,
        args: Vec<Box<Arg<'f>>>,
        body: AstNodeBox<'f>,
        double_splat: Option<Box<Arg<'f>>>,
        block_arg: Option<Box<Arg<'f>>>,
        name_location: Option<Location<'f>>,
        splat_index: Option<usize>,
        doc: Option<Vec<char>>,
        // visibility: Visibility,
    ) -> Box<Self> {
        new_node! {
            name: name,
            args: args,
            body: body,
            double_splat: double_splat,
            block_arg: block_arg,
            name_location: name_location,
            splat_index: splat_index,
            doc: doc,
            visibility: Visibility::Public,
        }
    }
}

macro_rules! UnaryExpression {
    ($name:ident) => {
        Node!(
            $name;
            pub exp: AstNodeBox<'f>,
        );

        impl<'f> $name<'f> {
            pub fn new(exp: AstNodeBox<'f>) -> Box<Self> {
                new_node! {
                    exp: exp,
                }
            }
        }
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

impl<'f> OffsetOf<'f> {
    pub fn new(offsetof_type: AstNodeBox<'f>, offset: AstNodeBox<'f>) -> Box<Self> {
        new_node! {
            offsetof_type: offsetof_type,
            offset: offset,
        }
    }
}

Node!(
    VisibilityModifier;
    pub modifier: Visibility,
    pub exp: AstNodeBox<'f>,
    pub doc: Option<Vec<char>>,
);

impl<'f> VisibilityModifier<'f> {
    pub fn new(modifier: Visibility, exp: AstNodeBox<'f>, doc: Option<Vec<char>>) -> Box<Self> {
        new_node! {
            modifier: modifier,
            exp: exp,
            doc: doc,
        }
    }
}

Node!(
    IsA;
    pub obj: AstNodeBox<'f>,
    pub const_: AstNodeBox<'f>,
    pub nil_check: bool,
);

impl<'f> IsA<'f> {
    pub fn new(obj: AstNodeBox<'f>, const_: AstNodeBox<'f>, nil_check: bool) -> Box<Self> {
        new_node! {
            obj: obj,
            const_: const_,
            nil_check: nil_check,
        }
    }
}

Node!(
    RespondsTo;
    pub obj: AstNodeBox<'f>,
    pub name: Vec<char>,
);

impl<'f> RespondsTo<'f> {
    pub fn new(obj: AstNodeBox<'f>, name: Vec<char>) -> Box<Self> {
        new_node! {
            obj: obj,
            name: name,
        }
    }
}

Node!(
    Require;
    pub string: Vec<char>,
);

impl<'f> Require<'f> {
    pub fn new(string: Vec<char>) -> Box<Self> {
        new_node! {
            string: string,
        }
    }
}

Node!(
    When;
    pub conds: Vec<AstNodeBox<'f>>,
    pub body: AstNodeBox<'f>,
    pub exhaustive: bool,
);

impl<'f> When<'f> {
    pub fn new(conds: Vec<AstNodeBox<'f>>, body: AstNodeBox<'f>, exhaustive: bool) -> Box<Self> {
        new_node! {
            conds: conds,
            body: body,
            exhaustive: exhaustive,
        }
    }
}

Node!(
    Case;
    pub cond: Option<AstNodeBox<'f>>,
    pub whens: Vec<When<'f>>,
    pub else_: Option<AstNodeBox<'f>>,
    pub exhaustive: bool,
);

impl<'f> Case<'f> {
    pub fn new(
        cond: Option<AstNodeBox<'f>>,
        whens: Vec<When<'f>>,
        else_: Option<AstNodeBox<'f>>,
        exhaustive: bool,
    ) -> Box<Self> {
        new_node! {
            cond: cond,
            whens: whens,
            else_: else_,
            exhaustive: exhaustive,
        }
    }
}

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

impl<'f> Select<'f> {
    pub fn new(whens: Vec<SelectWhen<'f>>, else_: Option<AstNodeBox<'f>>) -> Box<Self> {
        new_node! {
            whens: whens,
            else_: else_,
        }
    }
}

Node!(ImplicitObj);

impl<'f> ImplicitObj<'f> {
    pub fn new() -> Box<Self> {
        new_node!()
    }
}

Node!(
    Path;
    pub names: Vec<Vec<char>>,
    pub global: bool,
    pub visibility: Visibility,
);

impl<'f> Path<'f> {
    pub fn new(
        names: Vec<Vec<char>>,
        global: bool,
        // visibility: Visibility,
    ) -> Box<Self> {
        new_node! {
            names: names,
            global: global,
            visibility: Visibility::Public,
        }
    }
}

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

impl<'f> ClassDef<'f> {
    pub fn new(
        name: Box<Path<'f>>,
        body: AstNodeBox<'f>,
        superclass: Option<AstNodeBox<'f>>,
        type_vars: Option<Vec<Vec<char>>>,
        name_location: Option<Location<'f>>,
        doc: Option<Vec<char>>,
        splat_index: Option<usize>,
        abstract_: bool,
        struct_: bool,
        // visibility: Visibility,
    ) -> Box<Self> {
        new_node! {
            name: name,
            body: body,
            superclass: superclass,
            type_vars: type_vars,
            name_location: name_location,
            doc: doc,
            splat_index: splat_index,
            abstract_: abstract_,
            struct_: struct_,
            visibility: Visibility::Public,
        }
    }
}

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

impl<'f> ModuleDef<'f> {
    pub fn new(
        name: Box<Path<'f>>,
        body: AstNodeBox<'f>,
        type_vars: Option<Vec<Vec<char>>>,
        splat_index: Option<usize>,
        name_location: Option<Location<'f>>,
        doc: Option<Vec<char>>,
        // visibility: Visibility,
    ) -> Box<Self> {
        new_node! {
            name: name,
            body: body,
            type_vars: type_vars,
            splat_index: splat_index,
            name_location: name_location,
            doc: doc,
            visibility: Visibility::Public,
        }
    }
}

Node!(
    AnnotationDef;
    pub name: Box<Path<'f>>,
    pub doc: Option<Vec<char>>,
    pub name_location: Option<Location<'f>>,
);

impl<'f> AnnotationDef<'f> {
    pub fn new(
        name: Box<Path<'f>>,
        doc: Option<Vec<char>>,
        name_location: Option<Location<'f>>,
    ) -> Box<Self> {
        new_node! {
            name: name,
            doc: doc,
            name_location: name_location,
        }
    }
}

Node!(
    While;
    pub cond: AstNodeBox<'f>,
    pub body: AstNodeBox<'f>,
);

impl<'f> While<'f> {
    pub fn new(cond: AstNodeBox<'f>, body: AstNodeBox<'f>) -> Box<Self> {
        new_node! {
            cond: cond,
            body: body,
        }
    }
}

Node!(
    Until;
    pub cond: AstNodeBox<'f>,
    pub body: AstNodeBox<'f>,
);

impl<'f> Until<'f> {
    pub fn new(cond: AstNodeBox<'f>, body: AstNodeBox<'f>) -> Box<Self> {
        new_node! {
            cond: cond,
            body: body,
        }
    }
}

Node!(
    Generic;
    pub name: AstNodeBox<'f>,
    pub type_vars: Vec<AstNodeBox<'f>>,
    pub named_args: Option<Vec<Box<NamedArgument<'f>>>>,
    pub suffix: GenericSuffix,
);

impl<'f> Generic<'f> {
    pub fn new(
        name: AstNodeBox<'f>,
        type_vars: Vec<AstNodeBox<'f>>,
        named_args: Option<Vec<Box<NamedArgument<'f>>>>,
        suffix: GenericSuffix,
    ) -> Box<Self> {
        new_node! {
            name: name,
            type_vars: type_vars,
            named_args: named_args,
            suffix: suffix,
        }
    }
}

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

impl<'f> TypeDeclaration<'f> {
    pub fn new(
        var: AstNodeBox<'f>,
        declared_type: AstNodeBox<'f>,
        value: Option<AstNodeBox<'f>>,
    ) -> Box<Self> {
        new_node! {
            var: var,
            declared_type: declared_type,
            value: value,
        }
    }
}

Node!(
    UninitializedVar;
    pub var: AstNodeBox<'f>,
    pub declared_type: AstNodeBox<'f>,
);

impl<'f> UninitializedVar<'f> {
    pub fn new(var: AstNodeBox<'f>, declared_type: AstNodeBox<'f>) -> Box<Self> {
        new_node! {
            var: var,
            declared_type: declared_type,
        }
    }
}

Node!(
    Rescue;
    pub body: AstNodeBox<'f>,
    pub types: Option<Vec<AstNodeBox<'f>>>,
    pub name: Option<Vec<char>>,
);

impl<'f> Rescue<'f> {
    pub fn new(
        body: AstNodeBox<'f>,
        types: Option<Vec<AstNodeBox<'f>>>,
        name: Option<Vec<char>>,
    ) -> Box<Self> {
        new_node! {
            body: body,
            types: types,
            name: name,
        }
    }
}

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

impl<'f> ExceptionHandler<'f> {
    pub fn new(
        body: AstNodeBox<'f>,
        rescues: Option<Vec<Box<Rescue<'f>>>>,
        else_: Option<AstNodeBox<'f>>,
        ensure: Option<AstNodeBox<'f>>,
        // implicit: bool,
        // suffix: bool,
        else_location: Option<Location<'f>>,
        ensure_location: Option<Location<'f>>,
    ) -> Box<Self> {
        new_node! {
            body: body,
            rescues: rescues,
            else_: else_,
            ensure: ensure,
            implicit: false,
            suffix: false,
            else_location: else_location,
            ensure_location: ensure_location,
        }
    }
}

Node!(
    ProcLiteral;
    pub def: Box<Def<'f>>,
);

impl<'f> ProcLiteral<'f> {
    pub fn new(def: Box<Def<'f>>) -> Box<Self> {
        new_node! {
            def: def,
        }
    }
}

Node!(
    ProcPointer;
    pub obj: Option<AstNodeBox<'f>>,
    pub name: Vec<char>,
    pub args: Vec<AstNodeBox<'f>>,
    pub global: bool,
);

impl<'f> ProcPointer<'f> {
    pub fn new(
        obj: Option<AstNodeBox<'f>>,
        name: Vec<char>,
        args: Vec<AstNodeBox<'f>>,
        global: bool,
    ) -> Box<Self> {
        new_node! {
            obj: obj,
            name: name,
            args: args,
            global: global,
        }
    }
}

Node!(
    Union;
    pub types: Vec<AstNodeBox<'f>>,
);

impl<'f> Union<'f> {
    pub fn new(types: Vec<AstNodeBox<'f>>) -> Box<Self> {
        new_node! {
            types: types,
        }
    }
}

Node!(Self_);

impl<'f> Self_<'f> {
    pub fn new() -> Box<Self> {
        new_node!()
    }
}

macro_rules! ControlExpressions {
    ($($name:ident;)+) => {
        const CONTROL_EXPRESSIONS: &[AstTag] = &[$(AstTag::$name),+];

        $(Node!(
            $name;
            pub exp: Option<AstNodeBox<'f>>,
        );)+

        $(impl<'f> $name<'f> {
            pub fn new(exp: Option<AstNodeBox<'f>>) -> Box<Self> {
                new_node! {
                    exp: exp,
                }
            }
        })+
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

impl<'f> Yield<'f> {
    pub fn new(
        exps: Vec<AstNodeBox<'f>>,
        scope: Option<AstNodeBox<'f>>,
        // has_parentheses: bool,
    ) -> Box<Self> {
        new_node! {
            exps: exps,
            scope: scope,
            has_parentheses: false,
        }
    }
}

Node!(
    Include;
    pub name: AstNodeBox<'f>,
);

impl<'f> Include<'f> {
    pub fn new(name: AstNodeBox<'f>) -> Box<Self> {
        new_node! {
            name: name,
        }
    }
}

Node!(
    Extend;
    pub name: AstNodeBox<'f>,
);

impl<'f> Extend<'f> {
    pub fn new(name: AstNodeBox<'f>) -> Box<Self> {
        new_node! {
            name: name,
        }
    }
}

Node!(
    LibDef;
    pub name: Vec<char>,
    pub body: AstNodeBox<'f>,
    pub name_location: Option<Location<'f>>,
    pub visibility: Visibility,
);

impl<'f> LibDef<'f> {
    pub fn new(
        name: Vec<char>,
        body: AstNodeBox<'f>,
        name_location: Option<Location<'f>>,
        // visibility: Visibility,
    ) -> Box<Self> {
        new_node! {
            name: name,
            body: body,
            name_location: name_location,
            visibility: Visibility::Public,
        }
    }
}

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

impl<'f> FunDef<'f> {
    pub fn new(
        name: Vec<char>,
        args: Vec<Box<Arg<'f>>>,
        return_type: Option<AstNodeBox<'f>>,
        body: Option<AstNodeBox<'f>>,
        real_name: Vec<char>,
        doc: Option<Vec<char>>,
        varargs: bool,
    ) -> Box<Self> {
        new_node! {
            name: name,
            args: args,
            return_type: return_type,
            body: body,
            real_name: real_name,
            doc: doc,
            varargs: varargs,
        }
    }
}

Node!(
    TypeDef;
    pub name: Vec<char>,
    pub type_spec: AstNodeBox<'f>,
    pub name_location: Option<Location<'f>>,
);

impl<'f> TypeDef<'f> {
    pub fn new(
        name: Vec<char>,
        type_spec: AstNodeBox<'f>,
        name_location: Option<Location<'f>>,
    ) -> Box<Self> {
        new_node! {
            name: name,
            type_spec: type_spec,
            name_location: name_location,
        }
    }
}

Node!(
    CStructOrUnionDef;
    pub name: Vec<char>,
    pub body: AstNodeBox<'f>,
    pub union: bool,
);

impl<'f> CStructOrUnionDef<'f> {
    pub fn new(name: Vec<char>, body: AstNodeBox<'f>, union: bool) -> Box<Self> {
        new_node! {
            name: name,
            body: body,
            union: union,
        }
    }
}

Node!(
    EnumDef;
    pub name: Box<Path<'f>>,
    pub members: Vec<AstNodeBox<'f>>,
    pub base_type: Option<AstNodeBox<'f>>,
    pub doc: Option<Vec<char>>,
    pub visibility: Visibility,
);

impl<'f> EnumDef<'f> {
    pub fn new(
        name: Box<Path<'f>>,
        members: Vec<AstNodeBox<'f>>,
        base_type: Option<AstNodeBox<'f>>,
        doc: Option<Vec<char>>,
        // visibility: Visibility,
    ) -> Box<Self> {
        new_node! {
            name: name,
            members: members,
            base_type: base_type,
            doc: doc,
            visibility: Visibility::Public,
        }
    }
}

Node!(
    ExternalVar;
    pub name: Vec<char>,
    pub type_spec: AstNodeBox<'f>,
    pub real_name: Option<Vec<char>>,
);

impl<'f> ExternalVar<'f> {
    pub fn new(
        name: Vec<char>,
        type_spec: AstNodeBox<'f>,
        real_name: Option<Vec<char>>,
    ) -> Box<Self> {
        new_node! {
            name: name,
            type_spec: type_spec,
            real_name: real_name,
        }
    }
}

Node!(
    Alias;
    pub name: Box<Path<'f>>,
    pub value: AstNodeBox<'f>,
    pub doc: Option<Vec<char>>,
    pub visibility: Visibility,
);

impl<'f> Alias<'f> {
    pub fn new(
        name: Box<Path<'f>>,
        value: AstNodeBox<'f>,
        doc: Option<Vec<char>>,
        // visibility: Visibility,
    ) -> Box<Self> {
        new_node! {
            name: name,
            value: value,
            doc: doc,
            visibility: Visibility::Public,
        }
    }
}

Node!(
    Metaclass;
    pub name: AstNodeBox<'f>,
);

impl<'f> Metaclass<'f> {
    pub fn new(name: AstNodeBox<'f>) -> Box<Self> {
        new_node! {
            name: name,
        }
    }
}

Node!(
    Cast;
    pub obj: AstNodeBox<'f>,
    pub to: AstNodeBox<'f>,
);

impl<'f> Cast<'f> {
    pub fn new(obj: AstNodeBox<'f>, to: AstNodeBox<'f>) -> Box<Self> {
        new_node! {
            obj: obj,
            to: to,
        }
    }
}

Node!(
    NilableCast;
    pub obj: AstNodeBox<'f>,
    pub to: AstNodeBox<'f>,
);

impl<'f> NilableCast<'f> {
    pub fn new(obj: AstNodeBox<'f>, to: AstNodeBox<'f>) -> Box<Self> {
        new_node! {
            obj: obj,
            to: to,
        }
    }
}

Node!(
    TypeOf;
    pub expressions: Vec<AstNodeBox<'f>>,
);

impl<'f> TypeOf<'f> {
    pub fn new(expressions: Vec<AstNodeBox<'f>>) -> Box<Self> {
        new_node! {
            expressions: expressions,
        }
    }
}

Node!(
    Annotation;
    pub path: Box<Path<'f>>,
    pub args: Vec<AstNodeBox<'f>>,
    pub named_args: Option<Vec<Box<NamedArgument<'f>>>>,
    pub doc: Option<Vec<char>>,
);

impl<'f> Annotation<'f> {
    pub fn new(
        path: Box<Path<'f>>,
        args: Vec<AstNodeBox<'f>>,
        named_args: Option<Vec<Box<NamedArgument<'f>>>>,
        doc: Option<Vec<char>>,
    ) -> Box<Self> {
        new_node! {
            path: path,
            args: args,
            named_args: named_args,
            doc: doc,
        }
    }
}

Node!(
    MacroExpression;
    pub exp: AstNodeBox<'f>,
    pub output: bool,
);

impl<'f> MacroExpression<'f> {
    pub fn new(exp: AstNodeBox<'f>, output: bool) -> Box<Self> {
        new_node! {
            exp: exp,
            output: output,
        }
    }
}

Node!(
    MacroLiteral;
    pub value: Vec<char>,
);

impl<'f> MacroLiteral<'f> {
    pub fn new(value: Vec<char>) -> Box<Self> {
        new_node! {
            value: value,
        }
    }
}

UnaryExpression!(MacroVerbatim);

Node!(
    MacroIf;
    pub cond: AstNodeBox<'f>,
    pub then: AstNodeBox<'f>,
    pub else_: AstNodeBox<'f>,
);

impl<'f> MacroIf<'f> {
    pub fn new(cond: AstNodeBox<'f>, then: AstNodeBox<'f>, else_: AstNodeBox<'f>) -> Box<Self> {
        new_node! {
            cond: cond,
            then: then,
            else_: else_,
        }
    }
}

Node!(
    MacroFor;
    pub vars: Vec<Box<Var<'f>>>,
    pub exp: AstNodeBox<'f>,
    pub body: AstNodeBox<'f>,
);

impl<'f> MacroFor<'f> {
    pub fn new(vars: Vec<Box<Var<'f>>>, exp: AstNodeBox<'f>, body: AstNodeBox<'f>) -> Box<Self> {
        new_node! {
            vars: vars,
            exp: exp,
            body: body,
        }
    }
}

Node!(
    MacroVar;
    pub name: Vec<char>,
    pub exps: Option<Vec<AstNodeBox<'f>>>,
);

impl<'f> MacroVar<'f> {
    pub fn new(name: Vec<char>, exps: Option<Vec<AstNodeBox<'f>>>) -> Box<Self> {
        new_node! {
            name: name,
            exps: exps,
        }
    }
}

Node!(Underscore);

impl<'f> Underscore<'f> {
    pub fn new() -> Box<Self> {
        new_node!()
    }
}

UnaryExpression!(Splat);

UnaryExpression!(DoubleSplat);

Node!(
    MagicConstant;
    pub name: TokenKind,
);

impl<'f> MagicConstant<'f> {
    pub fn new(name: TokenKind) -> Box<Self> {
        new_node! {
            name: name,
        }
    }
}

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

impl<'f> Asm<'f> {
    pub fn new(
        text: Vec<char>,
        outputs: Option<Vec<Box<AsmOperand<'f>>>>,
        inputs: Option<Vec<Box<AsmOperand<'f>>>>,
        clobbers: Option<Vec<Vec<char>>>,
        volatile: bool,
        alignstack: bool,
        intel: bool,
        can_throw: bool,
    ) -> Box<Self> {
        new_node! {
            text: text,
            outputs: outputs,
            inputs: inputs,
            clobbers: clobbers,
            volatile: volatile,
            alignstack: alignstack,
            intel: intel,
            can_throw: can_throw,
        }
    }
}

Node!(
    AsmOperand;
    pub constraint: Vec<char>,
    pub exp: AstNodeBox<'f>,
);

impl<'f> AsmOperand<'f> {
    pub fn new(constraint: Vec<char>, exp: AstNodeBox<'f>) -> Box<Self> {
        new_node! {
            constraint: constraint,
            exp: exp,
        }
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
    // let mut node = Nop::new();
    // node.at(Location::new("foo", 12, 34));
    // node.at_node_end(Nop::new().as_ref());

    let node: AstNodeBox = Nop::new();
    assert_eq!(node.tag(), AstTag::Nop);
    let node: Box<Nop> = Nop::new();
    assert_eq!(node.tag(), AstTag::Nop);

    let node: AstNodeBox = Return::new(None);
    assert!(node.is_control_expression());
}
