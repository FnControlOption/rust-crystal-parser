use crate::ast::NumberKind;
use crate::location::Location;
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Keyword {
    Abstract,
    Alias,
    Annotation,
    As,
    AsQuestion,
    Asm,
    Begin,
    Break,
    Case,
    Class,
    Def,
    Do,
    Else,
    Elsif,
    End,
    Ensure,
    Enum,
    Extend,
    False,
    For,
    Fun,
    If,
    In,
    Include,
    InstanceSizeof,
    IsAQuestion,
    Lib,
    Macro,
    Module,
    Next,
    Nil,
    NilQuestion,
    Of,
    Offsetof,
    Out,
    Pointerof,
    Private,
    Protected,
    Require,
    Rescue,
    RespondsToQuestion,
    Return,
    Select,
    Self_,
    Sizeof,
    Struct,
    Super,
    Then,
    True,
    Type,
    Typeof,
    Uninitialized,
    Union,
    Unless,
    Until,
    Verbatim,
    When,
    While,
    With,
    Yield,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Keyword::Abstract => "abstract",
                Keyword::Alias => "alias",
                Keyword::Annotation => "annotation",
                Keyword::As => "as",
                Keyword::AsQuestion => "as?",
                Keyword::Asm => "asm",
                Keyword::Begin => "begin",
                Keyword::Break => "break",
                Keyword::Case => "case",
                Keyword::Class => "class",
                Keyword::Def => "def",
                Keyword::Do => "do",
                Keyword::Else => "else",
                Keyword::Elsif => "elsif",
                Keyword::End => "end",
                Keyword::Ensure => "ensure",
                Keyword::Enum => "enum",
                Keyword::Extend => "extend",
                Keyword::False => "false",
                Keyword::For => "for",
                Keyword::Fun => "fun",
                Keyword::If => "if",
                Keyword::In => "in",
                Keyword::Include => "include",
                Keyword::InstanceSizeof => "instance_sizeof",
                Keyword::IsAQuestion => "is_a?",
                Keyword::Lib => "lib",
                Keyword::Macro => "macro",
                Keyword::Module => "module",
                Keyword::Next => "next",
                Keyword::Nil => "nil",
                Keyword::NilQuestion => "nil?",
                Keyword::Of => "of",
                Keyword::Offsetof => "offsetof",
                Keyword::Out => "out",
                Keyword::Pointerof => "pointerof",
                Keyword::Private => "private",
                Keyword::Protected => "protected",
                Keyword::Require => "require",
                Keyword::Rescue => "rescue",
                Keyword::RespondsToQuestion => "responds_to?",
                Keyword::Return => "return",
                Keyword::Select => "select",
                Keyword::Self_ => "self",
                Keyword::Sizeof => "sizeof",
                Keyword::Struct => "struct",
                Keyword::Super => "super",
                Keyword::Then => "then",
                Keyword::True => "true",
                Keyword::Type => "type",
                Keyword::Typeof => "typeof",
                Keyword::Uninitialized => "uninitialized",
                Keyword::Union => "union",
                Keyword::Unless => "unless",
                Keyword::Until => "until",
                Keyword::Verbatim => "verbatim",
                Keyword::When => "when",
                Keyword::While => "while",
                Keyword::With => "with",
                Keyword::Yield => "yield",
            }
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    Eof,
    Space,
    Newline,

    Ident,
    Const,
    InstanceVar,
    ClassVar,

    Char,
    String,
    Symbol,
    Number,

    Underscore,
    Comment,

    DelimiterStart,
    DelimiterEnd,

    StringArrayStart,
    InterpolationStart,
    SymbolArrayStart,
    StringArrayEnd,

    Global,
    GlobalMatchDataIndex,

    Magic(Magic),

    MacroLiteral,
    MacroExpressionStart,
    MacroControlStart,
    MacroVar,
    MacroEnd,

    Op(Op),
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenKind::Eof => "EOF",
                TokenKind::Space => "SPACE",
                TokenKind::Newline => "NEWLINE",

                TokenKind::Ident => "IDENT",
                TokenKind::Const => "CONST",
                TokenKind::InstanceVar => "INSTANCE_VAR",
                TokenKind::ClassVar => "CLASS_VAR",

                TokenKind::Char => "CHAR",
                TokenKind::String => "STRING",
                TokenKind::Symbol => "SYMBOL",
                TokenKind::Number => "NUMBER",

                TokenKind::Underscore => "UNDERSCORE",
                TokenKind::Comment => "COMMENT",

                TokenKind::DelimiterStart => "DELIMITER_START",
                TokenKind::DelimiterEnd => "DELIMITER_END",

                TokenKind::StringArrayStart => "STRING_ARRAY_START",
                TokenKind::InterpolationStart => "INTERPOLATION_START",
                TokenKind::SymbolArrayStart => "SYMBOL_ARRAY_START",
                TokenKind::StringArrayEnd => "STRING_ARRAY_END",

                TokenKind::Global => "GLOBAL",
                TokenKind::GlobalMatchDataIndex => "GLOBAL_MATCH_DATA_INDEX",

                TokenKind::Magic(magic) => return magic.fmt(f),

                TokenKind::MacroLiteral => "MACRO_LITERAL",
                TokenKind::MacroExpressionStart => "MACRO_EXPRESSION_START",
                TokenKind::MacroControlStart => "MACRO_CONTROL_START",
                TokenKind::MacroVar => "MACRO_VAR",
                TokenKind::MacroEnd => "MACRO_END",

                TokenKind::Op(op) => return op.fmt(f),
            }
        )
    }
}

impl TokenKind {
    pub fn is_assignment_operator(&self) -> bool {
        match self {
            TokenKind::Op(op) => op.is_assignment_operator(),
            _ => false,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Magic {
    Dir,
    EndLine,
    File,
    Line,
}

impl fmt::Display for Magic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Magic::Dir => "__DIR__",
                Magic::EndLine => "__END_LINE__",
                Magic::File => "__FILE__",
                Magic::Line => "__LINE__",
            }
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Op {
    Bang,                   // !
    BangEq,                 // !=
    BangTilde,              // !~
    DollarQuestion,         // $?
    DollarTilde,            // $~
    Percent,                // %
    PercentEq,              // %=
    PercentRcurly,          // %}
    Amp,                    // &
    AmpAmp,                 // &&
    AmpAmpEq,               // &&=
    AmpStar,                // &*
    AmpStarStar,            // &**
    AmpStarEq,              // &*=
    AmpPlus,                // &+
    AmpPlusEq,              // &+=
    AmpMinus,               // &-
    AmpMinusEq,             // &-=
    AmpEq,                  // &=
    Lparen,                 // (
    Rparen,                 // )
    Star,                   // *
    StarStar,               // **
    StarStarEq,             // **=
    StarEq,                 // *=
    Plus,                   // +
    PlusEq,                 // +=
    Comma,                  // ,
    Minus,                  // -
    MinusEq,                // -=
    MinusGt,                // ->
    Period,                 // .
    PeriodPeriod,           // ..
    PeriodPeriodPeriod,     // ...
    Slash,                  // /
    SlashSlash,             // //
    SlashSlashEq,           // //=
    SlashEq,                // /=
    Colon,                  // :
    ColonColon,             // ::
    Semicolon,              // ;
    Lt,                     // <
    LtLt,                   // <<
    LtLtEq,                 // <<=
    LtEq,                   // <=
    LtEqGt,                 // <=>
    Eq,                     // =
    EqEq,                   // ==
    EqEqEq,                 // ===
    EqGt,                   // =>
    EqTilde,                // =~
    Gt,                     // >
    GtEq,                   // >=
    GtGt,                   // >>
    GtGtEq,                 // >>=
    Question,               // ?
    AtLsquare,              // @[
    Lsquare,                // [
    LsquareRsquare,         // []
    LsquareRsquareEq,       // []=
    LsquareRsquareQuestion, // []?
    Rsquare,                // ]
    Caret,                  // ^
    CaretEq,                // ^=
    Grave,                  // `
    Lcurly,                 // {
    LcurlyPercent,          // {%
    LcurlyLcurly,           // {{
    Bar,                    // |
    BarEq,                  // |=
    BarBar,                 // ||
    BarBarEq,               // ||=
    Rcurly,                 // }
    Tilde,                  // ~
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Bang => "!",
                Op::BangEq => "!=",
                Op::BangTilde => "!~",
                Op::DollarQuestion => "$?",
                Op::DollarTilde => "$~",
                Op::Percent => "%",
                Op::PercentEq => "%=",
                Op::PercentRcurly => "%}",
                Op::Amp => "&",
                Op::AmpAmp => "&&",
                Op::AmpAmpEq => "&&=",
                Op::AmpStar => "&*",
                Op::AmpStarStar => "&**",
                Op::AmpStarEq => "&*=",
                Op::AmpPlus => "&+",
                Op::AmpPlusEq => "&+=",
                Op::AmpMinus => "&-",
                Op::AmpMinusEq => "&-=",
                Op::AmpEq => "&=",
                Op::Lparen => "(",
                Op::Rparen => ")",
                Op::Star => "*",
                Op::StarStar => "**",
                Op::StarStarEq => "**=",
                Op::StarEq => "*=",
                Op::Plus => "+",
                Op::PlusEq => "+=",
                Op::Comma => ",",
                Op::Minus => "-",
                Op::MinusEq => "-=",
                Op::MinusGt => "->",
                Op::Period => ".",
                Op::PeriodPeriod => "..",
                Op::PeriodPeriodPeriod => "...",
                Op::Slash => "/",
                Op::SlashSlash => "//",
                Op::SlashSlashEq => "//=",
                Op::SlashEq => "/=",
                Op::Colon => ":",
                Op::ColonColon => "::",
                Op::Semicolon => ";",
                Op::Lt => "<",
                Op::LtLt => "<<",
                Op::LtLtEq => "<<=",
                Op::LtEq => "<=",
                Op::LtEqGt => "<=>",
                Op::Eq => "=",
                Op::EqEq => "==",
                Op::EqEqEq => "===",
                Op::EqGt => "=>",
                Op::EqTilde => "=~",
                Op::Gt => ">",
                Op::GtEq => ">=",
                Op::GtGt => ">>",
                Op::GtGtEq => ">>=",
                Op::Question => "?",
                Op::AtLsquare => "@[",
                Op::Lsquare => "[",
                Op::LsquareRsquare => "[]",
                Op::LsquareRsquareEq => "[]=",
                Op::LsquareRsquareQuestion => "[]?",
                Op::Rsquare => "]",
                Op::Caret => "^",
                Op::CaretEq => "^=",
                Op::Grave => "`",
                Op::Lcurly => "{",
                Op::LcurlyPercent => "{%",
                Op::LcurlyLcurly => "{{",
                Op::Bar => "|",
                Op::BarEq => "|=",
                Op::BarBar => "||",
                Op::BarBarEq => "||=",
                Op::Rcurly => "}",
                Op::Tilde => "~",
            }
        )
    }
}

impl Op {
    pub fn is_assignment_operator(&self) -> bool {
        match self {
            Op::PlusEq
            | Op::MinusEq
            | Op::StarEq
            | Op::SlashEq
            | Op::SlashSlashEq
            | Op::PercentEq
            | Op::BarEq
            | Op::AmpEq
            | Op::CaretEq
            | Op::StarStarEq
            | Op::LtLtEq
            | Op::GtGtEq
            | Op::BarBarEq
            | Op::AmpAmpEq
            | Op::AmpPlusEq
            | Op::AmpMinusEq
            | Op::AmpStarEq => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct MacroState {
    pub whitespace: bool,
    pub nest: usize,
    pub control_nest: usize,
    pub delimiter_state: Option<DelimiterState>,
    pub beginning_of_line: bool,
    pub yields: bool,
    pub comment: bool,
    pub heredocs: Option<Vec<DelimiterState>>,
}

impl Default for MacroState {
    fn default() -> Self {
        Self {
            whitespace: true,
            nest: 0,
            control_nest: 0,
            delimiter_state: None,
            beginning_of_line: true,
            yields: false,
            comment: false,
            heredocs: None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum DelimiterValue {
    String((char, char)),
    Regex((char, char)),
    StringArray((char, char)),
    SymbolArray((char, char)),
    Command((char, char)),
    Heredoc(String),
}

#[derive(Clone, Debug)]
pub struct DelimiterState {
    pub value: DelimiterValue,
    pub open_count: usize,
    pub heredoc_indent: usize,
    pub allow_escapes: bool,
}

impl Default for DelimiterState {
    fn default() -> Self {
        Self {
            value: DelimiterValue::String(('\0', '\0')),
            open_count: 0,
            heredoc_indent: 0,
            allow_escapes: true,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenValue {
    Char(char),
    String(Vec<char>),
    Keyword(Keyword),
    None,
}

impl From<char> for TokenValue {
    fn from(c: char) -> Self {
        TokenValue::Char(c)
    }
}

impl From<Vec<char>> for TokenValue {
    fn from(string: Vec<char>) -> Self {
        TokenValue::String(string)
    }
}

impl From<Keyword> for TokenValue {
    fn from(keyword: Keyword) -> Self {
        TokenValue::Keyword(keyword)
    }
}

impl PartialEq<char> for TokenValue {
    fn eq(&self, c: &char) -> bool {
        match self {
            TokenValue::Char(value) => value == c,
            _ => false,
        }
    }
}

impl PartialEq<Vec<char>> for TokenValue {
    fn eq(&self, string: &Vec<char>) -> bool {
        match self {
            TokenValue::String(value) => value == string,
            _ => false,
        }
    }
}

impl PartialEq<Keyword> for TokenValue {
    fn eq(&self, keyword: &Keyword) -> bool {
        match self {
            TokenValue::Keyword(value) => value == keyword,
            _ => false,
        }
    }
}

impl TokenValue {
    pub fn is_any_keyword(&self) -> bool {
        match self {
            TokenValue::Keyword(_) => true,
            _ => false,
        }
    }

    pub fn is_none(&self) -> bool {
        match self {
            TokenValue::None => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub value: TokenValue,
    pub number_kind: NumberKind,
    pub line_number: usize,
    pub column_number: usize,
    pub filename: &'a str,
    pub delimiter_state: DelimiterState,
    pub macro_state: MacroState,
    pub passed_backslash_newline: bool,
    pub doc_buffer: Option<Vec<char>>,
    pub raw: &'a [char],
    pub start: usize,
    pub invalid_escape: bool,
    location: Option<Location<'a>>,
}

impl<'a> Default for Token<'a> {
    fn default() -> Self {
        Self {
            kind: TokenKind::Eof,
            value: TokenValue::None,
            number_kind: NumberKind::I32,
            line_number: 0,
            column_number: 0,
            filename: "",
            delimiter_state: DelimiterState::default(),
            macro_state: MacroState::default(),
            passed_backslash_newline: false,
            doc_buffer: None,
            raw: &[],
            start: 0,
            invalid_escape: false,
            location: None,
        }
    }
}

impl<'a> Token<'a> {
    pub fn location(&mut self) -> &Location {
        if self.location.is_none() {
            self.location = Some(Location::new(
                self.filename.clone(),
                self.line_number,
                self.column_number,
            ));
        }
        self.location.as_ref().unwrap()
    }

    pub fn set_location(&mut self, location: Option<Location<'a>>) {
        self.location = location;
    }

    pub fn is_any_keyword(&self) -> bool {
        self.kind == TokenKind::Ident && self.value.is_any_keyword()
    }

    pub fn is_keyword(&self, keyword: Keyword) -> bool {
        self.kind == TokenKind::Ident && self.value == keyword
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.value {
            TokenValue::Char(c) => c.fmt(f),
            TokenValue::String(string) => {
                for c in string.iter() {
                    c.fmt(f)?;
                }
                Ok(())
            }
            TokenValue::Keyword(keyword) => keyword.fmt(f),
            TokenValue::None => self.kind.fmt(f),
        }
    }
}

#[test]
fn it_works() {
    assert_eq!("as?", Keyword::AsQuestion.to_string());

    let token = Token {
        value: TokenValue::String("foo".chars().collect()),
        ..Default::default()
    };
    assert_eq!("foo", token.to_string());
}
