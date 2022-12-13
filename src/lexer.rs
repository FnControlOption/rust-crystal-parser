#![allow(dead_code, unused_imports)]

use crate::char_reader::CharReader;
use crate::error::SyntaxError;
use crate::location::Location;
use crate::token::Magic::*;
use crate::token::Op::*;
use crate::token::TokenKind::*;
use crate::token::*;

type Result<'a, T> = std::result::Result<T, SyntaxError<'a>>;

pub struct Lexer<'a> {
    wants_regex: bool,
    doc_enabled: bool,
    comment_is_doc: bool,
    comments_enabled: bool,
    count_whitespace: bool,
    wants_raw: bool,
    slash_is_regex: bool,
    wants_def_or_macro_name: bool,
    wants_symbol: bool,

    reader: CharReader<'a>,
    token: Token<'a>,
    temp_token: Token<'a>,
    line_number: usize,
    column_number: usize,
    filename: &'a str,
    stacked: bool,
    stacked_filename: &'a str,
    stacked_line_number: usize,
    stacked_column_number: usize,
    token_end_location: Option<Location<'a>>,
    // string_pool
    heredocs: Vec<DelimiterState>,
    // macro_expansion_pragmas
    // warnings
}

impl<'a> Lexer<'a> {
    pub fn new(string: &'a [char]) -> Self {
        Self {
            // warnings
            reader: CharReader::new(string),
            token: Token::default(),
            temp_token: Token::default(),
            line_number: 1,
            column_number: 1,
            filename: "",
            wants_regex: true,
            doc_enabled: false,
            comment_is_doc: true,
            comments_enabled: false,
            count_whitespace: false,
            slash_is_regex: true,
            wants_raw: false,
            wants_def_or_macro_name: false,
            wants_symbol: true,
            // string_pool
            // delimiter_state_stack
            // macro_curly_count
            stacked: false,
            stacked_filename: "",
            stacked_line_number: 1,
            stacked_column_number: 1,
            token_end_location: None,
            heredocs: Vec::new(),
        }
    }

    pub fn next_token(&mut self) -> Result<'a, &Token<'a>> {
        if matches!(self.token.kind, Newline | Eof) {
            self.comment_is_doc = true;
        } else if self.token.kind != Space {
            self.comment_is_doc = false;
        }

        self.reset_token();

        // TODO: Skip comments

        let start = self.current_pos();

        // TODO: implement macro_expansion_pragmas

        let mut reset_regex_flags = true;

        match self.current_char() {
            '\0' => {
                self.token.kind = Eof;
            }
            ' ' | '\t' => {
                self.consume_whitespace()?;
                reset_regex_flags = false;
            }
            '\\' => match self.next_char() {
                '\r' | '\n' => {
                    self.handle_slash_r_slash_n_or_slash_n()?;
                    self.incr_line_number();
                    self.token.passed_backslash_newline = true;
                    self.consume_whitespace()?;
                    reset_regex_flags = false;
                }
                _ => {
                    return self.unknown_token();
                }
            },
            '\n' => {
                self.token.kind = Newline;
                self.next_char();
                self.incr_line_number();
                reset_regex_flags = false;
                self.consume_newlines()?;
            }
            '\r' => {
                if self.next_char() == '\n' {
                    self.next_char();
                    self.token.kind = Newline;
                    self.incr_line_number();
                    reset_regex_flags = false;
                    self.consume_newlines()?;
                } else {
                    return self.raise("expected '\\n' after '\\r'");
                }
            }
            '=' => match self.next_char() {
                '=' => match self.next_char() {
                    '=' => self.next_char2(Op(EqEqEq)),
                    _ => self.token.kind = Op(EqEq),
                },
                '>' => self.next_char2(Op(EqGt)),
                '~' => self.next_char2(Op(EqTilde)),
                _ => self.token.kind = Op(Eq),
            },
            '!' => match self.next_char() {
                '=' => self.next_char2(Op(BangEq)),
                '~' => self.next_char2(Op(BangTilde)),
                _ => self.token.kind = Op(Bang),
            },
            '<' => {
                match self.next_char() {
                    '=' => match self.next_char() {
                        '>' => self.next_char2(Op(LtEqGt)),
                        _ => self.token.kind = Op(LtEq),
                    },
                    '<' => {
                        match self.next_char() {
                            '=' => self.next_char2(Op(LtLtEq)),
                            '-' => {
                                // TODO: implement consume_heredoc_start
                                return self.unknown_token();
                            }
                            _ => self.token.kind = Op(LtLt),
                        }
                    }
                    _ => self.token.kind = Op(Lt),
                }
            }
            '>' => match self.next_char() {
                '=' => self.next_char2(Op(GtEq)),
                '>' => match self.next_char() {
                    '=' => self.next_char2(Op(GtGtEq)),
                    _ => self.token.kind = Op(GtGt),
                },
                _ => self.token.kind = Op(Gt),
            },
            '+' => {
                self.token.start = start;
                match self.next_char() {
                    '=' => self.next_char2(Op(PlusEq)),
                    '0'..='9' => self.scan_number(start),
                    '+' => return self.raise("postfix increment is not supported, use `exp += 1`"),
                    _ => self.token.kind = Op(Plus),
                }
            }
            '-' => {
                self.token.start = start;
                match self.next_char() {
                    '=' => self.next_char2(Op(MinusEq)),
                    '>' => self.next_char2(Op(MinusGt)),
                    '0'..='9' => self.scan_number2(start, true),
                    '+' => return self.raise("postfix decrement is not supported, use `exp -= 1`"),
                    _ => self.token.kind = Op(Minus),
                }
            }
            '*' => match self.next_char() {
                '=' => self.next_char2(Op(StarEq)),
                '*' => match self.next_char() {
                    '=' => self.next_char2(Op(StarStarEq)),
                    _ => self.token.kind = Op(StarStar),
                },
                _ => self.token.kind = Op(Star),
            },
            '/' => match self.next_char() {
                '/' if self.wants_def_or_macro_name || !self.slash_is_regex => {
                    match self.next_char() {
                        '=' => {
                            self.next_char2(Op(SlashSlashEq));
                        }
                        _ => {
                            self.token.kind = Op(SlashSlash);
                        }
                    }
                }
                '=' if !self.slash_is_regex => {
                    self.next_char2(Op(SlashEq));
                }
                _ if self.wants_def_or_macro_name => {
                    self.token.kind = Op(Slash);
                }
                _ if self.slash_is_regex => {
                    self.delimited_pair_no_advance(DelimiterValue::Regex(('/', '/')), start);
                }
                c if c.is_ascii_whitespace() || c == '\0' => {
                    self.token.kind = Op(Slash);
                }
                _ if self.wants_regex => {
                    self.delimited_pair_no_advance(DelimiterValue::Regex(('/', '/')), start);
                }
                _ => {
                    self.token.kind = Op(Slash);
                }
            },
            '%' => {
                let c = self.next_char();
                match c {
                    _ if self.wants_def_or_macro_name => {
                        self.next_char2(Op(Percent));
                    }
                    '=' => {
                        self.next_char2(Op(PercentEq));
                    }
                    '(' | '[' | '{' | '<' | '|' => {
                        self.delimited_pair(DelimiterValue::String((c, closing_char(c))), start)
                    }
                    'i' => {
                        let c = self.peek_next_char();
                        if matches!(c, '(' | '{' | '[' | '<' | '|') {
                            self.next_char();
                            self.next_char2(SymbolArrayStart);
                            self.set_token_raw_from_start(start);
                            self.token.delimiter_state = DelimiterState {
                                value: DelimiterValue::SymbolArray((c, closing_char(c))),
                                ..Default::default()
                            };
                        } else {
                            self.token.kind = Op(Percent);
                        }
                    }
                    'q' => {
                        let c = self.peek_next_char();
                        if matches!(c, '(' | '{' | '[' | '<' | '|') {
                            self.next_char();
                            self.delimited_pair(
                                DelimiterValue::String((c, closing_char(c))),
                                start,
                            );
                            self.token.delimiter_state.allow_escapes = false;
                        } else {
                            self.token.kind = Op(Percent);
                        }
                    }
                    'Q' => {
                        let c = self.peek_next_char();
                        if matches!(c, '(' | '{' | '[' | '<' | '|') {
                            self.next_char();
                            self.delimited_pair(
                                DelimiterValue::String((c, closing_char(c))),
                                start,
                            );
                        } else {
                            self.token.kind = Op(Percent);
                        }
                    }
                    'r' => {
                        let c = self.next_char();
                        if matches!(c, '(' | '[' | '{' | '<' | '|') {
                            self.delimited_pair(DelimiterValue::Regex((c, closing_char(c))), start);
                        } else {
                            return self.raise("unknown %r char");
                        }
                    }
                    'x' => {
                        let c = self.next_char();
                        if matches!(c, '(' | '[' | '{' | '<' | '|') {
                            self.delimited_pair(
                                DelimiterValue::Command((c, closing_char(c))),
                                start,
                            );
                        } else {
                            return self.raise("unknown %x char");
                        }
                    }
                    'w' => {
                        let c = self.peek_next_char();
                        if matches!(c, '(' | '{' | '[' | '<' | '|') {
                            self.next_char();
                            self.next_char2(StringArrayStart);
                            self.set_token_raw_from_start(start);
                            self.token.delimiter_state = DelimiterState {
                                value: DelimiterValue::StringArray((c, closing_char(c))),
                                ..Default::default()
                            };
                        } else {
                            self.token.kind = Op(Percent);
                        }
                    }
                    '}' => {
                        self.next_char2(Op(PercentRcurly));
                    }
                    _ => {
                        self.token.kind = Op(Percent);
                    }
                }
            }
            '(' => self.next_char2(Op(Lparen)),
            ')' => self.next_char2(Op(Rparen)),
            '{' => match self.next_char() {
                '%' => self.next_char2(Op(LcurlyPercent)),
                '{' => self.next_char2(Op(LcurlyLcurly)),
                _ => self.token.kind = Op(Lcurly),
            },
            '}' => self.next_char2(Op(Rcurly)),
            '[' => match self.next_char() {
                ']' => match self.next_char() {
                    '=' => self.next_char2(Op(LsquareRsquareEq)),
                    '?' => self.next_char2(Op(LsquareRsquareQuestion)),
                    _ => self.token.kind = Op(LsquareRsquare),
                },
                _ => self.token.kind = Op(Lsquare),
            },
            ']' => self.next_char2(Op(Rsquare)),
            ',' => self.next_char2(Op(Comma)),
            '?' => self.next_char2(Op(Question)),
            ';' => {
                reset_regex_flags = false;
                self.next_char2(Op(Semicolon));
            }
            ':' => {
                if self.next_char() == ':' {
                    self.next_char2(Op(ColonColon));
                } else if self.wants_symbol {
                    self.consume_symbol()?;
                } else {
                    self.token.kind = Op(Colon);
                }
            }
            '~' => self.next_char2(Op(Tilde)),
            '.' => {
                let line = self.line_number;
                let column = self.column_number;
                match self.next_char() {
                    '.' => match self.next_char() {
                        '.' => self.next_char2(Op(PeriodPeriodPeriod)),
                        _ => self.token.kind = Op(PeriodPeriod),
                    },
                    c if c.is_ascii_digit() => {
                        return self.raise_at(
                            ".1 style number literal is not supported, put 0 before dot",
                            line,
                            column,
                        )
                    }
                    _ => self.token.kind = Op(Period),
                }
            }
            '&' => match self.next_char() {
                '&' => match self.next_char() {
                    '=' => self.next_char2(Op(AmpAmpEq)),
                    _ => self.token.kind = Op(AmpAmp),
                },
                '=' => self.next_char2(Op(AmpEq)),
                '+' => match self.next_char() {
                    '=' => self.next_char2(Op(AmpPlusEq)),
                    _ => self.token.kind = Op(AmpPlus),
                },
                '-' => {
                    // Check if '>' comes after '&-', making it '&->'.
                    // We want to parse that like '&(->...)',
                    // so we only return '&' for now.
                    if self.peek_next_char() == '>' {
                        self.token.kind = Op(Amp);
                    } else {
                        match self.next_char() {
                            '=' => self.next_char2(Op(AmpMinusEq)),
                            _ => self.token.kind = Op(AmpMinus),
                        }
                    }
                }
                '*' => match self.next_char() {
                    '*' => self.next_char2(Op(AmpStarStar)),
                    '=' => self.next_char2(Op(AmpStarEq)),
                    _ => self.token.kind = Op(AmpStar),
                },
                _ => self.token.kind = Op(Amp),
            },
            '|' => match self.next_char() {
                '|' => match self.next_char() {
                    '=' => self.next_char2(Op(BarBarEq)),
                    _ => self.token.kind = Op(BarBar),
                },
                '=' => self.next_char2(Op(BarEq)),
                _ => self.token.kind = Op(Bar),
            },
            '^' => match self.next_char() {
                '=' => self.next_char2(Op(CaretEq)),
                _ => self.token.kind = Op(Caret),
            },
            '\'' => {
                let start = self.current_pos();
                let line = self.line_number;
                let column = self.column_number;
                self.token.kind = Char;
                match self.next_char() {
                    '\\' => match self.next_char() {
                        '\\' => {
                            self.token.value = TokenValue::Char('\\');
                        }
                        '\'' => {
                            self.token.value = TokenValue::Char('\'');
                        }
                        'a' => {
                            self.token.value = char::from(7).into();
                        }
                        'b' => {
                            self.token.value = char::from(8).into();
                        }
                        'e' => {
                            self.token.value = char::from(27).into();
                        }
                        'f' => {
                            self.token.value = char::from(12).into();
                        }
                        'n' => {
                            self.token.value = TokenValue::Char('\n');
                        }
                        'r' => {
                            self.token.value = TokenValue::Char('\r');
                        }
                        't' => {
                            self.token.value = TokenValue::Char('\t');
                        }
                        'v' => {
                            self.token.value = char::from(11).into();
                        }
                        'u' => {
                            // TODO: implement consume_char_unicode_escape
                            return self.unknown_token();
                        }
                        '0' => {
                            self.token.value = TokenValue::Char('\0');
                        }
                        '\0' => {
                            return self.raise_at("unterminated char literal", line, column);
                        }
                        c => {
                            return self.raise_at(
                                &format!("invalid char escape sequence '\\{c}'"),
                                line,
                                column,
                            );
                        }
                    },
                    '\'' => {
                        return self.raise_at(
                            "invalid empty char literal (did you mean '\\''?)",
                            line,
                            column,
                        );
                    }
                    '\0' => {
                        return self.raise_at("unterminated char literal", line, column);
                    }
                    c => {
                        self.token.value = c.into();
                    }
                }
                if self.next_char() != '\'' {
                    return self.raise_at(
                        "unterminated char literal, use double quotes for strings",
                        line,
                        column,
                    );
                }
                self.next_char();
                self.set_token_raw_from_start(start);
            }
            '`' => {
                if self.wants_def_or_macro_name {
                    self.next_char2(Op(Grave));
                } else {
                    self.delimited_pair(DelimiterValue::Command(('`', '`')), start);
                }
            }
            '"' => {
                self.delimited_pair(DelimiterValue::String(('"', '"')), start);
            }
            '0'..='9' => {
                self.scan_number(start);
            }
            '@' => {
                let start = self.current_pos();
                match self.next_char() {
                    '[' => {
                        self.next_char2(Op(AtLsquare));
                    }
                    '@' => {
                        self.consume_variable(ClassVar, start)?;
                    }
                    _ => {
                        self.consume_variable(InstanceVar, start)?;
                    }
                }
            }
            '$' => {
                let start = self.current_pos();
                self.next_char();
                match self.current_char() {
                    '~' => {
                        self.next_char2(Op(DollarTilde));
                    }
                    '?' => {
                        self.next_char2(Op(DollarQuestion));
                    }
                    c if c.is_ascii_digit() => {
                        let start = self.current_pos();
                        if self.current_char() == '0' {
                            self.next_char();
                        } else {
                            while self.next_char().is_ascii_digit() {
                                // Nothing to do
                            }
                            if self.current_char() == '?' {
                                self.next_char();
                            }
                        }
                        self.token.kind = GlobalMatchDataIndex;
                        self.token.value = self.string_range(start);
                    }
                    _ => {
                        self.consume_variable(Global, start)?;
                    }
                }
            }
            'a' => {
                match self.next_char() {
                    'b' => {
                        if self.char_sequence(&['s', 't', 'r', 'a', 'c', 't']) {
                            return self.check_ident_or_keyword(Keyword::Abstract, start);
                        }
                    }
                    'l' => {
                        if self.char_sequence(&['i', 'a', 's']) {
                            return self.check_ident_or_keyword(Keyword::Alias, start);
                        }
                    }
                    's' => match self.peek_next_char() {
                        'm' => {
                            self.next_char();
                            return self.check_ident_or_keyword(Keyword::Asm, start);
                        }
                        '?' => {
                            self.next_char();
                            self.next_char();
                            self.token.kind = Ident;
                            self.token.value = Keyword::AsQuestion.into();
                            return Ok(&self.token);
                        }
                        _ => {
                            return self.check_ident_or_keyword(Keyword::As, start);
                        }
                    },
                    'n' => {
                        if self.char_sequence(&['n', 'o', 't', 'a', 't', 'i', 'o', 'n']) {
                            return self.check_ident_or_keyword(Keyword::Annotation, start);
                        }
                    }
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            'b' => {
                match self.next_char() {
                    'e' => {
                        if self.char_sequence(&['g', 'i', 'n']) {
                            return self.check_ident_or_keyword(Keyword::Begin, start);
                        }
                    }
                    'r' => {
                        if self.char_sequence(&['e', 'a', 'k']) {
                            return self.check_ident_or_keyword(Keyword::Break, start);
                        }
                    }
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            'c' => {
                match self.next_char() {
                    'a' => {
                        if self.char_sequence(&['s', 'e']) {
                            return self.check_ident_or_keyword(Keyword::Case, start);
                        }
                    }
                    'l' => {
                        if self.char_sequence(&['a', 's', 's']) {
                            return self.check_ident_or_keyword(Keyword::Class, start);
                        }
                    }
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            'd' => {
                match self.next_char() {
                    'e' => {
                        if self.next_char() == 'f' {
                            return self.check_ident_or_keyword(Keyword::Def, start);
                        }
                    }
                    'o' => {
                        return self.check_ident_or_keyword(Keyword::Do, start);
                    }
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            'e' => {
                match self.next_char() {
                    'l' => match self.next_char() {
                        's' => match self.next_char() {
                            'e' => {
                                return self.check_ident_or_keyword(Keyword::Else, start);
                            }
                            'i' => {
                                if self.next_char() == 'f' {
                                    return self.check_ident_or_keyword(Keyword::Elsif, start);
                                }
                            }
                            _ => {} // scan_ident
                        },
                        _ => {} // scan_ident
                    },
                    'n' => match self.next_char() {
                        'd' => {
                            return self.check_ident_or_keyword(Keyword::End, start);
                        }
                        's' => {
                            if self.char_sequence(&['u', 'r', 'e']) {
                                return self.check_ident_or_keyword(Keyword::Ensure, start);
                            }
                        }
                        'u' => {
                            if self.next_char() == 'm' {
                                return self.check_ident_or_keyword(Keyword::Enum, start);
                            }
                        }
                        _ => {} // scan_ident
                    },
                    'x' => {
                        if self.char_sequence(&['t', 'e', 'n', 'd']) {
                            return self.check_ident_or_keyword(Keyword::Extend, start);
                        }
                    }
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            'f' => {
                match self.next_char() {
                    'a' => {
                        if self.char_sequence(&['l', 's', 'e']) {
                            return self.check_ident_or_keyword(Keyword::False, start);
                        }
                    }
                    'o' => {
                        if self.next_char() == 'r' {
                            return self.check_ident_or_keyword(Keyword::For, start);
                        }
                    }
                    'u' => {
                        if self.next_char() == 'n' {
                            return self.check_ident_or_keyword(Keyword::Fun, start);
                        }
                    }
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            'i' => {
                match self.next_char() {
                    'f' => {
                        return self.check_ident_or_keyword(Keyword::If, start);
                    }
                    'n' => {
                        if is_ident_part_or_end(self.peek_next_char()) {
                            match self.next_char() {
                                'c' => {
                                    if self.char_sequence(&['l', 'u', 'd', 'e']) {
                                        return self
                                            .check_ident_or_keyword(Keyword::Include, start);
                                    }
                                }
                                's' => {
                                    if self.char_sequence(&[
                                        't', 'a', 'n', 'c', 'e', '_', 's', 'i', 'z', 'e', 'o', 'f',
                                    ]) {
                                        return self.check_ident_or_keyword(
                                            Keyword::InstanceSizeof,
                                            start,
                                        );
                                    }
                                }
                                _ => {} // scan_ident
                            }
                        } else {
                            self.next_char();
                            self.token.kind = Ident;
                            self.token.value = Keyword::In.into();
                            return Ok(&self.token);
                        }
                    }
                    's' => {
                        if self.char_sequence(&['_', 'a', '?']) {
                            return self.check_ident_or_keyword(Keyword::IsAQuestion, start);
                        }
                    }
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            'l' => {
                match self.next_char() {
                    'i' => {
                        if self.next_char() == 'b' {
                            return self.check_ident_or_keyword(Keyword::Lib, start);
                        }
                    }
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            'm' => {
                match self.next_char() {
                    'a' => {
                        if self.char_sequence(&['c', 'r', 'o']) {
                            return self.check_ident_or_keyword(Keyword::Macro, start);
                        }
                    }
                    'o' => match self.next_char() {
                        'd' => {
                            if self.char_sequence(&['u', 'l', 'e']) {
                                return self.check_ident_or_keyword(Keyword::Module, start);
                            }
                        }
                        _ => {} // scan_ident
                    },
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            'n' => {
                match self.next_char() {
                    'e' => {
                        if self.char_sequence(&['x', 't']) {
                            return self.check_ident_or_keyword(Keyword::Next, start);
                        }
                    }
                    'i' => match self.next_char() {
                        'l' => {
                            if self.peek_next_char() == '?' {
                                self.next_char();
                                return self.check_ident_or_keyword(Keyword::NilQuestion, start);
                            } else {
                                return self.check_ident_or_keyword(Keyword::Nil, start);
                            }
                        }
                        _ => {} // scan_ident
                    },
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            'o' => {
                match self.next_char() {
                    'f' => {
                        if self.peek_next_char() == 'f' {
                            self.next_char();
                            if self.char_sequence(&['s', 'e', 't', 'o', 'f']) {
                                return self.check_ident_or_keyword(Keyword::Offsetof, start);
                            }
                        } else {
                            return self.check_ident_or_keyword(Keyword::Of, start);
                        }
                    }
                    'u' => {
                        if self.next_char() == 't' {
                            return self.check_ident_or_keyword(Keyword::Out, start);
                        }
                    }
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            'p' => {
                match self.next_char() {
                    'o' => {
                        if self.char_sequence(&['i', 'n', 't', 'e', 'r', 'o', 'f']) {
                            return self.check_ident_or_keyword(Keyword::Pointerof, start);
                        }
                    }
                    'r' => match self.next_char() {
                        'i' => {
                            if self.char_sequence(&['v', 'a', 't', 'e']) {
                                return self.check_ident_or_keyword(Keyword::Private, start);
                            }
                        }
                        'o' => {
                            if self.char_sequence(&['t', 'e', 'c', 't', 'e', 'd']) {
                                return self.check_ident_or_keyword(Keyword::Protected, start);
                            }
                        }
                        _ => {} // scan_ident
                    },
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            'r' => {
                match self.next_char() {
                    'e' => match self.next_char() {
                        's' => match self.next_char() {
                            'c' => {
                                if self.char_sequence(&['u', 'e']) {
                                    return self.check_ident_or_keyword(Keyword::Rescue, start);
                                }
                            }
                            'p' => {
                                if self.char_sequence(&['o', 'n', 'd', 's', '_', 't', 'o', '?']) {
                                    return self.check_ident_or_keyword(
                                        Keyword::RespondsToQuestion,
                                        start,
                                    );
                                }
                            }
                            _ => {} // scan_ident
                        },
                        't' => {
                            if self.char_sequence(&['u', 'r', 'n']) {
                                return self.check_ident_or_keyword(Keyword::Return, start);
                            }
                        }
                        'q' => {
                            if self.char_sequence(&['u', 'i', 'r', 'e']) {
                                return self.check_ident_or_keyword(Keyword::Require, start);
                            }
                        }
                        _ => {} // scan_ident
                    },
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            's' => {
                match self.next_char() {
                    'e' => {
                        if self.next_char() == 'l' {
                            match self.next_char() {
                                'e' => {
                                    if self.char_sequence(&['c', 't']) {
                                        return self.check_ident_or_keyword(Keyword::Select, start);
                                    }
                                }
                                'f' => {
                                    return self.check_ident_or_keyword(Keyword::Self_, start);
                                }
                                _ => {} // scan_ident
                            }
                        }
                    }
                    'i' => {
                        if self.char_sequence(&['z', 'e', 'o', 'f']) {
                            return self.check_ident_or_keyword(Keyword::Sizeof, start);
                        }
                    }
                    't' => {
                        if self.char_sequence(&['r', 'u', 'c', 't']) {
                            return self.check_ident_or_keyword(Keyword::Struct, start);
                        }
                    }
                    'u' => {
                        if self.char_sequence(&['p', 'e', 'r']) {
                            return self.check_ident_or_keyword(Keyword::Super, start);
                        }
                    }
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            't' => {
                match self.next_char() {
                    'h' => {
                        if self.char_sequence(&['e', 'n']) {
                            return self.check_ident_or_keyword(Keyword::Then, start);
                        }
                    }
                    'r' => {
                        if self.char_sequence(&['u', 'e']) {
                            return self.check_ident_or_keyword(Keyword::True, start);
                        }
                    }
                    'y' => {
                        if self.char_sequence(&['p', 'e']) {
                            if self.peek_next_char() == 'o' {
                                self.next_char();
                                if self.next_char() == 'f' {
                                    return self.check_ident_or_keyword(Keyword::Typeof, start);
                                }
                            } else {
                                return self.check_ident_or_keyword(Keyword::Type, start);
                            }
                        }
                    }
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            'u' => {
                if self.next_char() == 'n' {
                    match self.next_char() {
                        'i' => match self.next_char() {
                            'o' => {
                                if self.next_char() == 'n' {
                                    return self.check_ident_or_keyword(Keyword::Union, start);
                                }
                            }
                            'n' => {
                                if self
                                    .char_sequence(&['i', 't', 'i', 'a', 'l', 'i', 'z', 'e', 'd'])
                                {
                                    return self
                                        .check_ident_or_keyword(Keyword::Uninitialized, start);
                                }
                            }
                            _ => {} // scan_ident
                        },
                        'l' => {
                            if self.char_sequence(&['e', 's', 's']) {
                                return self.check_ident_or_keyword(Keyword::Unless, start);
                            }
                        }
                        't' => {
                            if self.char_sequence(&['i', 'l']) {
                                return self.check_ident_or_keyword(Keyword::Until, start);
                            }
                        }
                        _ => {} // scan_ident
                    }
                }
                self.scan_ident(start);
            }
            'v' => {
                if self.char_sequence(&['e', 'r', 'b', 'a', 't', 'i', 'm']) {
                    return self.check_ident_or_keyword(Keyword::Verbatim, start);
                }
                self.scan_ident(start);
            }
            'w' => {
                match self.next_char() {
                    'h' => match self.next_char() {
                        'e' => {
                            if self.next_char() == 'n' {
                                return self.check_ident_or_keyword(Keyword::When, start);
                            }
                        }
                        'i' => {
                            if self.char_sequence(&['l', 'e']) {
                                return self.check_ident_or_keyword(Keyword::While, start);
                            }
                        }
                        _ => {} // scan_ident
                    },
                    'i' => {
                        if self.char_sequence(&['t', 'h']) {
                            return self.check_ident_or_keyword(Keyword::With, start);
                        }
                    }
                    _ => {} // scan_ident
                }
                self.scan_ident(start);
            }
            'y' => {
                if self.char_sequence(&['i', 'e', 'l', 'd']) {
                    return self.check_ident_or_keyword(Keyword::Yield, start);
                }
                self.scan_ident(start);
            }
            '_' => {
                match self.next_char() {
                    '_' => {
                        match self.next_char() {
                            'D' => {
                                if self.char_sequence(&['I', 'R', '_', '_']) {
                                    if !is_ident_part_or_end(self.peek_next_char()) {
                                        self.next_char();
                                        self.token.kind = Magic(Dir);
                                        return Ok(&self.token);
                                    }
                                }
                            }
                            'E' => {
                                if self
                                    .char_sequence(&['N', 'D', '_', 'L', 'I', 'N', 'E', '_', '_'])
                                {
                                    if !is_ident_part_or_end(self.peek_next_char()) {
                                        self.next_char();
                                        self.token.kind = Magic(EndLine);
                                        return Ok(&self.token);
                                    }
                                }
                            }
                            'F' => {
                                if self.char_sequence(&['I', 'L', 'E', '_', '_']) {
                                    if !is_ident_part_or_end(self.peek_next_char()) {
                                        self.next_char();
                                        self.token.kind = Magic(File);
                                        return Ok(&self.token);
                                    }
                                }
                            }
                            'L' => {
                                if self.char_sequence(&['I', 'N', 'E', '_', '_']) {
                                    if !is_ident_part_or_end(self.peek_next_char()) {
                                        self.next_char();
                                        self.token.kind = Magic(Line);
                                        return Ok(&self.token);
                                    }
                                }
                            }
                            _ => {} // scan_ident
                        }
                    }
                    _ => {
                        if !is_ident_part(self.current_char()) {
                            self.token.kind = Underscore;
                            return Ok(&self.token);
                        }
                    }
                }
                self.scan_ident(start);
            }
            _ => {
                if self.current_char().is_ascii_uppercase() {
                    let start = self.current_pos();
                    while is_ident_part(self.next_char()) {
                        // Nothing to do
                    }
                    self.token.kind = Const;
                    self.token.value = self.string_range(start);
                } else if is_ident_start(self.current_char()) {
                    self.next_char();
                    self.scan_ident(start);
                } else {
                    return self.unknown_token();
                }
            }
        }

        if reset_regex_flags {
            self.wants_regex = true;
            self.slash_is_regex = false;
        }

        Ok(&self.token)
    }

    fn token_end_location(&mut self) -> &Location {
        if self.token_end_location.is_none() {
            self.token_end_location = Some(Location::new(
                self.filename,
                self.line_number,
                self.column_number - 1,
            ))
        }
        self.token_end_location.as_ref().unwrap()
    }

    fn consume_comment(&mut self, start_pos: usize) {
        self.skip_comment();
        self.token.kind = Comment;
        self.token.value = self.string_range(start_pos);
    }

    fn consume_doc(&mut self) {
        if self.current_char() == ' ' {
            self.next_char();
        }

        let start_pos = self.current_pos();

        self.skip_comment();

        let slice = self.slice_range(start_pos);

        match &mut self.token.doc_buffer {
            Some(doc_buffer) => {
                doc_buffer.push('\n');
            }
            None => {
                self.token.doc_buffer = Some(Vec::new());
            }
        }

        let doc_buffer = self.token.doc_buffer.as_mut().unwrap();
        doc_buffer.extend_from_slice(slice)
    }

    fn skip_comment(&mut self) {
        let mut c = self.current_char();
        while c != '\n' && c != '\0' {
            c = self.next_char_no_column_increment();
        }
    }

    fn consume_whitespace(&mut self) -> Result<'a, ()> {
        let start_pos = self.current_pos();
        self.token.kind = Space;
        self.next_char();
        loop {
            match self.current_char() {
                ' ' | '\t' => {
                    self.next_char();
                }
                '\\' => {
                    if matches!(self.next_char(), '\r' | '\n') {
                        self.handle_slash_r_slash_n_or_slash_n()?;
                        self.next_char();
                        self.incr_line_number();
                        self.token.passed_backslash_newline = true;
                    } else {
                        return self.unknown_token();
                    }
                }
                _ => {
                    break;
                }
            }
        }
        if self.count_whitespace {
            self.token.value = self.string_range(start_pos);
        }
        Ok(())
    }

    fn consume_newlines(&mut self) -> Result<'a, ()> {
        if !self.heredocs.is_empty() {
            return Ok(());
        }

        if self.count_whitespace {
            return Ok(());
        }

        loop {
            match self.current_char() {
                '\n' => {
                    self.next_char_no_column_increment();
                    self.incr_line_number_no_column();
                    self.token.doc_buffer = None;
                }
                '\r' => {
                    if self.next_char_no_column_increment() != '\n' {
                        return self.raise("expected '\\n' after '\\r'");
                    }
                    self.next_char_no_column_increment();
                    self.incr_line_number_no_column();
                    self.token.doc_buffer = None;
                }
                _ => {
                    break;
                }
            }
        }

        Ok(())
    }

    fn check_ident_or_keyword(&mut self, keyword: Keyword, start: usize) -> Result<'a, &Token<'a>> {
        if is_ident_part_or_end(self.peek_next_char()) {
            self.scan_ident(start);
        } else {
            self.next_char();
            self.token.kind = Ident;
            self.token.value = keyword.into();
        }
        Ok(&self.token)
    }

    fn scan_ident(&mut self, start: usize) {
        while is_ident_part(self.current_char()) {
            self.next_char();
        }
        if matches!(self.current_char(), '?' | '!') && self.peek_next_char() != '=' {
            self.next_char();
        }
        self.token.kind = Ident;
        self.token.value = self.string_range(start);
    }

    fn next_char_and_symbol(&mut self, value: &str) {
        self.next_char();
        self.symbol(value);
    }

    fn symbol(&mut self, value: &str) {
        let start = self.current_pos() - value.len();
        debug_assert_eq!(
            Vec::from(self.slice_range(start)),
            Vec::from_iter(value.chars())
        );
        self.token.kind = Symbol;
        self.token.value = self.string_range(start);
        if self.wants_raw {
            self.token.raw = self.slice_range(start - 1);
        }
    }

    fn scan_number(&mut self, start: usize) {
        self.scan_number2(start, false);
    }

    fn scan_number2(&mut self, start: usize, _negative: bool) {
        // TODO: implement
        while self.next_char().is_ascii_digit() {}
        self.token.kind = Number;
        self.set_token_raw_from_start(start);
        self.token.value = self.string_range(start);
    }

    fn delimited_pair(&mut self, value: DelimiterValue, start: usize) {
        self.next_char();
        self.delimited_pair_no_advance(value, start);
    }

    fn delimited_pair_no_advance(&mut self, value: DelimiterValue, start: usize) {
        self.token.kind = DelimiterStart;
        self.token.delimiter_state = DelimiterState {
            value,
            ..Default::default()
        };
        self.set_token_raw_from_start(start);
    }

    fn consume_symbol(&mut self) -> Result<'a, ()> {
        match self.current_char() {
            ':' => self.next_char2(Op(ColonColon)),
            '+' => self.next_char_and_symbol("+"),
            '-' => self.next_char_and_symbol("-"),
            '*' => {
                if self.next_char() == '*' {
                    self.next_char_and_symbol("**");
                } else {
                    self.symbol("*");
                }
            }
            '/' => {
                if self.next_char() == '/' {
                    self.next_char_and_symbol("//");
                } else {
                    self.symbol("/");
                }
            }
            '=' => match self.next_char() {
                '=' => {
                    if self.next_char() == '=' {
                        self.next_char_and_symbol("===");
                    } else {
                        self.symbol("==");
                    }
                }
                '~' => self.next_char_and_symbol("=~"),
                _ => return self.unknown_token(),
            },
            '!' => match self.next_char() {
                '=' => self.next_char_and_symbol("!="),
                '~' => self.next_char_and_symbol("!~"),
                _ => self.symbol("!"),
            },
            '<' => match self.next_char() {
                '=' => {
                    if self.next_char() == '>' {
                        self.next_char_and_symbol("<=>");
                    } else {
                        self.symbol("<=");
                    }
                }
                '<' => self.next_char_and_symbol("<<"),
                _ => self.symbol("<"),
            },
            '>' => match self.next_char() {
                '=' => self.next_char_and_symbol(">="),
                '>' => self.next_char_and_symbol(">>"),
                _ => self.symbol(">"),
            },
            '&' => match self.next_char() {
                '+' => self.next_char_and_symbol("&+"),
                '-' => self.next_char_and_symbol("&-"),
                '*' => {
                    if self.next_char() == '*' {
                        self.next_char_and_symbol("&**");
                    } else {
                        self.symbol("&*");
                    }
                }
                _ => self.symbol("&"),
            },
            '|' => self.next_char_and_symbol("|"),
            '^' => self.next_char_and_symbol("^"),
            '~' => self.next_char_and_symbol("~"),
            '%' => self.next_char_and_symbol("%"),
            '[' => {
                if self.next_char() == ']' {
                    match self.next_char() {
                        '=' => self.next_char_and_symbol("[]="),
                        '?' => self.next_char_and_symbol("[]?"),
                        _ => self.symbol("[]"),
                    }
                } else {
                    return self.unknown_token();
                }
            }
            '"' => {
                let line = self.line_number;
                let column = self.column_number;
                let start = self.current_pos() + 1;
                let mut string = Vec::<char>::new();
                loop {
                    match self.next_char() {
                        '\\' => match self.next_char() {
                            'a' => string.push(char::from(7)),
                            'b' => string.push(char::from(8)),
                            'e' => string.push(char::from(27)),
                            'f' => string.push(char::from(12)),
                            'n' => string.push('\n'),
                            'r' => string.push('\r'),
                            't' => string.push('\t'),
                            'v' => string.push(char::from(11)),
                            'x' => {
                                // TODO: implement consume_string_hex_escape
                                return self.unknown_token();
                            }
                            'u' => {
                                // TODO: implement consume_string_unicode_escape
                                return self.unknown_token();
                            }
                            '0'..='7' => {
                                // TODO: implement consume_octal_escape
                                return self.unknown_token();
                            }
                            '\n' => {
                                self.incr_line_number_no_column();
                                string.push('\n');
                            }
                            '\0' => {
                                return self.raise_at("unterminated quoted symbol", line, column);
                            }
                            c => string.push(c),
                        },
                        '"' => break,
                        '\0' => return self.raise_at("unterminated quoted symbol", line, column),
                        c => string.push(c),
                    }
                }

                self.token.kind = Symbol;
                self.token.value = string.into();
                self.next_char();
                self.set_token_raw_from_start(start - 2);
            }
            c if is_ident_start(c) => {
                let start = self.current_pos();
                while is_ident_part(self.next_char()) {
                    // Nothing to do
                }
                if self.current_char() == '?'
                    || (matches!(self.current_char(), '!' | '=') && self.peek_next_char() != '=')
                {
                    self.next_char();
                }
                self.token.kind = Symbol;
                self.token.value = self.string_range(start);
                self.set_token_raw_from_start(start - 1);
            }
            _ => {
                self.token.kind = Op(Colon);
            }
        }

        Ok(())
    }

    fn consume_variable(&mut self, token_kind: TokenKind, start: usize) -> Result<'a, ()> {
        if is_ident_start(self.current_char()) {
            while is_ident_part(self.next_char()) {
                // Nothing to do
            }
            self.token.kind = token_kind;
            self.token.value = self.string_range(start);
            Ok(())
        } else {
            self.unknown_token()
        }
    }

    fn incr_column_number(&mut self) {
        self.incr_column_number_by(1);
    }

    fn incr_column_number_by(&mut self, d: usize) {
        self.column_number += d;
        if self.stacked {
            self.stacked_column_number += d;
        }
    }

    fn incr_line_number(&mut self) {
        self.incr_line_number_no_column();
        self.set_column_number(1);
    }

    fn incr_line_number_no_column(&mut self) {
        self.line_number += 1;
        if self.stacked {
            self.stacked_line_number += 1;
        }
    }

    fn set_column_number(&mut self, column_number: usize) {
        self.column_number = column_number;
        if self.stacked {
            self.stacked_column_number = column_number;
        }
    }

    fn next_char_no_column_increment(&mut self) -> char {
        self.reader.next_char()
    }

    fn next_char(&mut self) -> char {
        self.incr_column_number();
        self.next_char_no_column_increment()
    }

    fn next_char_check_line(&mut self) -> char {
        let char = self.next_char_no_column_increment();
        if char == '\n' {
            self.incr_line_number();
        } else {
            self.incr_column_number();
        }
        char
    }

    fn next_char2(&mut self, token_kind: TokenKind) {
        self.next_char();
        self.token.kind = token_kind;
    }

    fn reset_token(&mut self) {
        self.token.value = TokenValue::None;
        self.token.line_number = self.line_number;
        self.token.column_number = self.column_number;
        self.token.filename = self.filename;
        self.token.set_location(None);
        self.token.passed_backslash_newline = false;
        if !matches!(self.token.kind, Space | Newline) {
            self.token.doc_buffer = None;
        }
        self.token.invalid_escape = false;
        self.token_end_location = None;
    }

    fn next_token_skip_space(&mut self) -> Result<'a, ()> {
        self.next_token()?;
        self.skip_space()?;
        Ok(())
    }

    fn next_token_skip_space_or_newline(&mut self) -> Result<'a, ()> {
        self.next_token()?;
        self.skip_space_or_newline()?;
        Ok(())
    }

    fn next_token_skip_statement_end(&mut self) -> Result<'a, ()> {
        self.next_token()?;
        self.skip_statement_end()?;
        Ok(())
    }

    fn next_token_never_a_symbol(&mut self) -> Result<'a, &Token<'a>> {
        self.wants_symbol = false;
        self.next_token()?;
        self.wants_symbol = true;
        Ok(&self.token)
    }

    fn current_char(&self) -> char {
        self.reader.current_char()
    }

    fn peek_next_char(&self) -> char {
        self.reader.peek_next_char()
    }

    fn current_pos(&self) -> usize {
        self.reader.pos()
    }

    fn set_current_pos(&mut self, pos: usize) {
        self.reader.set_pos(pos);
    }

    fn string_range(&self, start_pos: usize) -> TokenValue {
        self.string_range2(start_pos, self.current_pos())
    }

    fn string_range2(&self, start_pos: usize, end_pos: usize) -> TokenValue {
        TokenValue::String(self.reader.string()[start_pos..end_pos].to_vec())
    }

    fn slice_range(&self, start_pos: usize) -> &'a [char] {
        self.slice_range2(start_pos, self.current_pos())
    }

    fn slice_range2(&self, start_pos: usize, end_pos: usize) -> &'a [char] {
        &self.reader.string()[start_pos..end_pos]
    }

    fn peek_not_ident_part_or_end_next_char(&mut self) -> bool {
        if is_ident_part_or_end(self.peek_next_char()) {
            return false;
        }
        if self.peek_next_char() == ':' {
            return false;
        }
        self.next_char();
        true
    }

    fn skip_space(&mut self) -> Result<'a, ()> {
        while self.token.kind == Space {
            self.next_token()?;
        }
        Ok(())
    }

    fn skip_space_or_newline(&mut self) -> Result<'a, ()> {
        while matches!(self.token.kind, Space | Newline) {
            self.next_token()?;
        }
        Ok(())
    }

    fn skip_statement_end(&mut self) -> Result<'a, ()> {
        while matches!(self.token.kind, Space | Newline | Op(Semicolon)) {
            self.next_token()?;
        }
        Ok(())
    }

    fn handle_slash_r_slash_n_or_slash_n(&mut self) -> Result<'a, bool> {
        let is_slash_r = self.current_char() == '\r';
        if is_slash_r {
            if self.next_char() != '\n' {
                return self.raise("expecting '\\n' after '\\r'");
            }
        }
        Ok(is_slash_r)
    }

    fn char_sequence(&mut self, tokens: &[char]) -> bool {
        for token in tokens.iter() {
            if *token != self.next_char() {
                return false;
            }
        }
        true
    }

    fn unknown_token<T>(&self) -> Result<'a, T> {
        Err(SyntaxError::new(
            format!("unknown token: {}", self.current_char()),
            self.line_number,
            self.column_number,
            self.filename,
            None,
        ))
    }

    fn set_token_raw_from_start(&mut self, start: usize) {
        if self.wants_raw {
            self.token.raw = self.slice_range(start);
        }
    }

    fn raise<T>(&self, message: &str) -> Result<'a, T> {
        Err(SyntaxError::new(
            message.to_string(),
            self.line_number,
            self.column_number,
            self.filename,
            None,
        ))
    }

    fn raise_at<T>(
        &self,
        message: &str,
        line_number: usize,
        column_number: usize,
    ) -> Result<'a, T> {
        Err(SyntaxError::new(
            message.to_string(),
            line_number,
            column_number,
            self.filename,
            None,
        ))
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_' || c > '\u{9F}'
}

fn is_ident_part(c: char) -> bool {
    is_ident_start(c) || c.is_ascii_digit()
}

fn is_ident(name: &[char]) -> bool {
    name.first().map(|c| is_ident_start(*c)).unwrap_or(false)
}

fn is_setter(name: &[char]) -> bool {
    is_ident(name) && name.last().map(|c| *c == '=').unwrap_or(false)
}

fn is_ident_part_or_end(c: char) -> bool {
    is_ident_part(c) || c == '?' || c == '!'
}

fn closing_char(c: char) -> char {
    match c {
        '<' => '>',
        '(' => ')',
        '[' => ']',
        '{' => '}',
        _ => c,
    }
}

#[test]
fn it_works() {
    fn to_unicode(string: &str) -> Vec<char> {
        string.chars().collect()
    }

    let string = to_unicode("foo");
    let mut lexer = Lexer::new(&string);
    assert!(lexer.next_token().is_ok());
    assert_eq!("foo", lexer.token.to_string());

    let string = to_unicode("123");
    let mut lexer = Lexer::new(&string);
    assert!(lexer.next_token().is_ok());
    assert_eq!("123", lexer.token.to_string());

    let string = to_unicode(":+");
    let mut lexer = Lexer::new(&string);
    lexer.wants_raw = true;
    assert!(lexer.next_token().is_ok());
    assert_eq!(Vec::from_iter(":+".chars()), Vec::from(lexer.token.raw));
    assert_eq!("+", lexer.token.to_string());

    let string = to_unicode(":[]?");
    let mut lexer = Lexer::new(&string);
    lexer.wants_raw = true;
    assert!(lexer.next_token().is_ok());
    assert_eq!(Vec::from_iter(":[]?".chars()), Vec::from(lexer.token.raw));
    assert_eq!("[]?", lexer.token.to_string());

    let string = to_unicode("while");
    let mut lexer = Lexer::new(&string);
    assert!(lexer.next_token().is_ok());
    assert_eq!("while", lexer.token.to_string());
    assert_eq!(lexer.token.value, Keyword::While);

    let string = to_unicode("%=");
    let mut lexer = Lexer::new(&string);
    lexer.wants_def_or_macro_name = true;
    assert!(lexer.next_token().is_ok());
    assert_eq!(Op(Percent), lexer.token.kind);
    assert_eq!("%", lexer.token.to_string());

    let string = to_unicode("%=");
    let mut lexer = Lexer::new(&string);
    lexer.wants_def_or_macro_name = false;
    assert!(lexer.next_token().is_ok());
    assert_eq!(Op(PercentEq), lexer.token.kind);
    assert_eq!("%=", lexer.token.to_string());
}
