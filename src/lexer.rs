#![allow(dead_code, unused_imports)]

use crate::char_reader::CharReader;
use crate::error::SyntaxError;
use crate::location::Location;
use crate::token::Magic::*;
use crate::token::Op::*;
use crate::token::TokenKind::*;
use crate::token::*;

type Result<T> = std::result::Result<T, SyntaxError>;

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

    reader: CharReader,
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
    pub fn new(string: &str) -> Self {
        Self {
            // warnings
            reader: CharReader::new(string.chars().collect()),
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

    pub fn next_token(&mut self) -> Result<()> {
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
            // TODO
            ':' => {
                if self.next_char() == ':' {
                    self.skip_token_char(Op(ColonColon));
                } else if self.wants_symbol {
                    self.consume_symbol();
                } else {
                    self.token.kind = Op(Colon);
                }
            }
            // TODO
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
            // TODO
            '0'..='9' => {
                self.scan_number(start);
            }
            // TODO
            _ => {
                if self.current_char().is_ascii_uppercase() {
                    let start = self.current_pos();
                    while is_ident_part(self.next_char()) {
                        // Nothing to do
                    }
                    self.token.kind = Const;
                    self.token.value = self.string_range(start).into();
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

        Ok(())
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
        self.token.value = self.string_range(start_pos).into();
    }

    fn consume_doc(&mut self) {
        if self.current_char() == ' ' {
            self.next_char();
        }

        let start_pos = self.current_pos();

        self.skip_comment();

        let mut string = self.string_range(start_pos);

        match &mut self.token.doc_buffer {
            Some(doc_buffer) => {
                doc_buffer.push('\n');
            }
            None => {
                self.token.doc_buffer = Some(Vec::new());
            }
        }

        let doc_buffer = self.token.doc_buffer.as_mut().unwrap();
        doc_buffer.append(&mut string);
    }

    fn skip_comment(&mut self) {
        let mut c = self.current_char();
        while c != '\n' && c != '\0' {
            c = self.next_char_no_column_increment();
        }
    }

    fn consume_whitespace(&mut self) -> Result<()> {
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
            self.token.value = self.string_range(start_pos).into();
        }
        Ok(())
    }

    fn consume_newlines(&mut self) -> Result<()> {
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

    fn check_ident_or_keyword(&mut self, keyword: Keyword, start: usize) {
        if is_ident_part_or_end(self.peek_next_char()) {
            self.scan_ident(start);
        } else {
            self.next_char();
            self.token.kind = Ident;
            self.token.value = keyword.into();
        }
    }

    fn scan_ident(&mut self, start: usize) {
        while is_ident_part(self.current_char()) {
            self.next_char();
        }
        if matches!(self.current_char(), '?' | '!') && self.peek_next_char() != '=' {
            self.next_char();
        }
        self.token.kind = Ident;
        self.token.value = self.string_range(start).into();
    }

    fn skip_symbol_char(&mut self, value: &str) {
        self.next_char();
        self.symbol(value);
    }

    fn symbol(&mut self, value: &str) {
        self.token.kind = Symbol;
        self.token.value = TokenValue::String(value.to_string().chars().collect());
        if self.wants_raw {
            self.token.raw = format!(":{value}").chars().collect();
        }
    }

    fn scan_number(&mut self, start: usize) {
        // TODO: implement
        while self.next_char().is_ascii_digit() {}
        self.token.kind = Number;
        self.set_token_raw_from_start(start);
        self.token.value = self.string_range(start).into();
    }

    fn consume_symbol(&mut self) {
        match self.current_char() {
            // TODO
            c => {
                if is_ident_start(c) {
                    let start = self.current_pos();
                    while is_ident_part(self.next_char()) {
                        // Nothing to do
                    }
                    if self.current_char() == '?'
                        || (matches!(self.current_char(), '!' | '=')
                            && self.peek_next_char() != '=')
                    {
                        self.next_char();
                    }
                    self.token.kind = Symbol;
                    self.token.value = self.string_range(start).into();
                    self.set_token_raw_from_start(start - 1);
                } else {
                    self.token.kind = Op(Colon);
                }
            }
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

    fn skip_token_char(&mut self, token_kind: TokenKind) {
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

    fn next_token_skip_space(&mut self) -> Result<()> {
        self.next_token()?;
        self.skip_space()?;
        Ok(())
    }

    fn next_token_skip_space_or_newline(&mut self) -> Result<()> {
        self.next_token()?;
        self.skip_space_or_newline()?;
        Ok(())
    }

    fn next_token_skip_statement_end(&mut self) -> Result<()> {
        self.next_token()?;
        self.skip_statement_end()?;
        Ok(())
    }

    fn next_token_never_a_symbol(&mut self) -> Result<()> {
        self.wants_symbol = false;
        let result = self.next_token();
        self.wants_symbol = true;
        result
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

    fn string_range(&self, start_pos: usize) -> Vec<char> {
        self.string_range2(start_pos, self.current_pos())
    }

    fn string_range2(&self, start_pos: usize, end_pos: usize) -> Vec<char> {
        self.reader.string()[start_pos..end_pos].to_vec()
    }

    fn slice_range(&self, start_pos: usize) -> &[char] {
        self.slice_range2(start_pos, self.current_pos())
    }

    fn slice_range2(&self, start_pos: usize, end_pos: usize) -> &[char] {
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

    fn skip_space(&mut self) -> Result<()> {
        while self.token.kind == Space {
            self.next_token()?;
        }
        Ok(())
    }

    fn skip_space_or_newline(&mut self) -> Result<()> {
        while matches!(self.token.kind, Space | Newline) {
            self.next_token()?;
        }
        Ok(())
    }

    fn skip_statement_end(&mut self) -> Result<()> {
        while matches!(self.token.kind, Space | Newline | Op(Semicolon)) {
            self.next_token()?;
        }
        Ok(())
    }

    fn handle_slash_r_slash_n_or_slash_n(&mut self) -> Result<bool> {
        let is_slash_r = self.current_char() == '\r';
        if is_slash_r {
            if self.next_char() != '\n' {
                return self.raise("expecting '\\n' after '\\r'");
            }
        }
        Ok(is_slash_r)
    }

    fn unknown_token<T>(&self) -> Result<T> {
        Err(SyntaxError::new(
            format!("unknown token: {}", self.current_char()),
            self.line_number,
            self.column_number,
            self.filename.to_string(),
            None,
        ))
    }

    fn set_token_raw_from_start(&mut self, start: usize) {
        if self.wants_raw {
            self.token.raw = self.string_range(start);
        }
    }

    fn raise<T>(&self, message: &str) -> Result<T> {
        Err(SyntaxError::new(
            message.to_string(),
            self.line_number,
            self.column_number,
            self.filename.to_string(),
            None,
        ))
    }

    fn raise_at<T>(&self, message: &str, line_number: usize, column_number: usize) -> Result<T> {
        Err(SyntaxError::new(
            message.to_string(),
            line_number,
            column_number,
            self.filename.to_string(),
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

fn is_ident(name: &Vec<char>) -> bool {
    name.first().map(|c| is_ident_start(*c)).unwrap_or(false)
}

fn is_setter(name: &Vec<char>) -> bool {
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
    let mut lexer = Lexer::new("foo");
    assert!(lexer.next_token().is_ok());
    assert_eq!("foo", lexer.token.to_string());

    lexer = Lexer::new("123");
    assert!(lexer.next_token().is_ok());
    assert_eq!("123", lexer.token.to_string());
}
