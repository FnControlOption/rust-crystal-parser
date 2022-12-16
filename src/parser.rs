#![allow(dead_code)]

use std::collections::HashSet;

use crate::ast::*;
use crate::error::Result;
use crate::lexer::{Lexer, Raise, RaiseAt};
use crate::location::Location;
use crate::token::{Keyword, TokenKind, TokenValue};

struct Unclosed<'f> {
    name: Vec<char>,
    location: Location<'f>,
}

pub struct Parser<'s, 'f> {
    lexer: Lexer<'s, 'f>,
    visibility: Option<Visibility>,
    def_nest: usize,
    fun_nest: usize,
    type_nest: usize,
    _wants_doc: bool,
    block_arg_name: Option<Vec<char>>,
    var_scopes: Vec<HashSet<Vec<char>>>,
    unclosed_stack: Vec<Unclosed<'f>>,
    calls_super: bool,
    calls_initialize: bool,
    calls_previous_def: bool,
    uses_block_arg: bool,
    is_macro_def: bool,
    assigns_special_var: bool,
    is_constant_assignment: bool,
    call_args_start_locations: Vec<Location<'f>>,
    temp_arg_count: usize,
    in_macro_expression: bool,
    stop_on_yield: usize,
    inside_c_struct: bool,
    doc_enabled: bool,
    no_type_declaration: usize,
    consuming_heredocs: bool,
    inside_interpolation: bool,
    stop_on_do: bool,
    assigned_vars: Vec<Vec<char>>,
}

impl<'s, 'f> Parser<'s, 'f> {
    pub fn new(str: &'s [char]) -> Self {
        Self {
            lexer: Lexer::new(str),
            visibility: None,
            block_arg_name: None,
            var_scopes: vec![HashSet::new()],
            unclosed_stack: Vec::new(),
            calls_super: false,
            calls_initialize: false,
            calls_previous_def: false,
            uses_block_arg: false,
            is_macro_def: false,
            assigns_special_var: false,
            def_nest: 0,
            fun_nest: 0,
            type_nest: 0,
            is_constant_assignment: false,
            call_args_start_locations: Vec::new(),
            temp_arg_count: 0,
            in_macro_expression: false,
            stop_on_yield: 0,
            inside_c_struct: false,
            _wants_doc: false,
            doc_enabled: false,
            no_type_declaration: 0,
            consuming_heredocs: false,
            inside_interpolation: false,
            stop_on_do: false,
            assigned_vars: Vec::new(),
        }
    }

    fn wants_doc(&self) -> bool {
        return self._wants_doc;
    }

    fn set_wants_doc(&mut self, wants_doc: bool) {
        self._wants_doc = wants_doc;
        self.doc_enabled = wants_doc;
    }

    fn parse(&mut self) -> Result<'f, AstNodeBox<'f>> {
        self.lexer.next_token_skip_statement_end()?;
        Ok(Nop::new())
    }

    fn next_comes_colon_space(&mut self) -> bool {
        if self.no_type_declaration != 0 {
            return false;
        }

        todo!();
    }

    fn is_var_in_scope(&self, name: Vec<char>) -> bool {
        self.var_scopes.last().unwrap().contains(&name)
    }

    fn check_void_value(&self, exp: AstNodeBox<'f>, location: &Location<'f>) -> Result<'f, ()> {
        if exp.is_control_expression() {
            self.raise_at("void value expression", location)
        } else {
            Ok(())
        }
    }

    fn check_void_expression_keyword(&mut self) -> Result<'f, ()> {
        if let TokenValue::Keyword(keyword) = self.lexer.token().value {
            if matches!(keyword, Keyword::Break | Keyword::Next | Keyword::Return) {
                if !self.next_comes_colon_space() {
                    let token = self.lexer.token();
                    return self.raise_at("void value expression", token);
                    // TODO: keyword.to_string().len()
                }
            }
        }
        Ok(())
    }

    fn check_any(&self, token_kinds: &[TokenKind]) -> Result<'f, ()> {
        let token = self.lexer.token();
        for kind in token_kinds {
            if token.kind == *kind {
                return Ok(());
            }
        }
        let mut message = String::new();
        message.push_str("expecting any of these tokens: ");
        let mut first = true;
        for kind in token_kinds {
            if !first {
                message.push_str(", ");
            }
            message.push_str(&kind.to_string());
            first = false;
        }
        message.push_str(" (not '");
        message.push_str(&token.kind.to_string());
        message.push_str("')");
        self.raise_at(message, token)
    }

    fn check(&self, token_kind: TokenKind) -> Result<'f, ()> {
        let token = self.lexer.token();
        if token_kind != token.kind {
            self.raise_at(
                format!("expecting token '{token_kind}', not '{token}'"),
                token,
            )
        } else {
            Ok(())
        }
    }

    fn check_ident(&self, value: Keyword) -> Result<'f, ()> {
        let token = self.lexer.token();
        if !token.is_keyword(value) {
            self.raise_at(
                format!("expecting identifier '{value}', not '{token}'"),
                token,
            )
        } else {
            Ok(())
        }
    }

    fn check_any_ident(&self) -> Result<'f, Vec<char>> {
        self.check(TokenKind::Ident)?;
        let token = self.lexer.token();
        Ok(token.value.to_string().chars().collect())
    }

    fn check_const(&self) -> Result<'f, Vec<char>> {
        self.check(TokenKind::Const)?;
        let token = self.lexer.token();
        Ok(token.value.to_string().chars().collect())
    }

    fn unexpected_token<T>(&self) -> Result<'f, T> {
        self.unexpected_token2(None)
    }

    fn unexpected_token2<T>(&self, msg: Option<&str>) -> Result<'f, T> {
        let token = self.lexer.token();
        let mut message = String::new();
        message.push_str("unexpected token: ");
        if token.kind == TokenKind::Eof {
            message.push_str("EOF");
        } else {
            message.push('"');
            message.push_str(&token.to_string());
            message.push('"');
        };
        if let Some(msg) = msg {
            message.push_str(" (");
            message.push_str(msg);
            message.push(')');
        }
        self.raise_at(message, token)
    }

    fn unexpected_token_in_atomic<T>(&self) -> Result<'f, T> {
        if let Some(unclosed) = self.unclosed_stack.last() {
            return self.raise_at(
                format!("unterminated {}", String::from_iter(&unclosed.name)),
                &unclosed.location,
            );
        }

        self.unexpected_token()
    }

    fn is_var(&self, name: Vec<char>) -> bool {
        if self.in_macro_expression {
            return true;
        }

        &name == &['s', 'e', 'l', 'f'] || self.is_var_in_scope(name)
    }

    fn push_visibilty<F, T>(&mut self, mut f: F) -> T
    where
        F: FnMut() -> T,
    {
        let old_visibility = self.visibility;
        self.visibility = None;
        let value = f();
        self.visibility = old_visibility;
        value
    }

    fn temp_arg_name(&mut self) -> Vec<char> {
        let arg_name = format!("__arg{}", self.temp_arg_count);
        self.temp_arg_count += 1;
        arg_name.chars().collect()
    }
}

impl<'s, 'f, S> Raise<'f, S> for Parser<'s, 'f>
where
    Lexer<'s, 'f>: Raise<'f, S>,
{
    fn raise<T>(&self, message: S) -> Result<'f, T> {
        self.lexer.raise(message)
    }
}

impl<'s, 'f, S, L> RaiseAt<'f, S, L> for Parser<'s, 'f>
where
    Lexer<'s, 'f>: RaiseAt<'f, S, L>,
{
    fn raise_at<T>(&self, message: S, location: L) -> Result<'f, T> {
        self.lexer.raise_at(message, location)
    }
}

#[test]
fn it_works() {
    fn to_unicode(string: &str) -> Vec<char> {
        string.chars().collect()
    }

    let str = to_unicode("foo");
    let mut parser = Parser::new(&str);
    parser.push_visibilty(|| {});
}
