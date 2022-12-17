#![allow(dead_code)]

use std::collections::HashSet;

use crate::ast::*;
use crate::error::Result;
use crate::lexer::{Lexer, Raise, RaiseAt};
use crate::location::Location;
use crate::token::{DelimiterValue, Keyword, Op, TokenKind, TokenValue};

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
        let node = self.parse_expressions()?;
        self.check(TokenKind::Eof)?;
        Ok(node)
    }

    fn parse_expressions(&mut self) -> Result<'f, AstNodeBox<'f>> {
        self.preserve_stop_on_do(|p| p.parse_expressions_internal())
    }

    fn parse_expressions_internal(&mut self) -> Result<'f, AstNodeBox<'f>> {
        todo!("parse_expressions_internal")
    }

    fn parse_or(&mut self) -> Result<'f, AstNodeBox<'f>> {
        todo!("parse_or")
    }

    fn is_multi_assign_target(exp: AstRef<'_, '_>) -> bool {
        match exp {
            AstRef::Underscore(_)
            | AstRef::Var(_)
            | AstRef::InstanceVar(_)
            | AstRef::ClassVar(_)
            | AstRef::Global(_)
            | AstRef::Assign(_) => true,

            AstRef::Call(call) => {
                !call.has_parentheses
                    && ((call.args.is_empty() && call.named_args.is_none())
                        || Lexer::is_setter(&call.name)
                        || call.name == &['[', ']']
                        || call.name == &['[', ']', '='])
            }

            _ => false,
        }
    }

    fn is_multi_assign_middle(exp: AstRef<'_, '_>) -> bool {
        match exp {
            AstRef::Assign(_) => true,
            AstRef::Call(call) => call.name.last().map(|c| *c == '=').unwrap_or(false),
            _ => false,
        }
    }

    fn multi_assign_left_hand(&mut self, exp: AstNodeBox<'f>) -> Result<'f, AstNodeBox<'f>> {
        let exp: AstNodeBox<'f> = match exp.to_box() {
            AstBox::Path(path) => {
                let location = path.location().unwrap();
                return self.raise_at(
                    "can't assign to constant in multiple assignment",
                    location.as_ref(),
                );
            }
            AstBox::Call(call) => match call.obj {
                None if call.args.is_empty() => {
                    let location = call.location();
                    let end_location = call.end_location();
                    let mut exp = Var::new(call.name);
                    exp.set_location(location);
                    exp.set_end_location(end_location);
                    exp
                }
                Some(obj) => match obj.to_box() {
                    AstBox::Global(global)
                        if global.name == &['$', '~'] && call.name == &['[', ']'] =>
                    {
                        let location = global.location().unwrap();
                        return self.raise_at(
                            "global match data cannot be assigned to",
                            location.as_ref(),
                        );
                    }
                    obj => obj.into(),
                },
                None => call,
            },
            exp => exp.into(),
        };

        if let AstRef::Var(var) = exp.to_ref() {
            if var.name == &['s', 'e', 'l', 'f'] {
                let location = var.location().unwrap();
                return self.raise_at("can't change the value of self", location.as_ref());
            }
            self.push_var(var.to_ref());
        }

        Ok(exp)
    }

    fn new_range(
        &mut self,
        exp: AstNodeBox<'f>,
        location: Location<'f>,
        exclusive: bool,
    ) -> Result<'f, Box<RangeLiteral<'f>>> {
        self.check_void_value(exp.as_ref(), &location)?;
        self.lexer.next_token_skip_space()?;
        self.check_void_expression_keyword()?;
        let right = if self.is_end_token()
            || matches!(
                self.lexer.token.kind,
                TokenKind::Op(Op::Rparen)
                    | TokenKind::Op(Op::Comma)
                    | TokenKind::Op(Op::Semicolon)
                    | TokenKind::Op(Op::EqGt)
                    | TokenKind::Newline
            ) {
            Nop::new()
        } else {
            self.parse_or()?
        };
        let end_location = right.end_location();
        let mut range = RangeLiteral::new(exp, right, exclusive);
        range.at(location);
        range.set_end_location(end_location);
        Ok(range)
    }

    fn next_comes_colon_space(&mut self) -> bool {
        if self.no_type_declaration != 0 {
            return false;
        }

        let pos = self.lexer.current_pos();
        while self.lexer.current_char().is_ascii_whitespace() {
            self.lexer.next_char_no_column_increment();
        }
        let mut comes_colon_space = self.lexer.current_char() == ':';
        if comes_colon_space {
            self.lexer.next_char_no_column_increment();
            comes_colon_space = self.lexer.current_char().is_ascii_whitespace();
        }
        self.lexer.set_current_pos(pos);
        comes_colon_space
    }

    fn check_not_inside_def<F, T>(&mut self, message: &'_ str, mut f: F) -> Result<'f, T>
    where
        F: FnMut(&mut Self) -> Result<'f, T>,
    {
        if self.def_nest == 0 && self.fun_nest == 0 {
            f(self)
        } else {
            let suffix = if self.def_nest > 0 {
                " inside def"
            } else {
                " inside fun"
            };
            let token = &self.lexer.token;
            self.raise_at(
                format!("{message}{suffix}"),
                (token.line_number, token.column_number),
            )
        }
    }

    fn is_inside_def(&self) -> bool {
        self.def_nest > 0
    }

    fn is_inside_fun(&self) -> bool {
        self.fun_nest > 0
    }

    fn call_block_arg_follows(&self) -> bool {
        self.lexer.token.kind == TokenKind::Op(Op::Amp)
            && !self.lexer.current_char().is_ascii_whitespace()
    }

    fn is_statement_end(&self) -> bool {
        let token = &self.lexer.token;
        matches!(
            token.kind,
            TokenKind::Newline | TokenKind::Op(Op::Semicolon)
        ) || token.is_keyword(Keyword::End)
    }

    fn check_not_pipe_before_proc_literal_body(&mut self) -> Result<'f, ()> {
        if self.lexer.token.kind == TokenKind::Op(Op::Bar) {
            let location = self.lexer.token.location();
            self.lexer.next_token_skip_space()?;
            let mut msg = String::new();
            msg.push_str(
                "unexpected token: \"|\", proc literals specify their parameters like this: ->(",
            );
            if self.lexer.token.kind == TokenKind::Ident {
                msg.push_str(&self.lexer.token.value.to_string());
                msg.push_str(" : Type");
                self.lexer.next_token_skip_space_or_newline()?;
                if self.lexer.token.kind == TokenKind::Op(Op::Comma) {
                    msg.push_str(", ...");
                }
            } else {
                msg.push_str("param : Type");
            }
            msg.push_str(") { ... }");
            self.raise_at(msg, location.as_ref())
        } else {
            Ok(())
        }
    }

    fn is_named_tuple_start(&self) -> bool {
        matches!(self.lexer.token.kind, TokenKind::Ident | TokenKind::Const)
            && self.lexer.current_char() == ':'
            && self.lexer.peek_next_char() != ':'
    }

    fn is_string_literal_start(&self) -> bool {
        self.lexer.token.kind == TokenKind::DelimiterStart
            && matches!(
                self.lexer.token.delimiter_state.value,
                DelimiterValue::String(_)
            )
    }

    fn preserve_stop_on_do<F, T>(&mut self, f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        self.preserve_stop_on_do2(false, f)
    }

    fn preserve_stop_on_do2<F, T>(&mut self, new_value: bool, mut f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        let old_stop_on_do = self.stop_on_do;
        self.stop_on_do = new_value;
        let value = f(self);
        self.stop_on_do = old_stop_on_do;
        value
    }

    fn is_end_token(&mut self) -> bool {
        todo!("is_end_token")
    }

    fn push_var(&mut self, _node: AstRef<'_, 'f>) {
        todo!("push_var")
    }

    fn is_var_in_scope(&self, name: Vec<char>) -> bool {
        self.var_scopes.last().unwrap().contains(&name)
    }

    fn check_void_value(
        &self,
        exp: AstNodeRef<'_, 'f>,
        location: &'_ Location<'f>,
    ) -> Result<'f, ()> {
        if exp.is_control_expression() {
            self.raise_at("void value expression", location)
        } else {
            Ok(())
        }
    }

    fn check_void_expression_keyword(&mut self) -> Result<'f, ()> {
        if let TokenValue::Keyword(keyword) = self.lexer.token.value {
            if matches!(keyword, Keyword::Break | Keyword::Next | Keyword::Return) {
                if !self.next_comes_colon_space() {
                    return self.raise_at("void value expression", &self.lexer.token);
                    // TODO: keyword.to_string().len()
                }
            }
        }
        Ok(())
    }

    fn check_any(&self, token_kinds: &[TokenKind]) -> Result<'f, ()> {
        let token = &self.lexer.token;
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
        let token = &self.lexer.token;
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
        let token = &self.lexer.token;
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
        Ok(self.lexer.token.value.to_string().chars().collect())
    }

    fn check_const(&self) -> Result<'f, Vec<char>> {
        self.check(TokenKind::Const)?;
        Ok(self.lexer.token.value.to_string().chars().collect())
    }

    fn unexpected_token<T>(&self) -> Result<'f, T> {
        self.unexpected_token2(None)
    }

    fn unexpected_token2<T>(&self, msg: Option<&str>) -> Result<'f, T> {
        let token = &self.lexer.token;
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
