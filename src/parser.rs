#![allow(dead_code)]

use crate::ast::*;
use crate::error::Result;
use crate::lexer::{Lexer, Raise, RaiseAt};
use crate::location::Location;
use crate::token::*;
use std::collections::HashSet;
use std::rc::Rc;

struct Unclosed<'f> {
    name: String,
    location: Rc<Location<'f>>,
}

pub struct Parser<'s, 'f> {
    lexer: Lexer<'s, 'f>,
    visibility: Option<Visibility>,
    def_nest: usize,
    fun_nest: usize,
    type_nest: usize,
    _wants_doc: bool,
    block_arg_name: Option<String>,

    var_scopes: Vec<HashSet<String>>,
    unclosed_stack: Vec<Unclosed<'f>>,
    calls_super: bool,
    calls_initialize: bool,
    calls_previous_def: bool,
    uses_block_arg: bool,
    is_macro_def: bool,
    assigns_special_var: bool,
    is_constant_assignment: bool,
    call_args_start_locations: Vec<Rc<Location<'f>>>,
    temp_arg_count: usize,
    in_macro_expression: bool,
    stop_on_yield: usize,
    inside_c_struct: bool,
    doc_enabled: bool,
    no_type_declaration: usize,
    consuming_heredocs: bool,
    inside_interpolation: bool,
    stop_on_do: bool,
    assigned_vars: Vec<String>,

    block_arity: Option<usize>,
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
            block_arity: None,
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
        if self.is_end_token() {
            return Ok(Nop::new());
        }

        let exp = self.parse_multi_assign()?;

        self.lexer.slash_is_regex = true;
        self.lexer.skip_statement_end()?;

        if self.is_end_token() {
            return Ok(exp);
        }

        let mut exps = vec![exp];

        loop {
            exps.push(self.parse_multi_assign()?);
            self.lexer.skip_statement_end()?;
            if self.is_end_token() {
                break;
            }
        }

        Ok(Expressions::from(exps))
    }

    fn parse_multi_assign(&mut self) -> Result<'f, AstNodeBox<'f>> {
        // todo!("parse_multi_assign");
        self.parse_expression()
    }

    fn is_multi_assign_target(exp: AstRef<'_, 'f>) -> bool {
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
                        || call.name == "[]"
                        || call.name == "[]=")
            }
            _ => false,
        }
    }

    fn is_multi_assign_middle(exp: AstRef<'_, 'f>) -> bool {
        match exp {
            AstRef::Assign(_) => true,
            AstRef::Call(call) => call.name.ends_with('='),
            _ => false,
        }
    }

    fn multi_assign_left_hand(&mut self, exp: AstNodeBox<'f>) -> Result<'f, AstNodeBox<'f>> {
        let exp: AstNodeBox<'f> = match exp.to_ast_box() {
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
                    exp.at(location);
                    exp.at_end(end_location);
                    exp
                }
                Some(obj) => match obj.to_ast_box() {
                    AstBox::Global(global) if global.name == "$~" && call.name == "[]" => {
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

        if let AstRef::Var(var) = exp.to_ast_ref() {
            if var.name == "self" {
                let location = var.location().unwrap();
                return self.raise_at("can't change the value of self", location.as_ref());
            }
            self.push_var(var.to_ast_ref());
        }

        Ok(exp)
    }

    fn parse_expression(&mut self) -> Result<'f, AstNodeBox<'f>> {
        // todo!("parse_expression");
        self.parse_op_assign()
    }

    fn parse_op_assign(&mut self) -> Result<'f, AstNodeBox<'f>> {
        // todo!("parse_op_assign");
        self.parse_question_colon()
    }

    fn parse_question_colon(&mut self) -> Result<'f, AstNodeBox<'f>> {
        // todo!("parse_question_colon");
        self.parse_range()
    }

    fn parse_range(&mut self) -> Result<'f, AstNodeBox<'f>> {
        // todo!("parse_range");
        self.parse_or()
    }

    fn new_range(
        &mut self,
        exp: AstNodeBox<'f>,
        location: Rc<Location<'f>>,
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
        range.at_end(end_location);
        Ok(range)
    }

    fn parse_or(&mut self) -> Result<'f, AstNodeBox<'f>> {
        // todo!("parse_or")
        self.parse_atomic()
    }

    fn parse_atomic(&mut self) -> Result<'f, AstNodeBox<'f>> {
        // todo!("parse_atomic")
        self.parse_atomic_without_location()
    }

    fn parse_atomic_without_location(&mut self) -> Result<'f, AstNodeBox<'f>> {
        let token = &self.lexer.token;
        match token.kind {
            TokenKind::Number => {
                self.lexer.wants_regex = false;
                self.node_and_next_token(NumberLiteral::new(
                    token.value.to_string(),
                    token.number_kind,
                ))
            }
            TokenKind::Char => {
                if let TokenValue::Char(c) = token.value {
                    self.node_and_next_token(CharLiteral::new(c))
                } else {
                    unreachable!()
                }
            }
            TokenKind::Symbol => {
                self.node_and_next_token(SymbolLiteral::new(token.value.to_string()))
            }
            TokenKind::Global => {
                self.raise("$global_variables are not supported, use @@class_variables instead")
            }
            _ => todo!("parse_atomic_without_location"),
        }
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

    fn check_not_inside_def<F, T>(&mut self, message: &str, mut f: F) -> Result<'f, T>
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

    fn is_named_tuple_start(lexer: &mut Lexer<'s, 'f>) -> bool {
        matches!(lexer.token.kind, TokenKind::Ident | TokenKind::Const)
            && lexer.current_char() == ':'
            && lexer.peek_next_char() != ':'
    }

    fn is_string_literal_start(&self) -> bool {
        self.lexer.token.kind == TokenKind::DelimiterStart
            && matches!(
                self.lexer.token.delimiter_state.value,
                DelimiterValue::String(_)
            )
    }

    /*
    fn add_when_exp(
        &self,
        when_exps: &mut Vec<AstNodeRc<'f>>,
        exp: AstNodeRc<'f>,
    ) -> Result<'f, ()> {
        if !Self::is_when_exp_constant(exp.to_ast_ref()) {
            return Ok(());
        }

        if when_exps.contains(&exp) {
            return self.raise_at(
                format!("duplicate when {exp} in case"),
                exp.location().unwrap().as_ref(),
            );
        }

        when_exps.push(exp);
        Ok(())
    }
    */

    fn is_when_exp_constant(exp: AstRef<'_, 'f>) -> bool {
        match exp {
            #[rustfmt::skip]
            AstRef::NilLiteral(_) | AstRef::BoolLiteral(_) | AstRef::CharLiteral(_) | AstRef::NumberLiteral(_) |
            AstRef::StringLiteral(_) | AstRef::SymbolLiteral(_) | AstRef::Path(_)
            => true,
            AstRef::ArrayLiteral(exp) => exp
                .elements
                .iter()
                .all(|e| Self::is_when_exp_constant(e.to_ast_ref())),
            AstRef::TupleLiteral(exp) => exp
                .elements
                .iter()
                .all(|e| Self::is_when_exp_constant(e.to_ast_ref())),
            AstRef::RegexLiteral(exp) => Self::is_when_exp_constant(exp.value.to_ast_ref()),
            AstRef::RangeLiteral(exp) => {
                Self::is_when_exp_constant(exp.from.to_ast_ref())
                    && Self::is_when_exp_constant(exp.to.to_ast_ref())
            }
            _ => false,
        }
    }

    fn when_expression_end(&mut self) -> Result<'f, bool> {
        self.lexer.slash_is_regex = true;
        if self.lexer.token.is_keyword(Keyword::Then) {
            self.lexer.next_token_skip_space_or_newline()?;
            return Ok(true);
        } else {
            match self.lexer.token.kind {
                TokenKind::Op(Op::Comma) => {
                    self.lexer.next_token_skip_space_or_newline()?;
                }
                TokenKind::Newline => {
                    self.lexer.skip_space_or_newline()?;
                    return Ok(true);
                }
                TokenKind::Op(Op::Semicolon) => {
                    self.lexer.skip_statement_end()?;
                    return Ok(true);
                }
                _ => {
                    return self.unexpected_token2(Some("expecting ',', ';' or '\\n'"));
                }
            }
        }
        Ok(false)
    }

    fn is_valid_select_when(node: AstRef<'_, 'f>) -> bool {
        match node {
            AstRef::Assign(assign) => assign.value.tag() == AstTag::Call,
            AstRef::Call(_) => true,
            _ => false,
        }
    }

    fn prepare_parse_def(&mut self) {
        self.calls_super = false;
        self.calls_initialize = false;
        self.calls_previous_def = false;
        self.uses_block_arg = false;
        self.block_arg_name = None;
        self.assigns_special_var = false;
        self.is_macro_def = false;
    }

    fn check_valid_def_name(&self) -> Result<'f, ()> {
        let token = &self.lexer.token;
        if let TokenValue::Keyword(keyword) = token.value {
            if matches!(
                keyword,
                Keyword::IsAQuestion
                    | Keyword::As
                    | Keyword::AsQuestion
                    | Keyword::RespondsToQuestion
                    | Keyword::NilQuestion
            ) {
                return self.raise_at(
                    format!("'{keyword}' is a pseudo-method and can't be redefined"),
                    token,
                );
            }
        }
        Ok(())
    }

    fn check_valid_def_op_name(&self) -> Result<'f, ()> {
        let token = &self.lexer.token;
        if token.kind == TokenKind::Op(Op::Bang) {
            return self.raise_at("'!' is a pseudo-method and can't be redefined", token);
        }
        Ok(())
    }

    fn compute_block_arg_yields(&mut self, block_arg: &Arg<'f>) {
        if let Some(restriction) = &block_arg.restriction {
            if let AstRef::ProcNotation(restriction) = restriction.to_ast_ref() {
                self.block_arity = Some(restriction.inputs.as_ref().map_or(0, Vec::len));
                return;
            }
        }
        self.block_arity = Some(0);
    }

    fn is_invalid_internal_name(keyword: &TokenValue) -> bool {
        match keyword {
            TokenValue::Keyword(keyword) => match keyword {
                #[rustfmt::skip]
                Keyword::Begin | Keyword::Nil | Keyword::True | Keyword::False | Keyword::Yield | Keyword::With | Keyword::Abstract |
                Keyword::Def | Keyword::Macro | Keyword::Require | Keyword::Case | Keyword::Select | Keyword::If | Keyword::Unless | Keyword::Include |
                Keyword::Extend | Keyword::Class | Keyword::Struct | Keyword::Module | Keyword::Enum | Keyword::While | Keyword::Until | Keyword::Return |
                Keyword::Next | Keyword::Break | Keyword::Lib | Keyword::Fun | Keyword::Alias | Keyword::Pointerof | Keyword::Sizeof | Keyword::Offsetof |
                Keyword::InstanceSizeof | Keyword::Typeof | Keyword::Private | Keyword::Protected | Keyword::Asm | Keyword::Out |
                Keyword::Self_ | Keyword::In | Keyword::End
                => true,
                _ => false,
            },
            TokenValue::String(keyword) => match keyword.as_ref() {
                #[rustfmt::skip]
                "begin" | "nil" | "true" | "false" | "yield" | "with" | "abstract" |
                "def" | "macro" | "require" | "case" | "select" | "if" | "unless" | "include" |
                "extend" | "class" | "struct" | "module" | "enum" | "while" | "until" | "return" |
                "next" | "break" | "lib" | "fun" | "alias" | "pointerof" | "sizeof" | "offsetof" |
                "instance_sizeof" | "typeof" | "private" | "protected" | "asm" | "out" |
                "self" | "in" | "end"
                => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn set_visibility<T>(&self, mut node: Box<T>) -> Box<T>
    where
        T: Visible,
    {
        if let Some(visibility) = self.visibility {
            node.set_visibility(visibility);
        }
        node
    }

    fn next_comes_plus_or_minus(&mut self) -> bool {
        let pos = self.lexer.current_pos();
        while self.lexer.current_char().is_ascii_whitespace() {
            self.lexer.next_char_no_column_increment();
        }
        let comes_plus_or_minus = match self.lexer.current_char() {
            '+' | '-' => true,
            _ => false,
        };
        self.lexer.set_current_pos(pos);
        comes_plus_or_minus
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

    fn make_nilable_type(type_: AstNodeBox<'f>) -> Box<Union<'f>> {
        let mut union = Union::new(Vec::new());
        union.at(type_.as_ref());
        let mut nil = Path::global(["Nil"]);
        nil.at(type_.as_ref());
        union.types.push(type_);
        union.types.push(nil);
        union
    }

    fn make_nilable_expression(type_: AstNodeBox<'f>) -> Box<Generic<'f>> {
        let mut name = Path::global(["Union"]);
        name.at(type_.as_ref());
        let mut generic = Generic::new(name, Vec::new());
        generic.at(type_.as_ref());
        let mut nil = Path::global(["Nil"]);
        nil.at(type_.as_ref());
        generic.type_vars.push(type_);
        generic.type_vars.push(nil);
        generic.suffix = GenericSuffix::Question;
        generic
    }

    fn make_pointer_type(type_: AstNodeBox<'f>) -> Box<Generic<'f>> {
        let mut name = Path::global(["Pointer"]);
        name.at(type_.as_ref());
        let mut generic = Generic::new(name, Vec::new());
        generic.at(type_.as_ref());
        generic.type_vars.push(type_);
        generic.suffix = GenericSuffix::Asterisk;
        generic
    }

    fn make_static_array_type(type_: AstNodeBox<'f>, size: AstNodeBox<'f>) -> Box<Generic<'f>> {
        let mut name = Path::global(["StaticArray"]);
        name.at(type_.as_ref());
        let mut generic = Generic::new(name, Vec::new());
        generic.at(type_.as_ref());
        generic.type_vars.push(type_);
        generic.type_vars.push(size);
        generic.suffix = GenericSuffix::Bracket;
        generic
    }

    fn make_tuple_type(types: Vec<AstNodeBox<'f>>) -> Box<Generic<'f>> {
        Generic::new(Path::global(["Tuple"]), types)
    }

    fn make_named_tuple_type(named_args: Vec<Box<NamedArgument<'f>>>) -> Box<Generic<'f>> {
        let mut generic = Generic::new(Path::global(["NamedTuple"]), Vec::new());
        generic.named_args = Some(named_args);
        generic
    }

    fn is_type_start(&mut self, consume_newlines: bool) -> bool {
        self.lexer
            .peek_ahead(|lexer| {
                if consume_newlines {
                    lexer.next_token_skip_space_or_newline()?;
                } else {
                    lexer.next_token_skip_space()?;
                }

                Self::type_start(lexer)
            })
            .unwrap_or(false)
    }

    fn type_start(lexer: &mut Lexer<'s, 'f>) -> Result<'f, bool> {
        while matches!(
            lexer.token.kind,
            TokenKind::Op(Op::Lparen) | TokenKind::Op(Op::Lcurly)
        ) {
            lexer.next_token_skip_space_or_newline()?;
        }

        match lexer.token.kind {
            TokenKind::Ident => {
                if Self::is_named_tuple_start(lexer) {
                    return Ok(false);
                }
                if lexer.token.value == Keyword::Typeof {
                    Ok(true)
                } else if lexer.token.value == Keyword::Self_ || lexer.token.value == "self?" {
                    lexer.next_token_skip_space()?;
                    Self::delimiter_or_type_suffix(lexer)
                } else {
                    Ok(false)
                }
            }
            TokenKind::Const => {
                if Self::is_named_tuple_start(lexer) {
                    return Ok(false);
                }
                Self::type_path_start(lexer)
            }
            TokenKind::Op(Op::ColonColon) => {
                lexer.next_token()?;
                Self::type_path_start(lexer)
            }
            TokenKind::Underscore | TokenKind::Op(Op::MinusGt) => Ok(true),
            TokenKind::Op(Op::Star) => {
                lexer.next_token_skip_space_or_newline()?;
                Self::type_path_start(lexer)
            }
            _ => Ok(false),
        }
    }

    fn type_path_start(lexer: &mut Lexer<'s, 'f>) -> Result<'f, bool> {
        while lexer.token.kind == TokenKind::Const {
            lexer.next_token()?;
            if lexer.token.kind != TokenKind::Op(Op::ColonColon) {
                break;
            }
            lexer.next_token_skip_space_or_newline()?;
        }

        lexer.skip_space()?;
        Self::delimiter_or_type_suffix(lexer)
    }

    fn delimiter_or_type_suffix(lexer: &mut Lexer<'s, 'f>) -> Result<'f, bool> {
        match lexer.token.kind {
            TokenKind::Op(Op::Period) => {
                lexer.next_token_skip_space_or_newline()?;
                Ok(lexer.token.is_keyword(Keyword::Class))
            }
            TokenKind::Op(Op::Question) | TokenKind::Op(Op::Star) | TokenKind::Op(Op::StarStar) => {
                lexer.next_token_skip_space()?;
                Self::delimiter_or_type_suffix(lexer)
            }
            TokenKind::Op(Op::MinusGt)
            | TokenKind::Op(Op::Bar)
            | TokenKind::Op(Op::Comma)
            | TokenKind::Op(Op::EqGt)
            | TokenKind::Newline
            | TokenKind::Eof
            | TokenKind::Op(Op::Eq)
            | TokenKind::Op(Op::Semicolon)
            | TokenKind::Op(Op::Lparen)
            | TokenKind::Op(Op::Rparen)
            | TokenKind::Op(Op::Lsquare)
            | TokenKind::Op(Op::Rsquare) => {
                // -> | , => \n EOF = ; ( ) [ ]
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn node_and_next_token(&mut self, mut node: AstNodeBox<'f>) -> Result<'f, AstNodeBox<'f>> {
        node.at_end(self.lexer.token_end_location());
        self.lexer.next_token()?;
        Ok(node)
    }

    fn is_end_token(&mut self) -> bool {
        let token = &self.lexer.token;
        if matches!(
            token.kind,
            TokenKind::Op(Op::Rcurly)
                | TokenKind::Op(Op::Rsquare)
                | TokenKind::Op(Op::PercentRcurly)
                | TokenKind::Eof
        ) {
            return true;
        }

        if let TokenValue::Keyword(keyword) = token.value {
            if matches!(
                keyword,
                Keyword::Do
                    | Keyword::End
                    | Keyword::Else
                    | Keyword::Elsif
                    | Keyword::When
                    | Keyword::In
                    | Keyword::Rescue
                    | Keyword::Ensure
                    | Keyword::Then
            ) {
                !self.next_comes_colon_space()
            } else {
                false
            }
        } else {
            false
        }
    }

    fn can_be_assigned(node: AstRef<'_, 'f>) -> bool {
        match node {
            AstRef::Var(_)
            | AstRef::InstanceVar(_)
            | AstRef::ClassVar(_)
            | AstRef::Path(_)
            | AstRef::Global(_)
            | AstRef::Underscore(_) => true,
            AstRef::Call(call) => {
                (call.obj.is_none() && call.args.is_empty() && call.block.is_none())
                    || call.name == "[]"
            }
            _ => false,
        }
    }

    #[rustfmt::skip]
    const DEF_OR_MACRO_CHECK1: &[TokenKind] = &[
        TokenKind::Ident, TokenKind::Const, TokenKind::Op(Op::Grave),
        TokenKind::Op(Op::LtLt), TokenKind::Op(Op::Lt), TokenKind::Op(Op::LtEq), TokenKind::Op(Op::EqEq), TokenKind::Op(Op::EqEqEq), TokenKind::Op(Op::BangEq), TokenKind::Op(Op::EqTilde),
        TokenKind::Op(Op::BangTilde), TokenKind::Op(Op::GtGt), TokenKind::Op(Op::Gt), TokenKind::Op(Op::GtEq), TokenKind::Op(Op::Plus), TokenKind::Op(Op::Minus), TokenKind::Op(Op::Star), TokenKind::Op(Op::Slash),
        TokenKind::Op(Op::SlashSlash), TokenKind::Op(Op::Bang), TokenKind::Op(Op::Tilde), TokenKind::Op(Op::Percent), TokenKind::Op(Op::Amp), TokenKind::Op(Op::Bar), TokenKind::Op(Op::Caret), TokenKind::Op(Op::StarStar),
        TokenKind::Op(Op::LsquareRsquare), TokenKind::Op(Op::LsquareRsquareEq), TokenKind::Op(Op::LsquareRsquareQuestion), TokenKind::Op(Op::LtEqGt),
        TokenKind::Op(Op::AmpPlus), TokenKind::Op(Op::AmpMinus), TokenKind::Op(Op::AmpStar), TokenKind::Op(Op::AmpStarStar),
    ];

    fn consume_def_or_macro_name(&mut self) -> Result<'f, String> {
        self.lexer.wants_def_or_macro_name = true;
        self.lexer.next_token_skip_space_or_newline()?;
        self.check_any(Self::DEF_OR_MACRO_CHECK1)?;
        self.lexer.wants_def_or_macro_name = false;
        Ok(self.lexer.token.to_string())
    }

    fn consume_def_equals_sign_skip_space(&mut self) -> Result<'f, bool> {
        self.lexer.next_token()?;
        if self.lexer.token.kind == TokenKind::Op(Op::Eq) {
            self.lexer.next_token_skip_space()?;
            Ok(true)
        } else {
            self.lexer.skip_space()?;
            Ok(false)
        }
    }

    fn with_isolated_var_scope<F, T>(&mut self, f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        self.with_isolated_var_scope2(true, f)
    }

    fn with_isolated_var_scope2<F, T>(&mut self, create_scope: bool, mut f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        if !create_scope {
            return f(self);
        }

        self.var_scopes.push(HashSet::new());
        let value = f(self);
        self.var_scopes.pop();
        value
    }

    fn with_lexical_var_scope<F, T>(&mut self, mut f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        let current_scope = self.var_scopes.last().unwrap().clone();
        self.var_scopes.push(current_scope);
        let value = f(self);
        self.var_scopes.pop();
        value
    }

    fn push_vars(&mut self, vars: &[AstNodeBox<'f>]) {
        for var in vars {
            self.push_var(var.to_ast_ref());
        }
    }

    fn push_var(&mut self, node: AstRef<'_, 'f>) {
        self.push_var_name(match node {
            AstRef::Var(var) => &var.name,
            AstRef::Arg(arg) => &arg.name,
            AstRef::TypeDeclaration(type_declaration) => match type_declaration.var.to_ast_ref() {
                AstRef::Var(var) => &var.name,
                AstRef::InstanceVar(var) => &var.name,
                _ => panic!("can't happen"),
            },
            _ => return, // Nothing
        });
    }

    fn push_var_name(&mut self, name: &str) {
        self.var_scopes.last_mut().unwrap().insert(name.to_string());
    }

    fn is_var_in_scope(&self, name: &str) -> bool {
        self.var_scopes.last().unwrap().contains(name)
    }

    fn check_void_value(&self, exp: AstNodeRef<'_, 'f>, location: &Location<'f>) -> Result<'f, ()> {
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

    fn check_any_ident(&self) -> Result<'f, String> {
        self.check(TokenKind::Ident)?;
        Ok(self.lexer.token.value.to_string())
    }

    fn check_const(&self) -> Result<'f, String> {
        self.check(TokenKind::Const)?;
        Ok(self.lexer.token.value.to_string())
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
                format!("unterminated {}", &unclosed.name),
                unclosed.location.as_ref(),
            );
        }

        self.unexpected_token()
    }

    fn is_var(&self, name: &str) -> bool {
        if self.in_macro_expression {
            return true;
        }

        name == "self" || self.is_var_in_scope(name)
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

    fn temp_arg_name(&mut self) -> String {
        let arg_name = format!("__arg{}", self.temp_arg_count);
        self.temp_arg_count += 1;
        arg_name
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
