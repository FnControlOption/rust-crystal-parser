#![allow(dead_code)]

use crate::{ast::Visibility, lexer::Lexer, location::Location};
// use std::fmt;

struct Unclosed<'b> {
    name: Vec<char>,
    location: Location<'b>,
}

pub struct Parser<'a, 'b> {
    lexer: Lexer<'a, 'b>,
    visibility: Option<Visibility>,
    def_nest: usize,
    fun_nest: usize,
    type_nest: usize,
    _wants_doc: bool,
    block_arg_name: Option<Vec<char>>,
    unclosed_stack: Vec<Unclosed<'b>>,
    calls_super: bool,
    calls_initialize: bool,
    calls_previous_def: bool,
    uses_block_arg: bool,
    is_macro_def: bool,
    assigns_special_var: bool,
    is_constant_assignment: bool,
    call_args_start_locations: Vec<Location<'b>>,
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

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(str: &'a [char]) -> Self {
        Self {
            lexer: Lexer::new(str),
            visibility: None,
            block_arg_name: None,
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
}

// impl<'a> Location<'a> {
//     pub fn new(filename: &'a str, line_number: usize, column_number: usize) -> Self {
//         Self {
//             filename,
//             line_number,
//             column_number,
//         }
//     }

//     pub fn line_number(&self) -> usize {
//         self.line_number
//     }

//     pub fn column_number(&self) -> usize {
//         self.column_number
//     }

//     pub fn filename(&self) -> &'a str {
//         self.filename
//     }
// }

// impl<'a> fmt::Display for Location<'a> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(
//             f,
//             "{}:{}:{}",
//             self.filename, self.line_number, self.column_number,
//         )
//     }
// }

// #[test]
// fn it_works() {
//     let result = Location::new("foo", 12, 34);
//     assert_eq!("foo:12:34", result.to_string());
// }
