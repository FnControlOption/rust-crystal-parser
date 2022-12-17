pub struct CharReader<'s> {
    pub string: &'s [char],
    current_char: char,
    pos: usize,
    end: bool,
}

impl<'s> CharReader<'s> {
    pub fn new(string: &'s [char]) -> Self {
        let mut reader = Self {
            string,
            pos: 0,
            current_char: '\0',
            end: false,
        };
        reader.reset_current_char();
        reader
    }

    pub fn current_char(&self) -> char {
        self.current_char
    }

    pub fn has_next(&self) -> bool {
        !self.end
    }

    pub fn next_char(&mut self) -> char {
        self.pos += 1;
        self.reset_current_char();
        self.current_char
    }

    pub fn peek_next_char(&self) -> char {
        let next_pos = self.pos + 1;
        if next_pos < self.string.len() {
            self.string[next_pos]
        } else {
            '\0'
        }
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn set_pos(&mut self, pos: usize) {
        self.pos = pos;
        self.reset_current_char();
    }

    fn reset_current_char(&mut self) {
        if self.pos < self.string.len() {
            self.end = false;
            self.current_char = self.string[self.pos];
        } else {
            self.end = true;
            self.current_char = '\0';
        }
    }
}
