use std::fmt;

#[derive(Clone, Debug)]
pub struct Location<'f> {
    line_number: usize,
    column_number: usize,
    filename: &'f str,
}

impl<'f> Location<'f> {
    pub fn new(filename: &'f str, line_number: usize, column_number: usize) -> Self {
        Self {
            filename,
            line_number,
            column_number,
        }
    }

    pub fn line_number(&self) -> usize {
        self.line_number
    }

    pub fn column_number(&self) -> usize {
        self.column_number
    }

    pub fn filename(&self) -> &'f str {
        self.filename
    }
}

impl<'f> fmt::Display for Location<'f> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.filename, self.line_number, self.column_number,
        )
    }
}

#[test]
fn it_works() {
    let result = Location::new("foo", 12, 34);
    assert_eq!("foo:12:34", result.to_string());
}
