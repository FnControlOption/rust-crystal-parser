pub type Result<'f, T> = std::result::Result<T, SyntaxError<'f>>;

pub trait Foo {
    type Result<T>;
}

pub struct SyntaxError<'f> {
    message: String,
    line_number: usize,
    column_number: usize,
    filename: &'f str,
    // size: Option<usize>,
}

impl<'f> SyntaxError<'f> {
    pub fn new(
        message: String,
        line_number: usize,
        column_number: usize,
        filename: &'f str,
        // size: Option<usize>,
    ) -> Self {
        Self {
            message,
            line_number,
            column_number,
            filename,
            // size,
        }
    }

    pub fn message(&self) -> &str {
        &self.message
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

    // pub fn size(&self) -> Option<usize> {
    //     self.size
    // }
}
