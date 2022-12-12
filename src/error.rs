pub struct SyntaxError<'a> {
    message: String,
    line_number: usize,
    column_number: usize,
    filename: &'a str,
    size: Option<usize>,
}

impl<'a> SyntaxError<'a> {
    pub fn new(
        message: String,
        line_number: usize,
        column_number: usize,
        filename: &'a str,
        size: Option<usize>,
    ) -> Self {
        Self {
            message,
            line_number,
            column_number,
            filename,
            size,
        }
    }

    pub fn message(&self) -> &String {
        &self.message
    }

    pub fn line_number(&self) -> usize {
        self.line_number
    }

    pub fn column_number(&self) -> usize {
        self.column_number
    }

    pub fn filename(&self) -> &'a str {
        self.filename
    }

    pub fn size(&self) -> Option<usize> {
        self.size
    }
}
