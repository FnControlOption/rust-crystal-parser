pub struct SyntaxError {
    message: String,
    line_number: usize,
    column_number: usize,
    filename: String,
    size: Option<usize>,
}

impl SyntaxError {
    pub fn new(
        message: String,
        line_number: usize,
        column_number: usize,
        filename: String,
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

    pub fn filename(&self) -> &String {
        &self.filename
    }

    pub fn size(&self) -> Option<usize> {
        self.size
    }
}
