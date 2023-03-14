use std::rc::Rc;

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug, Default)]
pub struct DebugInfo {
    file_info: Rc<FileInfo>,
    pos: Position,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Default)]
pub struct FileInfo {
    pub file_name: String,
    pub src: String,
}

impl std::fmt::Debug for FileInfo {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl FileInfo {
    pub const fn new(file_name: String, src: String) -> Self {
        Self { file_name, src }
    }

    pub fn get_file_name(&self) -> &str {
        &self.file_name
    }

    pub fn get_file_src(&self) -> &str {
        &self.src
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug, Copy, Default)]
pub struct Position {
    pub n_char: usize,
    pub n_line: usize,
}

impl Position {
    pub const fn new(n_char: usize, n_line: usize) -> Self {
        Self { n_char, n_line }
    }

    pub const fn get_n_char(&self) -> usize {
        self.n_char
    }

    pub const fn get_n_line(&self) -> usize {
        self.n_line
    }

    pub fn get_n_char_mut(&mut self) -> &mut usize {
        &mut self.n_char
    }

    pub fn get_n_line_mut(&mut self) -> &mut usize {
        &mut self.n_line
    }
}

impl DebugInfo {
    pub fn new(file_info: Rc<FileInfo>, n_char: usize, n_line: usize) -> Self {
        Self {
            file_info,
            pos: Position { n_char, n_line },
        }
    }

    pub fn default_with_file_info(file_info: Rc<FileInfo>) -> Self {
        Self {
            file_info,
            pos: Position::default(),
        }
    }

    pub fn to_error_msg_prefix_string(&self) -> String {
        format!("{}:{}", self.file_info.file_name, self.get_n_line(),)
    }

    pub fn get_cloned_file_info(&self) -> Rc<FileInfo> {
        Rc::clone(&self.file_info)
    }

    pub fn get_file_name(&self) -> String {
        self.file_info.file_name.clone()
    }

    pub fn get_file_src(&self) -> String {
        self.file_info.src.clone()
    }

    pub const fn get_n_char(&self) -> usize {
        self.pos.n_char
    }

    pub const fn get_n_line(&self) -> usize {
        self.pos.n_line
    }

    pub fn get_n_char_mut(&mut self) -> &mut usize {
        &mut self.pos.n_char
    }

    pub fn get_n_line_mut(&mut self) -> &mut usize {
        &mut self.pos.n_line
    }

    pub fn advance_line(&mut self) {
        self.pos.n_char = 0;
        self.pos.n_line += 1;
    }

    #[allow(clippy::return_self_not_must_use)]
    pub fn get_pos_and_advance(&mut self, len_token: usize) -> Self {
        let return_struct = self.clone();
        self.pos.n_char += len_token;
        return_struct
    }

    /// just advance `self.n_char` by `len_token`
    pub fn advance(&mut self, len_token: usize) {
        self.pos.n_char += len_token;
    }
}
