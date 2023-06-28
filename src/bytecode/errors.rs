pub trait Reportable {
    fn report(&self, src: &[char]);
}

pub struct LoxError {
    msg: String,
    offset: Option<usize>,
    length: Option<usize>,
    line: usize,
}

impl LoxError {
    pub fn new(msg: String, offset: Option<usize>, length: Option<usize>, line: usize) -> Self {
        Self {
            msg,
            offset,
            length,
            line,
        }
    }
}

impl Reportable for LoxError {
    fn report(&self, src: &[char]) {
        eprint!("[line {}] Error", self.line);

        if self.offset.is_some() && self.length.is_some() {
            let offset = self.offset.unwrap();
            if offset > src.len() {
                eprint!(" at end");
            } else {
                let length = self.length.unwrap();
                eprint!(
                    " at {}",
                    src[offset..offset + length].iter().collect::<String>()
                );
            }
        }

        eprintln!(": {}", self.msg);
    }
}
