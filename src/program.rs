use std::ffi::CStr;

#[derive(Debug, Clone)]
pub struct Program {
    pub code: Vec<u8>,
}
impl Program {
    pub fn syscall(mut self) -> Self {
        self.code.push(0xCD); // int
        self.code.push(0x80); // syscall
        self
    }
    pub fn string(mut self, string: &CStr) -> Self {
        self.code.extend_from_slice(string.to_bytes_with_nul());
        self
    }
    pub fn exit(mut self, code: [u8; 4]) -> Self {
        self.code.extend_from_slice(&[0xB8, 0x01, 0x00, 0x00, 0x00]); // eax <- 1 (exot)
        self.code.push(0xBB); // ebx <- code
        self.code.extend_from_slice(&code);

        self.syscall()
    }
}
