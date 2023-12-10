use std::ffi::CStr;

use crate::elf::Endian;

#[derive(Debug, Clone)]
pub struct Program<E: Endian> {
    pub code: Vec<u8>,
    e: E,
}
impl<E: Endian> Program<E> {
    pub fn new(e: E) -> Self {
        Self {
            code: Vec::new(),
            e,
        }
    }
    pub fn imm(mut self, imm: u32) -> Self {
        self.code.extend_from_slice(&self.e.u32_bytes(imm));
        self
    }
    pub fn string(mut self, string: &CStr) -> Self {
        self.code.extend_from_slice(string.to_bytes_with_nul());
        self
    }
    pub fn jmp_rel(mut self, rel: i8) -> Self {
        self.code.push(0xEB);
        self.code.push(rel as u8);
        self
    }
    pub fn mov_eax_imm(mut self, imm: u32) -> Self {
        self.code.push(0xB8);
        self.imm(imm)
    }
    pub fn mov_ebx_imm(mut self, imm: u32) -> Self {
        self.code.push(0xBB);
        self.imm(imm)
    }
    pub fn mov_ecx_imm(mut self, imm: u32) -> Self {
        self.code.push(0xB9);
        self.imm(imm)
    }
    pub fn mov_edx_imm(mut self, imm: u32) -> Self {
        self.code.push(0xBA);
        self.imm(imm)
    }

    pub fn syscall(mut self) -> Self {
        self.code.extend_from_slice(&[0xCD, 0x80]);
        self
    }
    pub fn exit(self, code: u32) -> Self {
        self.mov_ebx_imm(code) // code
            .mov_eax_imm(0x01) // exit
            .syscall()
    }
    pub fn write(self, addr: u32, size: u32) -> Self {
        self.mov_ebx_imm(0x01) // stdout
            .mov_ecx_imm(addr) // buf
            .mov_edx_imm(size) // size
            .mov_eax_imm(0x04) // write
            .syscall()
    }
}
