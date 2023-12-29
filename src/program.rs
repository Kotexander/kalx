#![allow(unused)]

use std::{ffi::CStr, marker::PhantomData};

use crate::elf::Endian;

type ModRM32 = u8;
#[repr(u8)]
#[allow(clippy::upper_case_acronyms)]
pub enum Mod32 {
    Disp00 = 0b00,
    Disp08 = 0b01,
    Disp32 = 0b10,
    Direct = 0b11,
}
#[repr(u8)]
#[allow(clippy::upper_case_acronyms)]
pub enum RM32 {
    EAX = 0b000,
    ECX = 0b001,
    EDX = 0b010,
    EBX = 0b011,
    /// this is ESP when Mod is 0b11
    SIB = 0b100,
    /// this is disp32 when Mod is 0b00
    EBP = 0b101,
    ESI = 0b110,
    EDI = 0b111,
}
#[repr(u8)]
#[allow(clippy::upper_case_acronyms)]
pub enum Reg32 {
    EAX = 0b000,
    ECX = 0b001,
    EDX = 0b010,
    EBX = 0b011,
    ESP = 0b100,
    EBP = 0b101,
    ESI = 0b110,
    EDI = 0b111,
}
pub fn modrm32(m: Mod32, rm: RM32, reg: Reg32) -> ModRM32 {
    ((m as u8) << 6) | ((reg as u8) << 3) | (rm as u8)
}

#[derive(Debug, Clone)]
pub struct Program<E: Endian> {
    pub code: Vec<u8>,
    phantom: PhantomData<E>,
}
impl<E: Endian> Program<E> {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            phantom: PhantomData,
        }
    }

    pub fn imm32(&mut self, imm: u32) {
        self.code.extend_from_slice(&E::u32_bytes(imm));
    }

    fn replace_u32(&mut self, addr: u32, imm: u32) {
        let bytes = E::u32_bytes(imm);
        for (i, b) in bytes.into_iter().enumerate() {
            self.code[addr as usize + i] = b;
        }
    }

    pub fn mov_rm8_imm(&mut self, rm: RM32, disp: i8, imm: u32) -> u32 {
        let modrm32 = modrm32(Mod32::Disp08, rm, Reg32::EAX);
        self.code.extend_from_slice(&[0x81, modrm32, disp as u8]);
        let rel = self.code.len() as u32;
        self.imm32(imm);
        rel
    }
    pub fn mov_rm8_r(&mut self, rm: RM32, reg: Reg32, disp: i8) {
        let modrm32 = modrm32(Mod32::Disp08, rm, reg);
        self.code.extend_from_slice(&[0x89, modrm32, disp as u8]);
    }
    pub fn mov_r_rm8(&mut self, reg: Reg32, rm: RM32, disp: i8) {
        let modrm32 = modrm32(Mod32::Disp08, rm, reg);
        self.code.extend_from_slice(&[0x8B, modrm32, disp as u8]);
    }

    pub fn mov_eax_imm(&mut self, imm: u32) -> u32 {
        self.code.push(0xB8);
        let rel = self.code.len() as u32;
        self.imm32(imm);
        rel
    }
    pub fn mov_ebx_imm(&mut self, imm: u32) -> u32 {
        self.code.push(0xBB);
        let rel = self.code.len() as u32;
        self.imm32(imm);
        rel
    }
    pub fn mov_ecx_imm(&mut self, imm: u32) -> u32 {
        self.code.push(0xB9);
        let rel = self.code.len() as u32;
        self.imm32(imm);
        rel
    }
    pub fn mov_edx_imm(&mut self, imm: u32) -> u32 {
        self.code.push(0xBA);
        let rel = self.code.len() as u32;
        self.imm32(imm);
        rel
    }

    /// used to sace the stack
    pub fn mov_edp_esp(&mut self) {
        self.code.extend_from_slice(&[
            0x89, // move
            0xE5, // esp -> ebp
        ]);
    }
    /// used to restore the stack
    pub fn mov_esp_edp(&mut self) {
        self.code.extend_from_slice(&[
            0x89, // move
            0xEC, // ebp -> esp
        ]);
    }

    pub fn add_eax_imm(&mut self, imm: u32) {
        self.code.push(0x05);
        self.imm32(imm);
    }
    pub fn add_rm8_imm(&mut self, rm: RM32, disp: i8, imm: u32) {
        let modrm32 = modrm32(Mod32::Disp08, rm, Reg32::EAX);
        self.code.extend_from_slice(&[0x81, modrm32, disp as u8]);
        self.imm32(imm);
    }
    pub fn add_rm_imm(&mut self, rm: RM32, imm: u32) {
        let modrm32 = modrm32(Mod32::Direct, rm, Reg32::EAX);
        self.code.extend_from_slice(&[0x81, modrm32]);
        self.imm32(imm);
    }
    pub fn add_rm8_r(&mut self, rm: RM32, reg: Reg32, disp: i8) {
        let modrm32 = modrm32(Mod32::Disp08, rm, reg);
        self.code.extend_from_slice(&[0x01, modrm32, disp as u8]);
    }
    pub fn add_r_rm8(&mut self, reg: Reg32, rm: RM32, disp: i8) {
        let modrm32 = modrm32(Mod32::Disp08, rm, reg);
        self.code.extend_from_slice(&[0x03, modrm32, disp as u8]);
    }

    pub fn sub_rm8_r(&mut self, rm: RM32, reg: Reg32, disp: i8) {
        let modrm32 = modrm32(Mod32::Disp08, rm, reg);
        self.code.extend_from_slice(&[0x29, modrm32, disp as u8]);
    }
    pub fn sub_r_rm8(&mut self, reg: Reg32, rm: RM32, disp: i8) {
        let modrm32 = modrm32(Mod32::Disp08, rm, reg);
        self.code.extend_from_slice(&[0x2B, modrm32, disp as u8]);
    }
    pub fn sub_rm_imm(&mut self, rm: RM32, imm: u32) {
        let modrm32 = modrm32(Mod32::Direct, rm, Reg32::EBP);
        self.code.extend_from_slice(&[0x81, modrm32]);
        self.imm32(imm);
    }

    pub fn sub_esp_imm8(&mut self, imm: u8) {
        self.code.extend_from_slice(&[
            0x83, // sub
            0xEC, // esp
            imm,
        ]);
    }
    pub fn add_esp_imm8(&mut self, imm: u8) {
        self.code.extend_from_slice(&[
            0x83, // sub
            0xC4, // esp
            imm,
        ]);
    }

    pub fn string(&mut self, string: &CStr) {
        self.code.extend_from_slice(string.to_bytes_with_nul());
    }
    pub fn jmp_rel(&mut self, rel: i8) {
        self.code.extend_from_slice(&[0xEB, rel as u8]);
    }
    pub fn jl(&mut self, rel: i8) {
        self.code.extend_from_slice(&[0x7C, rel as u8]);
    }
    pub fn jg(&mut self, rel: i8) {
        self.code.extend_from_slice(&[0x7f, rel as u8]);
    }
    pub fn loop_fn<F: FnMut(&mut Program<E>)>(&mut self, mut f: F) {
        let start = self.code.len();
        f(self);
        self.jmp_rel(0);
        let end = self.code.len();
        self.code[end - 1] = (start as i32 - end as i32) as u8;
    }

    pub fn cmp_r_rm(&mut self, reg: Reg32, rm: RM32) {
        let modrm32 = modrm32(Mod32::Direct, rm, reg);
        self.code.extend_from_slice(&[0x3B, modrm32])
    }

    pub fn push_eax(&mut self) {
        self.code.push(0x50);
    }
    pub fn call_rel32(&mut self, rel: i32) -> u32 {
        self.code.push(0xE8);
        let r = self.code.len() as u32;
        self.imm32(rel as u32);
        r
    }

    pub fn syscall(&mut self) {
        self.code.extend_from_slice(&[0xCD, 0x80]);
    }
    pub fn exit(&mut self) {
        self.mov_eax_imm(0x01); // exit
        self.syscall();
    }

    /// returns a relocation address
    pub fn write_const(&mut self, addr: u32, size: u32) -> u32 {
        self.mov_ebx_imm(0x01); // stdout
        let rel = self.mov_ecx_imm(addr); // buf
        self.mov_edx_imm(size); // size
        self.mov_eax_imm(0x04); // write
        self.syscall();
        rel
    }

    pub fn write_rm8(&mut self, rm: RM32, disp: i8, size: u32) {
        self.mov_ebx_imm(0x01); // stdout
        self.mov_r_rm8(Reg32::ECX, rm, disp);
        self.mov_edx_imm(size); // size
        self.mov_eax_imm(0x04); // write
        self.syscall();
    }
}
