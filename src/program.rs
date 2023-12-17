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
}
#[repr(u8)]
#[allow(clippy::upper_case_acronyms)]
pub enum RM32 {
    EAX = 0b000,
    ECX = 0b001,
    EDX = 0b010,
    EBX = 0b011,
    // SIB = 0b100,
    /// this is also disp32 when Mod is 0
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
    pub fn mov_rm8_imm(&mut self, rm: RM32, disp: i8, imm: u32) {
        let modrm32 = modrm32(Mod32::Disp08, rm, Reg32::EAX);
        self.code.extend_from_slice(&[0xC7, modrm32, disp as u8]);
        self.imm32(imm);
    }
    pub fn mov_rm8_r(&mut self, rm: RM32, reg: Reg32, disp: i8) {
        let modrm32 = modrm32(Mod32::Disp08, rm, reg);
        self.code.extend_from_slice(&[0x89, modrm32, disp as u8]);
    }
    pub fn mov_r_rm8(&mut self, reg: Reg32, rm: RM32, disp: i8) {
        let modrm32 = modrm32(Mod32::Disp08, rm, reg);
        self.code.extend_from_slice(&[0x8B, modrm32, disp as u8]);
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

    // pub fn mov_ptr_esp_disp8_imm(&mut self, disp: i8, imm: u32) {
    //     self.code.extend_from_slice(&[0xC7, 0x45, disp as u8]);
    //     self.imm32(imm);
    // }
    // pub fn mov_eax_edp_disp8(&mut self, disp: i8) {
    //     self.code.extend_from_slice(&[0x8B, 0x5D, disp as u8]);
    // }
    // pub fn mov_ebx_edp_disp8(&mut self, disp: i8) {
    //     self.code.extend_from_slice(&[0x8B, 0x5D, disp as u8]);
    // }
    pub fn sub_esp_imm8(&mut self, imm: u8) {
        self.code.extend_from_slice(&[
            0x83, // sub
            0xEC, // esp
            imm,
        ]);
    }
    pub fn imm32(&mut self, imm: u32) {
        self.code.extend_from_slice(&E::u32_bytes(imm));
    }
    pub fn string(&mut self, string: &CStr) {
        self.code.extend_from_slice(string.to_bytes_with_nul());
    }
    pub fn jmp_rel(&mut self, rel: i8) {
        self.code.push(0xEB);
        self.code.push(rel as u8);
    }
    pub fn loop_fn<F: Fn(&mut Program<E>)>(&mut self, f: F) {
        let start = self.code.len() as i32;
        f(self);
        let end = self.code.len() as i32;
        self.jmp_rel((start - (end + 2)) as i8);
    }
    pub fn mov_eax_imm(&mut self, imm: u32) {
        self.code.push(0xB8);
        self.imm32(imm);
    }
    pub fn mov_ebx_imm(&mut self, imm: u32) {
        self.code.push(0xBB);
        self.imm32(imm);
    }
    pub fn mov_ecx_imm(&mut self, imm: u32) {
        self.code.push(0xB9);
        self.imm32(imm);
    }
    pub fn mov_edx_imm(&mut self, imm: u32) {
        self.code.push(0xBA);
        self.imm32(imm);
    }

    pub fn syscall(&mut self) {
        self.code.extend_from_slice(&[0xCD, 0x80]);
    }
    pub fn exit(&mut self) {
        self.mov_eax_imm(0x01); // exit
        self.syscall();
    }
    pub fn write(&mut self, addr: u32, size: u32) {
        self.mov_ebx_imm(0x01); // stdout
        self.mov_ecx_imm(addr); // buf
        self.mov_edx_imm(size); // size
        self.mov_eax_imm(0x04); // write
        self.syscall();
    }
}
