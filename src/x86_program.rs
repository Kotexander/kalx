#![allow(unused)]

use std::{ffi::CStr, marker::PhantomData, fmt::Display};

use crate::elf::Endian;

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
#[allow(clippy::upper_case_acronyms)]
pub enum Mod32 {
    Disp00 = 0b00,
    Disp08 = 0b01,
    Disp32 = 0b10,
    Direct = 0b11,
}
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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
type ModRM32 = u8;
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

    pub fn replace_u8(&mut self, addr: u32, b: u8) {
        self.code[addr as usize] = b;
    }
    pub fn replace_u32(&mut self, addr: u32, imm: u32) {
        let bytes = E::u32_bytes(imm);
        for (i, b) in bytes.into_iter().enumerate() {
            self.code[addr as usize + i] = b;
        }
    }

    pub fn mov_rm8_imm32(&mut self, rm: RM32, disp: i8, imm: u32) -> u32 {
        let modrm32 = modrm32(Mod32::Disp08, rm, Reg32::EAX);
        self.code.extend_from_slice(&[0xC7, modrm32, disp as u8]);
        let rel = self.addr();
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
    pub fn mov_rm_r(&mut self, rm: RM32, reg: Reg32) {
        let modrm32 = modrm32(Mod32::Direct, rm, reg);
        self.code.extend_from_slice(&[0x89, modrm32]);
    }
    pub fn mov_r_rm(&mut self, reg: Reg32, rm: RM32) {
        let modrm32 = modrm32(Mod32::Direct, rm, reg);
        self.code.extend_from_slice(&[0x8B, modrm32]);
    }

    pub fn mov_r_imm32(&mut self, reg: Reg32, imm: u32) -> u32 {
        self.code.push(0xB8 + reg as u8);
        let rel = self.addr();
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

    pub fn add_eax_imm32(&mut self, imm: u32) {
        self.code.push(0x05);
        self.imm32(imm);
    }
    pub fn sub_eax_imm32(&mut self, imm: u32) {
        self.code.push(0x2D);
        self.imm32(imm);
    }

    pub fn add_rm8_imm32(&mut self, rm: RM32, disp: i8, imm: u32) {
        let modrm32 = modrm32(Mod32::Disp08, rm, Reg32::EAX);
        self.code.extend_from_slice(&[0x81, modrm32, disp as u8]);
        self.imm32(imm);
    }
    pub fn add_rm_imm32(&mut self, rm: RM32, imm: u32) {
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
    pub fn add_r_rm(&mut self, reg: Reg32, rm: RM32) {
        let modrm32 = modrm32(Mod32::Direct, rm, reg);
        self.code.extend_from_slice(&[0x03, modrm32]);
    }
    pub fn add_rm_r(&mut self, rm: RM32, reg: Reg32) {
        let modrm32 = modrm32(Mod32::Direct, rm, reg);
        self.code.extend_from_slice(&[0x01, modrm32]);
    }

    pub fn sub_rm_imm32(&mut self, rm: RM32, imm: u32) {
        let modrm32 = modrm32(Mod32::Direct, rm, Reg32::EBP);
        self.code.extend_from_slice(&[0x81, modrm32]);
        self.imm32(imm);
    }
    pub fn sub_rm8_imm32(&mut self, rm: RM32, disp: i8, imm: u32) {
        let modrm32 = modrm32(Mod32::Disp08, rm, Reg32::EBP);
        self.code.extend_from_slice(&[0x81, modrm32, disp as u8]);
        self.imm32(imm);
    }
    pub fn sub_rm8_r(&mut self, rm: RM32, reg: Reg32, disp: i8) {
        let modrm32 = modrm32(Mod32::Disp08, rm, reg);
        self.code.extend_from_slice(&[0x29, modrm32, disp as u8]);
    }
    pub fn sub_r_rm8(&mut self, reg: Reg32, rm: RM32, disp: i8) {
        let modrm32 = modrm32(Mod32::Disp08, rm, reg);
        self.code.extend_from_slice(&[0x2B, modrm32, disp as u8]);
    }
    pub fn sub_rm_r(&mut self, rm: RM32, reg: Reg32) {
        let modrm32 = modrm32(Mod32::Direct, rm, reg);
        self.code.extend_from_slice(&[0x29, modrm32]);
    }
    pub fn sub_r_rm(&mut self, reg: Reg32, rm: RM32) {
        let modrm32 = modrm32(Mod32::Direct, rm, reg);
        self.code.extend_from_slice(&[0x2B, modrm32]);
    }

    pub fn sub_esp_imm8(&mut self, imm: i8) {
        self.code.extend_from_slice(&[
            0x83, // sub
            0xEC, // esp
            imm as u8,
        ]);
    }
    pub fn add_esp_imm8(&mut self, imm: u8) {
        self.code.extend_from_slice(&[
            0x83, // sub
            0xC4, // esp
            imm,
        ]);
    }

    pub fn mul_r_rm_imm8(&mut self, reg: Reg32, rm: RM32, imm: u8) {
        let modrm32 = modrm32(Mod32::Direct, rm, reg);
        self.code.push(0x6B);
        self.code.extend_from_slice(&[modrm32, imm])
    }
    pub fn mul_r_rm_imm32(&mut self, reg: Reg32, rm: RM32, imm: u32) {
        let modrm32 = modrm32(Mod32::Direct, rm, reg);
        self.code.push(0x69);
        self.code.push(modrm32);
        self.imm32(imm);
    }
    pub fn mul_r_rm8_imm32(&mut self, reg: Reg32, rm: RM32, disp: i8, imm: u32) {
        let modrm32 = modrm32(Mod32::Disp08, rm, reg);
        self.code.push(0x69);
        self.code.extend_from_slice(&[modrm32, disp as u8]);
        self.imm32(imm);
    }
    pub fn mul_r_rm8(&mut self, reg: Reg32, rm: RM32, disp: i8) {
        let modrm32 = modrm32(Mod32::Disp08, rm, reg);
        self.code
            .extend_from_slice(&[0x0F, 0xAF, modrm32, disp as u8]);
    }
    pub fn mul_r_rm(&mut self, reg: Reg32, rm: RM32) {
        let modrm32 = modrm32(Mod32::Direct, rm, reg);
        self.code.extend_from_slice(&[0x0F, 0xAF, modrm32]);
    }
    pub fn div_rm(&mut self, rm: RM32) {
        let modrm32 = modrm32(Mod32::Direct, rm, Reg32::ESI);
        self.code.extend_from_slice(&[0xF7, modrm32]);
    }
    pub fn idiv_rm(&mut self, rm: RM32) {
        let modrm32 = modrm32(Mod32::Direct, rm, Reg32::EDI);
        self.code.extend_from_slice(&[0xF7, modrm32]);
    }

    pub fn cmp_eax_imm32(&mut self, imm: u32) {
        self.code.push(0x3D);
        self.imm32(imm);
    }
    pub fn cmp_r_rm(&mut self, reg: Reg32, rm: RM32) {
        let modrm32 = modrm32(Mod32::Direct, rm, reg);
        self.code.extend_from_slice(&[0x3B, modrm32])
    }
    pub fn cmp_rm_r(&mut self, rm: RM32, reg: Reg32) {
        let modrm32 = modrm32(Mod32::Direct, rm, reg);
        self.code.extend_from_slice(&[0x39, modrm32])
    }
    pub fn cmp_rm_imm32(&mut self, rm: RM32, imm: u32) {
        let modrm32 = modrm32(Mod32::Direct, rm, Reg32::EDI);
        self.code.extend_from_slice(&[0x81, modrm32]);
        self.imm32(imm);
    }
    pub fn cmp_rm8_r(&mut self, rm: RM32, r: Reg32, disp: i8) {
        let modrm32 = modrm32(Mod32::Disp08, rm, r);
        self.code.extend_from_slice(&[0x39, modrm32, disp as u8]);
    }
    pub fn cmp_r_rm8(&mut self, reg: Reg32, rm: RM32, disp: i8) {
        let modrm32 = modrm32(Mod32::Disp08, rm, reg);
        self.code.extend_from_slice(&[0x3B, modrm32, disp as u8])
    }
    pub fn cmp_rm8_imm32(&mut self, rm: RM32, disp: i8, imm: u32) {
        let modrm32 = modrm32(Mod32::Disp08, rm, Reg32::EDI);
        self.code.extend_from_slice(&[0x81, modrm32, disp as u8]);
        self.imm32(imm);
    }

    pub fn push_imm32(&mut self, imm: u32) -> u32 {
        self.code.push(0x68);
        let rel = self.addr();
        self.imm32(imm);
        rel
    }
    pub fn push_r(&mut self, reg: Reg32) {
        self.code.push(0x50 + reg as u8);
    }
    // pub fn push_rm(&mut self, rm: RM32) {
    //     let modrm32 = modrm32(Mod32::Direct, rm, Reg32::ESI);
    //     self.code.extend_from_slice(&[0xFF, modrm32]);
    // }
    pub fn push_rm8(&mut self, rm: RM32, disp: i8) {
        let modrm32 = modrm32(Mod32::Disp08, rm, Reg32::ESI);
        self.code.extend_from_slice(&[0xFF, modrm32, disp as u8]);
    }

    pub fn call_rel32(&mut self, rel: i32) -> u32 {
        self.code.push(0xE8);
        let r = self.addr();
        self.imm32(rel as u32);
        r
    }

    pub fn jmp_rel(&mut self, rel: i8) -> u32 {
        self.code.push(0xEB);
        let rel = self.addr();
        self.code.push(rel as u8);
        rel
    }
    pub fn jl(&mut self, rel: i8) -> u32 {
        self.code.push(0x7C);
        let rel = self.addr();
        self.code.push(rel as u8);
        rel
    }
    pub fn jg(&mut self, rel: i8) -> u32 {
        self.code.push(0x7F);
        let rel = self.addr();
        self.code.push(rel as u8);
        rel
    }
    pub fn jle(&mut self, rel: i8) -> u32 {
        self.code.push(0x7E);
        let rel = self.addr();
        self.code.push(rel as u8);
        rel
    }
    pub fn jge(&mut self, rel: i8) -> u32 {
        self.code.push(0x7D);
        let rel = self.addr();
        self.code.push(rel as u8);
        rel
    }
    pub fn jne(&mut self, rel: i8) -> u32 {
        self.code.push(0x75);
        let rel = self.addr();
        self.code.push(rel as u8);
        rel
    }

    pub fn set_e(&mut self, rm: RM32) {
        let modrm32 = modrm32(Mod32::Direct, rm, Reg32::EAX);
        self.code.extend_from_slice(&[0x0F, 0x94, modrm32]);
    }
    pub fn set_ne(&mut self, rm: RM32) {
        let modrm32 = modrm32(Mod32::Direct, rm, Reg32::EAX);
        self.code.extend_from_slice(&[0x0F, 0x95, modrm32]);
    }
    pub fn set_l(&mut self, rm: RM32) {
        let modrm32 = modrm32(Mod32::Direct, rm, Reg32::EAX);
        self.code.extend_from_slice(&[0x0F, 0x9C, modrm32]);
    }
    pub fn set_ge(&mut self, rm: RM32) {
        let modrm32 = modrm32(Mod32::Direct, rm, Reg32::EAX);
        self.code.extend_from_slice(&[0x0F, 0x9D, modrm32]);
    }
    pub fn set_le(&mut self, rm: RM32) {
        let modrm32 = modrm32(Mod32::Direct, rm, Reg32::EAX);
        self.code.extend_from_slice(&[0x0F, 0x9E, modrm32]);
    }
    pub fn set_g(&mut self, rm: RM32) {
        let modrm32 = modrm32(Mod32::Direct, rm, Reg32::EAX);
        self.code.extend_from_slice(&[0x0F, 0x9F, modrm32]);
    }

    pub fn movzx(&mut self, r: Reg32, rm: RM32) {
        let modrm32 = modrm32(Mod32::Direct, rm, r);
        self.code.extend_from_slice(&[0x0F, 0xB6, modrm32])
    }

    pub fn loop_fn<F: FnMut(&mut Program<E>)>(&mut self, mut f: F) {
        let start = self.addr();
        f(self);
        self.jmp_rel(0);
        let end = self.addr();
        self.code[(end - 1) as usize] = (start as i32 - end as i32) as u8;
    }

    pub fn syscall(&mut self) {
        self.code.extend_from_slice(&[0xCD, 0x80]);
    }
    pub fn exit(&mut self) {
        self.mov_r_imm32(Reg32::EAX, 0x01);
        self.syscall();
    }

    /// returns a relocation address
    // pub fn write_const(&mut self, addr: u32, size: u32) -> u32 {
    //     self.mov_ebx_imm32(0x01); // stdout
    //     let rel = self.mov_ecx_imm32(addr); // buf
    //     self.mov_edx_imm32(size); // size
    //     self.mov_eax_imm32(0x04); // write
    //     self.syscall();
    //     rel
    // }

    // pub fn write_rm8(&mut self, rm: RM32, disp: i8, size: u32) {
    //     self.mov_ebx_imm32(0x01); // stdout
    //     self.mov_r_rm8(Reg32::ECX, rm, disp);
    //     self.mov_edx_imm32(size); // size
    //     self.mov_eax_imm32(0x04); // write
    //     self.syscall();
    // }

    pub fn string(&mut self, string: &CStr) {
        self.code.extend_from_slice(string.to_bytes_with_nul());
    }

    pub fn addr(&self) -> u32 {
        self.code.len() as u32
    }
}
impl<E: Endian> Display for Program<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // writeln!(f, "offset: {}", self.offset)?;

        for line in self.code.chunks(16) {
            for byte in line {
                write!(f, "{byte:02X} ");
            }
            writeln!(f)?;
        }
        
        std::fmt::Result::Ok(())
    }
}