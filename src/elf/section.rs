#![allow(unused)]

use super::*;
use std::ffi::CStr;

#[repr(u32)]
pub enum SHType {
    Null = 0x00,
    ProgBits = 0x01,
    SymTab = 0x02,
    StrTab = 0x03,
    Rela = 0x04,
    Hash = 0x05,
    Dynamic = 0x06,
    Note = 0x07,
    NoBits = 0x08,
    Rel = 0x09,
    SHLib = 0x0A,
    DynSym = 0x0B,
    InnitArray = 0x0E,
    FiniArray = 0x0F,
    PreInnitArray = 0x10,
    Group = 0x11,
    SymTabSHNDX = 0x12,
    Num = 0x13,
}
impl<E: Endian> Into<U32<E>> for SHType {
    fn into(self) -> U32<E> {
        U32::new(self as u32)
    }
}

#[repr(u32)]
pub enum SHFlag {
    Write = 0x1,
    Alloc = 0x2,
    ExecInstr = 0x4,
    Merge = 0x10,
    Strings = 0x20,
    InfoLink = 0x40,
    LinkOrder = 0x80,
    OSNonConforming = 0x100,
    Group = 0x200,
    /// holds thread local data
    TLS = 0x400,
    MaskOS = 0x0FF00000,
    MaskProc = 0xF0000000,
    // Ordered = 0x4000000,
    // Exclude = 0x8000000,
}
impl<E: Endian> Into<U32<E>> for SHFlag {
    fn into(self) -> U32<E> {
        U32::new(self as u32)
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct SectionHeader32<E: Endian> {
    /// offset to a string in the .shstrtab section
    pub name: U32<E>,
    /// Section type
    ///
    /// Equals to [`SHType`]
    pub typ: U32<E>,
    /// Section Flag
    ///
    /// Equals to [`SHFlag`]
    pub flags: U32<E>,
    /// virtual address of section in memory (only if loaded)
    pub addr: U32<E>,
    /// section offset in file image
    pub offset: U32<E>,
    /// size in bytes
    pub size: U32<E>,
    /// depends on type
    pub link: U32<E>,
    /// depends on type
    pub info: U32<E>,
    /// required alignment
    pub addralign: U32<E>,
    /// size in bytes of each entry for sections
    /// that have fixed-size entries
    ///
    /// otherwise this is zero
    pub entsize: U32<E>,
}
impl<E: Endian> SectionHeader32<E> {
    pub fn null() -> Self {
        Self::default()
    }
}
impl<E: Endian> Default for SectionHeader32<E> {
    fn default() -> Self {
        Self {
            name: U32::zero(),
            typ: U32::zero(),
            flags: U32::zero(),
            addr: U32::zero(),
            offset: U32::zero(),
            size: U32::zero(),
            link: U32::zero(),
            info: U32::zero(),
            addralign: U32::zero(),
            entsize: U32::zero(),
        }
    }
}
unsafe impl<E: Endian> Pod for SectionHeader32<E> {}

#[repr(u8)]
pub enum SEBind {
    Local = 0,
    Global = 1,
    Weak = 2,
    LowProc = 13,
    HighProc = 15,
}
#[repr(u8)]
pub enum SEType {
    NoType = 0,
    Object = 1,
    Func = 2,
    Section = 3,
    File = 4,
    LowProc = 13,
    HighProc = 15,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct SymbolEntry<E: Endian> {
    /// offset to a string defined by this entie's section header
    pub name: U32<E>,
    ///
    pub value: U32<E>,
    ///
    pub size: U32<E>,
    /// use [`Self::info()`]
    pub info: u8,
    /// 0 unused
    pub other: u8,
    pub shndx: U16<E>,
}
impl<E: Endian> SymbolEntry<E> {
    pub const fn info(b: SEBind, t: SEType) -> u8 {
        ((b as u8) << 4) + ((t as u8) & 0xF)
    }
    pub fn index_zero() -> Self {
        Self::default()
    }
}
impl<E: Endian> Default for SymbolEntry<E> {
    fn default() -> Self {
        Self {
            name: U32::zero(),
            value: U32::zero(),
            size: U32::zero(),
            info: 0,
            other: 0,
            shndx: U16::zero(),
        }
    }
}
unsafe impl<E: Endian> Pod for SymbolEntry<E> {}

pub struct StringTable {
    strings: Vec<u8>,
}
impl StringTable {
    pub fn new() -> Self {
        Self { strings: vec![0x0] }
    }
    pub fn bytes(&self) -> &[u8] {
        &self.strings
    }
    pub fn add(&mut self, string: &CStr) -> u32 {
        let i = self.size();
        self.strings.extend_from_slice(string.to_bytes_with_nul());
        i
    }
    pub fn size(&self) -> u32 {
        self.strings.len() as u32
    }
}

// pub struct SectionHeaderTable<E: Endian> {
//     headers: Vec<SectionHeader32<E>>,
// }
// impl<E: Endian> SectionHeaderTable<E> {
//     pub fn new() -> Self {
//         Self { headers: vec![] }
//     }
//     pub fn add(&mut self, header: SectionHeader32<E>) {
//         self.headers.push(header);
//     }
//     pub fn bytes(self) -> Vec<u8> {
//         self.headers
//             .into_iter()
//             .map(|h| h.bytes().to_vec())
//             .flatten()
//             .collect()
//     }
// }

// struct ELFBuilder<E: Endian> {
//     header: ELFHeader32<E>,
// }
// impl<E: Endian> ELFBuilder<E> {
//     fn new(header: ELFHeader32<E>) -> Self {
//         Self { header }
//     }
// }
