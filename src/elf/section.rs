#![allow(unused)]

use super::*;
use std::ffi::{CStr, CString};

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
#[allow(clippy::from_over_into)]
impl<E: Endian> Into<U32<E>> for SHType {
    fn into(self) -> U32<E> {
        U32::new(self as u32)
    }
}

#[repr(u32)]
#[allow(clippy::upper_case_acronyms)]
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
impl std::ops::BitOr for SHFlag {
    type Output = u32;

    fn bitor(self, rhs: Self) -> Self::Output {
        self as u32 | rhs as u32
    }
}
#[allow(clippy::from_over_into)]
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
    pub value: U32<E>,
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
    pub fn zero() -> Self {
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

pub struct ELFStrings {
    strings: Vec<u8>,
}
impl ELFStrings {
    pub fn new() -> Self {
        Self { strings: vec![0x0] }
    }
    pub fn bytes(self) -> Vec<u8> {
        self.strings
    }
    pub fn add_str(&mut self, string: &str) -> Option<u32> {
        Some(self.add_cstr(&CString::new(string).ok()?))
    }
    pub fn add_cstr(&mut self, string: &CStr) -> u32 {
        let i = self.size();
        self.strings.extend_from_slice(string.to_bytes_with_nul());
        i
    }
    pub fn size(&self) -> u32 {
        self.strings.len() as u32
    }
}

pub struct SymbolTable<E: Endian> {
    pub strtab: (SectionHeader32<E>, ELFStrings),
    pub symbolsh: SectionHeader32<E>,
    pub entries: Vec<SymbolEntry<E>>,
}
impl<E: Endian> SymbolTable<E> {
    pub fn new() -> Self {
        let strings = ELFStrings::new();
        let tab = SectionHeader32 {
            typ: SHType::StrTab.into(),
            ..Default::default()
        };

        let symbolsh = SectionHeader32 {
            typ: SHType::SymTab.into(),
            entsize: U32::new(std::mem::size_of::<SymbolEntry<E>>() as u32),
            ..Default::default()
        };
        let entries = vec![SymbolEntry::zero()];

        Self {
            symbolsh,
            strtab: (tab, strings),
            entries,
        }
    }
    pub fn add(&mut self, name: &str, mut entry: SymbolEntry<E>) {
        let i = self.strtab.1.add_str(name).unwrap();
        entry.name = i.into();
        self.entries.push(entry);
    }
    pub fn set_info(&mut self) {
        let info = self.entries.len() as u32;
        self.symbolsh.info = info.into();
    }
}

pub struct SectionHeaders<E: Endian> {
    pub sections: Vec<(SectionHeader32<E>, Vec<u8>)>,
    pub shstrtab: (SectionHeader32<E>, ELFStrings),
}
impl<E: Endian> SectionHeaders<E> {
    pub fn new() -> Self {
        let mut strings = ELFStrings::new();
        let i = strings.add_str(".shstrtab").unwrap();
        let shstrtab = SectionHeader32 {
            name: U32::new(i),
            typ: SHType::StrTab.into(),
            ..Default::default()
        };

        Self {
            sections: vec![(SectionHeader32::<E>::null(), vec![])],
            shstrtab: (shstrtab, strings),
        }
    }
    pub fn add(&mut self, name: &str, mut header: SectionHeader32<E>, data: Vec<u8>) {
        let i = self.shstrtab.1.add_str(name).unwrap();
        header.name = i.into();
        self.sections.push((header, data));
    }
    pub fn update(&mut self, initial_offset: u32) {
        let mut offset = (std::mem::size_of::<SectionHeader32<E>>() * (self.sections.len() + 1))
            as u32
            + initial_offset;
        for (section, data) in self.sections.iter_mut().skip(1) {
            let size = data.len() as u32;
            section.offset = offset.into();
            section.size = size.into();
            offset += size;
        }

        let size = self.shstrtab.1.size();
        self.shstrtab.0.offset = offset.into();
        self.shstrtab.0.size = size.into();
    }
    pub fn bytes(self) -> Vec<u8> {
        self.sections
            .iter()
            .flat_map(|(h, _)| h.bytes())
            .chain(self.shstrtab.0.bytes())
            .chain(self.sections.iter().flat_map(|(_, d)| d))
            .chain(self.shstrtab.1.bytes().iter())
            .copied()
            .collect()
    }
    pub fn len(&self) -> u16 {
        self.sections.len() as u16 + 1
    }
    pub fn shstrndx(&self) -> u16 {
        self.sections.len() as u16
    }
}
