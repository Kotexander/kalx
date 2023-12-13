#![allow(unused)]

use std::marker::PhantomData;

mod program;
pub use program::*;

mod section;
pub use section::*;

pub enum Endianess {
    Little,
    Big,
}
pub trait Endian {
    fn endianess() -> Endianess;

    fn u16_bytes(n: u16) -> [u8; 2] {
        match Self::endianess() {
            Endianess::Little => n.to_le_bytes(),
            Endianess::Big => n.to_be_bytes(),
        }
    }
    fn u32_bytes(n: u32) -> [u8; 4] {
        match Self::endianess() {
            Endianess::Little => n.to_le_bytes(),
            Endianess::Big => n.to_be_bytes(),
        }
    }
}
pub unsafe trait Pod: Sized {
    fn bytes(&self) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts(std::ptr::addr_of!(*self) as _, std::mem::size_of::<Self>())
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LittleEndian;
impl Endian for LittleEndian {
    fn endianess() -> Endianess {
        Endianess::Little
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BigEndian;
impl Endian for BigEndian {
    fn endianess() -> Endianess {
        Endianess::Big
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct U16<E: Endian>([u8; 2], PhantomData<E>);
impl<E: Endian> U16<E> {
    pub fn new(n: u16) -> Self {
        Self(E::u16_bytes(n), PhantomData)
    }
    pub fn zero() -> Self {
        Self([0, 0], PhantomData)
    }
    // pub fn bytes(self) -> [u8; 2] {
    //     self.0
    // }
}
#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct U32<E: Endian>([u8; 4], PhantomData<E>);
impl<E: Endian> U32<E> {
    pub fn new(n: u32) -> Self {
        Self(E::u32_bytes(n), PhantomData)
    }
    pub fn zero() -> Self {
        Self([0, 0, 0, 0], PhantomData)
    }
    // pub fn bytes(self) -> [u8; 4] {
    // self.0
    // }
}

#[repr(u8)]
pub enum EFIIdentClass {
    Bit32 = 1,
    Bit64 = 2,
}
#[repr(u8)]
pub enum EFIIData {
    LittleEndian = 1,
    BigEndian = 2,
}
impl From<Endianess> for EFIIData {
    fn from(value: Endianess) -> Self {
        match value {
            Endianess::Little => Self::LittleEndian,
            Endianess::Big => Self::BigEndian,
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct EFIIdent {
    /// ELF Magic. Always set to [0x7F, b'E', b'L', b'F']
    pub magic: [u8; 4],
    /// File class
    ///
    /// Equals to [`EFIIdentClass`]
    pub class: u8,
    /// Data encoding
    ///
    /// Equals to [`EFIIdentData`]
    pub data: u8,
    /// Current version of ELF. Always set 1
    pub version: u8,
    /// OS ABI
    /// * set 3 for Linux but 0 is also ok
    pub os_abi: u8,
    /// ABI version
    /// * 0 is ok for Linux
    pub abi_version: u8,
    /// unused
    pub padding: [u8; 7],
}
impl EFIIdent {
    pub const MAGIC: [u8; 4] = [0x7F, b'E', b'L', b'F'];
    pub fn new_32bit(endianess: Endianess) -> Self {
        Self {
            class: EFIIdentClass::Bit32 as u8,
            data: EFIIData::from(endianess) as u8,
            ..Default::default()
        }
    }
}
impl Default for EFIIdent {
    fn default() -> Self {
        Self {
            magic: Self::MAGIC,
            class: 0,
            data: 0,
            version: 1,
            os_abi: 0,
            abi_version: 0,
            padding: [0; 7],
        }
    }
}

#[repr(u16)]
pub enum EFIHType {
    None = 0x00,
    Rel = 0x01,
    Exec = 0x02,
    Dyn = 0x03,
    Core = 0x04,
    LowOS = 0xFE00,
    HighOS = 0xFEFF,
    LowProc = 0xFF00,
    HighProc = 0xFFFF,
}
impl<E: Endian> Into<U16<E>> for EFIHType {
    fn into(self) -> U16<E> {
        U16::new(self as u16)
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct ELFHeader32<E: Endian> {
    pub ident: EFIIdent,
    /// Object file type
    ///
    /// Equals to [`EFIIdentType`]
    pub typ: U16<E>,
    /// target architecture
    pub machine: U16<E>,
    /// always set 1
    pub version: U32<E>,
    /// entry point
    pub entry: U32<E>,
    /// program header table
    pub phoff: U32<E>,
    /// section header table
    pub shoff: U32<E>,
    /// flags specific to machine
    pub flags: U32<E>,
    /// this header's size
    pub ehsize: U16<E>,
    /// program header's size
    pub phentsize: U16<E>,
    /// number of program header table entries
    pub phnum: U16<E>,
    /// section header table size
    pub shentsize: U16<E>,
    /// number of section header table entries
    pub shnum: U16<E>,
    /// index of the section header table entry that contains the section names
    pub shstrndx: U16<E>,
}
impl<E: Endian> Default for ELFHeader32<E> {
    fn default() -> Self {
        use std::mem::size_of;
        Self {
            ident: Default::default(),
            typ: EFIHType::None.into(),
            machine: U16::zero(),
            version: U32::new(1),
            entry: U32::zero(),
            phoff: U32::zero(),
            shoff: U32::zero(),
            flags: U32::zero(),
            ehsize: U16::new(size_of::<Self>() as u16),
            phentsize: U16::new(size_of::<ProgramHeader32<E>>() as u16),
            phnum: U16::zero(),
            shentsize: U16::new(size_of::<SectionHeader32<E>>() as u16),
            shnum: U16::zero(),
            shstrndx: U16::zero(),
        }
    }
}
unsafe impl<E: Endian> Pod for ELFHeader32<E> {}
