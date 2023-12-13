use std::ffi::CStr;
use std::fmt::Debug;
use std::marker::PhantomData;

pub enum Endianess {
    Little,
    Big,
}
pub trait Endian: Copy {
    fn endianess(&self) -> Endianess;

    fn u16_bytes(self, n: u16) -> [u8; 2] {
        match self.endianess() {
            Endianess::Little => n.to_le_bytes(),
            Endianess::Big => n.to_be_bytes(),
        }
    }
    fn u32_bytes(self, n: u32) -> [u8; 4] {
        match self.endianess() {
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
    fn endianess(&self) -> Endianess {
        Endianess::Little
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BigEndian;
impl Endian for BigEndian {
    fn endianess(&self) -> Endianess {
        Endianess::Big
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct U16<E: Endian>([u8; 2], PhantomData<E>);
impl<E: Endian> U16<E> {
    pub fn new(e: E, n: u16) -> Self {
        Self(e.u16_bytes(n), PhantomData)
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
    pub fn new(e: E, n: u32) -> Self {
        Self(e.u32_bytes(n), PhantomData)
    }
    pub fn zero() -> Self {
        Self([0, 0, 0, 0], PhantomData)
    }
    // pub fn bytes(self) -> [u8; 4] {
    // self.0
    // }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct EFIIdent {
    /// ELF Magic. Always set to [0x7F, b'E', b'L', b'F']
    pub magic: [u8; 4],
    /// File class
    /// * set 1 for 32 bit
    /// * set 2 for 64 bit
    pub class: u8,
    /// Data encoding
    /// * set 1 for little endian
    /// * set 2 for big endian
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
        let data = match endianess {
            Endianess::Little => 1,
            Endianess::Big => 2,
        };
        Self {
            magic: Self::MAGIC,
            class: 1,       // 32 bit
            data,           // endianess
            version: 1,     // always 1
            os_abi: 0,      // SYS V
            abi_version: 0, // always 0
            padding: [0; 7],
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct ELFHeader32<E: Endian> {
    pub ident: EFIIdent,
    /// object file type
    /// * set 0 for none
    /// * set 1 for relocatable file
    /// * set 2 for executable file
    /// * set 3 for shared file
    /// * set 4 for core file
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
unsafe impl<E: Endian> Pod for ELFHeader32<E> {}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct ProgramHeader32<E: Endian> {
    /// segment type
    /// * 0 null (unused)
    /// * 1 loadable segment
    /// * 2 dynamic linking information
    /// * 3 interpreter information
    /// * 4 auxiliary information
    /// * 6 program header table
    /// * 7 thread-local storage template
    pub typ: U32<E>,
    /// where the file image is from in the file
    pub offset: U32<E>,
    /// virtual address
    pub vaddr: U32<E>,
    /// pyhiscal address
    pub paddr: U32<E>,
    /// file image size (in bytes)
    pub filesz: U32<E>,
    /// memomry size (in bytes)
    pub memsz: U32<E>,
    /// * 1 executable
    /// * 2 writeable
    /// * 4 readable
    pub flags: U32<E>,
    /// set 0 or 1 for no alignment. Otherwise, must be a power of 2.
    pub align: U32<E>,
}
unsafe impl<E: Endian> Pod for ProgramHeader32<E> {}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct SectionHeader32<E: Endian> {
    /// offset to a string in the .shstrtab section
    pub name: U32<E>,
    /// section
    /// * 0x00 null
    /// * 0x01 program data
    /// * 0x02 symbol table
    /// * 0x03 string table
    /// * 0x04 relocation entries with addends
    /// * 0x05 symbol hash table
    /// * 0x06 dynamic linking information
    /// * 0x07 notes
    /// * 0x08 program space with no data (bss)
    /// * 0x09 relocation entries, no addends
    /// * 0x0A SHT_SHLIB (reserved)
    /// * 0x0B dynamic linker symbol table
    /// * 0x0E array of constructors
    /// * 0x0F array of deconstructors
    /// * 0x10 array of pre-constructors
    /// * 0x11 section group
    /// * 0x12 extended section indices
    /// * 0x13 number of defined types
    pub typ: U32<E>,
    /// * 0b00000001 writeable
    /// * 0b00000010 occupies memory during execution (ALLOC)
    /// * 0b00000100 might be merged
    /// * 0b00001000 contains null terminated strings
    /// * 0b00010000 contains SHT index
    /// * 0b00100000 preserve order after combining
    /// * 0b01000000 non standard OS specfig handling required
    /// * 0b10000000 sectino hold thread-local data
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

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct SymbolEntry<E: Endian> {
    /// offset to a string defined by this entie's section header
    pub name: U32<E>,
    ///
    pub value: U32<E>,
    ///
    pub size: U32<E>,
    /// use `bind()` `typ()` or `info()`
    pub info: u8,
    /// 0 unused
    pub other: u8,
    pub shndx: U16<E>,
}
impl<E: Endian> SymbolEntry<E> {
    /// 0 local - not visable outside of object file
    /// 1 global - visable to all object file being combined
    /// 2 weak - resemble global but have less precedence
    pub const fn bind(i: u8) -> u8 {
        i >> 4
    }
    /// 0 no type
    /// 1 object (var, array)
    /// 2 function
    /// 3 section has an associated symbol. For relocation. Has local binding.
    /// 4 file
    /// 5 common
    pub const fn typ(i: u8) -> u8 {
        i & 0xF
    }
    /// refer to `Self::bind()` and `Self::typ()`
    pub const fn info(b: u8, t: u8) -> u8 {
        (b << 4) + (t & 0xF)
    }
    pub fn index_zero() -> Self {
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
