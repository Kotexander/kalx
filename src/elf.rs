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
    /// * 1 for loadable segment
    /// * 2 for dynamic linking information
    /// * 3 for interpreter information
    /// * 4 for auxiliary information
    /// * 6 for program header table
    /// * 7 for thread-local storage template
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
    /// * 1 for executable
    /// * 2 for writeable
    /// * 4 for readable
    pub flags: U32<E>,
    /// set 0 or 1 for no alignment. Otherwise, must be a power of 2.
    pub align: U32<E>,
}
unsafe impl<E: Endian> Pod for ProgramHeader32<E> {}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct SectionHeader32<E: Endian> {
    pub name: U32<E>,
    pub typ: U32<E>,
    pub flags: U32<E>,
    pub addr: U32<E>,
    pub offset: U32<E>,
    pub size: U32<E>,
    pub link: U32<E>,
    pub info: U32<E>,
    pub addralign: U32<E>,
    pub entsize: U32<E>,
}
