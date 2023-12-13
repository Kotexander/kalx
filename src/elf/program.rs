use super::*;

#[repr(u32)]
pub enum PHType {
    Null = 0x0,
    Load = 0x1,
    Dynamic = 0x2,
    Interp = 0x3,
    Note = 0x4,
    /// Reserved
    SHLib = 0x5,
    /// Segment containing program header table itself.
    PHDR = 0x6,
    /// Thread-Local Storage template.
    TLS = 0x7,
    LowOS = 0x60000000,
    HighOS = 0x6FFFFFFF,
    LowProc = 0x70000000,
    HighProc = 0x7FFFFFFF,
}
impl<E: Endian> Into<U32<E>> for PHType {
    fn into(self) -> U32<E> {
        U32::new(self as u32)
    }
}
#[repr(u32)]
pub enum PHFlag {
    /// Executable
    X = 0b001,
    /// Writable
    W = 0b010,
    /// Readable
    R = 0b100,
}
impl<E: Endian> Into<U32<E>> for PHFlag {
    fn into(self) -> U32<E> {
        U32::new(self as u32)
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct ProgramHeader32<E: Endian> {
    /// Segment type
    ///
    /// Equals to [`PHType`]
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
    /// Equals to [`PHFlag`]
    pub flags: U32<E>,
    /// set 0 or 1 for no alignment. Otherwise, must be a power of 2.
    pub align: U32<E>,
}
impl<E: Endian> Default for ProgramHeader32<E> {
    fn default() -> Self {
        Self {
            typ: PHType::Null.into(),
            offset: U32::zero(),
            vaddr: U32::zero(),
            paddr: U32::zero(),
            filesz: U32::zero(),
            memsz: U32::zero(),
            flags: U32::zero(),
            align: U32::zero(), // should this be set to 0x1000?
        }
    }
}
unsafe impl<E: Endian> Pod for ProgramHeader32<E> {}
