mod elf;
use std::{ffi::CString, io::Write};

use elf::*;

mod program;
use program::*;

fn program<E: Endian>(e: E) -> Program {
    let mut program = Program { code: vec![] };
    let string = CString::new("HELLO WORLD FROM KALX\n").unwrap();
    program
        .code
        .extend_from_slice(&[0xB8, 0x04, 0x00, 0x00, 0x00]); // eax <- 4 (write)
    program
        .code
        .extend_from_slice(&[0xBB, 0x01, 0x00, 0x00, 0x00]); // ebx <- 1 (stdout)
    program
        .code
        .extend_from_slice(&[0xB9, 0x76, 0x80, 0x04, 0x08]); // ecx <- buf

    let count = e.u32_bytes(string.as_bytes_with_nul().len() as u32);
    program
        .code
        .extend_from_slice(&[0xBA, count[0], count[1], count[2], count[3]]); // edx <- count

    program.syscall().exit(e.u32_bytes(69)).string(&string)
}

fn run<E: Endian>(e: E) {
    let ehsize = std::mem::size_of::<ELFHeader32<E>>();
    let phsize = std::mem::size_of::<ProgramHeader32<E>>();
    let offset = ehsize + phsize;
    let entry = 0x08048000 + offset;
    // let entry = 0x08000000 + offset;

    let elf = ELFHeader32 {
        ident: EFIIdent::new_32bit(e.endianess()),
        typ: U16::new(e, 2),                   // EXEC
        machine: U16::new(e, 3),               // i686 x86
        version: U32::new(e, 1),               // always 1
        entry: U32::new(e, entry as u32),      // something safe
        phoff: U32::new(e, ehsize as u32),     // right after header
        shoff: U32::new(e, 0),                 // none
        flags: U32::new(e, 0),                 // none
        ehsize: U16::new(e, ehsize as u16),    // own size
        phentsize: U16::new(e, phsize as u16), // ph size
        phnum: U16::new(e, 1),                 // 1
        shentsize: U16::new(e, std::mem::size_of::<SectionHeader32<E>>() as u16), // sh size
        shnum: U16::new(e, 0),                 // none
        shstrndx: U16::new(e, 0),              //none
    };
    let program: &[u8] = &program(e).code;

    let size = program.len() as u32;
    let ph = ProgramHeader32 {
        typ: U32::new(e, 1),
        offset: U32::new(e, offset as u32), // right after ELF header
        vaddr: U32::new(e, entry as u32),
        paddr: U32::new(e, 0), // not used
        filesz: U32::new(e, size),
        memsz: U32::new(e, size),
        flags: U32::new(e, 1 | 4), // EXEC and READ
        align: U32::new(e, 0x1000),
    };
    use std::os::unix::fs::PermissionsExt;
    let mut file = std::fs::File::create("out").unwrap();
    let mut perms = file.metadata().unwrap().permissions();
    perms.set_mode(0o775);
    file.set_permissions(perms).unwrap();

    file.write_all(
        &elf.bytes()
            .into_iter()
            .copied()
            .chain(ph.bytes().into_iter().copied())
            .chain(program.into_iter().copied())
            .collect::<Vec<u8>>(),
    )
    .unwrap();
}

fn main() {
    // dbg!(0x54.ex);

    let e = LittleEndian;
    run(e);
}
