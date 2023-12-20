mod elf;

use std::{collections::HashMap, rc::Rc};

use elf::*;

mod program;
use program::*;

mod tokenizer;

mod parser;
use parser::*;
use tokenizer::*;

mod code_generator;
use code_generator::*;

fn run<E: Endian>() {
    let _ = std::fs::remove_file("output/main.o");
    let _ = std::fs::remove_file("output/main");
    let code = std::fs::read_to_string("main.kx").unwrap();

    let instruction = match parse(&code) {
        Ok(instructions) => instructions,
        Err(e) => {
            panic!("{e}");
        }
    };
    // println!("instructions: {instruction:#?}");
    match std::fs::write(
        "output/log.txt",
        format!("instructions: {instruction:#?}").as_bytes(),
    ) {
        Ok(_) => println!("successfully writen log file"),
        Err(_) => println!("failed to write log file"),
    }

    let mut vars_declared = VarTypes::new();
    analyse_instruction(&instruction, &mut vars_declared);

    let mut text = Program::<E>::new();

    // setup stack
    let mut vars_addrs = VarAddresses::new();
    let mut alloc = 0;
    for (id, typ) in vars_declared {
        let var = match typ {
            Type::String => Var::Global,
            Type::U32 => {
                alloc -= 4;
                Var::Local(alloc)
            }
            Type::Bool => Var::Local(1),
        };
        vars_addrs.insert(id, var);
    }
    text.mov_edp_esp();
    text.sub_esp_imm8(alloc.unsigned_abs().try_into().unwrap());

    generate_instruction(&mut text, &instruction, &vars_addrs);
    text.mov_esp_edp();

    let ehsize = std::mem::size_of::<ELFHeader32<E>>() as u32;
    let elf_header = ELFHeader32::<E> {
        ident: EFIIdent::new_32bit::<E>(),
        typ: EHType::Rel.into(),
        machine: U16::new(3),    // i686 x86
        shoff: U32::new(ehsize), // section headers start immediatly after
        ..Default::default()
    };
    let mut builder = ELFBuilder::new(elf_header);
    builder.sh.add(
        ".text",
        SectionHeader32 {
            typ: SHType::ProgBits.into(),
            flags: (SHFlag::ExecInstr | SHFlag::Alloc).into(),
            ..Default::default()
        },
        text.code,
    );

    let mut sym_table = SymbolTable::new();
    sym_table.add(
        "main.kx",
        SymbolEntry {
            info: SymbolEntry::<E>::info(SEBind::Local, SEType::File),
            shndx: U16::new(0xfff1),
            ..Default::default()
        },
    );
    sym_table.add(
        ".text",
        SymbolEntry::<E> {
            info: SymbolEntry::<E>::info(SEBind::Local, SEType::Section),
            shndx: U16::new(1),
            ..Default::default()
        },
    );
    sym_table.set_info();
    sym_table.add(
        "_start",
        SymbolEntry::<E> {
            info: SymbolEntry::<E>::info(SEBind::Global, SEType::NoType),
            shndx: U16::new(1),
            ..Default::default()
        },
    );
    let i = builder.sh.sections.len() as u32;
    builder
        .sh
        .add(".strtab", sym_table.strtab.0, sym_table.strtab.1.bytes());
    sym_table.symbolsh.link = i.into();
    builder.sh.add(
        ".symtab",
        sym_table.symbolsh,
        sym_table
            .entries
            .iter()
            .flat_map(|e| e.bytes())
            .copied()
            .collect(),
    );

    builder.update();
    write(&builder.bytes())
}

fn write(bytes: &[u8]) {
    use std::io::Write;
    use std::os::unix::fs::PermissionsExt;

    let mut file = std::fs::File::create("output/main.o").unwrap();
    let mut perms = file.metadata().unwrap().permissions();
    perms.set_mode(0o775);
    file.set_permissions(perms).unwrap();

    file.write_all(bytes).unwrap();
}

fn main() {
    run::<LittleEndian>();
}
