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

fn run<E: Endian>() -> Result<(), String> {
    let _ = std::fs::remove_file("output/main.o");
    let _ = std::fs::remove_file("output/main");
    let code = std::fs::read_to_string("main.kx").unwrap();

    let instruction = parse(&code)?;

    let mut vars = Vars::new();
    let mut strings = Strings::new();
    analyse_instruction(&instruction, &mut vars, &mut strings)?;

    let mut text = Program::<E>::new();

    // setup stack
    text.mov_edp_esp();
    text.sub_esp_imm8(vars.allocated().unsigned_abs().try_into().unwrap());

    generate_instruction(&mut text, &instruction, &vars, &mut strings);
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
        ".rodata",
        SectionHeader32 {
            typ: SHType::ProgBits.into(),
            flags: (SHFlag::Alloc).into(),
            ..Default::default()
        },
        strings
            .keys()
            .iter()
            .flat_map(|s| s.as_bytes_with_nul())
            .copied()
            .collect(),
    );
    let text_i = builder.sh.add(
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
    let symrodata_i = sym_table.add(
        ".rodata",
        SymbolEntry {
            info: SymbolEntry::<E>::info(SEBind::Local, SEType::Section),
            shndx: U16::new(1),
            ..Default::default()
        },
    );
    for (rel_str, addr) in strings.values_mut() {
        sym_table.add(
            &rel_str.sym,
            SymbolEntry {
                info: SymbolEntry::<E>::info(SEBind::Local, SEType::NoType),
                shndx: U16::new(1),
                value: (*addr).into(),
                ..Default::default()
            },
        );
    }
    sym_table.add(
        ".text",
        SymbolEntry {
            info: SymbolEntry::<E>::info(SEBind::Local, SEType::Section),
            shndx: U16::new(2),
            ..Default::default()
        },
    );
    sym_table.set_info();
    sym_table.add(
        "_start",
        SymbolEntry::<E> {
            info: SymbolEntry::<E>::info(SEBind::Global, SEType::NoType),
            shndx: U16::new(2),
            ..Default::default()
        },
    );
    let strtab_i = builder
        .sh
        .add(".strtab", sym_table.strtab.0, sym_table.strtab.1.bytes());
    sym_table.symbolsh.link = strtab_i.into();
    let symtab_i = builder.sh.add(
        ".symtab",
        sym_table.symbolsh,
        sym_table
            .entries
            .iter()
            .flat_map(|e| e.bytes())
            .copied()
            .collect(),
    );

    let rel_entries: Vec<RelEntry<E>> = strings
        .values()
        .iter()
        .flat_map(|(rel_str, _addr)| {
            rel_str.rels.iter().map(|offset| RelEntry::<E> {
                offset: (*offset).into(),
                info: RelEntry::<E>::info(symrodata_i as u8, 1).into(),
            })
        })
        .collect();

    builder.sh.add(
        ".rel.text",
        SectionHeader32 {
            typ: SHType::Rel.into(),
            link: symtab_i.into(),
            info: text_i.into(),
            entsize: (std::mem::size_of::<RelEntry<E>>() as u32).into(),
            ..Default::default()
        },
        rel_entries
            .iter()
            .flat_map(|re| re.bytes())
            .copied()
            .collect(),
    );

    builder.update();
    write(&builder.bytes());
    Ok(())
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
    match run::<LittleEndian>() {
        Ok(_) => println!("Build successfull!\n\n"),
        Err(e) => println!("Build failed: {e}\n\n"),
    }
}
