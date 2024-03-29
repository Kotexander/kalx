mod elf;

use std::process::exit;

use elf::*;

mod x86_program;
// use x86_program::*;
mod tokenizer;
// use tokenizer::*;
mod optimizer;
mod parser;
// mod analyser_old;
mod analyser;
mod ir;
// mod generator_old;
mod x86_generator;

struct ProcessedStrSymbol {
    name: String,
    index: u32,
    rels: Vec<u32>,
}
struct ProcessedStrs {
    data: Vec<u8>,
    symbols: Vec<ProcessedStrSymbol>,
}
fn process_strs(strs: x86_generator::RelStrings) -> ProcessedStrs {
    let data = strs
        .0
        .iter()
        .flat_map(|(s, _)| s.as_bytes_with_nul())
        .copied()
        .collect();

    let mut index = 0;
    let symbols: Vec<_> = strs
        .0
        .into_iter()
        .enumerate()
        .map(|(i, s)| {
            let name = format!("str{i}");
            let idx = index;
            let rels = s.1;
            index += s.0.as_bytes_with_nul().len() as u32;
            ProcessedStrSymbol {
                name,
                index: idx,
                rels,
            }
        })
        .collect();

    ProcessedStrs { data, symbols }
}

fn run<E: Endian>() -> Result<(), String> {
    let _ = std::fs::remove_file("output/main.o");
    let _ = std::fs::remove_file("output/main");
    let code = std::fs::read_to_string("main.kx").unwrap();

    let mut functions = parser::parse(&code)?;
    optimizer::optimize(&mut functions);
    analyser::analyse(&mut functions)?;

    let _ = std::fs::create_dir("output/dump");
    let mut reformated_file = String::new();
    for function in functions.iter() {
        reformated_file += &format!("{function}\n");
    }
    let _ = std::fs::write("output/dump/main.kx.reformatted", reformated_file);
    
    let ir_functions = ir::generate(functions);
    let mut ir_file = String::new();
    for function in ir_functions.iter() {
        ir_file += &format!("{function}");
    }
    let _ = std::fs::write("output/dump/main.kx.ir", ir_file);
    let (text, rel_info, funs) = x86_generator::generate::<E>(ir_functions);
    let processed_strs = process_strs(rel_info.strs);

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
        processed_strs.data,
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
    sym_table.add(
        ".rodata",
        SymbolEntry {
            info: SymbolEntry::<E>::info(SEBind::Local, SEType::Section),
            shndx: U16::new(1),
            ..Default::default()
        },
    );

    let str_rels: Vec<_> = processed_strs
        .symbols
        .into_iter()
        .map(|sym| {
            let i = sym_table.add(
                &sym.name,
                SymbolEntry {
                    info: SymbolEntry::<E>::info(SEBind::Local, SEType::NoType),
                    shndx: U16::new(1),
                    value: sym.index.into(),
                    ..Default::default()
                },
            );

            (i, sym.rels)
        })
        .collect();

    sym_table.add(
        ".text",
        SymbolEntry {
            info: SymbolEntry::<E>::info(SEBind::Local, SEType::Section),
            shndx: U16::new(2),
            ..Default::default()
        },
    );

    // start of global symbols
    sym_table.set_info();
    for fun in funs {
        sym_table.add(
            fun.name.0.as_str(),
            SymbolEntry::<E> {
                info: SymbolEntry::<E>::info(SEBind::Global, SEType::NoType),
                shndx: U16::new(2),
                value: fun.entry.into(),
                size: fun.size.into(),
                ..Default::default()
            },
        );
    }

    let fun_rel: Vec<_> = rel_info
        .funs
        .0
        .into_iter()
        .map(|(name, rels)| {
            let i = sym_table.add(
                &name.0,
                SymbolEntry::<E> {
                    info: SymbolEntry::<E>::info(SEBind::Global, SEType::NoType),
                    shndx: U16::new(0),
                    ..Default::default()
                },
            );
            (i, rels)
        })
        .collect();

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

    let rel_entries: Vec<RelEntry<E>> = str_rels
        .into_iter()
        .flat_map(|(index, rels)| {
            rels.into_iter().map(move |offset| RelEntry::<E> {
                offset: offset.into(),
                info: RelEntry::<E>::info(index as u8, 1).into(),
            })
        })
        .chain(fun_rel.into_iter().flat_map(|(index, rels)| {
            rels.into_iter().map(move |offset| RelEntry::<E> {
                offset: offset.into(),
                info: RelEntry::<E>::info(index as u8, 2).into(),
            })
        }))
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

    let _ = std::fs::create_dir("output");
    let mut file = std::fs::File::create("output/main.o").unwrap();
    let mut perms = file.metadata().unwrap().permissions();
    perms.set_mode(0o775);
    file.set_permissions(perms).unwrap();

    file.write_all(bytes).unwrap();
}

fn main() {
    match run::<LittleEndian>() {
        Ok(_) => println!("Build successfull!\n\n"),
        Err(e) => {
            println!("Build failed: {e}\n\n");
            exit(1);
        }
    }
}
