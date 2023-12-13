mod elf;

use std::{collections::HashMap, ffi::CString, rc::Rc};

use elf::*;

mod program;
use program::*;

mod tokenizer;

mod parser;
use parser::*;

fn generate_code<E: Endian>(
    program: Program<E>,
    // string_info: &HashMap<Rc<String>, (u32, u32)>,
    instruction: &Instruction,
) -> Program<E> {
    match instruction {
        Instruction::Assign {
            id: _,
            value: _,
            typ: _,
        } => program,
        Instruction::Exit(expr) => match &**expr {
            Expression::Number(num) => program.exit(*num),
            Expression::String(_) => panic!("can't exit with a string"),
            Expression::Ident(_) => todo!(),
        },
        Instruction::Print(expr) => match &**expr {
            Expression::Number(_) => panic!("can't print a number"),
            Expression::String(_) => todo!(),
            Expression::Ident(id) => {
                // let (addr, size) = string_info[id];
                // program.write(addr, size - 1)
                unimplemented!();
            }
        },
        Instruction::Binary { lhs, rhs } => {
            // let p = generate_code(program, string_info, lhs);
            // generate_code(p, string_info, rhs)
            let p = generate_code(program, lhs);
            generate_code(p, rhs)
        }
        Instruction::Loop(block) => {
            let start = program.code.len() as i32;
            let p = if let Some(block) = block {
                // generate_code(program, string_info, block)
                generate_code(program, block)
            } else {
                program
            };
            let end = (p.code.len() + 2) as i32;
            let rel = (start - end) as i8;
            p.jmp_rel(rel)
        }
    }
}
fn find_global_variables(
    global_strings: &mut HashMap<Rc<String>, Rc<String>>,
    instruction: &Instruction,
) {
    match instruction {
        Instruction::Assign { id, value, typ: _ } => match &**value {
            Expression::String(string) => {
                global_strings.insert(id.clone(), string.clone());
            }
            _ => {}
        },
        Instruction::Binary { lhs, rhs } => {
            find_global_variables(global_strings, lhs);
            find_global_variables(global_strings, rhs);
        }
        Instruction::Exit(_) => {}
        Instruction::Print(_) => {}
        Instruction::Loop(block) => {
            if let Some(block) = block {
                find_global_variables(global_strings, block);
            }
        }
    }
}

// fn program<E: Endian>(e: E, entry: u32, instruction: &Instruction) -> (Program<E>, Program<E>) {
// find all global strings
// let mut global_strings = HashMap::new();
// find_global_variables(&mut global_strings, instruction);

// add global strings to .data
// let mut string_info = HashMap::new();
// let mut data = Program::new(e);
// for (id, string) in global_strings {
//     let cstring = CString::new(string.as_str()).unwrap();
//     let addr = data.code.len() as u32 + entry;

//     string_info.insert(id, (addr, cstring.as_bytes_with_nul().len() as u32));
//     data = data.string(&cstring);
// }

// let text = generate_code(Program::new(e), &string_info, instruction);

// (text, data)
// }

fn run<E: Endian>() {
    let code = std::fs::read_to_string("main.kx").unwrap();

    let instruction = match parse(&code) {
        Ok(instructions) => instructions,
        Err(e) => {
            panic!("{e}");
        }
    };

    let ehsize = std::mem::size_of::<ELFHeader32<E>>() as u32;
    // let phsize = std::mem::size_of::<ProgramHeader32<E>>() as u32;
    let shsize = std::mem::size_of::<SectionHeader32<E>>() as u32;
    let symentrysize = std::mem::size_of::<SymbolEntry<E>>() as u32;

    let num_sh = 5;
    let offset = ehsize + shsize * num_sh;
    // let align = 0x1000;
    // let offset = (ehsize + phsize) % align;
    // let entry = 0x08048000;

    // let (text, data) = program(e, entry, &instruction);

    let null_sh = SectionHeader32::<E>::null();

    let program = Program::<E>::new();
    let text_program = generate_code(program, &instruction);

    let mut shstrings = StringTable::new();
    let text_i = shstrings.add(&CString::new(".text").unwrap()) as u32;
    let strtab_i = shstrings.add(&CString::new(".strtab").unwrap()) as u32;
    let shsymtab_i = shstrings.add(&CString::new(".symtab").unwrap()) as u32;
    let shstrtab_i = shstrings.add(&CString::new(".shstrtab").unwrap()) as u32;

    let text_size = text_program.code.len() as u32;
    let text = SectionHeader32::<E> {
        name: U32::new(text_i),
        typ: U32::new(1),
        flags: U32::new(6),
        addr: U32::zero(),
        offset: U32::new(offset),
        size: U32::new(text_size),
        link: U32::zero(),
        info: U32::zero(),
        addralign: U32::new(0),
        entsize: U32::zero(),
    };

    let mut strings = StringTable::new();
    let symfile_i = strings.add(&CString::new("main.kx").unwrap());
    let symtext_i = strings.add(&CString::new(".text").unwrap());
    let sym_start_i = strings.add(&CString::new("_start").unwrap());
    let strtab = SectionHeader32::<E> {
        name: U32::new(strtab_i),
        typ: U32::new(3),
        flags: U32::zero(),
        addr: U32::zero(),
        offset: U32::new(offset + text_size),
        size: U32::new(strings.size()),
        link: U32::zero(),
        info: U32::zero(),
        addralign: U32::new(0),
        entsize: U32::zero(),
    };

    let sym_entry = SymbolEntry::<E>::index_zero();
    let sym_file = SymbolEntry::<E> {
        name: U32::new(symfile_i),
        value: U32::zero(),
        size: U32::zero(),
        info: SymbolEntry::<E>::info(SEBind::Local, SEType::File),
        other: 0,
        shndx: U16::new(0xfff1),
    };
    let sym_text = SymbolEntry::<E> {
        name: U32::new(symtext_i),
        value: U32::zero(),
        size: U32::zero(),
        info: SymbolEntry::<E>::info(SEBind::Local, SEType::Section),
        other: 0,
        shndx: U16::new(1),
    };
    let sym_start = SymbolEntry::<E> {
        name: U32::new(sym_start_i),
        value: U32::zero(),
        size: U32::zero(),
        info: SymbolEntry::<E>::info(SEBind::Global, SEType::NoType),
        other: 0,
        shndx: U16::new(1),
    };
    let sym_size = symentrysize * 4;
    let symtab = SectionHeader32::<E> {
        name: U32::new(shsymtab_i),
        typ: U32::new(0x02),
        flags: U32::zero(),
        addr: U32::zero(),
        offset: U32::new(offset + text_size + strings.size()),
        size: U32::new(sym_size),
        link: U32::new(2),
        info: U32::new(3),
        addralign: U32::new(0),
        entsize: U32::new(symentrysize),
    };
    let shstrtab = SectionHeader32::<E> {
        name: U32::new(shstrtab_i),
        typ: U32::new(3),
        flags: U32::zero(),
        addr: U32::zero(),
        offset: U32::new(offset + text_size + strings.size() + sym_size),
        size: U32::new(shstrings.size()),
        link: U32::zero(),
        info: U32::zero(),
        addralign: U32::new(0x0),
        entsize: U32::zero(),
    };

    let elf = ELFHeader32::<E> {
        ident: EFIIdent::new_32bit(E::endianess()),
        typ: U16::new(1),     // REL
        machine: U16::new(3), // i686 x86
        version: U32::new(1), // always 1
        entry: U32::zero(),   // none
        phoff: U32::zero(),   // none
        shoff: U32::new(ehsize),
        flags: U32::zero(),                 // none
        ehsize: U16::new(ehsize as u16),    // own size
        phentsize: U16::zero(),             // not used
        phnum: U16::zero(),                 // none
        shentsize: U16::new(shsize as u16), // sh size
        shnum: U16::new(num_sh as u16),
        shstrndx: U16::new(4),
    };

    write(
        &elf.bytes()
            .iter()
            // --
            .chain(null_sh.bytes().iter())
            .chain(text.bytes().iter())
            .chain(strtab.bytes().iter())
            .chain(symtab.bytes().iter())
            .chain(shstrtab.bytes().iter())
            // --
            .chain(text_program.code.iter())
            .chain(strings.bytes().iter())
            .chain(sym_entry.bytes().iter())
            .chain(sym_file.bytes().iter())
            .chain(sym_text.bytes().iter())
            .chain(sym_start.bytes().iter())
            .chain(shstrings.bytes().iter())
            // --
            .copied()
            .collect::<Vec<u8>>(),
    )
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
