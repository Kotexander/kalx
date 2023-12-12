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
    string_info: &HashMap<Rc<String>, (u32, u32)>,
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
                let (addr, size) = string_info[id];
                program.write(addr, size - 1)
            }
        },
        Instruction::Binary { lhs, rhs } => {
            let p = generate_code(program, string_info, lhs);
            generate_code(p, string_info, rhs)
        }
        Instruction::Loop(block) => {
            let start = program.code.len() as i32;
            let p = if let Some(block) = block {
                generate_code(program, string_info, block)
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

fn program<E: Endian>(e: E, entry: u32, instruction: &Instruction) -> (Program<E>, Program<E>) {
    // find all global strings
    let mut global_strings = HashMap::new();
    find_global_variables(&mut global_strings, instruction);

    // add global strings to .data
    let mut string_info = HashMap::new();
    let mut data = Program::new(e);
    for (id, string) in global_strings {
        let cstring = CString::new(string.as_str()).unwrap();
        let addr = data.code.len() as u32 + entry;

        string_info.insert(id, (addr, cstring.as_bytes_with_nul().len() as u32));
        data = data.string(&cstring);
    }

    let text = generate_code(Program::new(e), &string_info, instruction);

    (text, data)
}

fn run<E: Endian>(e: E) {
    let code = std::fs::read_to_string("main.kx").unwrap();

    let instruction = match parse(&code) {
        Ok(instructions) => instructions,
        Err(e) => {
            panic!("{e}");
        }
    };
    // let instruction = parse(&code).unwrap();

    let ehsize = std::mem::size_of::<ELFHeader32<E>>() as u32;
    let phsize = std::mem::size_of::<ProgramHeader32<E>>() as u32;
    let shsize = std::mem::size_of::<SectionHeader32<E>>() as u32;
    let align = 0x1000;
    let offset = (ehsize + phsize) % align;
    let entry = 0x08048000 + offset;

    let (text, data) = program(e, entry, &instruction);

    // let data_size = data.code.len() as u32;
    // let data_ph = ProgramHeader32 {
    //     typ: U32::new(e, 1),
    //     offset: U32::new(e, offset),
    //     vaddr: U32::new(e, entry),
    //     paddr: U32::new(e, 0),
    //     filesz: U32::new(e, data_size),
    //     memsz: U32::new(e, data_size),
    //     flags: U32::new(e, 4),
    //     align: U32::new(e, align),
    // };
    let data_size = data.code.len() as u32;
    let text_size = text.code.len() as u32;
    let size = data_size + text_size;
    let text_ph = ProgramHeader32 {
        typ: U32::new(e, 1),
        offset: U32::new(e, offset), // right after ELF header
        vaddr: U32::new(e, entry),
        paddr: U32::new(e, 0), // not used
        filesz: U32::new(e, size),
        memsz: U32::new(e, size),
        flags: U32::new(e, 1 | 4), // EXEC and READ
        align: U32::new(e, align),
    };

    let elf = ELFHeader32 {
        ident: EFIIdent::new_32bit(e.endianess()),
        typ: U16::new(e, 2),                   // EXEC
        machine: U16::new(e, 3),               // i686 x86
        version: U32::new(e, 1),               // always 1
        entry: U32::new(e, entry + data_size), // something safe
        phoff: U32::new(e, ehsize),            // right after header
        shoff: U32::new(e, 0),                 // none
        flags: U32::new(e, 0),                 // none
        ehsize: U16::new(e, ehsize as u16),    // own size
        phentsize: U16::new(e, phsize as u16), // ph size
        phnum: U16::new(e, 1),                 // 1
        shentsize: U16::new(e, shsize as u16), // sh size
        shnum: U16::new(e, 0),                 // none
        shstrndx: U16::new(e, 0),              // none
    };

    write(
        &elf.bytes()
            .iter()
            .chain(text_ph.bytes().iter())
            .chain(data.code.iter())
            .chain(text.code.iter())
            .copied()
            .collect::<Vec<u8>>(),
    )
}

fn write(bytes: &[u8]) {
    use std::io::Write;
    use std::os::unix::fs::PermissionsExt;

    let mut file = std::fs::File::create("output/out").unwrap();
    let mut perms = file.metadata().unwrap().permissions();
    perms.set_mode(0o775);
    file.set_permissions(perms).unwrap();

    file.write_all(bytes).unwrap();
}

fn main() {
    let e = LittleEndian;

    run(e);
}
