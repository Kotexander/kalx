mod elf;

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
            Expression::Ident(_id) => {
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
// fn find_global_variables(
//     global_strings: &mut HashMap<Rc<String>, Rc<String>>,
//     instruction: &Instruction,
// ) {
//     match instruction {
//         Instruction::Assign { id, value, typ: _ } => match &**value {
//             Expression::String(string) => {
//                 global_strings.insert(id.clone(), string.clone());
//             }
//             _ => {}
//         },
//         Instruction::Binary { lhs, rhs } => {
//             find_global_variables(global_strings, lhs);
//             find_global_variables(global_strings, rhs);
//         }
//         Instruction::Exit(_) => {}
//         Instruction::Print(_) => {}
//         Instruction::Loop(block) => {
//             if let Some(block) = block {
//                 find_global_variables(global_strings, block);
//             }
//         }
//     }
// }

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

    let text = generate_code(Program::<E>::new(), &instruction);

    let elf_header = ELFHeader32::<E> {
        ident: EFIIdent::new_32bit::<E>(),
        typ: EHType::Rel.into(),
        machine: U16::new(3),    // i686 x86
        shoff: U32::new(ehsize), // section headers start immediatly after
        ..Default::default()
    };
    let mut builder = ELFBuilder::new(elf_header);

    builder.sections.add(
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

    builder
        .sections
        .add_sym_table(".strtab", ".symtab", sym_table);

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
