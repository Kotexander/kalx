mod elf;

use std::{collections::HashMap, rc::Rc};

use elf::*;

mod program;
use program::*;

mod tokenizer;

mod parser;
use parser::*;
use tokenizer::Type;

#[derive(Debug, Clone, Copy)]
enum Var {
    Global(Type),
    Local(i8, Type),
}

type VarTypes = HashMap<Rc<String>, Type>;
type VarAddresses = HashMap<Rc<String>, Var>;

fn analyse_expr(expr: &Expression, var: &VarTypes) -> Type {
    match expr {
        Expression::Number(_) => Type::U32,
        Expression::String(_) => Type::String,
        Expression::Ident(id) => var[id],
    }
}
fn analyse_instruction(instruction: &Instruction, vars: &mut VarTypes) {
    match instruction {
        Instruction::Assign { id, value } => {
            if vars.contains_key(id) {
                panic!("duplicate declaration");
            } else {
                let typ = analyse_expr(value, vars);
                vars.insert(id.clone(), typ);
            }
        }
        Instruction::Exit(expr) => {
            analyse_expr(expr, vars);
        }
        Instruction::Print(expr) => {
            analyse_expr(expr, vars);
        }
        Instruction::Binary { lhs, rhs } => {
            analyse_instruction(lhs, vars);
            analyse_instruction(rhs, vars);
        }
        Instruction::Loop(block) => {
            if let Some(block) = block {
                analyse_instruction(block, vars);
            }
        }
    }
}

fn generate_code<E: Endian>(
    program: &mut Program<E>,
    instruction: &Instruction,
    vars: &VarAddresses,
) {
    match instruction {
        Instruction::Assign { id, value } => {
            match vars[id] {
                Var::Global(_typ) => match &**value {
                    Expression::Number(_) => todo!(),
                    Expression::String(_) => todo!(),
                    Expression::Ident(_) => todo!(),
                },
                Var::Local(addr, _typ) => match &**value {
                    Expression::Number(num) => {
                        program.mov_rm8_imm(RM32::EBP, addr, *num);
                    }
                    Expression::String(_) => todo!(),
                    Expression::Ident(id) => match vars[id] {
                        Var::Global(_) => todo!(),
                        Var::Local(addr2, _typ) => {
                            program.mov_r_rm8(Reg32::EAX, RM32::EBP, addr2);
                            program.mov_rm8_r(RM32::EBP, Reg32::EAX, addr);
                        }
                    },
                },
            };
        }
        Instruction::Exit(expr) => match &**expr {
            Expression::Number(num) => {
                program.mov_ebx_imm(*num);
                program.exit();
            }
            Expression::String(_) => panic!("can't exit with a string"),
            Expression::Ident(id) => match vars[id] {
                Var::Global(_) => todo!(),
                Var::Local(addr, _typ) => {
                    program.mov_r_rm8(Reg32::EBX, RM32::EBP, addr);
                    program.exit();
                }
            },
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
            generate_code(program, lhs, vars);
            generate_code(program, rhs, vars);
        }
        Instruction::Loop(block) => {
            program.loop_fn(|program| {
                if let Some(block) = block {
                    generate_code(program, block, vars);
                }
            });
        }
    }
}

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

    let mut vars_declared = VarTypes::new();
    analyse_instruction(&instruction, &mut vars_declared);

    let mut text = Program::<E>::new();

    // setup stack
    let mut vars_addrs = VarAddresses::new();
    let mut alloc = 0i8;
    for (id, typ) in vars_declared {
        let var = match typ {
            Type::String => Var::Global(typ),
            Type::I32 | Type::U32 => {
                alloc -= 4;
                Var::Local(alloc, typ)
            }
        };
        vars_addrs.insert(id, var);
    }
    text.mov_edp_esp();
    text.sub_esp_imm8(alloc.unsigned_abs());

    generate_code(&mut text, &instruction, &vars_addrs);
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
