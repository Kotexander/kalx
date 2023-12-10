mod elf;

use std::{collections::HashMap, ffi::CString};

use elf::*;

mod program;
use program::*;

mod parser;
use parser::*;

#[derive(Debug)]
enum AST<'a> {
    Assign { id: &'a str, value: String },
    Exit { num: u32 },
    Print { id: &'a str },
}

fn parse<'a>(code: &'a str) -> Vec<AST<'a>> {
    let mut tokenizer = Tokenizer::new(&code);
    let mut tokens = Vec::new();

    let mut ast = Vec::new();

    // let mut table = HashMap::new();
    while let Some(token) = tokenizer.next() {
        tokens.push(token);

        loop {
            let num = match &tokens[..] {
                [.., Token::Exit, Token::Number(num), Token::Semicolon] => {
                    // program = program.exit(U32::new(e, num as u32).bytes());
                    ast.push(AST::Exit { num: *num });
                    3
                }
                [.., Token::Print, Token::Ident(id), Token::Semicolon] => {
                    ast.push(AST::Print { id });
                    3
                }
                [.., Token::Var, Token::Ident(id), Token::Equal, Token::String(string), Token::Semicolon] =>
                {
                    // program = program.string(&CString::new(string).unwrap());
                    ast.push(AST::Assign {
                        id,
                        value: string.clone(),
                    });
                    5
                }
                _ => 0,
            };
            if num == 0 {
                break;
            } else {
                for _ in 0..num {
                    tokens.pop();
                }
            }
        }
    }
    if !tokens.is_empty() {
        panic!("token stream is not empty: {tokens:?} is left");
    }
    ast
}

fn program<'a, E: Endian>(e: E, entry: u32, ast: &[AST<'a>]) -> (Program<E>, Program<E>) {
    // find all global strings
    let mut global_strings = HashMap::new();
    for node in ast.iter() {
        if let AST::Assign { id, value } = node {
            global_strings.insert(*id, value);
        }
    }
    // add global strings to .data
    let mut string_info = HashMap::new();
    let mut data = Program::new(e);
    for (id, string) in global_strings.iter() {
        let cstring = CString::new((**string).clone()).unwrap();
        let addr = data.code.len() as u32 + entry;

        string_info.insert(id, (addr as u32, cstring.as_bytes_with_nul().len() as u32));
        data = data.string(&cstring);
    }

    let mut text = Program::new(e);
    for node in ast.iter() {
        match node {
            AST::Assign { id: _, value: _ } => {}
            AST::Exit { num } => {
                text = text.exit(*num);
            }
            AST::Print { id } => {
                let (addr, size) = string_info[id];
                text = text.write(addr, size);
            }
        }
    }
    (text, data)
}

fn run<E: Endian>(e: E) {
    let code = std::fs::read_to_string("main.kx").unwrap();
    let ast = parse(&code);

    let ehsize = std::mem::size_of::<ELFHeader32<E>>() as u32;
    let phsize = std::mem::size_of::<ProgramHeader32<E>>() as u32;
    let shsize = std::mem::size_of::<SectionHeader32<E>>() as u32;
    let align = 0x1000;
    let offset = (ehsize + phsize) % align;
    let entry = 0x08048000 + offset;

    let (text, data) = program(e, entry, &ast);

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
            .into_iter()
            .copied()
            .chain(text_ph.bytes().into_iter().copied())
            .chain(data.code.into_iter())
            .chain(text.code.into_iter())
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
