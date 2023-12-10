mod elf;

use std::{collections::HashMap, ffi::CString, rc::Rc};

use elf::*;

mod program;
use program::*;

mod parser;
use parser::*;

#[derive(Debug, Clone)]
enum Instruction {
    Assign { id: Rc<String>, value: Rc<String> },
    Exit { num: u32 },
    Print { id: Rc<String> },
    Binary { lhs: Rc<Self>, rhs: Rc<Self> },
}

#[derive(Debug, Clone)]
enum AST {
    Instruction(Rc<Instruction>),
}
impl From<Instruction> for AST {
    fn from(value: Instruction) -> Self {
        Self::Instruction(Rc::new(value))
    }
}
impl From<Rc<Instruction>> for AST {
    fn from(value: Rc<Instruction>) -> Self {
        Self::Instruction(value.clone())
    }
}
#[derive(Debug, Clone)]
enum Node {
    Token(Token),
    AST(AST),
}
impl From<Token> for Node {
    fn from(value: Token) -> Self {
        Self::Token(value)
    }
}
impl From<AST> for Node {
    fn from(value: AST) -> Self {
        Self::AST(value)
    }
}
#[derive(Debug, Clone)]
struct Nodes {
    nodes: Vec<Node>,
}
impl Nodes {
    fn new() -> Self {
        Self { nodes: Vec::new() }
    }
    fn reduce(&mut self, offset: usize) {
        self.nodes.truncate(self.nodes.len() - offset);
    }
    fn push(&mut self, node: impl Into<Node>) {
        let node = node.into();
        self.nodes.push(node);
    }
}

fn parse(code: &str) -> Rc<Instruction> {
    let tokenizer = Tokenizer::new(code);

    let mut nodes = Nodes::new();

    for token in tokenizer {
        nodes.push(token);

        loop {
            let repeat = match &nodes.nodes[..] {
                [.., Node::Token(Token::Exit), Node::Token(Token::Number(num)), Node::Token(Token::Semicolon)] =>
                {
                    let node: AST = Instruction::Exit { num: *num }.into();
                    nodes.reduce(3);
                    nodes.push(node);
                    true
                }
                [.., Node::Token(Token::Print), Node::Token(Token::Ident(id)), Node::Token(Token::Semicolon)] =>
                {
                    let node: AST = Instruction::Print { id: id.clone() }.into();
                    nodes.reduce(3);
                    nodes.push(node);
                    true
                }
                [.., Node::Token(Token::Var), Node::Token(Token::Ident(id)), Node::Token(Token::Equal), Node::Token(Token::String(string)), Node::Token(Token::Semicolon)] =>
                {
                    let node: AST = Instruction::Assign {
                        id: id.clone(),
                        value: string.clone(),
                    }
                    .into();
                    nodes.reduce(5);
                    nodes.push(node);
                    true
                }
                [.., Node::AST(AST::Instruction(lhs)), Node::AST(AST::Instruction(rhs))] => {
                    let node: AST = Instruction::Binary {
                        lhs: lhs.clone(),
                        rhs: rhs.clone(),
                    }
                    .into();
                    nodes.reduce(2);
                    nodes.push(node);
                    true
                }
                _ => false,
            };
            if !repeat {
                break;
            }
        }
    }

    if nodes.nodes.len() != 1 {
        panic!("node stream is not 1:\n{nodes:#?}");
    }
    if let Node::AST(AST::Instruction(i)) = nodes.nodes.pop().unwrap() {
        i
    } else {
        panic!("last node is not AST:\n{nodes:#?}");
    }
}

fn generate_code<E: Endian>(
    program: Program<E>,
    string_info: &HashMap<Rc<String>, (u32, u32)>,
    instruction: &Instruction,
) -> Program<E> {
    match instruction {
        Instruction::Assign { id: _, value: _ } => program,
        Instruction::Exit { num } => program.exit(*num),
        Instruction::Print { id } => {
            let (addr, size) = string_info[id];
            program.write(addr, size)
        }
        Instruction::Binary { lhs, rhs } => {
            let p = generate_code(program, string_info, lhs);
            generate_code(p, string_info, rhs)
        }
    }
}
fn find_global_variables(
    global_strings: &mut HashMap<Rc<String>, Rc<String>>,
    instruction: &Instruction,
) {
    match instruction {
        Instruction::Assign { id, value } => {
            global_strings.insert(id.clone(), value.clone());
        }
        Instruction::Binary { lhs, rhs } => {
            find_global_variables(global_strings, lhs);
            find_global_variables(global_strings, rhs);
        }
        _ => {}
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
    let instruction = parse(&code);

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
