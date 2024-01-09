use std::{fmt::Display, iter::Peekable, rc::Rc, str::CharIndices};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Id(pub Rc<String>);
impl From<Rc<String>> for Id {
    fn from(value: Rc<String>) -> Self {
        Self(value)
    }
}
impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    String,
    U32,
    Bool,
    Void,
}
impl Type {
    pub fn parse(typ: &str) -> Option<Self> {
        let typ = match typ {
            "u32" => Self::U32,
            "str" => Self::String,
            "bool" => Self::Bool,
            "void" => Self::Void,
            _ => {
                return None;
            }
        };
        Some(typ)
    }
    pub fn size(&self) -> u32 {
        match self {
            Type::String => 4,
            Type::U32 => 4,
            Type::Bool => 0,
            Type::Void => 1,
        }
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Type::String => "str",
            Type::U32 => "u32",
            Type::Bool => "bool",
            Type::Void => "void",
        };
        write!(f, "{s}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
pub enum BoolOperation {
    GTC,
    LTC,
    GTE,
    LTE,
    Eql,
    And,
    Or_,
    NEq,
    NNd,
    Nor,
}
impl BoolOperation {
    // fn switch(self) -> Self {
    //     match self {
    //         BoolOperation::GTC => BoolOperation::LTC,
    //         BoolOperation::LTC => BoolOperation::GTC,
    //         BoolOperation::GTE => BoolOperation::LTE,
    //         BoolOperation::LTE => BoolOperation::GTE,
    //         BoolOperation::Eql => BoolOperation::Eql,
    //         BoolOperation::And => BoolOperation::And,
    //         BoolOperation::Or_ => BoolOperation::Or_,
    //     }
    // }
    // pub fn is_logic(&self) -> bool {
    //     matches!(self, Self::And | Self::Or_)
    // }
    pub fn not(self) -> Self {
        match self {
            BoolOperation::GTC => BoolOperation::LTE,
            BoolOperation::LTC => BoolOperation::GTE,
            BoolOperation::GTE => BoolOperation::LTC,
            BoolOperation::LTE => BoolOperation::GTC,
            BoolOperation::Eql => BoolOperation::NEq,
            BoolOperation::And => BoolOperation::NNd,
            BoolOperation::Or_ => BoolOperation::Nor,
            BoolOperation::NEq => BoolOperation::Eql,
            BoolOperation::NNd => BoolOperation::And,
            BoolOperation::Nor => BoolOperation::Or_,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
pub enum Operation {
    Bool(BoolOperation),
    BND,
    BOR,
    Add,
    Sub,
    Mul,
    Div,
}
impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = Self::MAPPING
            .iter()
            .find_map(|(s, op)| if *self == *op { Some(s) } else { None })
            .unwrap();

        write!(f, "{s}")
    }
}
impl Operation {
    pub fn precedence(self) -> i32 {
        use BoolOperation::*;
        match self {
            Operation::Bool(And | Or_) => 0,
            Operation::Bool(_) => 1,
            Operation::Add | Operation::Sub => 2,
            Operation::Mul | Operation::Div => 3,
            Operation::BND | Operation::BOR => 4,
        }
    }
    pub fn is_commutative(&self) -> bool {
        matches!(
            self,
            Operation::Bool(BoolOperation::Or_)
                | Operation::Bool(BoolOperation::And)
                | Operation::Add
                | Operation::Mul
        )
    }

    pub fn from_start(string: &str) -> Option<(Self, usize)> {
        for (op_str, op) in Operation::MAPPING.iter() {
            if string.starts_with(op_str) {
                return Some((*op, op_str.len()));
            }
        }
        None
    }

    pub fn is_bool_op(&self) -> bool {
        matches!(self, Operation::Bool(_))
    }

    const MAPPING: &'static [(&'static str, Self)] = &[
        (">=", Self::Bool(BoolOperation::GTE)),
        ("<=", Self::Bool(BoolOperation::LTE)),
        ("&&", Self::Bool(BoolOperation::And)),
        ("||", Self::Bool(BoolOperation::Or_)),
        ("==", Self::Bool(BoolOperation::Eql)),
        ("+", Self::Add),
        ("-", Self::Sub),
        ("*", Self::Mul),
        ("/", Self::Div),
        (">", Self::Bool(BoolOperation::GTC)),
        ("<", Self::Bool(BoolOperation::LTC)),
        ("&", Self::BND),
        ("|", Self::BOR),
    ];
}

#[derive(Debug, Clone)]
pub enum Token {
    Var,
    Loop,
    While,
    Ident(Id),
    Number(u32),
    String(Rc<String>),
    Bool(bool),
    Type(Type),
    Operation(Operation),
    If,
    Else,
    // symbols
    Semicolon,
    Colon,
    Equal,
    BOpen,
    BClose,
    POpen,
    PClose,
    SOpen,
    SClose,
    Comma,
}
impl Token {
    /// doesn't do symbols
    fn parse(token: String) -> Self {
        let token = match token.as_str() {
            "var" => Self::Var,
            "loop" => Self::Loop,
            "while" => Self::While,
            "true" => Self::Bool(true),
            "false" => Self::Bool(false),
            "if" => Self::If,
            "else" => Self::Else,
            _ => {
                // string
                if token.starts_with('"') && token.ends_with('"') {
                    let string = token.trim_matches('"').replace("\\n", "\n");
                    Self::String(Rc::new(string))
                }
                // type
                else if let Some(typ) = Type::parse(&token) {
                    Self::Type(typ)
                }
                // number
                else if let Ok(num) = u32::from_str_radix(&token, 10) {
                    Self::Number(num)
                }
                // identifier
                else {
                    Self::Ident(Rc::new(token).into())
                }
            }
        };
        token
    }
    fn symbol(sym: char) -> Option<Self> {
        let sym = match sym {
            ';' => Token::Semicolon,
            ':' => Token::Colon,
            '=' => Token::Equal,
            '{' => Token::BOpen,
            '}' => Token::BClose,
            '(' => Token::POpen,
            ')' => Token::PClose,
            '[' => Token::SOpen,
            ']' => Token::SClose,
            ',' => Token::Comma,
            _ => {
                return None;
            }
        };
        Some(sym)
    }
}
pub struct Tokenizer<'a> {
    code: &'a str,
    code_chars: Peekable<CharIndices<'a>>,
}
impl<'a> Tokenizer<'a> {
    pub fn new(code: &'a str) -> Self {
        let code_chars = code.char_indices().peekable();
        Self { code, code_chars }
    }
    pub fn strip_leading_whitespace(&mut self) {
        while let Some((_, c)) = self.code_chars.peek() {
            if c.is_whitespace() {
                self.code_chars.next();
            } else {
                break;
            }
        }
    }
}
impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let (start, c) = loop {
            self.strip_leading_whitespace();

            let (start, c) = self.code_chars.next()?;

            // check if comment
            let mut is_comment = false;
            if c == '/' {
                if let Some((_, c2)) = self.code_chars.peek() {
                    if *c2 == '/' {
                        is_comment = true;
                    }
                }
            }
            if is_comment {
                // skip to the next line and try again
                for (_, c) in self.code_chars.by_ref() {
                    if c == '\n' {
                        break;
                    }
                }
            } else {
                // exit if not comment
                break (start, c);
            };
        };

        let sym = Token::symbol(c);
        if sym.is_some() {
            return sym;
        }
        if let Some((op, len)) = Operation::from_start(&self.code[start..]) {
            for _ in 1..len {
                self.code_chars.next();
            }
            return Some(Token::Operation(op));
        }
        let mut end = start;

        let is_string = c == '"';

        while let Some((i, c)) = self.code_chars.peek() {
            if !is_string
                && (c.is_whitespace()
                    || Token::symbol(*c).is_some()
                    || Operation::from_start(&self.code[*i..]).is_some())
            {
                break;
            }
            let (i, c) = self.code_chars.next().unwrap();
            end = i;

            if is_string && c == '"' {
                break;
            }
        }

        let token = &self.code[start..=end];
        Some(Token::parse(String::from(token)))
    }
}
