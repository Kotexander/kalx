use std::{fmt::Display, iter::Peekable, rc::Rc, str::CharIndices};

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
pub enum Operation {
    GTC,
    LTC,
    GTE,
    LTE,
    And,
    Or_,
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
        match self {
            Operation::GTC
            | Operation::LTC
            | Operation::GTE
            | Operation::LTE
            | Operation::And
            | Operation::Or_ => 0,
            Operation::Add | Operation::Sub => 1,
            Operation::Mul | Operation::Div => 2,
            Operation::BND | Operation::BOR => 3,
        }
    }
    pub fn is_commutative(&self) -> bool {
        matches!(
            self,
            Operation::Or_ | Operation::And | Operation::Add | Operation::Mul
        )
    }
    pub fn can_be_operator(c: char) -> bool {
        Self::MAPPING.iter().any(|(s, _)| s.starts_with(c))
    }
    /// parse with peek
    pub fn parse(op: &str) -> Option<Self> {
        Self::MAPPING
            .iter()
            .find_map(|(s, o)| if *s == op { Some(*o) } else { None })
    }

    pub fn is_comparator(&self) -> bool {
        matches!(
            self,
            Operation::GTC
                | Operation::LTC
                | Operation::GTE
                | Operation::LTE
                | Operation::And
                | Operation::Or_
        )
    }

    const MAPPING: &'static [(&'static str, Self)] = &[
        (">=", Self::GTE),
        ("<=", Self::LTE),
        ("&&", Self::And),
        ("||", Self::Or_),
        ("+", Self::Add),
        ("-", Self::Sub),
        ("*", Self::Mul),
        ("/", Self::Div),
        (">", Self::GTC),
        ("<", Self::LTC),
        ("&", Self::BND),
        ("|", Self::BOR),
    ];
}

#[derive(Debug, Clone)]
pub enum Token {
    Var,
    Loop,
    While,
    Ident(Rc<String>),
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
                    Self::Ident(Rc::new(token))
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
        // try 2 char op
        if self.code_chars.peek().is_some() {
            if let Some(op) = Operation::parse(&self.code[start..=(start + 1)]) {
                self.code_chars.next();
                return Some(Token::Operation(op));
            }
        }
        // try 1 char op
        if let Some(op) = Operation::parse(&self.code[start..=start]) {
            return Some(Token::Operation(op));
        }

        let mut end = start;

        let is_string = c == '"';

        while let Some((_i, c)) = self.code_chars.peek() {
            if !is_string
                && (c.is_whitespace()
                    || Token::symbol(*c).is_some()
                    || Operation::can_be_operator(*c))
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
