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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    String,
    U32,
    U8,
    Bool,
    Void,
    Ptr(Box<Self>),
    Array(Box<Self>),
}
impl Type {
    pub fn size(&self) -> u32 {
        match self {
            Type::String => 4,
            Type::U32 => 4,
            Type::U8 => 1,
            Type::Bool => 1,
            Type::Void => 0,
            Type::Ptr(_) => 4,
            Type::Array(_) => 4,
        }
    }
    const MAPPING: &'static [(&'static str, Self)] = &[
        ("str", Self::String),
        ("u32", Self::U32),
        ("u8", Self::U8),
        ("bool", Self::Bool),
        ("void", Self::Void),
    ];
}
impl TryFrom<&str> for Type {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::MAPPING
            .iter()
            .find_map(|(s, t)| if *s == value { Some(t.clone()) } else { None })
            .ok_or(())
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Ptr(t) => write!(f, "#{t}"),
            Type::Array(t) => write!(f, "[{t}]"),
            _ => {
                let s = Self::MAPPING
                    .iter()
                    .find_map(|(s, t)| if *t == *self { Some(s) } else { None })
                    .unwrap();
                write!(f, "{s}")
            }
        }
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
    NOr,
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
            BoolOperation::Or_ => BoolOperation::NOr,
            BoolOperation::NEq => BoolOperation::Eql,
            BoolOperation::NNd => BoolOperation::And,
            BoolOperation::NOr => BoolOperation::Or_,
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
        let op = Token::Operation(*self);
        for (s, t) in Token::STRICT_MAPPING.iter().chain(Token::MAPPING.iter()) {
            if *t == op {
                return write!(f, "{s}");
            }
        }
        unreachable!();
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

    pub fn is_bool_op(&self) -> bool {
        matches!(self, Operation::Bool(_))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    Fun,
    Arrow,
    Ref,
    Deref,
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
    fn parse(token: String) -> Self {
        if let Some(token) =
            Self::MAPPING
                .iter()
                .find_map(|(s, t)| if *s == token { Some(t.clone()) } else { None })
        {
            token
        } else {
            // string
            if token.starts_with('"') && token.ends_with('"') {
                let string = token.trim_matches('"').replace("\\n", "\n");
                Self::String(Rc::new(string))
            }
            // type
            else if let Ok(typ) = Type::try_from(token.as_str()) {
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
    }
    fn symbol(sym: char) -> Option<Self> {
        Self::MAPPING.iter().find_map(|(s, t)| {
            if s.len() == 1 && s.chars().next().unwrap() == sym {
                Some(t.clone())
            } else {
                None
            }
        })
    }
    pub fn from_strict(string: &str) -> Option<(Token, usize)> {
        for (op_str, op) in Self::STRICT_MAPPING.iter() {
            if string.starts_with(op_str) {
                return Some((op.clone(), op_str.len()));
            }
        }
        None
    }

    const MAPPING: &'static [(&'static str, Self)] = &[
        (";", Self::Semicolon),
        (":", Self::Colon),
        ("=", Self::Equal),
        ("{", Self::BOpen),
        ("}", Self::BClose),
        ("(", Self::POpen),
        (")", Self::PClose),
        ("[", Self::SOpen),
        ("]", Self::SClose),
        (",", Self::Comma),
        ("if", Self::If),
        ("var", Self::Var),
        ("fun", Self::Fun),
        ("else", Self::Else),
        ("true", Self::Bool(true)),
        ("loop", Self::Loop),
        ("false", Self::Bool(false)),
        ("while", Self::While),
    ];

    const STRICT_MAPPING: &'static [(&'static str, Self)] = &[
        (">=", Self::Operation(Operation::Bool(BoolOperation::GTE))),
        ("<=", Self::Operation(Operation::Bool(BoolOperation::LTE))),
        ("&&", Self::Operation(Operation::Bool(BoolOperation::And))),
        ("||", Self::Operation(Operation::Bool(BoolOperation::Or_))),
        ("!&", Self::Operation(Operation::Bool(BoolOperation::NNd))),
        ("!|", Self::Operation(Operation::Bool(BoolOperation::NOr))),
        ("==", Self::Operation(Operation::Bool(BoolOperation::Eql))),
        ("!=", Self::Operation(Operation::Bool(BoolOperation::NEq))),
        ("->", Self::Arrow),
        ("#", Self::Ref),
        ("@", Self::Deref),
        ("+", Self::Operation(Operation::Add)),
        ("-", Self::Operation(Operation::Sub)),
        ("*", Self::Operation(Operation::Mul)),
        ("/", Self::Operation(Operation::Div)),
        (">", Self::Operation(Operation::Bool(BoolOperation::GTC))),
        ("<", Self::Operation(Operation::Bool(BoolOperation::LTC))),
        ("&", Self::Operation(Operation::BND)),
        ("|", Self::Operation(Operation::BOR)),
    ];
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Ident(id) => write!(f, "{}", id.0),
            Token::Number(n) => write!(f, "{n}"),
            Token::String(s) => write!(f, "{s}"),
            Token::Bool(b) => write!(f, "{b}"),
            Token::Type(t) => write!(f, "{t}"),
            Token::Operation(op) => write!(f, "{op}"),
            _ => {
                for (s, t) in Self::MAPPING.iter().chain(Self::STRICT_MAPPING.iter()) {
                    if t == self {
                        return write!(f, "{s}");
                    }
                }
                unreachable!()
            }
        }
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
        if let Some((t, len)) = Token::from_strict(&self.code[start..]) {
            for _ in 1..len {
                self.code_chars.next();
            }
            return Some(t);
        }
        let mut end = start;

        let is_string = c == '"';

        while let Some((i, c)) = self.code_chars.peek() {
            if !is_string
                && (c.is_whitespace()
                    || Token::symbol(*c).is_some()
                    || Token::from_strict(&self.code[*i..]).is_some())
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
