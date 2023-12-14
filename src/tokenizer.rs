use std::{iter::Peekable, rc::Rc, str::CharIndices};

#[derive(Debug, Clone, Copy)]
pub enum Type {
    String,
    I32,
    U32,
}
impl Type {
    pub fn parse(typ: &str) -> Option<Self> {
        let typ = match typ {
            "u32" => Self::U32,
            "i32" => Self::I32,
            "str" => Self::String,
            _ => {
                return None;
            }
        };
        Some(typ)
    }
}

#[derive(Debug, Clone)]
pub enum Token {
    Var,
    Exit,
    Print,
    Loop,
    Ident(Rc<String>),
    Number(u32),
    String(Rc<String>),
    Type(Type),
    // symbols
    Semicolon,
    Colon,
    Equal,
    Open,
    Close,
}
impl Token {
    /// doesn't do symbols
    fn parse(token: String) -> Option<Self> {
        let token = match token.as_str() {
            "exit" => Self::Exit,
            "var" => Self::Var,
            "print" => Self::Print,
            "loop" => Self::Loop,
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
        Some(token)
    }
    fn symbol(sym: char) -> Option<Self> {
        let sym = match sym {
            ';' => Token::Semicolon,
            ':' => Token::Colon,
            '=' => Token::Equal,
            '{' => Token::Open,
            '}' => Token::Close,
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
        let mut end = start;

        let is_string = c == '"';

        while let Some((_i, c)) = self.code_chars.peek() {
            if !is_string && (c.is_whitespace() || Token::symbol(*c).is_some()) {
                break;
            }
            let (i, c) = self.code_chars.next().unwrap();
            end = i;

            if is_string && c == '"' {
                break;
            }
        }

        let token = &self.code[start..=end];
        Token::parse(String::from(token))
    }
}
