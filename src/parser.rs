use std::{iter::Peekable, rc::Rc, str::CharIndices};

#[derive(Debug, Clone)]
pub enum Token {
    Var,
    Exit,
    Print,
    Ident(Rc<String>),
    Number(u32),
    String(Rc<String>),
    // symbols
    Semicolon,
    Equal,
}
impl Token {
    /// doesn't do symbols
    fn parse(token: String) -> Option<Self> {
        let token = match token.as_str() {
            "exit" => Self::Exit,
            "var" => Self::Var,
            "print" => Self::Print,
            _ => {
                if token.starts_with('"') && token.ends_with('"') {
                    let string = token.trim_matches('"').replace("\\n", "\n");
                    Self::String(Rc::new(string))
                } else if let Ok(num) = u32::from_str_radix(&token, 10) {
                    Self::Number(num)
                } else {
                    Self::Ident(Rc::new(token))
                }
            }
        };
        Some(token)
    }
    fn symbol(sym: char) -> Option<Self> {
        let sym = match sym {
            ';' => Token::Semicolon,
            '=' => Token::Equal,
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
}
impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // strip leading whitespace
        while let Some((_, c)) = self.code_chars.peek() {
            if c.is_whitespace() {
                self.code_chars.next();
            } else {
                break;
            }
        }

        let (start, c) = self.code_chars.next()?;

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
