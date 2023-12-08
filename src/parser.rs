use std::{iter::Peekable, str::CharIndices};

#[derive(Debug, Clone, Copy)]
pub enum Token {
    Exit,
    Semicolon,
    Number(u32),
}

fn symbol(c: char) -> Option<Token> {
    if c == ';' {
        return Some(Token::Semicolon);
    }
    return None;
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

        let sym = symbol(c);
        if sym.is_some() {
            return sym;
        }

        let mut end = start;

        while let Some((_i, c)) = self.code_chars.peek() {
            if c.is_whitespace() || symbol(*c).is_some() {
                break;
            }
            let (i, _c) = self.code_chars.next().unwrap();
            end = i;
        }

        let token = &self.code[start..=end];

        match u32::from_str_radix(token, 10) {
            Ok(n) => Some(Token::Number(n)),
            Err(_) => match token {
                "exit" => Some(Token::Exit),
                _ => {
                    panic!("unknown token: {token}")
                }
            },
        }
    }
}
