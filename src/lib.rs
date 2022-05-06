

mod lexer {
    use crate::lexer::Token::PipePipe;

    const START_COL: usize = 1;    // usually 1 or 0 for error message purpose
    const START_LINE: usize = 1;   // usually 1 or 0 for error message purpose

    #[derive(Debug)]
    struct GenErr<'a>(&'a str);

    #[derive(Debug, PartialEq)]
    enum Keyword {
        Fn,           //  fn
        Let,          //  let
        Var,          //  var
        Pub,          //  pub
        Pri,          //  pri
        Mod,          //  mod
        Class,        //  class
        If,           //  if
        Else,         //  else
        Return,       //  return
        Continue,     //  continue
        Break,        //  break
        For,          //  for
        While,        //  while
        Loop,         //  loop
        Yield,        //  yield
        Async,        //  async
        Await,        //  await
        True,         //  true
        False,        //  false
        Null,         //  null
    }

    impl Keyword {
        fn from_str(str: &str) -> Option<Keyword> {
            if str == "fn"       { return Some(Keyword::Fn) }
            if str == "let"      { return Some(Keyword::Let) }
            if str == "var"      { return Some(Keyword::Var) }
            if str == "pub"      { return Some(Keyword::Pub) }
            if str == "pri"      { return Some(Keyword::Pri) }  // i'm debating whether to go forward with this
            if str == "mod"      { return Some(Keyword::Mod) }
            if str == "class"    { return Some(Keyword::Class) }
            if str == "if"       { return Some(Keyword::If) }
            if str == "else"     { return Some(Keyword::Else) }
            if str == "return"   { return Some(Keyword::Return) }
            if str == "continue" { return Some(Keyword::Continue) }
            if str == "break"    { return Some(Keyword::Break) }
            if str == "for"      { return Some(Keyword::For) }
            if str == "while"    { return Some(Keyword::While) }
            if str == "loop"     { return Some(Keyword::Loop) }
            if str == "yield"    { return Some(Keyword::Yield) }
            if str == "async"    { return Some(Keyword::Async) }
            if str == "await"    { return Some(Keyword::Await) }
            if str == "true"     { return Some(Keyword::True) }
            if str == "false"    { return Some(Keyword::False) }
            if str == "null"     { return Some(Keyword::Null) }

            None
        }
    }

    #[derive(Debug, PartialEq)]
    enum Token {
        OpenParen,              //  (
        CloseParen,             //  )
        OpenBracket,            //  [
        CloseBracket,           //  ]
        OpenBrace,              //  {
        CloseBrace,             //  }
        Colon,                  //  :
        ColonColon,             //  ::
        Bang,                   //  !
        Question,               //  ?
        Comma,                  //  ,
        Slash,                  //  /
        Star,                   //  *
        StarStar,               //  **
        Dash,                   //  -
        DashEq,                 //  -=
        Plus,                   //  +
        PlusEq,                 //  +=
        Percent,                //  %
        Dot,                    //  .
        DotDot,                 //  ..
        Semicolon,              //  ;
        Amp,                    //  &
        AmpAmp,                 //  &&
        Pipe,                   //  |
        PipePipe,               //  ||
        Arrow,                  //  ->
        FatArrow,               //  =>
        Eq,                     //  =
        EqEq,                   //  ==
        BangEq,                 //  !=
        Gt,                     //  >
        Lt,                     //  <
        GtEq,                   //  >=
        LtEq,                   //  <=
        NewLine,                //  \n   -- \n\r are first batch normalized to \n before tokenizing

        Ident(String),       //  any valid identifier

        StrInterp(Vec<char>),   //  "a ${expr} b"  =>  StrInterp("a ") + Expr + StrLit(" b")
        StrBlock(Vec<char>),    //  """str block"""  => StrBlock("str block")
        StrLit(Vec<char>),      //  "string"
        Int64Lit(i64),          //  0 -1 -94399 42452
        Uint64Lit(u64),         //  0u 9u 19u 424252u
        Float64Lit(f64),        //  0.0 1e5 1.3E31
        BigIntLit(String),      //  0n 3881974n -1n
        Uint8Lit(u8),           //  0b 127b 128b 255b
        Keyword(Keyword),       //  if let var else class pub pri return continue yield ......
        Eof                     //  Eof
    }

    fn char_can_start_ident(char: char) -> bool {
        (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || char == '_'
    }
    fn char_can_cont_ident(char: char) -> bool {
        (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || (char >= '0' && char <= '9') || char == '_'
    }

    // Line/Block Comments
    // It's the lexer's job to discard any line comments and block comments
    // Also the Shebang at line 1
    // So comments are not considered Tokens

    #[derive(Debug)]
    pub struct LexingWorktable {
        program: Vec<char>,
        index: usize,
        cur_line: usize,
        cur_col: usize,
    }

    impl LexingWorktable {
        fn new(program: Vec<char>) -> LexingWorktable {
            LexingWorktable {
                program,
                index: 0,
                cur_line: START_LINE,
                cur_col: START_COL,
            }
        }
        fn consume(&mut self) -> Option<char> {
            let char = self.peek();
            self.advance();
            char
        }
        fn advance(&mut self) {
            if self.peek() == Some('\n') {
                self.cur_line += 1;
                self.cur_col = START_COL;
            }
            else {
                self.cur_col += 1;
            }
            self.index += 1;
        }
        fn advance_x(&mut self, chars: usize) {
            for _ in 1..chars {
                self.advance()
            }
        }
        fn peek(&self) -> Option<char> {
            if self.index >= self.program.len() { return None }
            Some(self.program[self.index])
        }
        fn peek_next(&self) -> Option<char> {
            if self.index + 1 >= self.program.len() { return None }
            Some(self.program[self.index+1])
        }
        fn peek_next_next(&self) -> Option<char> {
            self.peek_x(2)
        }
        fn peek_next_next_next(&self) -> Option<char> {
            self.peek_x(3)
        }
        fn peek_x(&self, advance: usize) -> Option<char> {
            if self.index + advance >= self.program.len() { return None }
            Some(self.program[self.index + advance])
        }
        fn skip_line_comment(&mut self) {
            while self.peek() != Some('\n') && self.peek() != None {
                self.advance();
            }
        }
        fn skip_block_comment(&mut self) -> Option<GenErr> {
            let mut nesting: usize = 1;
            while nesting > 0 {
                if self.peek() == None {
                    return Some(GenErr("Unclosed block comment at EOF."));
                }
                else if self.peek() == Some('/') && self.peek_next() == Some('*') {
                    self.advance();
                    self.advance();
                    nesting += 1;
                    continue;
                }
                else if self.peek() == Some('*') && self.peek_next() == Some('/') {
                    self.advance();
                    self.advance();
                    nesting -= 1;
                    continue;
                }
                else {
                    self.advance();
                }
            }
            None
        }
        fn skip_whitespace(&mut self) {
            let char = self.peek();
            while char == Some(' ') || char == Some('\r') || char == Some('\t') {
                self.advance()
            }
        }
        fn resolve_2ch_token(&mut self, second_ch: char, if_2ch: Token, if_1ch: Token) -> Token {
            if self.peek() == Some(second_ch) {
                self.advance();
                if_2ch
            } else {
                if_1ch
            }
        }

        fn consume_ident(&mut self, first_char: char) -> Token {
            let mut chars = vec![first_char];
            while let Some(char) = self.peek() {
                if !char_can_cont_ident(char) { break }
                chars.push(char);
                self.advance()
            }
            let possible_ident: String = chars.into_iter().collect();
            if let Some(keyword) = Keyword::from_str(&possible_ident) {
                Token::Keyword(keyword)
            } else {
                Token::Ident(possible_ident)
            }
        }

    }

    #[derive(Debug)]
    struct LexerErrMsg<'a> {
        index: usize,
        line: usize,
        col: usize,
        desc: &'a str,
    }

    fn lexer(mut worktable: LexingWorktable) -> Result<Vec<Token>, LexerErrMsg<'static>> {
        let mut tvec: Vec<Token> = vec![];
        while let Some(char) = worktable.consume() {
            match char {
                '(' => tvec.push(Token::OpenParen),
                ')' => tvec.push(Token::CloseParen),
                '[' => tvec.push(Token::OpenBracket),
                ']' => tvec.push(Token::CloseBracket),
                '{' => tvec.push(Token::OpenBrace),
                '}' => tvec.push(Token::CloseBrace),
                ':' => tvec.push(worktable.resolve_2ch_token(':', Token::ColonColon, Token::Colon)),
                '!' => tvec.push(worktable.resolve_2ch_token('=', Token::BangEq, Token::Bang)),
                '?' => tvec.push(Token::Question),
                ',' => tvec.push(Token::Comma),
                '/' => if worktable.peek() == Some('/') {
                    worktable.skip_line_comment();
                    continue;
                } else if worktable.peek() == Some('*') {
                    let skip_result = &worktable.skip_block_comment();
                    // if let Some(err) = skip_result {
                    //     return Err(LexerErrMsg {
                    //         index: worktable.index,
                    //         line: worktable.cur_line,
                    //         col: worktable.cur_col,
                    //         desc: err.0
                    //     })
                    // }
                } else {
                    tvec.push(Token::Slash)
                }
                '*' => tvec.push(worktable.resolve_2ch_token('*', Token::StarStar, Token::Star)),
                '-' => if worktable.peek() == Some('>') {
                    tvec.push(Token::Arrow);
                    worktable.advance()
                } else {
                    tvec.push(worktable.resolve_2ch_token('=', Token::DashEq, Token::Dash))
                }
                '+' => tvec.push(worktable.resolve_2ch_token('=', Token::PlusEq, Token::Plus)),
                '%' => tvec.push(Token::Percent),
                '.' => tvec.push(worktable.resolve_2ch_token('.', Token::DotDot, Token::Dot)),
                ';' => tvec.push(Token::Semicolon),
                '&' => tvec.push(worktable.resolve_2ch_token('&', Token::AmpAmp, Token::Amp)),
                '|' => tvec.push(worktable.resolve_2ch_token('|', Token::PipePipe, Token::Pipe)),
                '=' => if worktable.peek() == Some('>') {
                    tvec.push(Token::FatArrow);
                    worktable.advance()
                } else {
                    tvec.push(worktable.resolve_2ch_token('=', Token::EqEq, Token::Eq))
                }
                '>' => tvec.push(worktable.resolve_2ch_token('=', Token::GtEq, Token::Gt)),
                '<' => tvec.push(worktable.resolve_2ch_token('=', Token::LtEq, Token::Lt)),
                '\n' => tvec.push(Token::NewLine),
                ' ' | '\t' | '\r' => worktable.skip_whitespace(),
                _ => {
                    if char_can_start_ident(char) {
                        tvec.push(worktable.consume_ident(char))
                    }
                }
            }
        }

        return Ok(tvec)
    }



    #[cfg(test)]
    mod tests {
        use std::ops::Index;
        use crate::lexer::{self, Keyword, LexingWorktable, Token};

        #[test]
        fn lexing_worktable_basic_operations() {
            let mut worktable = LexingWorktable::new(
                "program\n".chars().collect()
            );
            assert_eq!(worktable.consume(), Some('p'));
            assert_eq!(worktable.consume(), Some('r'));
            assert_eq!(worktable.consume(), Some('o'));
            assert_eq!(worktable.consume(), Some('g'));
            assert_eq!(worktable.consume(), Some('r'));
            assert_eq!(worktable.consume(), Some('a'));
            assert_eq!(worktable.consume(), Some('m'));

            assert_eq!(worktable.cur_line, 1);
            assert_eq!(worktable.cur_col, 8);
            assert_eq!(worktable.index, 7);

            assert_eq!(worktable.consume(), Some('\n'));

            assert_eq!(worktable.cur_line, 2);
            assert_eq!(worktable.cur_col, 1);
        }

        #[test]
        fn lexer_basics() {
            let mut worktable = LexingWorktable::new(
                "(-===+= . ident..ident2 ... /* comments /* nested */ */....for)"
                    .chars().collect()
            );
            let results = lexer::lexer(worktable).unwrap();
            assert_eq!(results.len(), 14);
            assert_eq!(results[0], Token::OpenParen);
            assert_eq!(results[1], Token::DashEq);
            assert_eq!(results[2], Token::EqEq);
            assert_eq!(results[3], Token::PlusEq);
            assert_eq!(results[4], Token::Dot);
            assert_eq!(results[5], Token::Ident("ident".into()));
            assert_eq!(results[6], Token::DotDot);
            assert_eq!(results[7], Token::Ident("ident2".into()));
            assert_eq!(results[8], Token::DotDot);
            assert_eq!(results[9], Token::Dot);
            assert_eq!(results[10], Token::DotDot);
            assert_eq!(results[11], Token::DotDot);
            assert_eq!(results[12], Token::Keyword(Keyword::For));
            assert_eq!(results[13], Token::CloseParen);

            let worktable = LexingWorktable::new(
                "let fn var pub mod".chars().collect()
            );
            let results = lexer::lexer(worktable).unwrap();
            assert_eq!(results[0], Token::Keyword(Keyword::Let));
            assert_eq!(results[1], Token::Keyword(Keyword::Fn));
            assert_eq!(results[2], Token::Keyword(Keyword::Var));
            assert_eq!(results[3], Token::Keyword(Keyword::Pub));
            assert_eq!(results[4], Token::Keyword(Keyword::Mod));

            let worktable = LexingWorktable::new(
                "fn add(x: i64, y: i64) -> i64 { x + y } ;;\nlet id = x: i64 -> i64 => x "
                    .chars().collect()
            );
            let results = lexer::lexer(worktable).unwrap();
            println!("{:?}", results);
            let mut i: usize = 0;
            let mut ip = || { i+=1 ; i-1 };
            assert_eq!(results[ip()], Token::Keyword(Keyword::Fn));
            assert_eq!(results[ip()], Token::Ident("add".into()));
            assert_eq!(results[ip()], Token::OpenParen);
            assert_eq!(results[ip()], Token::Ident("x".into()));
            assert_eq!(results[ip()], Token::Colon);
            assert_eq!(results[ip()], Token::Ident("i64".into()));
            assert_eq!(results[ip()], Token::Comma);
            assert_eq!(results[ip()], Token::Ident("y".into()));
            assert_eq!(results[ip()], Token::Colon);
            assert_eq!(results[ip()], Token::Ident("i64".into()));
            assert_eq!(results[ip()], Token::CloseParen);
            assert_eq!(results[ip()], Token::Arrow);
            assert_eq!(results[ip()], Token::Ident("i64".into()));
            assert_eq!(results[ip()], Token::OpenBrace);
            assert_eq!(results[ip()], Token::Ident("x".into()));
            assert_eq!(results[ip()], Token::Plus);
            assert_eq!(results[ip()], Token::Ident("y".into()));
            assert_eq!(results[ip()], Token::CloseBrace);
            assert_eq!(results[ip()], Token::Semicolon);
            assert_eq!(results[ip()], Token::Semicolon);
            assert_eq!(results[ip()], Token::NewLine);
            assert_eq!(results[ip()], Token::Keyword(Keyword::Let));
            assert_eq!(results[ip()], Token::Ident("id".into()));
            assert_eq!(results[ip()], Token::Eq);
            assert_eq!(results[ip()], Token::Ident("x".into()));
            assert_eq!(results[ip()], Token::Colon);
            assert_eq!(results[ip()], Token::Ident("i64".into()));
            assert_eq!(results[ip()], Token::Arrow);
            assert_eq!(results[ip()], Token::Ident("i64".into()));
            assert_eq!(results[ip()], Token::FatArrow);
            assert_eq!(results[ip()], Token::Ident("x".into()));
        }
    }
}