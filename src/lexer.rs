#![allow(dead_code)]


const MAX_IDENT_LENGTH: usize = 1024;
const MAX_NUM_LITERAL_LENGTH: usize = 32768;   // A 0.25MB num literal maximum
const MAX_INTERPOLATION_NESTING: usize = 32;   // Arbitrary
const START_COL: usize = 1;    // usually 1 or 0 for error message purpose
const START_LINE: usize = 1;   // usually 1 or 0 for error message purpose

mod helper {

    use std::num::ParseIntError;
    use super::LexerErrMsg;
    use super::LexerErrCases;
    use super::Token;

    pub fn char_can_start_ident(char: char) -> bool {
        (char >= 'a' && char <= 'z') || char == '_' || (char >= 'A' && char <= 'Z')
    }
    pub fn char_can_cont_ident(char: char) -> bool {
        (char >= 'a' && char <= 'z') || char == '_' || (char >= 'A' && char <= 'Z') || (char >= '0' && char <= '9')
    }
    pub fn char_can_start_num(char: char) -> bool {
        char >= '0' && char <= '9'
    }
    pub fn char_can_cont_num(char: char) -> bool {
        (char >= '0' && char <= '9') || char == '.'
            || (char >= 'a' && char <= 'z')
            || (char >= 'A' && char <= 'Z')  // a-zA-Z for hex and suffix explicit type annotations
            || char == '_'  // numerical visual separator largely ignored
    }
    pub fn hex_digit_val(c: char) -> Option<u8> {
        if c as u8 >= '0' as u8 && c as u8 <= '9' as u8 { return Some(c as u8 - '0' as u8) }
        if c as u8 >= 'a' as u8 && c as u8 <= 'f' as u8 { return Some(c as u8 - 'a' as u8 + 10) }
        if c as u8 >= 'A' as u8 && c as u8 <= 'F' as u8 { return Some(c as u8 - 'A' as u8 + 10) }
        None
    }
    pub fn only_hex_chars(cs: &[char]) -> bool {
        for char in cs {
            if let Some(_hex_digit_val) = hex_digit_val(*char) {}
            else { return false }
        }
        true
    }
    pub fn chars_is_pure_decimal(cs: &[char]) -> bool {
        cs.iter().filter(|c| {
            let c = **c as u8;
            c < '0' as u8 || c > '9' as u8
        }).collect::<Vec<&char>>().len() == 0
    }
    pub fn has_double_underscore(cs: &[char]) -> bool {
        for i in 0..cs.len()-1 {
            if cs[i] == '_' && cs[i+1] == '_' { return true }
        }
        false
    }
    pub fn remove_leading_0s(cs: &[char]) -> &[char] {
        if cs.len() == 0 { return cs }
        let mut i = 0;
        loop {
            if cs[i] != '0' || i >= cs.len() - 1 { break }
            i += 1
        }
        return &cs[i..]
    }
    pub fn last_1_is(cs: &[char], match_with: char) -> Option<&[char]> {
        if cs.len() >= 1 && cs[cs.len()-1] == match_with {
            return Some(&cs[..cs.len()-1])
        }
        None
    }
    pub fn last_2_are(cs: &[char], match_with: [char; 2]) -> Option<&[char]> {
        if cs.len() >= 2 && cs[cs.len()-1] == match_with[1] && cs[cs.len()-2] == match_with[0] {
            return Some(&cs[..cs.len()-2])
        }
        None
    }
    pub fn last_3_are(cs: &[char], match_with: [char; 3]) -> Option<&[char]> {
        if cs.len() >= 3 && cs[cs.len()-1] == match_with[2] && cs[cs.len()-2] == match_with[1]
            && cs[cs.len()-3] == match_with[0] {
            return Some(&cs[..cs.len()-3])
        }
        None
    }
    pub fn last_4_are(cs: &[char], match_with: [char; 4]) -> Option<&[char]> {
        if cs.len() >= 4 && cs[cs.len()-1] == match_with[3] && cs[cs.len()-2] == match_with[2]
            && cs[cs.len()-3] == match_with[1] && cs[cs.len()-4] == match_with[0] {
            return Some(&cs[..cs.len()-4])
        }
        None
    }
    pub fn err_transform<T>(res: Result<T, ParseIntError>) -> Result<T, LexerErrCases> {
        match res {
            Ok(t) => Ok(t),
            Err(err) => Err(LexerErrCases::ParseIntError(err.to_string())),
        }
    }
    pub fn err_transform2<T>(res: Result<T, std::num::ParseFloatError>) -> Result<T, LexerErrCases> {
        match res {
            Ok(t) => Ok(t),
            Err(err) => Err(LexerErrCases::ParseFloatError(err.to_string())),
        }
    }
    pub fn coll(
        tokens: &mut Vec<Token>, errs: &mut Vec<LexerErrMsg>, res: Result<Token, LexerErrMsg>
    ) -> () {
        match res {
            Ok(tok) => tokens.push(tok),
            Err(err) => errs.push(err),
        }
    }
}
use helper::{*};

#[derive(Debug, PartialEq)]
pub enum LexerErrCases {
    UnclosedBlockComment,
    IdentOverMaxLength,
    NumLiteralOverMaxLength,
    NumLiteralTrailingUnderscore,
    NumLiteralContinuousUnderscore,
    HexIntegerLiteralInvalidChar,
    BigIntLiteralNoBody,
    BigIntLiteralBodyInvalid,
    ParseIntError(String),
    ParseFloatError(String),
    ZeroByteInSourceInStrLit,
    StrInterpolMaxDepthExceeded,
    UnfinishedStrLiteral,
    InvalidEscapeChar(char),
    InvalidEscapeAtEof,
}

#[derive(Debug, PartialEq)]
pub struct LexerErrMsg {
    pub index: usize,
    pub line: usize,
    pub col: usize,
    pub desc: LexerErrCases,
}

impl LexerErrMsg {
    fn new(worktable: &LexingWorktable, err: LexerErrCases) -> LexerErrMsg {
        return LexerErrMsg {
            index: worktable.index,
            line: worktable.cur_line,
            col: worktable.cur_col,
            desc: err
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Fn,           //  fn
    Let,          //  let
    Mut,          //  mut
    Pub,          //  pub
    Mod,          //  mod
    Use,          //  use
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
    In,           //  in
    Impl,         //  impl
    Super,        //  super
    Crate,        //  crate
    Self_,        //  self
}

impl Keyword {
    fn from_str(str: &str) -> Option<Keyword> {
        return match str {
            "fn"       => Some(Keyword::Fn),
            "let"      => Some(Keyword::Let),
            "mut"      => Some(Keyword::Mut),
            "pub"      => Some(Keyword::Pub),
            "mod"      => Some(Keyword::Mod),
            "use"      => Some(Keyword::Use),
            "class"    => Some(Keyword::Class),
            "if"       => Some(Keyword::If),
            "else"     => Some(Keyword::Else),
            "return"   => Some(Keyword::Return),
            "continue" => Some(Keyword::Continue),
            "break"    => Some(Keyword::Break),
            "for"      => Some(Keyword::For),
            "while"    => Some(Keyword::While),
            "loop"     => Some(Keyword::Loop),
            "yield"    => Some(Keyword::Yield),
            "async"    => Some(Keyword::Async),
            "await"    => Some(Keyword::Await),
            "true"     => Some(Keyword::True),
            "false"    => Some(Keyword::False),
            "null"     => Some(Keyword::Null),
            "in"       => Some(Keyword::In),
            "impl"     => Some(Keyword::Impl),
            "super"    => Some(Keyword::Super),
            "crate"    => Some(Keyword::Crate),
            "self"     => Some(Keyword::Self_),
            _          => None,
        }

    }
}

#[derive(Debug, PartialEq)]
pub enum NumericLiteral {
    //  1. Unary operator '-' not seen as numerical token by lexer (necessitates N.B. #2)
    //  2. Float values may not begin with '.'
    //  3. Float values must have decimal point in it or be of scientific notation
    //  4. Int values mustn't have decimal point nor be scientific.
    ImplicitDecI(String),  //  Unannotated integer literal  - not parsed - decimal
    ImplicitDecF(f64),     //  Unannotated floating literal - not parsed - decimal
    ImplicitHexI(String),  //  Unannotated integer literal  - not parsed - hex
    // ImplicitHexF,          //  Unannotated floating literal - not parsed - hex     - currently not supported
    I128Lit(u128),         //  Annotated i128 literal       - parsed     - decimal
    U128Lit(u128),         //  Annotated u128 literal       - parsed     - decimal
    I64Lit(u64),           //  Annotated i64 literal        - parsed     - decimal
    U64Lit(u64),           //  Annotated u64 literal        - parsed     - decimal
    F64Lit(f64),           //  Annotated f64 literal        - parsed     - decimal
    I32Lit(u32),           //  Annotated i32 literal        - parsed     - decimal
    U32Lit(u32),           //  Annotated u32 literal        - parsed     - decimal
    F32Lit(f32),           //  Annotated f32 literal        - parsed     - decimal
    I8Lit(u8),             //  Annotated i8 literal         - parsed     - decimal
    U8Lit(u8),             //  Annotated u8 literal         - parsed     - decimal
    BigDecLit(String),     //  Annotated bigint literal     - not parsed - decimal
    BigHexLit(String),     //  Annotated bigint literal     - not parsed - hex
    //  N.B 1 also, ImplicitI types must remain String because they might need to be parsed
    //        differently based on semantics and type inference.
    //  N.B 2 IxxInt are represented as Uint so that abs(ixx::MIN) does not cause overflow
    //        Otherwise, (-128i8) will be parsed as (Token::Dash, I8Lit(128)) which overflows.
}

impl NumericLiteral {
    fn from_chars(chars: Vec<char>) -> Result<NumericLiteral, LexerErrCases> {
        // No trailing underscore
        if chars.last() == Some(&'_') {
            return Err(LexerErrCases::NumLiteralTrailingUnderscore)
        }
        // ban continuous underscore
        if has_double_underscore(&chars) {
            return Err(LexerErrCases::NumLiteralContinuousUnderscore)
        }
        // getting rid of underscores (visual digit separators)
        let chars = chars.into_iter().filter(|x| { x != &'_' }).collect::<Vec<char>>();
        // If is hex num
        if chars.len() >= 3 && chars[0] == '0' && chars[1] == 'x' {
            return NumericLiteral::from_chars_hex(&chars[2..])
        }
        // Decimal literal from here on out
        // Cases:
        //   Case(1) Pure decimal integer                       ->   ImplicitDecI      NEEDS remove leading 0
        //   Case(2) Annotated by i128/u128/i64/u64/i32/u32/u8  ->   I128Lit .. U8Lit  NEEDS remove annotation, validate pure decimal, remove leading 0
        //   Case(3) Annotated by f64/f32                       ->   F64Lit .. F32Lit  NEEDS remove annotation, validate floating syntax, remove leading 0 (for float)
        //   Case(4) Annotated by n                             ->   BigDecHex         NEEDS remove annotation, validate pure decimal, remove leading 0
        //   Case(5) Else                                       ->   ImplicitDecF      NEEDS validate floating syntax, remove leading 0 (for float)
        // Case (1) Pure decimal integer
        if chars_is_pure_decimal(&chars) {
            return Ok(NumericLiteral::ImplicitDecI(remove_leading_0s(&chars).into_iter().collect::<String>()));
        }
        // Case (2) Annotated Int/Uint
        if let Some(annot_axed) = last_4_are(&chars, ['i', '1', '2', '8']) {
            let parsed = err_transform(u128::from_str_radix(&annot_axed.iter().collect::<String>(), 10))?;
            return Ok(NumericLiteral::I128Lit(parsed));
        }
        if let Some(annot_axed) = last_4_are(&chars, ['u', '1', '2', '8']) {
            let parsed = err_transform(u128::from_str_radix(&annot_axed.iter().collect::<String>(), 10))?;
            return Ok(NumericLiteral::U128Lit(parsed));
        }
        if let Some(annot_axed) = last_3_are(&chars, ['i', '6', '4']) {
            let parsed = err_transform(u64::from_str_radix(&annot_axed.iter().collect::<String>(), 10))?;
            return Ok(NumericLiteral::I64Lit(parsed));
        }
        if let Some(annot_axed) = last_3_are(&chars, ['u', '6', '4']) {
            let parsed = err_transform(u64::from_str_radix(&annot_axed.iter().collect::<String>(), 10))?;
            return Ok(NumericLiteral::U64Lit(parsed));
        }
        if let Some(annot_axed) = last_3_are(&chars, ['i', '3', '2']) {
            let parsed = err_transform(u32::from_str_radix(&annot_axed.iter().collect::<String>(), 10))?;
            return Ok(NumericLiteral::I32Lit(parsed));
        }
        if let Some(annot_axed) = last_3_are(&chars, ['u', '3', '2']) {
            let parsed = err_transform(u32::from_str_radix(&annot_axed.iter().collect::<String>(), 10))?;
            return Ok(NumericLiteral::U32Lit(parsed));
        }
        if let Some(annot_axed) = last_2_are(&chars, ['i', '8']) {
            let parsed = err_transform(u8::from_str_radix(&annot_axed.iter().collect::<String>(), 10))?;
            return Ok(NumericLiteral::I8Lit(parsed));
        }
        if let Some(annot_axed) = last_2_are(&chars, ['u', '8']) {
            let parsed = err_transform(u8::from_str_radix(&annot_axed.iter().collect::<String>(), 10))?;
            return Ok(NumericLiteral::U8Lit(parsed));
        }
        // Case (3) Annotated Float
        if let Some(annot_axed) = last_3_are(&chars, ['f', '6', '4']) {
            let parsed = err_transform2(annot_axed.iter().collect::<String>().parse::<f64>())?;
            return Ok(NumericLiteral::F64Lit(parsed));
        }
        if let Some(annot_axed) = last_3_are(&chars, ['f', '3', '2']) {
            let parsed = err_transform2(annot_axed.iter().collect::<String>().parse::<f32>())?;
            return Ok(NumericLiteral::F32Lit(parsed));
        }
        // Case (4) Bigint Decimal
        if let Some(annot_axed) = last_1_is(&chars, 'n') {
            return if chars_is_pure_decimal(&annot_axed) {
                Ok(NumericLiteral::BigDecLit(remove_leading_0s(&annot_axed).into_iter().collect::<String>()))
            } else {
                Err(LexerErrCases::BigIntLiteralBodyInvalid)
            }
        }
        // Case (5) ImplicitF -- we parse it into f64 by default
        let parsed = err_transform2(chars.iter().collect::<String>().parse::<f64>())?;
        return Ok(NumericLiteral::ImplicitDecF(parsed));
    }
    /** CALLERS: drop the leading "0x" then pass the rest of the chars to this function for parsing.
               Hex Numeric literal currently only support integers. To add decimal point need to support modify this function
     */
    fn from_chars_hex(chars: &[char]) -> Result<NumericLiteral, LexerErrCases> {
        // check if the hex literal is legal
        // 2 legal forms of hex literal:
        //   1) [0-9a-fA-F]+    Compiles to an ImplicitHexI
        //   2) [0-9a-fA-F]+n   Compiles to a BigHexLit
        // (2) will enter this if block.
        if chars.last() == Some(&'n') {
            let (_n, bigint_chars) = chars.split_last().unwrap();
            if bigint_chars.len() == 0 { return Err(LexerErrCases::BigIntLiteralNoBody) }
            if !only_hex_chars(bigint_chars)  { return Err(LexerErrCases::BigIntLiteralBodyInvalid) }
            let bigint_unparsed_string = remove_leading_0s(bigint_chars).iter().collect::<String>();
            return Ok(NumericLiteral::BigHexLit(bigint_unparsed_string));
        }

        if !only_hex_chars(chars) { return Err(LexerErrCases::HexIntegerLiteralInvalidChar) }
        return Ok(NumericLiteral::ImplicitHexI(remove_leading_0s(chars).into_iter().collect()))
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    OpenParen,                 //  (
    CloseParen,                //  )
    OpenBracket,               //  [
    CloseBracket,              //  ]
    OpenBrace,                 //  {
    CloseBrace,                //  }
    Colon,                     //  :
    ColonColon,                //  ::
    Bang,                      //  !
    Question,                  //  ?
    Comma,                     //  ,
    Slash,                     //  /
    Star,                      //  *
    StarStar,                  //  **
    Dash,                      //  -
    DashEq,                    //  -=
    Plus,                      //  +
    PlusEq,                    //  +=
    Percent,                   //  %
    Dot,                       //  .
    DotDot,                    //  ..
    Semicolon,                 //  ;
    Amp,                       //  &
    AmpAmp,                    //  &&
    Pipe,                      //  |
    PipePipe,                  //  ||
    Arrow,                     //  ->
    FatArrow,                  //  =>
    Eq,                        //  =
    EqEq,                      //  ==
    BangEq,                    //  !=
    Gt,                        //  >
    Lt,                        //  <
    GtEq,                      //  >=
    LtEq,                      //  <=
    Hash,                      //  #
                               //
    Ident(String),             //  any valid identifier
                               //
    StrInterpol(               //  "a ${expr} b"  =>  StrInterpol("a ", 0, Start) + Expr + StrInterpol(" b", 0, End)
        String,                //
        usize,                 //  usize documents the level of interpolation
        StrInterpolKind,       //
    ),                         //
    StrLit(String),            //  "string"
                               //
    Numeric(NumericLiteral),   //  0 0e0 0.0 0i32
                               //
    Keyword(Keyword),          //  if let var else class pub pri return continue yield ......
    Eof,                       //  Eof
}

#[derive(Debug, PartialEq)]
enum StrInterpolKind {
    Start,
    Cont,
    End,
}

// Line/Block Comments:
// It's the lexer's job to discard any line comments and block comments
// Also the Shebang at line 1
// So comments are not considered Tokens

#[derive(Debug)]
pub struct LexingWorktable {
    program: Vec<char>,
    index: usize,
    cur_line: usize,
    cur_col: usize,
    str_interpolation_depth: usize,
    cur_interpol_open_braces: [usize; MAX_INTERPOLATION_NESTING],
}

impl LexingWorktable {
    pub fn new(program: Vec<char>) -> LexingWorktable {
        LexingWorktable {
            program,
            index: 0,
            cur_line: START_LINE,
            cur_col: START_COL,
            str_interpolation_depth: 0,
            cur_interpol_open_braces: [0; MAX_INTERPOLATION_NESTING],
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
    fn skip_block_comment(&mut self) -> Result<(), LexerErrMsg> {
        let mut nesting: usize = 1;
        while nesting > 0 {
            if self.peek() == None {
                return Err(LexerErrMsg::new(&self, LexerErrCases::UnclosedBlockComment));
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
        Ok(())
    }
    fn skip_whitespace(&mut self) {
        let mut char = self.peek();
        while char == Some(' ') || char == Some('\n') || char == Some('\r') || char == Some('\t') {
            self.advance();
            char = self.peek()
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

    // Numerical Literal Rules:
    //   1) Integer literal: NO decimal point, NO scientific notation
    //   2) Float literal: MUST EITHER decimal point OR scientific notation
    //   3) With hex literals, scientific notation goes out the window.
    //      n.b. Scientific = must be float!
    //   4) Hex literals can have decimal point, but cannot start with dot.
    //   5) Hex literals cannot use number type suffix. (NO 0x0i64 or 0x0f64, vs 40i64 7.25f64)
    //   6) Scientific notation can have decimal point.
    // In conclusion, the following cases must be dealt with:
    //   1) 0xBBBBBB      Plain integer
    //   2) 0xBBB.BB      Plain float
    // Negative sign need not be considered, it's a token that's fed to the AST parser.
    fn consume_possible_numeric_literal(&mut self, first_char: char) -> Result<Token, LexerErrMsg> {
        let mut chars = vec![first_char];
        while let Some(char) = self.peek() {
            if !char_can_cont_num(char) {
                if char == '-' && (chars.last() == Some(&'e') || chars.last() == Some(&'E')) {
                    // this is a gotcha case for e.g. "1e-35", when in scientific notation
                    // the "e-" sequence should be considered part of the num literal,
                    // so do not break if this is the case.
                    // this block intentionally empty
                }
                else {
                    break
                }
            }
            chars.push(char);
            self.advance();
            if chars.len() > MAX_NUM_LITERAL_LENGTH {
                return Err(LexerErrMsg::new(&self, LexerErrCases::NumLiteralOverMaxLength))
            }
        }
        let num_lit = NumericLiteral::from_chars(chars);
        match num_lit {
            Ok(num_lit) => Ok(Token::Numeric(num_lit)),
            Err(lex_err_case) => Err(LexerErrMsg::new(&self, lex_err_case))
        }
    }

    fn consume_ident(&mut self, first_char: char) -> Result<Token, LexerErrMsg> {
        let mut chars = vec![first_char];
        while let Some(char) = self.peek() {
            if !char_can_cont_ident(char) { break }
            chars.push(char);
            self.advance();
            if chars.len() > MAX_IDENT_LENGTH {
                return Err(LexerErrMsg::new(&self, LexerErrCases::IdentOverMaxLength))
            }
        }
        let possible_ident: String = chars.into_iter().collect();
        if let Some(keyword) = Keyword::from_str(&possible_ident) {
            Ok(Token::Keyword(keyword))
        } else {
            Ok(Token::Ident(possible_ident))
        }
    }



    // IMPLEMENTING
    // IMPLEMENTING
    // IMPLEMENTING
    // IMPLEMENTING
    // IMPLEMENTING
    // IMPLEMENTING
    // IMPLEMENTING
    fn consume_str_interpol(&mut self) -> Result<Token, LexerErrMsg> { self._consume_str_literal(true) }
    fn consume_str_literal(&mut self) -> Result<Token, LexerErrMsg> { self._consume_str_literal(false) }
    fn _consume_str_literal(&mut self, is_interpol_cont: bool) -> Result<Token, LexerErrMsg> {
        let mut chars: Vec<char> = vec![];
        while let Some(char) = self.peek() {
            if char == '"' {
                self.advance();
                return if is_interpol_cont {
                    Ok(Token::StrInterpol(chars.into_iter().collect(), self.str_interpolation_depth, StrInterpolKind::End))
                }
                else { Ok(Token::StrLit(chars.into_iter().collect())) };
            }
            else if char == '\r' {  // ignore '\r' completely
                self.advance();
                continue
            }
            else if char == '\0' {  // Bob says do not advance, but why?? See wren_compiler.c:986 but I don't understand why
                self.advance();  // I don't think rust works the same as C so I advance it
                return Err(LexerErrMsg::new(&self, LexerErrCases::ZeroByteInSourceInStrLit))
            }
            else if char == '$' && self.peek_next() == Some('{') {
                if self.str_interpolation_depth >= MAX_INTERPOLATION_NESTING {
                    return Err(LexerErrMsg::new(&self, LexerErrCases::StrInterpolMaxDepthExceeded))
                }
                self.cur_interpol_open_braces[self.str_interpolation_depth] += 1;
                self.str_interpolation_depth += 1;
                self.advance();
                self.advance();
                return Ok(Token::StrInterpol(
                    chars.into_iter().collect(),
                    self.str_interpolation_depth-1,
                    if is_interpol_cont { StrInterpolKind::Cont } else { StrInterpolKind::Start }
                ));
            }
            else if char == '\\' {
                self.advance();
                let esc = self.peek();
                self.advance();
                match esc {
                    Some('"') => chars.push('"'),
                    Some('\'') => chars.push('\''),
                    Some('\\') => chars.push('\\'),
                    Some('$') => chars.push('$'),
                    Some('0') => chars.push('\0'),
                    // Some('a') => chars.push(0x07 as char),  // '\a' "Alert"
                    // Some('b') => chars.push(0x08 as char),  // '\b' "Backspace"
                    // Some('e') => chars.push(0x1B as char),  // '\e' <escape>
                    // Some('f') => chars.push(0x0C as char),  // '\f' "Form-feed page-break"
                    // !! Since rust has deprecated the above escape sequences I think bruh should too
                    Some('n') => chars.push('\n'),
                    Some('r') => chars.push('\r'),
                    Some('t') => chars.push('\t'),
                    Some('x') => chars.push(self.read_hex_escape(2)?),
                    Some('u') => chars.push(self.read_unicode_escape(4)?),
                    Some('U') => chars.push(self.read_unicode_escape(8)?),
                    Some(ch) => return Err(
                        LexerErrMsg::new(&self, LexerErrCases::InvalidEscapeChar(ch))
                    ),
                    None => return Err(LexerErrMsg::new(&self, LexerErrCases::InvalidEscapeAtEof))
                }
            }
            else {
                self.advance();
                chars.push(char)
            }
        }
        return Err(LexerErrMsg::new(&self, LexerErrCases::UnfinishedStrLiteral))
    }
    // IMPLEMENTING
    // IMPLEMENTING
    // IMPLEMENTING
    // IMPLEMENTING
    // IMPLEMENTING
    // IMPLEMENTING
    // IMPLEMENTING

    fn read_hex_escape(&mut self, len: usize) -> Result<char, LexerErrMsg> {
        unimplemented!();
        Ok(0x69 as char)
    }

    fn read_unicode_escape(&mut self, len: usize) -> Result<char, LexerErrMsg> {
        unimplemented!();
        Ok(0x69 as char)
    }
}

pub fn lexer_step(worktable: &mut LexingWorktable) -> Result<Token, LexerErrMsg> {
    while let Some(char) = worktable.consume() {
        match char {
            '(' => return Ok(Token::OpenParen),
            ')' => return Ok(Token::CloseParen),
            '[' => return Ok(Token::OpenBracket),
            ']' => return Ok(Token::CloseBracket),
            '{' => {
                if worktable.str_interpolation_depth > 0 {
                    worktable.cur_interpol_open_braces[worktable.str_interpolation_depth - 1] += 1;
                }
                else {
                    return Ok(Token::OpenBrace)
                }
            },
            '}' => {
                if worktable.str_interpolation_depth > 0 {
                    let braces_left = {
                        worktable.cur_interpol_open_braces[worktable.str_interpolation_depth - 1] -= 1;
                        worktable.cur_interpol_open_braces[worktable.str_interpolation_depth - 1]
                    };
                    if braces_left == 0 {
                        worktable.str_interpolation_depth -= 1;
                        return Ok(worktable.consume_str_interpol()?);
                    }
                }
                else {
                    return Ok(Token::CloseBrace)
                }
            },
            ':' => return Ok(worktable.resolve_2ch_token(':', Token::ColonColon, Token::Colon)),
            '!' => return Ok(worktable.resolve_2ch_token('=', Token::BangEq, Token::Bang)),
            '?' => return Ok(Token::Question),
            ',' => return Ok(Token::Comma),
            '/' => if worktable.peek() == Some('/') {
                worktable.skip_line_comment();
                continue;
            } else if worktable.peek() == Some('*') {
                worktable.skip_block_comment()?
            } else {
                return Ok(Token::Slash)
            }
            '*' => return Ok(worktable.resolve_2ch_token('*', Token::StarStar, Token::Star)),
            '-' => return if worktable.peek() == Some('>') {
                worktable.advance();
                Ok(Token::Arrow)
            } else {
                Ok(worktable.resolve_2ch_token('=', Token::DashEq, Token::Dash))
            },
            '+' => return Ok(worktable.resolve_2ch_token('=', Token::PlusEq, Token::Plus)),
            '%' => return Ok(Token::Percent),
            '.' => return Ok(worktable.resolve_2ch_token('.', Token::DotDot, Token::Dot)),
            ';' => return Ok(Token::Semicolon),
            '&' => return Ok(worktable.resolve_2ch_token('&', Token::AmpAmp, Token::Amp)),
            '|' => return Ok(worktable.resolve_2ch_token('|', Token::PipePipe, Token::Pipe)),
            '=' => return if worktable.peek() == Some('>') {
                worktable.advance();
                Ok(Token::FatArrow)
            } else {
                Ok(worktable.resolve_2ch_token('=', Token::EqEq, Token::Eq))
            },
            '>' => return Ok(worktable.resolve_2ch_token('=', Token::GtEq, Token::Gt)),
            '<' => return Ok(worktable.resolve_2ch_token('=', Token::LtEq, Token::Lt)),
            '#' => return Ok(Token::Hash),
            ' ' | '\t' | '\n' | '\r' => worktable.skip_whitespace(),
            '"' => return Ok(worktable.consume_str_literal()?),
            _ => {
                if char_can_start_ident(char) {
                    return Ok(worktable.consume_ident(char)?);
                }
                else if char_can_start_num(char) {
                    return Ok(worktable.consume_possible_numeric_literal(char)?)
                }

            }
        }
    }

    return Ok(Token::Eof)
}

pub fn lexer(worktable: &mut LexingWorktable) -> (Vec<Token>, Vec<LexerErrMsg>) {
    let mut tokens: Vec<Token> = vec![];
    let mut errors: Vec<LexerErrMsg> = vec![];
    while let result = lexer_step(worktable) {
        if let Ok(Token::Eof) = result {
           tokens.push(Token::Eof);
           break
        }
        match result {
            Ok(tok) => tokens.push(tok),
            Err(err) => errors.push(err),
        }
    }
    (tokens, errors)
}

#[cfg(test)]
mod tests {

    use super::{lexer, Keyword, LexingWorktable, Token, last_2_are, last_3_are, last_4_are, LexerErrCases, LexerErrMsg, NumericLiteral, remove_leading_0s};

    fn wtb(str: &str) -> LexingWorktable {
        return LexingWorktable::new(str.chars().collect())
    }

    #[test]
    fn meta() {
        let (results, err) = lexer(&mut wtb("if for var while in impl super mod pub crate loop"));
        println!("{:?}", results)


    }

    #[test]
    fn misc() {
        assert_eq!("1123", remove_leading_0s(&("001123".chars().collect::<Vec<char>>())).into_iter().collect::<String>());
        assert_eq!("0", remove_leading_0s(&("000000000".chars().collect::<Vec<char>>())).into_iter().collect::<String>());
        assert_eq!("0", remove_leading_0s(&("0".chars().collect::<Vec<char>>())).into_iter().collect::<String>());
        assert_eq!("1", remove_leading_0s(&("1".chars().collect::<Vec<char>>())).into_iter().collect::<String>());
        assert_eq!("1", remove_leading_0s(&("01".chars().collect::<Vec<char>>())).into_iter().collect::<String>());
        assert_eq!("", remove_leading_0s(&("".chars().collect::<Vec<char>>())).into_iter().collect::<String>());
        assert_eq!(['2'], last_2_are(&['2', '1', '2'], ['1', '2']).unwrap());
        assert_eq!(['4', '7', '8', '3', '5'], last_2_are(&['4', '7', '8', '3', '5', 'u', '8'], ['u', '8']).unwrap());
        assert_eq!([] as [char; 0], last_3_are(&['2', '1', '2'], ['2', '1', '2']).unwrap());
        assert_eq!(['2'], last_4_are(&['2', '1', '2', '5', '4'], ['1', '2', '5', '4']).unwrap());
    }

    #[test]
    fn lexing_worktable_basic_operations() {
        let mut worktable = wtb("program\n");
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
        let mut worktable = wtb(
            "(-===+= . ident..ident2 ... /* comments /* nested */ */....for)"
        );
        let (results, errs) = lexer(&mut worktable);
        assert_eq!(errs.len(), 0);
        assert_eq!(results.len(), 15);
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
        assert_eq!(results[14], Token::Eof);

        let mut worktable = LexingWorktable::new(
            "let fn mut pub mod".chars().collect()
        );
        let (results, err) = lexer(&mut worktable);
        assert_eq!(errs.len(), 0);
        assert_eq!(results[0], Token::Keyword(Keyword::Let));
        assert_eq!(results[1], Token::Keyword(Keyword::Fn));
        assert_eq!(results[2], Token::Keyword(Keyword::Mut));
        assert_eq!(results[3], Token::Keyword(Keyword::Pub));
        assert_eq!(results[4], Token::Keyword(Keyword::Mod));

        let mut worktable = LexingWorktable::new(
            "fn add(x: i64, y: i64) -> i64 { x + y } ;;\nlet id = x: i64 -> i64 => x "
                .chars().collect()
        );
        let (results, errs) = lexer(&mut worktable);
        let mut i: usize = 0;
        let mut ip = || { i+=1 ; i-1 };
        assert_eq!(errs.len(), 0);
        assert_eq!(results.len(), 31);
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

    #[test]
    fn lexer_numeric_literals_1() {
        let mut worktable = wtb(
            "let x: i64 = 488285i64-535e-99 0x4294967296 4294967296"  // something random
        );
        let (results, errs) = lexer(&mut worktable);
        assert_eq!(errs.len(), 0);
        assert_eq!(results.len(), 11);
        assert_eq!(results[0], Token::Keyword(Keyword::Let));
        assert_eq!(results[1], Token::Ident("x".into()));
        assert_eq!(results[2], Token::Colon);
        assert_eq!(results[3], Token::Ident("i64".into()));
        assert_eq!(results[4], Token::Eq);
        assert_eq!(results[5], Token::Numeric(NumericLiteral::I64Lit(488285)));
        assert_eq!(results[6], Token::Dash);
        assert_eq!(results[7], Token::Numeric(NumericLiteral::ImplicitDecF(535e-99)));
        assert_eq!(results[8], Token::Numeric(NumericLiteral::ImplicitHexI("4294967296".into())));
        assert_eq!(results[9], Token::Numeric(NumericLiteral::ImplicitDecI("4294967296".into())));
        assert_eq!(results[10], Token::Eof);

        let mut worktable = wtb(
            "5783628 0 483.535 553.0 0.0 435e3 30e0 0x425 0x0 0x000000"  // every int type (1/4)
        );
        let (results, errs) = lexer(&mut worktable);
        assert_eq!(errs.len(), 0);
        assert_eq!(results.len(), 11);
        assert_eq!(results[0], Token::Numeric(NumericLiteral::ImplicitDecI("5783628".into())));
        assert_eq!(results[1], Token::Numeric(NumericLiteral::ImplicitDecI("0".into())));
        assert_eq!(results[2], Token::Numeric(NumericLiteral::ImplicitDecF(483.535)));
        assert_eq!(results[3], Token::Numeric(NumericLiteral::ImplicitDecF(553.0)));
        assert_eq!(results[4], Token::Numeric(NumericLiteral::ImplicitDecF(0.0)));
        assert_eq!(results[5], Token::Numeric(NumericLiteral::ImplicitDecF(435e3)));
        assert_eq!(results[6], Token::Numeric(NumericLiteral::ImplicitDecF(30e0)));
        assert_eq!(results[7], Token::Numeric(NumericLiteral::ImplicitHexI("425".into())));
        assert_eq!(results[8], Token::Numeric(NumericLiteral::ImplicitHexI("0".into())));
        assert_eq!(results[9], Token::Numeric(NumericLiteral::ImplicitHexI("0".into())));
        assert_eq!(results[10], Token::Eof);

        let mut worktable = wtb(
            "477924i128 0i128 i128 179074592501695641056747015895i128 48924757387452198439824u128 \
            0u128 3749257i64 0i64 47837585325u64 0u64"  // every int type (2/4)
        );
        let (results, errs) = lexer(&mut worktable);
        assert_eq!(errs.len(), 0);
        assert_eq!(results.len(), 11);
        assert_eq!(results[0], Token::Numeric(NumericLiteral::I128Lit(477924)));
        assert_eq!(results[1], Token::Numeric(NumericLiteral::I128Lit(0)));
        assert_eq!(results[2], Token::Ident("i128".into()));
        assert_eq!(results[3], Token::Numeric(NumericLiteral::I128Lit(179074592501695641056747015895)));
        assert_eq!(results[4], Token::Numeric(NumericLiteral::U128Lit(48924757387452198439824)));
        assert_eq!(results[5], Token::Numeric(NumericLiteral::U128Lit(0)));
        assert_eq!(results[6], Token::Numeric(NumericLiteral::I64Lit(3749257)));
        assert_eq!(results[7], Token::Numeric(NumericLiteral::I64Lit(0)));
        assert_eq!(results[8], Token::Numeric(NumericLiteral::U64Lit(47837585325)));
        assert_eq!(results[9], Token::Numeric(NumericLiteral::U64Lit(0)));
        assert_eq!(results[10], Token::Eof);

        let mut worktable = wtb(
            "0i32 59753989i32 0_323_994i32 0u32 48284934u32 4294967295u32"  // every int type (3/4)
        );
        let (results, errs) = lexer(&mut worktable);
        assert_eq!(errs.len(), 0);
        assert_eq!(results.len(), 7);
        assert_eq!(results[0], Token::Numeric(NumericLiteral::I32Lit(0)));
        assert_eq!(results[1], Token::Numeric(NumericLiteral::I32Lit(59753989)));
        assert_eq!(results[2], Token::Numeric(NumericLiteral::I32Lit(0_323_994)));
        assert_eq!(results[3], Token::Numeric(NumericLiteral::U32Lit(0)));
        assert_eq!(results[4], Token::Numeric(NumericLiteral::U32Lit(48284934)));
        assert_eq!(results[5], Token::Numeric(NumericLiteral::U32Lit(4294967295)));
        assert_eq!(results[6], Token::Eof);

        let mut worktable = wtb(
            "0i8 127i8 0u8 255u8 0n 2048n 9999999999999999999999999999999999999999999999999999999999999999999999999999n \
            0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFn"  // every int type (4/4)
        );
        let (results, errs) = lexer(&mut worktable);
        assert_eq!(errs.len(), 0);
        assert_eq!(results.len(), 9);
        assert_eq!(results[0], Token::Numeric(NumericLiteral::I8Lit(0)));
        assert_eq!(results[1], Token::Numeric(NumericLiteral::I8Lit(127)));
        assert_eq!(results[2], Token::Numeric(NumericLiteral::U8Lit(0)));
        assert_eq!(results[3], Token::Numeric(NumericLiteral::U8Lit(255)));
        assert_eq!(results[4], Token::Numeric(NumericLiteral::BigDecLit("0".into())));
        assert_eq!(results[5], Token::Numeric(NumericLiteral::BigDecLit("2048".into())));
        assert_eq!(results[6], Token::Numeric(NumericLiteral::BigDecLit("9999999999999999999999999999999999999999999999999999999999999999999999999999".into())));
        assert_eq!(results[7], Token::Numeric(NumericLiteral::BigHexLit("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF".into())));
        assert_eq!(results[8], Token::Eof);

        let mut worktable = wtb(
            "128i8 2147483648i32"  // some edge cases
        );
        let (results, errs) = lexer(&mut worktable);
        assert_eq!(errs.len(), 0);
        assert_eq!(results.len(), 3);
        assert_eq!(results[0], Token::Numeric(NumericLiteral::I8Lit(128)));
        assert_eq!(results[1], Token::Numeric(NumericLiteral::I32Lit(2147483648)));
        assert_eq!(results[2], Token::Eof);

    }

    #[test]
    fn lexer_numeric_literals_2_should_err() {
        // double underscore
        let results = &lexer(&mut wtb("0__00")).1[0];
        // println!("{:?}", results);
        assert_eq!(results.desc, LexerErrCases::NumLiteralContinuousUnderscore);
        let results = &lexer(&mut wtb("0x0__00")).1[0];
        assert_eq!(results.desc, LexerErrCases::NumLiteralContinuousUnderscore);
        let results = &lexer(&mut wtb("0000__i128")).1[0];
        println!("{:?}", results);
        assert_eq!(results.desc, LexerErrCases::NumLiteralContinuousUnderscore);

        // tailing underscore
        let results = &lexer(&mut wtb("0000__")).1[0];
        assert_eq!(results.desc, LexerErrCases::NumLiteralTrailingUnderscore);
        let results = &lexer(&mut wtb("0000_")).1[0];
        assert_eq!(results.desc, LexerErrCases::NumLiteralTrailingUnderscore);
        let results = &lexer(&mut wtb("0000_i128")).0[0];  // except this trailing case is allowed
        assert_eq!(results, &Token::Numeric(NumericLiteral::I128Lit(0)));

        // wacky letters in literal
        let results = &lexer(&mut wtb("\n\n\n\n999m99"));
        println!("{:?}", results);

        // multiple syntax errors
        let results = &lexer(&mut wtb("0000__ 0xxxx 000000r"));
        println!("{:?}", results);
    }

    #[test]
    fn str_interpol() {
        // simple interpolation
        let results = lexer(&mut wtb(r###"
            "the red ${ fox("lol", 0x77457, "fox! ${ interpol }") } jumped over the ${ "graphite" } dog"
        "###));
        println!("{:?}", results);
        // assert_eq!();

    }

}
