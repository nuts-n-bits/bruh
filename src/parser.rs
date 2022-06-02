#![allow(dead_code, redundant_semicolons)]

use crate::lexer::Token;
use crate::lexer::TokenCore;
use crate::lexer::Keyword;
use crate::lexer::NumericLiteral;
use std::cell::Cell;
use std::num::ParseIntError;
use std::string::ParseError;
use ast::{*};

fn pg() {

    let clo = |i: i32, j: i32| -> i64 { i as i64 * j as i64 };
}

pub mod ast {
    use crate::lexer::NumericLiteral;

    #[derive(Debug)]
    pub struct NumericLiteralExpr<'a> {
        pub num_lit: &'a NumericLiteral,
        pub neg: bool,
    }

    #[derive(Debug)]
    pub struct FnDecl<'a> {
        pub fn_ident: &'a str,
        pub return_type: Type<'a>,
        pub visibility: Vis,
        pub param_list: Vec<(&'a str, Type<'a>)>,
        pub body: Vec<FnBodyStuff<'a>>,
    }

    #[derive(Debug)]
    pub enum FnBodyStuff<'a> {
        FnDecl(FnDecl<'a>),
        Struct,
        Mod,
        Enum,
    }

    #[derive(Debug)]
    pub enum Type<'a> {
        Unit,
        Never,
        Unknown,
        Named { type_name: &'a str, params: Vec<Type<'a>> },
        LiteralStr(&'a str),
        LiteralInt(i64),
        Bracket(Box<Type<'a>>),
        Tuple(Vec<Type<'a>>),
    }

    impl<'a> Type<'a> {
        pub fn from_vec(mut vec: Vec<Type<'a>>) -> Self {
            if vec.len() == 0 {
                Type::Unit
            }
            else if vec.len() == 1 {  // unwraps if the 'tuple' is exactly 1 in length
                let only_type = vec.remove(0);
                match only_type {
                    Type::Never | Type::Unknown | Type::Unit |
                    Type::Named { .. } | Type::Bracket(..) |
                    Type::LiteralInt(..) | Type::LiteralStr(..) => only_type,
                    Type::Tuple(vec) => Type::from_vec(vec)
                }
            }
            else {
                Type::Tuple(vec)
            }
        }
    }

    #[derive(Debug)]
    pub struct Mod<'a> {
        pub visibility: Vis,
        pub ident: &'a str,
        pub stuff: Vec<TopLevelStuff<'a>>,
    }

    #[derive(Debug)]
    pub struct ModPtr<'a> {
        pub visibility: Vis,
        pub mod_name: &'a str,
    }

    #[derive(Debug)]
    pub struct Enum<'a> {
        pub visibility: Vis,
        pub ident: &'a str,
        pub body: Vec<EnumVariant<'a>>,
    }

    #[derive(Debug)]
    pub enum EnumVariant<'a> {
        BareIdent(&'a str),
        WithAssoc(&'a str, Vec<Type<'a>>)
    }

    #[derive(Debug)]
    pub enum Vis {
        Pub,
        Private,
        PubCrate,
        PubSuper,
        PubIn(Path),
    }

    #[derive(Debug)]
    pub struct Path;

    #[derive(Debug)]
    pub enum TopLevelStuff<'a> {
        FnDecl(FnDecl<'a>),
        Mod(Mod<'a>),
        ModPtr(ModPtr<'a>),
        Enum(Enum<'a>),
        Struct(Struct<'a>),
        Impl(Impl<'a>),
    }

    #[derive(Debug)]
    pub enum Struct<'a> {
        Tagged(Vec<(&'a str, Type<'a>)>),
        Untagged(Vec<Type<'a>>),
        Unit,
    }

    #[derive(Debug)]
    pub struct Impl<'a> (pub &'a str);

    #[derive(Debug)]
    pub struct File<'a> (pub Vec<TopLevelStuff<'a>>);
}

#[derive(Debug, Clone, Copy)]
pub enum NowParsingWhat {
    Enum,
    Fn,
    FnParamList,
    FnBody,
    Type,
    Mod,
    File,
}

#[derive(Debug)]
pub enum ParserError<'a> {
    // (Error case)                                 (NowParsingWhat, Offending token)
    OpenAngleBracketExpected                        (NowParsingWhat, &'a Token),
    ArrowExpected                                   (NowParsingWhat, &'a Token),
    DashExpected                                    (NowParsingWhat, &'a Token),
    OpenParenExpected                               (NowParsingWhat, &'a Token),
    CloseParenExpected                              (NowParsingWhat, &'a Token),
    OpenCurlyExpected                               (NowParsingWhat, &'a Token),
    CloseCurlyExpected                              (NowParsingWhat, &'a Token),
    OpenBracketExpected                             (NowParsingWhat, &'a Token),
    CloseBracketExpected                            (NowParsingWhat, &'a Token),
    IdentExpected                                   (NowParsingWhat, &'a Token),
    KeywordExpected                                 (NowParsingWhat, &'a Token),
    CommaExpected                                   (NowParsingWhat, &'a Token),
    Keyword1ExpectedGotKeyword2                     (NowParsingWhat, Keyword, &'a Keyword),
    VisFineGrainModifierExpected                    (NowParsingWhat, &'a Token),
    ColonExpected                                   (NowParsingWhat, &'a Token),
    BangExpected                                    (NowParsingWhat, &'a Token),
    QuestionExpected                                (NowParsingWhat, &'a Token),
    StrLitExpected                                  (NowParsingWhat, &'a Token),
    TopLevelItemExpected                            (NowParsingWhat, &'a Token),
    TopLevelItemOrEofExpected                       (NowParsingWhat, &'a Token),
    TopLevelItemOrCloseCurlyExpected                (NowParsingWhat, &'a Token),
    NumericLiteralTypeMustBeI64                     (NowParsingWhat, &'a NumericLiteral),
    NumericLiteralExpected                          (NowParsingWhat, &'a Token),

    ParseIntError                                   (ParseIntError),

    DevGeneric,

    // Fn Decl
    FnDeclParamListOpenParenExpected                (&'a Token),
    FnDeclParamListCloseParenExpected               (&'a Token),
    FnDeclBodyCloseBraceExpected                    (&'a Token),
    // Type
    TypeExpected                                    (&'a Token),
    TypeParamListUnexpectedToken                    (&'a Token),
    TypeTupleCommaOrCloseParenExpected              (&'a Token),
    TypeCloseBracketExpected                        (&'a Token),
    // Mod
    ModUnexpectedEof                                (&'a Token),
    ModUnexpectedCloseBrace                         (&'a Token),
    ModBodyOpenBraceExpected                        (&'a Token),
    // Eof
    UnexpectedEof                                              ,
    UnexpectedBeginOfFile                                      ,
}

#[derive(Debug)]
pub struct ParsingWorktable<'a> {
    tokens: &'a [Token],
    index: Cell<usize>,
    now_parsing: Cell<NowParsingWhat>,
}

// Most basic primitive helper operations, not directly related to parsing
impl ParsingWorktable<'_> {
    pub fn new(tokens: &[Token]) -> ParsingWorktable {
        ParsingWorktable { tokens: &tokens, index: Cell::new(0), now_parsing: Cell::new(NowParsingWhat::File) }
    }
    fn peek(&self) -> &Token {
        &self.tokens[self.index.get()]
    }
    fn peek_x(&self, offset: usize) -> Result<&Token, ParserError> {
        let wanted = self.index.get() + offset;
        if wanted >= self.tokens.len() { Err(ParserError::UnexpectedEof) }
        else { Ok(&self.tokens[wanted]) }
    }
    fn peek_back_x(&self, offset: usize) -> Result<&Token, ParserError> {
        if offset > self.index.get() { Err(ParserError::UnexpectedBeginOfFile) }
        else { Ok(&self.tokens[self.index.get() - offset]) }
    }
    fn get_return_point(&self) -> usize {
        self.index.get()
    }
    fn return_to_point(&self, return_point: usize) {
        self.index.set(return_point)
    }
    fn advance(&self) {
        if self.index.get() < self.tokens.len() { self.index.set(self.index.get() + 1) }
    }
    fn un_advance(&self) {
        if self.index.get() > 0 { self.index.set(self.index.get() - 1) }
    }
    fn consume(&self) -> Result<&Token, ParserError> {
        if self.index.get() >= self.tokens.len() { return Err(ParserError::UnexpectedEof) }
        let ret = &self.tokens[self.index.get()];
        self.index.set(self.index.get() + 1);
        Ok(ret)
    }
    fn try_it<'a, T>(&self, closure: impl Fn() -> Result<T, ParserError<'a>>) -> Result<T, ParserError<'a>> {
        let return_point = self.get_return_point();
        let now_parsing = self.now_parsing_what();
        match closure() {
            Ok(t) => Ok(t),
            Err(e) => {
                self.return_to_point(return_point);
                self.now_parsing.set(now_parsing);
                Err(e)
            }
        }
    }
    fn now_parsing_what(&self) -> NowParsingWhat {
        self.now_parsing.get()
    }
}

// Try consume one very simple Token
impl ParsingWorktable<'_> {
    fn try_consume_comma(&self) -> Result<(), ParserError> {
        return match self.peek() {
            Token { core: TokenCore::Comma, .. } => { self.advance() ;; Ok(()) },
            token => Err(ParserError::CommaExpected(self.now_parsing_what(), token)),
        }
    }
    fn try_consume_open_paren(&self) -> Result<(), ParserError> {
        return match self.peek() {
            Token { core: TokenCore::OpenParen, .. } => { self.advance() ;; Ok(()) },
            token => Err(ParserError::OpenParenExpected(self.now_parsing_what(), token)),
        }
    }
    fn try_consume_close_paren(&self) -> Result<(), ParserError> {
        return match self.peek() {
            Token { core: TokenCore::CloseParen, .. } => { self.advance() ;; Ok(()) },
            token => Err(ParserError::CloseParenExpected(self.now_parsing_what(), token)),
        }
    }
    fn try_consume_open_curly(&self) -> Result<(), ParserError> {
        return match self.peek() {
            Token { core: TokenCore::OpenCurly, .. } => { self.advance() ;; Ok(()) },
            token => Err(ParserError::OpenCurlyExpected(self.now_parsing_what(), token))
        }
    }
    fn try_consume_open_bracket(&self) -> Result<(), ParserError> {
        return match self.peek() {
            Token { core: TokenCore::OpenBracket, .. } => { self.advance() ;; Ok(()) },
            token => Err(ParserError::OpenBracketExpected(self.now_parsing_what(), token)),
        }
    }
    fn try_consume_keyword(&self, keyword1: Keyword) -> Result<(), ParserError> {
        return match self.peek() {
            Token { core: TokenCore::Keyword(keyword2), .. } => {
                if &keyword1 == keyword2 { { self.advance() ;; Ok(()) } }
                else { Err(ParserError::Keyword1ExpectedGotKeyword2(self.now_parsing_what(), keyword1, keyword2)) }
            }
            token => Err(ParserError::KeywordExpected(self.now_parsing_what(), token)),
        }
    }
    fn try_consume_ident(&self) -> Result<&str, ParserError> {
        return match self.peek() {
            Token { core: TokenCore::Ident(id), .. } => { self.advance() ;; Ok(id) },
            token => Err(ParserError::IdentExpected(self.now_parsing_what(), token)),
        }
    }
    fn try_consume_close_bracket(&self) -> Result<(), ParserError> {
        return match self.peek() {
            Token { core: TokenCore::CloseBracket, .. } => { self.advance() ;; Ok(()) },
            token => Err(ParserError::CloseBracketExpected(self.now_parsing_what(), token)),
        }
    }
    fn try_consume_close_curly(&self) -> Result<(), ParserError> {
        return match self.peek() {
            Token { core: TokenCore::CloseCurly, .. } => { self.advance() ;; Ok(()) },
            token => Err(ParserError::CloseCurlyExpected(self.now_parsing_what(), token)),
        }
    }
    fn try_consume_open_angle(&self) -> Result<(), ParserError> {
        return match self.peek() {
            Token { core: TokenCore::Lt, .. } => { self.advance() ;; Ok(()) }
            token => Err(ParserError::OpenAngleBracketExpected(self.now_parsing_what(), token)),
        }
    }
    fn try_consume_colon(&self) -> Result<(), ParserError> {
        return match self.peek() {
            Token { core: TokenCore::Colon, .. } => { self.advance() ;; Ok(()) }
            token => Err(ParserError::ColonExpected(self.now_parsing_what(), token)),
        }
    }
    fn try_consume_bang(&self) -> Result<(), ParserError> {
        return match self.peek() {
            Token { core: TokenCore::Bang, .. } => { self.advance() ;; Ok(()) }
            token => Err(ParserError::BangExpected(self.now_parsing_what(), token)),
        }
    }
    fn try_consume_question(&self) -> Result<(), ParserError> {
        return match self.peek() {
            Token { core: TokenCore::Question, .. } => { self.advance() ;; Ok(()) }
            token => Err(ParserError::QuestionExpected(self.now_parsing_what(), token)),
        }
    }
    fn try_consume_str_lit(&self) -> Result<&str, ParserError> {
        return match self.peek() {
            Token { core: TokenCore::StrLit(str), .. } => { self.advance() ;; Ok(str) }
            token => Err(ParserError::StrLitExpected(self.now_parsing_what(), token)),
        }
    }
    fn try_consume_num_lit(&self) -> Result<NumericLiteralExpr, ParserError> {
        self.try_it(|| self._try_consume_num_lit(false))
    }
    fn _try_consume_num_lit(&self, neg: bool) -> Result<NumericLiteralExpr, ParserError> {
        return if let Ok(num_lit) = self._try_consume_bare_bones_num_lit() {
            Ok(NumericLiteralExpr { num_lit, neg: neg })
        }
        else if let Ok(_) = self.try_consume_open_paren() {
            let num_lit_expr = self._try_consume_num_lit(neg)?;
            let _ = self.try_consume_close_paren();
            Ok(num_lit_expr)
        }
        else if let Ok(_) = self.try_consume_dash() {
            let num_lit_expr = self._try_consume_num_lit(!neg)?;
            Ok(num_lit_expr)
        }
        else {
            Err(ParserError::NumericLiteralExpected(self.now_parsing_what(), self.peek()))
        }
    }
    fn _try_consume_bare_bones_num_lit(&self) -> Result<&NumericLiteral, ParserError> {
        return match self.peek() {
            Token { core: TokenCore::Numeric(num_lit), .. } => { self.advance() ;; Ok(num_lit) },
            token => Err(ParserError::NumericLiteralExpected(self.now_parsing_what(), token)),
        }
    }
    fn try_consume_arrow(&self) -> Result<(), ParserError> {
        return match self.peek() {
            Token { core: TokenCore::Arrow, .. } => { self.advance() ;; Ok(()) }
            token => Err(ParserError::ArrowExpected(self.now_parsing_what(), token)),
        }
    }
    fn try_consume_dash(&self) -> Result<(), ParserError> {
        return match self.peek() {
            Token { core: TokenCore::Dash, .. } => { self.advance() ;; Ok(()) }
            token => Err(ParserError::DashExpected(self.now_parsing_what(), token)),
        }
    }
}

// Try consume basic patterns: AngleParamList<Type, Type, ...>
impl ParsingWorktable<'_> {
    // fn try_consume_angle_bracket_type_list(&self, end_with: TokenCore) -> Result<Vec<Type>, ParserError> {
    //     let _open_angle_bracket = self.try_consume_open_angle(Type)?;
    //     let mut type_list: Vec<Type> = vec![];
    //     loop {
    //         type_list.push(self.try_consume_type()?);
    //         match self.peek() {
    //             Ok(Token { core: TokenCore::Comma, .. }) => { self.advance() ;; continue },
    //             Ok(Token { core: TokenCore::Gt, .. }) => { self.advance() ;; return Ok(type_list) },
    //             token => return Err(ParserError::TypeParamListUnexpectedToken(token?)),
    //         }
    //     }
    // }
}

// Try consume some fragments: pub (visibility modifier) and type.
impl ParsingWorktable<'_> {

    fn try_consume_path(&self) -> Result<Path, ParserError> {
        todo!();
    }
    fn _consume_path(&self) -> Result<Path, ParserError> {
        todo!();
    }

    fn try_consume_visibility_modifier(&self) -> Result<Vis, ParserError> {
        self.try_it(|| { self._consume_visibility_modifier() })
    }
    fn _consume_visibility_modifier(&self) -> Result<Vis, ParserError> {
        // if no pub keyword, then it's simply private by default
        if let Err(_no_pub_keyword) = self.try_consume_keyword(Keyword::Pub) { return Ok(Vis::Private) }
        // pub, but no open paren -> regular pub. with open paren -> modified pub
        if let Err(_no_open_paren) = self.try_consume_open_paren() { return Ok(Vis::Pub) }
        // OpenParen exists!
        //    |-- pub(self)
        //    |-- pub(crate)
        //    |-- pub(super)
        //    +-- pub(in ...)
        return if let Ok(_keyword_crate) = self.try_consume_keyword(Keyword::Crate) {
            let _close_paren = self.try_consume_close_paren()?;
            Ok(Vis::PubCrate)
        }
        else if let Ok(_keyword_super) = self.try_consume_keyword(Keyword::Super) {
            let _close_paren = self.try_consume_close_paren()?;
            Ok(Vis::PubSuper)
        }
        else if let Ok(_keyword_in) = self.try_consume_keyword(Keyword::In) {
            let path = self.try_consume_path()?;
            let _close_paren = self.try_consume_close_paren()?;
            Ok(Vis::PubIn(path))
        }
        else if let Ok(_keyword_self) = self.try_consume_keyword(Keyword::Self_) {
            let _close_paren = self.try_consume_close_paren()?;
            Ok(Vis::Private)
        }
        else {
            Err(ParserError::VisFineGrainModifierExpected(self.now_parsing_what(), self.peek()))
        }
    }

    fn try_consume_type(&self) -> Result<Type, ParserError> {
        self.try_it(|| { self._consume_type() })
    }

    fn _consume_type(&self) -> Result<Type, ParserError> {

        if let Ok(bracket_type) = self.try_consume_bracket_type() { return Ok(bracket_type) }
        else if let Ok(named_type) = self.try_consume_named_type() { return Ok(named_type) }
        else if let Ok(_) = self.try_consume_bang() { return Ok(Type::Never) }
        else if let Ok(_) = self.try_consume_question() { return Ok(Type::Unknown) }
        else if let Ok(str_lit) = self.try_consume_str_lit() { return Ok(Type::LiteralStr(str_lit)) }
        else if let Ok(num_lit) = self.try_consume_num_lit() {
            return match num_lit {
                NumericLiteralExpr { num_lit: NumericLiteral::ImplicitDecI(ref int_str), neg } => Ok(Type::LiteralInt(i64::from_str_radix(int_str, 10)?)),
                NumericLiteralExpr { num_lit: NumericLiteral::ImplicitHexI(ref int_hex), neg } => Ok(Type::LiteralInt(i64::from_str_radix(int_hex, 16)?)),
                NumericLiteralExpr { num_lit: NumericLiteral::I64Lit(ref i64_str), neg } => Ok(Type::LiteralInt(i64::from_str_radix(i64_str, 10)?)),
                NumericLiteralExpr { num_lit, .. } => Err(ParserError::NumericLiteralTypeMustBeI64(self.now_parsing_what(), num_lit) )
            }
        }
        else { return Err(ParserError::DevGeneric) } // TODO delete dev generic


        match self.peek() {
            Token { core: TokenCore::OpenParen, .. } => self.advance(),  // () or (Type) or (Type, Type, ...)
            Token { core: TokenCore::StrLit(str), .. } => { self.advance() ;; return Ok(Type::LiteralStr(str)) },
            Token { core: TokenCore::Numeric(NumericLiteral::ImplicitDecI(str)), .. } => { self.advance() ;; return Ok(Type::LiteralInt(i64::from_str_radix(str, 10)?)) }
            Token { core: TokenCore::Numeric(NumericLiteral::ImplicitHexI(str)), .. } => { self.advance() ;; return Ok(Type::LiteralInt(i64::from_str_radix(str, 16)?)) }
            token => return Err(ParserError::TypeExpected(token))
        };  // if seen open paren, continue, expect (), (Type), (Type, Type, ...)
        let mut tuple_list: Vec<Type> = vec![];
        let _unreachable = loop {
            tuple_list.push(self.try_consume_type()?);
            match self.peek() {
                Token { core: TokenCore::Comma, .. } => self.advance(),
                Token { core: TokenCore::CloseParen, .. } => { self.advance() ;; return Ok(Type::from_vec(tuple_list)); },
                token => return Err(ParserError::TypeTupleCommaOrCloseParenExpected(token)),
            }
        };
        impl From<ParseIntError> for ParserError<'_> {
            fn from(parse_int_error: ParseIntError) -> Self {
                ParserError::ParseIntError(parse_int_error)
            }
        }
    }

    fn try_consume_bracket_type(&self) -> Result<Type, ParserError> {
        self.try_it(|| { self._consume_bracket_type() })
    }
    fn _consume_bracket_type(&self) -> Result<Type, ParserError> {
        let _ = self.try_consume_open_bracket()?;
        let type_within = self.try_consume_type()?;
        let _ = self.try_consume_close_bracket()?;
        return Ok(Type::Bracket(Box::new(type_within)))
    }

    fn try_consume_named_type(& self) -> Result<Type, ParserError> {
        self.try_it(|| { self._consume_named_type() })
    }
    fn _consume_named_type(&self) -> Result<Type, ParserError> {
        let type_ident = self.try_consume_ident()?;
        if let Err(_) = self.try_consume_open_angle() {
            // if no angle bracket follows the ident, this is a 0-param type
            return Ok(Type::Named { type_name: type_ident, params: vec![] })
        }
        // angle bracket follows, this is a param list, each term being a Type, until a '>'
        let mut type_param_list: Vec<Type> = vec![];
        loop {
            type_param_list.push(self.try_consume_type()?);
            match self.peek() {
                Token { core: TokenCore::Comma, .. } => { self.advance() ;; continue },
                Token { core: TokenCore::Gt, .. } => { self.advance() ;; return Ok(Type::Named { type_name: type_ident, params: type_param_list }) },
                token => return Err(ParserError::TypeParamListUnexpectedToken(token)),
            };
        }
    }
}

// try consume whole items: Fn, Struct, Enum
impl ParsingWorktable<'_> {
    fn try_consume_fn_decl(&self) -> Result<FnDecl, ParserError> {
        self.try_it(|| { self._consume_fn_decl() })
    }
    fn _consume_fn_decl(&self) -> Result<FnDecl, ParserError> {
        self.now_parsing.set(NowParsingWhat::Fn);
        let visibility = self.try_consume_visibility_modifier()?;
        let _ = self.try_consume_keyword(Keyword::Fn)?;
        let fn_ident = self.try_consume_ident()?;
        self.now_parsing.set(NowParsingWhat::FnParamList);
        let _ = self.try_consume_open_paren()?;
        let mut param_list: Vec<(&str, Type)> = vec![];
        'expect_param_ident: loop {
            let ident = match self.try_consume_ident() {
                Err(_) => break 'expect_param_ident,
                Ok(ident) => ident,
            };
            let colon = self.try_consume_colon()?;
            let type_ = self.try_consume_type()?;
            param_list.push((ident, type_));
            let maybe_comma = match self.try_consume_comma() {
                Err(_) => break 'expect_param_ident,
                Ok(comma) => comma,
            };
        }
        let _ = self.try_consume_close_paren();

        let return_type = if let Ok(_) = self.try_consume_arrow() { self.try_consume_type()? }
        else if let Token { core: TokenCore::OpenCurly, .. } = self.peek() { Type::Unit }
        else { return Err(ParserError::OpenCurlyExpected(self.now_parsing_what(), self.peek())) };


        let open_curly = self.try_consume_open_curly()?;
        self.now_parsing.set(NowParsingWhat::FnBody);
        let mut fn_body_items: Vec<FnBodyStuff> = vec![];

        // loop {
        //     // TODO: consume fn body
        //     // TODO: consume fn body
        //     // TODO: consume fn body
        //     todo!()
        //     // TODO: consume fn body
        //     // TODO: consume fn body
        //     // TODO: consume fn body
        //
        // }

        let close_curly = self.try_consume_close_curly()?;

        return Ok(FnDecl {
            fn_ident,
            return_type,
            visibility,
            param_list,
            body: fn_body_items,
        })
    }
}

// consume: concrete mod, file
enum EndWith { CloseBrace, Eof }
impl ParsingWorktable<'_> {
    fn try_consume_file(&self) -> Result<File, ParserError> {
        self.try_it(|| {self._consume_file() })
    }
    fn _consume_file(&self) -> Result<File, ParserError> {
        let mut top_level_vec: Vec<TopLevelStuff> = vec![];
        loop {
            if let Ok(top_level) = self.try_consume_top_level_stuff() {
                top_level_vec.push(top_level)
            }
            else if let Token { core: TokenCore::Eof, .. } = self.peek() {
                return Ok(File(top_level_vec))
            }
            else {
                return Err(ParserError::TopLevelItemOrEofExpected(self.now_parsing_what(), self.peek()))
            }
        }
    }

    fn try_consume_mod(&self) -> Result<Mod, ParserError> {
        self.try_it(|| { self._consume_mod() })
    }
    fn _consume_mod(&self) -> Result<Mod, ParserError> {
        let visibility = self.try_consume_visibility_modifier()?;
        let _mod_keyword = self.try_consume_keyword(Keyword::Mod)?;
        let mod_name = self.try_consume_ident()?;
        let _open_curly = self.try_consume_open_curly()?;
        let mut top_level_vec: Vec<TopLevelStuff> = vec![];
        loop {
            if let Ok(top_level) = self.try_consume_top_level_stuff() {
                top_level_vec.push(top_level)
            }
            else if let Ok(_close_curly) = self.try_consume_close_curly() {
                return Ok(Mod { ident: mod_name, visibility, stuff: top_level_vec })
            }
            else {
                return Err(ParserError::TopLevelItemOrEofExpected(self.now_parsing_what(), self.peek()))
            }
        }
    }
    fn try_consume_top_level_stuff(&self) -> Result<TopLevelStuff, ParserError> {
        self.try_it(|| { self._consume_top_level_stuff() })
    }
    fn _consume_top_level_stuff(&self) -> Result<TopLevelStuff, ParserError> {
        if let Ok(fn_decl) = self.try_consume_fn_decl() { Ok(TopLevelStuff::FnDecl(fn_decl)) }
        // TODO: else if let Ok(enum_) = self.try_consume_enum() { Ok(TopLevelStuff::Enum(enum_)) }
        // TODO: else if let Ok(struct_) = self.try_consume_struct() { Ok(TopLevelStuff::Struct(struct_)) }
        // else
        else if let Ok(mod_) = self.try_consume_mod() { Ok(TopLevelStuff::Mod(mod_)) }
        // TODO: else if let Ok(impl_) = self.try_consume_impl() { Ok(TopLevelStuff::Impl(impl_)) }
        else { Err(ParserError::TopLevelItemExpected(self.now_parsing_what(), self.peek())) }
    }
}

#[cfg(test)]
mod test {

    use crate::lexer::{*};
    use crate::parser::ParsingWorktable;
    use super::ast::{*};

    #[test]
    fn temp() {

        let (tokens, errs) = lexer(
            "pub fn some_fn_lol () -> (Option<(str, i64, !), Triplet<str, !, ?>>, ?) {} pub fn unit () {}"
        );
        //println!("{:?}", (&tokens, &errs));

        let mut worktable = ParsingWorktable::new(&tokens);
        let fn_decl = worktable.try_consume_file();
        println!("{:?}", fn_decl);
        //println!("{:?}", worktable);
    }

    #[test]
    fn temp2() {
        let (tokens, errs) = lexer(
            "mod outer_mod { mod inner_mod { fn inner_fn() {} }  }"
        );
        //println!("{:?}", (&tokens, &errs));

        let mut worktable = ParsingWorktable::new(&tokens);
        let fn_decl = worktable.try_consume_file();
        println!("{:?}", fn_decl);
        //println!("{:?}", worktable);
    }

    #[test]
    fn temp3() {
        let program =
            r########"

            // enum Option<T> {
            //     None,
            //     Some(T)
            // }

            // struct Point0D;
            // struct Point1D(i64);
            // struct Point2D(i64, i64);
            // struct FirstAndLast {
            //     first_name: str,
            //     last_name: str,
            // }

            fn x(
                x: i64,
                y: i64,
            )
                -> [u8]
            {

            }

            // mod outermod { mod innermod { fn innerfn() {} }  }

            // mod mod_decl;


            "########;
        let (tokens, errs) = lexer(program);
        //println!("{:?}", (&tokens, &errs));

        let mut worktable = ParsingWorktable::new(&tokens);
        let fn_decl = worktable.try_consume_file();
        dbg!(fn_decl);
    }
}