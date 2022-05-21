#![allow(dead_code, redundant_semicolons)]

use crate::lexer::Token;
use crate::lexer::TokenCore;
use crate::lexer::Keyword;
use crate::lexer::NumericLiteral;
use std::cell::Cell;
use ast::{*};
use WhilePArsingWhat::{*};
use crate::parser::WhileParsingWhat::{FnBody, FnParamList};

pub mod ast {
    use std::fmt::Formatter;

    #[derive(Debug)]
    pub struct FnDecl<'a> {
        pub fn_ident: &'a str,
        pub return_type: Type<'a>,
        pub visibility: Vis,
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
                    Type::Never | Type::Unknown | Type::Unit | Type::Named { .. } | Type::Bracket(..) => only_type,
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
    }

    #[derive(Debug)]
    pub enum TopLevelStuff<'a> {
        FnDecl(FnDecl<'a>),
        Mod(Mod<'a>),
        ModPtr(ModPtr<'a>),
    }

    #[derive(Debug)]
    pub enum StructDecl<'a> {
        Tagged(Vec<(&'a str, Type<'a>)>),
        Untagged(Vec<Type<'a>>),
        Unit,
    }
}

enum X {
    A(String),
    B = 2,
}

const m: i32 = X::A as i32;

#[derive(Debug)]
pub enum WhileParsingWhat {
    Enum,
    Fn,
    FnParamList,
    FnBody,
    Type,
    Mod,
}

#[derive(Debug)]
pub enum ParserError<'a> {
    // (Error case)                                 (WhileParsingWhat, Offending token)
    OpenAngleBracketExpected                        (WhileParsingWhat, &'a Token),
    OpenParenExpected                               (WhileParsingWhat, &'a Token),
    CloseParenExpected                              (WhileParsingWhat, &'a Token),
    OpenCurlyExpected                               (WhileParsingWhat, &'a Token),
    OpenBracketExpected                             (WhileParsingWhat, &'a Token),
    CloseBracketExpected                            (WhileParsingWhat, &'a Token),
    IdentExpected                                   (WhileParsingWhat, &'a Token),
    KeywordExpected                                 (WhileParsingWhat, &'a Token),
    Keyword1ExpectedGotKeyword2                     (WhileParsingWhat, Keyword, &'a Keyword),
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
}

// Most basic primitive helper operations, not directly related to parsing
impl ParsingWorktable<'_> {
    pub fn new(tokens: &[Token]) -> ParsingWorktable {
        ParsingWorktable { tokens: &tokens, index: Cell::new(0) }
    }
    fn peek(&self) -> Result<&Token, ParserError> {
        if self.index.get() >= self.tokens.len() { Err(ParserError::UnexpectedEof) }
        else { Ok(&self.tokens[self.index.get()]) }
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
}

// Try consume one very simple Token
impl ParsingWorktable<'_> {
    fn try_consume_open_paren(&self, while_parsing: WhileParsingWhat) -> Result<(), ParserError> {
        return match self.peek()? {
            Token { core: TokenCore::OpenParen, .. } => { self.advance() ;; Ok(()) },
            token => Err(ParserError::OpenParenExpected(while_parsing, token)),
        }
    }
    fn try_consume_open_curly(&self, while_parsing: WhileParsingWhat) -> Result<(), ParserError> {
        return match self.peek()? {
            Token { core: TokenCore::OpenCurly, .. } => { self.advance() ;; Ok(()) },
            token => Err(ParserError::OpenCurlyExpected(while_parsing, token))
        }
    }
    fn try_consume_open_bracket(&self, while_parsing: WhileParsingWhat) -> Result<(), ParserError> {
        return match self.peek()? {
            Token { core: TokenCore::OpenBracket, .. } => { self.advance() ;; Ok(()) },
            token => Err(ParserError::OpenBracketExpected(while_parsing, token)),
        }
    }
    fn try_consume_keyword(&self, keyword1: Keyword, while_parsing: WhileParsingWhat) -> Result<(), ParserError> {
        return match self.peek()? {
            Token { core: TokenCore::Keyword(keyword2), .. } => {
                if keyword1 == keyword2 { { self.advance() ;; Ok(()) } }
                else { Err(ParserError::Keyword1ExpectedGotKeyword2(while_parsing, keyword1, keyword2)) }
            }
            token => Err(ParserError::KeywordExpected(while_parsing, token)),
        }
    }
    fn try_consume_ident(&self, while_parsing: WhileParsingWhat) -> Result<&str, ParserError> {
        return match self.peek()? {
            Token { core: TokenCore::Ident(id), .. } => { self.advance() ;; Ok(id) },
            token => Err(ParserError::IdentExpected(while_parsing, token)),
        }
    }
    fn try_consume_close_bracket(&self, while_parsing: WhileParsingWhat) -> Result<(), ParserError> {
        return match self.peek()? {
            Token { core: TokenCore::CloseBracket, .. } => { self.advance() ;; Ok(()) },
            token => Err(ParserError::CloseBracketExpected(while_parsing, token)),
        }
    }
    fn try_consume_open_angle(&self, while_parsing: WhileParsingWhat) -> Result<(), ParserError> {
        return match self.peek()? {
            Token { core: TokenCore::Lt, .. } => { self.advance() ;; Ok(()) }
            token => Err(ParserError::OpenAngleBracketExpected(while_parsing, token)),
        }
    }
}

// Try consume basic patterns: AngleParamList<Type, Type, ...>
impl ParsingWorktable<'_> {
    fn try_consume_angle_bracket_type_list(&self, end_with) -> Result<Vec<Type>, ParserError> {
        let _open_angle_bracket = self.try_consume_open_angle(Type)?;
        let mut type_list: Vec<Type> = vec![];
        loop {
            type_list.push(self.try_consume_type()?);
            match self.peek() {
                Ok(Token { core: TokenCore::Comma, .. }) => { self.advance() ;; continue },
                Ok(Token { core: TokenCore::Gt, .. }) => { self.advance() ;; return Ok(type_list) },
                token => return Err(ParserError::TypeParamListUnexpectedToken(token?)),
            }
        }
    }
}

impl ParsingWorktable<'_> {

    fn try_consume_type(&self) -> Result<Type, ParserError> {
        let _open_paren = match self.peek()? {
            Token { core: TokenCore::OpenParen, .. } => self.advance(),  // () or (Type) or (Type, Type, ...)
            Token { core: TokenCore::Ident(id), .. }  => { self.advance() ;; return self.try_consume_named_type(id) }, // NamedType
            Token { core: TokenCore::OpenBracket, .. } => { self.advance() ;; return self.try_consume_bracket_type() }  // [Type]
            Token { core: TokenCore::Bang, .. } => { self.advance() ;; return Ok(Type::Never); },  // ! type is never type
            Token { core: TokenCore::Question, .. } => { self.advance() ;; return Ok(Type::Unknown); },  // ? type is unknown type
            Token { core: TokenCore::StrLit(str), .. } => { self.advance() ;; return Ok((Type::LiteralStr(str))) },
            Token { core: TokenCore::Numeric(NumericLiteral::ImplicitDecI(str)), .. } => { i64::try_from(str) }
            token => return Err(ParserError::TypeExpected(token))
        };  // if seen open paren, continue, expect (), (Type), (Type, Type, ...)
        let mut tuple_list: Vec<Type> = vec![];
        let _unreachable = loop {
            tuple_list.push(self.try_consume_type()?);
            match self.peek() {
                Ok(Token { core: TokenCore::Comma, .. }) => self.advance(),
                Ok(Token { core: TokenCore::CloseParen, .. }) => { self.advance() ;; return Ok(Type::from_vec(tuple_list)); },
                token => return Err(ParserError::TypeTupleCommaOrCloseParenExpected(token?)),
            }
        };
    }
    fn try_consume_bracket_type(&self) -> Result<Type, ParserError> {
        let type_within = self.try_consume_type()?;
        let _close_bracket = self.try_consume_close_bracket(Type)?;
        return Ok(Type::Bracket(Box::new(type_within)))
    }

    /** Only call after having de-parenthesized the type expression */
    fn try_consume_named_type<'a>(&'a self, type_ident: &'a str) -> Result<Type<'a>, ParserError<'a>> {
        if let Err(_) = self.try_consume_open_angle(Type) {
            // if no angle bracket follows the ident, this is a 0-param type
            return Ok(Type::Named { type_name: type_ident, params: vec![] })
        }
        // angle bracket follows, this is a param list, each term being a Type, until a '>'
        let mut type_param_list: Vec<Type> = vec![];
        loop {
            type_param_list.push(self.try_consume_type()?);
            match self.peek() {
                Ok(Token { core: TokenCore::Comma, .. }) => { self.advance() ;; continue },
                Ok(Token { core: TokenCore::Gt, .. }) => { self.advance() ;; return Ok(Type::Named { type_name: type_ident, params: type_param_list }) },
                token => return Err(ParserError::TypeParamListUnexpectedToken(token?)),
            };
        }
    }



    /** Consume Keyword '(pub) enum' before calling */
    fn try_consume_enum(&self, vis: Vis) -> Result<Enum, ParserError> {
        let enum_ident = self.try_consume_ident(Enum)?;
        let _open_curly = self.try_consume_open_curly(Enum);
        let mut variants: Vec<EnumVariant> = vec![];
        'consume_variant: loop {
            match self.consume()? {
                Token { core: TokenCore::CloseCurly, .. } => { break },
                Token { core: TokenCore::Ident(ident), .. } => {
                    match self.peek()? {
                        Token { core: TokenCore::OpenParen, .. } => loop {

                        },
                        Token { core: TokenCore::Comma, .. } => {
                            variants.push(EnumVariant::BareIdent(ident));
                            continue 'consume_variant
                        }
                    }
                }
            }
        }
        let enum_ = Enum { ident: enum_ident, visibility: vis, body: variants }
    }

    /** Keyword::Fn should have already been consumed before calling this */
    fn try_consume_function_declaration(&self, vis: Vis) -> Result<FnDecl, ParserError> {

        let fn_name = self.try_consume_ident(Fn)?;

        let _open_paren = self.try_consume_open_paren(FnParamList)?;

        println!("starting to read parameter list");

        //let mut param_list: Vec<Param> = vec![];

        println!("done reading parameter list");

        let _close_paren = match self.consume()? {
            Token { core: TokenCore::CloseParen, .. } => (),
            token => return Err(ParserError::FnDeclParamListCloseParenExpected(token)),
        };

        let return_type = match self.consume()? {
            Token { core: TokenCore::Arrow, .. } => self.try_consume_type()?,
            _not_arrow => { self.un_advance() ;; Type::Unit }
        };

        let _open_curly = self.try_consume_open_curly(FnBody);

        let _close_curly = match self.consume()? {
            Token { core: TokenCore::CloseCurly, .. } => (),
            token => return Err(ParserError::FnDeclBodyCloseBraceExpected(token)),
        };

        Ok(FnDecl {
            fn_ident: fn_name,
            return_type,
            visibility: vis,
        })
    }
    // fn _try_consume_struct_declaration(&self) -> Result<StructDecl, ParserError> {
    //
    // }
}

// consume: concrete mod, file
enum EndWith { CloseBrace, Eof }
impl ParsingWorktable<'_> {
    fn try_consume_file<'a>(&'a self, mod_name: &'a str) -> Result<Mod<'a>, ParserError<'a>> {
        let return_point = self.get_return_point();
        return match self._try_consume_top_level_stuff(mod_name, EndWith::Eof, Vis::Pub) {
            Ok(mod_) => Ok(mod_),
            Err(e) => { self.return_to_point(return_point) ;; Err(e)}
        }
    }
    fn try_consume_mod(&self, vis: Vis) -> Result<TopLevelStuff, ParserError> {
        // expects: mod_identifier {...}, with `(pub)? mod` keyword(s) already consumed
        let return_point = self.get_return_point();
        let mod_name = self.try_consume_ident(Mod);
        let _consume_mod_open_brace = match self.consume()? {
            Token { core: TokenCore::OpenCurly, .. } => (),
            Token { core: TokenCore::Semicolon, .. } => return Ok(
                TopLevelStuff::ModPtr(ModPtr { visibility: vis, mod_name })
            ),
            token => return Err(ParserError::ModBodyOpenBraceExpected(token)),
        };
        return match self._try_consume_top_level_stuff(mod_name, EndWith::CloseBrace, vis) {
            Ok(mod_) => Ok(TopLevelStuff::Mod(mod_)),
            Err(e) => { self.return_to_point(return_point) ;; Err(e)}
        }
    }
    fn _try_consume_top_level_stuff<'a>(&'a self, mod_name: &'a str, end_with: EndWith, visibility: Vis) -> Result<Mod<'a>, ParserError<'a>> {
        let mut stuff: Vec<TopLevelStuff> = vec![];
        loop {
            let cur_tok = self.consume()?;
            match cur_tok.core {
                TokenCore::Keyword(Keyword::Pub) => {
                    let tok_after_pub = self.consume()?;
                    match tok_after_pub.core {
                        TokenCore::Keyword(Keyword::Fn) => stuff.push(TopLevelStuff::FnDecl(self.try_consume_function_declaration(Vis::Pub)?)),
                        TokenCore::Keyword(Keyword::Struct) => todo!(),
                        TokenCore::Keyword(Keyword::Mod) => stuff.push(self.try_consume_mod(Vis::Pub)?),
                        TokenCore::Keyword(Keyword::Enum) => todo!(),
                        TokenCore::Keyword(Keyword::Use) => todo!(),
                        TokenCore::Keyword(Keyword::Let) => todo!(),
                        _ => todo!(),
                    }
                }
                TokenCore::Keyword(Keyword::Fn) => stuff.push(TopLevelStuff::FnDecl(self.try_consume_function_declaration(Vis::Private)?)),
                TokenCore::Keyword(Keyword::Struct) => todo!(),
                TokenCore::Keyword(Keyword::Mod) => stuff.push(self.try_consume_mod(Vis::Private)?),
                TokenCore::Keyword(Keyword::Enum) => todo!(),
                TokenCore::Keyword(Keyword::Use) => todo!(),
                TokenCore::Keyword(Keyword::Impl) => todo!(),
                TokenCore::Keyword(Keyword::Let) => todo!(),
                TokenCore::Eof => {
                    return match end_with {
                        EndWith::Eof => Ok(Mod { ident: mod_name, stuff, visibility }),
                        EndWith::CloseBrace => Err(ParserError::ModUnexpectedEof(cur_tok)),
                    }
                },
                TokenCore::CloseCurly => {
                    return match end_with {
                        EndWith::Eof => Err(ParserError::ModUnexpectedCloseBrace(cur_tok)),
                        EndWith::CloseBrace => Ok(Mod { ident: mod_name, stuff, visibility })
                    }
                }
                _ => todo!(),
            }
        }
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
        let fn_decl = worktable.try_consume_file("anonymous");
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
        let fn_decl = worktable.try_consume_file("anonymous");
        println!("{:?}", fn_decl);
        //println!("{:?}", worktable);
    }

    #[test]
    fn temp3() {
        let program =
            r########"

            enum Option<T> {
                None,
                Some(T)
            }

            struct Point0D;
            struct Point1D(i64);
            struct Point2D(i64, i64);
            struct FirstNameLastName {
                first_name: str,
                last_name: str,
            }

            fn x() -> [u8] {}

            // mod outermod { mod innermod { fn innerfn() {} }  }

            mod mod_decl;


            "########;
        let (tokens, errs) = lexer(program);
        //println!("{:?}", (&tokens, &errs));

        let mut worktable = ParsingWorktable::new(&tokens);
        let fn_decl = worktable.try_consume_file("anonymous");
        println!("{:?}", fn_decl);
        //println!("{:?}", worktable);
    }
}