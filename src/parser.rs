#![allow(dead_code, redundant_semicolons)]

use crate::lexer::Token;
use crate::lexer::TokenCore;
use crate::lexer::Keyword;
use std::cell::Cell;
use ast::{*};

mod ast {
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
                    Type::Never | Type::Unknown | Type::Unit | Type::Named { .. } => only_type,
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
}

#[derive(Debug)]
pub enum ParserError<'a> {
    // Error case  (Offending token)
    // Fn Decl
    FnDeclNameExpected                                 (&'a Token),
    FnDeclParamListOpenParenExpected                   (&'a Token),
    FnDeclParamListCloseParenExpected                  (&'a Token),
    FnDeclBodyOpenBraceExpected                        (&'a Token),
    FnDeclBodyCloseBraceExpected                       (&'a Token),
    // Type
    TypeNameIdentExpected                              (&'a Token),
    TypeDidNotDeParenthesizeBeforeTryingConsumeOneType (&'a Token),
    TypeNameOrOpenParenExpected                        (&'a Token),
    TypeParamListUnexpectedToken                       (&'a Token),
    TypeTupleCommaOrCloseParenExpected                 (&'a Token),
    // Mod
    ModUnexpectedEof                                   (&'a Token),
    ModUnexpectedCloseBrace                            (&'a Token),
    ModNameIdentExpected                               (&'a Token),
    ModBodyOpenBraceExpected                           (&'a Token),
    // Eof
    UnexpectedEof                                                 ,
    UnexpectedBeginOfFile                                         ,
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

impl ParsingWorktable<'_> {

    fn try_consume_type(&self) -> Result<Type, ParserError> {
        let _open_paren = match self.peek()? {
            Token { core: TokenCore::OpenParen, .. } => self.advance(),  // () or (Type) or (Type, Type, ...)
            Token { core: TokenCore::Ident(_), .. }  => { return Ok(self.try_consume_one_type()?); }, // OneType
            Token { core: TokenCore::Bang, .. } => { self.advance() ;; return Ok(Type::Never); },  // ! type is never type
            Token { core: TokenCore::Question, .. } => { self.advance() ;; return Ok(Type::Unknown); },  // ? type is unknown type
            token => return Err(ParserError::TypeNameOrOpenParenExpected(token))
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

    /** Only call after having de-parenthesized the type expression */
    fn try_consume_one_type(&self) -> Result<Type, ParserError> {
        let return_point = self.get_return_point();
        match self._try_consume_one_type() {
            Ok(one_type) => Ok(one_type),
            Err(err) => { self.return_to_point(return_point) ;; Err(err) }
        }
    }
    fn _try_consume_one_type(&self) -> Result<Type, ParserError> {
        let type_ident = match self.consume()? {
            Token { core: TokenCore::Ident(type_ident), .. } => { type_ident }
            token => {
                return match token.core {
                    TokenCore::OpenParen => Err(
                        ParserError::TypeDidNotDeParenthesizeBeforeTryingConsumeOneType(token)
                    ),
                    _ => Err(ParserError::TypeNameIdentExpected(token)),
                }
            }
        };
        let _open_angle_bracket = match self.peek()? {
            Token { core: TokenCore::Lt, .. } => { self.advance() }
            _not_lt => return Ok(Type::Named { type_name: type_ident, params: vec![] })
            // if no angle bracket follows the ident, this is a 0-param type
        };
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

    /** Keyword::Fn should have already been consumed before calling this */
    fn try_consume_function_declaration(&self, vis: Vis) -> Result<FnDecl, ParserError> {
        let saved_return_point = self.get_return_point();
        match self._try_consume_function_declaration(vis) {
            Err(err) => { self.return_to_point(saved_return_point) ;; Err(err) }
            ok => ok,
        }
    }
    fn _try_consume_function_declaration(&self, vis: Vis) -> Result<FnDecl, ParserError> {

        let fn_name = match self.consume()? {
            Token { core: TokenCore::Ident(fn_name), .. } => fn_name,
            token => return Err(ParserError::FnDeclNameExpected(token)),
        };

        let _open_paren = match self.consume()? {
            Token { core: TokenCore::OpenParen , .. } => (),
            token => return Err(ParserError::FnDeclParamListOpenParenExpected(token)),
        };

        println!();

        let _close_paren = match self.consume()? {
            Token { core: TokenCore::CloseParen, .. } => (),
            token => return Err(ParserError::FnDeclParamListCloseParenExpected(token)),
        };

        let return_type = match self.consume()? {
            Token { core: TokenCore::Arrow, .. } => self.try_consume_type()?,
            _not_arrow => { self.un_advance() ;; Type::Unit }
        };

        let _open_brace = match self.consume()? {
            Token { core: TokenCore::OpenBrace, .. } => (),
            token => return Err(ParserError::FnDeclBodyOpenBraceExpected(token)),
        };

        let _close_brace = match self.consume()? {
            Token { core: TokenCore::CloseBrace, .. } => (),
            token => return Err(ParserError::FnDeclBodyCloseBraceExpected(token)),
        };

        Ok(FnDecl {
            fn_ident: fn_name,
            return_type,
            visibility: vis,
        })
    }
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
        let mod_name = match self.consume()? {
            Token { core: TokenCore::Ident(mod_ident), .. } => mod_ident,
            token => return Err(ParserError::ModNameIdentExpected(token)),
        };
        let _consume_mod_open_brace = match self.consume()? {
            Token { core: TokenCore::OpenBrace, .. } => (),
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
                        _ => todo!(),
                    }
                }
                TokenCore::Keyword(Keyword::Fn) => stuff.push(TopLevelStuff::FnDecl(self.try_consume_function_declaration(Vis::Private)?)),
                TokenCore::Keyword(Keyword::Struct) => todo!(),
                TokenCore::Keyword(Keyword::Mod) => stuff.push(self.try_consume_mod(Vis::Private)?),
                TokenCore::Keyword(Keyword::Enum) => todo!(),
                TokenCore::Keyword(Keyword::Use) => todo!(),
                TokenCore::Eof => {
                    return match end_with {
                        EndWith::Eof => Ok(Mod { ident: mod_name, stuff, visibility }),
                        EndWith::CloseBrace => Err(ParserError::ModUnexpectedEof(cur_tok)),
                    }
                },
                TokenCore::CloseBrace => {
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
        let mut worktable = LexingWorktable::new(
            "pub fn some_fn_lol () -> (Option<(str, i64, !), Triplet<str, !, ?>>, ?) {} pub fn unit () {}".chars().collect()
        );
        let (tokens, errs) = lexer(&mut worktable);
        //println!("{:?}", (&tokens, &errs));

        let mut worktable = ParsingWorktable::new(&tokens);
        let fn_decl = worktable.try_consume_file("anonymous");
        println!("{:?}", fn_decl);
        //println!("{:?}", worktable);
    }

    #[test]
    fn temp2() {
        let mut worktable = LexingWorktable::new(
            "mod outer_mod { mod inner_mod { fn inner_fn() {} }  }".chars().collect()
        );
        let (tokens, errs) = lexer(&mut worktable);
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