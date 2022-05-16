use crate::lexer::Token;
use crate::lexer::TokenCore;

struct ParsingWorktable {
    tokens: Vec<Token>,
    index: usize,
}

impl ParsingWorktable {
    fn new(tokens: Vec<Token>) -> ParsingWorktable {
        ParsingWorktable { tokens, index: 0 }
    }
    fn peek(&self) -> Option<&Token> {
        if self.index >= tokens.len() { None }
        else { Some(&self.tokens[self.index]) }
    }
    fn peek_x(&self, offset: usize) -> Option<&Token> {
        let wanted = self.index + offset;
        if wanted >= tokens.len() { None }
        else { Some(&self.tokens[self.index]) }
    }
    fn peek_back_x(&self, offset: usize) -> Option<&Token> {
        if offset > self.index { None }
        else { Some(&self.tokens[self.index - offset]) }
    }
    fn advance(&mut self) {
        if self.index < tokens.len() { None }
        self.index += 1
    }
    fn consume(&mut self) -> Option<&Token> {
        if self.index >= tokens.len() { None }
        else { self.index += 1 ;; Some(&self.tokens[self.index]) }
    }
}



mod helper {
    use super::{*};

    fn compile_time_playground() {



    }

    pub macro_rules! is_var {
        ($val:ident, $var:path) => {
            match $val {
                $var{..} => true,
                _ => false
            }
        }
    }

    pub macro_rules! is_var_2 {
        ($val:ident, $var:path, $var2:path) => {
            match $val {
                $var{..} => true,
                $var2{..} => true,
                _ => false
            }
        }
    }

    pub macro_rules! is_var_3 {
        ($val:ident, $var:path, $var2:path, $var3:path) => {
            match $val {
                $var{..} => true,
                $var2{..} => true,
                $var3{..} => true,
                _ => false
            }
        }
    }

    pub macro_rules! is_var_4 {
        ($val:ident, $var:path, $var2:path, $var3:path, $var4:path) => {
            match $val {
                $var{..} => true,
                $var2{..} => true,
                $var3{..} => true,
                $var4{..} => true,
                _ => false
            }
        }
    }
}
use helper::{*};

mod representation {
    use super::{*};

    struct BinaryExpr { left: Expr, op: Token, right: Expr }

    enum Expr {
        BinaryExpr
    }

}
use representation::{*};