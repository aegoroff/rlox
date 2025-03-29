use crate::lexer::Token;

pub trait Visitor {
    fn visit_literal(&self, literal: &Literal);
    fn visit_binary_expr(&self, binary: &BinaryExpr);
    fn visit_unary_expr(&self, binary: &UnaryExpr);
}

pub trait Expr {
    fn visit<V: Visitor>(&self, visitor: V);
}

pub struct Literal<'a> {
    pub token: Token<'a>,
}

impl Expr for Literal<'_> {
    fn visit<V: Visitor>(&self, visitor: V) {
        visitor.visit_literal(self);
    }
}

pub struct BinaryExpr {}

impl Expr for BinaryExpr {
    fn visit<V: Visitor>(&self, visitor: V) {
        visitor.visit_binary_expr(self);
    }
}

pub struct UnaryExpr {}

impl Expr for UnaryExpr {
    fn visit<V: Visitor>(&self, visitor: V) {
        visitor.visit_unary_expr(self);
    }
}
