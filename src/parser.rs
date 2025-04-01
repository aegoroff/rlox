use crate::lexer::Token;

// Expressions

pub trait ExprVisitor<'a, R> {
    fn visit_literal(&self, token: &Option<Token<'a>>) -> R;
    fn visit_binary_expr(&self, operator: &Token<'a>, left: &Expr<'a>, right: &Expr<'a>) -> R;
    fn visit_unary_expr(&self, operator: &Token<'a>, expr: &Expr<'a>) -> R;
    fn visit_assign_expr(&self, name: &Token<'a>, value: &Expr<'a>) -> R;
    fn visit_call_expr(&self, call: &Expr<'a>) -> R;
    fn visit_get_expr(&self, get: &Expr<'a>) -> R;
    fn visit_grouping_expr(&self, grouping: &Expr<'a>) -> R;
    fn visit_logical_expr(&self, logical: &Expr<'a>) -> R;
    fn visit_set_expr(&self, set: &Expr<'a>) -> R;
    fn visit_super_expr(&self, super_expr: &Expr<'a>) -> R;
    fn visit_this_expr(&self, this: &Expr<'a>) -> R;
    fn visit_variable_expr(&self, variable: &Expr<'a>) -> R;
}

pub enum Expr<'a> {
    Literal(Option<Token<'a>>),
    Binary(Token<'a>, Box<Expr<'a>>, Box<Expr<'a>>),
    Unary(Token<'a>, Box<Expr<'a>>),
    Assign(Token<'a>, Box<Expr<'a>>),
    /// paren, callee, args
    Call(Token<'a>, Box<Expr<'a>>, Vec<Box<Expr<'a>>>),
    Get(Token<'a>, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
    Logical(Token<'a>, Box<Expr<'a>>, Box<Expr<'a>>),
    /// name, object, value
    Set(Token<'a>, Box<Expr<'a>>, Box<Expr<'a>>),
    /// keyword, method
    Super(Token<'a>, Token<'a>),
    This(Token<'a>),
    Variable(Token<'a>),
}

impl<'a> Expr<'a> {
    pub fn accept<R>(&self, visitor: impl ExprVisitor<'a, R>) -> R {
        match self {
            Expr::Literal(token) => visitor.visit_literal(token),
            Expr::Binary(token, left, right) => visitor.visit_binary_expr(token, left, right),
            Expr::Unary(token, expr) => visitor.visit_unary_expr(token, expr),
            Expr::Assign(token, expr) => visitor.visit_assign_expr(token, expr),
            Expr::Call(token, expr, exprs) => todo!(),
            Expr::Get(token, expr) => todo!(),
            Expr::Grouping(expr) => visitor.visit_grouping_expr(expr),
            Expr::Logical(token, expr, expr1) => todo!(),
            Expr::Set(token, expr, expr1) => todo!(),
            Expr::Super(token, token1) => todo!(),
            Expr::This(token) => todo!(),
            Expr::Variable(token) => todo!(),
        }
    }
}

// Statements

pub enum Stmt<'a> {
    Block(Vec<Box<Stmt<'a>>>),
    /// name, superclass, methods
    Class(Token<'a>, Box<Stmt<'a>>, Vec<Box<Stmt<'a>>>),
    Expression(Expr<'a>),
    /// token, params, body
    Function(Token<'a>, Vec<Box<Stmt<'a>>>, Vec<Box<Stmt<'a>>>),
    /// condition, then, else
    If(Expr<'a>, Box<Stmt<'a>>, Box<Stmt<'a>>),
    Print(Expr<'a>),
    Return(Token<'a>, Expr<'a>),
    Variable(Token<'a>, Expr<'a>),
    While(Expr<'a>, Box<Stmt<'a>>),
}

pub trait StmtVisitor<'a, R> {
    fn visit_block_stmt(&self, block: &Stmt<'a>) -> R;
    fn visit_class_stmt(&self, class: &Stmt<'a>) -> R;
    fn visit_expression_stmt(&self, expr: &Stmt<'a>) -> R;
    fn visit_function_stmt(&self, function: &Stmt<'a>) -> R;
    fn visit_if_stmt(&self, if_stmt: &Stmt<'a>) -> R;
    fn visit_print_stmt(&self, print: &Stmt<'a>) -> R;
    fn visit_return_stmt(&self, ret: &Stmt<'a>) -> R;
    fn visit_variable_stmt(&self, variable: &Stmt<'a>) -> R;
    fn visit_while_stmt(&self, while_stmt: &Stmt<'a>) -> R;
}

pub struct AstPrinter {}

impl AstPrinter {
    fn parenthesize(&self, name: &str, expressions: Vec<&Expr<'_>>) -> String {
        let expressions = expressions
            .iter()
            .map(|e| e.accept(self))
            .collect::<Vec<String>>()
            .join(" ");

        format!("({name} {expressions})")
    }

    pub fn print(&self, expr: &Expr<'_>) {
        println!("{}", expr.accept(self))
    }
}

impl<'a> ExprVisitor<'a, String> for &AstPrinter {
    fn visit_literal(&self, token: &Option<Token<'a>>) -> String {
        match token {
            Some(t) => format!("{t}"),
            None => "null".to_owned(),
        }
    }

    fn visit_binary_expr(&self, operator: &Token<'a>, left: &Expr<'a>, right: &Expr<'a>) -> String {
        let op = format!("{}", operator);
        self.parenthesize(&op, vec![left, right])
    }

    fn visit_unary_expr(&self, operator: &Token<'a>, expr: &Expr<'a>) -> String {
        let op = format!("{}", operator);
        self.parenthesize(&op, vec![expr])
    }

    fn visit_assign_expr(&self, name: &Token<'a>, value: &Expr<'a>) -> String {
        todo!()
    }

    fn visit_call_expr(&self, _call: &Expr<'a>) -> String {
        todo!()
    }

    fn visit_get_expr(&self, _get: &Expr<'a>) -> String {
        todo!()
    }

    fn visit_grouping_expr(&self, grouping: &Expr<'a>) -> String {
        self.parenthesize("group", vec![grouping])
    }

    fn visit_logical_expr(&self, _logical: &Expr<'a>) -> String {
        todo!()
    }

    fn visit_set_expr(&self, _set: &Expr<'a>) -> String {
        todo!()
    }

    fn visit_super_expr(&self, _super_expr: &Expr<'a>) -> String {
        todo!()
    }

    fn visit_this_expr(&self, _this: &Expr<'a>) -> String {
        todo!()
    }

    fn visit_variable_expr(&self, _variable: &Expr<'a>) -> String {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ast_printing_test() {
        // Arrange
        let literal1 = Expr::Literal(Some(Token::Number(123.0)));
        let left = Expr::Unary(Token::Minus, Box::new(literal1));
        let literal2 = Expr::Literal(Some(Token::Number(45.67)));
        let right = Expr::Grouping(Box::new(literal2));
        let bin = Expr::Binary(Token::Star, Box::new(left), Box::new(right));
        let ast_printer = AstPrinter {};

        // Act
        ast_printer.print(&bin);
    }
}
