use crate::lexer::Token;

// Expressions

pub trait ExprVisitor<'a, R> {
    fn visit_literal(&self, token: &Option<Token<'a>>) -> R;
    fn visit_binary_expr(&self, operator: &Token<'a>, left: &Expr<'a>, right: &Expr<'a>) -> R;
    fn visit_unary_expr(&self, operator: &Token<'a>, expr: &Expr<'a>) -> R;
    fn visit_assign_expr(&self, name: &Token<'a>, value: &Expr<'a>) -> R;
    fn visit_call_expr(&self, paren: &Token<'a>, callee: &Expr<'a>, args: &[Box<Expr<'a>>]) -> R;
    fn visit_get_expr(&self, name: &Token<'a>, object: &Expr<'a>) -> R;
    fn visit_grouping_expr(&self, grouping: &Expr<'a>) -> R;
    fn visit_logical_expr(&self, operator: &Token<'a>, left: &Expr<'a>, right: &Expr<'a>) -> R;
    fn visit_set_expr(&self, name: &Token<'a>, obj: &Expr<'a>, val: &Expr<'a>) -> R;
    fn visit_super_expr(&self, keyword: &Token<'a>, method: &Token<'a>) -> R;
    fn visit_this_expr(&self, keyword: &Token<'a>) -> R;
    fn visit_variable_expr(&self, name: &Token<'a>) -> R;
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
            Expr::Call(token, expr, exprs) => visitor.visit_call_expr(token, expr, exprs),
            Expr::Get(name, object) => visitor.visit_get_expr(name, object),
            Expr::Grouping(expr) => visitor.visit_grouping_expr(expr),
            Expr::Logical(token, left, right) => visitor.visit_logical_expr(token, left, right),
            Expr::Set(name, obj, val) => visitor.visit_set_expr(name, obj, val),
            Expr::Super(keyword, method) => visitor.visit_super_expr(keyword, method),
            Expr::This(keyword) => visitor.visit_this_expr(keyword),
            Expr::Variable(name) => visitor.visit_variable_expr(name),
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
        let _ = value;
        let _ = name;
        todo!()
    }

    fn visit_call_expr(
        &self,
        paren: &Token<'a>,
        callee: &Expr<'a>,
        args: &[Box<Expr<'a>>],
    ) -> String {
        let _ = args;
        let _ = callee;
        let _ = paren;
        todo!()
    }

    fn visit_get_expr(&self, name: &Token<'a>, object: &Expr<'a>) -> String {
        let _ = object;
        let _ = name;
        todo!()
    }

    fn visit_grouping_expr(&self, grouping: &Expr<'a>) -> String {
        self.parenthesize("group", vec![grouping])
    }

    fn visit_logical_expr(
        &self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> String {
        let _ = right;
        let _ = left;
        let _ = operator;
        todo!()
    }

    fn visit_set_expr(&self, name: &Token<'a>, obj: &Expr<'a>, val: &Expr<'a>) -> String {
        let _ = val;
        let _ = obj;
        let _ = name;
        todo!()
    }

    fn visit_super_expr(&self, keyword: &Token<'a>, method: &Token<'a>) -> String {
        let _ = method;
        let _ = keyword;
        todo!()
    }

    fn visit_this_expr(&self, keyword: &Token<'a>) -> String {
        let _ = keyword;
        todo!()
    }

    fn visit_variable_expr(&self, name: &Token<'a>) -> String {
        let _ = name;
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
