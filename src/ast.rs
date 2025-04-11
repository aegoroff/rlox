use miette::miette;

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

#[derive(Debug)]
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
    pub fn accept<R>(&self, visitor: &impl ExprVisitor<'a, R>) -> R {
        match self {
            Expr::Literal(token) => visitor.visit_literal(token),
            Expr::Binary(operator, left, right) => visitor.visit_binary_expr(operator, left, right),
            Expr::Unary(operator, expr) => visitor.visit_unary_expr(operator, expr),
            Expr::Assign(name, value) => visitor.visit_assign_expr(name, value),
            Expr::Call(paren, callee, args) => visitor.visit_call_expr(paren, callee, args),
            Expr::Get(name, object) => visitor.visit_get_expr(name, object),
            Expr::Grouping(grouping) => visitor.visit_grouping_expr(grouping),
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

impl<'a> Stmt<'a> {
    pub fn accept<R>(&self, visitor: &impl StmtVisitor<'a, R>) -> R {
        match self {
            Stmt::Block(body) => visitor.visit_block_stmt(body),
            Stmt::Class(name, superclass, methods) => {
                visitor.visit_class_stmt(name, superclass, methods)
            }
            Stmt::Expression(expr) => visitor.visit_expression_stmt(expr),
            Stmt::Function(token, params, body) => visitor.visit_function_stmt(token, params, body),
            Stmt::If(cond, then, otherwise) => visitor.visit_if_stmt(cond, then, otherwise),
            Stmt::Print(expr) => visitor.visit_print_stmt(expr),
            Stmt::Return(keyword, value) => visitor.visit_return_stmt(keyword, value),
            Stmt::Variable(name, initializer) => visitor.visit_variable_stmt(name, initializer),
            Stmt::While(cond, body) => visitor.visit_while_stmt(cond, body),
        }
    }
}

pub trait StmtVisitor<'a, R> {
    fn visit_block_stmt(&self, body: &[Box<Stmt<'a>>]) -> R;
    fn visit_class_stmt(
        &self,
        name: &Token<'a>,
        superclass: &Stmt<'a>,
        methods: &[Box<Stmt<'a>>],
    ) -> R;
    fn visit_expression_stmt(&self, expr: &Expr<'a>) -> R;
    fn visit_function_stmt(
        &self,
        token: &Token<'a>,
        params: &[Box<Stmt<'a>>],
        body: &[Box<Stmt<'a>>],
    ) -> R;
    fn visit_if_stmt(&self, cond: &Expr<'a>, then: &Stmt<'a>, otherwise: &Stmt<'a>) -> R;
    fn visit_print_stmt(&self, expr: &Expr<'a>) -> R;
    fn visit_return_stmt(&self, keyword: &Token<'a>, value: &Expr<'a>) -> R;
    fn visit_variable_stmt(&self, name: &Token<'a>, initializer: &Expr<'a>) -> R;
    fn visit_while_stmt(&self, cond: &Expr<'a>, body: &Stmt<'a>) -> R;
}

pub struct AstPrinter {}

impl AstPrinter {
    fn parenthesize(&self, name: &str, expressions: &[&Expr<'_>]) -> String {
        let expressions = expressions
            .iter()
            .map(|e| e.accept(&self))
            .collect::<Vec<String>>()
            .join(" ");

        format!("({name} {expressions})")
    }

    pub fn print(&self, expr: &Expr<'_>) {
        println!("{}", expr.accept(&self));
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
        let op = format!("{operator}");
        self.parenthesize(&op, &[left, right])
    }

    fn visit_unary_expr(&self, operator: &Token<'a>, expr: &Expr<'a>) -> String {
        let op = format!("{operator}");
        self.parenthesize(&op, &[expr])
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
        self.parenthesize("group", &[grouping])
    }

    fn visit_logical_expr(
        &self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> String {
        let op = format!("{operator}");
        self.parenthesize(&op, &[left, right])
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

pub enum LoxValue {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

impl LoxValue {
    pub fn try_num(&self) -> miette::Result<f64> {
        if let LoxValue::Number(n) = self {
            Ok(*n)
        } else {
            Err(miette!("Number expected"))
        }
    }

    pub fn try_str(&self) -> miette::Result<&String> {
        if let LoxValue::String(s) = self {
            Ok(s)
        } else {
            Err(miette!("String expected"))
        }
    }

    pub fn try_bool(&self) -> miette::Result<bool> {
        match self {
            LoxValue::Bool(b) => Ok(*b),
            LoxValue::Nil => Ok(false),
            _ => Err(miette!("Boolean expected or Nil")),
        }
    }
}

pub struct Evaluator {}

impl Evaluator {
    pub fn print(&self, expr: &Expr<'_>) {
        let expr = self.evaluate(expr);
        let expr = match expr {
            Ok(e) => e,
            Err(e) => {
                println!("{e}");
                return;
            }
        };
        match expr {
            LoxValue::String(s) => println!("{s}"),
            LoxValue::Number(n) => println!("{n}"),
            LoxValue::Bool(b) => println!("{b}"),
            LoxValue::Nil => println!("Null"),
        }
    }

    pub fn evaluate(&self, expr: &Expr<'_>) -> miette::Result<LoxValue> {
        expr.accept(&self)
    }
}

const ERROR_MARGIN: f64 = 0.00001;

impl<'a> ExprVisitor<'a, miette::Result<LoxValue>> for &Evaluator {
    fn visit_literal(&self, token: &Option<Token<'a>>) -> miette::Result<LoxValue> {
        match token {
            Some(t) => match t {
                Token::String(s) => Ok(LoxValue::String((*s).to_string())),
                Token::Number(n) => Ok(LoxValue::Number(*n)),
                Token::False => Ok(LoxValue::Bool(false)),
                Token::True => Ok(LoxValue::Bool(true)),
                _ => Err(miette!("Invalid literal")),
            },
            None => Ok(LoxValue::Nil),
        }
    }

    fn visit_binary_expr(
        &self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> miette::Result<LoxValue> {
        let lhs = self.evaluate(left)?;
        let rhs = self.evaluate(right)?;

        match operator {
            Token::Minus => {
                let l = lhs.try_num()?;
                let r = rhs.try_num()?;
                let result = l - r;
                Ok(LoxValue::Number(result))
            }
            Token::Plus => {
                if let Ok(l) = lhs.try_num() {
                    let r = rhs.try_num()?;
                    let result = l + r;
                    Ok(LoxValue::Number(result))
                } else if let Ok(l) = lhs.try_str() {
                    let r = rhs.try_str()?;
                    let result = l.to_owned() + r;
                    Ok(LoxValue::String(result))
                } else {
                    Err(miette!("Invalid operands types for plus"))
                }
            }
            Token::Slash => {
                let l = lhs.try_num()?;
                let r = rhs.try_num()?;
                let result = l / r;
                Ok(LoxValue::Number(result))
            }
            Token::Star => {
                let l = lhs.try_num()?;
                let r = rhs.try_num()?;
                let result = l * r;
                Ok(LoxValue::Number(result))
            }
            Token::BangEqual => {
                if let Ok(l) = lhs.try_num() {
                    let r = rhs.try_num()?;
                    Ok(LoxValue::Bool((l - r).abs() > ERROR_MARGIN))
                } else if let Ok(l) = lhs.try_bool() {
                    let r = rhs.try_bool()?;
                    let result = l != r;
                    Ok(LoxValue::Bool(result))
                } else if let Ok(l) = lhs.try_str() {
                    let r = rhs.try_str()?;
                    let result = l != r;
                    Ok(LoxValue::Bool(result))
                } else {
                    Err(miette!("Invalid operands types for not equal"))
                }
            }
            Token::EqualEqual => {
                if let Ok(l) = lhs.try_num() {
                    let r = rhs.try_num()?;
                    Ok(LoxValue::Bool((l - r).abs() < ERROR_MARGIN))
                } else if let Ok(l) = lhs.try_bool() {
                    let r = rhs.try_bool()?;
                    let result = l == r;
                    Ok(LoxValue::Bool(result))
                } else if let Ok(l) = lhs.try_str() {
                    let r = rhs.try_str()?;
                    let result = l == r;
                    Ok(LoxValue::Bool(result))
                } else {
                    Err(miette!("Invalid operands types for equal"))
                }
            }
            Token::Greater => {
                if let Ok(l) = lhs.try_num() {
                    let r = rhs.try_num()?;
                    Ok(LoxValue::Bool(l > r))
                } else if let Ok(l) = lhs.try_bool() {
                    let r = rhs.try_bool()?;
                    let result = l & !r;
                    Ok(LoxValue::Bool(result))
                } else if let Ok(l) = lhs.try_str() {
                    let r = rhs.try_str()?;
                    let result = l > r;
                    Ok(LoxValue::Bool(result))
                } else {
                    Err(miette!("Invalid operands types for greater"))
                }
            }
            Token::GreaterEqual => {
                if let Ok(l) = lhs.try_num() {
                    let r = rhs.try_num()?;
                    Ok(LoxValue::Bool(l >= r))
                } else if let Ok(l) = lhs.try_bool() {
                    let r = rhs.try_bool()?;
                    let result = l >= r;
                    Ok(LoxValue::Bool(result))
                } else if let Ok(l) = lhs.try_str() {
                    let r = rhs.try_str()?;
                    let result = l >= r;
                    Ok(LoxValue::Bool(result))
                } else {
                    Err(miette!("Invalid operands types for greater or equal"))
                }
            }
            Token::Less => {
                if let Ok(l) = lhs.try_num() {
                    let r = rhs.try_num()?;
                    Ok(LoxValue::Bool(l < r))
                } else if let Ok(l) = lhs.try_bool() {
                    let r = rhs.try_bool()?;
                    let result = !l & r;
                    Ok(LoxValue::Bool(result))
                } else if let Ok(l) = lhs.try_str() {
                    let r = rhs.try_str()?;
                    let result = l < r;
                    Ok(LoxValue::Bool(result))
                } else {
                    Err(miette!("Invalid operands types for less"))
                }
            }
            Token::LessEqual => {
                if let Ok(l) = lhs.try_num() {
                    let r = rhs.try_num()?;
                    Ok(LoxValue::Bool(l <= r))
                } else if let Ok(l) = lhs.try_bool() {
                    let r = rhs.try_bool()?;
                    let result = l <= r;
                    Ok(LoxValue::Bool(result))
                } else if let Ok(l) = lhs.try_str() {
                    let r = rhs.try_str()?;
                    let result = l <= r;
                    Ok(LoxValue::Bool(result))
                } else {
                    Err(miette!("Invalid operands types for less or equal"))
                }
            }
            _ => Err(miette!("Invalid binary operator")),
        }
    }

    fn visit_unary_expr(&self, operator: &Token<'a>, expr: &Expr<'a>) -> miette::Result<LoxValue> {
        let val = self.evaluate(expr)?;
        match operator {
            Token::Minus => Ok(LoxValue::Number(-val.try_num()?)),
            Token::Bang => Ok(LoxValue::Bool(!val.try_bool()?)),
            _ => Err(miette!("Invalid unary operator")),
        }
    }

    fn visit_assign_expr(&self, name: &Token<'a>, value: &Expr<'a>) -> miette::Result<LoxValue> {
        let _ = value;
        let _ = name;
        todo!()
    }

    fn visit_call_expr(
        &self,
        paren: &Token<'a>,
        callee: &Expr<'a>,
        args: &[Box<Expr<'a>>],
    ) -> miette::Result<LoxValue> {
        let _ = args;
        let _ = callee;
        let _ = paren;
        todo!()
    }

    fn visit_get_expr(&self, name: &Token<'a>, object: &Expr<'a>) -> miette::Result<LoxValue> {
        let _ = object;
        let _ = name;
        todo!()
    }

    fn visit_grouping_expr(&self, grouping: &Expr<'a>) -> miette::Result<LoxValue> {
        self.evaluate(grouping)
    }

    fn visit_logical_expr(
        &self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> miette::Result<LoxValue> {
        let _ = right;
        let _ = left;
        let _ = operator;
        todo!()
    }

    fn visit_set_expr(
        &self,
        name: &Token<'a>,
        obj: &Expr<'a>,
        val: &Expr<'a>,
    ) -> miette::Result<LoxValue> {
        let _ = val;
        let _ = obj;
        let _ = name;
        todo!()
    }

    fn visit_super_expr(
        &self,
        keyword: &Token<'a>,
        method: &Token<'a>,
    ) -> miette::Result<LoxValue> {
        let _ = method;
        let _ = keyword;
        todo!()
    }

    fn visit_this_expr(&self, keyword: &Token<'a>) -> miette::Result<LoxValue> {
        let _ = keyword;
        todo!()
    }

    fn visit_variable_expr(&self, name: &Token<'a>) -> miette::Result<LoxValue> {
        let _ = name;
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;

    use super::*;
    use test_case::test_case;

    #[test_case("--1", 1.0)]
    #[test_case("1 - 1", 0.0)]
    #[test_case("1 - 2", -1.0)]
    #[test_case("2 - 1", 1.0)]
    #[test_case("2 + 3", 5.0)]
    #[test_case("2 + 3 - 1", 4.0)]
    #[test_case("3 + 3 / 3", 4.0)]
    #[test_case("(3 + 3) / 3", 2.0)]
    #[test_case("4 / 2", 2.0)]
    #[test_case("4 / 1", 4.0)]
    #[test_case("5 / -1", -5.0)]
    #[test_case("(5 - (3-1)) + -1", 2.0)]
    #[test_case("(5 - (3-1)) * -1", -3.0)]
    #[test_case("((5 - (3-1)) * -2) / 4", -1.5)]
    #[test_case("((5 - (3-1) + 3) * -2) / 4", -3.0)]
    fn evaluator_numeric_positive_tests(input: &str, expected: f64) {
        // Arrange
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap().unwrap();
        let eval = Evaluator {};

        // Act
        let actual = eval.evaluate(&expr);

        // Assert
        assert!(actual.is_ok());
        let actual = actual.unwrap();
        if let LoxValue::Number(actual) = actual {
            assert_eq!(actual, expected)
        } else {
            todo!()
        }
    }

    #[test_case("(\"a\" + \"b\") + \"c\"", "abc")]
    fn evaluator_string_positive_tests(input: &str, expected: &str) {
        // Arrange
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap().unwrap();
        let eval = Evaluator {};

        // Act
        let actual = eval.evaluate(&expr);

        // Assert
        assert!(actual.is_ok());
        let actual = actual.unwrap();
        if let LoxValue::String(actual) = actual {
            assert_eq!(actual, expected)
        } else {
            todo!()
        }
    }

    #[test_case("(\"a\" == \"b\")", false)]
    #[test_case("(\"a\" != \"c\")", true)]
    #[test_case("(\"ab\" == \"ab\")", true)]
    #[test_case("(\"aa\" > \"bb\")", false)]
    #[test_case("(\"bb\" > \"aa\")", true)]
    #[test_case("(\"bba\" >= \"aaa\")", true)]
    #[test_case("(\"bba\" <= \"aaa\")", false)]
    #[test_case("1 == 2", false)]
    #[test_case("2 == 2", true)]
    #[test_case("3 >= 3", true)]
    #[test_case("3 >= 2", true)]
    #[test_case("3 <= 1", false)]
    #[test_case("(3 - 1) * 200 <= 1", false)]
    #[test_case("3 > 1 == true", true)]
    fn evaluator_predicates_tests(input: &str, expected: bool) {
        // Arrange
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap().unwrap();
        let eval = Evaluator {};

        // Act
        let actual = eval.evaluate(&expr);

        // Assert
        assert!(actual.is_ok());
        let actual = actual.unwrap();
        if let LoxValue::Bool(actual) = actual {
            assert_eq!(actual, expected)
        } else {
            todo!()
        }
    }
}
