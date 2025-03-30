use crate::lexer::Token;

// Expressions

pub trait ExprVisitor<R> {
    fn visit_literal(&self, literal: &LiteralExpr) -> R;
    fn visit_binary_expr(&self, binary: &BinaryExpr) -> R;
    fn visit_unary_expr(&self, unary: &UnaryExpr) -> R;
    fn visit_assign_expr(&self, assign: &AssignExpr) -> R;
    fn visit_call_expr(&self, call: &CallExpr) -> R;
    fn visit_get_expr(&self, get: &GetExpr) -> R;
    fn visit_grouping_expr(&self, grouping: &GroupingExpr) -> R;
    fn visit_logical_expr(&self, logical: &LogicalExpr) -> R;
    fn visit_set_expr(&self, set: &SetExpr) -> R;
    fn visit_super_expr(&self, super_expr: &SuperExpr) -> R;
    fn visit_this_expr(&self, this: &ThisExpr) -> R;
    fn visit_variable_expr(&self, variable: &VariableExpr) -> R;
}

pub trait Expr {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R
    where
        Self: Sized;
}

pub struct LiteralExpr<'a> {
    pub token: Option<Token<'a>>,
}

impl<'a> LiteralExpr<'a> {
    pub fn new(token: Option<Token<'a>>) -> Self {
        Self { token }
    }
}

impl Expr for LiteralExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_literal(self)
    }
}

impl Expr for &LiteralExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_literal(self)
    }
}

pub struct BinaryExpr<'a> {
    pub operator: Token<'a>,
    pub left: Box<dyn Expr>,
    pub right: Box<dyn Expr>,
}

impl Expr for BinaryExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_binary_expr(self)
    }
}

impl Expr for &BinaryExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_binary_expr(self)
    }
}

pub struct UnaryExpr<'a> {
    pub operator: Token<'a>,
    pub right: Box<dyn Expr>,
}

impl Expr for UnaryExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_unary_expr(self)
    }
}

impl Expr for &UnaryExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_unary_expr(self)
    }
}

pub struct AssignExpr<'a> {
    pub name: Token<'a>,
    pub expr: Box<dyn Expr>,
}

impl Expr for &AssignExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_assign_expr(self)
    }
}

pub struct CallExpr<'a> {
    pub paren: Token<'a>,
    pub callee: Box<dyn Expr>,
    pub args: Vec<Box<dyn Expr>>,
}

impl Expr for CallExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_call_expr(self)
    }
}

impl Expr for &CallExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_call_expr(self)
    }
}

pub struct GetExpr<'a> {
    pub name: Token<'a>,
    pub object: Box<dyn Expr>,
}

impl Expr for GetExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_get_expr(self)
    }
}

impl Expr for &GetExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_get_expr(self)
    }
}

pub struct GroupingExpr {
    pub expression: Box<dyn Expr>,
}

impl Expr for GroupingExpr {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_grouping_expr(self)
    }
}

impl Expr for &GroupingExpr {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_grouping_expr(self)
    }
}

pub struct LogicalExpr<'a> {
    pub operator: Token<'a>,
    pub left: Box<dyn Expr>,
    pub right: Box<dyn Expr>,
}

impl Expr for LogicalExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_logical_expr(self)
    }
}

impl Expr for &LogicalExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_logical_expr(self)
    }
}

pub struct SetExpr<'a> {
    pub name: Token<'a>,
    pub object: Box<dyn Expr>,
    pub value: Box<dyn Expr>,
}

impl Expr for SetExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_set_expr(self)
    }
}

impl Expr for &SetExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_set_expr(self)
    }
}

pub struct SuperExpr<'a> {
    pub keyword: Token<'a>,
    pub method: Token<'a>,
}

impl Expr for SuperExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_super_expr(self)
    }
}

impl Expr for &SuperExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_super_expr(self)
    }
}

pub struct ThisExpr<'a> {
    pub keyword: Token<'a>,
}

impl Expr for ThisExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_this_expr(self)
    }
}

impl Expr for &ThisExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_this_expr(self)
    }
}

pub struct VariableExpr<'a> {
    pub name: Token<'a>,
}

impl Expr for VariableExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_variable_expr(self)
    }
}

impl Expr for &VariableExpr<'_> {
    fn accept<R>(&self, visitor: impl ExprVisitor<R>) -> R {
        visitor.visit_variable_expr(self)
    }
}

// Statements

pub trait StmtVisitor<R> {
    fn visit_block_stmt<S: Stmt>(&self, block: &BlockStmt<S>) -> R;
    fn visit_class_stmt<S: Stmt>(&self, class: &ClassStmt<S>) -> R;
    fn visit_expression_stmt<E: Expr>(&self, expr: &ExpressionStmt<E>) -> R;
    fn visit_function_stmt<S: Stmt>(&self, function: &FunctionStmt<S>) -> R;
    fn visit_if_stmt<E: Expr, S: Stmt>(&self, if_stmt: &IfStmt<E, S>) -> R;
    fn visit_print_stmt<E: Expr>(&self, print: &PrintStmt<E>) -> R;
    fn visit_return_stmt<E: Expr>(&self, ret: &ReturnStmt<E>) -> R;
    fn visit_variable_stmt<E: Expr>(&self, variable: &VariableStmt<E>) -> R;
    fn visit_while_stmt<E: Expr, S: Stmt>(&self, while_stmt: &WhileStmt<E, S>) -> R;
}

pub trait Stmt {
    fn accept<V: StmtVisitor<R>, R>(&self, visitor: V) -> R;
}

pub struct BlockStmt<S: Stmt> {
    pub args: Vec<S>,
}

impl<S: Stmt> Stmt for BlockStmt<S> {
    fn accept<V: StmtVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_block_stmt(self)
    }
}

pub struct ClassStmt<'a, S: Stmt> {
    pub methods: Vec<FunctionStmt<'a, S>>,
    pub superclass: VariableExpr<'a>,
    pub name: Token<'a>,
}

impl<S: Stmt> Stmt for ClassStmt<'_, S> {
    fn accept<V: StmtVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_class_stmt(self)
    }
}

pub struct ExpressionStmt<E: Expr> {
    pub expression: E,
}

impl<E: Expr> Stmt for ExpressionStmt<E> {
    fn accept<V: StmtVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_expression_stmt(self)
    }
}

pub struct FunctionStmt<'a, S: Stmt> {
    pub token: Token<'a>,
    pub params: Vec<S>,
    pub body: Vec<S>,
}

impl<S: Stmt> Stmt for FunctionStmt<'_, S> {
    fn accept<V: StmtVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_function_stmt(self)
    }
}

pub struct IfStmt<E: Expr, S: Stmt> {
    pub condition: E,
    pub then_branch: S,
    pub else_branch: S,
}

impl<E: Expr, S: Stmt> Stmt for IfStmt<E, S> {
    fn accept<V: StmtVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_if_stmt(self)
    }
}

pub struct PrintStmt<E: Expr> {
    pub expression: E,
}

impl<E: Expr> Stmt for PrintStmt<E> {
    fn accept<V: StmtVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_print_stmt(self)
    }
}

pub struct ReturnStmt<'a, E: Expr> {
    pub token: Token<'a>,
    pub value: E,
}

impl<E: Expr> Stmt for ReturnStmt<'_, E> {
    fn accept<V: StmtVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_return_stmt(self)
    }
}

pub struct VariableStmt<'a, E: Expr> {
    pub token: Token<'a>,
    pub initializer: E,
}

impl<E: Expr> Stmt for VariableStmt<'_, E> {
    fn accept<V: StmtVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_variable_stmt(self)
    }
}

pub struct WhileStmt<E: Expr, S: Stmt> {
    pub condition: E,
    pub body: S,
}

impl<E: Expr, S: Stmt> Stmt for WhileStmt<E, S> {
    fn accept<V: StmtVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_while_stmt(self)
    }
}

pub struct AstPrinter {}

impl AstPrinter {
    fn parenthesize(&self, name: &str, expressions: Vec<impl Expr>) -> String {
        let expressions = expressions
            .iter()
            .map(|e| e.accept(self))
            .collect::<Vec<String>>()
            .join(" ");

        format!("({name} {expressions})")
    }

    pub fn print<E: Expr>(&self, expr: E) {
        println!("{}", expr.accept(self))
    }
}

impl ExprVisitor<String> for &AstPrinter {
    fn visit_literal(&self, literal: &LiteralExpr) -> String {
        match &literal.token {
            Some(t) => format!("{t}"),
            None => "null".to_owned(),
        }
    }

    fn visit_binary_expr(&self, binary: &BinaryExpr) -> String {
        let op = format!("{}", binary.operator);
        self.parenthesize(&op, vec![binary.left.as_ref(), binary.right.as_ref()])
    }

    fn visit_unary_expr(&self, unary: &UnaryExpr) -> String {
        let op = format!("{}", unary.operator);
        self.parenthesize(&op, vec![unary.right.as_ref()])
    }

    fn visit_assign_expr(&self, _assign: &AssignExpr) -> String {
        todo!()
    }

    fn visit_call_expr(&self, _call: &CallExpr) -> String {
        todo!()
    }

    fn visit_get_expr(&self, _get: &GetExpr) -> String {
        todo!()
    }

    fn visit_grouping_expr(&self, grouping: &GroupingExpr) -> String {
        self.parenthesize("group", vec![grouping])
    }

    fn visit_logical_expr(&self, _logical: &LogicalExpr) -> String {
        todo!()
    }

    fn visit_set_expr(&self, _set: &SetExpr) -> String {
        todo!()
    }

    fn visit_super_expr(&self, _super_expr: &SuperExpr) -> String {
        todo!()
    }

    fn visit_this_expr(&self, _this: &ThisExpr) -> String {
        todo!()
    }

    fn visit_variable_expr(&self, _variable: &VariableExpr) -> String {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ast_printing_test() {
        // Arrange
        let literal1 = LiteralExpr::new(Some(Token::Number(123.0)));
        let left = UnaryExpr {
            operator: Token::Minus,
            right: Box::new(literal1),
        };
        let literal2 = LiteralExpr::new(Some(Token::Number(45.67)));
        let right = GroupingExpr {
            expression: Box::new(literal2),
        };
        let bin = BinaryExpr {
            left: Box::new(left),
            operator: Token::Star,
            right: Box::new(right),
        };
        let ast_printer = AstPrinter {};

        // Act
        ast_printer.print(bin);
    }
}
