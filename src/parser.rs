use crate::lexer::Token;

// Expressions

pub trait ExprVisitor<R> {
    fn visit_literal(&self, literal: &Literal) -> R;
    fn visit_binary_expr<E: Expr>(&self, binary: &BinaryExpr<E>) -> R;
    fn visit_unary_expr<E: Expr>(&self, binary: &UnaryExpr<E>) -> R;
    fn visit_assign_expr<E: Expr>(&self, binary: &AssignExpr<E>) -> R;
    fn visit_call_expr<E: Expr>(&self, binary: &CallExpr<E>) -> R;
    fn visit_get_expr<E: Expr>(&self, binary: &GetExpr<E>) -> R;
    fn visit_grouping_expr<E: Expr>(&self, binary: &GroupingExpr<E>) -> R;
    fn visit_logical_expr<E: Expr>(&self, binary: &LogicalExpr<E>) -> R;
    fn visit_set_expr<E: Expr>(&self, binary: &SetExpr<E>) -> R;
    fn visit_super_expr(&self, binary: &SuperExpr) -> R;
    fn visit_this_expr(&self, binary: &ThisExpr) -> R;
    fn visit_variable_expr(&self, binary: &VariableExpr) -> R;
}

pub trait Expr {
    fn accept<V: ExprVisitor<R>, R>(&self, visitor: V) -> R;
}

pub struct Literal<'a> {
    pub token: Token<'a>,
}

impl<'a> Literal<'a> {
    pub fn new(token: Token<'a>) -> Self {
        Self { token }
    }
}

impl Expr for Literal<'_> {
    fn accept<V: ExprVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_literal(self)
    }
}

pub struct BinaryExpr<'a, E: Expr> {
    pub operator: Token<'a>,
    pub left: E,
    pub right: E,
}

impl<E: Expr> Expr for BinaryExpr<'_, E> {
    fn accept<V: ExprVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_binary_expr(self)
    }
}

pub struct UnaryExpr<'a, E: Expr> {
    pub operator: Token<'a>,
    pub right: E,
}

impl<E: Expr> Expr for UnaryExpr<'_, E> {
    fn accept<V: ExprVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_unary_expr(self)
    }
}

pub struct AssignExpr<'a, E: Expr> {
    pub name: Token<'a>,
    pub expr: E,
}

impl<E: Expr> Expr for AssignExpr<'_, E> {
    fn accept<V: ExprVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_assign_expr(self)
    }
}

pub struct CallExpr<'a, E: Expr> {
    pub paren: Token<'a>,
    pub callee: E,
    pub args: Vec<E>,
}

impl<E: Expr> Expr for CallExpr<'_, E> {
    fn accept<V: ExprVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_call_expr(self)
    }
}

pub struct GetExpr<'a, E: Expr> {
    pub name: Token<'a>,
    pub object: E,
}

impl<E: Expr> Expr for GetExpr<'_, E> {
    fn accept<V: ExprVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_get_expr(self)
    }
}

pub struct GroupingExpr<E: Expr> {
    pub expression: E,
}

impl<E: Expr> Expr for GroupingExpr<E> {
    fn accept<V: ExprVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_grouping_expr(self)
    }
}

pub struct LogicalExpr<'a, E: Expr> {
    pub operator: Token<'a>,
    pub left: E,
    pub right: E,
}

impl<E: Expr> Expr for LogicalExpr<'_, E> {
    fn accept<V: ExprVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_logical_expr(self)
    }
}

pub struct SetExpr<'a, E: Expr> {
    pub name: Token<'a>,
    pub object: E,
    pub value: E,
}

impl<E: Expr> Expr for SetExpr<'_, E> {
    fn accept<V: ExprVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_set_expr(self)
    }
}

pub struct SuperExpr<'a> {
    pub keyword: Token<'a>,
    pub method: Token<'a>,
}

impl Expr for SuperExpr<'_> {
    fn accept<V: ExprVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_super_expr(self)
    }
}

pub struct ThisExpr<'a> {
    pub keyword: Token<'a>,
}

impl Expr for ThisExpr<'_> {
    fn accept<V: ExprVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_this_expr(self)
    }
}

pub struct VariableExpr<'a> {
    pub name: Token<'a>,
}

impl Expr for VariableExpr<'_> {
    fn accept<V: ExprVisitor<R>, R>(&self, visitor: V) -> R {
        visitor.visit_variable_expr(self)
    }
}

// Statements

pub trait StmtVisitor<R> {
    fn visit_block_stmt<S: Stmt>(&self, binary: &BlockStmt<S>) -> R;
    fn visit_class_stmt<S: Stmt>(&self, binary: &ClassStmt<S>) -> R;
    fn visit_expression_stmt<E: Expr>(&self, binary: &ExpressionStmt<E>) -> R;
    fn visit_function_stmt<S: Stmt>(&self, binary: &FunctionStmt<S>) -> R;
    fn visit_if_stmt<E: Expr, S: Stmt>(&self, binary: &IfStmt<E, S>) -> R;
    fn visit_print_stmt<E: Expr>(&self, binary: &PrintStmt<E>) -> R;
    fn visit_return_stmt<E: Expr>(&self, binary: &ReturnStmt<E>) -> R;
    fn visit_variable_stmt<E: Expr>(&self, binary: &VariableStmt<E>) -> R;
    fn visit_while_stmt<E: Expr, S: Stmt>(&self, binary: &WhileStmt<E, S>) -> R;
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
