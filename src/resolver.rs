use std::collections::HashMap;

use crate::{
    ast::{Expr, ExprKind, ExprVisitor, Interpreter, Stmt, StmtVisitor},
    lexer::Token,
};

pub struct Resolver<'a, W: std::io::Write> {
    interpreter: Interpreter<'a, W>,
    scopes: Vec<HashMap<&'a str, bool>>,
}

impl<'a, W: std::io::Write> Resolver<'a, W> {
    pub fn new(interpreter: Interpreter<'a, W>) -> Self {
        Self {
            interpreter,
            scopes: vec![],
        }
    }

    pub fn interpret(mut self, stmts: &'a [miette::Result<Stmt<'a>>]) -> miette::Result<()> {
        self.resolve_statements(stmts);
        self.interpreter.interpret(stmts)
    }

    fn resolve_statement(&mut self, stmt: &'a miette::Result<Stmt<'a>>) {
        if let Ok(stmt) = stmt {
            stmt.accept(self);
        }
    }

    fn resolve_statements(&mut self, stmts: &'a [miette::Result<Stmt<'a>>]) {
        for stmt in stmts {
            self.resolve_statement(stmt);
        }
    }

    fn resolve_expression(&mut self, expr: &Expr<'a>) {
        expr.accept(self);
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, token: &Token<'a>) {
        self.add_id_to_scope(token, false);
    }

    fn define(&mut self, token: &Token<'a>) {
        self.add_id_to_scope(token, true);
    }

    fn add_id_to_scope(&mut self, token: &Token<'a>, defined: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            if let Token::Identifier(id) = token {
                scope.insert(id, defined);
            }
        }
    }

    fn resolve_local(&mut self, name: &Token<'a>) {
        let id = if let Token::Identifier(name) = name {
            name
        } else {
            return;
        };
        let mut i = self.scopes.len();
        for scope in self.scopes.iter().rev() {
            i -= 1;
            if scope.contains_key(id) {
                self.interpreter.resolve(name, self.scopes.len() - 1 - i);
                return;
            }
        }
    }

    fn resolve_function(
        &mut self,
        params: &[Box<Expr<'a>>],
        body: &'a Result<Stmt<'a>, miette::Error>,
    ) {
        self.begin_scope();
        for p in params {
            if let ExprKind::Variable(p) = &p.kind {
                self.declare(p);
                self.define(p);
            }
        }
        self.resolve_statement(body);
        self.end_scope();
    }
}

impl<'a, W: std::io::Write> StmtVisitor<'a, ()> for Resolver<'a, W> {
    fn visit_block_stmt(&mut self, body: &'a [miette::Result<Stmt<'a>>]) {
        self.begin_scope();
        self.resolve_statements(body);
        self.end_scope();
    }

    fn visit_class_stmt(&self, name: &Token<'a>, superclass: &Stmt<'a>, methods: &[Box<Stmt<'a>>]) {
        let _ = methods;
        let _ = superclass;
        let _ = name;
    }

    fn visit_expression_stmt(&mut self, expr: &Expr<'a>) {
        self.resolve_expression(expr);
    }

    fn visit_function_decl_stmt(
        &mut self,
        token: &Token<'a>,
        params: &[Box<Expr<'a>>],
        body: &'a miette::Result<Stmt<'a>>,
    ) {
        self.declare(token);
        self.define(token);
        self.resolve_function(params, body);
    }

    fn visit_if_stmt(
        &mut self,
        cond: &Expr<'a>,
        then: &'a miette::Result<Stmt<'a>>,
        otherwise: &'a Option<Box<miette::Result<Stmt<'a>>>>,
    ) {
        self.resolve_expression(cond);
        self.resolve_statement(then);
        if let Some(other) = otherwise {
            self.resolve_statement(other);
        }
    }

    fn visit_print_stmt(&mut self, expr: &Expr<'a>) {
        self.resolve_expression(expr);
    }

    fn visit_return_stmt(&mut self, keyword: &Token<'a>, value: &Expr<'a>) {
        let _ = keyword;
        self.resolve_expression(value);
    }

    fn visit_variable_stmt(&mut self, name: &Token<'a>, initializer: &Option<Box<Expr<'a>>>) {
        self.declare(name);
        if let Some(init) = initializer {
            self.resolve_expression(init);
        }
        self.define(name);
    }

    fn visit_while_stmt(&mut self, cond: &Expr<'a>, body: &'a miette::Result<Stmt<'a>>) {
        self.resolve_expression(cond);
        self.resolve_statement(body);
    }
}

impl<'a, W: std::io::Write> ExprVisitor<'a, ()> for Resolver<'a, W> {
    fn visit_literal(&self, token: &Option<crate::lexer::Token<'a>>) {
        let _ = token;
    }

    fn visit_binary_expr(&mut self, operator: &Token<'a>, left: &Expr<'a>, right: &Expr<'a>) {
        let _ = operator;
        self.resolve_expression(left);
        self.resolve_expression(right);
    }

    fn visit_unary_expr(&mut self, operator: &Token<'a>, expr: &Expr<'a>) {
        let _ = operator;
        self.resolve_expression(expr);
    }

    fn visit_assign_expr(&mut self, name: &Token<'a>, value: &Expr<'a>) {
        self.resolve_expression(value);
        self.resolve_local(name);
    }

    fn visit_call_expr(&mut self, paren: &Token<'a>, callee: &Expr<'a>, args: &[Box<Expr<'a>>]) {
        let _ = paren;
        self.resolve_expression(callee);
        for arg in args {
            self.resolve_expression(arg);
        }
    }

    fn visit_get_expr(&mut self, name: &Token<'a>, object: &Expr<'a>) {
        let _ = object;
        let _ = name;
    }

    fn visit_grouping_expr(&mut self, grouping: &Expr<'a>) {
        self.resolve_expression(grouping);
    }

    fn visit_logical_expr(&mut self, operator: &Token<'a>, left: &Expr<'a>, right: &Expr<'a>) {
        let _ = right;
        let _ = left;
        let _ = operator;
    }

    fn visit_set_expr(&mut self, name: &Token<'a>, obj: &Expr<'a>, val: &Expr<'a>) {
        let _ = val;
        let _ = obj;
        let _ = name;
    }

    fn visit_super_expr(&mut self, keyword: &Token<'a>, method: &Token<'a>) {
        let _ = method;
        let _ = keyword;
    }

    fn visit_this_expr(&mut self, keyword: &Token<'a>) {
        let _ = keyword;
    }

    fn visit_variable_expr(&mut self, name: &Token<'a>) {
        if let Some(scope) = self.scopes.last() {
            if let Token::Identifier(id) = name {
                if let Some(defined) = scope.get(id) {
                    if !(*defined) {
                        // TODO: Error here
                    }
                }
            }
        }
        self.resolve_local(name);
    }
}
