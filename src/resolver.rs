use std::collections::HashMap;

use crate::{
    ast::{Expr, ExprKind, ExprVisitor, Interpreter, StmtVisitor},
    lexer::Token,
};

pub struct Resolver<'a, W: std::io::Write> {
    interpreter: &'a mut Interpreter<'a, W>,
    scopes: Vec<HashMap<&'a str, bool>>,
}

impl<'a, W: std::io::Write> Resolver<'a, W> {
    pub fn new(interpreter: &'a mut Interpreter<'a, W>) -> Self {
        Self {
            interpreter,
            scopes: vec![],
        }
    }

    pub fn resolve_statement(&mut self, stmt: &'a miette::Result<crate::ast::Stmt<'a>>) {
        if let Ok(stmt) = stmt {
            stmt.accept(self);
        }
    }

    pub fn resolve_statements(&mut self, stmts: &'a [miette::Result<crate::ast::Stmt<'a>>]) {
        for stmt in stmts {
            self.resolve_statement(stmt);
        }
    }

    pub fn resolve_expression(&mut self, expr: &Expr<'a>) {
        expr.accept(self);
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, token: &Token<'a>) {
        if let Some(scope) = self.scopes.last_mut() {
            if let Token::Identifier(id) = token {
                scope.insert(id, false);
            }
        }
    }

    fn define(&mut self, token: &Token<'a>) {
        if let Some(scope) = self.scopes.last_mut() {
            if let Token::Identifier(id) = token {
                scope.insert(id, true);
            }
        }
    }

    fn resolve_local(&mut self, expr: &Expr<'a>, name: &Token<'a>) {
        let mut i = self.scopes.len();
        for scope in self.scopes.iter().rev() {
            let name = if let Token::Identifier(name) = name {
                name
            } else {
                break;
            };
            i -= 1;
            if scope.contains_key(name) {
                self.interpreter.resolve(expr, self.scopes.len() - 1 - i);
                return;
            }
        }
    }
}

impl<'a, W: std::io::Write> StmtVisitor<'a, ()> for Resolver<'a, W> {
    fn visit_block_stmt(&mut self, body: &'a [miette::Result<crate::ast::Stmt<'a>>]) {
        self.begin_scope();
        self.resolve_statements(body);
        self.end_scope();
    }

    fn visit_class_stmt(
        &self,
        name: &crate::lexer::Token<'a>,
        superclass: &crate::ast::Stmt<'a>,
        methods: &[Box<crate::ast::Stmt<'a>>],
    ) {
        let _ = methods;
        let _ = superclass;
        let _ = name;
    }

    fn visit_expression_stmt(&mut self, expr: &crate::ast::Expr<'a>) {
        self.resolve_expression(expr);
    }

    fn visit_function_decl_stmt(
        &mut self,
        token: &crate::lexer::Token<'a>,
        params: &[Box<crate::ast::Expr<'a>>],
        body: &'a miette::Result<crate::ast::Stmt<'a>>,
    ) {
        self.declare(token);
        self.define(token);
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

    fn visit_if_stmt(
        &mut self,
        cond: &crate::ast::Expr<'a>,
        then: &'a miette::Result<crate::ast::Stmt<'a>>,
        otherwise: &'a Option<Box<miette::Result<crate::ast::Stmt<'a>>>>,
    ) {
        let _ = otherwise;
        let _ = then;
        let _ = cond;
    }

    fn visit_print_stmt(&mut self, expr: &crate::ast::Expr<'a>) {
        let _ = expr;
    }

    fn visit_return_stmt(
        &mut self,
        keyword: &crate::lexer::Token<'a>,
        value: &crate::ast::Expr<'a>,
    ) {
        let _ = value;
        let _ = keyword;
    }

    fn visit_variable_stmt(
        &mut self,
        name: &crate::lexer::Token<'a>,
        initializer: &Option<Box<crate::ast::Expr<'a>>>,
    ) {
        self.declare(name);
        if let Some(init) = initializer {
            self.resolve_expression(init);
        }
        self.define(name);
    }

    fn visit_while_stmt(
        &mut self,
        cond: &crate::ast::Expr<'a>,
        body: &'a miette::Result<crate::ast::Stmt<'a>>,
    ) {
        let _ = body;
        let _ = cond;
    }
}

impl<'a, W: std::io::Write> ExprVisitor<'a, ()> for Resolver<'a, W> {
    fn visit_literal(&self, token: &Option<crate::lexer::Token<'a>>) {
        let _ = token;
    }

    fn visit_binary_expr(
        &mut self,
        operator: &crate::lexer::Token<'a>,
        left: &crate::ast::Expr<'a>,
        right: &crate::ast::Expr<'a>,
    ) {
        let _ = right;
        let _ = left;
        let _ = operator;
    }

    fn visit_unary_expr(
        &mut self,
        operator: &crate::lexer::Token<'a>,
        expr: &crate::ast::Expr<'a>,
    ) {
        let _ = expr;
        let _ = operator;
    }

    fn visit_assign_expr(&mut self, name: &crate::lexer::Token<'a>, value: &crate::ast::Expr<'a>) {
        self.resolve_expression(value);
        self.resolve_local(value, name);
    }

    fn visit_call_expr(
        &mut self,
        paren: &crate::lexer::Token<'a>,
        callee: &crate::ast::Expr<'a>,
        args: &[Box<crate::ast::Expr<'a>>],
    ) {
        let _ = args;
        let _ = callee;
        let _ = paren;
    }

    fn visit_get_expr(&mut self, name: &crate::lexer::Token<'a>, object: &crate::ast::Expr<'a>) {
        let _ = object;
        let _ = name;
    }

    fn visit_grouping_expr(&mut self, grouping: &crate::ast::Expr<'a>) {
        let _ = grouping;
    }

    fn visit_logical_expr(
        &mut self,
        operator: &crate::lexer::Token<'a>,
        left: &crate::ast::Expr<'a>,
        right: &crate::ast::Expr<'a>,
    ) {
        let _ = right;
        let _ = left;
        let _ = operator;
    }

    fn visit_set_expr(
        &mut self,
        name: &crate::lexer::Token<'a>,
        obj: &crate::ast::Expr<'a>,
        val: &crate::ast::Expr<'a>,
    ) {
        let _ = val;
        let _ = obj;
        let _ = name;
    }

    fn visit_super_expr(
        &mut self,
        keyword: &crate::lexer::Token<'a>,
        method: &crate::lexer::Token<'a>,
    ) {
        let _ = method;
        let _ = keyword;
    }

    fn visit_this_expr(&mut self, keyword: &crate::lexer::Token<'a>) {
        let _ = keyword;
    }

    fn visit_variable_expr(&mut self, name: &crate::lexer::Token<'a>) {
        let _ = name;
    }
}
