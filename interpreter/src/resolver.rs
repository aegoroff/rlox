#![allow(clippy::missing_errors_doc)]

use std::collections::{HashMap, HashSet};

use crate::{
    LoxError,
    ast::{Expr, ExprKind, ExprVisitor, FunctionKind, Stmt, StmtVisitor},
    int::Interpreter,
    lexer::{SUPER, THIS, Token},
};
use miette::{LabeledSpan, miette};

#[derive(Debug, Clone, Copy)]
enum ClassKind {
    None = 0,
    Class = 1,
    Subclass = 2,
}

pub struct Resolver<'a, W: std::io::Write> {
    interpreter: Interpreter<'a, W>,
    scopes: Vec<HashMap<&'a str, bool>>,
    current_function: FunctionKind,
    current_class: ClassKind,
}

impl<'a, W: std::io::Write> Resolver<'a, W> {
    pub fn new(interpreter: Interpreter<'a, W>) -> Self {
        Self {
            interpreter,
            scopes: vec![],
            current_function: FunctionKind::None,
            current_class: ClassKind::None,
        }
    }

    pub fn interpret(mut self, stmts: &'a [crate::Result<Stmt<'a>>]) -> crate::Result<()> {
        self.resolve_statements(stmts)?;
        self.interpreter.interpret(stmts)
    }

    fn resolve_statement(&mut self, stmt: &'a crate::Result<Stmt<'a>>) -> crate::Result<()> {
        if let Ok(stmt) = stmt {
            stmt.accept(self)
        } else {
            Err(LoxError::Error(miette!("Failed to resolve statement")))
        }
    }

    fn resolve_statements(
        &mut self,
        statements: &'a [crate::Result<Stmt<'a>>],
    ) -> crate::Result<()> {
        let mut errors = vec![];

        let mut spans = HashSet::new();
        let mut add_error = |e: LoxError| {
            if let LoxError::Error(report) = e {
                if let Some(label) = report.labels() {
                    for l in label {
                        if !spans.contains(&(l.len(), l.offset())) {
                            spans.insert((l.len(), l.offset()));
                            errors.push(l);
                        }
                    }
                }
            }
        };

        for stmt in statements {
            if let Err(e) = self.resolve_statement(stmt) {
                add_error(e);
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(LoxError::Error(miette!(
                labels = errors,
                "Program completed with errors"
            )))
        }
    }

    fn resolve_expression(&mut self, expr: &Expr<'a>) -> crate::Result<()> {
        expr.accept(self)
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

    fn resolve_local(&mut self, value: &Expr<'a>, name: &Token<'a>) {
        let id = match name {
            Token::Identifier(id) => id,
            Token::This => THIS,
            Token::Super => SUPER,
            _ => {
                return;
            }
        };
        let mut i = self.scopes.len();
        for scope in self.scopes.iter().rev() {
            i -= 1;
            if scope.contains_key(id) {
                let depth = self.scopes.len() - 1 - i;
                self.interpreter.resolve(value, depth);
                return;
            }
        }
    }

    fn resolve_function(
        &mut self,
        params: &[Box<Expr<'a>>],
        body: &'a crate::Result<Stmt<'a>>,
        kind: FunctionKind,
    ) -> crate::Result<()> {
        let enclosing_function = self.current_function;
        self.begin_scope();
        for p in params {
            if let ExprKind::Variable(p) = &p.kind {
                self.declare(p);
                self.define(p);
            }
        }
        self.current_function = kind;
        self.resolve_statement(body)?;
        self.end_scope();
        self.current_function = enclosing_function;
        Ok(())
    }
}

impl<'a, W: std::io::Write> StmtVisitor<'a, crate::Result<()>> for Resolver<'a, W> {
    fn visit_block_stmt(&mut self, body: &'a [crate::Result<Stmt<'a>>]) -> crate::Result<()> {
        self.begin_scope();
        self.resolve_statements(body)?;
        self.end_scope();
        Ok(())
    }

    fn visit_class_stmt(
        &mut self,
        name: &Token<'a>,
        superclass: &Option<Box<Expr<'a>>>,
        methods: &'a [crate::Result<Stmt<'a>>],
    ) -> crate::Result<()> {
        let enclosing_class = self.current_class;
        self.current_class = ClassKind::Class;
        self.declare(name);
        self.define(name);
        if let Some(superclass) = superclass {
            self.current_class = ClassKind::Subclass;
            self.resolve_expression(superclass)?;
            if let ExprKind::Variable(Token::Identifier(super_name)) = &superclass.kind {
                if let Token::Identifier(name) = name {
                    if *super_name == *name {
                        return Err(LoxError::Error(miette!(
                            labels = vec![LabeledSpan::at(
                                superclass.location.clone(),
                                "A class cannot inherit from itself"
                            )],
                            "Invalid superclass"
                        )));
                    }
                }
            }
            self.begin_scope();
            self.define(&Token::Identifier(SUPER));
        }
        self.begin_scope();
        self.define(&Token::Identifier(THIS));

        for method in methods {
            self.resolve_statement(method)?;
        }
        self.end_scope();
        if superclass.is_some() {
            self.end_scope();
        }
        self.current_class = enclosing_class;
        Ok(())
    }

    fn visit_expression_stmt(&mut self, expr: &Expr<'a>) -> crate::Result<()> {
        self.resolve_expression(expr)
    }

    fn visit_function_decl_stmt(
        &mut self,
        kind: FunctionKind,
        token: &Token<'a>,
        params: &[Box<Expr<'a>>],
        body: &'a crate::Result<Stmt<'a>>,
    ) -> crate::Result<()> {
        self.declare(token);
        self.define(token);
        self.resolve_function(params, body, kind)
    }

    fn visit_if_stmt(
        &mut self,
        cond: &Expr<'a>,
        then: &'a crate::Result<Stmt<'a>>,
        otherwise: &'a Option<Box<crate::Result<Stmt<'a>>>>,
    ) -> crate::Result<()> {
        self.resolve_expression(cond)?;
        self.resolve_statement(then)?;
        if let Some(other) = otherwise {
            self.resolve_statement(other)?;
        }
        Ok(())
    }

    fn visit_print_stmt(&mut self, expr: &Expr<'a>) -> crate::Result<()> {
        self.resolve_expression(expr)
    }

    fn visit_return_stmt(&mut self, keyword: &Token<'a>, value: &Expr<'a>) -> crate::Result<()> {
        let _ = keyword;
        match self.current_function {
            FunctionKind::None => Err(LoxError::Error(miette!(
                labels = vec![LabeledSpan::at(
                    value.location.clone(),
                    "Cannot return from top level code"
                )],
                "Syntax error"
            ))),
            FunctionKind::Initializer => {
                // TODO: print error
                self.resolve_expression(value)
            }
            _ => self.resolve_expression(value),
        }
    }

    fn visit_variable_stmt(
        &mut self,
        name: &Token<'a>,
        initializer: &Option<Box<Expr<'a>>>,
    ) -> crate::Result<()> {
        self.declare(name);
        if let Some(init) = initializer {
            self.resolve_expression(init)?;
        }
        self.define(name);
        Ok(())
    }

    fn visit_while_stmt(
        &mut self,
        cond: &Expr<'a>,
        body: &'a crate::Result<Stmt<'a>>,
    ) -> crate::Result<()> {
        self.resolve_expression(cond)?;
        self.resolve_statement(body)?;
        Ok(())
    }
}

impl<'a, W: std::io::Write> ExprVisitor<'a, crate::Result<()>> for Resolver<'a, W> {
    fn visit_literal(&self, token: &Option<Token<'a>>) -> crate::Result<()> {
        let _ = token;
        Ok(())
    }

    fn visit_binary_expr(
        &mut self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> crate::Result<()> {
        let _ = operator;
        self.resolve_expression(left)?;
        self.resolve_expression(right)?;
        Ok(())
    }

    fn visit_unary_expr(&mut self, operator: &Token<'a>, expr: &Expr<'a>) -> crate::Result<()> {
        let _ = operator;
        self.resolve_expression(expr)
    }

    fn visit_assign_expr(&mut self, lhs: &Expr<'a>, rhs: &Expr<'a>) -> crate::Result<()> {
        self.resolve_expression(rhs)?;
        if let ExprKind::Variable(name) = &lhs.kind {
            self.resolve_local(lhs, name);
        }
        Ok(())
    }

    fn visit_call_expr(
        &mut self,
        paren: &Token<'a>,
        callee: &Expr<'a>,
        args: &[Box<Expr<'a>>],
    ) -> crate::Result<()> {
        let _ = paren;
        self.resolve_expression(callee)?;
        for arg in args {
            self.resolve_expression(arg)?;
        }
        Ok(())
    }

    fn visit_get_expr(&mut self, name: &Token<'a>, object: &Expr<'a>) -> crate::Result<()> {
        self.resolve_expression(object)?;
        let _ = name;
        Ok(())
    }

    fn visit_grouping_expr(&mut self, grouping: &Expr<'a>) -> crate::Result<()> {
        self.resolve_expression(grouping)
    }

    fn visit_logical_expr(
        &mut self,
        operator: &Token<'a>,
        left: &Expr<'a>,
        right: &Expr<'a>,
    ) -> crate::Result<()> {
        let _ = right;
        let _ = left;
        let _ = operator;
        Ok(())
    }

    fn visit_set_expr(
        &mut self,
        _: &Token<'a>,
        obj: &Expr<'a>,
        val: &Expr<'a>,
    ) -> crate::Result<()> {
        self.resolve_expression(val)?;
        self.resolve_expression(obj)?;
        Ok(())
    }

    fn visit_super_expr(
        &mut self,
        obj: &Expr<'a>,
        keyword: &Token<'a>,
        method: &Token<'a>,
    ) -> crate::Result<()> {
        let _ = method;
        if let ClassKind::None = self.current_class {
            Err(LoxError::Error(miette!(
                labels = vec![LabeledSpan::at(
                    obj.location.clone(),
                    "Can't use 'super' outside of a class."
                )],
                "Syntax error"
            )))
        } else if let ClassKind::Class = self.current_class {
            Err(LoxError::Error(miette!(
                labels = vec![LabeledSpan::at(
                    obj.location.clone(),
                    "Can't use 'super' in a class with no superclass."
                )],
                "Syntax error"
            )))
        } else {
            self.resolve_local(obj, keyword);
            Ok(())
        }
    }

    fn visit_this_expr(&mut self, obj: &Expr<'a>, keyword: &Token<'a>) -> crate::Result<()> {
        if let ClassKind::None = self.current_class {
            Err(LoxError::Error(miette!(
                labels = vec![LabeledSpan::at(
                    obj.location.clone(),
                    "Cannot use 'this' outside of a class"
                )],
                "Syntax error"
            )))
        } else {
            self.resolve_local(obj, keyword);
            Ok(())
        }
    }

    fn visit_variable_expr(&mut self, obj: &Expr<'a>, name: &Token<'a>) -> crate::Result<()> {
        if let Some(scope) = self.scopes.last() {
            if let Token::Identifier(id) = name {
                if let Some(defined) = scope.get(id) {
                    if !(*defined) {
                        return Err(LoxError::Error(miette!(
                            labels = vec![LabeledSpan::at(
                                obj.location.clone(),
                                format!("Cant read local variable '{id}' in its own initializer")
                            )],
                            "Variable is not defined"
                        )));
                    }
                }
            }
        }
        self.resolve_local(obj, name);
        Ok(())
    }
}
