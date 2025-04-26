use std::{iter::Peekable, ops::RangeInclusive};

use miette::{LabeledSpan, miette};

use crate::{
    ast::{Expr, ExprKind, Stmt, StmtKind},
    lexer::{Lexer, Token},
};

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
}

impl<'a> Iterator for &mut Parser<'a> {
    type Item = miette::Result<Stmt<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.declaration()
    }
}

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(content: &'a str) -> Self {
        Self {
            tokens: Lexer::new(content).peekable(),
        }
    }

    fn declaration(&mut self) -> Option<miette::Result<Stmt<'a>>> {
        let current = self.tokens.peek()?;
        match current {
            Ok((_, Token::Var, _)) => self.var_declaration(),
            Ok((_, Token::Class, _)) => self.class_declaration(),
            Ok((_, Token::Fun, _)) => self.function("function"),
            _ => self.statement(),
        }
    }

    fn function(&mut self, kind: &'static str) -> Option<miette::Result<Stmt<'a>>> {
        let t = self.tokens.next(); // consume FUN token TODO: include FUN start position into stmt location
        let (start, _, finish) = t.unwrap().unwrap(); // TODO: handle error

        let name = match self.consume_name(kind, start, finish) {
            Ok(name) => name,
            Err(e) => return Some(Err(e)),
        };

        if let Err(e) = self.consume(Token::LeftParen) {
            return Some(Err(e));
        };
        let mut args = vec![];
        if !self.matches(&[Token::RightParen]) {
            loop {
                let (arg_start, arg, arg_end) = match self.tokens.next() {
                    Some(Ok(expr)) => expr,
                    Some(Err(e)) => return Some(Err(e)),
                    None => return None,
                };

                if args.len() >= 255 {
                    return Some(Err(miette!(
                        labels = vec![LabeledSpan::at(
                            start..=finish,
                            "Cant have more then 255 arguments"
                        )],
                        "Arguments number exceeded"
                    )));
                }
                if let Token::Identifier(_) = arg {
                    let arg = Expr {
                        kind: ExprKind::Variable(arg),
                        location: arg_start..=arg_end,
                    };
                    args.push(Box::new(arg));
                } else {
                    return Some(Err(miette!(
                        labels = vec![LabeledSpan::at(
                            arg_start..=arg_end,
                            "Invalid argument definition"
                        )],
                        "Invalid argument definition"
                    )));
                }

                if !self.matches(&[Token::Comma]) {
                    break;
                }
            }
            if let Err(e) = self.consume(Token::RightParen) {
                return Some(Err(e));
            }
        }

        let Some(left_paren) = self.tokens.peek() else {
            return Some(Err(miette!(
                labels = vec![LabeledSpan::at(
                    start..=finish,
                    format!("{kind} block expected after this")
                )],
                "Missing {kind} block"
            )));
        };

        if let Err(e) = left_paren {
            return Some(Err(miette!(
                labels = vec![LabeledSpan::at(start..=finish, e.to_string())],
                "Missing {kind} block"
            )));
        }

        let block = match self.block() {
            Some(b) => b,
            None => {
                return Some(Err(miette!(
                    labels = vec![LabeledSpan::at(
                        start..=finish,
                        format!("Missing {kind} block")
                    )],
                    "Missing {kind} block"
                )));
            }
        };

        let kind = StmtKind::Function(name?, args, Box::new(block));

        Some(Ok(Stmt {
            kind,
            location: start..=finish,
        }))
    }

    fn class_declaration(&mut self) -> Option<miette::Result<Stmt<'a>>> {
        let t = self.tokens.next(); // consume CLASS token TODO: include CLASS start position into stmt location
        let (start, _, finish) = t.unwrap().unwrap(); // TODO: handle error
        let name = match self.consume_name("class", start, finish) {
            Ok(name) => name,
            Err(e) => return Some(Err(e)),
        };

        let Some(name) = name else {
            return Some(Err(miette!(
                labels = vec![LabeledSpan::at(
                    start..=finish,
                    "class name expected after this"
                )],
                "Missing class name"
            )));
        };

        if let Err(e) = self.consume(Token::LeftBrace) {
            return Some(Err(e));
        };

        let mut methods = vec![];
        if !self.matches(&[Token::RightBrace]) {
            loop {
                let method = self.function("method")?;
                methods.push(method);

                if self.matches(&[Token::RightBrace]) {
                    break;
                }
            }
        }
        let kind = StmtKind::Class(name, None, methods);

        Some(Ok(Stmt {
            kind,
            location: start..=finish, // TODO: include CLASS finish position into stmt location
        }))
    }

    fn var_declaration(&mut self) -> Option<miette::Result<Stmt<'a>>> {
        let t = self.tokens.next(); // consume VAR token TODO: include VAR start position into stmt location
        let (start, _, mut finish) = t.unwrap().unwrap(); // TODO: handle error
        // IMPORTANT: dont call expression here so as not to conflict with assignment
        let name = match self.or_expression() {
            Some(result) => match result {
                Ok(expr) => {
                    finish = *expr.location.end();
                    match expr.kind {
                        ExprKind::Variable(token) => Ok(token),
                        _ => Err(miette!("Invalid variable name")),
                    }
                }
                Err(e) => Err(e),
            },
            None => Err(miette!(
                labels = vec![LabeledSpan::at(
                    start..=finish,
                    "Variable name expected after this"
                )],
                "Missing variable name"
            )),
        };
        let name = match name {
            Ok(name) => name,
            Err(e) => return Some(Err(e)),
        };
        // if var without initializer check semicolon
        if self.tokens.peek().is_none() {
            if let Err(e) = self.consume_semicolon(finish) {
                return Some(Err(e));
            }
        }
        let current = self.tokens.peek()?;
        let stmt = if let Ok((_, Token::Equal, _)) = current {
            self.tokens.next(); // consume EQUAL token
            if let Some(initializer) = self.expression() {
                match initializer {
                    Ok(initializer) => {
                        let end = *initializer.location.end();
                        finish = end;
                        let kind = StmtKind::Variable(name, Some(Box::new(initializer)));
                        Ok(Stmt {
                            kind,
                            location: start..=end,
                        })
                    }
                    Err(e) => Err(e),
                }
            } else {
                Err(miette!(
                    labels = vec![LabeledSpan::at(
                        start..=finish,
                        "Variable name expected here"
                    )],
                    "Missing variable name"
                ))
            }
        } else {
            // Variable declaration without initializer
            let kind = StmtKind::Variable(name, None);
            Ok(Stmt {
                kind,
                location: start..=finish,
            })
        };
        if let Err(e) = self.consume_semicolon(finish) {
            Some(Err(e))
        } else {
            Some(stmt)
        }
    }

    fn statement(&mut self) -> Option<miette::Result<Stmt<'a>>> {
        let current = self.tokens.peek()?;
        match current {
            Ok((_, Token::If, _)) => self.if_statement(),
            Ok((_, Token::While, _)) => self.while_statement(),
            Ok((_, Token::For, _)) => self.for_statement(),
            Ok((_, Token::Print, _)) => self.print_statement(),
            Ok((_, Token::Return, _)) => self.return_statement(),
            Ok((_, Token::LeftBrace, _)) => self.block(),
            _ => self.expr_statement(),
        }
    }

    fn if_statement(&mut self) -> Option<miette::Result<Stmt<'a>>> {
        let start_loc = match self.consume_current_and_open_paren("if") {
            Ok(x) => x,
            Err(e) => return Some(Err(e)),
        };

        let Some(cond) = self.expression() else {
            return Some(Err(miette!(
                labels = vec![LabeledSpan::at(
                    (*start_loc.start() + 2)..=(start_loc.start() + 2),
                    "Condition must start here"
                )],
                "Invalid condition syntax"
            )));
        };
        let cond = match cond {
            Ok(cond) => cond,
            Err(e) => return Some(Err(e)),
        };

        let cond_end = *cond.location.end();

        let right_paren_location = match self.consume_right_parent(cond_end) {
            Ok(loc) => loc,
            Err(e) => return Some(Err(e)),
        };

        let Some(then_branch) = self.statement() else {
            return Some(Err(miette!(
                labels = vec![LabeledSpan::at(right_paren_location, "Missing then branch")],
                "Missing then branch"
            )));
        };

        let Some(current) = self.tokens.peek() else {
            let kind = StmtKind::If(Box::new(cond), Box::new(then_branch), None);
            return Some(Ok(Stmt {
                kind,
                location: *start_loc.start()..=*right_paren_location.end(),
            }));
        };

        let Ok((_, next_tok, _)) = current else {
            // Consume token if it's not a valid
            match self.tokens.next()? {
                Ok(_) => unreachable!(),
                Err(e) => return Some(Err(e)),
            }
        };

        if !matches!(next_tok, Token::Else) {
            let kind = StmtKind::If(Box::new(cond), Box::new(then_branch), None);
            return Some(Ok(Stmt {
                kind,
                location: *start_loc.start()..=*right_paren_location.end(),
            }));
        }

        let else_loc = self.consume(Token::Else).unwrap();

        let Some(else_branch) = self.statement() else {
            return Some(Err(miette!(
                labels = vec![LabeledSpan::at(else_loc, "Else branch expected")],
                "Missing else branch"
            )));
        };

        let kind = StmtKind::If(
            Box::new(cond),
            Box::new(then_branch),
            Some(Box::new(else_branch)),
        );
        Some(Ok(Stmt {
            kind,
            location: *start_loc.start()..=*else_loc.end(),
        }))
    }

    fn while_statement(&mut self) -> Option<miette::Result<Stmt<'a>>> {
        let start_loc = match self.consume_current_and_open_paren("while") {
            Ok(x) => x,
            Err(e) => return Some(Err(e)),
        };

        let Some(cond) = self.expression() else {
            return Some(Err(miette!(
                labels = vec![LabeledSpan::at(
                    *start_loc.start()..=(*start_loc.start() + 5),
                    "No while condition specified"
                )],
                "Invalid condition syntax"
            )));
        };
        let cond = match cond {
            Ok(cond) => cond,
            Err(e) => return Some(Err(e)),
        };

        let cond_end = *cond.location.end();

        let right_paren_location = match self.consume_right_parent(cond_end) {
            Ok(loc) => loc,
            Err(e) => return Some(Err(e)),
        };

        let Some(body) = self.statement() else {
            return Some(Err(miette!(
                labels = vec![LabeledSpan::at(right_paren_location, "Missing while body")],
                "Missing while body"
            )));
        };

        let kind = StmtKind::While(Box::new(cond), Box::new(body));
        Some(Ok(Stmt {
            kind,
            location: *start_loc.start()..=*right_paren_location.end(),
        }))
    }

    fn for_statement(&mut self) -> Option<miette::Result<Stmt<'a>>> {
        let start_loc = match self.consume_current_and_open_paren("for") {
            Ok(x) => x,
            Err(e) => return Some(Err(e)),
        };

        let initializer = if self.consume_semicolon(*start_loc.end()).is_ok() {
            None
        } else if let Some(Ok((_, Token::Var, _))) = self.tokens.peek() {
            self.var_declaration()
        } else {
            self.expr_statement()
        };

        let condition = if self.consume_semicolon(*start_loc.end()).is_ok() {
            Expr {
                kind: ExprKind::Literal(Some(Token::True)),
                location: start_loc.clone(),
            }
        } else {
            let Some(cond) = self.expression() else {
                return Some(Err(miette!(
                    labels = vec![LabeledSpan::at(
                        *start_loc.start()..=(*start_loc.start() + 5),
                        "No condition specified"
                    )],
                    "Invalid condition syntax"
                )));
            };
            if let Err(e) = self.consume_semicolon(*start_loc.end()) {
                return Some(Err(e));
            }
            match cond {
                Ok(cond) => cond,
                Err(e) => return Some(Err(e)),
            }
        };

        let increment = if let Some(Ok((_, Token::RightParen, _))) = self.tokens.peek() {
            None
        } else {
            self.expression()
        };

        let right_paren_location = match self.consume_right_parent(*start_loc.end()) {
            Ok(loc) => loc,
            Err(e) => return Some(Err(e)),
        };

        let Some(body) = self.statement() else {
            return Some(Err(miette!(
                labels = vec![LabeledSpan::at(right_paren_location, "Missing for body")],
                "Missing body"
            )));
        };

        let mut statements = vec![body];
        if let Some(Ok(incr)) = increment {
            let location = incr.location.clone();
            let kind = StmtKind::Expression(Box::new(incr));
            statements.push(Ok(Stmt { kind, location }));
        }

        let while_body = Ok(Stmt {
            kind: StmtKind::Block(statements),
            location: *start_loc.start()..=*start_loc.start(),
        });

        let while_stmt = Ok(Stmt {
            kind: StmtKind::While(Box::new(condition), Box::new(while_body)),
            location: *start_loc.start()..=*right_paren_location.end(),
        });

        if let Some(initializer) = initializer {
            let statements = vec![initializer, while_stmt];
            Some(Ok(Stmt {
                kind: StmtKind::Block(statements),
                location: *start_loc.start()..=*start_loc.start(),
            }))
        } else {
            Some(while_stmt)
        }
    }

    fn print_statement(&mut self) -> Option<miette::Result<Stmt<'a>>> {
        self.tokens.next(); // consume print token TODO: include print start position into stmt location
        match self.semicolon_terminated_expression()? {
            Ok(expr) => {
                let location = expr.location.clone();
                let kind = StmtKind::Print(Box::new(expr));
                Some(Ok(Stmt { kind, location }))
            }
            Err(e) => Some(Err(e)),
        }
    }

    fn return_statement(&mut self) -> Option<miette::Result<Stmt<'a>>> {
        if let Some(Ok((start, keyword, end))) = self.tokens.next() {
            let expr = if self.matches(&[Token::Semicolon]) {
                Expr {
                    kind: ExprKind::Literal(None),
                    location: start..=end,
                }
            } else {
                let Some(Ok(expr)) = self.semicolon_terminated_expression() else {
                    return None;
                };
                expr
            };
            let location = start..=*expr.location.end();
            let kind = StmtKind::Return(keyword, Box::new(expr));
            Some(Ok(Stmt { kind, location }))
        } else {
            None
        }
    }

    fn expr_statement(&mut self) -> Option<miette::Result<Stmt<'a>>> {
        match self.semicolon_terminated_expression()? {
            Ok(expr) => {
                let location = expr.location.clone();
                let kind = StmtKind::Expression(Box::new(expr));
                Some(Ok(Stmt { kind, location }))
            }
            Err(e) => Some(Err(e)),
        }
    }

    fn block(&mut self) -> Option<miette::Result<Stmt<'a>>> {
        let (start, _, mut finish) = self.tokens.next().unwrap().unwrap(); // consume '{' token TODO: handle error
        let mut statements = vec![];
        loop {
            if self.matches(&[Token::RightBrace]) {
                break;
            }

            if let Some(opt) = self.declaration() {
                match opt {
                    Ok(stmt) => {
                        finish = *stmt.location.end();
                        statements.push(Ok(stmt));
                    }
                    Err(e) => statements.push(Err(e)),
                }
            }
        }
        let kind = StmtKind::Block(statements);
        Some(Ok(Stmt {
            kind,
            location: start..=finish,
        }))
    }

    fn semicolon_terminated_expression(&mut self) -> Option<miette::Result<Expr<'a>>> {
        match self.expression()? {
            Ok(expr) => {
                let pos = expr.location.end();
                if let Err(e) = self.consume_semicolon(*pos) {
                    return Some(Err(e));
                }

                Some(Ok(expr))
            }
            Err(e) => Some(Err(e)),
        }
    }

    fn consume_name(
        &mut self,
        kind: &str,
        start: usize,
        finish: usize,
    ) -> miette::Result<Option<Token<'a>>> {
        // IMPORTANT: dont call expression here so as not to conflict with assignment
        let name = match self.primary() {
            Some(result) => match result {
                Ok(expr) => match expr.kind {
                    ExprKind::Literal(token) => Ok(token),
                    ExprKind::Variable(token) => Ok(Some(token)),
                    _ => Err(miette!("Invalid {kind} name")),
                },
                Err(e) => Err(e),
            },
            None => Err(miette!(
                labels = vec![LabeledSpan::at(
                    start..=finish,
                    format!("{kind} name expected after this")
                )],
                "Missing {kind} name"
            )),
        };
        let name = name?;
        Ok(name)
    }

    fn consume_semicolon(&mut self, position: usize) -> miette::Result<()> {
        if !self.matches(&[Token::Semicolon]) {
            return Err(miette!(
                labels = vec![LabeledSpan::at(
                    position..=position,
                    "Semicolon expected here"
                )],
                "Missing semicolon"
            ));
        }
        Ok(())
    }

    fn expression(&mut self) -> Option<miette::Result<Expr<'a>>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let lhs = self.or_expression();

        if !self.matches(&[Token::Equal]) {
            return lhs;
        }

        if let Some(rhs) = self.assignment() {
            if let Some(Ok(lhs)) = lhs {
                let start = *lhs.location.start();
                let lhs_finish = *lhs.location.end();
                match rhs {
                    Ok(rhs) => {
                        let rhs_finish = *rhs.location.end();
                        match &lhs.kind {
                            ExprKind::Variable(token) => match token {
                                Token::Identifier(id) => {
                                    let kind =
                                        ExprKind::Assign(Token::Identifier(id), Box::new(rhs));
                                    Some(Ok(Expr {
                                        kind,
                                        location: start..=rhs_finish,
                                    }))
                                }
                                _ => todo!(),
                            },
                            _ => Some(Err(miette!(
                                labels = vec![LabeledSpan::at(
                                    start..=lhs_finish,
                                    "Invalid assignment target"
                                )],
                                "Invalid assignment target"
                            ))),
                        }
                    }
                    Err(e) => Some(Err(e)),
                }
            } else {
                Some(Err(miette!("Invalid assignment target")))
            }
        } else {
            Some(Err(miette!("invalid assignment")))
        }
    }

    fn or_expression(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let mut expr = match self.and_expression()? {
            Ok(e) => e,
            Err(e) => return Some(Err(e)),
        };

        loop {
            let Some(current) = self.tokens.peek() else {
                return Some(Ok(expr));
            };
            let Ok(next_tok) = current else {
                // Consume token if it's not a valid
                match self.tokens.next()? {
                    Ok(_) => unreachable!(),
                    Err(e) => return Some(Err(e)),
                }
            };

            if !matches!(next_tok, (_, Token::Or, _)) {
                break;
            }

            // Consume operator
            let (s, operator, f) = match self.tokens.next()? {
                Ok(tok) => tok,
                Err(e) => return Some(Err(e)),
            };

            let Some(and) = self.and_expression() else {
                return Some(Err(miette!(
                    labels = vec![LabeledSpan::at(
                        s..=f,
                        "Missing expression in the right part of logic expression"
                    )],
                    "Invalid syntax"
                )));
            };

            let right = match and {
                Ok(r) => r,
                Err(e) => return Some(Err(e)),
            };

            let start = *expr.location.start();
            let end = *right.location.end() - 1;
            let kind = ExprKind::Logical(operator, Box::new(expr), Box::new(right));

            expr = Expr {
                kind,
                location: start..=end,
            };
        }

        Some(Ok(expr))
    }

    fn and_expression(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let mut expr = match self.equality()? {
            Ok(e) => e,
            Err(e) => return Some(Err(e)),
        };

        loop {
            let Some(current) = self.tokens.peek() else {
                return Some(Ok(expr));
            };
            let Ok(next_tok) = current else {
                // Consume token if it's not a valid
                match self.tokens.next()? {
                    Ok(_) => unreachable!(),
                    Err(e) => return Some(Err(e)),
                }
            };

            if !matches!(next_tok, (_, Token::And, _)) {
                break;
            }

            // Consume operator
            let (s, operator, f) = match self.tokens.next()? {
                Ok(tok) => tok,
                Err(e) => return Some(Err(e)),
            };

            let Some(equality) = self.equality() else {
                return Some(Err(miette!(
                    labels = vec![LabeledSpan::at(
                        s..=f,
                        "Missing expression in the right part of logic expression"
                    )],
                    "Invalid syntax"
                )));
            };

            let right = match equality {
                Ok(r) => r,
                Err(e) => return Some(Err(e)),
            };

            let start = *expr.location.start();
            let end = *right.location.end() - 1;
            let kind = ExprKind::Logical(operator, Box::new(expr), Box::new(right));

            expr = Expr {
                kind,
                location: start..=end,
            };
        }

        Some(Ok(expr))
    }

    fn equality(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let mut expr = match self.comparison()? {
            Ok(e) => e,
            Err(e) => return Some(Err(e)),
        };

        loop {
            let Some(current) = self.tokens.peek() else {
                return Some(Ok(expr));
            };
            let Ok(next_tok) = current else {
                // Consume token if it's not a valid
                match self.tokens.next()? {
                    Ok(_) => unreachable!(),
                    Err(e) => return Some(Err(e)),
                }
            };

            if !matches!(next_tok, (_, Token::EqualEqual | Token::BangEqual, _)) {
                break;
            }

            // Consume operator
            let (s, operator, f) = match self.tokens.next()? {
                Ok(tok) => tok,
                Err(e) => return Some(Err(e)),
            };

            let Some(comparison) = self.comparison() else {
                return Some(Err(miette!(
                    labels = vec![LabeledSpan::at(
                        s..=f,
                        "Missing comparison expression in the right part of binary expression"
                    )],
                    "Invalid syntax"
                )));
            };

            let right = match comparison {
                Ok(r) => r,
                Err(e) => return Some(Err(e)),
            };

            let start = *expr.location.start();
            let end = *right.location.end() - 1;
            let kind = ExprKind::Binary(operator, Box::new(expr), Box::new(right));

            expr = Expr {
                kind,
                location: start..=end,
            };
        }

        Some(Ok(expr))
    }

    fn comparison(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let mut expr = match self.term()? {
            Ok(e) => e,
            Err(e) => return Some(Err(e)),
        };

        loop {
            let Some(current) = self.tokens.peek() else {
                return Some(Ok(expr));
            };
            let Ok(next_tok) = current else {
                // Consume token if it's not a valid
                match self.tokens.next()? {
                    Ok(_) => unreachable!(),
                    Err(e) => return Some(Err(e)),
                }
            };

            if !matches!(
                next_tok,
                (
                    _,
                    Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual,
                    _
                )
            ) {
                break;
            }

            // Consume operator
            let (s, operator, f) = match self.tokens.next()? {
                Ok(tok) => tok,
                Err(e) => return Some(Err(e)),
            };

            let Some(term) = self.term() else {
                return Some(Err(miette!(
                    labels = vec![LabeledSpan::at(
                        s..=f,
                        "Missing term expression in the right part of binary expression"
                    )],
                    "Invalid syntax"
                )));
            };

            let right = match term {
                Ok(r) => r,
                Err(e) => return Some(Err(e)),
            };

            let start = *expr.location.start();
            let end = *right.location.end();
            let kind = ExprKind::Binary(operator, Box::new(expr), Box::new(right));

            expr = Expr {
                kind,
                location: start..=end,
            };
        }

        Some(Ok(expr))
    }

    fn term(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let mut expr = match self.factor()? {
            Ok(e) => e,
            Err(e) => return Some(Err(e)),
        };

        loop {
            let Some(current) = self.tokens.peek() else {
                return Some(Ok(expr));
            };
            let Ok(next_tok) = current else {
                // Consume token if it's not a valid
                match self.tokens.next()? {
                    Ok(_) => unreachable!(),
                    Err(e) => return Some(Err(e)),
                }
            };

            if !matches!(next_tok, (_, Token::Plus | Token::Minus, _)) {
                break;
            }

            // Consume operator
            let (s, operator, f) = match self.tokens.next()? {
                Ok(tok) => tok,
                Err(e) => return Some(Err(e)),
            };

            let Some(factor) = self.factor() else {
                return Some(Err(miette!(
                    labels = vec![LabeledSpan::at(
                        s..=f,
                        "Missing factor expression in the right part of binary expression"
                    )],
                    "Invalid syntax"
                )));
            };

            let right = match factor {
                Ok(r) => r,
                Err(e) => return Some(Err(e)),
            };

            let start = *expr.location.start();
            let end = *right.location.end();
            let kind = ExprKind::Binary(operator, Box::new(expr), Box::new(right));

            expr = Expr {
                kind,
                location: start..=end,
            };
        }

        Some(Ok(expr))
    }

    fn factor(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let mut expr = match self.unary()? {
            Ok(e) => e,
            Err(e) => return Some(Err(e)),
        };
        loop {
            let Some(current) = self.tokens.peek() else {
                return Some(Ok(expr));
            };
            let Ok(next_tok) = current else {
                // Consume token if it's not a valid
                match self.tokens.next()? {
                    Ok(_) => unreachable!(),
                    Err(e) => return Some(Err(e)),
                }
            };

            if !matches!(next_tok, (_, Token::Star | Token::Slash, _)) {
                break;
            }

            // Consume operator
            let (s, operator, f) = match self.tokens.next()? {
                Ok(tok) => tok,
                Err(e) => return Some(Err(e)),
            };

            let Some(unary) = self.unary() else {
                return Some(Err(miette!(
                    labels = vec![LabeledSpan::at(
                        s..=f,
                        "Missing unary expression in the right part of binary expression"
                    )],
                    "Invalid syntax"
                )));
            };

            let right = match unary {
                Ok(r) => r,
                Err(e) => return Some(Err(e)),
            };

            let start = *expr.location.start();
            let end = *right.location.end();
            let kind = ExprKind::Binary(operator, Box::new(expr), Box::new(right));

            expr = Expr {
                kind,
                location: start..=end,
            };
        }

        Some(Ok(expr))
    }

    fn unary(&mut self) -> Option<miette::Result<Expr<'a>>> {
        match self.tokens.peek()? {
            Ok(tok) => {
                if let (_, Token::Bang | Token::Minus, _) = tok {
                    // Consume operator
                    let (s, operator, f) = match self.tokens.next()? {
                        Ok(tok) => tok,
                        Err(e) => return Some(Err(e)),
                    };
                    let Some(unary) = self.unary() else {
                        return Some(Err(miette!(
                            labels = vec![LabeledSpan::at(
                                s..=f,
                                format!("Dangling {operator} operator in unary expression")
                            )],
                            "Invalid syntax"
                        )));
                    };

                    match unary {
                        Ok(r) => {
                            let end = *r.location.end() - 1;
                            let kind = ExprKind::Unary(operator, Box::new(r));
                            Some(Ok(Expr {
                                kind,
                                location: s..=end,
                            }))
                        }
                        Err(e) => Some(Err(e)),
                    }
                } else {
                    self.call()
                }
            }
            Err(_) => {
                // Consume token if it's not a valid unary operator
                match self.tokens.next()? {
                    Ok(_) => unreachable!(),
                    Err(e) => Some(Err(e)),
                }
            }
        }
    }

    fn call(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let mut expr = match self.primary() {
            Some(Ok(expr)) => expr,
            Some(Err(e)) => return Some(Err(e)),
            None => return None,
        };
        loop {
            if self.matches(&[Token::LeftParen]) {
                expr = match self.finish_call(expr) {
                    Some(Ok(expr)) => expr,
                    Some(Err(e)) => return Some(Err(e)),
                    None => return None,
                };
            } else {
                break;
            }
        }
        Some(Ok(expr))
    }

    fn finish_call(&mut self, call: Expr<'a>) -> Option<miette::Result<Expr<'a>>> {
        let mut args = vec![];
        let location = call.location.clone();
        if !self.matches(&[Token::RightParen]) {
            loop {
                let arg = match self.expression() {
                    Some(Ok(expr)) => expr,
                    Some(Err(e)) => return Some(Err(e)),
                    None => return None,
                };
                if args.len() >= 255 {
                    return Some(Err(miette!(
                        labels = vec![LabeledSpan::at(
                            location,
                            "Cant have more then 255 arguments"
                        )],
                        "Arguments number exceeded"
                    )));
                }
                args.push(Box::new(arg));
                if !self.matches(&[Token::Comma]) {
                    break;
                }
            }
            if let Err(e) = self.consume(Token::RightParen) {
                return Some(Err(e));
            }
        }
        Some(Ok(Expr {
            kind: ExprKind::Call(Token::RightParen, Box::new(call), args),
            location,
        }))
    }

    fn primary(&mut self) -> Option<miette::Result<Expr<'a>>> {
        let (start, tok, finish) = match self.tokens.next()? {
            Ok(t) => t,
            Err(e) => return Some(Err(e)),
        };
        match tok {
            Token::String(_) | Token::Number(_) | Token::False | Token::Nil | Token::True => {
                let end = finish - 1;
                Some(Ok(Expr {
                    kind: ExprKind::Literal(Some(tok)),
                    location: start..=end,
                }))
            }
            Token::LeftParen => {
                let Some(expr) = self.expression() else {
                    return Some(Err(miette!(
                        labels = vec![LabeledSpan::at(
                            start..=finish,
                            "Expect expression after '('"
                        )],
                        "Expect expression after '('"
                    )));
                };
                match expr {
                    Ok(expr) => {
                        let Some(next) = self.tokens.next() else {
                            return Some(Err(miette!(
                                labels = vec![LabeledSpan::at(
                                    start..=finish,
                                    "Expect ')' after grouping expression that starts here"
                                )],
                                "Expect ')' after expression"
                            )));
                        };

                        if let Ok((_, Token::RightParen, finish)) = next {
                            let end = finish - 1;
                            Some(Ok(Expr {
                                kind: ExprKind::Grouping(Box::new(expr)),
                                location: start..=end,
                            }))
                        } else {
                            Some(Err(miette!(
                                labels = vec![LabeledSpan::at(
                                    start..=finish,
                                    "Expect ')' after grouping expression that starts here"
                                )],
                                "Expect ')' after expression"
                            )))
                        }
                    }
                    Err(e) => Some(Err(e)),
                }
            }
            Token::Identifier(_) => Some(Ok(Expr {
                kind: ExprKind::Variable(tok),
                location: start..=finish,
            })),
            _ => Some(Err(miette!(
                labels = vec![LabeledSpan::at(
                    start..finish,
                    format!("Unexpected primary token '{tok}'")
                )],
                "Invalid syntax"
            ))),
        }
    }

    fn consume_right_parent(&mut self, position: usize) -> miette::Result<RangeInclusive<usize>> {
        if self.tokens.peek().is_none() {
            return Err(miette!(
                labels = vec![LabeledSpan::at(position..=position, "Missing closing )")],
                "Missing closing paren"
            ));
        }
        let loc = self.consume(Token::RightParen)?;
        Ok(loc)
    }

    fn consume_current_and_open_paren(
        &mut self,
        token: &str,
    ) -> miette::Result<RangeInclusive<usize>> {
        let (start_token, _, end_token) = self.tokens.next().unwrap().unwrap(); // consume token TODO: include print start position into stmt location
        if self.tokens.peek().is_none() {
            return Err(miette!(
                labels = vec![LabeledSpan::at(
                    end_token..=end_token,
                    format!("Dangling {token}")
                )],
                "Dangling {token}"
            ));
        }
        let loc = self.consume(Token::LeftParen)?;
        let result = start_token..=*loc.end();
        Ok(result)
    }

    /// Validates current token matches any of tokens specifies and if so
    /// consumes it advancing iterator
    fn matches(&mut self, tokens: &[Token]) -> bool {
        let Some(Ok((_, next_tok, _))) = self.tokens.peek() else {
            return false;
        };
        for t in tokens {
            if next_tok == t {
                self.tokens.next();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, token: Token) -> miette::Result<RangeInclusive<usize>> {
        let Some(current) = self.tokens.peek() else {
            return Err(miette!("Expected {token} here"));
        };
        let Ok((start, next_tok, end)) = current else {
            // Consume and validate token
            match self.tokens.next() {
                Some(Ok(_)) | None => unreachable!(),
                Some(Err(e)) => return Err(e),
            }
        };
        let start = *start;
        let end = *end;

        if *next_tok != token {
            return Err(miette!(
                labels = vec![LabeledSpan::at(
                    start..=end,
                    format!("Expected {token} here")
                )],
                "Invalid syntax"
            ));
        }
        self.tokens.next(); // consume 
        Ok(start..=end)
    }
}

#[cfg(test)]
mod tests {
    use std::io::stdout;

    use crate::{ast::LoxValue, int::Interpreter};

    use super::*;
    use test_case::test_case;

    #[test_case("1 + 2 * 3;")]
    #[test_case("1 + 2 - 4;")]
    #[test_case("1 * 2 * 6;")]
    #[test_case("(1 + 2) * 5;")]
    #[test_case("2 == 3;")]
    #[test_case("(1 + 0 + 1);")]
    #[test_case("(1 == 2 == 17);")]
    #[test_case("(1 <= 2 > 10);")]
    #[test_case("--1;")]
    #[test_case("!!2;")]
    fn parser_tests(input: &str) {
        // Arrange
        let mut parser = Parser::new(input);

        // Act
        for stmt in &mut parser {
            // Assert
            assert!(stmt.is_ok());
        }
    }

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
    fn eval_numeric_positive_tests(input: &str, expected: f64) {
        // Arrange
        let mut parser = Parser::new(input);
        let expr = parser.expression().unwrap().unwrap();
        let mut eval = Interpreter::new(stdout());

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
    #[test_case("(\"a\" + 4) + \"c\"", "a4c")]
    #[test_case("(4 + \"a\") + \"c\"", "4ac")]
    #[test_case("(true + \"a\") + \"c\"", "trueac")]
    #[test_case("(nil + \"a\") + \"c\"", "ac")]
    fn eval_string_positive_tests(input: &str, expected: &str) {
        // Arrange
        let mut parser = Parser::new(input);
        let expr = parser.expression().unwrap().unwrap();
        let mut eval = Interpreter::new(stdout());

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
    #[test_case("20 <= 20", true)]
    #[test_case("40 <= 50", true)]
    #[test_case("nil <= false", true ; "nil lrs less or equal")]
    #[test_case("nil < false", false ; "nil lrs less")]
    #[test_case("nil == false", true ; "nil lrs equal")]
    #[test_case("!nil", true ; "not nil")]
    fn eval_predicates_tests(input: &str, expected: bool) {
        // Arrange
        let mut parser = Parser::new(input);
        let expr = parser.expression().unwrap().unwrap();
        let mut eval = Interpreter::new(stdout());

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
