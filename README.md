# rlox

**rlox** is a Rust implementation of the Lox programming language, featuring both a tree-walk interpreter and a bytecode virtual machine. The project is modular, with separate crates for the scanner, interpreter, compiler, and the main CLI application.

## Project Structure

- **scanner/**  
  Contains the lexical analyzer (scanner) for the Lox language. It is responsible for tokenizing the source code and is used by both the interpreter and the compiler.

- **interpreter/**  
  Implements a tree-walk interpreter for Lox. This module parses the token stream into an abstract syntax tree (AST) and directly interprets the code.

- **compiler/**  
  Provides a bytecode compiler and a virtual machine (VM) for Lox. The compiler translates Lox source code into bytecode, which is then executed by the VM for improved performance.

- **rlox/**  
  The main CLI application that ties together the interpreter and compiler. It provides a command-line interface for running Lox programs in either interpretation or compilation mode. It also includes utilities for debugging and bug reporting.

## Features

- **Two execution modes:**  
  - Tree-walk interpretation for simplicity and easier debugging.
  - Bytecode compilation and execution for better performance.

- **Command-line interface:**  
  The CLI allows you to choose between interpretation and compilation, run code from files or standard input, and print disassembled bytecode.

- **Error reporting:**  
  Uses the `miette` crate for rich, user-friendly error messages.

- **Modular design:**  
  Each major component (scanner, interpreter, compiler, CLI) is implemented as a separate Rust crate for clarity and reusability.

- **Bug reporting:**  
  Built-in command to generate a bug report with environment and version information.

## Repository

- Source: [https://git.egoroff.spb.ru/root/rlox](https://git.egoroff.spb.ru/root/rlox)

## Authors

- Alexander Egorov <egoroff@gmail.com> 